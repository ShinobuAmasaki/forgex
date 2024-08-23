! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_debug_m module is a part of Forgex.
!
module forgex_cli_debug_m
   use, intrinsic :: iso_fortran_env, only: int32, real64, stderr => error_unit, stdout => output_unit
   use :: forgex_cli_time_measurement_m, only: time_begin, time_lap, get_lap_time_in_appropriate_unit
   use :: forgex_cli_parameters_m, only: NUM_DIGIT_KEY, fmt_out_time, fmt_out_int, fmt_out_ratio, &
            fmt_out_logi, fmta, fmt_out_char, CRLF, LF, HEADER_DFA, HEADER_NFA ,FOOTER
   use :: forgex_enums_m, only: FLAG_HELP, FLAG_NO_TABLE, FLAG_VERBOSE, FLAG_TABLE_ONLY, OS_WINDOWS
   use :: forgex_cli_utils_m, only: get_os_type, right_justify
   use :: forgex_cli_help_messages_m, only: print_help_debug_ast, print_help_debug_thompson
   implicit none
   private

   public :: do_debug_ast
   public :: do_debug_thompson

contains

   subroutine do_debug_ast(flags, pattern)
      use :: forgex_syntax_tree_graph_m
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_cli_memory_calculation_m
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern

      type(tree_t) :: tree
      integer :: root
      integer :: uni, ierr, siz
      character(:), allocatable :: buff
      character(:),allocatable :: ast, prefix, suffix, entire !, middle
      real(real64) :: lap1, lap2

      if (flags(FLAG_HELP)) call print_help_debug_ast

      call time_begin
      call tree%build(trim(pattern))
      lap1 = time_lap()

      entire = get_entire_literal(tree)
      prefix = get_prefix_literal(tree)
      ! middle = get_middle_literal(tree)
      suffix = get_suffix_literal(tree)
      lap2 = time_lap()

      open(newunit=uni, status='scratch')
      call tree%print(uni)

      inquire(unit=uni, size=siz)
      allocate(character(siz+2) :: buff)

      rewind(uni)
      read(uni, fmta, iostat=ierr) buff
      close(uni)

      ast = trim(buff)

      output: block
         character(NUM_DIGIT_KEY) :: parse_time, literal_time, tree_count, tree_allocated, &
            memory, literal_pre, literal_post, literal_all, literal_mid
         character(NUM_DIGIT_KEY) :: cbuff(9)
         integer :: i
         parse_time     = "parse time:"
         literal_time   = "extract time:"
         tree_count     = "tree node count:"
         tree_allocated = "tree node allocated:"
         literal_all    = "extracted literal:"
         literal_pre    = "extracted prefix:"
         literal_mid    = "extracted middle:"
         literal_post   = "extracted suffix:"
         memory         = "memory (estimated):"
         

         if (flags(FLAG_VERBOSE)) then
            cbuff = [parse_time, literal_time, literal_all, literal_pre, literal_mid, literal_post, &
                     memory, tree_count, tree_allocated]
            call right_justify(cbuff)

            write(stdout, fmt_out_time) trim(cbuff(1)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(2)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_char) trim(cbuff(3)), entire
            write(stdout, fmt_out_char) trim(cbuff(4)), prefix
            ! write(stdout, fmt_out_char) trim(cbuff(5)), middle
            write(stdout, fmt_out_char) trim(cbuff(6)), suffix
            write(stdout, fmt_out_int) trim(cbuff(7)), mem_tape(tree%tape) + mem_tree(tree%nodes)
            write(stdout, fmt_out_int) trim(cbuff(8)), root
            write(stdout, fmt_out_int) trim(cbuff(9)), size(tree%nodes, dim=1)
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff = [parse_time, literal_time, literal_all, literal_pre, literal_mid, &
                     literal_post, memory, (repeat(" ", NUM_DIGIT_KEY), i=1, 2)]
            call right_justify(cbuff)

            write(stdout, fmt_out_time) trim(cbuff(1)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(2)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_char) trim(cbuff(3)), entire
            write(stdout, fmt_out_char) trim(cbuff(4)), prefix
            ! write(stdout, fmt_out_char) trim(cbuff(5)), middle
            write(stdout, fmt_out_char) trim(cbuff(6)), suffix
            write(stdout, fmt_out_int) trim(cbuff(7)), mem_tape(tree%tape)+mem_tree(tree%nodes)
         end if
      end block output

      if (flags(FLAG_TABLE_ONLY)) return
      write(stdout, fmta) ast

   end subroutine do_debug_ast


   subroutine do_debug_thompson(flags, pattern)
      use :: forgex_cli_memory_calculation_m
      use :: forgex_automaton_m
      use :: forgex_syntax_tree_graph_m
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern

      type(tree_t) :: tree
      type(automaton_t) :: automaton
      integer :: root
      integer :: uni, ierr, i
      character(:), allocatable :: nfa
      character(256) :: line
      real(real64) :: lap1, lap2

      nfa = ''

      if (flags(FLAG_HELP)) call print_help_debug_thompson
      if (pattern == '') call print_help_debug_thompson

      call time_begin()
      ! call build_syntax_tree(trim(pattern), tree%tape, tree, root)
      call tree%build(trim(pattern))
      lap1 = time_lap()

      call automaton%nfa%build(tree, automaton%nfa_entry, automaton%nfa_exit, automaton%all_segments)
      lap2 = time_lap()

      open(newunit=uni, status='scratch')
      call automaton%nfa%print(uni, automaton%nfa_exit)

      rewind(uni)
      ierr = 0
      do while (ierr == 0)
         read(uni, fmta, iostat=ierr) line
         if (ierr /= 0) exit

         if (get_os_type() == OS_WINDOWS) then
            nfa = nfa//trim(line)//CRLF
         else
            nfa = nfa//trim(line)//LF
         end if

      end do
      close(uni)

      output: block
         character(NUM_DIGIT_KEY) :: parse_time, nfa_time, memory, nfa_count, nfa_allocated, tree_count, tree_allocated
         character(NUM_DIGIT_KEY) :: cbuff(7) = ''
         integer :: memsiz

         parse_time     = "parse time:"
         nfa_time       = "compile nfa time:"
         memory         = "memory (estimated):"

         nfa_count      = "nfa states:"
         nfa_allocated  = "nfa states allocated:"
         tree_count     = "tree node count:"
         tree_allocated = "tree node allocated:"

         memsiz = mem_tape(tree%tape) + mem_tree(tree%nodes) &
                  + mem_nfa_graph(automaton%nfa) + 4*3
         if (allocated(automaton%entry_set%vec)) then
            memsiz = memsiz + size(automaton%entry_set%vec, dim=1)
         end if
         if (allocated(automaton%all_segments)) then
            memsiz = memsiz + size(automaton%all_segments, dim=1)*8
         end if

         if (flags(FLAG_VERBOSE)) then
            cbuff = [parse_time,  nfa_time, memory, tree_count, tree_allocated, nfa_count, nfa_allocated]
            call right_justify(cbuff)

            write(stdout, fmt_out_time) trim(cbuff(1)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(2)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_int)  trim(cbuff(3)), memsiz
            write(stdout, fmt_out_int) trim(cbuff(4)), root
            write(stdout, fmt_out_int) trim(cbuff(5)), size(tree%nodes, dim=1)
            write(stdout, fmt_out_int) trim(cbuff(6)), automaton%nfa%nfa_top
            write(stdout, fmt_out_int) trim(cbuff(7)), automaton%nfa%nfa_limit
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff(:) = [parse_time, nfa_time, memory, (repeat(" ", NUM_DIGIT_KEY), i = 1, 4)]
            call right_justify(cbuff)

            write(stdout, fmt_out_time) trim(cbuff(1)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(2)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_int) trim(cbuff(3)), memsiz
         end if

         if (flags(FLAG_TABLE_ONLY)) return

         write(stdout, *) ""
         write(stdout, fmta) HEADER_NFA
         write(stdout, fmta) trim(nfa)
         write(stdout, fmta) "Note: all segments of NFA were disjoined with overlapping portions."
         write(stdout, fmta) FOOTER

      end block output
   end subroutine do_debug_thompson



!=====================================================================!



end module forgex_cli_debug_m
