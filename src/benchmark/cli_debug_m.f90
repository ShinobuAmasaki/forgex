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
            fmt_out_logi, fmta, CRLF, LF
   use :: forgex_enums_m, only: FLAG_HELP, FLAG_NO_TABLE, FLAG_VERBOSE, FLAG_TABLE_ONLY, OS_WINDOWS
   use :: forgex_cli_utils_m, only: get_os_type, right_justify
   use :: forgex_cli_help_messages_m, only: print_help_debug_ast, print_help_debug_thompson, print_help_debug_lazy_dfa
   implicit none
   private

   public :: do_debug_ast
   public :: do_debug_thompson
   public :: do_debug_lazy_dfa

contains

   subroutine do_debug_ast(flags, pattern)
      use :: forgex_syntax_tree_m
      use :: forgex_cli_memory_calculation_m
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern

      type(tree_node_t), allocatable :: tree(:)
      type(tape_t) :: tape
      integer :: root
      integer :: uni, ierr, siz
      character(:), allocatable :: buff
      character(:),allocatable :: ast
      real(real64) :: time

      if (flags(FLAG_HELP)) call print_help_debug_ast

      call time_begin
      call build_syntax_tree(trim(pattern), tape, tree, root)
      time = time_lap()

      open(newunit=uni, status='scratch')
      call print_tree(tree, root, uni)

      inquire(unit=uni, size=siz)
      allocate(character(siz+2) :: buff)

      rewind(uni)
      read(uni, fmta, iostat=ierr) buff
      close(uni)

      ast = trim(buff)

      output: block
         character(NUM_DIGIT_KEY) :: parse_time, tree_count, tree_allocated, memory
         character(NUM_DIGIT_KEY) :: cbuff(4)
         integer :: i
         parse_time = "parse time:"
         tree_count = "tree node count:"
         tree_allocated = "tree node allocated:"
         memory = "memory (estimated):"

         if (flags(FLAG_VERBOSE)) then
            cbuff = [parse_time, memory, tree_count, tree_allocated]
            call right_justify(cbuff)

            write(stdout, fmt_out_time) trim(cbuff(1)), get_lap_time_in_appropriate_unit(time)
            write(stdout, fmt_out_int) trim(cbuff(2)), mem_tape(tape) + mem_tree(tree)
            write(stdout, fmt_out_int) trim(cbuff(3)), root
            write(stdout, fmt_out_int) trim(cbuff(4)), size(tree, dim=1)
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff = [parse_time, memory, (repeat(" ", NUM_DIGIT_KEY), i=1, 2)]
            call right_justify(cbuff)

            write(stdout, fmt_out_time) trim(cbuff(1)), get_lap_time_in_appropriate_unit(time)
            write(stdout, fmt_out_int) trim(cbuff(2)), mem_tape(tape)+mem_tree(tree)
         end if
      end block output

      if (flags(FLAG_TABLE_ONLY)) return
      write(stdout, "(a)") ast

   end subroutine do_debug_ast


   subroutine do_debug_thompson(flags, pattern)
      use :: forgex_cli_memory_calculation_m
      use :: forgex_automaton_m
      use :: forgex_syntax_tree_m
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern

      type(tree_node_t), allocatable :: tree(:)
      type(tape_t) :: tape
      type(automaton_t) :: automaton
      integer :: root
      integer :: uni, ierr, siz, i
      character(:), allocatable :: nfa
      character(256) :: line
      real(real64) :: lap1, lap2
      
      nfa = ''

      if (flags(FLAG_HELP)) call print_help_debug_thompson
      if (pattern == '') call print_help_debug_thompson

      call time_begin()
      call build_syntax_tree(trim(pattern), tape, tree, root)
      lap1 = time_lap()

      call automaton%nfa%build(tree, root, automaton%nfa_entry, automaton%nfa_exit, automaton%all_segments)
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

         memsiz = mem_tape(tape) + mem_tree(tree) &
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
            write(stdout, fmt_out_int) trim(cbuff(5)), size(tree, dim=1)
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
         write(stdout, fmta) "=== NFA ==="
         write(stdout, fmta) trim(nfa)
         write(stdout, fmta) "Note: all segments of NFA were disjoined with overlapping portions."
         write(stdout, fmta) "==========="

      end block output
   end subroutine do_debug_thompson


   subroutine do_debug_lazy_dfa(flags, pattern, text, is_exactly)
      use :: forgex_automaton_m
      use :: forgex_syntax_tree_m
      use :: forgex_cli_memory_calculation_m
      use :: forgex_api_internal_m
      use :: forgex_nfa_state_set_m
      use :: forgex_cli_utils_m
      use :: forgex_utility_m, only: is_there_caret_at_the_top, is_there_dollar_at_the_end
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern
      character(*), intent(in) :: text
      logical, intent(in) :: is_exactly

      type(tree_node_t), allocatable :: tree(:)
      type(tape_t) :: tape
      type(automaton_t) :: automaton
      type(nfa_state_set_t) :: initial_closure
      integer(int32) :: new_index
      integer :: root

      integer :: uni, ierr, siz, i
      character(:), allocatable :: dfa_for_print
      character(256) :: line
      real(real64) :: lap1, lap2, lap3, lap4
      logical :: res

      dfa_for_print = ''

      if (flags(FLAG_HELP)) call print_help_debug_lazy_dfa
      if (pattern == '') call print_help_debug_lazy_dfa
      if (text == '') call print_help_debug_lazy_dfa

      call time_begin()
      call build_syntax_tree(trim(pattern), tape, tree, root)
      lap1 = time_lap()

      call automaton%preprocess(tree, root)
      lap2 = time_lap()

      call automaton%init()
      lap3 = time_lap()
      
      if (is_exactly) then
         call do_matching_exactly(automaton, text, res)
      else
         block
            integer :: from, to
            call do_matching_including(automaton, char(0)//text//char(0), from, to)
            if (is_there_caret_at_the_top(pattern)) then
               from = from
            else
               from = from -1
            end if
      
            if (is_there_dollar_at_the_end(pattern)) then
               to = to - 2
            else
               to = to - 1
            end if
      
            if (from > 0 .and. to > 0) then
               res = .true.
            else
               res = .false.
            end if

         end block
      end if
      lap4 = time_lap()

      open(newunit=uni, status='scratch')
      write(uni, fmta) "=== NFA ==="
      call automaton%nfa%print(uni, automaton%nfa_exit)
      write(uni, fmta) "=== DFA ==="
      call automaton%print_dfa(uni)

      rewind(uni)
      ierr = 0
      do while (ierr == 0)
         read(uni, fmta, iostat=ierr) line
         if (ierr/=0) exit
         if (get_os_type() == OS_WINDOWS) then
            dfa_for_print = dfa_for_print//trim(line)//CRLF
         else
            dfa_for_print = dfa_for_print//trim(line)//LF
         end if
      end do
      close(uni)

      output: block
         character(NUM_DIGIT_KEY) :: pattern_key, text_key
         character(NUM_DIGIT_KEY) :: parse_time, nfa_time, dfa_init_time, matching_time, memory
         character(NUM_DIGIT_KEY) :: tree_count
         character(NUM_DIGIT_KEY) :: nfa_count
         character(NUM_DIGIT_KEY) :: dfa_count, matching_result
         character(NUM_DIGIT_KEY) :: cbuff(11) = ''
         integer :: memsiz

         pattern_key    = "pattern:"
         text_key       = "text:"
         parse_time     = "parse time:"
         nfa_time       = "compile nfa time:"
         dfa_init_time  = "dfa initialize time:"
         matching_time  = "dfa matching time:"
         memory         = "memory (estimated):"
         matching_result= "matching result:"
         
         tree_count     = "tree node count:"
         nfa_count      = "nfa states:"
         dfa_count      = "dfa states:"

         memsiz = mem_tape(tape) + mem_tree(tree) + mem_nfa_graph(automaton%nfa) &
                   + mem_dfa_graph(automaton%dfa) + 4*3
         if (allocated(automaton%entry_set%vec)) then 
            memsiz = memsiz + size(automaton%entry_set%vec, dim=1)
         end if
         if (allocated(automaton%all_segments)) then
            memsiz = memsiz + size(automaton%all_segments, dim=1)*8
         end if

         if (flags(FLAG_VERBOSE)) then
            cbuff = [pattern_key, text_key, parse_time, nfa_time, dfa_init_time, matching_time, matching_result, memory, &
                     tree_count, nfa_count, dfa_count]
            call right_justify(cbuff)
            
            write(stdout, '(a, 1x, a)') trim(cbuff(1)), trim(adjustl(pattern))
            write(stdout, '(a, 1x, a)') trim(cbuff(2)), trim(adjustl(text))
            write(stdout, fmt_out_time) trim(cbuff(3)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(4)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_time) trim(cbuff(5)), get_lap_time_in_appropriate_unit(lap3)
            write(stdout, fmt_out_time) trim(cbuff(6)), get_lap_time_in_appropriate_unit(lap4)
            write(stdout, fmt_out_logi)  trim(cbuff(7)), res
            write(stdout, fmt_out_int) trim(cbuff(8)), memsiz
            write(stdout, fmt_out_ratio) trim(cbuff(9)), root, size(tree, dim=1)
            write(stdout, fmt_out_ratio) trim(cbuff(10)), automaton%nfa%nfa_top, automaton%nfa%nfa_limit
            write(stdout, fmt_out_ratio) trim(cbuff(11)), automaton%dfa%dfa_top, automaton%dfa%dfa_limit
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff(:) = [pattern_key, text_key, parse_time, nfa_time, dfa_init_time, matching_time, matching_result, memory, &
                        (repeat(" ", NUM_DIGIT_KEY), i = 1, 3)]
            call right_justify(cbuff)
            write(stdout, '(a,1x,a)') trim(cbuff(1)), pattern
            write(stdout, '(a,1x,a)') trim(cbuff(2)), text
            write(stdout, fmt_out_time) trim(cbuff(3)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(4)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_time) trim(cbuff(5)), get_lap_time_in_appropriate_unit(lap3)
            write(stdout, fmt_out_time) trim(cbuff(6)), get_lap_time_in_appropriate_unit(lap4)
            write(stdout, fmt_out_logi)  trim(cbuff(7)), res
            write(stdout, fmt_out_int) trim(cbuff(8)), memsiz
         end if

         if (flags(FLAG_TABLE_ONLY)) return

         write(stdout, *) ""

         write(stdout, fmta, advance='no') trim(dfa_for_print)
         write(stdout, fmta) "==========="

      end block output
      call automaton%free
   end subroutine do_debug_lazy_dfa

!=====================================================================!



end module forgex_cli_debug_m
