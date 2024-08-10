module forgex_cli_debug_m
#if defined(IMPURE) && defined(DEBUG)
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit, stdout => output_unit
   use :: forgex_cli_time_measurement_m
   use :: forgex_cli_parameters_m
   use :: forgex_cli_type_m
   use :: forgex_enums_m
   use :: forgex_cli_utils_m


   implicit none
   private

   public :: do_debug_ast
   public :: do_debug_thompson
   public :: print_help_debug_ast
   public :: print_help_debug_thompson

contains

   subroutine do_debug_ast(flags, pattern)
      use :: forgex_syntax_tree_m
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
         character(NUM_DIGIT_KEY) :: parse_time, tree_count, tree_allocated, cbuff(3)
         parse_time = "parse time:"
         tree_count = "tree node count:"
         tree_allocated = "tree node allocated:"

         if (flags(FLAG_VERBOSE)) then
            cbuff = [parse_time, tree_count, tree_allocated]
            call right_justify(cbuff)

            write(stdout, "(a, a13)") trim(cbuff(1)), get_lap_time_in_appropriate_unit(time)
            write(stdout, "(a, i8)") trim(cbuff(2)), root
            write(stdout, "(a, i8)") trim(cbuff(3)), size(tree, dim=1)
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            write(stdout, "(a, a)") trim(parse_time), get_lap_time_in_appropriate_unit(time)
         end if
      end block output
      write(stdout, "(a)") ast

   end subroutine do_debug_ast


   subroutine do_debug_thompson(flags, pattern)
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
         nfa = nfa//trim(line)//newline
      end do
      close(uni)

      output: block
         character(NUM_DIGIT_KEY) :: parse_time, nfa_time, nfa_count, nfa_allocated, tree_count, tree_allocated, cbuff(6) = ''
         parse_time = "parse time:"
         nfa_time = "compile nfa time:"
         nfa_count = "nfa states:"
         nfa_allocated = "nfa states allocated:"

         tree_count = "tree node count:"
         tree_allocated = "tree node allocated:"

         if (flags(FLAG_VERBOSE)) then
            cbuff = [parse_time,  nfa_time, tree_count, tree_allocated, nfa_count, nfa_allocated]
            call right_justify(cbuff)

            write(stdout, "(a, a13)") trim(cbuff(1)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, "(a, a13)") trim(cbuff(2)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_int) trim(cbuff(3)), root
            write(stdout, fmt_out_int) trim(cbuff(4)), size(tree, dim=1)
            write(stdout, fmt_out_int) trim(cbuff(5)), automaton%nfa%nfa_top
            write(stdout, fmt_out_int) trim(cbuff(6)), automaton%nfa%nfa_limit
            write(stdout, *) ""
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff(:) = [parse_time, nfa_time, (repeat(" ", NUM_DIGIT_KEY), i = 1, 4)]
            call right_justify(cbuff)

            write(stdout, "(a, a)") trim(cbuff(1)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, "(a, a)") trim(cbuff(2)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, *) ""
         end if

         write(stdout, fmta) "=== NFA ==="
         write(stdout, fmta) trim(nfa)
         write(stdout, fmta) "Note: all segments of NFA were disjoined with overlapping portions."
         write(stdout, fmta) "==========="

      end block output


   end subroutine do_debug_thompson

!=====================================================================!

   subroutine print_help_debug_ast
      implicit none
      write(stderr, fmta) "Print the debug representation of an abstract syntax tree (AST)."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli debug ast <pattern>"
      write(stderr, fmta) ""
      write(stderr, fmta) "OPTIONS:"
      write(stderr, fmta) "   --verbose      Print more information."
      write(stderr, fmta) "   --no-table     Passing this flag suppresses the output of the property information table."
      stop
   end subroutine

   subroutine print_help_debug_thompson
      implicit none
      write(stderr, fmta) "Print the debug representaion of a Thompson NFA."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli debug thompson <pattern>"
      write(stderr, fmta) ""
      write(stderr, fmta) "OPTIONS:"
      write(stderr, fmta) "   --verbose      Print more information."
      write(stderr, fmta) "   --no-table     Passing this flag suppresses the output of the property information table."
      stop
   end subroutine print_help_debug_thompson
#endif
end module forgex_cli_debug_m
