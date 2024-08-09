module forgex_cli_debug_m
#if defined(IMPURE) && defined(DEBUG)
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit, stdout => output_unit
   use :: forgex_time_measurement_m
   use :: forgex_cli_parameters_m
   use :: forgex_cli_type_m
   implicit none
   private

   public :: do_debug_ast

contains

   subroutine do_debug_ast(flags, pattern, ast, time)
      use :: forgex_enums_m
      use :: forgex_syntax_tree_m
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern
      character(:),allocatable, intent(inout) :: ast
      real(real64), intent(inout) :: time

      type(tree_node_t), allocatable :: tree(:)
      type(tape_t) :: tape
      integer :: root
      integer :: uni, ierr, siz
      character(:), allocatable :: buff

      open(newunit=uni, status='scratch')

      if (flags(FLAG_HELP)) call print_help_debug_ast

      call time_begin
      call build_syntax_tree(trim(pattern), tape, tree, root)
      time = time_lap()

      call print_tree(tree, root, uni)

      inquire(unit=uni, size=siz)
      allocate(character(siz+2) :: buff)

      rewind(uni)
      read(uni, '(a)', iostat=ierr) buff
      close(uni)

      ast = trim(buff)

      write(stdout, "(a, a13)") "parse time:", get_lap_time_in_appropriate_unit(time)
      if (flags(FLAG_VERBOSE)) then
         write(stdout, "(a, i4)") "tree node count:", root
         write(stdout, "(a, i4)") "tree node allocated:", size(tree, dim=1)
      end if
      write(stdout, "(a)") ast

   end subroutine do_debug_ast


!=====================================================================!

   subroutine print_help_debug_ast
      implicit none
      write(stderr, *) "Print the debug representation of an abstract syntax tree (AST)."
      write(stderr, *) ""
      write(stderr, *) "USAGE:"
      write(stderr, *) "   forgex-cli debug ast <pattern>"
      write(stderr, *) ""
      write(stderr, *) "OPTIONS:"
   end subroutine
#endif
end module forgex_cli_debug_m
