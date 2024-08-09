module forgex_cli_m
   use, intrinsic :: iso_fortran_env, only: int32, stderr => error_unit
   use :: forgex, only: operator(.match.)
   use :: forgex_cli_type_m
   implicit none
   private


   public :: print_help_message
   public :: print_help_message_for_debug


   ! The type of dat represents command line arguments
   type, public :: cla_t
      type(arg_t) :: arg_info
      type(subc_t) :: subc
      character(:), allocatable :: sub_sub_command
      character(:), allocatable :: value(:)
      logical :: flags(NUM_FLAGS)
   contains
      procedure :: init => cla__initialize
      procedure :: read_sub => cla__read_subcommand
      procedure :: read_subsub => cla__read_sub_subcommand
      procedure :: collect_flags => cla__collect_flags
   end type cla_t

   type(flag_t)  :: all_flags(NUM_FLAGS)
   type(subc_t)  :: all_subcs(NUM_SUB_C)

contains

   subroutine cla__collect_flags(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      type(argv_t), allocatable :: input_flags(:)
      integer :: n, i, j, k

      n = cla%arg_info%argc

      allocate(input_flags(n))

      j = 0
      do i = 1, n
         if ("(--)(\w+-?)+" .match. cla%arg_info%arg(i)%v) then
            j = j + 1
            input_flags(j)%v = cla%arg_info%arg(i)%v
         end if
      end do

      if (j == 0) return

      do k = 1, j
         if (input_flags(k)%v .in. all_flags) then
            do i = 1, ubound(all_flags, dim=1)
               if (input_flags(k)%v == all_flags(i)%name) cla%flags(i) = .true.
            end do
         end if
      end do

   end subroutine


   subroutine cla__read_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla
      character(:), allocatable :: subc

      subc = trim(cla%arg_info%arg(1)%v)

      if (subc .in. all_subcs) then
         cla%subc%name = subc
      else
         call print_help_message()
      end if
   end subroutine cla__read_subcommand


   subroutine cla__read_sub_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: subsubc

      subsubc = trim(cla%arg_info%arg(2)%v)
      if (subsubc .in. cla%subc%subsubc) then
         cla%sub_sub_command = subsubc
      end if
   end subroutine cla__read_sub_subcommand


   subroutine cla__initialize(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      call get_arg_command_line(cla%arg_info%argc, cla%arg_info%arg, cla%arg_info%entire)
      call init_flags
      call init_sub_commands

   end subroutine cla__initialize


!=====================================================================!

   subroutine init_flags()
      implicit none
      integer :: i

      call register_flag(all_flags(1),'help','--help', '-h', .false.)
   end subroutine init_flags


   subroutine init_sub_commands()
      implicit none

      call register_subc(all_subcs(1), 'debug')
   end subroutine init_sub_commands

!=====================================================================!

   subroutine print_help_message()
      implicit none

      write(stderr, '(a)') "A tool for interacting with Forgex on the command line."
      write(stderr, '(a)') ""
      write(stderr, '(a)') "USAGE:"
      write(stderr, '(a)') "   forgex-cli <command> ..."
      write(stderr, '(a)') ""
      write(stderr, '(a)') "COMMANDS:"
      write(stderr, '(a)') "   debug   Print the debug representations from Forgex's regex engine."
      stop
   end subroutine print_help_message


   subroutine  print_help_message_for_debug()
      write(stderr, '(a)') "Prints the debug representation Forgex provides."
      write(stderr, '(a)') ""
      write(stderr, '(a)') "USAGE:"
      write(stderr, '(a)') "   forgex-cli debug <command> ..."
      write(stderr, '(a)') ""
      write(stderr, '(a)') "COMMANDS:"
      write(stderr, '(a)') "   ast        Print the debug representation of an AST."
      write(stderr, '(a)') "   thompson   Print the debug representation of a Thompson NFA."
   end subroutine print_help_message_for_debug



end module forgex_cli_m