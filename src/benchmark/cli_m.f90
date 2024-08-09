module forgex_cli_m
#if defined(IMPURE) || defined(DEBUG)

   use, intrinsic :: iso_fortran_env, only: int32, real64, stderr => error_unit
   use :: forgex, only: operator(.match.)
   use :: forgex_cli_parameters_m
   use :: forgex_cli_type_m
   implicit none
   private

   public :: print_help_message
   public :: print_help_message_for_debug



   ! The type which represents command line arguments
   type, public :: cla_t
      type(arg_t) :: arg_info
      type(subc_t) :: subc
      character(:), allocatable :: sub_subc
      type(pattern_t), allocatable :: patterns(:)
      logical :: flags(NUM_FLAGS)
      integer :: flag_idx(NUM_FLAGS)
   contains
      procedure :: init => cla__initialize
      procedure :: read_sub => cla__read_subcommand
      procedure :: read_subsub => cla__read_sub_subcommand
      procedure :: collect_flags => cla__collect_flags
      procedure :: get_patterns => cla__get_patterns
      procedure :: init_debug => cla__init_debug_subc
      procedure :: do_debug => cla__do_debug_subc
   end type cla_t


contains

   subroutine cla__do_debug_subc(cla)
      use :: forgex_cli_debug_m
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: ast
      real(real64) :: time

      call cla%init_debug
      call cla%read_subsub()
      if (cla%sub_subc == '') then
         call print_help_message_for_debug
         stop
      end if

      select case (cla%sub_subc)
      case (SUB_SUBC_AST)
         call cla%get_patterns()
         if (size(cla%patterns) > 1) then
            write(stderr, '(a, i0, a)') "Only single pattern is expected, but ", size(cla%patterns), " were given."
         end if
         call do_debug_ast(cla%flags, cla%patterns(1)%p, ast, time)

      case (SUB_SUBC_THOMPSON)

      end select

   end subroutine

!=====================================================================!s


   subroutine cla__init_debug_subc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%subc%subsubcmd(NUM_SUBC_DEBUG))
      cla%subc%subsubcmd(1)%name = SUB_SUBC_AST
      cla%subc%subsubcmd(2)%name = SUB_SUBC_THOMPSON
   end subroutine

!=====================================================================!

   subroutine cla__get_patterns(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      integer :: i, j, k
      integer, allocatable :: idx(:)

      j = 0
      outer: do i = 3, cla%arg_info%argc

         ! 
         if (i <= maxval(cla%flag_idx)) then
            do k = 1, ubound(cla%flags, dim=1)
               if ( i == cla%flag_idx(k))  cycle outer
            end do
         end if

         j = j + 1
         if (.not. allocated(idx)) then
            idx = [i]
            cycle
         end if
         idx = [idx, i]

      end do outer

      allocate(cla%patterns(j))

      do i = 1, j
         cla%patterns(i)%p = cla%arg_info%arg(idx(i))%v
      end do

   end subroutine cla__get_patterns

   subroutine cla__collect_flags(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      type(arg_element_t), allocatable :: input_flags(:)
      integer :: n, i, j, k
      integer, allocatable :: indices(:)
      character(*), parameter :: pattern_long = "(--)(\w+-?)+"
      character(*), parameter :: pattern_short = "-\w+"

      n = cla%arg_info%argc

      allocate(input_flags(n))
      allocate(indices(n))

      indices(:) = 0

      ! Scan all command line arguments
      j = 0
      do i = 1, n
         if ((pattern_long .match. cla%arg_info%arg(i)%v) &
               .or. (pattern_short .match. cla%arg_info%arg(i)%v)) then
            ! If the CLA in question is a flag, register the CLA to input_flags array
            ! and record the index in indices array.
            j = j + 1 ! increment
            input_flags(j)%v = cla%arg_info%arg(i)%v
            indices(j) = i
         end if
      end do

      ! If there are no flags, return immediately.
      if (j == 0) return

      ! Register flags to cla object,
      ! stop the program if invalid flags are found.
      do k = 1, j
         if (input_flags(k) .in. all_flags) then
            i = get_flag_index(input_flags(k), all_flags)
            cla%flags(i) = .true.
            cla%flag_idx(i) = indices(k)
         else
            write(stderr, '(a)') "invalid option "//"'"//input_flags(k)%v//"'"
            stop
         end if
      end do

   end subroutine


   subroutine cla__read_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: subc

      subc = trim(cla%arg_info%arg(1)%v)
      if (subc .in. all_sub_cmds) then
         cla%subc%name = subc
      else
         cla%subc%name = ""
      end if
   end subroutine cla__read_subcommand


   subroutine cla__read_sub_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: ssc

      ssc = trim(cla%arg_info%arg(2)%v)
      if (ssc .in. cla%subc%subsubcmd) then
         cla%sub_subc= ssc
      else
         cla%sub_subc = ""
      end if
   end subroutine cla__read_sub_subcommand


   subroutine cla__initialize(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      call get_arg_command_line(cla%arg_info%argc, cla%arg_info%arg, cla%arg_info%entire)
      cla%flags = .false.
      cla%flag_idx = -1
      call init_flags
      call init_sub_commands

   end subroutine cla__initialize


!=====================================================================!

   subroutine init_flags()
      implicit none
      integer :: i

      call register_flag(all_flags(1), 'help','--help', '-h')
      call register_flag(all_flags(2), 'verbose', '--verbose', '-v')
   end subroutine init_flags


   subroutine init_sub_commands()
      implicit none

      call register_subc(all_sub_cmds(1), 'debug')
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

#endif

end module forgex_cli_m