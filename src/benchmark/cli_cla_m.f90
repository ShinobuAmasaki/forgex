! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_cla_m module is a part of Forgex.
!
!! This file includes to handle command line arguments for the tool of forgex-cli.
!>
module forgex_cli_cla_m
   use, intrinsic :: iso_fortran_env, only: int32, real64, stderr => error_unit
   use :: forgex, only: operator(.match.)
   use :: forgex_cli_parameters_m, only: NUM_FLAGS, NUM_SUB_C, NUM_SUBC_DEBUG, &
            SUB_SUBC_AST, SUB_SUBC_THOMPSON, SUB_SUBC_LAZY_DFA, OP_MATCH, OP_IN, fmta
   use :: forgex_cli_type_m, only: flag_t, subc_t, pattern_t, arg_t, arg_element_t
   use :: forgex_cli_utils_m, only: get_flag_index, operator(.in.), register_flag, register_subc, &
            get_arg_command_line
   use :: forgex_cli_help_messages_m, only: print_help_debug, print_help_debug_ast, &
            print_help_debug_thompson, print_help_debug_lazy_dfa
   implicit none
   private

   type(flag_t), public :: all_flags(NUM_FLAGS)
   type(subc_t), public :: all_sub_cmds(NUM_SUB_C)

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
      logical :: is_exactly

      call cla%init_debug
      call cla%read_subsub()
      if (cla%sub_subc == '') then
         call print_help_debug
         stop
      end if

      call cla%get_patterns()

      ! Handle errors when a pattern does not exist.
      if (.not. allocated(cla%patterns)) then
         select case (cla%sub_subc)
         case (SUB_SUBC_AST)
            call print_help_debug_ast
         case (SUB_SUBC_THOMPSON)
            call print_help_debug_thompson
         case (SUB_SUBC_LAZY_DFA)
            call print_help_debug_lazy_dfa
         case default
            call print_help_debug
         end select
      end if

      if (cla%sub_subc == SUB_SUBC_LAZY_DFA) then
         if (size(cla%patterns) /= 3) then
            write(stderr, "(a, i0, a)") "Three arguments are expected, but ", size(cla%patterns), " were given."
            stop
         else if (cla%patterns(2)%p /= OP_MATCH .and. cla%patterns(2)%p /= OP_IN) then
            write(stderr, "(a)") "Operator "//OP_MATCH//" or "//OP_IN//" are expected, but "//cla%patterns(2)%p//" was given."
            stop
         end if

         if (cla%patterns(2)%p == OP_MATCH) then
            is_exactly = .true.
         else if (cla%patterns(2)%p == OP_IN) then
            is_exactly = .false.
         else
            write(stderr, '(a)') "Unknown operator: "//cla%patterns(2)%p
         end if
      else
         if (size(cla%patterns) > 1) then
            write(stderr, '(a, i0, a)') "Only single pattern is expected, but ", size(cla%patterns), " were given."
            stop
         end if
      end if


      select case (cla%sub_subc)
      case (SUB_SUBC_AST)
         call do_debug_ast(cla%flags, cla%patterns(1)%p)

      case (SUB_SUBC_THOMPSON)
         call do_debug_thompson(cla%flags, cla%patterns(1)%p)
      
      case (SUB_SUBC_LAZY_DFA)
         call do_debug_lazy_dfa(cla%flags, cla%patterns(1)%p, cla%patterns(3)%p, is_exactly)

      end select

   end subroutine

!=====================================================================!s


   subroutine cla__init_debug_subc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%subc%subsubcmd(NUM_SUBC_DEBUG))
      cla%subc%subsubcmd(1)%name = SUB_SUBC_AST
      cla%subc%subsubcmd(2)%name = SUB_SUBC_THOMPSON
      cla%subc%subsubcmd(3)%name = SUB_SUBC_LAZY_DFA
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

      if (j == 0) return

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
            write(stderr, fmta) "invalid option "//"'"//input_flags(k)%v//"'"
            stop
         end if
      end do

   end subroutine


   subroutine cla__read_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: subc

      if (ubound(cla%arg_info%arg, dim=1) < 1) then
         subc = ""
         return
      end if

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
      call register_flag(all_flags(3), 'no-table', '--no-table')
      call register_flag(all_flags(4), 'table-only', '--table-only')
   end subroutine init_flags


   subroutine init_sub_commands()
      implicit none

      call register_subc(all_sub_cmds(1), 'debug')
   end subroutine init_sub_commands

end module forgex_cli_cla_m