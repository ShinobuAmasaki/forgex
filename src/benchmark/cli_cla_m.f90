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
   use :: forgex_cli_parameters_m
   use :: forgex_cli_type_m, only: flag_t, cmd_t, subc_t, pattern_t, arg_t, arg_element_t
   use :: forgex_cli_utils_m, only: get_flag_index, operator(.in.), register_flag, register_cmd, &
            get_arg_command_line
   use :: forgex_cli_help_messages_m, only: print_help_debug, print_help_debug_ast, &
            print_help_debug_thompson, print_help_find_match_lazy_dfa,  &
            print_help_find, print_help_find_match, print_help_find_match_lazy_dfa, print_help_find_match_dense_dfa
   implicit none
   private

   type(flag_t), public :: all_flags(NUM_FLAGS)
   type(cmd_t), public :: all_cmds(NUM_CMD)

   ! The type which represents command line arguments
   type, public :: cla_t
      type(arg_t) :: arg_info
      type(cmd_t) :: cmd
      character(:), allocatable :: subc
      integer :: subc_index = 0
      integer :: subsubc_index =0
      character(:), allocatable :: subsubc
      type(pattern_t), allocatable :: patterns(:)
      logical :: flags(NUM_FLAGS)
      integer :: flag_idx(NUM_FLAGS)
   contains
      procedure :: init => cla__initialize
      procedure :: read_cmd => cla__read_command
      procedure :: read_subc => cla__read_subcommand
      procedure :: read_subsubc => cla__read_sub_subcommand
      procedure :: collect_flags => cla__collect_flags
      procedure :: get_patterns => cla__get_patterns
      procedure :: init_debug => cla__init_debug_subc
      procedure :: init_find => cla__init_find_subc
      procedure :: init_find_match => cla__init_find_match_subsubc
      procedure :: do_debug => cla__do_debug_subc
      procedure :: do_find => cla__do_find_subc
   end type cla_t


contains

!=====================================================================!

   subroutine init_flags()
      implicit none
      call register_flag(all_flags(1), 'help','--help', '-h')
      call register_flag(all_flags(2), 'verbose', '--verbose', '-v')
      call register_flag(all_flags(3), 'no-table', '--no-table')
      call register_flag(all_flags(4), 'table-only', '--table-only')
   end subroutine init_flags


   subroutine init_commands()
      implicit none
      call register_cmd(all_cmds(1), 'debug')
      call register_cmd(all_cmds(2), 'find')
   end subroutine init_commands

!=====================================================================!

   subroutine cla__init_debug_subc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%cmd%sub_cmd(NUM_SUBC_DEBUG))
      cla%cmd%sub_cmd(1)%name = SUBC_AST
      cla%cmd%sub_cmd(2)%name = SUBC_THOMPSON
   end subroutine

   subroutine cla__init_find_subc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%cmd%sub_cmd(NUM_SUBC_FIND))
      cla%cmd%sub_cmd(1)%name = SUBC_MATCH
   end subroutine cla__init_find_subc

!---------------------------------!
   subroutine cla__init_find_match_subsubc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla
      integer :: idx

      idx = cla%subc_index
      allocate(cla%cmd%sub_cmd(idx)%subsub(NUM_SUBSUBC_MATCH))
      cla%cmd%sub_cmd(idx)%subsub(1)%name = ENGINE_LAZY_DFA
      cla%cmd%sub_cmd(idx)%subsub(2)%name = ENGINE_DENSE_DFA
   end subroutine cla__init_find_match_subsubc

!=====================================================================!
   subroutine cla__read_command(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: cmd

      if (ubound(cla%arg_info%arg, dim=1) < 1) then
         cmd = ""
         return
      end if

      cmd = trim(cla%arg_info%arg(1)%v)
      if (cmd .in. all_cmds) then
         cla%cmd%name = cmd
      else
         cla%cmd%name = ""
      end if
   end subroutine cla__read_command


   subroutine cla__read_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: subc
      integer :: i

      subc = trim(cla%arg_info%arg(2)%v)

      do i = 1, size(cla%cmd%sub_cmd)
         if (subc == cla%cmd%sub_cmd(i)%name) then
            cla%subc_index = i
            cla%subc = subc
            return
         end if
      end do

   end subroutine cla__read_subcommand


   subroutine cla__read_sub_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: subsubc
      integer :: i

      cla%subsubc = ''
      subsubc = trim(cla%arg_info%arg(3)%v)

      do i = 1, size(cla%cmd%sub_cmd(cla%subc_index)%subsub)
         if (subsubc == cla%cmd%sub_cmd(cla%subc_index)%subsub(i)%name) then
            cla%subsubc_index = i
            cla%subsubc = subsubc
            return
         end if
      end do
   end subroutine cla__read_sub_subcommand

!=====================================================================!
   subroutine cla__do_debug_subc(cla)
      use :: forgex_cli_debug_m
      implicit none
      class(cla_t), intent(inout) :: cla
      logical :: is_exactly

      call cla%init_debug
      call cla%read_subc()
      if (cla%subc == '') then
         call print_help_debug
         stop
      end if

      call cla%get_patterns(3)

      ! Handle errors when a pattern does not exist.
      if (.not. allocated(cla%patterns)) then
         select case (cla%subc)
         case (SUBC_AST)
            call print_help_debug_ast
         case (SUBC_THOMPSON)
            call print_help_debug_thompson
         case default
            call print_help_debug
         end select
      end if

      if (size(cla%patterns) > 1) then
         write(stderr, '(a, i0, a)') "Only single pattern is expected, but ", size(cla%patterns), " were given."
         stop
      end if


      select case (cla%subc)
      case (SUBC_AST)
         call do_debug_ast(cla%flags, cla%patterns(1)%p)

      case (SUBC_THOMPSON)
         call do_debug_thompson(cla%flags, cla%patterns(1)%p)

      end select

   end subroutine cla__do_debug_subc


   subroutine cla__do_find_subc(cla)
      use :: forgex_cli_find_m
      implicit none
      class(cla_t), intent(inout) :: cla
      logical :: is_exactly
      integer :: i

      call cla%init_find()
      call cla%read_subc()
      if (cla%subc == '') then
         call print_help_find
      else if (cla%subc == SUBC_MATCH) then
         call cla%init_find_match()
      endif
      
      call cla%read_subsubc()
      if (cla%subsubc == '') then
         select case (cla%subc)
         case (SUBC_MATCH)
            call print_help_find_match
         end select
      end if

      call cla%get_patterns(4)

      if (.not. allocated(cla%patterns)) then
         select case(cla%subsubc)
         case (ENGINE_LAZY_DFA)
            call print_help_find_match_lazy_dfa
         case (ENGINE_DENSE_DFA)
            call print_help_find_match_dense_dfa
         end select
      end if

      if (cla%subsubc == ENGINE_LAZY_DFA .or. cla%subsubc == ENGINE_DENSE_DFA) then
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
         call print_help_find_match
      end if

      select case (cla%subsubc)
      case (ENGINE_LAZY_DFA)
         call do_find_match_lazy_dfa(cla%flags, cla%patterns(1)%p, cla%patterns(3)%p, is_exactly)
      case (ENGINE_DENSE_DFA)
         ! call do_find_match_dense_dfa
         write(stderr, *) "dense matching is currently unavailable."
         stop
      case default
         call print_help_find_match
      end select

   end subroutine cla__do_find_subc
!=====================================================================!s


   subroutine cla__get_patterns(cla, offset)
      implicit none
      class(cla_t), intent(inout) :: cla
      integer, intent(in) :: offset
      integer :: i, j, k
      integer, allocatable :: idx(:)

      j = 0
      outer: do i = offset, cla%arg_info%argc

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


   subroutine cla__initialize(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      call get_arg_command_line(cla%arg_info%argc, cla%arg_info%arg, cla%arg_info%entire)
      cla%flags = .false.
      cla%flag_idx = -1
      call init_flags
      call init_commands

   end subroutine cla__initialize

end module forgex_cli_cla_m