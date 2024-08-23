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
   use :: forgex_cli_type_m, only: flag_t, cmd_t, pattern_t, arg_t, arg_element_t
   use :: forgex_cli_utils_m, only: get_flag_index, operator(.in.), register_flag, register_cmd, &
            get_arg_command_line
   use :: forgex_cli_help_messages_m, only: print_help_debug, print_help_debug_ast, &
            print_help_debug_thompson, print_help_find_match_lazy_dfa,  &
            print_help_find, print_help_find_match, print_help_find_match_lazy_dfa,  &
            print_help_find_match_dense_dfa, print_help_find_match_forgex_api
   implicit none
   private

   type(flag_t), public :: all_flags(NUM_FLAGS)
   type(cmd_t), public :: all_cmds(NUM_CMD)

   ! The type which represents command line arguments
   type, public :: cla_t
      type(arg_t) :: arg_info
      type(cmd_t) :: cmd, sub_cmd, sub_sub_cmd
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

   !> This subroutine registers all the flags forgex-cli accepts for the `flag_t` type array `all_flags`.
   subroutine init_flags()
      use :: forgex_enums_m
      implicit none
      call register_flag(all_flags(FLAG_HELP), 'help','--help', '-h')
      call register_flag(all_flags(FLAG_VERBOSE), 'verbose', '--verbose', '-v')
      call register_flag(all_flags(FLAG_NO_TABLE), 'no-table', '--no-table')
      call register_flag(all_flags(FLAG_TABLE_ONLY), 'table-only', '--table-only')
      call register_flag(all_flags(FLAG_NO_LITERAL), 'no-literal-optimize', '--disable-literal-optimize')
   end subroutine init_flags


   subroutine init_commands()
      implicit none
      call register_cmd(all_cmds(1), CMD_DEBUG)
      call register_cmd(all_cmds(2), CMD_FIND)
   end subroutine init_commands

!=====================================================================!

   !> Prepare subcommands for the `debug` command.
   subroutine cla__init_debug_subc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%cmd%subc(NUM_SUBC_DEBUG))
      cla%cmd%subc(1) = SUBC_AST
      cla%cmd%subc(2) = SUBC_THOMPSON
   end subroutine

   !> Prepare subcommands for the `find` command.
   subroutine cla__init_find_subc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%cmd%subc(NUM_SUBC_FIND))
      cla%cmd%subc(1) = SUBC_MATCH
   end subroutine cla__init_find_subc

!---------------------------------!
   !> Prepare sub-subcommands for the `match` subcommand.
   subroutine cla__init_find_match_subsubc(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      allocate(cla%sub_cmd%subc(NUM_SUBSUBC_MATCH))
      cla%sub_cmd%subc(1) = ENGINE_LAZY_DFA
      cla%sub_cmd%subc(2) = ENGINE_DENSE_DFA
      cla%sub_cmd%subc(3) = ENGINE_FORGEX_API
   end subroutine cla__init_find_match_subsubc

!=====================================================================!

   !> Read the first argument and match it with registered commands.
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
         call cla%cmd%set_name(cmd)
      else
         call cla%cmd%set_name("")
      end if
   end subroutine cla__read_command

   !> Read the second argument and match it with registered subcommands.
   subroutine cla__read_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: cmd
      integer :: i

      cmd = trim(cla%arg_info%arg(2)%v)

      do i = 1, size(cla%cmd%subc)
         if (cmd == cla%cmd%subc(i)) then
            call cla%sub_cmd%set_name(cmd)
            return
         end if
      end do
   end subroutine cla__read_subcommand


   !> Read the third argument and match it with registered sub-subcommands.
   subroutine cla__read_sub_subcommand(cla)
      implicit none
      class(cla_t), intent(inout) :: cla

      character(:), allocatable :: cmd
      integer :: i

      if (cla%arg_info%argc < 3) return

      cmd = trim(cla%arg_info%arg(3)%v)

      do i = 1, size(cla%sub_cmd%subc)
         if (cmd == cla%sub_cmd%subc(i)) then

            call cla%sub_sub_cmd%set_name(cmd)
            return
         end if
      end do
   end subroutine cla__read_sub_subcommand

!=====================================================================!

   !> Processes the `debug` command, reads a subcommand, and calls the corresponding procedure.
   subroutine cla__do_debug_subc(cla)
      use :: forgex_cli_debug_m
      implicit none
      class(cla_t), intent(inout) :: cla
      integer :: pattern_offset

      pattern_offset = 3

      call cla%init_debug()
      call cla%read_subc()
      if (cla%sub_cmd%get_name() == '') then
         call print_help_debug
      end if

      call cla%get_patterns(pattern_offset)

      ! Handle errors when a pattern does not exist.
      if (.not. allocated(cla%patterns)) then
         select case (cla%sub_cmd%get_name())
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


      select case (cla%sub_cmd%get_name())
      case (SUBC_AST)
         call do_debug_ast(cla%flags, cla%patterns(1)%p)

      case (SUBC_THOMPSON)
         call do_debug_thompson(cla%flags, cla%patterns(1)%p)

      end select

   end subroutine cla__do_debug_subc


   !> Processes the `debug` command, reads a subcommand and a sub-subcommand,
   !> and calls the corresponding procedure.
   subroutine cla__do_find_subc(cla)
      use :: forgex_cli_find_m
      implicit none
      class(cla_t), intent(inout) :: cla
      logical :: is_exactly
      integer :: pattern_offset
      character(:), allocatable :: text

      pattern_offset = 4

      call cla%init_find()
      call cla%read_subc()
      if (cla%sub_cmd%get_name() == '') then
         call print_help_find
      else if (cla%sub_cmd%get_name() == SUBC_MATCH) then
         call cla%init_find_match()
      endif

      call cla%read_subsubc()
      if (cla%sub_sub_cmd%get_name() == '') then
         select case (cla%sub_cmd%get_name())
         case (SUBC_MATCH)
            call print_help_find_match
         end select
      end if

      call cla%get_patterns(pattern_offset)

      if (.not. allocated(cla%patterns)) then
         select case(cla%sub_sub_cmd%get_name())
         case (ENGINE_LAZY_DFA)
            call print_help_find_match_lazy_dfa
         case (ENGINE_DENSE_DFA)
            call print_help_find_match_dense_dfa
         case (ENGINE_FORGEX_API)
            call print_help_find_match_forgex_api
         end select
      end if

      if ( cla%sub_sub_cmd%get_name() == ENGINE_LAZY_DFA  &
           .or. cla%sub_sub_cmd%get_name() == ENGINE_DENSE_DFA &
           .or. cla%sub_sub_cmd%get_name() == ENGINE_FORGEX_API) then
         if (size(cla%patterns) /= 3 .and. size(cla%patterns) /= 2) then
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

      if (size(cla%patterns) == 2) then
         text = ''
      else
         text = cla%patterns(3)%p
      end if

      select case (cla%sub_sub_cmd%get_name())
      case (ENGINE_LAZY_DFA)
         call do_find_match_lazy_dfa(cla%flags, cla%patterns(1)%p, text, is_exactly)
      case (ENGINE_DENSE_DFA)
         call do_find_match_dense_dfa(cla%flags, cla%patterns(1)%p, text, is_exactly)
      case (ENGINE_FORGEX_API)
         call do_find_match_forgex(cla%flags, cla%patterns(1)%p, text, is_exactly)
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