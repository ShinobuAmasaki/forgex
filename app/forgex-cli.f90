! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli program  is a part of Forgex.
!
!! This file includes a command line tool for debugging, ad hoc benchmarking
!! regular expressions.
!> The main program of forgex-cli command.
!> Most of the implementations is in the cli_cla_m module etc. 
program forgex_cli
   use :: forgex_enums_m, only: FLAG_HELP
   use :: forgex_cli_parameters_m, only: CMD_DEBUG, CMD_FIND
   use :: forgex_cli_cla_m, only: cla_t
   use :: forgex_cli_help_messages_m, only: print_help, print_help_debug, print_help_find
   implicit none

   type(cla_t) :: cla

   ! Read command line arguments and write them to the corresponding derived-type data. 
   call cla%init()
   call cla%collect_flags()

   ! If the --help or -h flags is first, output the help message.
   if (cla%flags(FLAG_HELP) .and. cla%flag_idx(FLAG_HELP) == 1) then
      call print_help
      stop
   else if (cla%arg_info%argc == 1) then
      select case (cla%arg_info%arg(1)%v)
      case (CMD_DEBUG)
         call print_help_debug
      case (CMD_FIND)
         call print_help_find
      case default
         call print_help
      end select
   end if

   ! Read subcommand. 
   call cla%read_cmd()

   ! Branch by specified subcommand.
   select case (cla%cmd%get_name())
   case (CMD_DEBUG)
      call cla%do_debug
   case (CMD_FIND)
      call cla%do_find
   case ('')
      call print_help
   end select

end program forgex_cli