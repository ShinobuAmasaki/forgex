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
program forgex_cli
   use :: forgex_enums_m, only: FLAG_HELP
   use :: forgex_cli_parameters_m, only: SUBC_DEBUG
   use :: forgex_cli_cla_m, only: cla_t, print_help_message
   implicit none

   type(cla_t) :: cla

   call cla%init()

   call cla%collect_flags()

   if (cla%flags(FLAG_HELP) .and. cla%flag_idx(FLAG_HELP) == 1) then
      call print_help_message
   end if

   call cla%read_sub()

   select case (cla%subc%name)
   case (SUBC_DEBUG)
      call cla%do_debug
   case ('')
      call print_help_message
   end select

end program forgex_cli