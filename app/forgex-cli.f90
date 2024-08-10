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
#if defined(IMPURE) && defined(DEBUG)
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
   use :: forgex_cli_parameters_m
   use :: forgex, only: operator(.match.), operator(.in.)
   use :: forgex_enums_m, only: FLAG_HELP
   use :: forgex_cli_m
   implicit none

   type(cla_t) :: cla

   call cla%init

   call cla%collect_flags

   if (cla%flags(FLAG_HELP) .and. cla%flag_idx(FLAG_HELP) == 1) then
      call print_help_message
   end if

   call cla%read_sub

   select case (cla%subc%name)
   case (SUBC_DEBUG)
      call cla%do_debug
   case ('')
      call print_help_message
   end select


#else
   implicit none
   call print_fpm_message

#endif
contains

   subroutine print_fpm_message
      implicit none
      print *, "forgex-cli is a simple benchmark program. To use this, you need to"
      print *, "build the program with IMPURE and DEBUG macros enabled."
      print *, 'Specifically, specify "-DIMPURE -DDEBUG" for the --flag of the fpm'
      print *, "command, or change [preprocess] in fpm.toml as follows:"
      print *, "..."
      print *, "[preprocess]"
      print *, "[preprocess.cpp]"
      print *, 'macros = ["IMPURE", "DEBUG"]'
      print *, ""
   end subroutine print_fpm_message

end program forgex_cli