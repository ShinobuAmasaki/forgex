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
   use :: forgex, only: operator(.match.), operator(.in.)
   use :: forgex_cli_m
   implicit none

   type(cla_t) :: cla


   call cla%init
   call cla%read_sub


   if (cla%sub_command == 'debug') then
      call print_help_message_for_debug
   else
      call print_help_message
   end if


#else
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
#endif
contains
end program forgex_cli