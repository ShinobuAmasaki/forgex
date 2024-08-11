! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_help_messages_m module is a part of Forgex.
!
module forgex_cli_help_messages_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
   use :: forgex_cli_parameters_m, only: fmta
   implicit none
   private
   
   public :: print_help_message
   public :: print_help_message_for_debug
   public :: print_help_debug_ast
   public :: print_help_debug_thompson
   public :: print_help_debug_lazy_dfa

contains

   subroutine print_help_message
      implicit none
      write(stderr, fmta) "A tool for interacting with Forgex on the command line."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli <command> ..."
      write(stderr, fmta) ""
      write(stderr, fmta) "COMMANDS:"
      write(stderr, fmta) "   debug   Print the debug representations from Forgex's regex engine."
      stop
   end subroutine print_help_message


   subroutine  print_help_message_for_debug
      write(stderr, fmta) "Prints the debug representation Forgex provides."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli debug <command> ..."
      write(stderr, fmta) ""
      write(stderr, fmta) "COMMANDS:"
      write(stderr, fmta) "   ast        Print the debug representation of an AST."
      write(stderr, fmta) "   thompson   Print the debug representation of a Thompson NFA."
   end subroutine print_help_message_for_debug


!=====================================================================!
   subroutine print_help_debug_ast
      implicit none
      write(stderr, fmta) "Print the debug representation of an abstract syntax tree (AST)."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli debug ast <pattern>"
      write(stderr, fmta) ""
      write(stderr, fmta) "OPTIONS:"
      write(stderr, fmta) "   --verbose      Print more information."
      write(stderr, fmta) "   --no-table     Passing this flag suppresses the output of the property information table."
      write(stderr, fmta) "   --table-only   Print the property information table only."
      stop
   end subroutine

   subroutine print_help_debug_thompson
      implicit none
      write(stderr, fmta) "Print the debug representaion of a Thompson NFA."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli debug thompson <pattern>"
      write(stderr, fmta) ""
      write(stderr, fmta) "OPTIONS:"
      write(stderr, fmta) "   --verbose      Print more information."
      write(stderr, fmta) "   --no-table     Suppresses the output of the property information table."
      write(stderr, fmta) "   --table-only   Print the property information table only."
      stop
   end subroutine print_help_debug_thompson

   subroutine print_help_debug_lazy_dfa
      implicit none
      write(stderr, fmta) "Do matching and print the debug representation of a lazy DFA."
      write(stderr, fmta) ""
      write(stderr, fmta) "USAGE:"
      write(stderr, fmta) "   forgex-cli debug lazy-dfa <pattern> .match. <text>"
      write(stderr, fmta) "   forgex-cli debug lazy-dfa <pattern> .in. <text>"
      write(stderr, fmta) ""
      write(stderr, fmta) "OPTIONS:"
      write(stderr, fmta) "   --verbose      Print more information."
      write(stderr, fmta) "   --no-table     Suppresses the output of the property information table."
      write(stderr, fmta) "   --table-only   Print the property information table only. "
      stop
   end subroutine print_help_debug_lazy_dfa
   
end module forgex_cli_help_messages_m