! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_help_messages_m module is a part of Forgex.
!
module forgex_cli_help_messages_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit, int32
   use :: forgex_cli_parameters_m, only: fmta
   implicit none
   private
   
   public :: print_help
   public :: print_help_debug
   public :: print_help_debug_ast
   public :: print_help_debug_thompson
   public :: print_help_debug_lazy_dfa

   integer(int32), parameter :: LINE_SIZ = 128
   integer(int32), parameter :: CMD_SIZ = 13
   integer(int32), parameter :: CMD_DESC_SIZ = 113

contains

   subroutine generate_and_output(header, usage, choice, cmd, cmd_desc, desc)
      implicit none
      character(LINE_SIZ), intent(in) :: header
      character(LINE_SIZ), intent(in) :: usage(:)
      character(*), intent(in) :: choice
      character(CMD_SIZ), intent(in)  :: cmd(:)     ! command
      character(CMD_DESC_SIZ), intent(in) :: cmd_desc(:)      ! description

      character(LINE_SIZ), intent(in), optional :: desc(:)

      character(LINE_SIZ), allocatable :: buff(:)
      integer :: num_line, i, offset

      num_line = 3 + size(desc) + size(usage) + 2 + size(cmd)
      ! header + blank + DESC + blank+ USAGE + size(usage) + blank + COMMANDS + size(cmd)
      allocate(buff(num_line))
      buff(:) = ""

      buff(1) = header
      ! buff(2) blank
      offset = 2
      do i = 1, size(desc)
         buff(i+offset) = desc(i)
      end do
      
      offset = offset + size(desc)
      buff(offset+1) = "USAGE:"

      offset = offset + 1
      do i = 1, size(usage)
         buff(i+offset) = "   "//usage(i)
      end do

      offset = offset + size(usage)
      buff(offset+2) = trim(choice)//":"

      offset = offset + 2
      do i = 1, size(cmd)
         buff(i+offset) = "   "//cmd(i)//" "//cmd_desc(i)
      enddo

      do i = 1, num_line
         write(stderr, fmta) buff(i)
      end do
      
      stop
   end subroutine generate_and_output


   subroutine print_help
      implicit none
      character(LINE_SIZ) :: header
      character(LINE_SIZ) :: usage(1)
      character(CMD_SIZ) :: cmd(2)
      character(CMD_DESC_SIZ) :: cdesc(2)

      header   = "A tool for interacting with Forgex on the command line."
      usage(1) = "forgex-cli <command> ..."
      
      cmd(1)   = "debug"
      cdesc(1)  = "Print the debug representation from Forgex's regex engine."

      cmd(2)   = "find"
      cdesc(2)  = "Search for a string using one of the regular expression engines." 

      call generate_and_output(header, usage, "COMMANDS",  cmd, cdesc)

   end subroutine print_help


   subroutine  print_help_debug
      implicit none
      character(LINE_SIZ) :: header
      character(LINE_SIZ) :: usage(1)
      character(CMD_SIZ) :: cmd(2)
      character(CMD_DESC_SIZ) :: cdesc(2)

      header   = "Prints the debug representation provided by Forgex."
      usage(1) = "forgex-cli debug <command> ..."
       
      cmd(1)   = "ast"
      cdesc(1) = "Print the debug representation of an AST."
      cmd(2)   = "thompson"
      cdesc(2) = "Print the debug representation of a Thompson NFA."
      
      call generate_and_output(header, usage, "COMMANDS", cmd, cdesc)
   end subroutine print_help_debug


!=====================================================================!
   subroutine print_help_debug_ast
      implicit none
      character(LINE_SIZ) :: header
      character(LINE_SIZ) :: usage(1)
      character(CMD_SIZ) :: op(3)
      character(CMD_DESC_SIZ) :: odesc(3)

      header = "Print the debug representation of an abstract syntax tree (AST)."
      usage(1) = "forgex-cli debug ast <pattern>"
      op(1)   ="--verbose"
      odesc(1) = "Print more information."
      op(2)   = "--no-table"
      odesc(2) = "Passing this flag suppresses the output of the property information table."
      op(3)   = "--table-only"
      odesc(3) ="Print the property information table only."

      call generate_and_output(header, usage, "OPTIONS", op, odesc)
   end subroutine

   subroutine print_help_debug_thompson
      implicit none
      character(LINE_SIZ) :: header
      character(LINE_SIZ) :: usage(1)
      character(CMD_SIZ) :: op(3)
      character(CMD_DESC_SIZ) :: odesc(3)

      header = "Print the debug representaion of a Thompson NFA."
      usage(1) = "forgex-cli debug thompson <pattern>"

      op(1)    = "--verbose"
      odesc(1) ="Print more information."
      op(2)    ="--no-table "
      odesc(2) ="Suppresses the output of the property information table."
      op(3)    ="--table-only "
      odesc(3) ="Print the property information table only."

      call generate_and_output(header, usage, "OPTIONS", op, odesc)
   end subroutine print_help_debug_thompson

   ! subroutine print_help_find_match_lazy_dfa
   subroutine print_help_debug_lazy_dfa
      implicit none
      character(LINE_SIZ) :: header
      character(LINE_SIZ) :: usage(2)
      character(CMD_SIZ) :: op(3)
      character(CMD_DESC_SIZ) :: odesc(3)
      header = "Do matching and print the debug representation of a lazy DFA."
      usage(1) = "forgex-cli debug lazy-dfa <pattern> .match. <text>"
      usage(2) = "forgex-cli debug lazy-dfa <pattern> .in. <text>"

      op(1)    = "--verbose"
      odesc(1) = "Print more information."
      op(2)    = "--no-table"
      odesc(2) = "Suppresses the output of the property information table."
      op(3)    = "--table-only"
      odesc(3) = "Print the property information table only. "

      call generate_and_output(header, usage, "OPTIONS", op, odesc)
   end subroutine print_help_debug_lazy_dfa
   !end subroutine print_help_find_match_lazy_dfa
   
end module forgex_cli_help_messages_m