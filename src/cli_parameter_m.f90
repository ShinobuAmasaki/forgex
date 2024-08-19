! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_parameter_m module is a part of Forgex.
!
module forgex_cli_parameters_m
   implicit none
   private

   !> Number of flags (without value) that forgex-cli accepts.
   integer, parameter, public :: NUM_FLAGS = 5

   !> Number of sub-command that forgec-cli accepts.
   integer, parameter, public :: NUM_CMD = 2

   !> Length
   integer, parameter, public :: LEN_CMD = 16

   !> Number of digits for time display.
   integer, parameter, public :: NUM_DIGIT_TIME = 13

   !> Maximum langth of table field name.
   integer, parameter, public :: NUM_DIGIT_KEY = 32

   !> Maximum length of an environment variable's value.
   integer, parameter, public :: LEN_ENV_VAR = 255

   !> The buffer length of displaying the AST.
   integer, parameter, public :: TREE_BUFF_LEN = 2**16

!---------------------------------------------------------------------!
   !> Name of the subcommand debug.
   character(*), parameter, public :: CMD_DEBUG = "debug"
   !> The number of sub-subcommands that debug accepts.
   integer, parameter, public :: NUM_SUBC_DEBUG = 2
   !> Name of the sub-subcommand ast.
   character(*), parameter, public :: SUBC_AST = "ast"
   !> Name of the sub-subcommand thompson.
   character(*), parameter, public :: SUBC_THOMPSON = "thompson"
!---------------------------------------------------------------------!
   !> Name of the subcommand find.
   character(*), parameter, public :: CMD_FIND = "find"
   integer, parameter, public :: NUM_SUBC_FIND = 1
   character(*), parameter, public :: SUBC_MATCH = "match"

   integer, parameter, public :: NUM_SUBSUBC_MATCH = 3
   character(*), parameter, public :: ENGINE_LAZY_DFA = "lazy-dfa"
   character(*), parameter, public :: ENGINE_DENSE_DFA = "dense"
   character(*), parameter, public :: ENGINE_FORGEX_API = "forgex"
!---------------------------------------------------------------------!

   !> Name of the sub-subcommand lazy dfa
   character(*), parameter, public :: OP_MATCH = ".match."
   character(*), parameter, public :: OP_IN = ".in."

   !> String to indicate invalidity if no short flag is present.
   character(*), parameter, public :: INVALID_FLAG = "INVALID"

   !> Output format for displaying an integer in tables.
   character(*), parameter, public :: fmt_out_int = "(a, i10)"
   character(*), parameter, public :: fmt_out_ratio = "(a, i10, '/', i0)"
   character(*), parameter, public :: fmt_out_char = "(a, 1x, a)"
   character(*), parameter, public :: fmt_out_time = "(a, a15)"
   character(*), parameter, public :: fmt_out_logi = "(a, l10)"

   character(*), parameter, public :: not_running = "not running"

   !> Format for outputting text only.
   character(*), parameter, public :: fmta = "(a)"

   !> Line ending characters for Windows OS
   character(*), parameter, public :: CRLF = char(13)//char(10)

   !> Line Feed.
   character(*), parameter, public :: LF = char(10)

   !> Headers
   character(*), parameter, public :: HEADER_NFA = "========== Thompson NFA ==========="
   character(*), parameter, public :: HEADER_DFA = "=============== DFA ==============="
   character(*), parameter, public :: FOOTER     = "==================================="

end module forgex_cli_parameters_m