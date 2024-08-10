module forgex_cli_parameters_m
   implicit none
   private

   !> Number of flags (without value) that forgex-cli accepts.
   integer, parameter, public :: NUM_FLAGS = 4

   !> Number of sub-command that forgec-cli accepts.
   integer, parameter, public :: NUM_SUB_C = 1

   !> Number of digits for time display.
   integer, parameter, public :: NUM_DIGIT_TIME = 13

   !> Maximum langth of table field name.
   integer, parameter, public :: NUM_DIGIT_KEY = 32

   !> Maximum length of an environment variable's value.
   integer, parameter, public :: LEN_ENV_VAR = 255

   !> The buffer length of displaying the AST.
   integer, parameter, public :: TREE_BUFF_LEN = 2**16

   !> Name of the subcommand debug.
   character(*), parameter, public :: SUBC_DEBUG = "debug"

   !> The number of sub-subcommands that debug accepts. 
   integer, parameter, public :: NUM_SUBC_DEBUG = 2
   
   !> Name of the sub-subcommand ast.
   character(*), parameter, public :: SUB_SUBC_AST = "ast"
   !> Name of the sub-subcommand thompson.
   character(*), parameter, public :: SUB_SUBC_THOMPSON = "thompson"

   !> String to indicate invalidity if no short flag is present. 
   character(*), parameter, public :: INVALID_FLAG = "INVALID"

   !> Output format for displaying an integer in tables.
   character(*), parameter, public :: fmt_out_int = "(a, i8)"

   !> Format for outputting text only.
   character(*), parameter, public :: fmta = "(a)"

   !> Line ending characters for Windows OS
   character(*), parameter, public :: CRLF = char(13)//char(10)
   
   !> Line Feed. 
   character(*), parameter, public :: LF = char(10)

end module forgex_cli_parameters_m