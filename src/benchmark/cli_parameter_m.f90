module forgex_cli_parameters_m
   implicit none
   private

   integer, parameter, public :: NUM_FLAGS = 3
   integer, parameter, public :: NUM_SUB_C = 1
   integer, parameter, public :: NUM_DIGIT_TIME = 13
   integer, parameter, public :: NUM_DIGIT_KEY = 32
   integer, parameter, public :: LEN_ENV_VAR = 255

   integer, parameter, public :: TREE_BUFF_LEN = 2**16

   integer, parameter, public :: NUM_SUBC_DEBUG = 2
   character(*), parameter, public :: SUBC_DEBUG = "debug"
   character(*), parameter, public :: SUB_SUBC_AST = "ast"
   character(*), parameter, public :: SUB_SUBC_THOMPSON = "thompson"

   character(*), parameter, public :: INVALID_FLAG = "INVALID"

   character(*), parameter, public :: fmt_out_int = "(a, i8)"
   character(*), parameter, public :: fmta = "(a)"


   character(*), parameter, public :: CRLF = char(13)//char(10)
   character(*), parameter, public :: LF = char(10)

end module forgex_cli_parameters_m