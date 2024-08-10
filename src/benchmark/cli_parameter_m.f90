module forgex_cli_parameters_m
   implicit none
   private

   integer, parameter, public :: NUM_FLAGS = 2
   integer, parameter, public :: NUM_SUB_C = 1
   integer, parameter, public :: NUM_DIGIT_TIME = 13
   integer, parameter, public :: NUM_DIGIT_KEY = 32

   integer, parameter, public :: TREE_BUFF_LEN = 2**16

   integer, parameter, public :: NUM_SUBC_DEBUG = 2
   character(*), parameter, public :: SUBC_DEBUG = "debug"
   character(*), parameter, public :: SUB_SUBC_AST = "ast"
   character(*), parameter, public :: SUB_SUBC_THOMPSON = "thompson"

   character(*), parameter, public :: fmt_out_int = "(a, i8)"
   character(*), parameter, public :: fmta = "(a)"

#if defined(_WIN32) || defined(_WIN64)
   character(*), parameter, public :: newline = char(13)//char(10)
#else
   character(*), parameter, public :: newline = char(10)
#endif

end module forgex_cli_parameters_m