module forgex_cli_parameters_m
   implicit none
   private

   integer, parameter, public :: NUM_FLAGS = 1
   integer, parameter, public :: NUM_SUB_C = 1
   integer, parameter, public :: NUM_DIGIT_TIME = 13

   integer, parameter, public :: TREE_BUFF_LEN = 2**16

   integer, parameter, public :: NUM_SUBC_DEBUG = 2
   character(*), parameter, public :: SUBC_DEBUG = "debug"
   character(*), parameter, public :: SUB_SUBC_AST = "ast"
   character(*), parameter, public :: SUB_SUBC_THOMPSON = "thompson"

end module forgex_cli_parameters_m