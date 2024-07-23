module forgex_parameters_m
   use, intrinsic :: iso_fortran_env, only: int32
   implicit none

   !> The maximum size of nodes for building the syntax tree.
   integer(int32), parameter :: TREE_NODE_LIMIT = 16
   ! integer(int32), parameter :: TREE_NODE_LIMIT = 1024

   !> Upper limit of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_MAX = 1024

   !> Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_VECTOR_SIZE = NFA_STATE_MAX

   !> Upper limit of DFA state instance. 
   integer(int32), parameter, public :: DFA_STATE_MAX = 1024
   
   
   ! For handling UTF-8
   integer, parameter, public :: UTF8_CODE_MAX     = 2**21-1 !
   integer, parameter, public :: UTF8_CODE_MIN     = 32 ! = 0x21: '!'
   integer, parameter, public :: UTF8_CODE_EMPTY   = 0
   integer, parameter, public :: UTF8_CODE_INVALID = -1
   integer, parameter, public :: UTF8_CHAR_SIZE    = 4

   ! For syntax-tree building
   character(1), parameter, public :: SYMBOL_VBAR = '|'  ! vartical bar
   character(1), parameter, public :: SYMBOL_LPAR = '('  ! left parentheses
   character(1), parameter, public :: SYMBOL_RPAR = ')'  ! right parentheses
   character(1), parameter, public :: SYMBOL_STAR = '*'  ! asterisk
   character(1), parameter, public :: SYMBOL_PLUS = '+'  ! plus
   character(1), parameter, public :: SYMBOL_QUES = '?'  ! question
   character(1), parameter, public :: SYMBOL_BSLH = '\'  ! backslash
   character(1), parameter, public :: SYMBOL_LSBK = '['  ! left square bracket
   character(1), parameter, public :: SYMBOL_RSBK = ']'  ! right square bracket
   character(1), parameter, public :: SYMBOL_LCRB = '{'  ! left curly brace
   character(1), parameter, public :: SYMBOL_RCRB = '}'  ! right curly brace
   character(1), parameter, public :: SYMBOL_DOLL = '$'  ! doller
   character(1), parameter, public :: SYMBOL_CRET = '^'  ! caret
   character(1), parameter, public :: SYMBOL_DOT  = '.'  ! dot
   character(1), parameter, public :: SYMBOL_HYPN = '-'  ! hyphen

   integer, parameter, public :: INVALID_DATA      = -1
   integer, parameter, public :: INVALID_INDEX     = -1
   integer, parameter, public :: INITIALIZED_INDEX = 0

end module forgex_parameters_m