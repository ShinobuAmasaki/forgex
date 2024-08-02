module forgex_parameters_m
   use, intrinsic :: iso_fortran_env, only: int32
   implicit none

   integer(int32), parameter :: TREE_NODE_UNIT = 256
      ! If it's too large it will cause a stack overflow.
   integer(int32), parameter :: TREE_NODE_BASE = 1

   !> The initial maximum size of nodes for building the syntax tree.
   integer(int32), parameter :: TREE_NODE_LIMIT = TREE_NODE_UNIT
   !> The maximum value that can be allocated to a syntax tree graph;
   !> exceeding this will cause ERROR STOP.
   integer(int32), parameter :: TREE_NODE_HARD_LIMIT = TREE_NODE_UNIT*16

   ! For handling UTF-8
   integer(int32), parameter, public :: UTF8_CODE_MAX     = 2**21-1 !
   integer(int32), parameter, public :: UTF8_CODE_MIN     = 32 ! = 0x21: '!'
   integer(int32), parameter, public :: UTF8_CODE_EMPTY   = 0
   integer(int32), parameter, public :: UTF8_CODE_INVALID = -1
   integer(int32), parameter, public :: UTF8_CHAR_SIZE    = 4

   ! For syntax-tree building
   character(1),   parameter, public :: SYMBOL_VBAR = '|'  ! vartical bar
   character(1),   parameter, public :: SYMBOL_LPAR = '('  ! left parentheses
   character(1),   parameter, public :: SYMBOL_RPAR = ')'  ! right parentheses
   character(1),   parameter, public :: SYMBOL_STAR = '*'  ! asterisk
   character(1),   parameter, public :: SYMBOL_PLUS = '+'  ! plus
   character(1),   parameter, public :: SYMBOL_QUES = '?'  ! question
   character(1),   parameter, public :: SYMBOL_BSLH = '\'  ! backslash
   character(1),   parameter, public :: SYMBOL_LSBK = '['  ! left square bracket
   character(1),   parameter, public :: SYMBOL_RSBK = ']'  ! right square bracket
   character(1),   parameter, public :: SYMBOL_LCRB = '{'  ! left curly brace
   character(1),   parameter, public :: SYMBOL_RCRB = '}'  ! right curly brace
   character(1),   parameter, public :: SYMBOL_DOLL = '$'  ! doller
   character(1),   parameter, public :: SYMBOL_CRET = '^'  ! caret
   character(1),   parameter, public :: SYMBOL_DOT  = '.'  ! dot
   character(1),   parameter, public :: SYMBOL_HYPN = '-'  ! hyphen
   integer(int32), parameter, public :: INVALID_DATA   = -1
   integer(int32), parameter, public :: INVALID_INDEX  = -1
   integer(int32), parameter, public :: TERMINAL_INDEX = 0
   character(1),   parameter, public :: ESCAPE_T = 't'
   character(1),   parameter, public :: ESCAPE_N = 'n'
   character(1),   parameter, public :: ESCAPE_R = 'r'
   character(1),   parameter, public :: ESCAPE_D = 'd'
   character(1),   parameter, public :: ESCAPE_W = 'w'
   character(1),   parameter, public :: ESCAPE_S = 's'
   character(1),   parameter, public :: ESCAPE_D_CAPITAL = 'D'
   character(1),   parameter, public :: ESCAPE_W_CAPITAL = 'W'
   character(1),   parameter, public :: ESCAPE_S_CAPITAL = 'S'

   integer(int32), parameter, public :: ALLOC_COUNT_INITTIAL = 0

   ! For NFA building
   integer(int32), parameter, public :: NFA_NULL_TRANSITION = -1

   !> Lower end of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_BASE = 0
   !> Upper limit of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_LIMIT = 1024

   !> Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_TRANSITION_UNIT = 32

   !> Upper limit of segments size of NFA transition instance
   integer(int32), parameter, public :: NFA_C_SIZE = 32

   integer(int32), parameter, public :: ZERO_C_TOP = 0

   !> For DFA building
   integer(int32), parameter, public :: DFA_NULL_TRANSITION = -1
   !> For DFA transition
   integer(int32), parameter, public :: DFA_NOT_INIT = -1
   !> Lower limit of DFA state instanse.
   integer(int32), parameter, public :: DFA_STATE_BASE = 0
   !> Upper limit of DFA state instance.

   integer(int32), parameter, public :: DFA_STATE_UNIT = 128
   integer(int32), parameter, public :: DFA_STATE_LIMIT = 1024

   !> If this limit is exceeded, program will do ERROR STOP.
   integer(int32), parameter, public :: DFA_STATE_HARD_LIMIT = DFA_STATE_LIMIT*16 + 1
   !! This hard limit is approximately on the order of gigabytes.


   integer(int32), parameter, public :: DFA_INVALID_INDEX = 0
   integer(int32), parameter, public :: DFA_INITIAL_INDEX = 1
   !>
   integer(int32), parameter, public :: DFA_TRANSITION_UNIT = 32
   integer(int32), parameter, public :: DFA_C_SIZE = 32
   integer(int32), parameter, public :: DFA_NOT_INIT_TRAENSITION_TOP = -999
   integer(int32), parameter, public :: DFA_INIT_TRANSITION_TOP = 0

end module forgex_parameters_m