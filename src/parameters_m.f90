! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_parameters_m module is a part of Forgex.
module forgex_parameters_m
   use, intrinsic :: iso_fortran_env, only: int32
   implicit none

   !> This constant defines the unit for adding nodes in the abstract syntax tree (AST).
   !> If it's too large it will cause a stack overflow.
   integer(int32), parameter :: TREE_NODE_UNIT = 32

   !> This constant defines the lower bound of the array that represents AST.
   integer(int32), parameter :: TREE_NODE_BASE = 1

   !> The initial maximum size of nodes for building AST.
   integer(int32), parameter :: TREE_NODE_LIMIT = TREE_NODE_UNIT*64 ! 32x64 = 2048 elements

   !> The maximum value that can be allocated to a syntax tree graph;
   !> exceeding this will cause ERROR STOP.
   integer(int32), parameter :: TREE_NODE_HARD_LIMIT = TREE_NODE_LIMIT

   integer(int32), parameter :: LIT_OPTS_INDEX_UNIT = 32

   integer(int32), parameter :: INVALID_REPEAT_VAL = -1
   integer(int32), parameter :: INFINITE = -2
   integer, parameter, public :: INVALID_CHAR_INDEX = -1

   ! For handling UTF-8
   integer(int32), parameter, public :: UTF8_CODE_MAX     = 2**21-1 !
   integer(int32), parameter, public :: UTF8_CODE_MIN     = 32 ! = 0x20: white space
   integer(int32), parameter, public :: UTF8_CODE_EMPTY   = 0
   integer(int32), parameter, public :: UTF8_CODE_INVALID = -1
   integer(int32), parameter, public :: UTF8_CHAR_SIZE    = 4

   ! These character constants represent characters that have special
   ! meaning in regular expression parsing.
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
   character(1), parameter, public :: ESCAPE_T = 't'
   character(1), parameter, public :: ESCAPE_N = 'n'
   character(1), parameter, public :: ESCAPE_R = 'r'
   character(1), parameter, public :: ESCAPE_D = 'd'
   character(1), parameter, public :: ESCAPE_W = 'w'
   character(1), parameter, public :: ESCAPE_S = 's'
   character(1), parameter, public :: ESCAPE_D_CAPITAL = 'D'
   character(1), parameter, public :: ESCAPE_W_CAPITAL = 'W'
   character(1), parameter, public :: ESCAPE_S_CAPITAL = 'S'

   !> This constant is used to indicate that the left and right destination
   !> have not yet been registered.
   integer(int32), parameter, public :: INVALID_INDEX  = -1

   !> This constant is used to represent a terminal node in a syntax tree that
   !> has no destination nodes to the left or right.
   integer(int32), parameter, public :: TERMINAL_INDEX = 0

   !> This constant is used as the initial value when the derived-type
   !> manages the number of allocations.
   integer(int32), parameter, public :: ALLOC_COUNT_INITTIAL = 0

   !> This constant represents the destinationless transition of
   !> an non-deterministic finite automaton (NFA) construction.
   integer(int32), parameter, public :: NFA_NULL_TRANSITION = -1

   !> Lower end of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_BASE = 1

   !> This constant defines the unit of  reallocation for the array representing a NFA graph.
   integer(int32), parameter, public :: NFA_STATE_UNIT = 16

   !> Upper limit of NFA state nodes
   integer(int32), parameter, public :: NFA_STATE_LIMIT = 1024+1

   !> Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_TRANSITION_UNIT = 16

   !> Upper limit of segments size of NFA transition instance
   integer(int32), parameter, public :: NFA_C_SIZE = 16

   integer(int32), parameter, public :: ZERO_C_TOP = 0

   !> This constant represents the destinationless transition of
   !> a deterministic finite automaton (DFA) construction.
   integer(int32), parameter, public :: DFA_NULL_TRANSITION = -1

   !> This constant represents an uninitialized index of a DFA node.
   integer(int32), parameter, public :: DFA_NOT_INIT = -1

   !> Lower bound of the array represents an DFA.
   integer(int32), parameter, public :: DFA_STATE_BASE = 0

   !> This constant defines the unit of reallocation for the array representing
   !> a DFA graph.
   integer(int32), parameter, public :: DFA_STATE_UNIT = 16

   !> This constant is provided to define the upper limit of DFA nodes,
   !> but is currently only used to define DFA_STATE_HARD_LIMIT.
   integer(int32), parameter, public :: DFA_STATE_LIMIT = 1024*16 +1

   !> If this limit is exceeded, program will do ERROR STOP.
   !> This hard limit is approximately on the order of gigabytes.
   integer(int32), parameter, public :: DFA_STATE_HARD_LIMIT = DFA_STATE_LIMIT

   !> This constant is used for the purpose of determining invalid DFA index.
   integer(int32), parameter, public :: DFA_INVALID_INDEX = 0

   !> This cosntant is used to initialize the current top index of the array
   !> representing the DFA graph.
   integer(int32), parameter, public :: DFA_INITIAL_INDEX = 1

   !> This constant defines the lower bound of the array that represents
   !> the DFA transitions.
   integer(int32), parameter, public :: DFA_TRANSITION_BASE = 1

   !> This constant defines the unit of additional allocation for DFA transitions.
   integer(int32), parameter, public :: DFA_TRANSITION_UNIT = 32

   !> This constant is used to represent that the array of DFA transitions
   !> has not yet been initialized.
   integer(int32), parameter, public :: DFA_NOT_INIT_TRAENSITION_TOP = -999

   !> This constant is used to represent that the array of DFA transitions
   !> has been initialized.
   integer(int32), parameter, public :: DFA_INIT_TRANSITION_TOP = 0

   integer(int32), parameter, public :: ACCEPTED_EMPTY = -2

end module forgex_parameters_m