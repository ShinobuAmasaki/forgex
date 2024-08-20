! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!    `forgex_syntax_tree_m` module is a part of Forgex.
!
!! This file defines syntactic parsing.

!> The`forgex_syntax_tree_m` module defines parsing and
!> the `tree_node_t` derived-type for building syntax-tree.
!>
#ifdef IMPURE
#define pure
#endif
module forgex_syntax_tree_node_m
   use, intrinsic :: iso_fortran_env, stderr => error_unit
   use :: forgex_parameters_m
   use :: forgex_segment_m, only: segment_t
   use :: forgex_enums_m
   implicit none
   private

   public :: tree_node_t
   public :: tape_t
   public :: make_atom
   public :: make_tree_node
   public :: make_repeat_node

   !! The regular expression parsing performed by this module
   !! is done using recursive descent parsing.

   character(UTF8_CHAR_SIZE), parameter, public :: EMPTY = char(0)

   type :: tree_node_t
      !! This type is used to construct a concrete syntax tree,
      !! later converted to NFA.
      integer(int32) :: op            = op_not_init
      type(segment_t), allocatable :: c(:)
      integer(int32) :: left_i        = INVALID_INDEX
      integer(int32) :: right_i       = INVALID_INDEX
      integer(int32) :: parent_i      = INVALID_INDEX
      integer(int32) :: own_i         = INVALID_INDEX
      integer(int32) :: min_repeat
      integer(int32) :: max_repeat
      logical        :: is_registered = .false.
   end type

   type :: tape_t
      !! This type holds the input pattern string and manages the index
      !! of the character it is currently focused.
      character(:), allocatable :: str
         ! Contains the entire input pattern string
      integer(int32) :: current_token
         ! token enumerator (cf. enums_m.f90)
      character(UTF8_CHAR_SIZE) :: token_char = EMPTY
         ! initialized as ASCII character number 0
      integer(int32) :: idx = 0
         ! index of the character that is currently focused
   contains
      procedure :: get_token
   end type

   type(tree_node_t), parameter, public :: terminal = &
      tree_node_t( op=op_not_init,&
                   left_i=TERMINAL_INDEX, &
                   right_i=TERMINAL_INDEX, &
                   parent_i=INVALID_INDEX, &
                   own_i=INVALID_INDEX, &
                   min_repeat=INVALID_REPEAT_VAL, &
                   max_repeat=INVALID_REPEAT_VAL)

contains


   pure subroutine reallocate_tree(tree, alloc_count)
      implicit none
      type(tree_node_t), allocatable, intent(inout) :: tree(:)
      integer,                    intent(inout)    :: alloc_count
      
      type(tree_node_t), allocatable  :: tmp(:)
      integer                     :: new_part_begin, new_part_end, i

      if (.not. allocated(tree)) then
         allocate(tree(TREE_NODE_BASE:TREE_NODE_UNIT))
         alloc_count = 1
         return
      end if

      new_part_begin = ubound(tree, dim=1) + 1
      new_part_end   = ubound(tree, dim=1)*2

      if (new_part_end > TREE_NODE_HARD_LIMIT) then
         error stop "Exceeded the maximum number of tree nodes can be allocated."
      end if

      call move_alloc(tree, tmp)

      allocate(tree(TREE_NODE_BASE:new_part_end))
      alloc_count = alloc_count + 1

      ! Deep copy
      tree(TREE_NODE_BASE:new_part_begin-1) = tmp(TREE_NODE_BASE:new_part_begin-1)
      
      ! Initialize new part
      tree(new_part_begin:new_part_end)%own_i = [(i, i = new_part_begin, new_part_end)]

      ! deallocate old tree
      deallocate(tmp)
   end subroutine reallocate_tree


   !> This subroutine deallocate the syntax tree.
   pure subroutine deallocate_tree(tree)
      implicit none
      type(tree_node_t), allocatable, intent(inout) :: tree(:)

      integer :: i

      do i = lbound(tree, dim=1), ubound(tree, dim=1)
         if (allocated(tree(i)%c)) deallocate(tree(i)%c)
      end do

      if (allocated(tree)) deallocate(tree)
   end subroutine deallocate_tree

   !| Get the currently focused character (1 to 4 bytes) from the entire string inside
   !  the `type_t` derived-type, and store the enumerator's numeric value in the
   !  `current_token` component.
   !  This is a type-bound procedure of `tape_t`.
   pure subroutine get_token(self, class_flag)
      use :: forgex_utf8_m, only: idxutf8
      implicit none
      class(tape_t),     intent(inout) :: self
      logical, optional, intent(in)    :: class_flag

      character(UTF8_CHAR_SIZE) :: c
      integer(int32)            :: ib, ie

      ib = self%idx
      if (ib > len(self%str)) then
         self%current_token = tk_end
         self%token_char = ''
      else

         ie = idxutf8(self%str, ib)

         c = self%str(ib:ie)

         if (present(class_flag)) then
            if (class_flag) then
               select case (trim(c))
               case (SYMBOL_RSBK)
                  self%current_token = tk_rsbracket
               case (SYMBOL_HYPN)
                  self%current_token = tk_hyphen
                  self%token_char = c
               case default
                  self%current_token = tk_char
                  self%token_char = c
               end select
            end if
         else
            select case(trim(c))
            case (SYMBOL_VBAR)
               self%current_token = tk_union
            case (SYMBOL_LPAR)
               self%current_token = tk_lpar
            case (SYMBOL_RPAR)
               self%current_token = tk_rpar
            case (SYMBOL_STAR)
               self%current_token = tk_star
            case (SYMBOL_PLUS)
               self%current_token = tk_plus
            case (SYMBOL_QUES)
               self%current_token = tk_question
            case (SYMBOL_BSLH)
               self%current_token = tk_backslash

               ib = ie +1
               ie = idxutf8(self%str, ib)

               self%token_char = self%str(ib:ie)
            case (SYMBOL_LSBK)
               self%current_token = tk_lsbracket
            case (SYMBOL_RSBK)
               self%current_token = tk_rsbracket
            case (SYMBOL_LCRB)
               self%current_token = tk_lcurlybrace
            case (SYMBOL_RCRB)
               self%current_token = tk_rcurlybrace
            case (SYMBOL_DOT)
               self%current_token = tk_dot
            case (SYMBOL_CRET)
               self%current_token = tk_caret
            case (SYMBOL_DOLL)
               self%current_token = tk_dollar
            case default
               self%current_token = tk_char
               self%token_char = c
            end select
         end if

         self%idx = ie + 1

      end if
   end subroutine get_token

!=====================================================================!

   pure function make_tree_node(op) result(node)
      implicit none
      integer(int32), intent(in) :: op
      type(tree_node_t) :: node

      node%op = op
   end function make_tree_node


   pure function make_atom(segment) result(node)
      implicit none
      type(segment_t), intent(in) :: segment
      type(tree_node_t) :: node

      node%op = op_char
      allocate(node%c(1))
      node%c = segment
    end function


    pure function make_repeat_node(min, max) result(node)
      implicit none
      integer(int32), intent(in) :: min, max
      type(tree_node_t) :: node

      node%op = op_repeat
      node%min_repeat = min
      node%max_repeat = max
   end function make_repeat_node

end module forgex_syntax_tree_node_m
