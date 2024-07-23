! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!    `forgex_syntax_tree_m` module is a part of Forgex.
!
!! This file defines syntactic parsing.

module forgex_syntax_tree_m
   use, intrinsic :: iso_fortran_env
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_enums_m
   implicit none
   private

   public :: tree_node_t
   public :: tape_t
   
   public :: build_syntax_tree

   character(UTF8_CHAR_SIZE), parameter, public :: EMPTY = char(0)

   type :: tree_node_t
      integer(int32) :: op            = op_not_init
      type(segment_t), allocatable :: c(:)
      integer(int32) :: left_i        = INVALID_INDEX
      integer(int32) :: right_i       = INVALID_INDEX
      integer(int32) :: parent_i      = INVALID_INDEX
      integer(int32) :: own_i         = INVALID_INDEX
      logical        :: is_registered = .false.
   contains
      procedure :: register_node_root
      procedure :: register_node_right
      procedure :: register_node_left
   end type

   type :: tape_t
      character(:), allocatable :: str
      integer(int32) :: current_token
      character(UTF8_CHAR_SIZE) :: token_char = EMPTY
      integer(int32) :: idx = 0
   contains
      procedure :: get_token
   end type

contains

   !> Copies the input pattern to `tape_t` type and builds a syntax tree.
   !> The result returns a pointer to the root of the tree.
   !> Expected to be used by the forgex module.
   pure subroutine build_syntax_tree(str, tape, tree, tree_idx)
      implicit none
      character(*), intent(in)    :: str
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), allocatable, intent(inout) :: tree(:)
      integer(int32), intent(inout) :: tree_idx
      
      integer :: i

      allocate(tree(0:TREE_NODE_LIMIT)) ! 0-based
      tree(0:TREE_NODE_LIMIT)%own_i = [(i, i = 0, TREE_NODE_LIMIT)]

      tape%idx = 1
      tape%str = str

      call tape%get_token()

      call regex(tape, tree, tree_idx)

   end subroutine build_syntax_tree

   
   pure subroutine deallocate_tree(tree)
      implicit none
      type(tree_node_t), allocatable, intent(inout) :: tree(:)
      integer :: i

      do i = lbound(tree, dim=1), ubound(tree, dim=1)
         if (allocated(tree(i)%c)) deallocate(tree(i)%c)
      end do

      if (allocated(tree)) deallocate(tree)
   end subroutine deallocate_tree


   pure subroutine get_token(self, class_flag)
      use :: forgex_enums_m
      use :: forgex_utf8_m
      implicit none
      class(tape_t), intent(inout) :: self
      logical, optional, intent(in) :: class_flag 

      character(UTF8_CHAR_SIZE) :: c
      integer(int32) :: ib, ie

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

   pure function make_tree_node(op, parent, left, right) result(node)
      implicit none
      integer(int32), intent(in) :: op
      integer(int32), intent(in) :: parent
      integer(int32), intent(in) :: left, right
      type(tree_node_t) :: node

      node%op = op
      node%parent_i = parent 
      node%left_i = left
      node%right_i = right

   end function make_tree_node


   pure function make_atom(segment) result(node)
      use :: forgex_enums_m
      implicit none
      type(segment_t), intent(in) :: segment
      type(tree_node_t) :: node

      node%op = op_char
      allocate(node%c(1))
      node%c  = segment
   end function make_atom
      

   pure subroutine register_node_root(self, tree, root_index)
      implicit none
      class(tree_node_t), intent(in) :: self
      type(tree_node_t),  intent(inout) :: tree(0:TREE_NODE_LIMIT)
      integer(int32), intent(in) :: root_index

      integer :: i
      i = root_index
      tree(i)%c             = self%c
      tree(i)%left_i        = self%left_i
      tree(i)%right_i       = self%right_i
      tree(i)%parent_i      = self%parent_i
      tree(i)%is_registered = .true.
   end subroutine register_node_root


   pure subroutine register_node_left(self, tree, node, count)
      implicit none
      class(tree_node_t), intent(inout) :: self
      type(tree_node_t),  intent(inout) :: tree(0:TREE_NODE_LIMIT)
      type(tree_node_t),  intent(in)    :: node
      integer(int32),     intent(inout) :: count
      
      self%op       = node%op

      if (.not. allocated(self%c)) then
         allocate(self%c(1))
      end if
      if (allocated(node%c)) self%c = node%c

      self%left_i = node%left_i
      self%right_i = node%right_i
      self%parent_i = node%parent_i
      self%is_registered = .true.

      if (node%parent_i > 0) then
         tree(self%parent_i)%left_i = self%own_i
      end if 
      count = count + 1
   end subroutine register_node_left


   pure subroutine register_node_right(self, tree, node, count)
      implicit none
      class(tree_node_t), intent(inout) :: self
      type(tree_node_t),  intent(inout) :: tree(0:TREE_NODE_LIMIT)
      type(tree_node_t),  intent(in)    :: node
      integer(int32),     intent(inout) :: count

      self%op       = node%op

      if (.not. allocated(self%c)) then
         allocate(self%c(1))
      end if
      if (allocated(node%c)) self%c = node%c

      self%left_i   = node%left_i
      self%right_i  = node%right_i
      self%parent_i = node%parent_i
      self%is_registered = .true.

      tree(self%parent_i)%right_i = self%own_i
       
      count = count + 1
   end subroutine register_node_right

!=====================================================================!

   pure subroutine regex(tape, tree, tree_idx)
      use :: forgex_enums_m
      implicit none
      type(tape_t),      intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(0:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: tree_idx

      type(tree_node_t) :: node, curr, prev 

      integer :: i

      i = tree_idx

      call term(tape, tree, tree_idx)
      prev = tree(i)

      do while (tape%current_token == tk_union)
         call tape%get_token()
         call tree(tree_idx)%register_node_left(tree, prev, tree_idx)

         call term(tape, tree, tree_idx)
         curr = make_tree_node(op_union, i, INVALID_INDEX, INVALID_INDEX)
         call tree(i)%register_node_right(tree, curr, tree_idx)
         
      end do

   end subroutine regex

   pure subroutine term(tape, tree, tree_idx)
      use :: forgex_enums_m
      use :: forgex_utf8_m
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(0:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: tree_idx

      type(tree_node_t) :: node, curr
      type(segment_t) :: seg
      integer :: i
      
      i = tree_idx

      curr = tree(i)

      if (tape%current_token == tk_union &
          .or. tape%current_token == tk_rpar &
          .or. tape%current_token == tk_end) then
         node = make_tree_node(op_empty, i, INVALID_INDEX, INVALID_INDEX)
         call tree(i)%register_node_left(tree, node, tree_idx)
         call tree(i)%register_node_right(tree, node, tree_idx)

      else if (tape%current_token == tk_char) then
         seg = segment_t(ichar_utf8(tape%token_char), ichar_utf8(tape%token_char))
         node = make_atom(seg)
         call tree(i)%register_node_left(tree, node, tree_idx)
         call tape%get_token()
      end if

   end subroutine term


end module forgex_syntax_tree_m
