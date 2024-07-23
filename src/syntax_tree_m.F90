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
   use, intrinsic :: iso_fortran_env, stderr => error_unit
   use, intrinsic :: iso_c_binding
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_enums_m
   implicit none
   private

   public :: tree_node_t
   public :: tape_t
   
   public :: build_syntax_tree
   public :: deallocate_tree
   public :: print_tree

   interface
      pure subroutine message(i, j) bind(c)
         import c_int
         implicit none
         integer(c_int), intent(in), value :: i, j
      end subroutine message
   end interface


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
      procedure :: register_node
   end type

   type :: tape_t
      character(:), allocatable :: str
      integer(int32) :: current_token
      character(UTF8_CHAR_SIZE) :: token_char = EMPTY
      integer(int32) :: idx = 0
   contains
      procedure :: get_token
   end type

   type(tree_node_t), parameter :: terminal_node = &
      tree_node_t( op=op_not_init,&
                   left_i=TERMINAL_INDEX, &
                   right_i=TERMINAL_INDEX, &
                   parent_i=INVALID_INDEX, &
                   own_i=TERMINAL_INDEX)

contains

   !> Copies the input pattern to `tape_t` type and builds a syntax tree.
   !> The result returns a pointer to the root of the tree.
   !> Expected to be used by the forgex module.
   subroutine build_syntax_tree(str, tape, tree, top_idx)
      implicit none
      character(*), intent(in)    :: str
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), allocatable, intent(inout) :: tree(:)
      integer(int32), intent(inout) :: top_idx
      
      integer :: i

      allocate(tree(TREE_NODE_BASE:TREE_NODE_LIMIT)) ! 0-based
      tree(TREE_NODE_BASE:TREE_NODE_LIMIT)%own_i = [(i, i = TREE_NODE_BASE, TREE_NODE_LIMIT)]

      tape%idx = 1
      tape%str = str
      top_idx = 0

      call tape%get_token()

      call regex(tape, tree, top_idx)
      tree(top_idx)%parent_i = TERMINAL_INDEX

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

   pure function make_tree_node(op) result(node)
      implicit none
      integer(int32), intent(in) :: op
      type(tree_node_t) :: node

      node%op = op

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


   pure subroutine register_node(self, tree, top_index)
      implicit none
      class(tree_node_t), intent(inout) :: self
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top_index


      integer :: i
      i = top_index + 1

      tree(i)%op = self%op
      
      if (.not. allocated(self%c)) then
         if (.not. allocated(tree(i)%c)) then
            allocate(tree(i)%c(1))
            tree(i)%c = SEG_INVALID
         end if
      else
         if (.not. allocated(tree(i)%c)) then
            allocate(tree(i)%c(size(self%c, dim=1)))
            tree(i)%c = self%c
         end if
      end if

      tree(i)%parent_i = INVALID_INDEX
      tree(i)%right_i  = INVALID_INDEX
      tree(i)%left_i   = INVALID_INDEX
      tree(i)%is_registered = .true.

      self%own_i = i

      top_index = i
   end subroutine register_node


   pure subroutine register_and_connector(tree, top, node, node_l, node_r)
      implicit none
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top
      type(tree_node_t), intent(inout) :: node
      type(tree_node_t), intent(in) :: node_l, node_r
   
      call node%register_node(tree, top)
      node = tree(top)

      call connect_left(tree, node%own_i, node_l%own_i)
      call connect_right(tree, node%own_i, node_r%own_i)

   end subroutine register_and_connector


   pure subroutine connect_left(tree, parent_i, child_i)
      implicit none
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(in) :: parent_i, child_i

      tree(parent_i)%left_i = child_i
      if (child_i /= INVALID_INDEX) tree(child_i)%parent_i = parent_i
   end subroutine connect_left


   pure subroutine connect_right(tree, parent_i, child_i)
      implicit none
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(in) :: parent_i, child_i

      tree(parent_i)%right_i = child_i
      if (child_i /= INVALID_INDEX) tree(child_i)%parent_i = parent_i
   end subroutine connect_right

!=====================================================================!

   pure subroutine regex(tape, tree, top)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: node, node_l, node_r
   
      call term(tape, tree, top)
      node_l = tree(top)

      do while (tape%current_token == tk_union)
         call tape%get_token()

         call term(tape, tree, top)
         node_r = tree(top)

         node = make_tree_node(op_union)
         call register_and_connector(tree, top, node, node_l, node_r)

         node_l = tree(top)
      end do
   end subroutine regex


   pure subroutine term(tape, tree, top)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: node, node_l, node_r
 
      if (tape%current_token == tk_union &
          .or. tape%current_token == tk_rpar &
          .or. tape%current_token == tk_end) then
         
         node = make_tree_node(op_empty)
         call register_and_connector(tree, top, node, terminal_node, terminal_node)

      else
         call postfix_op(tape, tree, top)
         node_l = tree(top)

         do while (tape%current_token /= tk_union &
                   .and. tape%current_token /= tk_rpar &
                   .and. tape%current_token /= tk_end)
   
            node = make_tree_node(op_concat)

            call node%register_node(tree, top)
            call connect_left(tree, node%own_i, node_l%own_i)
            
            call postfix_op(tape, tree, top)
            node_r = tree(top)

            call connect_right(tree, node%own_i, node_r%own_i)
            node_l = tree(top)
         end do
      end if
   end subroutine term


   pure subroutine postfix_op(tape, tree, top)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: node, node_l, node_r

      
      call primary(tape, tree, top)
      node_l = tree(top)

      if (tape%current_token == tk_star) then
         node_r = terminal_node
         node = make_tree_node(op_closure)
         call register_and_connector(tree, top, node, node_l, node_r)       
         call tape%get_token()

      else if (tape%current_token == tk_plus) then
         node_r = terminal_node
         node = make_tree_node(op_closure)
         call register_and_connector(tree, top, node, node_l, terminal_node)

         node_r = tree(top)
         node = make_tree_node(op_concat)
         call register_and_connector(tree, top, node, node_l, node_r)

         call tape%get_token()

      else if (tape%current_token == tk_question) then
         node_r = terminal_node
         node = make_tree_node(op_empty)
         call register_and_connector(tree, top, node, node_l, terminal_node)

         node_r = tree(top)
         node = make_tree_node(op_union)
         call register_and_connector(tree, top, node, node_l, node_r)

         call tape%get_token()
      end if
   end subroutine postfix_op


   pure subroutine primary(tape, tree, top)
      use :: forgex_utf8_m
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: node
      type(segment_t) :: seg

      if (tape%current_token == tk_char) then
         seg = segment_t(ichar_utf8(tape%token_char), ichar_utf8(tape%token_char))
         node = make_atom(seg)
      
         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         call tape%get_token()
      end if
   end subroutine primary


   recursive subroutine print_tree(tree, root_i)
      use :: forgex_utf8_m
      implicit none
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32) :: root_i

      type(tree_node_t) :: p

      p = tree(root_i)

      select case (p%op)
      case (op_not_init)
         return
      case (op_char)
         write(*, "(a)", advance='no') '"'//trim(char_utf8(p%c(1)%min))//'"'
      case (op_concat)
         write(*, "(a)", advance='no') "(concatenate "
         call print_tree(tree, p%left_i)
         write(*, "(a)", advance='no') ' '
         call print_tree(tree, p%right_i)
         write(*, "(a)", advance='no') ')'
      case (op_union)
         write(*, "(a)", advance='no') "(or "
         call print_tree(tree, p%left_i)
         write(*, "(a)", advance='no') ' '
         call print_tree(tree, p%right_i)
         write(*, "(a)", advance='no') ')'
      case (op_closure)
         write(*, "(a)", advance='no') "(closure "
         call print_tree(tree, p%left_i)
         write(*, "(a)", advance='no') ')'
      case (op_empty)
         write(*, '(a)', advance='no') "EMPTY"
      case default
         write(stderr, *) "This will not hoppen in 'print_tree'"
         error stop
      end select
   end subroutine print_tree

end module forgex_syntax_tree_m
