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

   !! The regular expression parsing performed by this module 
   !! is done using recursive descent parsing.

#ifdef DEBUG
   public :: print_tree
   interface print_tree
      module procedure :: print_tree_wrap
   end interface print_tree

   interface
      pure subroutine message(i, j) bind(c)
         import c_int
         implicit none
         integer(c_int), intent(in), value :: i, j
      end subroutine message
   end interface

   interface
      pure subroutine message_char(i, str) bind(c)
         import c_int, c_char
         implicit none
         integer(c_int), intent(in), value :: i
         character(1, kind=c_char), intent(in) :: str(*)
      end subroutine
   end interface
#endif

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
      logical        :: is_registered = .false.
   contains
      procedure :: register_node
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
            tree(i)%c = SEG_INIT
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

            call postfix_op(tape, tree, top)
            node_r = tree(top)

            node = make_tree_node(op_concat)
            call register_and_connector(tree, top, node, node_l, node_r)

            node_l = node

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
      else if (tape%current_token == tk_lcurlybrace) then

         call range_min_max(tape, tree, top)
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

      select case (tape%current_token)
      case (tk_char)
         seg = segment_t(ichar_utf8(tape%token_char), ichar_utf8(tape%token_char))
         node = make_atom(seg)

         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         call tape%get_token()

      case (tk_lpar)
         call tape%get_token()
         call regex(tape, tree, top)
         if (tape%current_token /= tk_rpar) then
            error stop "primary: Close parenthesis is expected."
         end if
         call tape%get_token()

      case (tk_lsbracket)
         call char_class(tape, tree, top)
         if (tape%current_token /= tk_rsbracket) then
            error stop "primary: Close square bracket is expected."
         end if
         call tape%get_token()

      case (tk_backslash)
         call shorthand(tape, tree, top)
         call tape%get_token()

      case (tk_dot)
         node = make_atom(SEG_ANY)
         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         call tape%get_token()

      case default
         error stop "primary: Pattern includes some syntax error."

      end select
   end subroutine primary


   pure subroutine range_min_max(tape, tree, top)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: node, node_l, node_r, node_rr, ptr
      integer(int32) :: arg(2), ios, min, max, count
      character(:), allocatable :: buf

      buf = ''
      arg(:) = 0
      node = terminal_node
      max = 0
      min = 0

      ptr = tree(top)

      call tape%get_token()

      do while (tape%current_token /= tk_rcurlybrace)
         buf= buf//trim(tape%token_char)
         call tape%get_token

         if (tape%current_token == tk_end) then
            error stop "range_min_max: Closing right curlybrace is expected."
         end if
      end do

      read(buf, *, iostat=ios) arg(:)
      buf = adjustl(buf)

      if (arg(1) == 0) then   ! {,max}, {0,max}
         min = 0
         max = arg(2)
      else if (arg(2) == 0) then ! {min,}, {num}
         if (buf(len_trim(buf):len_trim(buf)) == ',') then
            min = arg(1)
            max = 0
         else
            min = arg(1)
            max = arg(1)
         end if

      else
         min = arg(1)
         max = arg(2)
      end if

      if (max == 0) then

         node = make_tree_node(op_closure)
         call register_and_connector(tree, top, node, ptr, terminal_node)

         if (min == 0) return

         if (min >= 1) then
            node_r = tree(top)
            node = make_tree_node(op_concat)
            call register_and_connector(tree, top, node, ptr, node_r)
         end if

         if (min > 1) then
            count = 1
            do while (count < min)
               node_r = tree(top)
               node = make_tree_node(op_concat)
               call register_and_connector(tree, top, node, ptr, node_r)
               count = count + 1
            end do
         end if

         return

      else if (max == 1) then

         if (min == 0) then
            node_r = make_tree_node(op_union)
            call register_and_connector(tree, top, node_r, ptr, terminal_node)
            return
         end if

         if (min>= 1) then
            node = ptr
            call register_and_connector(tree, top, node, ptr, terminal_node)
            return
         end if

      else ! (max > 1)

         if (min == 0) then
            count = 1
            node = ptr
            do while (count < max)

               node_l = node
               node_rr = make_tree_node(op_empty)
               call register_and_connector(tree, top, node_rr, node, terminal_node)

               node_r = make_tree_node(op_union)
               call register_and_connector(tree, top, node_r, node_l, node_rr)

               node = make_tree_node(op_concat)
               call register_and_connector(tree, top, node, ptr, node_r)

               node = tree(top)
               count = count + 1
            end do

            node_l = tree(top)
            node_r = make_tree_node(op_empty)
            call register_and_connector(tree, top, node_r, node, terminal_node)

            node = make_tree_node(op_union)
            call register_and_connector(tree, top, node, node_l, node_r)

            return
         end if

         if (min == 1) then
            count = 1
            node = ptr
            do while (count < max-1)

               node_l = node
               node_rr = make_tree_node(op_empty)
               call register_and_connector(tree, top, node_rr, node, terminal_node)

               node_r = make_tree_node(op_union)
               call register_and_connector(tree, top, node_r, node_l, node_rr)

               node = make_tree_node(op_concat)
               call register_and_connector(tree, top, node, ptr, node_r)

               node = tree(top)
               count = count + 1
            end do

            node_l = tree(top)
            node_r = make_tree_node(op_empty)
            call register_and_connector(tree, top, node_r, node, terminal_node)

            node = make_tree_node(op_union)
            call register_and_connector(tree, top, node, node_l, node_r)

            node = make_tree_node(op_concat)
            call register_and_connector(tree, top, node, ptr, tree(top))
            return
         end if

         if (min > 1) then

            count = min + 1
            node = ptr
            do while (count < max+1)
               node_rr = make_tree_node(op_empty)
               call register_and_connector(tree, top, node_rr, node, terminal_node)

               node_r = make_tree_node(op_union)
               call register_and_connector(tree, top, node_r, node, node_rr)

               node = make_tree_node(op_concat)
               call register_and_connector(tree, top, node, ptr, node_r)

               node = tree(top)
               count = count +1
            end do

            count = 1
            node_l = tree(top)
            do while (count < min)

               node = make_tree_node(op_concat)
               call register_and_connector(tree, top, node, node_l, ptr)

               node_l = tree(top)
               count = count +1
            end do

         end if
      end if
   end subroutine range_min_max


   !> This procedure handles character classes.
   pure subroutine char_class(tape, tree, top)
      use :: forgex_utf8_m
      use :: forgex_enums_m
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(segment_t), allocatable :: seglist(:)
      character(:), allocatable :: buf
      integer :: siz, ie, i, j, inext, terminal
      logical :: is_inverted
      type(tree_node_t) :: node

      call tape%get_token(class_flag=.true.)

      buf = ""
      do while (tape%current_token /= tk_rsbracket)
         ie = idxutf8(tape%token_char, 1)
         buf = buf // tape%token_char(1:ie)
         call tape%get_token(class_flag=.true.)
      end do

      is_inverted = .false.
      if (buf(1:1) == SYMBOL_CRET) then
         is_inverted = .true.
         buf = buf(2:len(buf))
      end if

      siz = len_utf8(buf)

      siz = siz - 2*count_token(buf(2:len_trim(buf)-1), SYMBOL_HYPN)

      if (buf(len_trim(buf):len_trim(buf)) == SYMBOL_HYPN) siz = siz -1

      allocate(seglist(siz))

      terminal = len(buf)
      i = 1
      j = 1
      buf = buf//char(0)

      do while (i <= terminal)

         ie = idxutf8(buf, i)
         inext = ie + 1

         ! 次の文字がハイフンでないならば
         if (buf(inext:inext) /= SYMBOL_HYPN) then
            seglist(j)%min = ichar_utf8(buf(i:ie))
            seglist(j)%max = ichar_utf8(buf(i:ie))
            j = j + 1
         else
            seglist(j)%min = ichar_utf8(buf(i:ie))

            i = inext + 1
            ie = idxutf8(buf, i)
            inext = ie + 1

            seglist(j)%max = ichar_utf8(buf(i:ie))
            j = j + 1
         end if

         ! 先頭の記号がハイフンならば
         if (j == 1 .and. buf(1:1) == SYMBOL_HYPN) then
            seglist(1)%min = ichar_utf8(SYMBOL_HYPN)
            seglist(1)%max = ichar_utf8(SYMBOL_HYPN)
            i = inext
            j = j + 1
            cycle
         end if

         ! 最後の記号がハイフンならば
         if (i >= terminal .and. buf(terminal:terminal) == SYMBOL_HYPN) then
            seglist(siz)%max = UTF8_CODE_MAX
            exit
         end if

         i = inext
      end do

      if (is_inverted) then
         call invert_segment_list(seglist)
      end if

      node = make_tree_node(op_char)
      if (.not. allocated(node%c)) allocate(node%c(size(seglist, dim=1)))
      node%c(:) = seglist(:)

      call register_and_connector(tree, top, node, terminal_node, terminal_node)
   end subroutine char_class


   !> This function constructs a tree node for carriage return (CR) and line feed (LF) characters.
   pure subroutine make_tree_crlf(tree, top)
      implicit none
      type(tree_node_t), intent(inout) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: cr, lf, node_r, node

      cr = make_atom(SEG_CR)
      call register_and_connector(tree, top, cr, terminal_node, terminal_node)

      lf = make_atom(SEG_LF)
      call register_and_connector(tree, top, lf, terminal_node, terminal_node)

      node_r = make_tree_node(op_concat)
      call register_and_connector(tree, top, node_r, cr, lf)
      
      node = make_tree_node(op_union)
      call register_and_connector(tree, top, node, lf, node_r)
   
   end subroutine make_tree_crlf


   !> This function handles shorthand escape sequences (`\t`, `\n`, `\r`, `\d`, `\D`, 
   !> `\w`, `\W`, `\s`, `\S`).
   pure subroutine shorthand(tape, tree, top)
      use :: forgex_parameters_m
      use :: forgex_utf8_m
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), intent(inout) ::tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(inout) :: top

      type(tree_node_t) :: node
      type(segment_t), allocatable :: seglist(:)
      type(segment_t) :: seg
   

      select case (trim(tape%token_char))
      case (ESCAPE_T)
         node = make_atom(SEG_TAB)
         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         return
      
      case (ESCAPE_N)
         call make_tree_crlf(tree, top)
         return
      
      case (ESCAPE_R)
         node = make_atom(SEG_CR)
         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         return
         
      case (ESCAPE_D)
         node = make_atom(SEG_DIGIT)
         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         return

      case (ESCAPE_D_CAPITAL)
         allocate(seglist(1))
         seglist(1) = SEG_DIGIT
         call invert_segment_list(seglist)

      case (ESCAPE_W)
         allocate(seglist(4))
         seglist(1) = SEG_LOWERCASE
         seglist(2) = SEG_UPPERCASE
         seglist(3) = SEG_DIGIT
         seglist(4) = SEG_UNDERSCORE
      
      case (ESCAPE_W_CAPITAL)
         allocate(seglist(4))
         seglist(1) = SEG_LOWERCASE
         seglist(2) = SEG_UPPERCASE
         seglist(3) = SEG_DIGIT
         seglist(4) = SEG_UNDERSCORE
         call invert_segment_list(seglist)

      case (ESCAPE_S)
         allocate(seglist(6))
         seglist(1) = SEG_SPACE
         seglist(2) = SEG_TAB
         seglist(3) = SEG_CR
         seglist(4) = SEG_LF
         seglist(5) = SEG_FF
         seglist(6) = SEG_ZENKAKU_SPACE

      case (ESCAPE_S_CAPITAL)
         allocate(seglist(6))
         seglist(1) = SEG_SPACE
         seglist(2) = SEG_TAB
         seglist(3) = SEG_CR
         seglist(4) = SEG_LF
         seglist(5) = SEG_FF
         seglist(6) = SEG_ZENKAKU_SPACE
         call invert_segment_list(seglist)

      case default
         seg = segment_t(ichar_utf8(tape%token_char), ichar_utf8(tape%token_char))
         node = make_atom(seg)
         call register_and_connector(tree, top, node, terminal_node, terminal_node)
         return
      end select

      allocate(node%c(size(seglist, dim=1)))

      node%c(:) = seglist(:)
      node%op = op_char

      call register_and_connector(tree, top, node, terminal_node, terminal_node)

      deallocate(seglist)

   end subroutine shorthand
      

!=====================================================================!
#ifdef DEBUG

   subroutine print_tree_wrap(tree, node_i)
      implicit none
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer, intent(in) :: node_i

      call print_tree_internal(tree, node_i)
      write(stderr, *) ''
   end subroutine print_tree_wrap

   recursive subroutine print_tree_internal(tree, node_i)
      implicit none
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer, intent(in) :: node_i

      select case (tree(node_i)%op)
      case (op_char)
         write(stderr, '(a)', advance='no') trim(print_class_simplify(tree, node_i))
      case (op_concat)
         write(stderr, '(a)', advance='no') "(concatenate "
         call print_tree_internal(tree, tree(node_i)%left_i)
         write(stderr, '(a)', advance='no') ' '
         call print_tree_internal(tree, tree(node_i)%right_i)
         write(stderr, '(a)', advance='no') ')'

      case (op_union)
         write(stderr, '(a)', advance='no') "(or "
         call print_tree_internal(tree, tree(node_i)%left_i)
         write(stderr, '(a)', advance='no') ' '
         call print_tree_internal(tree, tree(node_i)%right_i)
         write(stderr, '(a)', advance='no') ')'

      case (op_closure)
         write(stderr, '(a)', advance='no') "(closure"
         call print_tree_internal(tree, tree(node_i)%left_i)
         write(stderr, '(a)', advance='no') ')'

      case (op_empty)
         write(stderr, '(a)', advance='no') 'EMPTY'

      case default
         write(stderr, '(a)') "This will not occur in 'print_tree'."
         error stop
      end select
   end subroutine print_tree_internal


   function print_class_simplify (tree, root_i) result(str)
      use :: forgex_utf8_m
      implicit none
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32) :: root_i
      character(:), allocatable :: str

      integer(int32) :: siz, j
      character(:),allocatable :: buf

      str = ''
      siz = size(tree(root_i)%c, dim=1)

      if (siz == 0) return

      if (tree(root_i)%c(1) == SEG_LF) then
         str = '<LF>'
         return

      else if (tree(root_i)%c(1) == SEG_CR) then
         str = '<CR>'
         return

      else if (siz == 1 .and. tree(root_i)%c(1)%min == tree(root_i)%c(1)%max) then
         str = '"'//char_utf8(tree(root_i)%c(1)%min)//'"'
         return

      else if (siz == 1 .and. tree(root_i)%c(1) == SEG_ANY) then
         str = '<ANY>'
         return
      end if

      buf = '[ '
      do j = 1, siz

         if (tree(root_i)%c(j) == SEG_LF) then
            buf = buf//'<LF>; '

         else if (tree(root_i)%c(j) == SEG_TAB) then
            buf = buf//'<TAB>; '

         else if (tree(root_i)%c(j) == SEG_CR) then
            buf = buf//'<CR>; '

         else if (tree(root_i)%c(j) == SEG_FF) then
            buf = buf//'<FF>; '

         else if (tree(root_i)%c(j) == SEG_SPACE) then
            buf = buf//'<SPACE>; '

         else if (tree(root_i)%c(j) == SEG_ZENKAKU_SPACE) then
            buf = buf//'<ZENKAKU SPACE>; '

         else if (tree(root_i)%c(j)%max == UTF8_CODE_MAX) then
            buf = buf//'"'//char_utf8(tree(root_i)%c(j)%min)//'"-"'//"<U+1FFFFF>"//'; '

         else
            buf = buf//'"'//char_utf8(tree(root_i)%c(j)%min)//'"-"'//char_utf8(tree(root_i)%c(j)%max)//'"; '
         end if
      end do

      buf = trim(buf)//']'

      str = trim(buf)

   end function print_class_simplify

#endif

end module forgex_syntax_tree_m
