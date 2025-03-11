! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_syntax_tree_graph_m module is a part of Forgex.
!
#ifdef IMPURE
#define pure
#endif
module forgex_syntax_tree_graph_m
   use :: forgex_parameters_m
   use :: forgex_enums_m
   use :: forgex_segment_m, register => register_segment_to_list
   use :: forgex_syntax_tree_node_m, &
      only: tree_node_t, tape_t, terminal, make_atom, make_tree_node, make_repeat_node
   use :: forgex_error_m
   implicit none
   private

   type, public :: tree_t
   !! This derived-type contains all node of syntax-tree in the tree_node_t type array `nodes`.
      type(tree_node_t), allocatable :: nodes(:)
      integer :: top = INVALID_INDEX
      integer :: num_alloc = 0
      type(tape_t) :: tape
      logical :: is_valid = .true.
      integer :: code = SYNTAX_VALID
      integer :: paren_balance
   contains
      procedure :: build => tree_graph__build_syntax_tree
      procedure :: reallocate => tree_graph__reallocate
      procedure :: deallocate => tree_graph__deallocate
      procedure :: register => tree_graph__register_node
      procedure :: register_connector => tree_graph__register_connector
      procedure :: connect_left => tree_graph__connect_left
      procedure :: connect_right => tree_graph__connect_right
      procedure :: get_top => tree_graph__get_top
      procedure :: regex => tree_graph__regex
      procedure :: term => tree_graph__term
      procedure :: suffix_op => tree_graph__suffix_op
      procedure :: primary => tree_graph__primary
      procedure :: char_class =>tree_graph__char_class
      procedure :: caret_dollar => tree_graph__make_tree_caret_dollar
      procedure :: crlf => tree_graph__make_tree_crlf
      procedure :: shorthand => tree_graph__shorthand
      procedure :: hex2seg => tree_graph__hexadecimal_to_segment
      procedure :: times => tree_graph__times
      procedure :: print => print_tree_wrap
   end type

   public :: dump_tree_table
   public :: interpret_class_string

   public :: hex2seg

contains

   !> This procedure builds an AST corresponding to a given (regular expression) pattern from it.
   pure subroutine tree_graph__build_syntax_tree(self, pattern)
      implicit none
      class(tree_t), intent(inout) :: self
      character(*), intent(in) :: pattern

      integer :: i, status

      ! if (allocated(self%nodes)) deallocate(self%nodes)
      allocate(self%nodes(TREE_NODE_BASE:TREE_NODE_UNIT), stat=status)
      
      self%nodes(TREE_NODE_BASE:TREE_NODE_UNIT)%own_i = [(i, i=TREE_NODE_BASE, TREE_NODE_UNIT)]
      self%num_alloc = 1
      self%tape%idx = 1
      self%tape%str = pattern
      self%top = 0
      self%paren_balance = 0
      call self%tape%get_token()

      ! Generate AST from a given pattern.
      call self%regex()

      ! Check the pattern is valid.
      if (.not. self%is_valid) return

      ! Determine if parentheses are balanced.
      if (self%paren_balance > 0) then
         self%is_valid = .false.
         self%code = SYNTAX_ERR_PARENTHESIS_MISSING
      else if (self%paren_balance < 0) then
         self%is_valid = .false.
         self%code = SYNTAX_ERR_PARENTHESIS_UNEXPECTED
      end if
      
      self%nodes(self%top)%parent_i = TERMINAL_INDEX
   end subroutine tree_graph__build_syntax_tree


   !> This procedure handles the reallcation of the `tree_node_t` type array 
   !> within the component of the `tree_t` object.
   !> However, it is not be used in v4.2.
   pure subroutine tree_graph__reallocate(self)
      implicit none
      class(tree_t), intent(inout) :: self
      integer :: new_part_begin, new_part_end, i
      type(tree_node_t), allocatable :: tmp(:)

      if (.not. allocated(self%nodes)) then
         allocate(self%nodes(TREE_NODE_BASE:TREE_NODE_UNIT))
         self%num_alloc = 1
      end if

      new_part_begin = ubound(self%nodes, dim=1) + 1
      new_part_end   = ubound(self%nodes, dim=1) * 2

      if (new_part_end > TREE_NODE_HARD_LIMIT) then
         error stop "Exceeded the maximum number of tree nodes can be allocated."
      end if

      call move_alloc(self%nodes, tmp)

      allocate(self%nodes(TREE_NODE_BASE:new_part_end))

      self%nodes(TREE_NODE_BASE:new_part_begin-1) = tmp(TREE_NODE_BASE:new_part_begin-1)

      self%nodes(new_part_begin:new_part_end)%own_i = [(i, i = new_part_begin, new_part_end)]

      deallocate(tmp)

   end subroutine tree_graph__reallocate


   !> This procedure deallocates `nodes` of `tree_t`
   pure subroutine tree_graph__deallocate(self)
      implicit none
      class(tree_t), intent(inout) :: self

      deallocate(self%nodes)
   end subroutine tree_graph__deallocate


   pure subroutine tree_graph__register_node(self, node)
      implicit none
      class(tree_t), intent(inout) :: self
      type(tree_node_t), intent(inout) :: node

      integer :: top

      top = self%top + 1
      if (top > ubound(self%nodes, dim=1)) then
         call self%reallocate()
      end if
      node%own_i = top

      self%nodes(top) = node
      self%nodes(top)%is_registered = .true.
      self%top = top

   end subroutine tree_graph__register_node

   
   pure subroutine tree_graph__register_connector(self, node, left, right)
      implicit none
      class(tree_t), intent(inout) :: self
      type(tree_node_t), intent(inout) :: node
      type(tree_node_t), intent(in) :: left, right

      call self%register(node)

      call self%connect_left(self%nodes(self%top)%own_i, left%own_i)
      call self%connect_right(self%nodes(self%top)%own_i, right%own_i)

   end subroutine tree_graph__register_connector

   pure subroutine tree_graph__connect_left(self, parent, child)
      implicit none
      class(tree_t), intent(inout) :: self
      integer, intent(in) :: parent, child

      if (parent /= INVALID_INDEX) self%nodes(parent)%left_i = child
      if (child /= INVALID_INDEX) self%nodes(child)%parent_i = parent

   end subroutine tree_graph__connect_left

   pure subroutine tree_graph__connect_right(self, parent, child)
      implicit none
      class(tree_t), intent(inout) :: self
      integer, intent(in) :: parent, child

      if (parent /= INVALID_INDEX) self%nodes(parent)%right_i = child
      if (child /= INVALID_INDEX) self%nodes(child)%parent_i = parent
   end subroutine tree_graph__connect_right

   pure function tree_graph__get_top(self) result(node)
      implicit none
      class(tree_t), intent(in) :: self
      type(tree_node_t) :: node

      node = self%nodes(self%top)
   end function tree_graph__get_top


!=====================================================================!
!  Parsing procedures

   pure recursive subroutine tree_graph__regex(self)
      implicit none
      class(tree_t), intent(inout) :: self

      type(tree_node_t) :: node, left, right

      call self%term()

      ! When term's analysis is valid,
      if (self%is_valid) then

         left = self%get_top()

         do while (self%tape%current_token == tk_union)
            call self%tape%get_token()

            call self%term()
            if (.not. self%is_valid) exit

            right = self%get_top()

            node = make_tree_node(op_union)
            call self%register_connector(node, left, right)

            left = self%get_top()
         end do
      
      else
         if (self%code /= SYNTAX_VALID) then
            return
         end if
      end if

   end subroutine tree_graph__regex


   pure recursive subroutine tree_graph__term(self)
      implicit none
      class(tree_t), intent(inout) :: self
      type(tree_node_t) :: node, left, right

      if (self%tape%current_token == tk_union &
            .or. self%tape%current_token == tk_rpar &
            .or. self%tape%current_token == tk_end) then
         
         node = make_tree_node(op_empty)
         call self%register_connector(node, terminal, terminal)
      else
         call self%suffix_op()
         if (.not. self%is_valid) return

         left = self%get_top()

         do while (self%tape%current_token /= tk_union &
                     .and. self%tape%current_token /= tk_rpar &
                     .and. self%tape%current_token /= tk_end)
            
            call self%suffix_op()
            if (.not. self%is_valid) return

            right = self%get_top()

            node = make_tree_node(op_concat)
            call self%register_connector(node, left, right)

            left = self%get_top()
         end do
      end if

      if (self%tape%current_token == tk_rpar) then
         self%paren_balance = self%paren_balance -1
      end if

   end subroutine


   pure recursive subroutine tree_graph__suffix_op(self)
      implicit none
      class(tree_t), intent(inout) :: self
      type(tree_node_t) :: node, left, right

      call self%primary()
      if (.not. self%is_valid) return

      left = self%get_top()

      select case (self%tape%current_token)
      case (tk_star)
         node = make_tree_node(op_closure)
         call self%register_connector(node, left, terminal)
         call self%tape%get_token()

      case (tk_plus)
         node = make_tree_node(op_closure)
         call self%register_connector(node, left, terminal)

         right = self%get_top()
         node = make_tree_node(op_concat)
         call self%register_connector(node, left, right)

         call self%tape%get_token()

      case (tk_question)
         node = make_tree_node(op_empty)
         call self%register_connector(node, left, terminal)

         right = self%get_top()
         node = make_tree_node(op_union)
         call self%register_connector(node, left, right)
         call self%tape%get_token()

      case (tk_lcurlybrace)
         call self%times()
         if (.not. self%is_valid) then
            ! self%code = SYNTAX_ERR_INVALID_TIMES
            return
         end if
         call self%tape%get_token()

      ! `case default` must NOT be placed here.
      end select
   end subroutine tree_graph__suffix_op


   pure recursive subroutine tree_graph__primary(self)
      use :: forgex_utf8_m, only: ichar_utf8
      implicit none
      class(tree_t), intent(inout) :: self

      type(tree_node_t) :: node
      type(segment_t) :: seg
      character(:), allocatable :: chara


      select case (self%tape%current_token)
      case (tk_char)
         chara = self%tape%token_char
         seg = segment_t(ichar_utf8(chara), ichar_utf8(chara))
         node = make_atom(seg)
         call self%register_connector(node, terminal, terminal)
         call self%tape%get_token() 
      
      case (tk_lpar)
         
         if (self%tape%current_token == tk_lpar) then
            self%paren_balance = self%paren_balance +1
         end if

         call self%tape%get_token()
         call self%regex()
      
         ! If regex fails, return immediately.
         if (.not. self%is_valid) return

         ! If not a right parenthesis, throw an error.
         if (self%tape%current_token /= tk_rpar) then
            self%code = SYNTAX_ERR_PARENTHESIS_MISSING
            self%is_valid = .false.
            return
         end if
         call self%tape%get_token()

      case (tk_lsbracket)
         call self%char_class()
         if (.not. self%is_valid) then
            return
         end if 
         if (self%tape%current_token /= tk_rsbracket) then
            self%code = SYNTAX_ERR_BRACKET_MISSING
            self%is_valid = .false.
            return
         end if
         call self%tape%get_token()

      case (tk_backslash)
         call self%shorthand()
         if (.not. self%is_valid) then
            return
         end if
         call self%tape%get_token()
      
      case (tk_dot)
         node = make_atom(SEG_ANY)
         call self%register_connector(node, terminal, terminal)
         call self%tape%get_token()

      case (tk_caret)
         call self%caret_dollar()
         call self%tape%get_token()
         
      case (tk_dollar)
         call self%caret_dollar()
         call self%tape%get_token()
      
      case (tk_rsbracket)
         self%code = SYNTAX_ERR_BRACKET_UNEXPECTED
         self%is_valid = .false.
         return

      case (tk_rpar)
         self%code = SYNTAX_ERR_PARENTHESIS_UNEXPECTED
         self%is_valid = .false.
         return
   
      ! Unescaped closing curly brace is allowed.
      case (tk_rcurlybrace)
         chara = self%tape%token_char
         seg = segment_t(ichar_utf8(chara), ichar_utf8(chara))
         node = make_atom(seg)
         call self%register_connector(node, terminal, terminal)
         call self%tape%get_token()

      case (tk_lcurlybrace)
         self%code = SYNTAX_ERR_INVALID_TIMES
         self%is_valid = .false.
         return

      case (tk_star)
         self%code = SYNTAX_ERR_STAR_INCOMPLETE
         self%is_valid = .false.
         return

      case (tk_plus)
         self%code = SYNTAX_ERR_PLUS_INCOMPLETE
         self%is_valid = .false.
         return

      case (tk_question)
         self%code = SYNTAX_ERR_QUESTION_INCOMPLETE
         self%is_valid = .false.
         return

      case default
         self%code = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
         self%is_valid = .false.
         return
      end select

   end subroutine tree_graph__primary
      

   !> This subroutine treats character class expression,
   !> and does not call any other recursive procedures.
   pure subroutine tree_graph__char_class(self)
      use :: forgex_utf8_m, only: idxutf8, len_utf8, count_token, ichar_utf8
      use :: forgex_enums_m
      implicit none
      class(tree_t), intent(inout) :: self

      type(segment_t), allocatable :: seglist(:)
      character(:), allocatable :: buf
      type(tree_node_t) :: node

      integer :: siz, ie
      logical :: is_inverted, backslashed
      character(:), allocatable :: prev, curr

      siz = 0

      call self%tape%get_token(class_flag=.true.)

      ! The variable buf stores the string representing the character class.
      buf = ''
      prev = ''
      curr = ''
      backslashed = .false.
      outer: do while (self%tape%current_token /= tk_rsbracket)
         prev = curr
         if (self%tape%current_token == tk_end) then
            return
         end if

         ie = idxutf8(self%tape%token_char, 1)
         curr = self%tape%token_char(1:ie)
         buf = buf//curr

         if (self%tape%current_token == tk_backslash .and. .not. backslashed) then
            backslashed = .true.
         else
            backslashed = .false.
         end if

         call self%tape%get_token(class_flag=.true.)
         
         ! for an escaped right square bracket 
         if (self%tape%current_token == tk_rsbracket .and. backslashed) then
            ie = idxutf8(self%tape%token_char, 1)
            curr = self%tape%token_char(1:ie)
            buf = buf//curr
            call self%tape%get_token(class_flag=.true.)
         end if

      end do outer

      ! If the character class pattern is empty, return false. 
      if (len(buf) == 0) then
         self%code = SYNTAX_ERR_EMPTY_CHARACTER_CLASS
         self%is_valid = .false.
         return
      end if

      ! Handling a negative class case.
      is_inverted = .false.
      if (buf(1:1) == SYMBOL_CRET) then
         is_inverted = .true.
         buf = buf(2:len(buf))  ! May this assignment be a problem?
      end if

      ! The variable siz stores the length of buf.
      siz = len_utf8(buf)

      if (siz < 1) then
         self%code = SYNTAX_ERR_EMPTY_CHARACTER_CLASS
         self%is_valid = .false.
         return
      end if

      call interpret_class_string(buf, seglist, self%is_valid, self%code)

      if (.not. self%is_valid) then
         return
      end if

      if (.not. allocated(seglist)) then
         self%code = ALLOCATION_ERR
         self%is_valid = .false.
         return
      end if

      if (size(seglist) < 1) then
         self%code = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
         self%is_valid = .false.
         return
      end if

      ! the seglist array have been allocated near the L362.
      if (is_inverted) then
         call invert_segment_list(seglist)
      end if

      if (.not. allocated(seglist)) then
         error stop "ERROR: `seg_list` is not allocated. This should not happen."
      end if

      node = make_tree_node(op_char)
      if (.not. allocated(node%c)) allocate(node%c(size(seglist, dim=1)))

      node%c(:) = seglist(:)

      call self%register_connector(node, terminal, terminal)

   end subroutine tree_graph__char_class

   ! This procedure registers line feed and carriage return nodes into the AST　under costruced.
   pure subroutine tree_graph__make_tree_crlf(self)
      implicit none
      class(tree_t), intent(inout) :: self

      type(tree_node_t) :: cr, lf, right, node

      cr = make_atom(SEG_CR)
      call self%register_connector(cr, terminal, terminal)

      lf = make_atom(SEG_LF)
      call self%register_connector(lf, terminal, terminal)

      right = make_tree_node(op_concat)
      call self%register_connector(right, cr, lf)

      node = make_tree_node(op_union)
      call self%register_connector(node, lf, right)

   end subroutine tree_graph__make_tree_crlf


   !> This function constructs a tree node for carriage return (CR) and line feed (LF) characters.
   pure subroutine tree_graph__make_tree_caret_dollar(self)
      implicit none
      class(tree_t), intent(inout) :: self

      type(tree_node_t) :: cr, lf, node_r_r, node_r, node, empty_r

      cr = make_atom(SEG_CR)
      call self%register_connector(cr, terminal, terminal)

      lf = make_atom(SEG_LF)
      call self%register_connector(lf, terminal, terminal)

      node_r_r = make_tree_node(op_concat)
      call self%register_connector(node_r_r, cr, lf)

      node_r = make_tree_node(op_union)
      call self%register_connector(node_r, lf, node_r_r)

      empty_r = make_atom(SEG_EMPTY)
      call self%register_connector(empty_r, terminal, terminal)

      node = make_tree_node(op_union)
      call self%register_connector(node, node_r, empty_r)

   end subroutine tree_graph__make_tree_caret_dollar


   !> This function handles shorthand escape sequences (`\t`, `\n`, `\r`, `\d`, `\D`,
   !> `\w`, `\W`, `\s`, `\S`).
   !> It does not call any other recursive procedures.
   pure subroutine tree_graph__shorthand(self)
      use :: forgex_utf8_m, only: ichar_utf8
      implicit none
      class(tree_t), intent(inout) :: self
      type(tree_node_t) :: node
      type(segment_t), allocatable :: seglist(:)
      type(segment_t) :: seg
      character(:), allocatable :: chara


      select case (trim(self%tape%token_char))
      case (ESCAPE_T)
         node = make_atom(SEG_TAB)
         call self%register_connector(node, terminal, terminal)
         return

      case (ESCAPE_N)
         call self%crlf()
         return

      case (ESCAPE_R)
         node = make_atom(SEG_CR)
         call self%register_connector(node, terminal, terminal)
         return

      case (ESCAPE_D)
         node = make_atom(SEG_DIGIT)
         call self%register_connector(node, terminal, terminal)
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

      case (ESCAPE_X)
         ! Error handling for x escape sequence is handled by hex2seg.
         call self%hex2seg(seglist)
         if (.not. self%is_valid) return
         ! It is not necessary to call self%tape%get_token() procedure.

      case (EMPTY_CHAR)
         self%code = SYNTAX_ERR_ESCAPED_SYMBOL_MISSING
         self%is_valid = .false.
         return
      case (SYMBOL_LSBK, SYMBOL_RSBK, &
            SYMBOL_LCRB, SYMBOL_RCRB, &
            SYMBOL_LPAR, SYMBOL_RPAR, &
            SYMBOL_DOLL, SYMBOL_BSLH, &
            SYMBOL_VBAR, SYMBOL_DOT, &
            SYMBOL_QUES, SYMBOL_CRET, &
            SYMBOL_STAR, SYMBOL_PLUS, &
            SYMBOL_HYPN)
         chara = self%tape%token_char
         seg = segment_t(ichar_utf8(chara), ichar_utf8(chara))
         node = make_atom(seg)
         call self%register_connector(node, terminal, terminal)
         return

      case default
         self%code = SYNTAX_ERR_ESCAPED_SYMBOL_INVALID
         self%is_valid = .false.
         ! chara = self%tape%token_char
         ! seg = segment_t(ichar_utf8(chara), ichar_utf8(chara))
         ! node = make_atom(seg)
         ! call self%register_connector(node, terminal, terminal)
         return
      end select

      allocate(node%c(size(seglist, dim=1)))
      ! This size function is safe because it is always allocated 
      ! to the non-returned branches of the select case above.

      node%c(:) = seglist(:)
      node%op = op_char

      call self%register_connector(node, terminal, terminal)

      deallocate(seglist)

   end subroutine tree_graph__shorthand

   !> This procedure handles a escape sequence with '\x'.
   pure subroutine tree_graph__hexadecimal_to_segment(self, seglist)
      implicit none
      class(tree_t), intent(inout) :: self
      type(segment_t), intent(inout), allocatable :: seglist(:)
      
      character(:), allocatable :: hex
      integer :: i
      logical :: is_two_digit, is_longer_digit

      hex = ''

      call self%tape%get_token()

      is_longer_digit = self%tape%current_token == tk_lcurlybrace
      is_two_digit = .not. is_longer_digit

      if (is_longer_digit) call self%tape%get_token()

      hex = self%tape%token_char(1:1) ! First, get the second digit.
      i = 2

      reader: do while(.true.)
         if (is_two_digit .and. i >= 3) exit reader
         call self%tape%get_token()

         if (is_longer_digit .and. self%tape%current_token /= tk_rcurlybrace .and. self%tape%current_token /= tk_char) then
            self%is_valid = .false.
            self%code = SYNTAX_ERR_CURLYBRACE_MISSING
            return
         end if

         if (self%tape%current_token == tk_rcurlybrace) exit reader
         hex = hex//self%tape%token_char(1:1)
         i = i + 1
      end do reader

      allocate(seglist(1))
      call hex2seg(trim(hex), seglist(1), self%code)

      if (self%code /= SYNTAX_VALID) then
         self%is_valid = .false.
         return
      end if

      self%is_valid = seglist(1) .in. SEG_WHOLE

      if (.not. self%is_valid) self%code  = SYNTAX_ERR_UNICODE_EXCEED


   end subroutine tree_graph__hexadecimal_to_segment


   !> This subroutine handles a quantifier range, and
   !> does not call any other recursive procedures.
   pure subroutine tree_graph__times(self)
      use :: forgex_utility_m, only: get_index_comma, is_integer
      implicit none
      class(tree_t), intent(inout) :: self
      character(:), allocatable :: buf
      integer(int32) :: arg(2), ios, min, max

      type(tree_node_t) :: left, node

      integer :: i, num_comma
      character(:), allocatable :: c1, c2
      logical :: is_infinite

      ! Initialize
      ios = 0
      buf = ''
      arg(:) = INVALID_REPEAT_VAL
      c1 = ''
      c2 = ''
      is_infinite = .false.
      max = INVALID_REPEAT_VAL
      min = INVALID_REPEAT_VAL

      call self%tape%get_token()

      ! Extract the part of the pattern that is the character class that
      ! this procedure should process.
      do while (self%tape%current_token /= tk_rcurlybrace)
         buf= buf//trim(self%tape%token_char)
         call self%tape%get_token

         if (self%tape%current_token == tk_end) then
            self%code = SYNTAX_ERR_CURLYBRACE_MISSING
            self%is_valid = .false.
            return
         end if
      end do

      if (len(buf) == 0) then
      ! Error for a{}
         self%is_valid = .false.
         self%code = SYNTAX_ERR_INVALID_TIMES
         return
      else if (len(buf) == 1) then
      ! Error for a{,}
         if (buf(1:1) == SYMBOL_COMMA) then
            self%is_valid = .false.
            self%code = SYNTAX_ERR_INVALID_TIMES
            return
         end if
      end if

      if (buf(1:1) == ',') then
         buf = "0"//buf ! e.g. {,2} =>{0,2}
      end if


      if (is_integer(buf)) then
         buf = trim(buf)//","//trim(buf) ! e.g. {0} => {0,0}
      end if

   !----------------
      call get_index_comma(buf, i, num_comma)

      ! ios has a negative value if an end-of-record condition is encountered during non-advancing input,
      ! a different negative value if and endfile condition was detected on the input device, a positive value
      ! if an error was detected, or the value zero otherwise.
      !
      ! cf. Michael Metcalf, John Reid and Malcolm Cohen (2018)
      !       - "Modern Fortran Explained--Incorporating Fortran 2018"

      ! patterns like {1,2,3} are error.
      if (num_comma > 1) then
         self%is_valid = .false.
         self%code = SYNTAX_ERR_INVALID_TIMES
         return
      end if

      c1 = buf(1:i-1)
      if (i+1 <= len_trim(buf)) c2 = buf(i+1:len_trim(buf))

      read(c1, fmt=*, iostat=ios) arg(1)
      if (ios > 0 .or. arg(1)< 0) then
         self%is_valid = .false.
         self%code = SYNTAX_ERR_INVALID_TIMES
         return
      end if

      if (trim(c2) == EMPTY_CHAR) then
         is_infinite = .true.
      else
         read(c2, fmt=*, iostat=ios) arg(2)
         if (ios > 0 .or. arg(2) < 0) then
            self%is_valid = .false.
            self%code = SYNTAX_ERR_INVALID_TIMES
            return
         end if
      end if

      
      if (is_infinite) then
         min = arg(1)
         max = INFINITE
      else
         min = arg(1)
         max = arg(2)
      end if

      if (min == 0 .and. max == 0) then
         continue
      else if (max /= INFINITE .and. min > max) then
         self%is_valid = .false.
         self%code = SYNTAX_ERR_INVALID_TIMES
         return
      else if (max == INVALID_REPEAT_VAL .and. min > max) then
         self%is_valid = .false.
         self%code = SYNTAX_ERR_INVALID_TIMES
         return
      end if

      node = make_repeat_node(min, max)
      left = self%get_top()
      call self%register_connector(node, left, terminal)

   end subroutine tree_graph__times


   !> This subroutine parses a pattern string and outputs a list of `segment_t` type.
   pure subroutine interpret_class_string(str, seglist, is_valid, ierr)
      use :: forgex_utf8_m, only: idxutf8, next_idxutf8, len_utf8, ichar_utf8
      use :: forgex_parameters_m
      use :: forgex_segment_m, register => register_segment_to_list
      use :: forgex_character_array_m
      implicit none

      character(*), intent(in) :: str
      type(segment_t), intent(inout), allocatable :: seglist(:)
      logical, intent(inout) :: is_valid
      integer, intent(inout) :: ierr

      integer :: i, j, k
      integer :: jerr
      type(segment_t) :: prev_seg, curr_seg
      type(segment_t), allocatable :: list(:), cache(:)
      logical :: backslashed
      logical :: prev_hyphenated, curr_hyphenated
      type(character_array_t), allocatable :: ca(:) ! character array
      integer :: siz ! total number of segment of `ca` array
      character(:), allocatable :: c ! Temporary variable stores a character of interest.

      ! Initialize
      is_valid = .true.
      backslashed = .false.
      prev_hyphenated = .false.
      curr_hyphenated = .false.
      prev_seg = segment_t()
      curr_seg = segment_t()
      
      if (len(str) >= 2) then
         if (str(1:2) == '--') then
            ierr = SYNTAX_ERR_MISPLACED_SUBTRACTION_OPERATOR
            is_valid = .false.
         end if
      end if

      ! Convert to an array from a pattern string.
      call character_string_to_array(str, ca)
      if (.not. allocated(ca)) then
         ierr = SYNTAX_ERR_EMPTY_CHARACTER_CLASS
         is_valid = .false.
         return
      end if

      ! for escape sequences such as \x, \x{...}, \p{...}.
      call parse_escape_sequence_with_argument(ca, ierr)
      if (ierr /= SYNTAX_VALID) then
         is_valid = .false.
         return
      end if

      ! Remove backslash and hyphen, and raise respective flag for each component.
      call parse_backslash_and_hyphen_in_char_array(ca, ierr)
      if (ierr == SYNTAX_ERR_MISPLACED_SUBTRACTION_OPERATOR) then
         is_valid = .false.
         return
      end if

      ! Each ca(:)%seg_size will be set by this procedure calling.
      call parse_segment_width_in_char_array(ca)

      ! If each of the array element is hyphenated,
      ! check that the range is not 1 and return invalid.
      siz = 0
      check: do i = 1, size(ca, dim=1)

         ! If the former hypenated range is invalid, throw an error.
         if (ca(i)%is_hyphenated .and. ca(i)%seg_size /= 1) then
            ierr = SYNTAX_ERR_RANGE_WITH_ESCAPE_SEQUENCES
            is_valid = .false.
            return
         end if

         ! If the range following hyphenataed is invalid, throw an error.
         if (i>1) then
            if (ca(i-1)%is_hyphenated .and. ca(i)%seg_size /= 1) then
               ierr = SYNTAX_ERR_RANGE_WITH_ESCAPE_SEQUENCES
               is_valid = .false.
               return
            end if
         end if

         ! If a subtraction flag appear, throw an error at the moment.
         if (ca(i)%is_subtract) then
            ierr = SYNTAX_ERR_CHAR_CLASS_SUBTRANCTION_NOT_IMPLEMENTED
            is_valid = .false.
            return
         end if

         ! If the loop reaches the end of `ca` array, cancel the hyphenated flag, and
         ! then add a literal hyphen to the end.
         if (i> 1 .and. i == size(ca, dim=1)) then
            if (ca(i)%is_hyphenated) then
               ca(i)%is_hyphenated = .false.

               ca = [ca(1:size(ca)), &
                    character_array_t(SYMBOL_HYPN, .false., .false., ca(size(ca))%is_subtract, 1)]
               siz = siz + 1
               exit check
            end if
         end if
         siz = siz + ca(i)%seg_size
      end do check

      if (siz < 1) then
         ierr = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
         is_valid = .false.
         return
      end if
      allocate(list(siz))

      ! Initialize cache and counter variable.
      j = 0 ! Couter of actual list size for `seglist`.
      c = EMPTY_CHAR
      i = 1
      outer: do while(i <= size(ca, dim=1))
         c = ca(i)%c
         backslashed = ca(i)%is_escaped  ! cache `is_escaped` flag
         curr_hyphenated = ca(i)%is_hyphenated
         if (i > 1) prev_hyphenated = ca(i-1)%is_hyphenated 

         ! For escape sequences that take arguments.
         if (backslashed .and. c == ESCAPE_X) then
            i = i + 1
            if (i> size(ca, dim=1)) then
               ierr = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
               is_valid = .false.
               return
            end if
            c = ca(i)%c
            backslashed = ca(i)%is_escaped
            call hex2seg(c, curr_seg, ierr)
            if (ierr /= SYNTAX_VALID) then
               is_valid = .false.
               return
            end if
         else if (backslashed .and. c == ESCAPE_P) then
            ierr = SYNTAX_ERR_UNICODE_PROPERTY_NOT_IMPLEMENTED
            is_valid = .false.
            return
         else
            curr_seg = segment_t(ichar_utf8(c), ichar_utf8(c))
         end if

         ! For escape sequences that do not take arguments
         if (backslashed) then

            call convert_escaped_character_into_segments(c, cache)
            if (cache(1) == SEG_ERROR) then
               ierr = SYNTAX_ERR_ESCAPED_SYMBOL_INVALID
               is_valid = .false.
               return
            end if

            ! If the number of segemnts is greater than 1, register them to the `list`.
            if (size(cache, dim=1) > 1) then
               do k = 1, size(cache)
                  call register(list, cache(k), j, ierr)
               end do
               deallocate(cache)
               prev_seg = segment_t()
               i = i + 1
               cycle outer
            end if 

            curr_seg = cache(1)
         end if


         if (prev_hyphenated) then
            curr_seg = join_two_segments(prev_seg, curr_seg)
            if (curr_seg == SEG_ERROR) then
               ierr = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
               is_valid = .false.
               return
            end if
         end if
      
         if (.not. curr_hyphenated) then
            call register(list, curr_seg, j, jerr)
            if (jerr == SEGMENT_REJECTED) then
               ierr = SYNTAX_ERR_INVALID_CHARACTER_RANGE
               is_valid = .false.
               return
            end if
         end if

         prev_seg = curr_seg
         i = i + 1
      end do outer

      if (j < 1) then
         ! pattern '[+--]' causes this error for now.
         ! ierr = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
         ierr = SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
         is_valid = .false.
         return
      end if

      allocate(seglist(j))

      seglist(1:j) = list(1:j) ! copy local array into the argument array.

   end subroutine interpret_class_string



   !> This subroutine converts escaped character of the argument `chara` into segment `seg_list`. 
   pure subroutine convert_escaped_character_into_segments(chara, seg_list) !, hexcode)
      use :: forgex_utf8_m, only: ichar_utf8
      implicit none
      character(*), intent(in) :: chara
      type(segment_t), allocatable, intent(inout) :: seg_list(:)

      integer :: unused

      if (allocated(seg_list)) deallocate(seg_list)

      select case (trim(chara))
      case (ESCAPE_T)
         allocate(seg_list(1))
         seg_list(1) = SEG_TAB
      case (ESCAPE_N)
         allocate(seg_list(2))
         seg_list(1) = SEG_LF
         seg_list(2) = SEG_CR
      case (ESCAPE_R)
         allocate(seg_list(1))
         seg_list(1) = SEG_CR
      case (ESCAPE_D)
         allocate(seg_list(1))
         seg_list(1) = SEG_DIGIT
      case (ESCAPE_D_CAPITAL)
         allocate(seg_list(1))
         seg_list(1) = SEG_DIGIT
         call invert_segment_list(seg_list)
      case (ESCAPE_W)
         allocate(seg_list(4))
         seg_list(1) = SEG_LOWERCASE
         seg_list(2) = SEG_UPPERCASE
         seg_list(3) = SEG_DIGIT
         seg_list(4) = SEG_UNDERSCORE
      case (ESCAPE_W_CAPITAL)
         allocate(seg_list(4))
         seg_list(1) = SEG_LOWERCASE
         seg_list(2) = SEG_UPPERCASE
         seg_list(3) = SEG_DIGIT
         seg_list(4) = SEG_UNDERSCORE
         call invert_segment_list(seg_list)
      case (ESCAPE_S)
         allocate(seg_list(6))
         seg_list(1) = SEG_SPACE
         seg_list(2) = SEG_TAB
         seg_list(3) = SEG_CR
         seg_list(4) = SEG_LF
         seg_list(5) = SEG_FF
         seg_list(6) = SEG_ZENKAKU_SPACE
      case (ESCAPE_S_CAPITAL)
         allocate(seg_list(6))
         seg_list(1) = SEG_SPACE
         seg_list(2) = SEG_TAB
         seg_list(3) = SEG_CR
         seg_list(4) = SEG_LF
         seg_list(5) = SEG_FF
         seg_list(6) = SEG_ZENKAKU_SPACE
         call invert_segment_list(seg_list)
      case (ESCAPE_X)
         allocate(seg_list(1))
         call hex2seg(chara, seg_list(1), unused)
      case (ESCAPE_P)
         allocate(seg_list(1))
         seg_list(1) = SEG_ERROR
         continue
      case (SYMBOL_BSLH)
         allocate(seg_list(1))
         seg_list(1)%min = ichar_utf8(SYMBOL_BSLH)
         seg_list(1)%max = ichar_utf8(SYMBOL_BSLH)
      case (SYMBOL_LCRB)
         allocate(seg_list(1))
         seg_list(1)%min = ichar_utf8(SYMBOL_LCRB)
         seg_list(1)%max = ichar_utf8(SYMBOL_LCRB)
      case (SYMBOL_RCRB)
         allocate(seg_list(1))
         seg_list(1)%min = ichar_utf8(SYMBOL_RCRB)
         seg_list(1)%max = ichar_utf8(SYMBOL_RCRB)
      case (SYMBOL_LSBK)
         allocate(seg_list(1))
         seg_list(1)%min = ichar_utf8(SYMBOL_LSBK)
         seg_list(1)%max = ichar_utf8(SYMBOL_LSBK)
      case (SYMBOL_RSBK)
         allocate(seg_list(1))
         seg_list(1)%min = ichar_utf8(SYMBOL_RSBK)
         seg_list(1)%max = ichar_utf8(SYMBOL_RSBK)
      case default
         allocate(seg_list(1))
         seg_list(1) = SEG_ERROR
      end select

   end subroutine convert_escaped_character_into_segments


!=====================================================================!
  
   subroutine dump_tree_table(tree)
      use, intrinsic :: iso_fortran_env, stderr => error_unit
      implicit none
      class(tree_node_t), intent(in) :: tree(:)

      integer :: i, k

      write(stderr, '(1x, a)') '  own index|  operation|     parent|       left|      right|   registered|  segments'
      do i = TREE_NODE_BASE, ubound(tree, dim=1)
         if (tree(i)%is_registered) then
            write(stderr, '(5i12, a, 10x, 1l, 3x)', advance='no') tree(i)%own_i, &
               tree(i)%op, tree(i)%parent_i, tree(i)%left_i, tree(i)%right_i, '   ', &
               tree(i)%is_registered

            if (allocated(tree(i)%c)) then
               do k = 1, ubound(tree(i)%c, dim=1)

                  if (k /= 1) write(stderr, '(a)', advance='no') ', '
                   write(stderr, '(a)', advance='no') tree(i)%c(k)%print()
                                 
               end do
               write(stderr, *) ""
            else
               write(stderr, *) " "
            end if
         end if
      end do
   end subroutine dump_tree_table

   subroutine print_tree_wrap(self, uni)
      implicit none
      ! type(tree_node_t), intent(in) :: tree(:)
      class(tree_t), intent(in) :: self
      integer, intent(in) :: uni

      call print_tree_internal(self%nodes, self%top, uni)
      write(uni, *) ''
   end subroutine print_tree_wrap

   recursive subroutine print_tree_internal(tree, node_i, uni)
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer, intent(in) :: node_i
      integer, intent(in) :: uni

      if (node_i == INVALID_INDEX) return
      select case (tree(node_i)%op)
      case (op_char)
         write(uni, '(a)', advance='no') trim(print_class_simplify(tree, node_i))
      case (op_concat)
         write(uni, '(a)', advance='no') "(concatenate "
         call print_tree_internal(tree, tree(node_i)%left_i, uni)
         write(uni, '(a)', advance='no') ' '
         call print_tree_internal(tree, tree(node_i)%right_i, uni)
         write(uni, '(a)', advance='no') ')'

      case (op_union)
         write(uni, '(a)', advance='no') "(or "
         call print_tree_internal(tree, tree(node_i)%left_i, uni)
         write(uni, '(a)', advance='no') ' '
         call print_tree_internal(tree, tree(node_i)%right_i, uni)
         write(uni, '(a)', advance='no') ')'

      case (op_closure)
         write(uni, '(a)', advance='no') "(closure"
         call print_tree_internal(tree, tree(node_i)%left_i, uni)
         write(uni, '(a)', advance='no') ')'
      case (op_repeat)
         write(uni, '(a)', advance='no') "(repeat "
         call print_tree_internal(tree, tree(node_i)%left_i, uni)
         if (tree(node_i)%min_repeat == INVALID_REPEAT_VAL) then
            write(uni, "('{', ',', i0, '}')", advance='no') tree(node_i)%max_repeat
         else if (tree(node_i)%max_repeat == INVALID_REPEAT_VAL) then
            write(uni, "('{', i0, ',}')", advance='no') tree(node_i)%min_repeat
         else
            write(uni, "('{', i0, ',', i0, '}')",advance='no') tree(node_i)%min_repeat, tree(node_i)%max_repeat
         end if

         write(uni, '(a)', advance='no') ')'
         
      case (op_empty)
         write(uni, '(a)', advance='no') 'EMPTY'

      case default
         write(uni, '(a)') "This will not occur in 'print_tree'."
         error stop
      end select
   end subroutine print_tree_internal


   function print_class_simplify (tree, root_i) result(str)
      use :: forgex_segment_m, only: SEG_EMPTY
      use :: forgex_utf8_m
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32) :: root_i
      character(:), allocatable :: str

      integer(int32) :: siz, j
      character(:),allocatable :: buf

      str = ''
      if (allocated(tree(root_i)%c)) then
         siz = size(tree(root_i)%c, dim=1)
      else
         return
      end if

      if (siz == 0) return

      if (tree(root_i)%c(1) == SEG_LF) then
         str = '<LF>'
         return

      else if (tree(root_i)%c(1) == SEG_CR) then
         str = '<CR>'
         return

      else if (tree(root_i)%c(1) == SEG_EMPTY) then
         str ="<EMPTY>"
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


end module forgex_syntax_tree_graph_m