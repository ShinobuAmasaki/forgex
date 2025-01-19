! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_syntax_tree_graph_m module is a part of Forgex.
!
#ifdef IMPURE
#define pure
#endif
module forgex_syntax_tree_graph_m
   use :: forgex_parameters_m
   use :: forgex_enums_m
   use :: forgex_segment_m
   use :: forgex_syntax_tree_node_m, &
      only: tree_node_t, tape_t, terminal, make_atom, make_tree_node, make_repeat_node
   use :: forgex_syntax_tree_error_m
   implicit none
   private

   type, public :: tree_t
      type(tree_node_t), allocatable :: nodes(:)
      integer :: top = INVALID_INDEX
      integer :: num_alloc = 0
      type(tape_t) :: tape
      logical :: is_valid_pattern = .true.
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
      procedure :: range => tree_graph__range
      procedure :: print => print_tree_wrap
   end type

   public :: dump_tree_table
      
contains

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
      if (.not. self%is_valid_pattern) return

      ! Determine if parentheses are balanced.
      if (self%paren_balance > 0) then
         self%is_valid_pattern = .false.
         self%code = SYNTAX_ERR_PARENTHESIS_MISSING
      else if (self%paren_balance < 0) then
         self%is_valid_pattern = .false.
         self%code = SYNTAX_ERR_PARENTHESIS_UNEXPECTED
      end if
      
      self%nodes(self%top)%parent_i = TERMINAL_INDEX
   end subroutine tree_graph__build_syntax_tree


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
      if (self%is_valid_pattern) then

         left = self%get_top()

         do while (self%tape%current_token == tk_union)
            call self%tape%get_token()

            call self%term()
            if (.not. self%is_valid_pattern) exit

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
         if (.not. self%is_valid_pattern) return

         left = self%get_top()

         do while (self%tape%current_token /= tk_union &
                     .and. self%tape%current_token /= tk_rpar &
                     .and. self%tape%current_token /= tk_end)
            
            call self%suffix_op()
            if (.not. self%is_valid_pattern) return

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
      if (.not. self%is_valid_pattern) return

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
         call self%range()
         if (.not. self%is_valid_pattern) then
            self%code = SYNTAX_ERR_INVALID_RANGE
            return
         end if
         call self%tape%get_token()

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
         if (.not. self%is_valid_pattern) return

         ! If not a right parenthesis, throw an error.
         if (self%tape%current_token /= tk_rpar) then
            self%code = SYNTAX_ERR_PARENTHESIS_MISSING
            self%is_valid_pattern = .false.
            return
         end if
         call self%tape%get_token()

      case (tk_lsbracket)
         call self%char_class()
         if (self%tape%current_token /= tk_rsbracket) then
            self%code = SYNTAX_ERR_BRACKET_MISSING
            self%is_valid_pattern = .false.
            return
         end if
         call self%tape%get_token()

      case (tk_backslash)
         call self%shorthand()
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
         self%is_valid_pattern = .false.
         return

      case (tk_rpar)
         self%code = SYNTAX_ERR_PARENTHESIS_UNEXPECTED
         self%is_valid_pattern = .false.
         return
   
      case default
         self%code = SYNTAX_ERR
         self%is_valid_pattern = .false.
         return
      end select

   end subroutine tree_graph__primary
      

   pure subroutine tree_graph__char_class(self)
      use :: forgex_utf8_m, only: idxutf8, len_utf8, count_token, ichar_utf8
      use :: forgex_enums_m
      implicit none
      class(tree_t), intent(inout) :: self

      type(segment_t), allocatable :: seglist(:)
      character(:), allocatable :: buf
      type(tree_node_t) :: node

      integer :: siz, ie, i, j, i_next, i_terminal
      logical :: is_inverted

      call self%tape%get_token(class_flag=.true.)

      buf = ''
      do while (self%tape%current_token /= tk_rsbracket)
         if (self%tape%current_token == tk_end) then
            return
         end if
         ie = idxutf8(self%tape%token_char, 1)
         buf = buf// self%tape%token_char(1:ie)
         call self%tape%get_token(class_flag=.true.)
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
      
      i_terminal = len(buf)
      i = 1
      j = 1
      buf = buf//char(0)

      do while (i <= i_terminal)
         ie = idxutf8(buf, i)
         i_next = ie + 1

         ! 次の文字がハイフンでないならば
         if (buf(i_next:i_next) /= SYMBOL_HYPN) then
            seglist(j)%min = ichar_utf8(buf(i:ie))
            seglist(j)%max = ichar_utf8(buf(i:ie))
            j = j + 1
         else
            seglist(j)%min = ichar_utf8(buf(i:ie))

            i = i_next + 1
            ie = idxutf8(buf, i)
            i_next = ie + 1

            seglist(j)%max = ichar_utf8(buf(i:ie))
            j = j + 1
         end if

         ! 先頭の記号がハイフンならば
         if (j == 1 .and. buf(1:1) == SYMBOL_HYPN) then
            seglist(1)%min = ichar_utf8(SYMBOL_HYPN)
            seglist(1)%max = ichar_utf8(SYMBOL_HYPN)
            i = i_next
            j = j + 1
            cycle
         end if

         ! 最後の記号がハイフンならば
         if (i >= i_terminal .and. buf(i_terminal:i_terminal) == SYMBOL_HYPN) then
            seglist(siz)%max = UTF8_CODE_MAX
            exit
         end if

         i = i_next
      end do

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

      case default
         chara = self%tape%token_char
         seg = segment_t(ichar_utf8(chara), ichar_utf8(chara))
         node = make_atom(seg)
         call self%register_connector(node, terminal, terminal)
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


   pure subroutine tree_graph__range(self)
      implicit none
      class(tree_t), intent(inout) :: self
      character(:), allocatable :: buf
      integer(int32) :: arg(2), ios, min, max

      type(tree_node_t) :: left, node

      ios = 0
      buf = ''
      arg(:) = INVALID_REPEAT_VAL

      call self%tape%get_token()

      do while (self%tape%current_token /= tk_rcurlybrace)
         buf= buf//trim(self%tape%token_char)
         call self%tape%get_token

         if (self%tape%current_token == tk_end) then
            self%code = SYNTAX_ERR_CURLYBRACE_MISSING
            self%is_valid_pattern = .false.
            return
         end if
      end do

      if (buf(1:1) == ',') then
         buf = "0"//buf
      end if

      read(buf, fmt=*, iostat=ios) arg(:)

      ! ios has a negative value if an end-of-record condition is encountered during non-advancing input,
      ! a different negative value if and endfile condition was detected on the input device, a positive value
      ! if an error was detected, or the value zero otherwise.
      !
      ! cf. Michael Metcalf, John Reid and Malcolm Cohen (2018)
      !       - "Modern Fortran Explained--Incorporating Fortran 2018"

      if (ios > 0) then
         self%is_valid_pattern = .false.
         return
      end if

      buf = adjustl(buf)

      if (arg(1) == 0) then   ! {,max}, {0,max}

         if (buf(len_trim(buf):len_trim(buf)) == ',') then
            min = arg(1)
            max = INFINITE
         else
            min = 0
            max = arg(2)
         end if
      else if (arg(2) == INVALID_REPEAT_VAL) then ! {min,}, {num}
         if (buf(len_trim(buf):len_trim(buf)) == ',') then
            min = arg(1)
            max = INFINITE
         else
            min = arg(1)
            max = arg(1)
         end if

      else
         min = arg(1)
         max = arg(2)
      end if

      if (max /= INVALID_REPEAT_VAL .and. max /= INFINITE .and. min > max) then
         self%is_valid_pattern = .false.
         return
      end if

      node = make_repeat_node(min, max)
      left = self%get_top()
      
      call self%register_connector(node, left, terminal)

   end subroutine tree_graph__range



      
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