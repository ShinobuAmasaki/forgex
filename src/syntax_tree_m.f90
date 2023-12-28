module syntax_tree_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: enums_m
   use :: utf8_m
   use :: segment_m
   implicit none
   private

   public :: tree_t
   public :: build_syntax_tree
   public :: tape_t
   public :: deallocate_tree
   public :: print_tree


   character(UTF8_CHAR_SIZE), parameter, public :: EMPTY = char(0)

   integer(int32), parameter :: TREE_MAX_SIZE = 1024

   character(1), parameter, private :: ESCAPE_T = 't'
   character(1), parameter, private :: ESCAPE_N = 'n'
   character(1), parameter, private :: ESCAPE_R = 'r'
   character(1), parameter, private :: ESCAPE_D = 'd'
   character(1), parameter, private :: ESCAPE_W = 'w'
   character(1), parameter, private :: ESCAPE_S = 's'
   character(1), parameter, private :: ESCAPE_D_CAPITAL = 'D'
   character(1), parameter, private :: ESCAPE_W_CAPITAL = 'W'
   character(1), parameter, private :: ESCAPE_S_CAPITAL = 'S'
   character(1), parameter, private :: HAT = '^'
   character(1), parameter, private :: HYPHEN = '-'
   character(1), parameter, private :: CARET = '^'
   character(1), parameter, private :: DOLLAR = '$'

   type :: allocated_list_t
      type(tree_t), pointer :: node
   end type 

   type :: tree_t
      integer(int32) :: op
      type(segment_t), allocatable :: c(:)
      type(tree_t), pointer :: left => null()
      type(tree_t), pointer :: right => null()
   end type 

   type :: tape_t
      character(:), allocatable :: str
      integer(int32) :: current_token
      character(UTF8_CHAR_SIZE) :: token_char = EMPTY
      integer(int32) :: idx = 1
   contains
      procedure :: get_token
   end type

   integer :: tree_node_count = 0

   type(allocated_list_t) :: array(TREE_MAX_SIZE)

contains

   function build_syntax_tree(tape, str) result(root)
      implicit none
      character(*), intent(in) :: str
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: root

      root => null()

      tape%idx = 1

      call initialize_parser(tape, str)
      
      root => regex(tape)

      if (tape%current_token /= tk_end) then
         write(stderr, *) "The pattern contains extra character at the end."
      end if

   end function build_syntax_tree


   subroutine deallocate_tree()
      implicit none
      integer :: i, max

      max = tree_node_count

      do i = 1, max
         if (associated(array(i)%node)) then
            deallocate(array(i)%node)
            tree_node_count = tree_node_count - 1
         end if
      end do
         
   end subroutine deallocate_tree


   
   subroutine print_tree(tree)
      implicit none
      type(tree_t), intent(in) :: tree

      write(stderr, '(a)') "--- PRINT TREE ---"
      call print_tree_internal(tree)
      write(stderr, '(a)') ''
   end subroutine print_tree


   subroutine initialize_parser(tape, str)
      implicit none
      type(tape_t), intent(inout) :: tape
      character(*), intent(in) :: str 

      tape%str = str

      call get_token(tape)
   end subroutine initialize_parser


   subroutine get_token(self, class)
      use :: utf8_m
      implicit none
      class(tape_t) :: self 
      logical, optional, intent(in) :: class
      
      logical :: class_flag

      integer(int32) :: i, nexti
      character(UTF8_CHAR_SIZE) :: c

      class_flag = .false.
      if (present(class)) class_flag = class

      i = self%idx

      if (i > len(self%str)) then
         self%current_token = tk_end
         self%token_char = ''
      else
         nexti = idxutf8(self%str, i) + 1

         c = self%str(i:nexti-1)

         if (class_flag) then

            select case (trim(c))
            case (']')
               self%current_token = tk_rsbracket
            case ('-')
               self%current_token = tk_hyphen
               self%token_char = c
            case default
               self%current_token = tk_char
               self%token_char = c
            end select
         
         else

            select case (trim(c))
            case ('|')
               self%current_token = tk_union
            case ('(')
               self%current_token = tk_lpar
            case (')')
               self%current_token = tk_rpar
            case ('*')
               self%current_token = tk_star
            case ('+')
               self%current_token = tk_plus
            case ('?')
               self%current_token = tk_question
            case ('\')
               self%current_token = tk_backslash
            
               i = nexti
               nexti = idxutf8(self%str, i) + 1

               c = self%str(i:nexti-1)
               self%token_char = c
            case  ('[')
               self%current_token = tk_lsbracket
            case (']')
               self%current_token = tk_rsbracket
            case ('{')
               self%current_token = tk_lcurlybrace
            case ('}')
               self%current_token = tk_rcurlybrace
            case ('.')
               self%current_token = tk_dot
            case ('^')
               self%current_token = tk_caret
            case ('$')
               self%current_token = tk_dollar
            case default
               self%current_token = tk_char
               self%token_char = c
            end select
         end if

         self%idx = nexti
      end if

   end subroutine get_token

!=====================================================================!

   function make_tree_node(op, left, right) result(node)
      implicit none
      integer(int32), intent(in) :: op
      type(tree_t), pointer, intent(in) :: left, right
      type(tree_t), pointer :: node

      node => null()

      allocate(node)

      node%op = op
      node%left => left
      node%right => right

      tree_node_count = tree_node_count + 1

      array(tree_node_count)%node => node
   end function


   function make_atom (segment) result(node)
      implicit none
      type(segment_t), intent(in) :: segment
      type(tree_t), pointer :: node

      node => null()
      allocate(node)
      allocate(node%c(1))

      node%op = op_char
      node%c = segment

      tree_node_count = tree_node_count + 1
      array(tree_node_count)%node => node
   end function make_atom

!=====================================================================!

   function regex(tape) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: tree

      tree => null()

      tree => term(tape)
      do while (tape%current_token == tk_union)
         call tape%get_token()
         tree => make_tree_node(op_union, tree, term(tape))
      end do

   end function regex


   function term(tape) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: tree
      
      tree => null() 

      if ( tape%current_token == tk_union &
           .or. tape%current_token == tk_rpar &
           .or. tape%current_token == tk_end) then
         tree => make_tree_node(op_empty, null(), null())
      else
         tree => postfix_op(tape)
         do while (tape%current_token /= tk_union &
                   .and. tape%current_token /= tk_rpar &
                   .and. tape%current_token /= tk_end )
            tree => make_tree_node(op_concat, tree, postfix_op(tape))
         end do
      end if 
   end function term


   function postfix_op(tape) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: tree
      
      tree => null()

      tree => primary(tape)

      select case (tape%current_token)
      case (tk_star)
         tree => make_tree_node(op_closure, tree, null())
         call tape%get_token()

      case (tk_plus)
         tree => make_tree_node(op_concat, tree, make_tree_node(op_closure, tree, null()))
         call tape%get_token()
      
      case (tk_question)
         tree => make_tree_node(op_union, tree, make_tree_node(op_empty, tree, null()))
         call tape%get_token()
      
      case (tk_lcurlybrace)
         tree => range_min_max(tape, tree)
         call tape%get_token()
      end select

   end function postfix_op


   function primary (tape) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: tree
      
      type(segment_t) :: seg

      tree => null()

      select case (tape%current_token)
      case (tk_char)
         seg = segment_t(ichar_utf8(tape%token_char), ichar_utf8(tape%token_char))
         tree => make_atom(seg)
         call tape%get_token()

      case (tk_lpar)
         call tape%get_token()
         tree => regex(tape)
         if (tape%current_token /= tk_rpar) then
            write(stderr, *) "Close parenthesis is expected."
         end if
         call tape%get_token()

      case (tk_lsbracket)
         call tape%get_token(class=.true.)
         tree => char_class(tape)
         if (tape%current_token /= tk_rsbracket) then
            write(stderr, *) "Close square bracket is expected."
         end if
         call tape%get_token()
         
      case (tk_dot)
         tree => make_atom(SEG_ANY)
         call tape%get_token()

      case (tk_backslash)
         tree => shorthand(tape)
         call tape%get_token()

      case (tk_caret)
         tree => make_tree_crlf()
         call tape%get_token()
      case (tk_dollar)
         tree => make_tree_crlf()
         call tape%get_token()

      case default
         write(stderr, *) "Pattern includes some syntax error."
      end select

   end function primary 


   function range_min_max(tape, ptr) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer, intent(in) :: ptr
      type(tree_t), pointer :: tree, tmp

      character(:), allocatable :: buf
      integer(int32) :: arg(2), ios, min, max, count, i

      buf = ''
      arg(:) = 0
      tree => null()
      max = 0
      min = 0

      call tape%get_token()

      do while (tape%current_token /= tk_rcurlybrace)
         buf = buf//trim(tape%token_char)
         call tape%get_token()

         if (tape%current_token == tk_end) then
            write(stderr, *) "range_min_max: Close curly brace is expected."
            exit
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
      
         if (min == 0) then
            tree => make_tree_node(op_closure, ptr, null())
            return
         end if 

         if (min >= 1) then
            tree => make_tree_node(op_union, ptr, make_tree_node(op_empty, tree, null()))
            tree => make_tree_node(op_concat, ptr, tree)
         end if

         if (min > 1) then
            count = 1
            do while (count < min)
               tree => make_tree_node(op_concat, ptr, tree)
               count = count + 1
            end do
         end if
         
         return


      else if (max == 1) then

         if (min == 0) then
            tree => make_tree_node(op_union, ptr, make_tree_node(op_empty, ptr, null()))
            return
         end if

         if (min >= 1) then
            tree => ptr
            return
         end if
                  

      else ! (max > 1)

         if (min == 0) then
            count = 1
            tree => ptr
            do while (count < max)
               tree => make_tree_node(op_union, tree, make_tree_node(op_empty, tree, null()))
               tree => make_tree_node(op_concat, ptr, tree)
               count = count + 1
            end do

            tree => make_tree_node(op_union, tree, make_tree_node(op_empty, tree, null()))

            return
         end if
         
         if (min == 1) then
            count = 1
            tree => ptr
            
            do while (count < max-1)
               tree => make_tree_node(op_union, tree, make_tree_node(op_empty, tree, null()))
               tree => make_tree_node(op_concat, ptr, tree)
               count = count + 1
            end do

            tree => make_tree_node(op_union, tree, make_tree_node(op_empty, tree, null()))
            tree => make_tree_node(op_concat, ptr, tree)
            return

         end if

         if (min > 1) then
            count = min + 1
            
            tree => ptr

            do while (count < max+1)
               tree => make_tree_node(op_union, tree, make_tree_node(op_empty, tree, null()))
               tree => make_tree_node(op_concat, ptr, tree)
               count = count + 1
            end do

            count = 1
            do while (count < min)
               tree => make_tree_node(op_concat, tree, ptr)
               count = count + 1
            end do

         end if 
      end if 

   end function range_min_max


   function char_class(tape) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: tree
      type(segment_t), allocatable :: seglist(:)

      character(:), allocatable :: buf
      integer :: siz, count_hyphen, i, inext, iend, j
      logical :: inverted   
   
      tree => null()

      buf = ''
      do while (tape%current_token /= tk_rsbracket)
         iend = idxutf8(tape%token_char, 1)
         buf = buf//tape%token_char(1:iend)
         call tape%get_token(class = .true.)
      end do

      inverted = .false.
      ! is there '^' at first?
      if (buf(1:1) == HAT) then
         inverted = .true.
         buf = buf(2:len(buf))
      end if

      siz = len_utf8(buf)

      siz = siz - 2*count_token(buf(2:len_trim(buf)-1), HYPHEN)

      if (buf(len_trim(buf):len_trim(buf)) == HYPHEN) siz = siz -1

      allocate(seglist(siz))


      iend = len(buf)
      i = 1
      j = 1
      buf = buf//char(0) !空文字を末尾に追加する。

      do while (i <= iend)

         inext = idxutf8(buf, i) + 1

         ! 次の文字がハイフンでないならば、
         if (buf(inext:inext) /= HYPHEN) then
            seglist(j)%min = ichar_utf8(buf(i:inext-1))
            seglist(j)%max = ichar_utf8(buf(i:inext-1))
            j = j + 1

         else
            seglist(j)%min = ichar_utf8(buf(i:inext-1))

            ! 2文字すすめる
            i = inext +1
            inext = idxutf8(buf, i) + 1

            seglist(j)%max = ichar_utf8(buf(i:inext-1))
            j = j + 1
         end if

         ! 先頭の文字がハイフンならば
         if (j == 1 .and. buf(1:1) == HYPHEN) then
            seglist(1)%min = ichar_utf8(HYPHEN)
            seglist(1)%max = ichar_utf8(HYPHEN)
            j = j + 1
            cycle
         end if

         if (i == iend .and. buf(iend:iend) == HYPHEN) then
            seglist(siz)%max = UTF8_CODE_MAX
            exit
         end if

         i = inext
      end do

      if (inverted) then
         call invert_segment_list(seglist)
      end if 

      allocate(tree)
      allocate(tree%c(size(seglist, dim=1)))

      tree%c(:) = seglist(:)
      tree%op = op_char

      tree_node_count = tree_node_count + 1
      array(tree_node_count)%node => tree

   end function char_class

      
   function make_tree_crlf() result(tree)
      implicit none
      type(tree_t), pointer :: tree
      type(tree_t), pointer :: cr, lf

      tree => null()
      cr => null()
      lf => null()

      allocate(cr)
      allocate(cr%c(1))
      cr%c(1) = SEG_CR
      cr%op = op_char

      tree_node_count = tree_node_count + 1
      array(tree_node_count)%node => cr

      allocate(lf)
      allocate(lf%c(1))

      lf%c(1) = SEG_LF
      lf%op = op_char

      tree_node_count = tree_node_count + 1
      array(tree_node_count)%node => lf

      tree => make_tree_node(op_union, lf, make_tree_node(op_concat, cr, lf))
   end function make_tree_crlf


   function shorthand(tape) result(tree)
      implicit none
      type(tape_t), intent(inout) :: tape
      type(tree_t), pointer :: tree, left, right 

      type(segment_t), allocatable :: seglist(:)
      type(segment_t) :: seg

      tree => null()
      left => null()
      right => null()

      select case (trim(tape%token_char))
      case (ESCAPE_T)
         tree => make_atom(SEG_TAB)
         return
      
      case (ESCAPE_N)
         tree => make_tree_crlf()
         return
      
      case (ESCAPE_R)
         tree => make_atom(SEG_CR)
         return
         
      case (ESCAPE_D)
         tree => make_atom(SEG_DIGIT)
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
         tree => make_atom(seg)
         return
      end select

      allocate(tree)
      allocate(tree%c(size(seglist, dim=1)))

      tree%c(:) = seglist(:)
      tree%op = op_char

      tree_node_count = tree_node_count +1
      array(tree_node_count)%node => tree

      deallocate(seglist)

   end function shorthand

   
   subroutine invert_segment_list(list)
      implicit none
      type(segment_t), intent(inout), allocatable :: list(:)

      logical :: unicode(UTF8_CODE_MIN:UTF8_CODE_MAX)
      logical :: inverted(UTF8_CODE_MIN-1:UTF8_CODE_MAX+1)

      integer :: i, j, count

      unicode(:) = .false.
      inverted(:) = .false.

      do i = UTF8_CODE_MIN, UTF8_CODE_MAX
         do j = 1, size(list, dim=1)
            unicode(i) = unicode(i) .or. (list(j)%min <= i .and. i <= list(j)%max)
         end do
      end do 

      inverted(UTF8_CODE_MIN-1) = .false.
      inverted(UTF8_CODE_MAX+1) = .false.
      inverted(UTF8_CODE_MIN:UTF8_CODE_MAX) = .not. unicode(UTF8_CODE_MIN:UTF8_CODE_MAX)

      count = 0
      do i = UTF8_CODE_MIN, UTF8_CODE_MAX
         if (.not. inverted(i-1) .and. inverted(i)) count = count + 1
      end do

      deallocate(list)
      allocate(list(count))

      count = 1
      do i = UTF8_CODE_MIN, UTF8_CODE_MAX+1
         if (.not. inverted(i-1) .and. inverted(i)) then
            list(count)%min = i
         end if

         if (inverted(i-1) .and. .not. inverted(i)) then
            list(count)%max = i-1
            count = count + 1
         end if 
      end do

   end subroutine invert_segment_list

!=====================================================================!

   recursive subroutine print_tree_internal(tree)
      implicit none
      type(tree_t), intent(in) :: tree

      select case (tree%op)
      case (op_char)
         write(stderr, '(a)', advance='no') trim(print_class_simplify(tree))
      case (op_concat)
         write(stderr, '(a)', advance='no') "(concatenate "
         call print_tree_internal(tree%left)
         write(stderr, '(a)', advance='no') ' '
         call print_tree_internal(tree%right)
         write(stderr, '(a)', advance='no') ')'

      case (op_union)
         write(stderr, '(a)', advance='no') "(or "
         call print_tree_internal(tree%left)
         write(stderr, '(a)', advance='no') ' '
         call print_tree_internal(tree%right)
         write(stderr, '(a)', advance='no') ')'

      case (op_closure)
         write(stderr, '(a)', advance='no') "(closure"
         call print_tree_internal(tree%left)
         write(stderr, '(a)', advance='no') ')'

      case (op_empty)
         write(stderr, '(a)', advance='no') 'EMPTY'
      
      case default
         write(stderr, '(a)') "This will not occur in 'print_tree'."
         error stop
      end select
   end subroutine print_tree_internal 


   function print_class_simplify (p) result(str)
      implicit none
      type(tree_t), intent(in) :: p
      character(:), allocatable :: str

      integer(int32) :: siz, j
      character(:),allocatable :: buf 

      str = ''
      siz = size(p%c, dim=1)

      if (siz == 0) return

      if (p%c(1) == SEG_LF) then
         str = '<LF>'
         return
      
      else if (p%c(1) == SEG_CR) then
         str = '<CR>'
         return

      else if (siz == 1 .and. p%c(1)%min == p%c(1)%max) then
         str = '"'//char_utf8(p%c(1)%min)//'"'
         return

      else if (siz == 1 .and. p%c(1) == SEG_ANY) then
         str = '<ANY>'
         return
      end if

      buf = '[ '
      do j = 1, siz

         if (p%c(j) == SEG_LF) then
            buf = buf//'<LF>; '
         
         else if (p%c(j) == SEG_TAB) then
            buf = buf//'<TAB>; '

         else if (p%c(j) == SEG_CR) then
            buf = buf//'<CR>; '
         
         else if (p%c(j) == SEG_FF) then
            buf = buf//'<FF>; '

         else if (p%c(j) == SEG_SPACE) then
            buf = buf//'<SPACE>; '

         else if (p%c(j) == SEG_ZENKAKU_SPACE) then
            buf = buf//'<ZENKAKU SPACE>; '
         
         else if (p%c(j)%max == UTF8_CODE_MAX) then
            buf = buf//'"'//char_utf8(p%c(j)%min)//'"-"'//"<U+1FFFFF>"//'; '
         
         else 
            buf = buf//'"'//char_utf8(p%c(j)%min)//'"-"'//char_utf8(p%c(j)%max)//'"; '
         end if
      end do

      buf = trim(buf)//']'

      str = trim(buf)

   end function print_class_simplify
         

end module syntax_tree_m