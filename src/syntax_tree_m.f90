!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     syntax_tree_m module is a part of Forgex.

module syntax_tree_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: utf8_m
   use :: segment_m
   implicit none
   private

   character(3), parameter, public :: EMPTY = char(0)

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



   public :: op_char
   public :: op_concat
   public :: op_union
   public :: op_closure
   public :: op_empty
   
   public :: tree_t
   public :: parse_regex
   public :: print_tree
   public :: deallocate_tree
   
   interface print_tree
      module procedure :: print_tree_api
   end interface

   character(1), parameter :: hyphen = '-'

   ! These enums will be rewritten in Fortran 2023's enumerator feature. 
   enum, bind(c)
      enumerator :: tk_char = 0
      enumerator :: tk_union
      enumerator :: tk_lpar
      enumerator :: tk_rpar
      enumerator :: tk_backslash
      enumerator :: tk_question
      enumerator :: tk_star
      enumerator :: tk_plus
      enumerator :: tk_lsbracket    ! left square bracket
      enumerator :: tk_rsbracket    ! right square bracket 
      enumerator :: tk_lcurlybrace  ! left curly brace
      enumerator :: tk_rcurlybrace  ! right curly brace
      enumerator :: tk_dot
      enumerator :: tk_hyphen
      enumerator :: tk_end
   end enum

   enum, bind(c)
      enumerator :: op_char = 0
      enumerator :: op_concat
      enumerator :: op_union
      enumerator :: op_closure
      enumerator :: op_empty
   end enum

   type :: tree_t
      integer(int32) :: op ! operator
      ! character(3) :: c = EMPTY
      type(segment_t), allocatable :: c(:)
      type(tree_t), pointer :: left => null()
      type(tree_t), pointer :: right => null()
   end type

   integer(int32) :: current_token
   character(4) :: token_char = EMPTY
   character(:), allocatable, target :: strbuff

contains

   ! Parse a regular expression and return a pointer to the corresponding syntax tree.
   function parse_regex(str) result(t)
      implicit none
      character(*), intent(in) :: str
      type(tree_t), pointer :: t

      call initialize_regex_parser(str)

      print *, str

      t => regex()

      if (current_token /= tk_end) then 
         write(stderr, *) "The pattern contains extra character at the end."
      end if

   end function parse_regex


   subroutine print_tree_api (tree)
      implicit none
      type(tree_t), pointer :: tree

      print *, "--- PRINT TREE ---"
      call print_tree_internal(tree)
      print *, ''

   end subroutine

   
   recursive subroutine deallocate_tree(tree)
      implicit none
      type(tree_t), pointer :: tree
      type(tree_t), pointer :: p

      if (.not. associated(tree)) then
         return
      end if

      call deallocate_tree(tree%left)
      call deallocate_tree(tree%right)

      if (associated(tree)) then
         if (allocated(tree%c)) then
             deallocate(tree%c)
         end if
         nullify(tree)
      end if

   end subroutine deallocate_tree

!---------------------------------------------------------------------!

   function get_token(str, class) result(res)
      use :: utf8_m
      implicit none
      character(*), intent(in) :: str
      logical, optional, intent(in) :: class

      logical :: class_flag

      integer(int32), save :: idx = 1
      integer(int32) :: next_idx
      integer(int32) :: res
      character(4) :: c

      class_flag = .false.
      if (present(class)) class_flag = class 

      if (idx > len(str)) then
         current_token = tk_end
         token_char = ''
      else

         next_idx = idxutf8(str, idx) + 1

         c = str(idx:next_idx-1)

         if (class_flag) then

            select case (trim(c))
            case (']')
               current_token = tk_rsbracket
            case ('-')
               current_token = tk_hyphen 
               token_char = c
            case default
               current_token = tk_char
               token_char = c
            end select
         
         else
            select case (trim(c))
            case ('|')
               current_token = tk_union
            case ('(')
               current_token = tk_lpar
            case (')')
               current_token = tk_rpar
            case ('*')
               current_token = tk_star
            case ('+')
               current_token = tk_plus
            case ('?')
               current_token = tk_question
            case ('\')
               current_token = tk_backslash
               ! Read the next character
               idx = next_idx
               next_idx = idxutf8(str, idx) +1 

               c = str(idx:next_idx - 1)
               token_char = c
            
            case ('[')
               current_token = tk_lsbracket
            case (']')
               current_token = tk_rsbracket
            case ('{')
               current_token = tk_lcurlybrace
            case ('}')
               current_token = tk_rcurlybrace

            case ('.')
               current_token = tk_dot
            
            case default
               current_token = tk_char
               token_char = c
            end select
         end if

         idx = next_idx

      end if

      res = current_token

   end function

   subroutine initialize_regex_parser(str)
      implicit none
      character(*), intent(in) :: str
      integer :: trush

      strbuff = str

      trush = get_token(strbuff)
   end subroutine initialize_regex_parser


   ! Make a node on the syntax tree. 
   function make_tree_node(op, left, right) result(p)
      implicit none
      integer(int32), intent(in) :: op
      type(tree_t), pointer, intent(in) :: left, right
      type(tree_t), pointer :: p

      allocate(p)

      p%op = op
      p%left => left
      p%right => right

   end function make_tree_node


   ! Make a leaf on the syntax tree. 
   function make_atom (seg) result(p)
      implicit none
      type(segment_t), intent(in) :: seg
      type(tree_t), pointer :: p

      allocate(p)
      allocate(p%c(1))

      p%op = op_char
      p%c = seg

   end function make_atom


   function make_atom_char (a,b) result(p)
      implicit none
      character(*), intent(in) :: a
      character(*), intent(in), optional :: b
      type(tree_t), pointer :: p

      allocate(p)
      allocate(p%c(1))

      p%op = op_char
      p%c%min = ichar_utf8(a)
      if (present(b)) then
         p%c%max = ichar_utf8(b)
      else
         p%c%max = ichar_utf8(a)
      end if

   end function make_atom_char


   ! Analysis alternation (X|Y)
   function regex() result(res)
      implicit none
      type(tree_t), pointer :: res
      integer :: void

      res => term()
      do while (current_token == tk_union)
         void = get_token(strbuff)
         res => make_tree_node(op_union, res, term())
      end do
   end function regex

   
   ! Analysis for concatenation
   function term() result(res)
      implicit none
      type(tree_t), pointer :: res

      if (current_token == tk_union &
          .or. current_token == tk_rpar &
          .or. current_token == tk_end) then
         res => make_tree_node(op_empty, null(), null())
      else 
         res => postfix_op()
         do while (current_token /= tk_union &
                   .and. current_token /= tk_rpar &
                   .and. current_token /= tk_end )
            res => make_tree_node(op_concat, res, postfix_op())
         end do 
      end if

   end function term


   ! Postfix Operators
   ! Analysis for repetition: *, +, ?.
   function postfix_op() result(res)
      implicit none
      type(tree_t), pointer :: res, p
      integer :: void

      res => primary()
      if (current_token == tk_star) then
         res => make_tree_node(op_closure, res, null())
         void = get_token(strbuff)

      else if (current_token == tk_plus) then
         res => make_tree_node(op_concat, res, make_tree_node(op_closure, res, null()))
         void = get_token(strbuff)

      else if (current_token == tk_question) then
         res => make_tree_node(op_union, res, make_tree_node(op_empty, res, null()))
         void = get_token(strbuff)
      
      else if (current_token == tk_lcurlybrace) then
         
         p => res
         res => range_min_max(p)
         void = get_token(strbuff) 
      end if

   end function postfix_op 

      ! Analysis for character itself. 
   function primary() result(res)
      implicit none
      type(tree_t), pointer :: res
      integer :: void
      type(segment_t) :: seg

      if (current_token == tk_char) then
         res => make_atom(segment_t(ichar_utf8(token_char), ichar_utf8(token_char)))
         void = get_token(strbuff)

      else if (current_token == tk_lpar) then
         void = get_token(strbuff)
         res => regex()
         if (current_token /= tk_rpar) then
            write(stderr, *) "Close parenthesis is expected."
         end if 
         void = get_token(strbuff)

      else if (current_token == tk_lsbracket) then
         void = get_token(strbuff)
         res => char_class()
         if (current_token /= tk_rsbracket) then
            write(stderr, *) "Close square bracket is expected."
         end if
         void = get_token(strbuff)
      else if (current_token == tk_dot) then
         res => make_atom(SEG_ANY)

         void = get_token(strbuff)

      else if (current_token == tk_backslash) then
         res => shorthand(token_char)
         void = get_token(strbuff)

      else
         write(stderr, *) "Normal character or open parenthesis is expected."
      end if
   end function primary

   function range_min_max(p) result(res)
      implicit none
      type(tree_t), pointer, intent(in) :: p
      type(tree_t), pointer :: res
      character(:), allocatable :: buf
      integer :: void
      integer(int32) :: arg(2), ios, min, max
      integer(int32) :: count

      buf = ''
      arg(:) = 0
      res => null()
      max = 0
      min = 0 

      void = get_token(strbuff)
        
      do while(current_token /= tk_rcurlybrace)
         buf = buf//trim(token_char) 
         void = get_token(strbuff)
      end do

      read(buf, *, iostat=ios) arg(:)

      if (arg(2) == 0) then
         if (buf(len_trim(buf):len_trim(buf)) == ',') then
            min = arg(1)
         else
            min = 1
            max = arg(1)
         end if
      else
         min = arg(1)
         max = arg(2)
      end if

      res => p
      count = min
      res => make_tree_node(op_union, res, make_tree_node(op_empty, res, null()))
      do while (count < max)
         res => make_tree_node(op_union, res, make_tree_node(op_empty, res, null()))
         res => make_tree_node(op_concat, p, res)
         count = count + 1
      end do

      count = 1
      do while (count < min)
         res => make_tree_node(op_concat, res, p)
         count = count + 1
      end do

      if (max == 0) then
         res => make_tree_node(op_concat, res, make_tree_node(op_closure, p, null()))
      end if

      
   end function range_min_max


   function char_class() result(res)
      implicit none
      type(tree_t), pointer :: res
      character(:), allocatable :: buf
      type(segment_t), allocatable :: segment_list(:)
      integer :: void
      integer :: siz, count_hyphen, i, inext, iend, j
      logical :: inverted


      res => null()

      buf = ''
      do while (current_token /= tk_rsbracket)
         iend = idxutf8(token_char, 1) 
         buf = buf//token_char(1:iend)
         void = get_token(strbuff, class=.true.)
      end do

      inverted = .false.
      ! is there '^' at first?
      if (buf(1:1) == HAT) then
         inverted = .true.
         buf = buf(2:len(buf))
      end if 

      siz = len_utf8(buf)

      siz = siz - 2*count_token(buf(2:len_trim(buf)-1), hyphen)

      if (buf(len_trim(buf):len_trim(buf)) == hyphen) siz = siz - 1

      allocate(segment_list(siz))

      iend = len_utf8(buf)
      i = 1
      j = 1
      buf = buf//char(0) ! 空文字を末尾に追加する。

      do while (i <= iend)

         inext = idxutf8(buf, i) + 1

         ! 次の文字がハイフンではないならば
         if (buf(inext:inext) /= hyphen) then
            segment_list(j)%min = ichar_utf8(buf(i:inext-1))
            segment_list(j)%max = ichar_utf8(buf(i:inext-1))

            j = j + 1
         else
            segment_list(j)%min = ichar_utf8(buf(i:inext-1))

            ! 2文字すすめる
            i = inext +1

            inext = idxutf8(buf, i) + 1
            segment_list(j)%max = ichar_utf8(buf(i:inext-1))
            j = j + 1

         end if

         ! 先頭の文字がハイフンならば、処理を分岐する。
         if (j == 1 .and. buf(1:1) == hyphen) then
            segment_list(1)%min = ichar_utf8(hyphen)
            segment_list(1)%max = ichar_utf8(hyphen)
            cycle
         end if

         if (i == iend .and. buf(iend:iend) == hyphen) then
            segment_list(siz)%max = UTF8_CODE_MAX
            exit
         end if

         i = inext 
      end do

      if (inverted) then
         call invert_segment_list(segment_list)
      end if

      allocate(res)
      allocate(res%c(size(segment_list, dim=1)))

      res%c(:) = segment_list(:)
      res%op = op_char

   end function char_class
   

   function make_tree_crlf() result(res)
      implicit none
      type(tree_t), pointer :: res
      type(tree_t), pointer :: cr, lf

      res => null()

      allocate(cr)
      allocate(cr%c(1))
      cr%c(1) = SEG_CR
      cr%op = op_char

      allocate(lf)
      allocate(lf%c(1))
      lf%c(1) = SEG_LF
      lf%op = op_char

      res => make_tree_node(op_union, lf, make_tree_node(op_concat, cr, lf))
   end function


   function shorthand(token) result(res)
      implicit none
      character(4), intent(in) :: token
      type(tree_t), pointer :: res, left, right, left_two

      type(segment_t), allocatable :: segment_list(:)
      type(segment_t) :: seg

      res => null()

      select case (trim(token))
      case (ESCAPE_T); res => make_atom(SEG_TAB);   return
      case (ESCAPE_N)
         ! res => make_atom(SEG_LF);
         res => make_tree_crlf()
         return
      case (ESCAPE_R); res => make_atom(SEG_CR);    return 
      case (ESCAPE_D); res => make_atom(SEG_DIGIT); return 
      case (ESCAPE_D_CAPITAL)
         allocate(segment_list(1))
         segment_list(1) = SEG_DIGIT
         call invert_segment_list(segment_list)

      case (ESCAPE_W)
         allocate(segment_list(4))
         segment_list(1) = SEG_LOWERCASE
         segment_list(2) = SEG_UPPERCASE
         segment_list(3) = SEG_DIGIT
         segment_list(4) = SEG_UNDERSCORE
      
      case (ESCAPE_W_CAPITAL)
         allocate(segment_list(4))
         segment_list(1) = SEG_LOWERCASE
         segment_list(2) = SEG_UPPERCASE
         segment_list(3) = SEG_DIGIT
         segment_list(4) = SEG_UNDERSCORE
         call invert_segment_list(segment_list)

      case (ESCAPE_S)
         allocate(segment_list(6))
         segment_list(1) = SEG_SPACE
         segment_list(2) = SEG_TAB
         segment_list(3) = SEG_CR
         segment_list(4) = SEG_LF
         segment_list(5) = SEG_FF
         segment_list(6) = SEG_ZENKAKU_SPACE
      
      case (ESCAPE_S_CAPITAL)
         allocate(segment_list(6))
         segment_list(1) = SEG_SPACE
         segment_list(2) = SEG_TAB
         segment_list(3) = SEG_CR
         segment_list(4) = SEG_LF
         segment_list(5) = SEG_FF
         segment_list(6) = SEG_ZENKAKU_SPACE
         call invert_segment_list(segment_list)
         
      case default
         seg = segment_t(ichar_utf8(token_char), ichar_utf8(token_char))
         res => make_atom(seg)
         return
      end select

      allocate(res)
      allocate(res%c(size(segment_list, dim=1)))

      res%c(:) = segment_list(:)
      res%op = op_char
   
      deallocate(segment_list)

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
         if (.not. inverted(i-1).and. inverted(i)) then
            list(count)%min = i
         end if
         
         if (inverted(i-1) .and. .not. inverted(i)) then
            list(count)%max = i-1
            count = count + 1
         end if 
      end do
   
   end subroutine invert_segment_list 
  

   recursive subroutine print_tree_internal(p)
      implicit none
      type(tree_t) :: p

      select case (p%op)
      case (op_char)
         write(*, "(a)", advance='no') trim(print_class(p))
      case (op_concat)
         write(*, "(a)", advance='no') "(concatenate "
         call print_tree_internal(p%left)
         write(*, "(a)", advance='no') ' '
         call print_tree_internal(p%right)
         write(*, "(a)", advance='no') ')'
      case (op_union)
         write(*, "(a)", advance='no') "(or "
         call print_tree_internal(p%left)
         write(*, "(a)", advance='no') ' '
         call print_tree_internal(p%right)
         write(*, "(a)", advance='no') ')'
      case (op_closure)
         write(*, "(a)", advance='no') "(closure "
         call print_tree_internal(p%left)
         write(*, "(a)", advance='no') ')'
      case (op_empty)
         write(*, '(a)', advance='no') "EMPTY"
      case default
         write(stderr, *) "This will not hoppen in 'print_tree'"
         error stop
      end select
   end subroutine print_tree_internal


   function print_class (p) result(str)
      implicit none
      type(tree_t), intent(in) :: p
      character(:), allocatable :: str
      character(:), allocatable :: buf
      character(:), allocatable :: c
      integer :: siz, j

      str = ''

      siz = size(p%c, dim=1)

      if (p%c(1) == SEG_LF) then
         str = '<LF>'
         return
      
      else if (p%c(1) == SEG_CR) then
         str = '<CR>'
         return
            
      ! 文字クラスが1文字のみの場合
      else if (siz == 1 .and. p%c(1)%min == p%c(1)%max) then
         str = '"'//char_utf8(p%c(1)%min)//'"'
         return
      end if

      if (siz == 1 .and. p%c(1) == SEG_ANY) then
         str = '<ANY>'
         return
      end if

      buf = '[ '
      do j = 1, siz

         if (p%c(j) == SEG_LF) then
            buf = buf//"<LF>; "
         
         else if (p%c(j) == SEG_TAB) then
            buf = buf//"<TAB>; "
         
         else if (p%c(j) == SEG_CR) then
            buf = buf//"<CR>; "
         
         else if (p%c(j) == SEG_FF) then
            buf = buf//"<FF>; "

         else if (p%c(j) == SEG_SPACE) then
            buf = buf//"<SPACE>; "

         else if (p%c(j) == SEG_ZENKAKU_SPACE) then
            buf = buf//"<ZENKAKU SPACE>; "

         else if (p%c(j)%max == UTF8_CODE_MAX) then
            buf = buf//'"'//char_utf8(p%c(j)%min)//'"-"'//"<U+1FFFFF>"//'; '
         
         else
            buf = buf//'"'//char_utf8(p%c(j)%min)//'"-"'//char_utf8(p%c(j)%max)//'"; '

         end if

      end do
      buf = trim(buf)//']'

      str = trim(buf)


   end function print_class

end module syntax_tree_m