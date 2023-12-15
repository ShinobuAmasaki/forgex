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

   public :: op_char
   public :: op_concat
   public :: op_union
   public :: op_closure
   public :: op_empty
   
   public :: tree_t
   public :: parse_regex
   public :: print_tree
   
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
      enumerator :: tk_lsbracket ! left square bracket
      enumerator :: tk_rsbracket ! right square bracket 
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

   type(segment_t), public :: SEG_EMPTY

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

               c = str(idx:idxutf8(str, idx))
               token_char = c
            
            case ('[')
               current_token = tk_lsbracket
            case (']')
               current_token = tk_rsbracket

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
   function make_atom (a, b) result(p)
      implicit none
      character(*), intent(in) :: a, b
      type(tree_t), pointer :: p

      allocate(p)
      allocate(p%c(1))

      p%op = op_char
      p%c%min = ichar_utf8(a)
      p%c%max = ichar_utf8(b)

   end function make_atom


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
      type(tree_t), pointer :: res
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
      end if

   end function postfix_op 

   function char_class() result(res)
      implicit none
      type(tree_t), pointer :: res
      character(9) :: buf
      type(segment_t), allocatable :: segment_list(:)
      integer :: void
      integer :: siz, count_hyphen, i, inext, iend, j

      res => null()

      buf = ''
      do while (current_token /= tk_rsbracket)
         buf = trim(buf)//token_char
         void = get_token(strbuff, class=.true.)
      end do 

      siz = len_trim_utf8(buf)

      siz = siz - 2*count_token(buf(2:len_trim(buf)-1), hyphen)

      if (buf(1:1) == hyphen)  siz = siz - 1
      if (buf(len_trim(buf):len_trim(buf)) == hyphen) siz = siz - 1

      allocate(segment_list(siz))

      iend = len_trim(buf)
      i = 1
      j = 1
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

         if (j == 1 .and. buf(1:1) == hyphen) then
            segment_list(1)%min = UTF8_CODE_MIN
            ! 一文字すすめる
            i = inext
            inext = idxutf8(buf, i) + 1
            segment_list(1)%max = ichar_utf8(buf(i:inext-1))
            ! もう1文字すすめる
            i = idxutf8(buf, i) + 1 
            j = j + 1
            cycle
         end if

         if (i == iend .and. buf(iend:iend) == hyphen) then
            segment_list(siz)%max = UTF8_CODE_MAX
            exit
         end if

         i = inext 
      end do

      allocate(res)
      allocate(res%c(siz))

      res%c(:) = segment_list(:)
      res%op = op_char

   end function 
   
 
   ! Analysis for character itself. 
   function primary() result(res)
      implicit none
      type(tree_t), pointer :: res
      integer :: void

      if (current_token == tk_char) then
         res => make_atom(token_char, token_char)
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
         res => make_atom(char_utf8(UTF8_CODE_MIN), char_utf8(UTF8_CODE_MAX))

         void = get_token(strbuff)

      else if (current_token == tk_backslash) then
         res => make_atom(token_char, token_char)
         void = get_token(strbuff)

      else
         write(stderr, *) "Normal character or open parenthesis is expected."
      end if
   end function primary
   

   recursive subroutine print_tree_internal(p)
      implicit none
      type(tree_t) :: p

      select case (p%op)
      case (op_char)
         write(*, "(a)", advance='no') '"'//trim(print_class(p))//'"'
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
      character(1024) :: str
      character(:), allocatable :: buf
      integer :: siz, j

      str = ''

      siz = size(p%c, dim=1)

      ! 文字クラスが1文字のみの場合
      if (siz == 1 .and. p%c(1)%min == p%c(1)%max) then
         str = char_utf8(p%c(1)%min)
         return
      end if

      if (siz == 1 .and. p%c(1)%min == UTF8_CODE_MIN .and. p%c(1)%max == UTF8_CODE_MAX) then
         str = "ANY"
         return
      end if

      buf = '[ '
      do j = 1, siz

         buf = buf//char_utf8(p%c(j)%min)//'-'//char_utf8(p%c(j)%max)//'; '

      end do
      buf = trim(buf)//']'

      str = trim(buf)


   end function print_class

end module syntax_tree_m