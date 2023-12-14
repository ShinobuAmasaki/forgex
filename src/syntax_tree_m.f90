module syntax_tree_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
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

   ! These enums will be rewritten in Fortran 2023's enumerator feature. 
   enum, bind(c)
      enumerator :: tk_char = 0
      enumerator :: tk_union
      enumerator :: tk_lpar
      enumerator :: tk_rpar
      enumerator :: tk_question
      enumerator :: tk_star
      enumerator :: tk_plus
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
      character(3) :: c = EMPTY
      type(tree_t), pointer :: left => null()
      type(tree_t), pointer :: right => null()
   end type

   integer(int32) :: current_token
   character(3) :: token_char = EMPTY
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

   function get_token(str) result(res)
      use :: utf8_m
      implicit none
      character(*), intent(in) :: str
      integer(int32), save :: idx = 1
      integer(int32) :: next_idx
      integer(int32) :: res
      character(3) :: c

      if (idx == len_trim(str)+1) then
         current_token = tk_end
      else

         next_idx = iutf8(str, idx) + 1

         c = str(idx:next_idx-1)
         idx = next_idx

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
         case default
            current_token = tk_char
            token_char = c
         end select
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
   function make_atom (c) result(p)
      implicit none
      character(3), intent(in) :: c
      type(tree_t), pointer :: p

      allocate(p)

      p%op = op_char
      p%c = c

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


   ! Analysis for character itself. 
   function primary() result(res)
      implicit none
      type(tree_t), pointer :: res
      integer :: void

      if (current_token == tk_char) then
         res => make_atom(token_char)
         void = get_token(strbuff)
      else if (current_token == tk_lpar) then
         void = get_token(strbuff)
         res => regex()
         if (current_token /= tk_rpar) then
            write(stderr, *) "Close parenthesis is expected."
         end if 
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
         write(*, "(a)", advance='no') '"'//trim(p%c)//'"'
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


end module syntax_tree_m