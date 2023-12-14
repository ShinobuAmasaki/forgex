!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!  
module forgex
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   implicit none

   integer(int32), parameter :: NFA_STATE_MAX = 128
   integer(int32), parameter :: NFA_VECTOR_SIZE = NFA_STATE_MAX
   integer(int32), parameter :: DFA_STATE_MAX = 100

   character(3), parameter :: EMPTY = char(0)

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
      ! enumerator :: 
      ! enumerator :: 
   end enum

   type :: tree_t
      integer(int32) :: op ! operator
      character(3) :: c = EMPTY
      type(tree_t), pointer :: left => null()
      type(tree_t), pointer :: right => null()
   end type

   type :: nlist_t
      character(3) :: c = EMPTY
      integer(int32) :: to
      type(nlist_t), pointer :: next => null()  
   end type 

   type :: NFA_state_set_t
      logical :: vec(NFA_VECTOR_SIZE) = .false.
   end type 

   type :: D_list_t
      character(3) :: c = EMPTY
      type(NFA_state_set_t) :: to
      type(D_list_t), pointer :: next => null()
   end type

   type :: D_slist_t
      character(3) :: c = EMPTY
      type(D_state_t), pointer :: to => null()
      type(D_slist_t), pointer :: next => null()
   end type

   type :: D_state_t
      integer(int32) :: index
      type(NFA_state_set_t), pointer :: state => null() 
      logical :: visited
      logical :: accepted
      type(D_slist_t), pointer :: next => null() 
   end type 

   integer(int32) :: current_token
   character(3) :: token_char = EMPTY
   character(:), allocatable, target :: strbuff

   type(nlist_t), target :: nfa(NFA_STATE_MAX)
   integer(int32) :: nfa_entry, nfa_exit
   integer(int32) :: nfa_nstate = 0

   integer(int32) :: dfa_nstate = 0
   type(D_state_t), target :: dfa(DFA_STATE_MAX)
   type(D_state_t), pointer :: initial_dfa_state => null()

   
contains

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
   

   recursive subroutine print_tree(p)
      implicit none
      type(tree_t) :: p

      select case (p%op)
      case (op_char)
         write(*, "(a)", advance='no') '"'//trim(p%c)//'"'
      case (op_concat)
         write(*, "(a)", advance='no') "(concatenate "
         call print_tree(p%left)
         write(*, "(a)", advance='no') ' '
         call print_tree(p%right)
         write(*, "(a)", advance='no') ')'
      case (op_union)
         write(*, "(a)", advance='no') "(or "
         call print_tree(p%left)
         write(*, "(a)", advance='no') ' '
         call print_tree(p%right)
         write(*, "(a)", advance='no') ')'
      case (op_closure)
         write(*, "(a)", advance='no') "(closure "
         call print_tree(p%left)
         write(*, "(a)", advance='no') ')'
      case (op_empty)
         write(*, '(a)', advance='no') "EMPTY"
      case default
         write(stderr, *) "This will not hoppen in 'print_tree'"
         error stop
      end select
   end subroutine print_tree


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

      print *, "--- PRINT TREE ---"
      call print_tree(t)
      print *, ''
   end function parse_regex

!=====================================================================!

   function generate_node()
      implicit none
      integer(int32) :: generate_node

      if (nfa_nstate >= NFA_STATE_MAX) then
         write(stderr, *) "Number of NFA states too large."
         error stop
      end if

      nfa_nstate = nfa_nstate + 1 
      generate_node = nfa_nstate
      
   end function generate_node


   subroutine add_transition(from, to, c)
      implicit none
      integer(int32), intent(in) :: from, to
      character(3), intent(in) :: c

      type(nlist_t), pointer :: p
      
      p => null()
      allocate(p)
      
      p = nfa(from)

      ! nfa(from)%is_active = .true.
      nfa(from)%c = c
      nfa(from)%to = to
      nfa(from)%next => p

   end subroutine add_transition
      

   recursive subroutine generate_nfa(tree, entry, way_out)
      implicit none
      type(tree_t), intent(in) :: tree
      integer(int32), intent(in) :: entry, way_out

      integer :: a1, a2

      select case (tree%op)
      case (op_char)
         call add_transition(entry, way_out, tree%c)
     
      case (op_empty)
         call add_transition(entry, way_out, EMPTY)

      case (op_union)
         call generate_nfa(tree%left, entry, way_out)
         call generate_nfa(tree%right, entry, way_out)
      
      case (op_closure)
         a1 = generate_node()
         a2 = generate_node()
         call add_transition(entry, a1, EMPTY)
         call generate_nfa(tree%left, a1, a2)
         call add_transition(a2, a1, EMPTY)
         call add_transition(a1, way_out, EMPTY)
      
      case (op_concat)
         a1 = generate_node()
         call generate_nfa(tree%left, entry, a1)
         call generate_nfa(tree%right, a1, way_out)
      
      case default
         write(stderr, *) "This will not happen in 'generate_nfa'."
         error stop
      end select

   end subroutine generate_nfa


   subroutine build_nfa(tree)
      implicit none
      type(tree_t), intent(in)  :: tree

      nfa_entry = generate_node()

      nfa_exit = generate_node()

      call generate_nfa(tree, nfa_entry, nfa_exit)

   end subroutine build_nfa
      
   subroutine print_nfa()
      implicit none
      integer :: i, j
      type(nlist_t), pointer :: p
      character(3) :: chara

      print *, "--- PRINT NFA ---"

      do i = 1, NFA_STATE_MAX
         if (i <= nfa_nstate) then
            write(*, '(a, i3, a)', advance='no') "state ", i, ": "
            p => nfa(i)
            do while (associated(p))
               if (p%to /= 0 ) then
                  chara = p%c
                  if (chara == char(0)) chara = '?'
                  write(*, "(a, a, a2, i0, a1)", advance='no') "(", trim(chara),", ", p%to, ")"
               end if
               p => p%next
            end do
            write(*, *) ''
         end if
      end do

   end subroutine print_nfa
         
   logical function check_NFA_state(state, s)
      implicit none
      type(NFA_state_set_t), intent(in) :: state
      integer(int32) :: s
      
      if (s /= 0) then 
         check_NFA_state = state%vec(s)
      else
         check_NFA_state = .false. 
      end if
      
   end function check_NFA_state

!=====================================================================!

   subroutine dfa_initialize()
      implicit none
      integer :: i, siz

      siz = size(dfa, dim=1)

      do i = 1, siz
         dfa(i)%index = i
      end do

   end subroutine dfa_initialize


   subroutine print_NFA_state_set (p)
      implicit none
      type(NFA_state_set_t), intent(in) :: p

      integer(int32) :: i

      do i = 1, nfa_nstate
         if (check_NFA_state(p, i)) write(*, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine print_NFA_state_set

   subroutine print_dfa()
      implicit none

      type(D_slist_t), pointer :: l
      integer(int32) :: i, j
      character(1) :: c_Accepted

      print *, "--- PRINT DFA---"

      do i = 1, dfa_nstate
         if (dfa(i)%accepted) then
            write(*, '(i2,a, a)', advance='no') i, 'A', ": "
         else
            write(*, '(i2,a, a)', advance='no') i, ' ', ": "
         end if

         l => dfa(i)%next
         do while (associated(l))
            write(*, '(a, a, i0, 1x)', advance='no') trim(l%c), '=>', l%to%index
            l => l%next
         end do
         write(*, *) ""
      end do 

      do i = 1, dfa_nstate
         if (dfa(i)%accepted) then
            write(*, '(a, i2, a)', advance='no') "state ", i, 'A = ( '
         else
            write(*, '(a, i2, a)', advance='no') "state ", i, '  = ( '
         end if
         call print_NFA_state_set(dfa(i)%state)
         write(*,'(a)') ")"
      end do

   end subroutine print_dfa


   subroutine add_NFA_state(state, s)
      implicit none
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in):: s

      state%vec(s) = .true.
   end subroutine add_NFA_state


   recursive subroutine mark_empty_transition(state, s)
      use, intrinsic :: iso_c_binding
      implicit none
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in) :: s 

      type(nlist_t), pointer :: p => null() 

      call add_NFA_state(state, s)
      
      p => nfa(s)
      do while (associated(p))

         if (p%c == EMPTY .and. .not. check_NFA_state(state, p%to) ) then
            if (p%to /= 0) call mark_empty_transition(state, p%to)
         end if 

         if (.not. associated(p)) exit
         p => p%next

      enddo 

   end subroutine mark_empty_transition


   subroutine collect_empty_transition (state)
      use, intrinsic :: iso_c_binding
      implicit none
      type(NFA_state_set_t), intent(inout), target :: state
      integer(int32) :: i

      do i = 1, nfa_nstate
         if (check_NFA_state(state, i)) call mark_empty_transition(state, i)
      end do
   end subroutine collect_empty_transition


   function equivalent_NFA_state_set(a, b) result(res)
      implicit none
      type(NFA_state_set_t), intent(in), pointer  :: a
      type(NFA_state_set_t), intent(in)  :: b
      integer(int32) :: i
      logical :: res


      do i = 1, NFA_VECTOR_SIZE
         if (a%vec(i) .neqv. b%vec(i)) then
            res = .false.
            return
         end if
      end do
      res = .true.

   end function equivalent_NFA_state_set


   function register_D_state(s) result(res)
      implicit none
      type(NFA_state_set_t), intent(in), target :: s
      integer(int32) :: i
      type(D_state_t), pointer :: res 

      res => null() 

      do i = 1, dfa_nstate

         if (equivalent_NFA_state_set(dfa(i)%state, s)) then
            res => dfa(i)
            return
         end if

      end do

      if (dfa_nstate >= DFA_STATE_MAX) then
         write(stderr, '(a)') "Number of DFA states too large.."
         error stop
      end if

      dfa_nstate = dfa_nstate + 1 

      dfa(dfa_nstate)%state => s
      dfa(dfa_nstate)%visited = .false.
      dfa(dfa_nstate)%accepted = check_NFA_state(s, nfa_exit)
      dfa(dfa_nstate)%next => null()

      res => dfa(dfa_nstate)

   end function register_D_state


   function fetch_unvisited_D_state() result(res)
      implicit none
      type(D_state_t), pointer :: res
      integer(int32) :: i

      res => null()

      do i = 1, dfa_nstate
         if (dfa(i)%visited .eqv. .false.) then
            res => dfa(i)
            return
         end if
      end do
   end function fetch_unvisited_D_state


   function compute_reachable_N_state(dstate) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(D_state_t), intent(in) :: dstate

      type(D_list_t), pointer :: res
      integer(int32):: i
      type(NFA_state_set_t), pointer :: state
      type(nlist_t), pointer :: p
      type(D_list_t), pointer :: b
      type(D_list_t), pointer :: a

      p => null()
      a => null()
      b => null()
      res => null() 
      state => dstate%state

      ! Iterate the all NFA states
      outer: do i = 1, nfa_nstate

         ! If NFA state <i> is included in DFA state <dstate>, perform the following processing.
         if (check_NFA_state(state, i)) then

            ! Examine all NFA states reachable from NFA state <i> and list them. 
            p => nfa(i)
            middle: do while (associated(p))

               ! Except for ε-transition.
               if (p%c /= EMPTY) then

                  a => res
                  inner: do while(associated(a))
                     
                     if (a%c == p%c) then
                        call add_NFA_state(a%to, p%to)
                        
                        ! Move to next NFA state
                        p => p%next
                        cycle middle

                     end if
                     a => a%next

                  end do inner

                  allocate(b)
                  b%c = p%c
                  call add_NFA_state(b%to, p%to)
                  b%next => res
                  res => b
                  
               end if

               p => p%next

            end do middle
         
         end if 
      end do outer

   end function compute_reachable_N_state

   
   subroutine convert_NFA_to_DFA()
      implicit none
      
      type(NFA_state_set_t), target :: initial_state
      type(D_state_t), pointer :: t
      type(D_list_t), pointer :: x
      type(D_slist_t), pointer :: p


      t => null()
      x => null()
      p => null()

      call dfa_initialize

      call add_NFA_state(initial_state, nfa_entry)
      call collect_empty_transition(initial_state)

      initial_dfa_state => register_D_state(initial_state)

      t => fetch_unvisited_D_state()
      do while (associated(t))


         t%visited = .true.

         x => compute_reachable_N_state(t)
         do while (associated(x))

            call collect_empty_transition(x%to)

            allocate(p)
            p%c = x%c

            p%to => register_D_state(x%to)
            p%next => t%next
            t%next => p

            x => x%next
         end do 

         t => fetch_unvisited_D_state()
      end do

      call print_dfa()

   end subroutine convert_NFA_to_DFA




end module forgex