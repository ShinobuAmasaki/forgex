!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     automaton_m module is a part of Forgex.

module automaton_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: segment_m 
   use :: enums_m
   use :: syntax_tree_m
   use :: utf8_m
   implicit none
   private 

   public :: check_NFA_state
   public :: build_nfa
   public :: print_nfa
   public :: disjoin_nfa

   ! Upper limit of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_MAX = 1024
   
   ! Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_VECTOR_SIZE = NFA_STATE_MAX

   ! nlist_t is a type represents a transition on NFA.
   ! It transit to state 'to' by character segment 'c'. 
   type, public :: nlist_t
      type(segment_t) :: c = SEG_EMPTY
      integer(int32) :: to = 0
      type(nlist_t), pointer :: next => null()
      integer(int32) :: index
   end type 

   ! NFA_state_set_t represents set of NFA states.
   type, public :: NFA_state_set_t
      logical :: vec(NFA_VECTOR_SIZE) = .false.
   end type 

   ! A table of transition on NFA.
   ! type(nlist_t), public, target :: nfa(NFA_STATE_MAX)

   ! Initial and accepting state on NFA.
   integer(int32), public :: nfa_entry, nfa_exit

!---------------------------------------------------------------------!

      ! Upper limit for number of DFA states instance.
   integer(int32), parameter :: DFA_STATE_MAX = 1024

   ! D_list_t is the type represents a list of transitionable NFA state.
   type :: D_list_t
      type(segment_t), allocatable :: c(:)
      type(NFA_state_set_t) :: to
      type(D_list_t), pointer :: next => null()
   end type


   ! D_slist_t is the type represents a list of transition destinations
   ! It transition to state 'to' by character segment 'c'.
   type :: D_slist_t
      type(segment_t), allocatable :: c(:)                
      type(D_state_t), pointer :: to => null() 
      type(D_slist_t), pointer :: next => null()    ! pointer to next data
   end type


   ! D_state_t is the type represents a state of DFA.
   type :: D_state_t
      integer(int32) :: index
      type(NFA_state_set_t), pointer :: state => null()  ! a set of NFA states represented by this DFA state.
      logical :: visited = .false.                       ! .true. if already processed.
      logical :: accepted = .false.                      ! .true. if it includes accepting state. 
      type(D_slist_t), pointer :: next => null()         ! list of transition destinations
   end type

!---------------------------------------------------------------------!

   type, public :: automaton_t
      character(:), allocatable :: pattern
      integer(int32) :: nfa_nstate = 0       ! Number of NFA states.
      integer(int32) :: dfa_nstate = 0       ! Number of DFA states.
      type(nlist_t), pointer :: nfa(:)
      type(D_state_t), pointer :: dfa(:)
      type(D_state_t), pointer :: initial_dfa_state => null()
      type(D_list_t), pointer :: dlist => null()
   contains
      procedure :: init
      procedure :: generate_node
      ! NFA procedures 
      procedure :: add_transition
      procedure :: generate_nfa
      procedure :: build_nfa
      procedure :: print_nfa
      procedure :: disjoin => disjoin_nfa
      ! DFA procedures
      procedure :: mark_empty_transition
      procedure :: register_D_state
      procedure :: fetch_unvisited_D_state
      procedure :: collect_empty_transition
      procedure :: compute_reachable_N_state
      procedure :: convert_NFA_to_DFA
      procedure :: print_NFA_state_set
      procedure :: print_dfa
      procedure :: matching
      procedure :: matching_exactly
      procedure :: free => deallocate_automaton
   end type 

   ! Pointer allocation monitoring

   type :: dlist_pointer_list_t
      type(D_list_t), pointer :: node
   end type 
   
   type :: dslist_pointer_list_t
      type(D_slist_t), pointer :: node
   end type

   type :: nlist_pointer_list_t
      type(nlist_t), pointer :: node
   end type

   type :: dstate_pointer_list_t
      type(D_state_t), pointer :: node
   end type

   type(dlist_pointer_list_t) :: dlist_node_list(DFA_STATE_MAX)
   type(dslist_pointer_list_t) :: dslist_node_list(DFA_STATE_MAX)
   type(nlist_pointer_list_t) :: nlist_node_list(NFA_STATE_MAX)
   type(dstate_pointer_list_t) :: dstate_node_list(DFA_STATE_MAX)

   integer(int32) :: dlist_node_count  = 0
   integer(int32) :: dslist_node_count = 0
   integer(int32) :: nlist_node_count  = 0
   integer(int32) :: dstate_node_count = 0
   

contains

   subroutine init(self)
      implicit none
      class(automaton_t) :: self
      integer :: i

      self%nfa_nstate = 0
      self%dfa_nstate = 0

      allocate(self%nfa(NFA_STATE_MAX))
      allocate(self%dfa(DFA_STATE_MAX))

      do i = 1, size(self%dfa, dim=1)
         self%dfa(i)%index = i
      end do 

      do i = 1, size(self%nfa, dim=1)
         self%nfa(i)%index = i
      end do

   end subroutine init


   subroutine build_nfa(self, tree)
      implicit none
      class(automaton_t) :: self
      type(tree_t), intent(in)  :: tree

      nfa_entry = self%generate_node()

      nfa_exit = self%generate_node()

      call self%generate_nfa(tree, nfa_entry, nfa_exit)

      call self%disjoin()

   end subroutine build_nfa


   subroutine deallocate_automaton(self)
      implicit none
      class(automaton_t) :: self
      integer :: j, max 

      max = nlist_node_count
      do j = 1, max
         if (associated(nlist_node_list(j)%node)) then
            deallocate(nlist_node_list(j)%node)
            nlist_node_count = nlist_node_count -1
         end if
      end do

      max = dlist_node_count
      do j = 1, max
         if (associated(dlist_node_list(j)%node)) then
            if (allocated(dlist_node_list(j)%node%c)) then
               deallocate(dlist_node_list(j)%node%c)
            end if

            deallocate(dlist_node_list(j)%node)
            dlist_node_count = dlist_node_count -1
         end if
      end do

      max = dslist_node_count
      do j = 1, max
         if (associated(dslist_node_list(j)%node)) then
            if (allocated(dslist_node_list(j)%node%c)) then
               deallocate(dslist_node_list(j)%node%c)
            end if

            deallocate(dslist_node_list(j)%node)
            dslist_node_count = dslist_node_count -1
         end if
      end do

      max = dstate_node_count
      do j = 1, max
         if (associated(dstate_node_list(j)%node)) then
            deallocate(dstate_node_list(j)%node)
            dstate_node_count = dstate_node_count -1
         end if
      end do

      deallocate(self%nfa)
 
      deallocate(self%dfa)


   end subroutine deallocate_automaton


!=====================================================================!

   function generate_node(self)
      implicit none
      class(automaton_t) :: self
      integer(int32) :: generate_node

      if (self%nfa_nstate >= NFA_STATE_MAX) then
         write(stderr, *) "Number of NFA states too large."
         error stop
      end if

      self%nfa_nstate = self%nfa_nstate + 1 
      generate_node = self%nfa_nstate
      
   end function generate_node


   subroutine add_transition(self, from, to, c)
      implicit none
      class(automaton_t) :: self
      integer(int32), intent(in) :: from, to
      type(segment_t), intent(in) :: c

      type(nlist_t), pointer :: p
      
      p => null()
      allocate(p)

      nlist_node_count = nlist_node_count + 1
      nlist_node_list(nlist_node_count)%node => p
      
      p = self%nfa(from)

      self%nfa(from)%c%min = c%min
      self%nfa(from)%c%max = c%max
      self%nfa(from)%to = to
      self%nfa(from)%next => p

   end subroutine add_transition
      

   recursive subroutine generate_nfa(self, tree, entry, way_out)
      implicit none
      class(automaton_t) :: self
      type(tree_t), intent(in) :: tree
      integer(int32), intent(in) :: entry, way_out

      integer :: a1, a2, j

      select case (tree%op)
      case (op_char)
         do j = 1, size(tree%c, dim=1)
            call self%add_transition(entry, way_out, tree%c(j))
         end do 
     
      case (op_empty)
         call self%add_transition(entry, way_out, SEG_EMPTY)

      case (op_union)
         call self%generate_nfa(tree%left, entry, way_out)
         call self%generate_nfa(tree%right, entry, way_out)
      
      case (op_closure)
         a1 = self%generate_node()
         a2 = self%generate_node()
         call self%add_transition(entry, a1, SEG_EMPTY)
         call self%generate_nfa(tree%left, a1, a2)
         call self%add_transition(a2, a1, SEG_EMPTY)
         call self%add_transition(a1, way_out, SEG_EMPTY)
      
      case (op_concat)
         a1 = self%generate_node()
         call self%generate_nfa(tree%left, entry, a1)
         call self%generate_nfa(tree%right, a1, way_out)
      
      case default
         write(stderr, *) "This will not happen in 'generate_nfa'."
         error stop
      end select

   end subroutine generate_nfa
      

   ! Is the arguement 'state' (set of NFA state) includes state 's'?
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


   subroutine disjoin_nfa(self)
      use :: priority_queue_m
      use :: segment_disjoin_m
      implicit none
      class(automaton_t) :: self
      type(nlist_t), pointer :: p, q
      integer(int32) :: i, j, k 
      type(priority_queue_t) :: queue
      type(segment_t), allocatable :: seg_list(:)
      integer :: num


      num = 0
      p => null()

      block ! enqueue
         do i = 1, self%nfa_nstate
            p => self%nfa(i)

            do while (associated(p))
               if (p%to /= 0 ) then

                  if (p%c /= SEG_EMPTY) call enqueue(queue, p%c)
               end if
               p => p%next
            end do
         end do
      end block ! enqueue

      num = queue%number

      allocate(seg_list(num))
      do j = 1, num
         seg_list(j) = dequeue(queue)
      end do

      !-- seg_list array is sorted.

      call disjoin(seg_list)

      do i = 1, self%nfa_nstate
         p => self%nfa(i)

         if (.not. is_prime_semgment(p%c, seg_list)) then
            call disjoin_nfa_state(p, seg_list)
         end if
      end do

      do i = 1, self%nfa_nstate 

         p => self%nfa(i)%next

         inner: do while (associated(p))

            if (.not. is_prime_semgment(p%c, seg_list)) then                     
               call disjoin_nfa_state(p, seg_list)
            end if

            if (p%index > 0) exit inner

            p => p%next

         end do inner
      end do

      !-- deallocate
      call clear(queue)
      deallocate(seg_list)

   end subroutine disjoin_nfa

   subroutine disjoin_nfa_state(state, seg_list)
      use :: segment_disjoin_m
      implicit none
      type(nlist_t), pointer, intent(inout) ::state 

      type(segment_t), intent(inout) :: seg_list(:)
      
      integer :: j, k, siz
      siz = size(seg_list, dim=1)

      block
         logical :: flag(siz)
         flag = is_overlap_to_seg_list(state%c, seg_list, siz)

         k = 1

         do j = 1, siz
            if (flag(j)) then
               block
                  type(nlist_t), pointer :: ptr
                  ptr => null() 
            
                  if (j == 1) then
                     state%c = seg_list(j)
                  else
                     allocate(ptr)

                     nlist_node_count = nlist_node_count + 1
                     nlist_node_list(nlist_node_count)%node => ptr

                     ptr = state 
                     state%c = seg_list(j)
                     state%to = ptr%to
                     state%next => ptr
                  end if

               end block
            end if 
         end do 
      end block



   end subroutine disjoin_nfa_state 

!==============================================================================!

   subroutine add_NFA_state(state, s)
      implicit none
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in):: s

      state%vec(s) = .true.
   end subroutine add_NFA_state


   recursive subroutine mark_empty_transition(self, state, s)
      use, intrinsic :: iso_c_binding
      implicit none
      class(automaton_t) :: self
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in) :: s 

      type(nlist_t), pointer :: p => null() 

      call add_NFA_state(state, s)
      
      p => self%nfa(s)
      do while (associated(p))

         if (p%c == SEG_EMPTY .and. .not. check_NFA_state(state, p%to) ) then
            if (p%to /= 0) call self%mark_empty_transition(state, p%to)
         end if 

         if (.not. associated(p)) exit
         p => p%next

      enddo 

   end subroutine mark_empty_transition


   subroutine collect_empty_transition (self, state)
      use, intrinsic :: iso_c_binding
      implicit none
      class(automaton_t) :: self
      type(NFA_state_set_t), intent(inout), target :: state
      integer(int32) :: i

      do i = 1, self%nfa_nstate

         if (check_NFA_state(state, i)) call self%mark_empty_transition(state, i)

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


   function register_D_state(self, s) result(res)
      implicit none
      class(automaton_t) :: self
      type(NFA_state_set_t), intent(in), target :: s
      integer(int32) :: i, k
      type(D_state_t), pointer :: res 

      res => null() 

      do i = 1, self%dfa_nstate

         if (equivalent_NFA_state_set(self%dfa(i)%state, s)) then
            res => self%dfa(i)
            return
         end if

      end do

      if (self%dfa_nstate >= DFA_STATE_MAX) then
         write(stderr, '(a)') "Number of DFA states too large.."
         error stop
      end if

      self%dfa_nstate = self%dfa_nstate + 1 

      k = self%dfa_nstate
      
      self%dfa(k)%state => s
      self%dfa(k)%visited = .false.
      self%dfa(k)%accepted = check_NFA_state(s, nfa_exit)
      self%dfa(k)%next => null()

      res => self%dfa(k)

   end function register_D_state


   function fetch_unvisited_D_state(self) result(res)
      implicit none
      class(automaton_t) :: self
      type(D_state_t), pointer :: res
      integer(int32) :: i

      res => null()

      do i = 1, self%dfa_nstate
         if (self%dfa(i)%visited .eqv. .false.) then
            res => self%dfa(i)
            return
         end if
      end do
   end function fetch_unvisited_D_state


   function compute_reachable_N_state(self, dstate) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      class(automaton_t) :: self

      type(D_state_t), intent(in) :: dstate

      type(D_list_t), pointer :: res
      integer(int32):: i, j
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
      outer: do i = 1, self%nfa_nstate

         ! If NFA state <i> is included in DFA state <dstate>, perform the following processing.
         if (check_NFA_state(state, i)) then

            ! Examine all NFA states reachable from NFA state <i> and list them. 
            p => self%nfa(i)

            middle: do while (associated(p))

               ! Except for ε-transition.
               if (p%c /= SEG_EMPTY) then

                  a => res
                  inner: do while(associated(a))
                     
                     do j = 1, size(a%c, dim=1)   
                        if (a%c(j) == p%c .and. p%to /= 0) then
                           call add_NFA_state(a%to, p%to)
                           
                           ! Move to next NFA state
                           p => p%next
                           cycle middle

                        end if
                     end do
                     a => a%next

                  end do inner

                  if (p%to /= 0) then 
                     allocate(b)
                     allocate(b%c(1))
                     
                     dlist_node_count = dlist_node_count +1
                     dlist_node_list(dlist_node_count)%node => b

                     b%c(1) = p%c
                     call add_NFA_state(b%to, p%to)
                     b%next => res
                     res => b
                  end if
                  
               end if

               p => p%next

            end do middle
         
         end if 
      end do outer

   end function compute_reachable_N_state


   subroutine convert_NFA_to_DFA(self)
      implicit none
      class(automaton_t) :: self
      type(NFA_state_set_t), target :: initial_state
      type(D_state_t), pointer :: t
      type(D_list_t), pointer :: x, ptr
      type(D_slist_t), pointer :: p

      integer :: j 

      t => null()
      x => null()
      p => null()

      call add_NFA_state(initial_state, nfa_entry)
      call self%collect_empty_transition(initial_state)

      self%initial_dfa_state => self%register_D_state(initial_state)

      t => self%fetch_unvisited_D_state()
      do while (associated(t))


         t%visited = .true.

         x => self%compute_reachable_N_state(t)

         do while (associated(x))

            call self%collect_empty_transition(x%to)

            allocate(p)

            dslist_node_count = dslist_node_count + 1
            dslist_node_list(dslist_node_count)%node => p

            p%c = x%c

            p%to => self%register_D_state(x%to)
            p%next => t%next
            t%next => p

            x => x%next
         end do

         t => self%fetch_unvisited_D_state()
      end do

   end subroutine convert_NFA_to_DFA

!=====================================================================!

   function next_state_dfa(state, chara) result(res)
      use :: utf8_m
      implicit none
      type(D_state_t), intent(in) :: state
      character(*), intent(in) :: chara
      type(D_slist_t), pointer :: ptr
      type(D_state_t), pointer :: res

      integer :: inext, j

      ptr => state%next
      do while (associated(ptr))
         inext = idxutf8(chara, 1) + 1

         do j = 1, size(ptr%c, dim=1)
            if ( ptr%c(j)%min <= ichar_utf8(chara(1:inext)) .and. ichar_utf8(chara(1:inext)) <= ptr%c(j)%max) then
               res => ptr%to
               return
            end if
         end do
         ptr => ptr%next
      end do


      res => null()
   end function next_state_dfa


   subroutine matching (self, str_arg, from, to)
      use :: utf8_m
      implicit none
      class(automaton_t) :: self
      character(*), intent(in) :: str_arg
      character(:), allocatable :: str 
      integer(int32), intent(inout) :: from, to
      type(D_state_t), pointer :: state

      integer(int32) :: start, next
      integer(int32) :: max_match, i, count

      str = str_arg

      from = 0
      to = 0

      if (str == char(10)//char(10)) then
         str = ''
         state => self%initial_dfa_state
         if (state%accepted) then
            from = 1
            to = 1
         end if

         return
      end if

      ! Match the pattern by shifting one character from the begining of string str.
      ! This loop should be parallelized.
      start = 1
      do while (start < len(str))

         ! Initialize DFA
         max_match = 0
         i = start
         state => self%initial_dfa_state

         !
         do while( associated(state))

            ! 任意の位置の空文字にはマッチさせない
            if (state%accepted .and. i /= start) then
               max_match = i
            end if

            if (i > len(str)) exit

            next = idxutf8(str, i) + 1

            state => next_state_dfa(state, str(i:next-1))

            i = next

         end do

         if (max_match > 1) then
            from = start
            to = max_match -1 
            return
         end if

         start = idxutf8(str, start) + 1
      end do

   end subroutine


   function matching_exactly (self, str) result(res)
      implicit none
      class(automaton_t) :: self
      character(*), intent(in) :: str
      logical :: res

      integer(int32) :: max_match, i, next
      type(D_state_t), pointer :: state

      ! Initialize DFA
      max_match = 0
      i = 1
      state => self%initial_dfa_state

      if (str == '') then
         res = state%accepted
         return
      end if

      do while( associated(state))

         if (state%accepted) then
            max_match = i
         end if

         if (i > len(str)) exit

         next = idxutf8(str, i) + 1

         state => next_state_dfa(state, str(i:next-1))

         i = next

      end do

      if (max_match == len(str)+1) then
         res = .true.
      else
         res = .false.
      end if

   end function matching_exactly

!======================================================================================!

   subroutine print_nfa(self)
      implicit none
      class(automaton_t) :: self 
      integer :: i, j
      type(nlist_t), pointer :: p
      character(:), allocatable :: cache

      write(stderr, *) "--- PRINT NFA ---"

      do i = 1, self%nfa_nstate
         if (i <= self%nfa_nstate) then

            write(stderr, '(a, i3, a)', advance='no') "state ", i, ": "
            p => self%nfa(i)

            do while (associated(p))
               if (p%to /= 0 ) then

                  cache = p%c%print()

                  if (p%c == SEG_EMPTY) cache = '?'
                  
                  write(stderr, "(a, a, a2, i0, a1)", advance='no') "(", trim(cache),", ", p%to, ")"
               end if
               p => p%next
            end do
            write(stderr, *) ''
         end if
      end do

   end subroutine print_nfa


   subroutine print_NFA_state_set (self, p)
      implicit none
      class(automaton_t) :: self 
      type(NFA_state_set_t), intent(in) :: p

      integer(int32) :: i

      do i = 1, self%nfa_nstate
         if (check_NFA_state(p, i)) write(stderr, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine print_NFA_state_set
   

   subroutine print_dfa(self)
      implicit none
      class(automaton_t) :: self
      type(D_slist_t), pointer :: l
      integer(int32) :: i, j
      character(1) :: c_Accepted

      write(stderr,*) "--- PRINT DFA---"

      do i = 1, self%dfa_nstate
         if (self%dfa(i)%accepted) then
            write(stderr, '(i2,a, a)', advance='no') i, 'A', ": "
         else
            write(stderr, '(i2,a, a)', advance='no') i, ' ', ": "
         end if

         l => self%dfa(i)%next
         do while (associated(l))
            do j = 1, size(l%c, dim=1)
               write(stderr, '(a, a, i0, 1x)', advance='no') l%c(j)%print(), '=>', l%to%index
            end do
            l => l%next
         end do
         write(stderr, *) ""
      end do 

      do i = 1, self%dfa_nstate
         if (self%dfa(i)%accepted) then
            write(stderr, '(a, i2, a)', advance='no') "state ", i, 'A = ( '
         else
            write(stderr, '(a, i2, a)', advance='no') "state ", i, '  = ( '
         end if
         call self%print_NFA_state_set(self%dfa(i)%state)
         write(stderr,'(a)') ")"
      end do

   end subroutine print_dfa

end module automaton_m