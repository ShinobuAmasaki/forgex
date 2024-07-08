!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023-2024
!!     A regular expression engine for Fortran.
!!     forgex_dfa_m module is a part of Forgex.
!!
module forgex_dfa_m
   use, intrinsic :: iso_fortran_env, stderr => error_unit
   use :: forgex_segment_m
   use :: forgex_enums_m
   use :: forgex_utf8_m
   use :: forgex_nfa_m
   implicit none
   
   private

!---------------------------------------------------------------------!

   ! Upper limit for number of DFA states instance.
   integer(int32), parameter, public :: DFA_STATE_MAX = 1024

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

   type, public :: dfa_t
      integer(int32) :: dfa_nstate = 0
      type(D_state_t), pointer :: dfa(:)
      type(D_state_t), pointer :: initial_dfa_state => null()
   contains
      procedure :: init              => dfa__init
      procedure :: free              => dfa__deallocate
      procedure :: register          => dfa__register_d_state
      procedure :: compute_reachable => dfa__compute_reachable_n_state
      procedure :: convert_n2d       => dfa__convert_NFA_to_DFA
      procedure :: fetch_unvisited   => dfa__fetch_unvisited_d_state
      procedure :: matching          => dfa__matching
      procedure :: matching_exactly  => dfa__matching_exactly
      procedure :: print             => dfa__print
   end type dfa_t 

   ! Pointer allocation monitoring
   type :: dlist_pointer_list_t
      type(D_list_t), pointer :: node
   end type 
   
   type :: dslist_pointer_list_t
      type(D_slist_t), pointer :: node
   end type

   type :: dstate_pointer_list_t
      type(D_state_t), pointer :: node
   end type

   type(dlist_pointer_list_t) :: dlist_node_list(DFA_STATE_MAX)
   type(dslist_pointer_list_t) :: dslist_node_list(DFA_STATE_MAX)
   type(dstate_pointer_list_t) :: dstate_node_list(DFA_STATE_MAX)

   integer(int32) :: dlist_node_count  = 0
   integer(int32) :: dslist_node_count = 0
   integer(int32) :: dstate_node_count = 0

contains

   subroutine dfa__init(self)
      implicit none
      class(dfa_t) :: self
      integer :: i

      self%dfa_nstate = 0
      allocate(self%dfa(DFA_STATE_MAX))

      do i = 1, size(self%dfa, dim=1)
         self%dfa(i)%index = i
      end do 
   end subroutine dfa__init


   subroutine dfa__deallocate(self)
      implicit none
      class(dfa_t) :: self
      integer :: j, max 

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

      if (associated(self%dfa)) nullify (self%dfa)

   end subroutine dfa__deallocate


   function dfa__register_d_state(self, s) result(res)
      implicit none
      class(dfa_t) :: self
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

   end function dfa__register_d_state

   function dfa__compute_reachable_N_state(self, nfa, dstate) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      class(dfa_t) :: self
      type(nfa_t), intent(in)  :: nfa

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
      outer: do i = 1, nfa%nfa_nstate

         ! If NFA state <i> is included in DFA state <dstate>, perform the following processing.
         if (check_NFA_state(state, i)) then

            ! Examine all NFA states reachable from NFA state <i> and list them. 
            p => nfa%states(i)

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

   end function dfa__compute_reachable_N_state


   function dfa__fetch_unvisited_d_state(self) result(res)
      implicit none
      class(dfa_t) :: self
      type(D_state_t), pointer :: res
      integer(int32) :: i

      res => null()

      do i = 1, self%dfa_nstate
         if (self%dfa(i)%visited .eqv. .false.) then
            res => self%dfa(i)
            return
         end if
      end do
   end function dfa__fetch_unvisited_d_state

   
   subroutine dfa__convert_NFA_to_DFA(self, nfa)
      implicit none
      class(dfa_t) :: self
      type(nfa_t)  :: nfa
      type(NFA_state_set_t), target :: initial_state
      type(D_state_t), pointer :: t
      type(D_list_t), pointer :: x
      type(D_slist_t), pointer :: p

      t => null()
      x => null()
      p => null()

      call add_NFA_state(initial_state, nfa_entry)
      call nfa%collect_empty_transition(initial_state)

      self%initial_dfa_state => self%register(initial_state)

      t => self%fetch_unvisited()
      do while (associated(t))


         t%visited = .true.

         x => self%compute_reachable(nfa, t)

         do while (associated(x))

            call nfa%collect_empty_transition(x%to)

            allocate(p)

            dslist_node_count = dslist_node_count + 1
            dslist_node_list(dslist_node_count)%node => p

            p%c = x%c

            p%to => self%register(x%to)
            p%next => t%next
            t%next => p

            x => x%next
         end do

         t => self%fetch_unvisited()
      end do

   end subroutine dfa__convert_NFA_to_DFA


   subroutine dfa__print(self, nfa)
      implicit none
      class(dfa_t), intent(in) :: self
      type(nfa_t), intent(in)  :: nfa
      type(D_slist_t), pointer :: l
      integer(int32)           :: i, j

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
         call nfa%print_state_set(self%dfa(i)%state)
         write(stderr,'(a)') ")"
      end do

   end subroutine dfa__print

!=========================================================================!

   function next_state_dfa(state, chara) result(res)
      use :: forgex_utf8_m
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


   subroutine dfa__matching (self, str_arg, from, to)
      use :: forgex_utf8_m
      implicit none
      class(dfa_t) :: self
      character(*), intent(in) :: str_arg
      character(:), allocatable :: str 
      integer(int32), intent(inout) :: from, to
      type(D_state_t), pointer :: state

      integer(int32) :: start, next
      integer(int32) :: max_match, i

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

   end subroutine dfa__matching


   function dfa__matching_exactly (self, str) result(res)
      implicit none
      class(dfa_t) :: self
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

   end function dfa__matching_exactly



end module forgex_dfa_m