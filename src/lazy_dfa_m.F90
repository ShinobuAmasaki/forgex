! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_lazy_dfa_m module is a part of Forgex.
!
!! This file contains `dfa_t` class and its type-bound procedures.

!> The `forgex_lazy_dfa_m` module defines the data structure of DFA
!> from NFA. The `dfa_t` is defined as a class representing DFA 
!> which is constructed dynamically with lazy-evaluation.
!> This module was previously named `dfa_m`.
module forgex_lazy_dfa_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: forgex_segment_m
   use :: forgex_enums_m
   use :: forgex_utf8_m
   use :: forgex_nfa_m

   implicit none
   private

   interface free_dlist
      procedure :: lazy_dfa__deallocate_dlist
   end interface
   
   public :: d_state_t
   public:: free_dlist
   
   integer(int32), parameter, public :: DFA_STATE_MAX = 1024


   !> The `d_list_t` is the type represents a list of transitionable NFA state
   !> This type holds a linked list of possible NFA states for a range of input characters.
   !> This is a component of the `dfa_t` type.
   type :: d_list_t
      type(segment_t), allocatable :: c(:)
      type(nfa_state_set_t) :: to
      type(d_list_t), pointer :: next => null()
   end type d_list_t


   !> The `d_state_t` is the type represents a state of DFA.
   !> This type has a set of NFA states that can be constructed by the powerset construction
   !> method as the `nfa_state_set_t` type component, which is internally composed of logical array.
   !> In addition, it has a flag indicating whether it is an accepting state and a list of transitions.
   type :: d_state_t
      integer(int32) :: index
      type(NFA_state_set_t) :: state_set
      logical :: accepted = .false.
      type(d_transition_t), pointer :: transition => null() ! list of transition destination
   end type d_state_t


   !> The `d_transition_t` is the type represents a transition a transition from a DFA state
   !> to the next DFA state.
   !> The set of transitions for a particular DFA state (represented as a node of `d_state_t` type) 
   !> is kept in a linked list. 
   type :: d_transition_t
      type(segment_t), allocatable :: c(:)            ! range of input characters involved in the transition
      type(d_state_t), pointer :: to => null()        ! destination
      type(d_transition_t), pointer :: next => null() ! pointer of next data
   end type d_transition_t


   !> The `dfa_t` class represents a single automaton as a set of DFA states.
   !> A DFA constructed by the powerset method has one initial state and accepting states.
   type, public :: dfa_t
      integer(int32)           :: dfa_nstate        = 0        ! counter
      type(d_state_t), pointer :: states(:)         => null()  ! DFA states of the DFA
      type(nfa_t), pointer     :: nfa               => null()  ! an NFA before powerset construction 
      type(d_state_t), pointer :: initial_dfa_state => null()  ! initial state of the DFA 
         ! Pointer attribute of this component is necessaryto realize a pointer reference to a derived-type component. 
      type(d_list_t), pointer  :: dlist             => null()  ! a linked list of reachable NFA states
   contains
      procedure :: init            => lazy_dfa__init
      procedure :: free            => lazy_dfa__deallocate
      procedure :: register        => lazy_dfa__register
      procedure :: epsilon_closure => lazy_dfa__epsilon_closure
#ifdef DEBUG
      procedure :: print           => lazy_dfa__print
#endif
      procedure :: move            => lazy_dfa__move
      procedure :: construct       => lazy_dfa__construct
      procedure :: is_registered   => lazy_dfa__is_registered
      procedure :: reachable       => lazy_dfa__compute_reachable_n_state
      procedure :: matching        => lazy_dfa__matching
      procedure :: matching_exactly=> lazy_dfa__matching_exactly
   end type dfa_t


!== Array to monitor for allocation to pointer variables
   !> Derived type definition for element that make up the pointer array
   !> for the monitor of the `d_list_t` type. 
   type :: dlist_pointer_list_t
      type(d_list_t), pointer :: node
   end type dlist_pointer_list_t


   !> Derived type definition for element that make up the pointer array
   !> for the monitor of the `d_state_t` type.
   type :: dstate_pointer_list_t
      type(d_state_t), pointer :: node
   end type dstate_pointer_list_t


   !> Derived type definition for element that make up the pointer array
   !> for the monitor of the `d_transition_t` type.
   type :: dtransition_pointer_list_t
      type(d_transition_t), pointer :: node
   end type dtransition_pointer_list_t


   !> The monitor array of the `d_list_t` type.
   type(dlist_pointer_list_t)        :: dlist_pointer_list(DFA_STATE_MAX)
   !> The monitor array of the `d_state_t` type.
   type(dstate_pointer_list_t)       :: dstate_pointer_list(DFA_STATE_MAX)
   !> The monitor array of the `d_transition_t` type.
   type(dtransition_pointer_list_t)  :: dtransition_pointer_list(DFA_STATE_MAX)


#ifndef DEBUG
   !> The number of nodes registered in the monitor array of the `dlist_pointer_list`.
   integer(int32) :: dlist_pointer_count = 0
   !> The number of nodes registered in the monitor array of the `dstate_pointer_list`.
   integer(int32) :: dstate_pointer_count = 0
   !> The number of nodes registered in the monitor array of the `dtransition_pointer_list`.
   integer(int32) :: dtransition_pointer_count = 0
#else
   integer(int32), public :: dlist_pointer_count = 0
   integer(int32), public :: dtransition_pointer_count = 0
   integer(int32), public :: dstate_pointer_count = 0
#endif

contains

   !> The constructor of the `dfa_t` class that initialize DFA by powerset construciton
   !> of the NFA of argument.
   subroutine lazy_dfa__init(self, nfa)
      implicit none
      class(dfa_t), intent(inout)        :: self
      type(nfa_t), intent(in), pointer   :: nfa

      type(d_state_t)                    :: initial
      type(d_state_t), pointer           :: tmp
      type(nfa_state_set_t)              :: nfa_entry_state_set
      type(nfa_state_set_t), allocatable :: initial_closure       ! for computing epsilon closure. 

      integer :: i

      ! Initialize the number of DFA states.
      self%dfa_nstate = 0

      ! Allocate memory for the DFA states array.
      allocate(self%states(DFA_STATE_MAX))

      ! Allocate memory for the initialclosure set.
      allocate(initial_closure)
      initial_closure%vec(:) = .false.
      nfa_entry_state_set%vec(:) = .false.

      ! Assign unique indices to each DFA state.
      do i = 1, size(self%states, dim=1)
         self%states(i)%index = i
      end do

      ! Associate the provided NFA with the component of DFA
      self%nfa => nfa

      ! Add the entry state of the NFA to the entry state set.
      call add_nfa_state(nfa_entry_state_set, nfa_entry)
      
      ! Compute the epsilon closure of the entry state set and store in `initial_closure`.
      call self%epsilon_closure(nfa_entry_state_set, initial_closure)

      ! Allocate memory for the initial DFA state.
      allocate(self%initial_dfa_state)

      ! Perform a deep copy of the `initial_closure` set to the `initial%state_set`.
      initial%state_set = initial_closure

      ! Determine if the `initial_state` is an accepting state.
      initial%accepted = check_NFA_state(initial%state_set, nfa_exit)
      
      ! Register the initial state set and assign it to the initial DFA state.
      tmp => self%register(initial%state_set)
      self%initial_dfa_state = tmp  ! Perform a deep copy

      ! Deallocate the initial closure set.
      deallocate(initial_closure)
   end subroutine lazy_dfa__init


   !> Deallocates all nodes registered in the monitor pointer arrays.
   subroutine  lazy_dfa__deallocate(self)
      implicit none
      class(dfa_t), intent(inout) :: self
      integer :: j, max

      ! Deallocate the initial dfa state node if it is associated.
      if (associated(self%initial_dfa_state)) then
         deallocate(self%initial_dfa_state)
      end if

      ! Deallocate all nodes in the `dlist_pointer_list` array.
      max = dlist_pointer_count
      do j = 1, max
         if (associated(dlist_pointer_list(j)%node)) then
            ! Deallocate the character segment array if it is allocated.
            if (allocated(dlist_pointer_list(j)%node%c)) then
               deallocate(dlist_pointer_list(j)%node%c) 
            end if

            ! Deallocate the node itself.
            deallocate(dlist_pointer_list(j)%node)
            ! Decrement the count of allocated `dlist` pointers.
            dlist_pointer_count = dlist_pointer_count -1
         end if
      end do

      ! Deallocate all nodes in the `dtransition_pointer_list` array.
      max = dtransition_pointer_count
      do j = 1, max
         if (associated(dtransition_pointer_list(j)%node)) then
            ! Deallocate the character segment array if it is allocated.
            if (allocated(dtransition_pointer_list(j)%node%c)) then
               deallocate(dtransition_pointer_list(j)%node%c)
            end if

            ! Deallocate the node itself.
            deallocate(dtransition_pointer_list(j)%node)
            ! Decrement the count of allocated dtransition pointers.
            dtransition_pointer_count = dtransition_pointer_count -1
         end if
      end do

      ! Nullify all nodes in the `dstate_pointer_list` array.
      max = dstate_pointer_count
      do j = 1, max
         if (associated(dstate_pointer_list(j)%node)) then
            nullify (dstate_pointer_list(j)%node) ! DO NOT use `deallocate`

            ! Decrement the count of allocated dstate pointers.
            dstate_pointer_count = dstate_pointer_count - 1
         end if
      end do 

      ! Deallocate the array of DFA states if it is associated.
      if (associated(self%states)) deallocate(self%states)
   end subroutine lazy_dfa__deallocate


   !> This subroutine deallocates all the node in the monitor array.
   subroutine lazy_dfa__deallocate_dlist
      implicit none
      integer :: j, max
      
      ! Get the current count of allocated dlist pointers.
      max = dlist_pointer_count
      do j = 1, max
         ! Check if the node is associated.
         if (associated(dlist_pointer_list(j)%node)) then
            ! Deallocate the character segment array if it is allocated.
            if (allocated(dlist_pointer_list(j)%node%c)) then
               deallocate(dlist_pointer_list(j)%node%c)
            end if
            ! Deallocate the node itself.
            deallocate(dlist_pointer_list(j)%node)
            ! Decrement the count of allocated `dlist` pointers.
            dlist_pointer_count = dlist_pointer_count -1
         end if
      end do
   end subroutine lazy_dfa__deallocate_dlist


   !> Take `nfa_state_set_t` as input and register the set as the DFA state in the DFA.  
   !> The result is returned as a pointer to the DFA state.
   function lazy_dfa__register(self, set) result(res)
      implicit none
      class(dfa_t), intent(inout)       :: self
      type(nfa_state_set_t), intent(in) :: set
      
      integer(int32)           :: i, k
      type(d_state_t), pointer :: res

      res => null()
      
      ! If the set is already registered, returns a pointer to the corresponding DFA state.
      if (self%is_registered(set, i)) then
         res => self%states(i)
         return
      end if

      ! Execute an error stop statement if the counter exceeds a limit. 
      if (self%dfa_nstate >= DFA_STATE_MAX) then
         write(stderr, '(a)') "ERROR: Number of DFA states too large."
         error stop
      end if

      self%dfa_nstate = self%dfa_nstate + 1 ! count up
      k = self%dfa_nstate                   ! Assigning to a short variable

      ! Register the NFA state set as a DFA state in the k-th element of the array component.
      self%states(k)%state_set = set
      self%states(k)%accepted = check_NFA_state(set, nfa_exit)
      self%states(k)%transition => null()   
         ! At this point the new DFA state has no transition (due to lazy evaluation).

      ! Also register this in the monitor array. 
      dstate_pointer_count = dstate_pointer_count + 1
      dstate_pointer_list(dstate_pointer_count)%node => self%states(k)

      ! Return a pointer reference to the registered DFA state.
      res => self%states(k)
   end function lazy_dfa__register


!=====================================================================!

   !> Compute the ε-closure for a set of NFA states.
   !>
   !> The ε-closure is the set of NFA states reachable from a given set of NFA states via ε-transition.
   !> This subroutine calculates the ε-closure and stores it in the `closure` parameter.
   subroutine lazy_dfa__epsilon_closure (self, state_set, closure)
      implicit none
      class(dfa_t),          intent(in)    :: self
      type(nfa_state_set_t), intent(in)    :: state_set
      type(nfa_state_set_t), intent(inout) :: closure

      type(nlist_t), pointer :: t
      integer(int32) :: i

      ! Initialize the `closure` with the input state set.
      closure = state_set

      ! Iterate over all NFA states.
      do i = 1, self%nfa%nfa_nstate
         ! Get the list of transitions for the current state.
         t => self%nfa%states(i)

         ! Process each transition in the list.
         do while(associated(t))
            ! Check if the transition is an ε-transition and leads to a valid state.
            if (t%c == SEG_EMPTY .and. t%to /= 0) then
               ! If the transition starts from the NFA entry state, add the destination state to the `closure`.
               if (t%index == nfa_entry) call add_NFA_state(closure, t%to)
            end if
            
            ! Move to the next transition in the list.
            t => t%next
         end do
      end do
   end subroutine lazy_dfa__epsilon_closure

   
   !> This function calculates a set of possible NFA states from the current DFA state by the input
   !> character `symbol`. 
   !>
   !> It scans through the NFA states and finds the set of reachable states by the given input `symbol`, excluding ε-transitions.
   function lazy_dfa__compute_reachable_n_state(self, current, symbol) result(res)
      implicit none
      class(dfa_t),    intent(in) :: self
      type(d_state_t), intent(in) :: current
      character(*),    intent(in) :: symbol

      type(d_list_t), pointer :: res

      type(nfa_state_set_t)   :: state_set         ! A set of NFA state
      type(nlist_t),  pointer :: ptr_nlist         ! Pointer to list of NFA states
      type(d_list_t), pointer :: a, b
      type(segment_t)         :: symbol_belong(1)  ! Holds the segment to which the symbol belongs
      integer(int32)          :: i, j

      ! Initialize variables
      symbol_belong = SEG_EMPTY
      ptr_nlist => null()
      a => null()
      b => null()
      res => null()
      state_set = current%state_set
   
      ! nfa状態をスキャン
      ! Scan through NFA states
      outer: do i = 1, self%nfa%nfa_nstate

         ! state_setのi番目が真ならば、states(i)のポインタをたどる
         ! If the `i`-th element fo state_set is true, follow the pointer of states(i)
         if (check_NFA_state(state_set, i)) then

            ! この状態へのポインタをptr_nlistに代入
            ! Assign pointer to this state to `ptr_nlist`.
            ptr_nlist => self%nfa%states(i)

            ! ptr_nlistをたどる
            ! Traverse through the list of transtions.
            middle: do while(associated(ptr_nlist))

               ! ! Exclude ε-transitions
               if (ptr_nlist%c /= SEG_EMPTY) then

                  a => res
                  inner: do while(associated(a))

                     ! Check if the current transition segment matches the input symbol segment
                     do j = 1, size(a%c, dim=1)
                        if (a%c(j) == ptr_nlist%c .and. ptr_nlist%to /= 0) then    
                           ! Add the destination NFA state to the result set.                     
                           call add_NFA_state(a%to, ptr_nlist%to)

                           ! Move to the next transition in the list.
                           ptr_nlist => ptr_nlist%next
                           cycle middle

                        end if
                     end do

                     ! Move to the next transition in the result set.
                     a => a%next

                  end do inner

               end if

               ! ptr_nlistの行き先がある場合
               ! If there is a valid destination state,
                if (ptr_nlist%to /= 0) then

                  ! ptr_nlist%cにsymbolが含まれる場合
                  ! check if the symbol belongs to the transition segment
                  if((symbol_to_segment(symbol) .in. ptr_nlist%c).or.(ptr_nlist%c == SEG_EMPTY)) then

                     ! symbolの属するsegmentを取得する
                     ! Get the segment to which the symbol belongs.
                     symbol_belong = which_segment_symbol_belong(self%nfa%all_segments, symbol)

                     ! Allocate a new `d_list_t` derived-type.
                     allocate(b)
                     allocate(b%c(1))

                     ! Update the monitor array and count.
                     dlist_pointer_count = dlist_pointer_count + 1
                     dlist_pointer_list(dlist_pointer_count)%node => b

                     ! Store the segment and add the destination state.
                     b%c(1) = symbol_belong(1)
                     call add_nfa_state(b%to, ptr_nlist%to)

                     ! resの先頭に挿入する
                     ! Insert the new object at the head of the result list.
                     b%next => res
                     res => b
                        
                  end if
               end if

               ! 次のnfa状態へ
               ! Move to the next transition in the list.
               ptr_nlist => ptr_nlist%next
            end do middle

         end if
      end do outer
   end function lazy_dfa__compute_reachable_n_state


   !> Returns `.true.` if the set of NFA states is already registered.
   !> It scans through the DFA states to check if the given NFA states set
   !> is already present in the DFA. If found, it sets the optional index parameter
   !> to the position of the registered state. 
   logical function lazy_dfa__is_registered(self, state_set, idx) result(res)
      implicit none
      class(dfa_t),             intent(in)    :: self
      type(nfa_state_set_t),    intent(in)    :: state_set
      integer(int32), optional, intent(inout) :: idx

      logical :: tmp
      integer :: i, n
      
      ! Initialize the result to `.false.`
      res = .false.
      tmp = .true.
      n = dstate_pointer_count ! Store the value into a short varibale. 

      ! Scan all DFA states.
      do i = 1, n
         ! 入力の集合と、登録された集合が等しいかどうかを比較して`tmp`に結果を格納する。
         ! Compare the input `state_set` with the registered state sets and store the result in `tmp`.
         tmp = equivalent_NFA_state_set(self%states(i)%state_set, state_set)
         ! Update the result with the logical OR of current result and `tmp`.
         res = res .or. tmp ! 論理和をとる

         if (res) then
            ! If true, return this function.
            if(present(idx)) idx = i  ! Store index infomation in optional arguments.
            return
         end if 
      end do
   end function lazy_dfa__is_registered


   ! 現在のDFA状態から、入力シンボルに対して、遷移可能ならば遷移する。
   !> This funciton performs a transition form the `current` DFA state based on the input `symbol`.
   !> It scans through the array of DFA states to find the state that can be reached from
   !> the `current` state for the given symbol, and returns a reference to the resulting state.
   function lazy_dfa__move(self, current, symbol) result(res)
      implicit none 
      class(dfa_t),    intent(inout) :: self
      type(d_state_t), intent(in)    :: current
      character(*),    intent(in)    :: symbol

      type(d_list_t), pointer :: res
      integer(int32) :: i

      res => null()  ! Initialize

      ! Scan the array of DFA states.
      do i = 1, self%dfa_nstate
         ! Get the reachable state for the `current` state and `symbol`.
         res => self%reachable(current, symbol) ! 
         ! If a reachable state is found, return the reference.
         if (associated(res)) return
      end do
   end function lazy_dfa__move


   !> This subroutine constructs a DFA state transition for a given `symbol` from the `current` state to the `destination` state.
   !>
   !> It calculates the set of NFA states that can be reached from the `current` state for the given `symbol`, 
   !> excluding epsilon transitions, and then registers the new DFA state if it has not already been registered.
   !> Finally, it adds the transition from the `current` state to the `destination` state in the DFA.
   subroutine lazy_dfa__construct(self, current, destination, symbol)
      implicit none
      class(dfa_t),             intent(inout) :: self
      type(d_state_t), target,  intent(in)    :: current
      type(d_state_t), pointer, intent(inout) :: destination
      character(*),             intent(in)    :: symbol

      type(d_state_t), pointer      :: prev, next
      type(d_list_t), pointer       :: x
      type(d_list_t)                :: without_epsilon
      type(segment_t), allocatable  :: all_segments(:)
      integer(int32) :: i

      ! Initialize pointers to `null`.
      x => null()
      prev => null()
      next => null()
      destination => null()

      ! Implicit array reallocation for `all_segments`
      all_segments = self%nfa%all_segments

      ! 遷移前の状態へのポインタをprevに代入
      ! Assign the pointer to the current state to prev.
      prev => current

      ! ε遷移を除いた行き先のstate_setを取得する
      ! Get the state set for the destination excluding epsilon-transition.
      x => self%move(prev, symbol)

      if (associated(x)) then
         ! Reduce the list to a single state set.
         x%to = dlist_reduction(x)
         ! Deep copy of the state list excluding epsilon-transition.
         without_epsilon = x ! deep copy
      else
         ! If no transition is found, set `next` to `null` and return.
         next => null()
         return
      end if

      ! ε遷移との和集合を取り、x%toに格納する
      ! Combine the state set with epsilon-transitions and store in `x%to`.
      call self%nfa%collect_empty_transition(x%to)
 
      if (.not. self%is_registered(x%to)) then
         ! まだDFA状態が登録されていない場合
         ! If the state is not registered, register it.
         next => self%register(x%to)
         ! Add a DFA transition from `prev` to `next` for the given `symbol`.
         call add_dfa_transition(prev, which_segment_symbol_belong(all_segments, symbol), next)   
      else
         ! 登録されている場合
         ! If the state is already registered.
         if (self%is_registered(x%to, i)) then
            ! Set `next` to the registered state.
            next => self%states(i)
         else
            ! Register the state excluding epsilon-transitions.
            next => self%register(without_epsilon%to)
         end if

         ! Add a DFA transition from `prev` to `next` for the given `symbol`.
         call add_dfa_transition(prev, which_segment_symbol_belong(all_segments, symbol), next)
      end if

      ! Assign the pointer to the `destination` state. 
      destination => next
   end subroutine lazy_dfa__construct


!=====================================================================!
!  Matching procedures
!     ...should we extract them into a separate module?

   subroutine lazy_dfa__matching(self, str_arg, from, to)
      use :: forgex_utf8_m
      implicit none
      class(dfa_t), intent(inout) :: self
      character(*), intent(in) :: str_arg
      integer(int32), intent(inout) :: from, to 

      type(d_state_t), pointer :: current
      type(d_state_t), pointer :: destination
      character(:), allocatable:: str
      integer(int32) :: start, next
      integer(int32) :: max_match, i

      nullify(current)
      nullify(destination)
      ! Initialize
      str = str_arg
      from = 0
      to = 0

      current => self%initial_dfa_state
      if (.not. associated(current)) then
         error stop
      end if

      if (str == char(10)//char(10)) then

         str = ''
         if (current%accepted) then
            from = 1
            to = 1
         end if
         return
      end if
      ! Match the pattern by shifting one character from the beginning of string str.
      ! This loop should be parallelized.
      start = 1
      do while(start < len(str))
         
         ! Initialize DFA
         max_match = 0
         i = start
         current => self%initial_dfa_state
         do while( associated(current))


            ! 任意の位置の空文字には一致させない
            if (current%accepted .and. i /= start) then
               max_match = i
            end if
   
            if (i > len(str)) exit

            next = idxutf8(str, i) + 1

            call self%construct(current, destination, str(i:next-1))
            current => destination
            
            i = next
         end do

         if (max_match > 1) then
            from = start
            to = max_match -1
            return
         end if

         start = idxutf8(str,start) + 1
      end do
   end subroutine lazy_dfa__matching


   !> This function returns `.true.` when the entire text string is accepted by DFA,
   !> otherwise returns `.false.`.
   function lazy_dfa__matching_exactly(self, str) result(res)
      implicit none
      class(dfa_t), intent(inout) :: self
      character(*), intent(in)    :: str     ! an input character string.
      logical :: res

      type(d_state_t), pointer :: current
      type(d_state_t), pointer :: destination

      integer(int32) :: max_match
      integer(int32) :: i
      integer(int32) :: next

      ! Initialize the pointers.
      nullify(current)
      nullify(destination)

      ! Initialize the pointer
      current => self%initial_dfa_state

      !! If the DFA does not have an initial state, execute an `error stop` statement.
      if (.not. associated(current)) then
         error stop
      end if

      !! If the input string is an empty string, returns a logical value indicating 
      !! whether the current state is accepting or not.
      if (len(str) == 0) then
         res = current%accepted
         return
      end if
      
      ! Initialize the loop variables.
      max_match = 0
      i = 1
      ! Loop as long as the current state is associated.
      do while (associated(current))

         ! If the current state acceptable, the value of `max_match` is updated with `i`.
         if (current%accepted) then
            max_match = i
         end if

         ! If the index of the string larger than the length, exit the loop.
         if (i > len(str)) exit

         ! Get the index of the next character and assign it to `next`.
         next = idxutf8(str, i) + 1

         ! Lazy evaluation is performed by calling this procedure here. 
         ! The transition destination is stored in `destination`.
         call self%construct(current, destination, str(i:next-1))

         ! Update the current state. 
         current => destination

         ! !If the `current` is not associated, exit the loop.
         if (.not. associated(current)) exit    ! Is this statement necessary? 

         ! Update the index to the index of the next character. 
         i = next
      end do

      ! If the maximum index of the match is one larger than the lenght of the string,
      ! the function returns `.true.`, otherwise it returns `.false.`
      if (max_match == len(str)+1) then
         res = .true.
      else
         res = .false.
      end if

      ! Finalize
      nullify(current)
   end function lazy_dfa__matching_exactly
      

!=====================================================================!
!  Helper procedures

   !> This function does nothing if the input symbol is already in the
   !> list of transition of the state, otherwise it registers a new transition in the list. 
   subroutine add_dfa_transition(state, symbols, destination)
      implicit none
      type(d_state_t), pointer, intent(inout) :: state            ! a DFA state
      type(segment_t),          intent(in)    :: symbols(:)
          ! input symbols represented as segments
      type(d_state_t), pointer, intent(in)    :: destination
         ! the destinational DFA state of current state.
      type(d_transition_t), pointer           :: new_transition
         ! A pointer to the location where the new transition is allocated.

      type(d_transition_t), pointer :: p
      integer(int32) :: i, j

      ! Initialize
      nullify(p)

      !! Is the symbol included in existing transitions?
      ! About the transitions that the current state object has,
      p => state%transition
      do while(associated(p))

         ! scan the segment array of the transition currently in focus.
         do i = 1, size(p%c)
            do j = 1, size(symbols)
               ! If the j-th symbol is conteined in it, return immediately. 
               if (symbols(j) .in. p%c(i)) return
            end do 
         end do
         p => p%next
      end do

      !! If the question is false, allocate an new transition into `new_transition` variable. 
      allocate(new_transition)
      !  Allocate a segment array to `new_transition` whose size is equal to the size of the segment array.
      allocate(new_transition%c(size(symbols)))

      !! Here, it is registered into the monitor array of `dtransition_pointer_list`.
      dtransition_pointer_count = dtransition_pointer_count + 1
      dtransition_pointer_list(dtransition_pointer_count)%node => new_transition

      ! Loading the new transition with values that define itself.
      do j = 1, size(symbols)
         new_transition%c(j) = symbols(j)
      end do
      new_transition%to => destination
      new_transition%next => state%transition  ! Add to the beginning of the linked list of the state.
      state%transition => new_transition
   end subroutine add_dfa_transition


   !> This function convert an input symbol into the segment corresponding it.
   function symbol_to_segment(symbol) result(res)
      use :: forgex_segment_m
      implicit none
      character(*), intent(in) :: symbol
      type(segment_t)          :: res

      integer(int32) :: i, i_end, code

      ! Initialize indices
      i = 1
      i_end = idxutf8(symbol, i)

      ! Get the code point of the input character.
      code = ichar_utf8(symbol(i:i_end))

      ! Create a segment corresponding to the code, and return it.
      res = segment_t(code, code)
   end function symbol_to_segment
   

   !> This function takes an array of segments and a character as arguments,
   !> and returns the segment as rank=1 array to which symbol belongs
   !> (included in the segment interval).
   function which_segment_symbol_belong (segments, symbol) result(res)
      implicit none
      type(segment_t), intent(in) :: segments(:)
      character(*),    intent(in) :: symbol
      type(segment_t)             :: res(1)

      integer         :: i, i_end, j
      type(segment_t) :: target_for_comparison

      ! Initialize indices.
      i = 1
      i_end = idxutf8(symbol, i)

      ! The target to check for inclusion.
      target_for_comparison = symbol_to_segment(symbol(i:i_end))

      ! Scan the segments array. 
      do j = 1, size(segments)
         ! Compare segments and return the later element of the segments, which contains the target segment.
         if (target_for_comparison .in. segments(j)) then
            res = segments(j)
            return
         end if
      end do

      ! If not found, returns SEG_EMPTY.
      res = SEG_EMPTY
   end function which_segment_symbol_belong


   !> This function performs reduction operation on the logical sum of the transition destinations of `dlist`.
   function dlist_reduction(dlist) result(res)
      implicit none
      type(d_list_t), pointer, intent(in) :: dlist
      type(nfa_state_set_t)               :: res

      type(d_list_t), pointer             :: p
      
      ! Initialize the scanning pointer
      nullify(p)
      p => dlist

      ! Initialize the return value so that all of its components have the value `.false.`
      res%vec(:) = .false.

      ! Scan the linked-list of the dlist argument.
      do while(associated(p))
         ! If the segment of the `dlist` is not `SEG_EMPTY`,
         if (.not. p%c(1) == SEG_EMPTY) then
            ! take logical OR of each individual element of `nfa_state_set_t`.
            res%vec(:) = res%vec(:) .or. p%to%vec(:) 
         end if
         p => p%next
      end do
   end function dlist_reduction

!=====================================================================!
! Procedures for Debugging

#ifdef DEBUG
   subroutine dump_d_list(dlist)
      implicit none
      type(d_list_t), intent(in), target :: dlist
      type(d_list_t), pointer :: ptr

      integer :: i
      i = 1
      ptr => dlist 
      do while(associated(ptr))
         write(stderr, *) "dump dlist: ",i,  dlist%to%vec(1:6)
         i = i +1
         ptr => dlist%next
      end do 
   end subroutine dump_d_list


   subroutine dump_n_list(nlist)
      implicit none
      type(nlist_t), intent(in), target :: nlist
      type(nlist_t), pointer :: ptr

      integer :: i

      nullify(ptr)
      i = 1
      ptr => nlist
      do while(associated(ptr))
         write(stderr, *) "dump nlist: ", ptr%c%print(), ptr%to
         i = i+1
         ptr => ptr%next
      end do
   end subroutine dump_n_list


   subroutine lazy_dfa__print(self)
      implicit none
      class(dfa_t), intent(in) :: self
      type(d_transition_t), pointer :: p
      integer(int32) :: i, j

      write(stderr,*) "--- PRINT DFA---"

      do i = 1, self%dfa_nstate
         if (self%states(i)%accepted) then
            write(stderr, '(i2,a, a)', advance='no') i, 'A', ": "
         else
            write(stderr, '(i2,a, a)', advance='no') i, ' ', ": "
         end if

         p => self%states(i)%transition
         do while (associated(p))
            do j = 1, size(p%c, dim=1)
               write(stderr, '(a, a, i0, 1x)', advance='no') p%c(j)%print(), '=>', p%to%index
            end do
            p => p%next
         end do
         write(stderr, *) ""
      end do 

      do i = 1, self%dfa_nstate
         if (self%states(i)%accepted) then
            write(stderr, '(a, i2, a)', advance='no') "state ", i, 'A = ( '
         else
            write(stderr, '(a, i2, a)', advance='no') "state ", i, '  = ( '
         end if
         call self%nfa%print_state_set(self%states(i)%state_set)
         write(stderr,'(a)') ")"
      end do
   end subroutine lazy_dfa__print
#endif

end module forgex_lazy_dfa_m