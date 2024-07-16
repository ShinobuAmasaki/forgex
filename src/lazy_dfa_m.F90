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

      ! Initialize
      self%dfa_nstate = 0
      allocate(self%states(DFA_STATE_MAX))
      allocate(initial_closure)
      initial_closure%vec(:) = .false.
      nfa_entry_state_set%vec(:) = .false.

      ! Indexing of DFA states
      do i = 1, size(self%states, dim=1)
         self%states(i)%index = i
      end do

      ! Associate a reference to the NFA of an argument to the derived-type component.
      self%nfa => nfa

      ! Using `nfa_entry_state_set` as input, calculate the ε-closure and store
      ! the result in `initial_closure`.
      call add_nfa_state(nfa_entry_state_set, nfa_entry)
      
      ! Compute epsilon closure 
      call self%epsilon_closure(nfa_entry_state_set, initial_closure)

      ! Create the initial state of the DFA
      allocate(self%initial_dfa_state)

      ! Do DEEP copy
      initial%state_set = initial_closure
      initial%accepted = check_NFA_state(initial%state_set, nfa_exit)
      
      tmp => self%register(initial%state_set)
      self%initial_dfa_state = tmp  ! Do DEEP copy

      deallocate(initial_closure)
   end subroutine lazy_dfa__init

   !> Deallocates all nodes registered in the monitor pointer arrays.
   subroutine  lazy_dfa__deallocate(self)
      implicit none
      class(dfa_t), intent(inout) :: self
      integer :: j, max

      ! Deallocate the initial node.
      if (associated(self%initial_dfa_state)) then
         deallocate(self%initial_dfa_state)
      end if

      ! 
      max = dlist_pointer_count
      do j = 1, max
         if (associated(dlist_pointer_list(j)%node)) then
            if (allocated(dlist_pointer_list(j)%node%c)) then
               deallocate(dlist_pointer_list(j)%node%c)
            end if
            deallocate(dlist_pointer_list(j)%node)
            dlist_pointer_count = dlist_pointer_count -1
         end if
      end do

      max = dtransition_pointer_count
      do j = 1, max
         if (associated(dtransition_pointer_list(j)%node)) then
            if (allocated(dtransition_pointer_list(j)%node%c)) then
               deallocate(dtransition_pointer_list(j)%node%c)
            end if

            deallocate(dtransition_pointer_list(j)%node)
            dtransition_pointer_count = dtransition_pointer_count -1
         end if
      end do

      max = dstate_pointer_count

      do j = 1, max
         if (associated(dstate_pointer_list(j)%node)) then
            nullify (dstate_pointer_list(j)%node) ! NOT deallocate
            dstate_pointer_count = dstate_pointer_count - 1
         end if
      end do 

      if (associated(self%states)) deallocate(self%states)
   end subroutine lazy_dfa__deallocate


   subroutine lazy_dfa__deallocate_dlist
      implicit none
      integer :: j, max
      
      max = dlist_pointer_count
      do j = 1, max
         if (associated(dlist_pointer_list(j)%node)) then
            if (allocated(dlist_pointer_list(j)%node%c)) then
               deallocate(dlist_pointer_list(j)%node%c)
            end if
            deallocate(dlist_pointer_list(j)%node)
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
   subroutine lazy_dfa__epsilon_closure (self, state_set, closure)
      implicit none
      class(dfa_t),          intent(in)    :: self
      type(nfa_state_set_t), intent(in)    :: state_set
      type(nfa_state_set_t), intent(inout) :: closure

      type(nlist_t), pointer :: t
      integer(int32) :: i

      closure = state_set

      do i = 1, self%nfa%nfa_nstate
         t => self%nfa%states(i)

         do while(associated(t))
            if (t%c == SEG_EMPTY .and. t%to /= 0) then
               if (t%index == nfa_entry) call add_NFA_state(closure, t%to)
            end if
            t => t%next
         end do
      end do
   end subroutine lazy_dfa__epsilon_closure

   
   !> Calculate a set of possible NFA states from the current DFA state by the input
   !> character `symbol`. 
   function lazy_dfa__compute_reachable_n_state(self, current, symbol) result(res)
      implicit none
      class(dfa_t),    intent(in) :: self
      type(d_state_t), intent(in) :: current
      character(*),    intent(in) :: symbol

      type(d_list_t), pointer :: res

      type(nfa_state_set_t)   :: state_set         ! a set of NFA state
      type(nlist_t),  pointer :: ptr_nlist         ! 
      type(d_list_t), pointer :: a, b
      type(segment_t)         :: symbol_belong(1)  ! Holds the segment to which the symbol belongs
      integer(int32)          :: i, j

      ! Initialize
      symbol_belong = SEG_EMPTY
      ptr_nlist => null()
      a => null()
      b => null()
      res => null()
      state_set = current%state_set
   
      ! nfa状態をスキャン
      outer: do i = 1, self%nfa%nfa_nstate

         ! state_setのi番目が真ならば、states(i)のポインタをたどる
         if (check_NFA_state(state_set, i)) then

            ! この状態へのポインタをptr_nlistに代入
            ptr_nlist => self%nfa%states(i)

            ! ptr_nlistをたどる
            middle: do while(associated(ptr_nlist))

               ! ! Except for ε-transition.
               if (ptr_nlist%c /= SEG_EMPTY) then

                  a => res
                  inner: do while(associated(a))

                     do j = 1, size(a%c, dim=1)   
                        if (a%c(j) == ptr_nlist%c .and. ptr_nlist%to /= 0) then                         
                           call add_NFA_state(a%to, ptr_nlist%to)

                           ! Move to next NFA state
                           ptr_nlist => ptr_nlist%next
                           cycle middle

                        end if
                     end do
                     a => a%next

                  end do inner

               end if

               ! ptr_nlistの行き先がある場合
                if (ptr_nlist%to /= 0) then

                  ! ptr_nlist%cにsymbolが含まれる場合
                  if((symbol_to_segment(symbol) .in. ptr_nlist%c).or.(ptr_nlist%c == SEG_EMPTY)) then

                     ! symbolの属するsegmentを取得する
                     symbol_belong = which_segment_symbol_belong(self%nfa%all_segments, symbol)

                     allocate(b)
                     allocate(b%c(1))

                     dlist_pointer_count = dlist_pointer_count + 1
                     dlist_pointer_list(dlist_pointer_count)%node => b

                     b%c(1) = symbol_belong(1)
                     call add_nfa_state(b%to, ptr_nlist%to)
                     ! resの先頭に挿入する
                     b%next => res
                     res => b
                        
                  end if
               end if

               ! 次のnfa状態へ
               ptr_nlist => ptr_nlist%next
            end do middle

         end if
      end do outer
   end function lazy_dfa__compute_reachable_n_state


   ! Returns `.true.` if the set of NFA states is already registered. 
   logical function lazy_dfa__is_registered(self, state_set, idx) result(res)
      implicit none
      class(dfa_t),             intent(in)    :: self
      type(nfa_state_set_t),    intent(in)    :: state_set
      integer(int32), optional, intent(inout) :: idx

      logical :: tmp
      integer :: i, n
      
      ! Initialize
      res = .false.
      tmp = .true.
      n = dstate_pointer_count ! Store the value into a short varibale. 

      ! Scan all DFA states.
      do i = 1, n
         ! 入力の集合と、登録された集合が等しいかどうかを比較して`tmp`に結果を格納する。
         tmp = equivalent_NFA_state_set(self%states(i)%state_set, state_set)
         res = res .or. tmp ! 論理和をとる

         if (res) then
            ! 真の場合、ループを抜ける
            if(present(idx)) idx = i  ! Store index infomation in optional arguments.
            return
         end if 
      end do
   end function lazy_dfa__is_registered


   ! 現在のDFA状態から、入力シンボルに対して、遷移可能ならば遷移する。
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
         res => self%reachable(current, symbol) ! 
         if (associated(res)) return ! Returns a reference to the destination DFA state.
      end do
   end function lazy_dfa__move


   subroutine lazy_dfa__construct(self, current, destination, symbol)
      use :: forgex_utf8_m
      implicit none
      class(dfa_t), intent(inout) :: self
      type(d_state_t), target, intent(in) :: current
      type(d_state_t), intent(inout), pointer :: destination
      character(*), intent(in) :: symbol

      type(d_state_t), pointer :: prev, next
      type(d_list_t), pointer :: x
      type(d_list_t) :: without_epsilon
      type(segment_t), allocatable :: all_segments(:)
      integer(int32) :: i

      x => null()
      prev => null()
      next => null()
      destination => null()

      ! Implicit array reallocation
      all_segments = self%nfa%all_segments

      ! 遷移前の状態へのポインタをprevに代入
      prev => current

      ! ε遷移を除いた行き先のstate_setを取得する
      x => self%move(prev, symbol)

      if (associated(x)) then
         x%to = dlist_reduction(x)
         without_epsilon = x ! deep copy
      else
         next => null()
         return
      end if

      ! ε遷移との和集合を取り、x%toに格納する
      call self%nfa%collect_empty_transition(x%to)
 
      if (.not. self%is_registered(x%to)) then

         ! まだDFA状態が登録されていない場合
         next => self%register(x%to)
         call add_dfa_transition(prev, which_segment_symbol_belong(all_segments, symbol), next)   
      else
         ! 登録されている場合
         if (self%is_registered(x%to, i)) then

            next => self%states(i)
         else

            next => self%register(without_epsilon%to)
         end if

         call add_dfa_transition(prev, which_segment_symbol_belong(all_segments, symbol), next)
      end if

      destination => next
   end subroutine lazy_dfa__construct

!=====================================================================!
!  Matching procedures
!     ...should I extract them into a separate module?

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


   function lazy_dfa__matching_exactly(self, str) result(res)
      implicit none
      class(dfa_t), intent(inout) :: self
      character(*), intent(in)    :: str
      logical :: res

      integer(int32) :: max_match, i, next
      type(d_state_t), pointer :: current
      type(d_state_t), pointer :: destination

      nullify(current)
      nullify(destination)

      ! Initialize
      max_match = 0
      i = 1
      current => self%initial_dfa_state
      if (.not. associated(current)) then
         error stop
      end if

      if (len(str) == 0) then
         res = current%accepted
         return
      end if
      
      do while (associated(current))
         if (current%accepted) then
            max_match = i
         end if

         if (i > len(str)) exit


         next = idxutf8(str, i) + 1
         call self%construct(current, destination, str(i:next-1))

         current => destination

         if (.not. associated(current)) exit

         i = next
      end do

      nullify(current)
      if (max_match == len(str)+1) then
         res = .true.
      else
         res = .false.
      end if
   end function lazy_dfa__matching_exactly
      

!=====================================================================!
!  Helper procedures

   subroutine add_dfa_transition(state, symbols, destination)
      implicit none
      type(d_state_t), pointer, intent(inout) :: state
      type(segment_t), intent(in) :: symbols(:)
      type(d_state_t), pointer, intent(in) :: destination
      type(d_transition_t), pointer :: new_transition

      integer(int32) :: i, j
      type(d_transition_t), pointer :: p

      p => state%transition
      do while(associated(p))
         do i = 1, size(p%c)
            do j = 1, size(symbols)
               if (symbols(j) .in. p%c(i)) return
            end do 
         end do
         p => p%next
      end do

      allocate(new_transition)
      allocate(new_transition%c(size(symbols)))

      dtransition_pointer_count = dtransition_pointer_count + 1
      dtransition_pointer_list(dtransition_pointer_count)%node => new_transition

      do j = 1, size(symbols)
         new_transition%c(j) = symbols(j)
      end do
      new_transition%to => destination
      new_transition%next => state%transition
      state%transition => new_transition
   end subroutine add_dfa_transition


   function symbol_to_segment(symbol) result(res)
      use :: forgex_segment_m
      implicit none
      character(*), intent(in) :: symbol
      type(segment_t) :: res

      integer(int32) :: i, i_end

      i = 1
      i_end = idxutf8(symbol, i)
      res = segment_t(ichar_utf8(symbol(i:i_end)), ichar_utf8(symbol(i:i_end)))
   end function symbol_to_segment
   

   ! rank=1 のsegment_t型配列を返す関数
   function which_segment_symbol_belong (segments, symbol) result(res)
      implicit none
      type(segment_t), intent(in) :: segments(:)
      character(*), intent(in)    :: symbol
      type(segment_t) :: res(1)

      integer :: i, i_end, j
      type(segment_t) :: symbol_s_t
      logical :: is_belong

      i = 1
      i_end = idxutf8(symbol, i)

      symbol_s_t = symbol_to_segment(symbol(i:i_end))

      do j = 1, size(segments)
         is_belong = symbol_s_t .in. segments(j)
         if (is_belong) then
            res = segments(j)
            return
         end if
      end do
      res = SEG_EMPTY
   end function which_segment_symbol_belong


   function dlist_reduction(dlist) result(res)
      implicit none
      type(d_list_t), pointer, intent(in) :: dlist
      type(d_list_t), pointer :: p
      type(nfa_state_set_t) :: res
      p => null()
      p => dlist

      res%vec(:) = .false.

      do while(associated(p))
         if (.not. p%c(1) == SEG_EMPTY) then
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