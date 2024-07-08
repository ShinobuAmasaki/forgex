!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023-2024
!!     A regular expression engine for Fortran.
!!     forgex_lazy_dfa_m module is a part of Forgex.
!!
module forgex_lazy_dfa_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: forgex_segment_m
   use :: forgex_enums_m
   use :: forgex_utf8_m
   use :: forgex_nfa_m
   use :: forgex_dfa_m, only: DFA_STATE_MAX
   implicit none
   private

   public :: d_state_t

   ! d_list_t is the type represents a list of transitionable NFA state
   type :: d_list_t
      type(segment_t), allocatable :: c(:)
      type(nfa_state_set_t) :: to
      type(d_list_t), pointer :: next => null()
   end type d_list_t

   ! d_state_t is the type represents a state of DFA.
   type :: d_state_t
      integer(int32) :: index
      type(NFA_state_set_t), pointer :: state_set => null()
      ! logical :: visited = .false. 
      logical :: accepted = .false.
      type(d_transition_t), pointer :: transition => null() ! list of transition destination
   end type d_state_t

   ! d_transition_t
   type :: d_transition_t
      type(segment_t), allocatable :: c(:)
      type(d_state_t), pointer :: to => null()
      type(d_transition_t), pointer :: next => null() ! pointer of next data
   end type d_transition_t

   type, public :: dfa_t
      integer(int32) :: dfa_nstate = 0
      type(d_state_t), pointer :: states(:)
      type(nfa_t), pointer :: nfa
      type(d_state_t), pointer :: initial_dfa_state => null()
      type(d_list_t), pointer :: dlist => null()
   contains
      procedure :: init            => lazy_dfa__init
      procedure :: free            => lazy_dfa__deallocate
      procedure :: register        => lazy_dfa__register
      procedure :: epsilon_closure => lazy_dfa__epsilon_closure
      procedure :: print           => lazy_dfa__print
      procedure :: move            => lazy_dfa__move
      procedure :: construct       => lazy_dfa__construct 
      ! procedure :: fetch_unvisited => lazy_dfa__fetch_unvisited
      procedure :: is_registered   => lazy_dfa__is_registered
      procedure :: reachable       => lazy_dfa__compute_reachable_n_state
   end type dfa_t


!= Array to monitor for allocation to pointer variables
   type :: dlist_pointer_list_t
      type(d_list_t), pointer :: node
   end type dlist_pointer_list_t

   type :: dstate_pointer_list_t
      type(d_state_t), pointer :: node
   end type dstate_pointer_list_t

   type :: dtransition_pointer_list_t
      type(d_transition_t), pointer :: node
   end type dtransition_pointer_list_t

   type(dlist_pointer_list_t)        :: dlist_pointer_list(DFA_STATE_MAX)
   type(dstate_pointer_list_t)       :: dstate_pointer_list(DFA_STATE_MAX)
   type(dtransition_pointer_list_t)  :: dtransition_pointer_list(DFA_STATE_MAX)

   integer(int32) :: dlist_pointer_count = 0
   integer(int32) :: dtransition_pointer_count = 0
   integer(int32) :: dstate_pointer_count = 0

contains

   subroutine lazy_dfa__init(self, nfa)
      implicit none
      class(dfa_t) :: self
      type(nfa_t), intent(in), pointer :: nfa
      type(nfa_state_set_t) :: nfa_entry_state_set
      type(nfa_state_set_t), allocatable :: initial_closure
      integer :: i

      self%dfa_nstate = 0
      allocate(self%states(DFA_STATE_MAX))

      do i = 1, size(self%states, dim=1)
         self%states(i)%index = i
      end do

      self%nfa => nfa

      allocate(initial_closure)
      initial_closure%vec(:) = .false.
      nfa_entry_state_set%vec(:) = .false.

      call add_nfa_state(nfa_entry_state_set, nfa_entry)
      
      call self%epsilon_closure(nfa_entry_state_set, initial_closure)

      allocate(self%initial_dfa_state)
      allocate(self%initial_dfa_state%state_set)
      self%initial_dfa_state%state_set = initial_closure
      self%initial_dfa_state => self%register(self%initial_dfa_state%state_set)

      deallocate(initial_closure)

   end subroutine lazy_dfa__init


   subroutine  lazy_dfa__deallocate(self)
      implicit none
      class(dfa_t) :: self
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
            deallocate(dstate_pointer_list(j)%node)
            dstate_pointer_count = dstate_pointer_count - 1
         end if
      end do 

      if (associated(self%states)) nullify(self%states)

   end subroutine lazy_dfa__deallocate

   ! dfa_tの配列dfaの要素として状態を登録する
   function lazy_dfa__register(self, s) result(res)
      implicit none
      class(dfa_t) :: self
      type(nfa_state_set_t), intent(in), target :: s
      
      integer(int32) :: i, k
      type(d_state_t), pointer :: res

      res => null()

      do i = 1, self%dfa_nstate
         if (equivalent_NFA_state_set(self%states(i)%state_set, s)) then
            res => self%states(i)
            return
         end if
      end do

      if (self%dfa_nstate >= DFA_STATE_MAX) then
         write(stderr, '(a)') "ERROR: Number of DFA states too large."
         error stop
      end if

      self%dfa_nstate = self%dfa_nstate + 1
      k = self%dfa_nstate

      self%states(k)%state_set => s
      self%states(k)%accepted  = check_NFA_state(s, nfa_exit)
      self%states(k)%transition => null()

      res => self%states(k)

   end function lazy_dfa__register


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


!=====================================================================!


   subroutine lazy_dfa__epsilon_closure (self, state_set, closure)
      class(dfa_t), intent(in) :: self
      type(nfa_state_set_t), intent(in) :: state_set
      type(nfa_state_set_t), intent(inout) :: closure

      type(nlist_t), pointer :: t
      integer(int32) :: i

      closure = state_set

      do i = 1, self%nfa%nfa_nstate
         t => self%nfa%states(i)
         do while(associated(t))
            if (t%c == SEG_EMPTY .and. t%to /= 0) then
               call add_NFA_state(closure, t%to)
            end if
            t => t%next
         end do 
      end do

   end subroutine lazy_dfa__epsilon_closure

   function lazy_dfa__compute_reachable_n_state(self, dstate, symbol) result(res)
      implicit none
      class(dfa_t), intent(in) :: self
      type(d_state_t), intent(in) :: dstate
      type(d_list_t), pointer :: res
      character(*), intent(in) :: symbol

      integer(int32) :: i, j
      type(nfa_state_set_t), pointer :: state_set
      type(nlist_t), pointer :: ptr_nlist
      type(d_list_t), pointer :: a, b

      type(segment_t) :: symbol_belong(1)

      symbol_belong = SEG_EMPTY
      ptr_nlist => null()
      a => null()
      b => null()
      res => null()
      state_set => dstate%state_set

      outer: do i = 1, self%nfa%nfa_nstate
         if (check_NFA_state(state_set, i)) then
            ptr_nlist => self%nfa%states(i)

            middle: do while(associated(ptr_nlist))
               if (ptr_nlist%c /= SEG_EMPTY) then
                  a => res
                  inner: do while (associated(a))
                     do j = 1, size(a%c)
                        if (a%c(j) == ptr_nlist%c .and. ptr_nlist%to /= 0) then
                          
                           call add_NFA_state(a%to, ptr_nlist%to)

                           ptr_nlist => ptr_nlist%next

                           cycle middle
                        end if
                     end do
                     a => a%next
                  end do inner

                  if (ptr_nlist%to /= 0) then
                     if (symbol_to_segment(symbol) .in. ptr_nlist%c) then

                        symbol_belong = which_segment_symbol_belong(self%nfa%all_segments, symbol)

                        allocate(b)
                        allocate(b%c(1))

                        dlist_pointer_count = dlist_pointer_count + 1
                        dlist_pointer_list(dlist_pointer_count)%node => b

                        b%c(1) = symbol_belong(1)
                        call add_nfa_state(b%to, ptr_nlist%to)
                        b%next => res
                        res => b

                     end if
                  end if
               end if
               ptr_nlist => ptr_nlist%next
            end do middle

         end if
      end do outer

   end function lazy_dfa__compute_reachable_n_state



   logical function lazy_dfa__is_registered(self, state_set) result(res)
      implicit none
      class(dfa_t), intent(in) :: self
      type(nfa_state_set_t), intent(in) :: state_set

      integer :: i, n

      n = dstate_pointer_count
      res = .false.
      do i = 1, n
         res = res .or. any(dstate_pointer_list(i)%node%state_set%vec .eqv. state_set%vec)
         if (res) return
      end do 

   end function lazy_dfa__is_registered


   function lazy_dfa__move(self, current, segments, symbol) result(res)
      use :: forgex_segment_m
      implicit none 
      class(dfa_t), intent(inout) :: self
      type(d_state_t), intent(in), pointer :: current
      type(segment_t), intent(in) :: segments(:)
      character(*), intent(in) :: symbol
      type(d_list_t), pointer :: res
      
      integer(int32) :: i, j, k, next

      res => self%reachable(self%states(current%index), symbol)

   end function lazy_dfa__move


   subroutine lazy_dfa__construct(self, current, symbol)
      use :: forgex_utf8_m
      implicit none
      class(dfa_t), intent(inout) :: self
      type(d_state_t), pointer, intent(inout) :: current
      character(*), intent(in) :: symbol

      type(d_state_t), pointer :: prev
      type(d_list_t), pointer :: x
      type(d_list_t) :: without_epsilon
      type(segment_t), allocatable :: all_segments(:)

      ! 暗黙の配列割り付けに注意
      all_segments = self%nfa%all_segments

      ! 遷移前の状態へのポインタをprevに代入
      prev => current
      
      ! ε遷移を除いた行き先のstate_setを取得する
      x => self%move(prev, self%nfa%all_segments, symbol)

      without_epsilon = x ! deep copy

      ! ε遷移との和集合を取り、x%toに格納する
      call self%nfa%collect_empty_transition(x%to)

      if (.not. self%is_registered(x%to)) then
         ! まだDFA状態が登録されていない場合
         current => self%register(x%to)
         call add_dfa_transition(prev, which_segment_symbol_belong(all_segments, symbol), current)   
      else
         ! 登録されている場合
         current => self%register(without_epsilon%to)
         call add_dfa_transition(prev, which_segment_symbol_belong(all_segments, symbol), current)
      end if
   
   end subroutine lazy_dfa__construct

!=====================================================================!


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


end module forgex_lazy_dfa_m