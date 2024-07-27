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
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_nfa_m
   use :: forgex_nfa_state_set_m
   
   implicit none
   private

   public :: init_lazy_dfa
   public :: construct_dfa
   
   interface construct_dfa
      module procedure :: node_construct_lazy_dfa
   end interface construct_dfa

   type, public :: dfa_transition_t
      type(segment_t), allocatable :: c(:)
      integer(int32) :: c_top = 1
      integer(int32) :: num_of_c_size = 1
      type(nfa_state_set_t) :: dst
      integer(int32) :: own_j = DFA_NOT_INIT
   end type dfa_transition_t

   type, public :: dfa_state_node_t
      integer(int32)        :: own_i = DFA_NOT_INIT
      type(nfa_state_set_t) :: nfa_set
      logical               :: accepted = .false.
      type(dfa_transition_t), allocatable :: transition(:)
      integer(int32)        :: tra_top = 1
   contains
      procedure :: reachable => compute_reachable_nfa_state_lazy_dfa
   end type dfa_state_node_t

contains

!-- type-bound procedures

!-- procedures for dfa_graph

   pure subroutine preprocess_lazy_dfa (dfa, dfa_top, dfa_base, dfa_limit)
      implicit none
      type(dfa_state_node_t), allocatable, intent(inout) :: dfa(:)
      integer(int32),                      intent(inout) :: dfa_top
      integer(int32),                      intent(in)    :: dfa_base
      integer(int32),                      intent(in)    :: dfa_limit

      integer(int32) :: i

      allocate(dfa(dfa_base:dfa_limit))

      do i = dfa_base, dfa_limit
         dfa(i)%own_i = i
      end do
      dfa_top = 1
   end subroutine preprocess_lazy_dfa
       

   pure subroutine init_lazy_dfa(nfa, nfa_entry, nfa_exit, nfa_top, dfa_graph, dfa_top)
      implicit none
      type(nfa_state_node_t),              intent(in)    :: nfa(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32),                      intent(in)    :: nfa_entry, nfa_exit, nfa_top
      type(dfa_state_node_t), allocatable, intent(inout) :: dfa_graph(:)
      integer(int32),                      intent(inout) :: dfa_top

      type(nfa_state_set_t) :: nfa_entry_set
      type(nfa_state_set_t) :: initial_closure
      integer(int32) :: i

      initial_closure%vec(:) = .false.
      nfa_entry_set%vec(:)   = .false.

      call preprocess_lazy_dfa(dfa_graph, dfa_top, DFA_STATE_BASE, DFA_STATE_LIMIT)

      call add_nfa_state(nfa_entry_set, nfa_entry)

      call epsilon_closure_lazy_dfa(nfa, nfa_entry, nfa_top, nfa_entry_set, initial_closure)

      i = DFA_STATE_BASE + 1

      ! nfa_exitを渡すのは受理状態かどうかを確認するため
      call register_lazy_dfa(dfa_graph, nfa_entry_set, nfa_exit, dfa_top)
      
   end subroutine init_lazy_dfa


   !> Compute the ε-closure for a set NFA states.
   !>
   !> The ε-closure is the set of NFA states reachable from a given set of NFA states via ε-transition
   !> This subroutine calculates the ε-closure and stores it in the `closure` argument.
   pure subroutine epsilon_closure_lazy_dfa (nfa_graph, nfa_entry, nfa_top, state_set, closure)
      implicit none
      type(nfa_state_node_t), intent(in) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32),         intent(in) :: nfa_entry, nfa_top
      type(nfa_state_set_t), intent(in) :: state_set
      type(nfa_state_set_t), intent(inout) :: closure

      type(nfa_transition_t) :: transition
      integer(int32) :: i, j, k
      closure = state_set

      do i = 1, nfa_top
         do j = 1, nfa_graph(i)%forward_top

            transition = nfa_graph(i)%forward(j)

            if (transition%is_registered) then
               
               do k = 1, transition%c_top
                  
                  if (transition%c(k) == SEG_EMPTY .and. transition%dst /= NFA_NULL_TRANSITION) then
                  
                     if (transition%own_j == nfa_entry) then
                  
                        call add_nfa_state(closure, transition%dst)
                  
                     end if
                 
                  end if
               
               end do
            
            end if

         end do
      end do

   end subroutine epsilon_closure_lazy_dfa


   !> This subroutine take `nfa_state_set_t` as input and register the set as the DFA
   !> state in DFA array.
   pure subroutine register_lazy_dfa(dfa_graph, nfa_set, nfa_exit, dfa_top)
      implicit none
      type(dfa_state_node_t), intent(inout) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      type(nfa_state_set_t), intent(in) :: nfa_set
      integer(int32), intent(in)    :: nfa_exit
      integer(int32), intent(inout) :: dfa_top

      integer(int32) :: i, j
      type(dfa_state_node_t) :: res

      if (is_registered_lazy_dfa(dfa_graph, nfa_set, dfa_top) /= DFA_INVALID_INDEX) then
         return
      end if

      if (dfa_top > DFA_STATE_LIMIT) then
         error stop "Number of DFA state nodes too large."
      end if

      i = dfa_top
      dfa_graph(i)%nfa_set = nfa_set
      dfa_graph(i)%accepted = check_nfa_state(nfa_set, nfa_exit)
      allocate(dfa_graph(i)%transition(DFA_TRANSITION_UNIT))
      dfa_top = i + 1
      
   end subroutine register_lazy_dfa


   pure function is_registered_lazy_dfa (dfa_graph, nfa_set, dfa_top) result(res)
      implicit none
      type(dfa_state_node_t), intent(in) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      type(nfa_state_set_t),  intent(in) :: nfa_set
      integer(int32), intent(in) :: dfa_top
      integer(int32) :: res

      integer :: i

      res = DFA_INVALID_INDEX

      do i = DFA_STATE_BASE+1, dfa_top
         if (equivalent_nfa_state_set(dfa_graph(i)%nfa_set, nfa_set)) then
            res = i
            return
         end if
      end do
   end function is_registered_lazy_dfa


   pure subroutine move_lazy_dfa (nfa_graph, nfa_top, dfa_graph, dfa_top, curr_i, symbol, segments, res)
      implicit none
      type(nfa_state_node_t), intent(in) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32),         intent(in) :: nfa_top
      type(dfa_state_node_t), intent(inout) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      integer(int32),         intent(in) :: dfa_top
      integer(int32),         intent(in) :: curr_i    ! current index
      character(*),           intent(in) :: symbol
      type(segment_t), intent(in) :: segments(:)
      integer(int32), intent(inout) :: res

      integer(int32) :: i

      res = DFA_INVALID_INDEX
      do i = DFA_STATE_BASE+1, dfa_top
         call dfa_graph(i)%reachable(nfa_graph, nfa_top, segments, symbol)
         if (res /= DFA_INVALID_INDEX) return      ! if res is reachable, do return.
      end do
   end subroutine move_lazy_dfa


   pure subroutine compute_reachable_nfa_state_lazy_dfa(dfa_node, nfa_graph, nfa_top, all_segments, symbol) 
      use :: forgex_segment_m
      implicit none
      class(dfa_state_node_t), intent(inout) :: dfa_node
      type(nfa_state_node_t), intent(in)  :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32), intent(in) :: nfa_top
      type(segment_t), intent(in) :: all_segments(:)
      character(*), intent(in) :: symbol
   
      type(nfa_state_node_t) :: nfa_node
      type(segment_t) :: tmp  ! temporary
      integer(int32) :: i, j, k, m, n, c_top

      logical :: reduce

      reduce = .true.
 
      ! Scan through all NFA states
      ! If the i-th element of state_set is true, scan the transition list
      outer: do i = NFA_STATE_BASE+1, nfa_top
      if (check_nfa_state(dfa_node%nfa_set, i)) then
         
         ! If the j-th element of transition list is allocated, scan the transitions
         if (allocated(nfa_graph(i)%forward)) then
         middle: do j = 1, nfa_graph(i)%forward_top-1

            ! If the k-th element of segment list is allocated, scan the 
            if (allocated(nfa_graph(i)%forward(j)%c)) then   
               inner: do k = 1, nfa_graph(i)%forward(j)%c_top -1
                  tmp = nfa_graph(i)%forward(j)%c(k)
                  if (tmp /=SEG_EMPTY .or. tmp /= SEG_EPSILON .or. tmp /= SEG_INIT) then
                     call add_nfa_state(dfa_node%nfa_set, nfa_graph(i)%forward(j)%dst)
                  end if
               end do inner 
            end if

            !-- 
            if (nfa_graph(i)%forward(j)%dst /= NFA_NULL_TRANSITION) then
               
               if (.not. allocated(nfa_graph(i)%forward(j)%c)) cycle middle
               c_top = nfa_graph(i)%forward(j)%c_top

               do k = 1, c_top
                  reduce = reduce .and. (nfa_graph(i)%forward(j)%c(k) == SEG_EMPTY)
               end do
               
               if (  &
                     (any([(symbol_to_segment(symbol), k = 1, c_top)] == nfa_graph(i)%forward(j)%c(1:c_top))) &
                     .or. reduce ) then
                  
                  m = dfa_node%tra_top
                  tmp = which_segment_symbol_belong(all_segments, symbol)

                  if (.not. allocated(dfa_node%transition)) then
                     allocate(dfa_node%transition(DFA_TRANSITION_UNIT))
                  end if
                  
                  if (.not. allocated(dfa_node%transition(m)%c)) then
                     allocate(dfa_node%transition(m)%c(DFA_C_SIZE))
                  end if

                  n = dfa_node%transition(m)%c_top

                  dfa_node%transition(m)%c(n) = tmp
                  dfa_node%transition(m)%c_top = n + 1

                  call add_nfa_state(dfa_node%nfa_set, nfa_graph(i)%forward(j)%dst)
               end if
            end if

         end do middle
         end if
      end if 
      end do outer
   end subroutine compute_reachable_nfa_state_lazy_dfa
   

   pure subroutine node_construct_lazy_dfa(nfa_graph, nfa_top, nfa_exit, dfa_graph, dfa_top, curr_i, dest_i, symbol, all_segments)
      implicit none
      type(nfa_state_node_t), intent(in)    :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32),         intent(in)    :: nfa_top
      integer(int32),         intent(in)    :: nfa_exit
      type(dfa_state_node_t), intent(inout) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      integer(int32),         intent(inout) :: dfa_top
      integer(int32),         intent(in)    :: curr_i    ! current index
      integer(int32),         intent(inout) :: dest_i    ! destination index
      character(*),           intent(in)    :: symbol
      type(segment_t),        intent(in)    :: all_segments(:)

      integer(int32) :: prev_i ! previous index
      integer(int32) :: i  ! temporary index
      integer(int32) :: dst ! result
      
      prev_i = curr_i
      
      call move_lazy_dfa(nfa_graph, nfa_top, dfa_graph, dfa_top, curr_i, symbol, all_segments, dst)
      
      ! ε遷移を除いた行き先のstate_setを取得する
      ! Get the state set for the destination excluding epsilon-transition.
      if (dst /= DFA_INVALID_INDEX) then
         dfa_graph(dst)%nfa_set = reduce_transitions(dfa_graph(dst), dfa_graph(dst)%tra_top)
      else
         dest_i = DFA_INVALID_INDEX
         return
      end if
      
      ! ε遷移との和集合を取り、x%toに格納する
      ! Combine the state set with epsilon-transitions and store in `x%to`.
      call collect_epsilon_transition(nfa_graph, nfa_top, dfa_graph(dst)%nfa_set)

      if (is_registered_lazy_dfa(dfa_graph, dfa_graph(dst)%nfa_set, dfa_top) == DFA_INVALID_INDEX) then

         call register_lazy_dfa(dfa_graph, dfa_graph(dst)%nfa_set, nfa_exit, dfa_top)

         call add_dfa_transition(dfa_graph, prev_i, dst, [which_segment_symbol_belong(all_segments, symbol)])

      else
      
      end if

   end subroutine node_construct_lazy_dfa

!=====================================================================!
!  Helper procedures


   pure function reduce_transitions(node, tra_top) result(res)
      implicit none
      type(dfa_state_node_t), intent(in) :: node ! dfa_node
      integer(int32), intent(in) :: tra_top
      type(nfa_state_set_t) :: res

      integer(int32) :: j, k

      res%vec(:) = .false.

      do j = 1, node%tra_top
         if (.not. allocated(node%transition(j)%c)) cycle

         do k = 1, node%transition(j)%c_top
            if (.not. node%transition(j)%c(k) == SEG_EPSILON) then
               res%vec(:) = res%vec(:) .or. node%transition(j)%dst%vec(:)
            end if
         end do

      end do
   end function reduce_transitions


   pure subroutine add_dfa_transition(dfa_graph, src, dst, segments)
      use :: forgex_segment_m
      implicit none
      type(dfa_state_node_t), intent(in) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      integer(int32), intent(in) :: src   ! source index
      integer(int32), intent(in) :: dst   ! destination index
      type(segment_t), intent(in) :: segments(:)
      
      integer(int32) :: j, k, m

      do j = lbound(dfa_graph(src)%transition, dim=1), dfa_graph(src)%tra_top
         do m = lbound(segments, dim=1), ubound(segments, dim=1)
            if ( (segments(m) .in. dfa_graph(src)%transition(j)%c)) return
         end do
      end do

      





      
   end subroutine add_dfa_transition

!=====================================================================!
! Procedures for Debugging

#ifdef DEBUG

   subroutine print_lazy_dfa(graph, dfa_top, nfa_top)
      use, intrinsic :: iso_fortran_env, only:stderr => error_unit
      implicit none
      type(dfa_state_node_t), intent(in) :: graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      integer(int32), intent(in) :: dfa_top, nfa_top
      integer(int32) :: i, j, k, tra_top, c_top

      write(stderr,*) "--- PRINT DFA---"

      do i = DFA_STATE_BASE+1, dfa_top
         if (graph(i)%accepted) then
            write(stderr, '(i2,a, a)', advance='no') i, 'A', ": "
         else
            write(stderr, '(i2,a, a)', advance='no') i, ' ', ": "
         end if

         tra_top = graph(i)%tra_top
         !p => self%states(i)%transition

         do j = 1, tra_top
            c_top = graph(i)%transition(j)%c_top
            do k = 1, c_top
               write(stderr, '(a, a, i0, 1x)', advance='no') &
                 graph(i)%transition(j)%c(k)%print(), '=>', graph(i)%transition(j)%dst
            end do
         end do
         write(stderr, *) ""
      end do 

      do i = DFA_STATE_BASE+1, dfa_top
         if (graph(i)%accepted) then
            !self%states(i)%accepted) then
            write(stderr, '(a, i2, a)', advance='no') "state ", i, 'A = ( '
         else
            write(stderr, '(a, i2, a)', advance='no') "state ", i, '  = ( '
         end if

         call print_nfa_state_set(graph(i)%nfa_set, nfa_top)

         write(stderr,'(a)') ")"
      end do
   end subroutine print_lazy_dfa
#endif

end module forgex_lazy_dfa_m