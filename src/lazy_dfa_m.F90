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

   type, public :: dfa_transition_t
      type(segment_t), allocatable :: c(:)
      integer(int32) :: c_top = 1
      integer(int32) :: num_of_c_size = 1
      integer(int32) :: dst = DFA_NULL_TRANSITION
      integer(int32) :: own_j = DFA_NOT_INIT
   end type dfa_transition_t

   type, public :: dfa_state_node_t
      integer(int32)        :: own_i = DFA_NOT_INIT
      type(nfa_state_set_t) :: nfa_set
      logical               :: accepted = .false.
      type(dfa_transition_t), allocatable :: transition(:)
      integer(int32)        :: tra_top = 1
   contains
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
      allocate(dfa_graph(i)%transition(DFA_TRANSITION_SIZE))
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


   pure function move_lazy_dfa (dfa_graph, dfa_top, curr_i, symbol) result(res)
      implicit none
      type(dfa_state_node_t), intent(in) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      integer(int32),         intent(in) :: dfa_top
      integer(int32),         intent(in) :: curr_i    ! current index

      integer(int32) :: res   ! result index
      integer(int32) :: i

      res = DFA_INVALID_INDEX
      do i = DFA_STATE_BASE+1, dfa_top
         res = graph(i)%reachable(curr_i, symbol)
         if (res /= DFA_INVALID_INDEX) return      ! if res is reachable, do return.
      end do
   end function move_lazy_dfa


   pure subroutine compute_reachable_nfa_state_lazy_dfa(dfa_node, nfa_graph, nfa_top, all_segments, symbol) 
      implicit none
      class(dfa_state_node_t), intent(inout) :: dfa_node
      type(nfa_state_node_t), intent(in)  :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32), intent(in) :: nfa_top
      type(segment_t), intent(in) :: all_segments(:)
      character(*), intent(in) :: symbol
   
      type(nfa_state_node_t) :: nfa_node
      type(segment_t) :: tmp  ! temporary
      integer(int32) :: i, j, k, m, n

      ! Scan through all NFA states
      ! If the i-th element of state_set is true, scan the transition list
      do i = NFA_STATE_BASE+1, nfa_top
      if (check_nfa_state(dfa_node%nfa_set, i)) then
         
         ! If the j-th element of transition list is allocated, follow the transitions
         if (allocated(nfa_graph(i)%forward)) then
         do j = 1, nfa_graph(i)%forward_top

            ! If the k-th element of segment list is allocated, scan the 
            if (allocated(nfa_graph(i)%forward(j)%c)) then           
            do k = 1, nfa_graph(i)%forward(j)%c_top
               tmp = nfa_graph(i)%forward(j)%c(k)
               if (tmp /=SEG_EMPTY .or. tmp /= SEG_EPSILON .or. tmp /= SEG_INIT) then
               end if
            end do
            end if

            !-- 
            if (nfa_graph(i)%forward(j)%dst /= NFA_NULL_TRANSITION) then

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

               call add_nfa_state(dfa_node%nfa_set, nfa_graph(i)%forward(j)%dst)

            end if

         end do
         end if
      end if
      end do 
   end subroutine compute_reachable_nfa_state_lazy_dfa
   
   pure subroutine node_construct_lazy_dfa(dfa_graph, dfa_top, curr_i, dest_i, symbol, all_segments)
      implicit none
      type(dfa_state_node_t), intent(inout) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      integer(int32),         intent(in)    :: dfa_top
      integer(int32),         intent(in)    :: curr_i    ! current index
      integer(int32),         intent(inout) :: dest_i    ! destination index
      character(*),           intent(in)    :: symbol
      type(segment_t),        intent(in)    :: all_segments(:)

      integer(int32) :: prev_i ! previous index
      integer(int32) :: i  ! temporary index
      
      prev_i = curr_i
      

      ! i = dfa_graph(prev_i)%move(symbol)

      if (i /= DFA_INVALID_INDEX) then
         ! 
      else
         dest_i = DFA_INVALID_INDEX
      end if    

   end subroutine node_construct_lazy_dfa


end module forgex_lazy_dfa_m