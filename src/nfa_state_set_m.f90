! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_nfa_m module is a part of Forgex.
!
!! This file contains `nfa_state_set_t` class.

!> `forgex_nfa_m` module defines a derived-type which is the set of NFA nodes.
!> `nfa_state_set_t` represents a set of NFA nodes for the power set construction method.
module forgex_nfa_state_set_m
   use :: forgex_parameters_m
   implicit none
   private

   public :: add_nfa_state
   public :: check_nfa_state
   public :: equivalent_nfa_state_set
   public :: collect_epsilon_transition
   
#ifdef DEBUG
   public :: print_nfa_state_set
#endif

   !> The `nfa_state_set_t` type represents set of NFA states.
   type, public :: nfa_state_set_t
      logical :: vec(NFA_STATE_BASE:NFA_STATE_LIMIT) = .false.
   end type

contains


   !> This function checks if the arguement 'state' (set of NFA state) includes state 's'.
   pure logical function check_nfa_state(state_set, state_index)
      implicit none
      type(nfa_state_set_t), intent(in) :: state_set

      integer(int32), intent(in) :: state_index

      if (state_index /= 0) then
         check_nfa_state = state_set%vec(state_index)

      else
         check_nfa_state = .false. 
      end if    
   end function check_nfa_state


   !> This subroutine adds a specified state (`s`) to an NFA state set `state_set`
   !> by setting the corresponding element in `state%vec` to true.
   pure subroutine add_nfa_state(state_set, s)
      implicit none
      type(nfa_state_set_t), intent(inout) :: state_set  ! NFA state set to modify.
      integer(int32),        intent(in)    :: s          ! State index to add to the state set

      ! Set the state `s` in the `state_set` to `.true.`
      state_set%vec(s) = .true.
   end subroutine add_nfa_state


   !> This function determines if two NFA state sets (logical vectors) are equivalent.
   !>
   !> It takes a pointer to NFA state set and NFA state set, and returns logical result
   !> indicating equivalent (`.true.` if equivalent, `.false.` otherwise).
   pure function equivalent_nfa_state_set(a, b) result(res)
      implicit none
      type(nfa_state_set_t), intent(in) :: a, b

      integer(int32) :: i
      logical        :: res

      ! Compare each element of the logical vectors in a and b.
      do i = NFA_STATE_BASE, NFA_STATE_LIMIT
         if (a%vec(i) .neqv. b%vec(i)) then
            res = .false.
            return
         end if
      end do
      
      ! If all elements match, set the result `res` to `.true.` indicating equivalence. 
      res = .true.
   end function equivalent_nfa_state_set


   recursive pure subroutine mark_epsilon_transition(nfa_graph, nfa_top, nfa_set, nfa_i)
      use :: forgex_nfa_node_m, only: nfa_state_node_t
      implicit none
      type(nfa_state_node_t), intent(in) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      type(nfa_state_set_t), intent(inout) :: nfa_set
      integer(int32), intent(in) :: nfa_i, nfa_top

      integer :: dst
      integer(int32) :: i, j, k
      call add_nfa_state(nfa_set, nfa_i)

      outer: do i = NFA_STATE_BASE+1, nfa_top
         if (.not. allocated(nfa_graph(i)%forward)) cycle outer
         
         middle: do j = lbound(nfa_graph(i)%forward, dim=1), nfa_graph(i)%forward_top
            if (.not. allocated(nfa_graph(i)%forward(j)%c)) cycle middle
            
            dst = nfa_graph(i)%forward(j)%dst
            if (dst /= NFA_NULL_TRANSITION) call mark_epsilon_transition(nfa_graph, nfa_top, nfa_set, nfa_i)
         end do middle
      end do outer
   end subroutine mark_epsilon_transition


   pure subroutine collect_epsilon_transition(nfa_graph, nfa_top, nfa_set)
      use :: forgex_nfa_node_m, only: nfa_state_node_t
      implicit none
      type(nfa_state_node_t), intent(in) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32), intent(in) :: nfa_top
      type(nfa_state_set_t), intent(inout) :: nfa_set

      integer(int32) :: i

      do i = NFA_STATE_BASE+1, nfa_top
         if (check_nfa_state(nfa_set, i)) then
            call mark_epsilon_transition(nfa_graph, nfa_top, nfa_set, i)
         end if
      end do
   end subroutine collect_epsilon_transition

   
#ifdef DEBUG
   subroutine print_nfa_state_set(set, top)
      use, intrinsic :: iso_fortran_env, only:stderr => error_unit
      implicit none
      type(nfa_state_set_t), intent(in) :: set
      integer(int32), intent(in) :: top
      integer(int32) :: i

      do i = 1, top
         if (check_nfa_state(set, i)) write(stderr, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine print_nfa_state_set
#endif

end module forgex_nfa_state_set_m