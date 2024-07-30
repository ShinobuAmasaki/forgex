! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_lazy_dfa_m module is a part of Forgex.
!
!! This file contains definitions of `dfa_transition_t` type and `dfa_state_node_t` class,
!! and its type-bound procedures.
#ifdef IMPURE
#define pure
#endif
!> The `forgex_lazy_dfa_node_m` module defines the state nodes and transitions of DFA. 
module forgex_lazy_dfa_node_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only: DFA_NOT_INIT, DFA_NOT_INIT_TRAENSITION_TOP, &
         DFA_TRANSITION_UNIT, DFA_INIT_TRANSITION_TOP
   use :: forgex_segment_m, only: segment_t
   use :: forgex_nfa_state_set_m, only: nfa_state_set_t
   implicit none
   private

   public :: copy_dfa_transition

   type, public :: dfa_transition_t
      type(segment_t)       :: c
      type(nfa_state_set_t) :: nfa_set
      integer(int32)        :: own_j = DFA_NOT_INIT ! Own index in the list of transitions
      integer(int32)        :: dst   = DFA_NOT_INIT ! The destination node index of DFA graph.
   end type dfa_transition_t


   type, public :: dfa_state_node_t
      integer(int32)                      :: own_i = DFA_NOT_INIT
      type(nfa_state_set_t)               :: nfa_set
      logical                             :: accepted = .false.
      type(dfa_transition_t), allocatable :: transition(:)
      integer(int32), private             :: tra_top = DFA_NOT_INIT_TRAENSITION_TOP
      logical                             :: registered = .false.
      logical                             :: initialized = .false.
   contains
      procedure :: get_tra_top       => dfa_state_node__get_transition_top
      procedure :: increment_tra_top => dfa_state_node__increment_transition_top
      procedure :: init_transition   => dfa_state_node__initialize_transition
      procedure :: add_transition    => dfa_state_node__add_transition
   end type dfa_state_node_t

contains

   !> This function returns the index of top transition in the list dfa_state_node_t has.
   pure function dfa_state_node__get_transition_top (self) result(res)
      implicit none
      class(dfa_state_node_t), intent(in) :: self
      integer :: res

      res = self%tra_top
   end function dfa_state_node__get_transition_top
      

   !> This subroutine increments the value of top transition index.
   pure subroutine dfa_state_node__increment_transition_top (self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self


      self%tra_top = self%tra_top + 1
   end subroutine dfa_state_node__increment_transition_top

   
   !> This subroutine initializes the transition list which belongs a dfa_state_node_t.
   pure subroutine dfa_state_node__initialize_transition (self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self
      
      allocate(self%transition(DFA_TRANSITION_UNIT))
      self%tra_top = DFA_INIT_TRANSITION_TOP
      self%initialized = .true.
   end subroutine dfa_state_node__initialize_transition


   !> This subroutine processes to add the given transition to the list which dfa_state_node_t has.
   pure subroutine dfa_state_node__add_transition (self, tra)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self
      type(dfa_transition_t), intent(in) :: tra

      integer :: j

      if (.not. self%initialized) then
         call self%init_transition()
      end if

      call self%increment_tra_top()
      j = self%get_tra_top()
   
      if (j < 0) then
         error stop "ERROR: Invalid counting transitions"
      end if

      self%transition(j) = tra
   end subroutine dfa_state_node__add_transition


   !> This subroutine copies the data of a specified transition into the
   !> variables of another dfa_transition_t.
   pure subroutine copy_dfa_transition(src, dst)
      implicit none
      type(dfa_transition_t), intent(in) :: src
      type(dfa_transition_t), intent(inout) :: dst

      dst%c = src%c
      dst%dst = src%dst
      dst%nfa_set = src%nfa_set
      dst%own_j = src%own_j
   end subroutine copy_dfa_transition

end module forgex_lazy_dfa_node_m