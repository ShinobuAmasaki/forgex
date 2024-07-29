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
module forgex_lazy_dfa_node_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_nfa_state_set_m
   implicit none
   private

   public :: copy_dfa_transition

   type, public :: dfa_transition_t
      type(segment_t), allocatable :: c(:)
      integer(int32) :: c_top = ZERO_C_TOP
      type(nfa_state_set_t) :: nfa_set
      integer(int32) :: own_j = DFA_NOT_INIT
      integer(int32) :: dst = 0     ! destination index of DFA graph
   end type dfa_transition_t


   type, public :: dfa_state_node_t
      integer(int32)        :: own_i = DFA_NOT_INIT
      type(nfa_state_set_t) :: nfa_set
      logical               :: accepted = .false.
      type(dfa_transition_t), allocatable :: transition(:)
      integer(int32),private :: tra_top = DFA_NOT_INIT_TRAENSITION_TOP
      logical               :: registered = .false.
      logical               :: initialized = .false.
   contains
      procedure :: get_tra_top => dfa_state_node__get_transition_top
      procedure :: increment_tra_top => dfa_state_node__increment_transition_top
      procedure :: init_transition => dfa_state_node__initialize_transition
   end type dfa_state_node_t

contains

   pure function dfa_state_node__get_transition_top (self) result(res)
      implicit none
      class(dfa_state_node_t), intent(in) :: self
      integer :: res

      res = self%tra_top
   end function dfa_state_node__get_transition_top
      

   pure subroutine dfa_state_node__increment_transition_top (self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self


      self%tra_top = self%tra_top + 1
   end subroutine dfa_state_node__increment_transition_top

   
   pure subroutine dfa_state_node__initialize_transition (self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self
      
      allocate(self%transition(DFA_TRANSITION_UNIT))
      self%tra_top = DFA_INIT_TRANSITION_TOP
      self%initialized = .true.
   end subroutine dfa_state_node__initialize_transition


   pure subroutine copy_dfa_transition(src, dst)
      implicit none
      type(dfa_transition_t), intent(in) :: src
      type(dfa_transition_t), intent(inout) :: dst

      dst%c = src%c
      dst%c_top = src%c_top
      dst%dst = src%dst
      dst%nfa_set = src%nfa_set
      dst%own_j = src%own_j
   end subroutine copy_dfa_transition


end module forgex_lazy_dfa_node_m