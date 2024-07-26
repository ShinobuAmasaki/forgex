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
   use :: forgex_nfa_m
   use :: forgex_nfa_state_set_m
   
   implicit none
   private

   type, public :: dfa_transition_t
      type(segment_t), allocatable :: c(:)
      integer(int32) :: c_top = 1
      integer(int32) :: dst = DFA_NULL_TRANSITION
      integer(int32) :: own_j = DFA_NOT_INIT
   end type dfa_transition_t

   type, public :: dfa_state_node_t
      integer(int32) :: own_i

   contains
   end type dfa_state_node_t

contains

   pure subroutine init_lazy_dfa(nfa, nfa_entry, nfa_exit, nfa_top, dfa, dfa_top)
      implicit none
      type(nfa_state_node_t), allocatable, intent(in)    :: nfa(:)
      integer(int32),                      intent(in)    :: nfa_entry, nfa_exit, nfa_top
      type(dfa_state_node_t), allocatable, intent(inout) :: dfa(:)
      integer(int32),                      intent(inout) :: dfa_top
   end subroutine init_lazy_dfa

end module forgex_lazy_dfa_m