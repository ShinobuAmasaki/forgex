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

   type, public :: dfa_transition_t
      type(segment_t), allocatable :: c(:)
      integer(int32) :: c_top = 0
      type(nfa_state_set_t) :: nfa_set
      integer(int32) :: own_j = DFA_NOT_INIT
      integer(int32) :: dst = 0     ! destination index of DFA graph
   end type dfa_transition_t


   type, public :: dfa_state_node_t
      integer(int32)        :: own_i = DFA_NOT_INIT
      type(nfa_state_set_t) :: nfa_set
      logical               :: accepted = .false.
      type(dfa_transition_t), allocatable :: transition(:)
      integer(int32)        :: tra_top = 1
      logical               :: registered = .false.
   end type dfa_state_node_t

contains



end module forgex_lazy_dfa_node_m