! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_nfa_m module is a part of Forgex.
!
!! This file contains `nfa_t` class and its type-bound procedures.

!> The `forgex_nfa_m` module defines the data structure of NFA.
!> The `nfa_t` is defined as a class representing NFA.
module forgex_nfa_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit, int32
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_syntax_tree_m
   implicit none

   type, public :: nfa_transition_t
      type(segment_t) :: c = SEG_INVALID
      integer(int32)  :: src = NFA_NULL_TRANSITION
      integer(int32)  :: dst = NFA_NULL_TRANSITION
      integer(int32)  :: idx = NFA_NULL_TRANSITION
   end type

   !> The `nfa_state_set_t` type represents set of NFA states.
   type, public :: nfa_state_set_t
      logical :: vec(0:NFA_STATE_LIMIT) = .false.
   end type

   type, public :: nfa_state_node_t
      integer(int32) :: own_i
      type(nfa_transition_t), allocatable :: nfa_transitions(:)
      type(segment_t), allocatable :: all_segments(:)
   contains
   end type 

contains

   pure subroutine build_nfa_graph (tree, root_i, nfa, current, nfa_entry, nfa_exit)
      implicit none
      type(tree_node_t),      intent(in)                 :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32),         intent(in)                 :: root_i
      type(nfa_state_node_t), intent(inout), allocatable :: nfa(:)
      integer(int32),         intent(inout)              :: current
      integer(int32),         intent(inout)              :: nfa_entry
      integer(int32),         intent(inout), allocatable :: nfa_exit(:)

      integer(int32) :: i

      allocate(nfa(NFA_STATE_BASE:NFA_STATE_LIMIT))

      ! Initialize
      nfa(NFA_STATE_BASE:NFA_STATE_LIMIT)%own_i = [(i, i =NFA_STATE_BASE, NFA_STATE_LIMIT)]

   end subroutine build_nfa_graph

   
   pure recursive subroutine nfa__generate_nfa(self, tree, entry, exit)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(in) :: entry
      integer(int32), intent(in) :: exit
   end subroutine nfa__generate_nfa


end module forgex_nfa_m