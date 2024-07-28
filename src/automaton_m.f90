module forgex_automaton_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_nfa_m
   use :: forgex_nfa_state_set_m
   use :: forgex_lazy_dfa_m
   implicit none
   private

   type, public :: nfa_graph_t
      type(nfa_state_node_t), allocatable :: dfa_nodes(:)
      integer(int32) :: nfa_base = NFA_STATE_BASE
      integer(int32) :: nfa_limit = NFA_STATE_LIMIT
      integer(int32) :: nfa_top = 0
   end type
   
   type, public :: dfa_graph_t
      type(dfa_state_node_t), allocatable :: dfa_nodes(:)
      integer(int32) :: dfa_base = DFA_STATE_BASE
      integer(int32) :: dfa_limit = DFA_STATE_LIMIT 
      integer(int32) :: dfa_top = 0
   contains
      procedure :: prep => lazy_dfa__preprocess
      procedure :: init => lazy_dfa__initialize 
   end type dfa_graph_t

   type, public :: automaton_t
      type(nfa_graph_t) :: nfa
      type(dfa_graph_t) :: dfa
   contains
   end type automaton_t
      

contains

!---------------------------------------------------------------------!
!- DFA_GRAPH_T

   pure subroutine lazy_dfa__preprocess (self)
      implicit none
      class(dfa_graph_t), intent(inout) :: self
      
      integer(int32) :: i, base, limit

      ! Initialize DFA
      base = self%dfa_base
      limit = self%dfa_limit

      allocate(self%dfa_nodes(base:limit))

      do i = base, limit
         self%dfa_nodes(i)%own_i = i
      end do
      self%dfa_top = 1

   end subroutine lazy_dfa__preprocess


   pure subroutine lazy_dfa__initialize(self)
      implicit none
      class(dfa_graph_t), intent(inout) :: self

   end subroutine lazy_dfa__initialize

!---------------------------------------------------------------------!
!- AUTOMATON_T

end module forgex_automaton_m