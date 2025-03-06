! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_nfa_graph_m module is a part of Forgex.
!
!! This file contains a derived-type which represents the NFA graph using an array. 
#ifdef IMPURE
#define pure
#endif
!> This module defines the `nfa_graph_t` derived-type which represents the NFA graph.
module forgex_nfa_graph_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only: TREE_NODE_BASE, TREE_NODE_LIMIT, &
         NFA_STATE_BASE, NFA_STATE_LIMIT, NFA_NULL_TRANSITION
   use :: forgex_nfa_node_m, only: nfa_state_node_t, nfa_transition_t, &
         nfa_deallocate, make_nfa_node, build_nfa_graph, generate_nfa

   implicit none
   private

   type, public :: nfa_graph_t
      type(nfa_state_node_t), allocatable :: nodes(:)
      integer(int32) :: nfa_base = NFA_STATE_BASE
      integer(int32) :: nfa_limit = NFA_STATE_LIMIT
      integer(int32) :: nfa_top = 0
   contains
      procedure :: build                      => nfa_graph__build
      procedure :: free                       => nfa_graph__deallocate
      procedure :: generate                   => nfa_graph__generate
      procedure :: collect_epsilon_transition => nfa_graph__collect_epsilon_transition
      procedure :: mark_epsilon_transition    => nfa_graph__mark_epsilon_transition
      procedure :: print                      => nfa_graph__print
   end type

contains

!== Currently, the nfa_graph_m procedures are just a wrapper around nfa_node_m.

   pure subroutine nfa_graph__build(self, tree, nfa_entry, nfa_exit, all_segments)
      use :: forgex_syntax_tree_graph_m
      use :: forgex_segment_m
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(tree_t),   intent(in) :: tree
      integer(int32), intent(inout) :: nfa_entry, nfa_exit
      type(segment_t), allocatable, intent(inout) :: all_segments(:)

      call build_nfa_graph(tree, self%nodes, nfa_entry, nfa_exit, self%nfa_top, all_segments)
      self%nfa_limit = ubound(self%nodes, dim=1)
   end subroutine nfa_graph__build


   !> This subroutine invokes procedure for deallocation.
   pure subroutine nfa_graph__deallocate(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self

      call nfa_deallocate(self%nodes)
   end subroutine


   pure subroutine nfa_graph__generate(self, tree, entry, exit)
      use :: forgex_syntax_tree_graph_m
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(tree_t),   intent(in) :: tree
      integer(int32), intent(in) :: entry, exit

      call generate_nfa(tree, tree%top, self%nodes, self%nfa_top, entry, exit)
   end subroutine nfa_graph__generate


   pure recursive subroutine nfa_graph__mark_epsilon_transition(self, state_set, idx)
      use :: forgex_segment_m
      use :: forgex_nfa_state_set_m
      implicit none
      class(nfa_graph_t),intent(in) :: self
      type(nfa_state_set_t), intent(inout) :: state_set
      integer, intent(in) :: idx

      type(nfa_state_node_t) :: n_node
      type(nfa_transition_t) :: n_tra
      integer :: j

      call add_nfa_state(state_set, idx)

      n_node = self%nodes(idx)

      if (.not. allocated(n_node%forward)) return

      do j = 1, n_node%forward_top

         n_tra = n_node%forward(j)

         if (.not. allocated(n_tra%c)) cycle

         if (any(n_tra%c == SEG_EPSILON) .and. .not. check_nfa_state(state_set, n_tra%dst)) then
            if (n_tra%dst /= NFA_NULL_TRANSITION) call self%mark_epsilon_transition(state_set, n_tra%dst)
         end if
      end do
   end subroutine nfa_graph__mark_epsilon_transition


   pure subroutine nfa_graph__collect_epsilon_transition(self, state_set)
      use :: forgex_segment_m
      use :: forgex_nfa_state_set_m
      implicit none
      class(nfa_graph_t), intent(in) :: self
      type(nfa_state_set_t), intent(inout) :: state_set

      integer :: i

      do i = NFA_STATE_BASE, self%nfa_top
         if (check_nfa_state(state_set, i)) then

            call self%mark_epsilon_transition(state_set, i)

         end if
      end do
   end subroutine nfa_graph__collect_epsilon_transition


   subroutine nfa_graph__print(self, uni, nfa_exit)
      use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
      use :: forgex_segment_m
      implicit none
      class(nfa_graph_t), intent(in) :: self
      integer(int32), intent(in) :: uni
      integer(int32), intent(in) :: nfa_exit

      type(nfa_state_node_t) :: node
      type(nfa_transition_t) :: transition
      character(:), allocatable :: buf
      integer(int32) :: i, j, k

      do i = self%nfa_base, self%nfa_top

         write(uni, '(a, i4, a)', advance='no') "state ", i, ": "
         node = self%nodes(i)
         if (i == nfa_exit) then
            write(uni, '(a)') "<Accepted>"
            cycle
         end if

         do j = 1, node%forward_top
            if (.not. allocated(node%forward)) cycle

            transition = node%forward(j)

            if (transition%dst > NFA_NULL_TRANSITION) then
               do k = 1, transition%c_top
                  if (transition%c(k) == SEG_INIT) cycle

                  buf = transition%c(k)%print()
                  if (transition%c(k) == SEG_EPSILON) buf = '?'
                  write(uni, '(a,a,a2,i0,a1)', advance='no') "(", trim(buf), ", ", transition%dst, ")"

               enddo
            end if
         end do

         write(uni, '(a)') ""
      end do
   end subroutine nfa_graph__print

end module forgex_nfa_graph_m