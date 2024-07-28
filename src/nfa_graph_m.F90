module forgex_nfa_graph_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m
   use :: forgex_nfa_node_m
   implicit none
   private

   type, public :: nfa_graph_t
      type(nfa_state_node_t), allocatable :: nodes(:)
      integer(int32) :: nfa_base = NFA_STATE_BASE
      integer(int32) :: nfa_limit = NFA_STATE_LIMIT
      integer(int32) :: nfa_top = 0
   contains
      procedure :: build => nfa_graph__build
      procedure :: free  => nfa_graph__deallocate
      procedure :: make_node => nfa_graph__make_nfa_node
      procedure :: generate => nfa_graph__generate
#ifdef DEBUG
      procedure :: print => nfa_graph__print
#endif
   end type
contains

!== Currently, the nfa_graph_m procedures are just a wrapper around nfa_node_m.

   pure subroutine nfa_graph__build(self, tree, root_i, nfa_entry, nfa_exit, all_segments)
      use :: forgex_syntax_tree_m
      use :: forgex_segment_m
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(in) :: root_i
      integer(int32), intent(inout) :: nfa_entry, nfa_exit
      type(segment_t), allocatable, intent(inout) :: all_segments(:)

      call build_nfa_graph(tree, root_i, self%nodes, nfa_entry, nfa_exit, self%nfa_top, all_segments)

   end subroutine nfa_graph__build

   
   pure subroutine nfa_graph__deallocate(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self

      call nfa_deallocate(self%nodes)
   end subroutine


   pure subroutine nfa_graph__make_nfa_node(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self

      call make_nfa_node(self%nfa_top)
   end subroutine nfa_graph__make_nfa_node


   pure subroutine nfa_graph__generate(self, tree, tree_root, entry, exit)
      use :: forgex_syntax_tree_m
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32), intent(in) :: tree_root
      integer(int32), intent(in) :: entry, exit

      call generate_nfa(tree, tree_root, self%nodes, self%nfa_top, entry, exit)

   end subroutine nfa_graph__generate


   pure subroutine nfa_graph__disjoin(self, all_segments)
      use :: forgex_segment_m
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(segment_t), allocatable, intent(inout) :: all_segments(:)

      call disjoin_nfa(self%nodes, self%nfa_top, all_segments)
      
   end subroutine nfa_graph__disjoin

#ifdef DEBUG

   subroutine nfa_graph__print(self)
      use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
      implicit none
      class(nfa_graph_t), intent(in) :: self
      
      type(nfa_state_node_t) :: node
      type(nfa_transition_t) :: transition
      character(:), allocatable :: buf
      integer(int32) :: i, j, k

      write(stderr, *) "--- PRINT NFA ---"

      do i = self%nfa_base, self%nfa_top
         
         write(stderr, '(a, i3, a)', advance='no') "state ", i, ": "
         node = self%nodes(i)

         do j = 1, node%forward_top
            transition = node%forward(j)

            if (transition%dst > NFA_NULL_TRANSITION) then
               do k = 1, transition%c_top -1
                  buf = transition%c(k)%print()
                  
                  if (transition%c(k) == SEG_EPSILON) buf = '?'
                  write(stderr, '(a,a,a2,i0,a1)', advance='no') "(", trim(buf), ", ", transition%dst, ")"
               enddo
            end if
         end do

         write(stderr, *) ''
      end do
   end subroutine print_nfa


   subroutine dump_segment_array (list)
      implicit none
      type(segment_t), intent(in) :: list(:)

      integer :: k
      write(stderr, *) "============================="
      do k = 1, size(list, dim=1)
         if (list(k)/= SEG_INIT) write(stderr, *) list(k)
      end do
   end subroutine dump_segment_array     

#endif


end module forgex_nfa_graph_m