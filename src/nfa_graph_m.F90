#ifdef PURE
#define pure
#endif
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
      procedure :: collect_e_t => nfa_graph__collect_epsilon_transition
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


   pure subroutine nfa_graph__collect_epsilon_transition(self, state_set)
      use :: forgex_segment_m
      use :: forgex_nfa_state_set_m
      implicit none
      class(nfa_graph_t), intent(in) :: self
      type(nfa_state_set_t), intent(inout) :: state_set

      type(nfa_transition_t) :: tra

      integer :: i, j, k

      do i = NFA_STATE_BASE+1, self%nfa_top
         if (check_nfa_state(state_set, i)) then
            if (.not. allocated(self%nodes(i)%forward)) cycle
            do j = 1, self%nodes(i)%forward_top
               tra = self%nodes(i)%forward(j)
               do k = 1, tra%c_top
                  if (tra%c(k) == SEG_EPSILON .and. .not. check_nfa_state(state_set, tra%dst)) then
                     if (tra%dst /= NFA_NULL_TRANSITION) call add_nfa_state(state_set, i)
                  end if
               end do
            end do
         end if
      end do

   end subroutine nfa_graph__collect_epsilon_transition

#ifdef DEBUG

   subroutine nfa_graph__print(self)
      use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
      use :: forgex_segment_m
      implicit none
      class(nfa_graph_t), intent(in) :: self
      
      type(nfa_state_node_t) :: node
      type(nfa_transition_t) :: transition
      character(:), allocatable :: buf
      integer(int32) :: i, j, k

      write(stderr, *) "--- PRINT NFA ---"

      do i = self%nfa_base+1, self%nfa_top
         
         write(stderr, '(a, i3, a)', advance='no') "state ", i, ": "
         node = self%nodes(i)

         do j = 1, node%forward_top
            transition = node%forward(j)

            if (transition%dst > NFA_NULL_TRANSITION) then
               do k = 1, transition%c_top
                  if (transition%c(k) == SEG_INIT) cycle

                  buf = transition%c(k)%print()
                  
                  if (transition%c(k) == SEG_EPSILON) buf = '?'
                  write(stderr, '(a,a,a2,i0,a1)', advance='no') "(", trim(buf), ", ", transition%dst, ")"
                 
               enddo
            end if
         end do

         write(stderr, *) ''
      end do
   end subroutine nfa_graph__print


   subroutine dump_segment_array (list)
      use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
      use :: forgex_segment_m
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