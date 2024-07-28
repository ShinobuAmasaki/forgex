program main
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_syntax_tree_m
   use :: forgex_parameters_m
   use :: forgex_nfa_graph_m
   use :: forgex_nfa_node_m
   use :: forgex_segment_m
   implicit none
   
   type(tree_node_t), allocatable :: tree(:)
   integer(int32) :: root_i, top_index
   type(tape_t) :: tape
   character(:), allocatable :: string
   integer :: nfa_entry, nfa_exit
   integer(int32) :: i

   type(segment_t), allocatable :: all_segments(:)
   type(nfa_graph_t) :: nfa
   
   string = 'h[a-z]d'

   call build_syntax_tree(string, tape, tree, top_index)

   print *, '  own index|  operation|     parent|       left|      right|    is registered'
   do i = 0, TREE_NODE_LIMIT
      if (tree(i)%is_registered) then
         write(*, '(5i12, a, 1x, 1l)', advance='no') tree(i)%own_i, &
            tree(i)%op, tree(i)%parent_i, tree(i)%left_i, tree(i)%right_i, '   ', &
            tree(i)%is_registered
         if (allocated(tree(i)%c)) print *, tree(i)%c
      end if
   end do

   call print_tree_internal(tree, top_index)

   call nfa%build(tree, top_index, nfa_entry, nfa_exit, all_segments)

   call print_nfa(nfa%nfa_nodes, nfa%nfa_top)

   print *, nfa%nfa_top

end program main