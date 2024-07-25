program main
   use, intrinsic :: iso_fortran_env
   use :: forgex_parameters_m
   use :: forgex_syntax_tree_m
   use :: forgex_utf8_m
   use :: forgex_nfa_m
   implicit none

   type(tree_node_t), allocatable :: tree(:)
   integer(int32) :: root_i, top_index
   type(tape_t) :: tape
   character(:), allocatable :: string
   integer :: i

   type(nfa_state_node_t), allocatable :: nfa(:)
   integer :: nfa_entry, nfa_exit, nfa_top

   root_i = 1

   string = 'a*b'

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
   write(stderr, *) ''

   call build_nfa_graph(tree, top_index, nfa, nfa_entry, nfa_exit, nfa_top)

   call nfa_print(nfa, nfa_top)

   call deallocate_tree(tree)

end program main
