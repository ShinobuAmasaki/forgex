program main 
   use, intrinsic :: iso_fortran_env
   use :: forgex_parameters_m
   use :: forgex_syntax_tree_m
   implicit none
   
   type(tree_node_t), allocatable :: tree(:)
   integer(int32) :: root_index, next_index
   type(tree_node_t) :: node
   type(tape_t) :: tape
   character(:), allocatable :: string
   integer :: i

   next_index = 1
   root_index = 1

   string = 'h|f'

   call build_syntax_tree(string, tape, tree, next_index)

   do i = 0, TREE_NODE_LIMIT
      print *, tree(i)%own_i, tree(i)%op, tree(i)%left_i, tree(i)%right_i, tree(i)%is_registered
   end do


end program main