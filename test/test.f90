program main 
   use, intrinsic :: iso_fortran_env
   use :: forgex_parameters_m
   use :: forgex_syntax_tree_m
   use :: forgex_utf8_m
   implicit none
   
   type(tree_node_t), allocatable :: tree(:)
   integer(int32) :: root_i, top_index
   type(tape_t) :: tape
   character(:), allocatable :: string
   integer :: i

   root_i = 1

   string = '\S'

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

   call deallocate_tree(tree)

end program main