program main
   use :: forgex_syntax_tree_graph_m
   use :: forgex_syntax_tree_optimize_m
   implicit none
   
   type(tree_t) :: tree
   integer :: root, from, to

   logical :: res = .false.

   character(:), allocatable :: pattern, text, prefix
   character(:), allocatable :: postfix

   pattern = ''

   ! pattern = "abc{0,2}def"    ! PASS
   ! pattern = "abc{1,2}def"    ! PASS
   ! pattern = "abc{0,2}def"    ! PASS
   ! pattern = "abc{1,2}def"    ! PASS
   ! pattern = "abc{2,2}def"    ! PASS
   ! pattern = "abc{2,3}def"    ! PASS

   ! pattern = "abc{2,3}def"     
   pattern = "(ああ|あい|あう)"
   prefix = ''

   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   postfix = get_postfix_literal(tree)
   print *, pattern, ": prefix:  ", prefix
   print *, pattern, ": postfix: ", postfix
   call tree%print(0)
   call tree%deallocate()
   
end program main