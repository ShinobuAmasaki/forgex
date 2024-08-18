program main
   use :: forgex_syntax_tree_graph_m
   use :: forgex_syntax_tree_optimize_m
   implicit none
   
   type(tree_t) :: tree
   integer :: root, from, to

   logical :: res = .false.

   character(:), allocatable :: pattern, text, prefix, suffix

   pattern = ''

   ! pattern = "abc{0,2}def"    ! PASS
   ! pattern = "abc{1,2}def"    ! PASS
   ! pattern = "abc{0,2}def"    ! PASS
   ! pattern = "abc{1,2}def"    ! PASS
   ! pattern = "abc{2,2}def"    ! PASS
   ! pattern = "abc{2,3}def"    ! PASS

   ! pattern = "abc{2,3}def"     
   pattern = "c{2,3}"
   prefix = ''

   call tree%build(pattern)
   print *, "L26"
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()


   pattern = "c{4,7}" 
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()

   pattern = "c{4,10}" 
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()
   

   pattern = "c{5,10}" 
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()
   
   pattern = "(ab){3,4}"
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()

   pattern = "(ab|ac){3,4}"
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()
   
   pattern = "(ab|ac|a+){3,4}"
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()

   pattern = "(b+)a*(ab){3,4}"
   call tree%build(pattern)
   prefix = get_prefix_literal(tree)
   print *, pattern, " : ", prefix
   call tree%print(0)
   call tree%deallocate()
   
   ! suffix = get_suffix_literal(tree, root)
   ! print *, suffix


end program main