program main
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use forgex_nfa_m
   use forgex_syntax_tree_m
   use forgex_lazy_dfa_m
   use forgex_segment_m
   implicit none
 
   type(nfa_t), target :: nfa
   type(dfa_t) :: dfa
   type(tree_t), pointer :: root
   type(tape_t) :: tape
   type(d_state_t), pointer:: current

   root => build_syntax_tree(tape, "a*[a-z]*cd")


 
   ! ! Initialize NFA
   call nfa%init()
   call nfa%build(root) ! 適切な構文木を渡してください
 
   call nfa%print()

   ! ! Initialize DFA with the NFA
   call dfa%init(nfa)
   current => dfa%initial_dfa_state

   call dfa%construct(current, "a")
   call dfa%construct(current, "b")
   call dfa%construct(current, "c")
  
   call dfa%construct(current, "d")
   call dfa%print()

   current => dfa%initial_dfa_state
   call dfa%construct(current, "a")
   call dfa%construct(current, "a")
   call dfa%construct(current, "b")
   call dfa%construct(current, "c")
   call dfa%construct(current, "d")
   call dfa%print()

   current => dfa%initial_dfa_state
   call dfa%construct(current, "z")
   call dfa%construct(current, "c")
   call dfa%construct(current, "d")
   call dfa%print()
 
 
 end program main
 