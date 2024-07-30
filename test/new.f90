program main
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_syntax_tree_m
   use :: forgex_parameters_m
   use :: forgex_automaton_m
   use :: forgex_segment_m
   implicit none

   type(tree_node_t), allocatable :: tree(:)
   integer(int32) :: root_i, top_index
   type(tape_t) :: tape
   character(:), allocatable :: string
   integer :: nfa_entry, nfa_exit
   integer(int32) :: i, dst

   type(segment_t), allocatable :: all_segments(:)
   type(automaton_t) :: automaton

   string = '([a-z]*)d'

   call build_syntax_tree(string, tape, tree, top_index)

#ifdef IMPURE
#ifdef DEBUG
   call dump_tree_table(tree)

   call print_tree(tree, top_index)
#endif
#endif
   call automaton%init(tree, top_index)

#ifdef IMPURE
#ifdef DEBUG
   call automaton%print()
#endif
#endif

end program main