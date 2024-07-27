module forgex_api_internal_m
   use :: forgex_parameters_m
   use :: forgex_nfa_m
   use :: forgex_lazy_dfa_m
   implicit none
   private
contains

   pure subroutine do_matching_exactly(nfa_graph, dfa_graph, string, res)
      implicit none
      type(nfa_state_node_t), intent(in) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      type(dfa_state_node_t), intent(inout) :: dfa_graph(DFA_STATE_BASE:DFA_STATE_LIMIT)
      character(*), intent(in) :: string
      logical, intent(inout) :: res

      type(dfa_state_node_t) :: cur ! current
      type(dfa_state_node_t) :: dst ! destination

      cur = dfa_graph(DFA_STATE_BASE+1)

   end subroutine do_matching_exactly

   
end module forgex_api_internal_m