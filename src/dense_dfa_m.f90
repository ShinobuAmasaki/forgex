module forgex_dense_dfa_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m
   use :: forgex_automaton_m
   use :: forgex_nfa_state_set_m
   use :: forgex_lazy_dfa_node_m

   implicit none
   private

   public :: construct_dense_dfa
   public :: match_dense_dfa

contains

   function compute_reachable_state(automaton, curr) result(state_set)
      use :: forgex_parameters_m
      use :: forgex_nfa_node_m
      use :: forgex_segment_m
      implicit none
      type(automaton_t), intent(in) :: automaton
      integer, intent(in) :: curr
      type(nfa_state_set_t) :: state_set

      type(nfa_state_set_t) :: current_set
      type(nfa_state_node_t) :: n_node
      type(nfa_transition_t) :: n_tra
      integer :: i, j, k

      call init_state_set(state_set, automaton%nfa%nfa_top)

      current_set = automaton%dfa%nodes(curr)%nfa_set

      outer: do i = 1, automaton%nfa%nfa_top

         if (check_nfa_state(state_set, i)) then
            n_node = automaton%nfa%nodes(i)
            if (.not. allocated(n_node%forward)) cycle

            middle: do j = 1, n_node%forward_top
               n_tra = n_node%forward(j)

               if (n_tra%dst /= NFA_NULL_TRANSITION) then
                  call add_nfa_state(state_set, n_node%forward(j)%dst)
               end if

            end do middle 
         end if
      end do outer 
   end function compute_reachable_state


   subroutine destination(automaton, curr, next, next_set)
      use :: forgex_parameters_m
      implicit none
      type(automaton_t), intent(in) :: automaton
      integer(int32), intent(in) :: curr
      integer(int32), intent(inout) :: next
      type(nfa_state_set_t), intent(inout) :: next_set

      integer :: i

      next_set = compute_reachable_state(automaton, curr)

      next = DFA_INVALID_INDEX

      do i = 1, automaton%dfa%dfa_top-1
         if (equivalent_nfa_state_set(next_set, automaton%dfa%nodes(i)%nfa_set)) then
            next = i
            return
         end if
      end do
   end subroutine destination

   function move(automaton, curr) result(res)
      implicit none
      type(automaton_t), intent(in) :: automaton
      integer(int32), intent(in) :: curr
      type(dfa_transition_t) :: res

      type(nfa_state_set_t) :: set
      integer :: next

      call destination(automaton, curr, next, set)

      res%dst = next
      res%nfa_set = set
   end function move

   !> This subroutine convert NFA to DFA
   subroutine construct_dense_dfa(automaton, curr_i)
      implicit none
      type(automaton_t), intent(inout) :: automaton
      integer(int32), intent(in) :: curr_i

      ! Already automaton is initialized
      type(dfa_transition_t) :: d_tra
      integer :: prev_i, dst_i
      
   end subroutine construct_dense_dfa

   function match_dense_dfa(automaton, text) result(res)
      implicit none
      type(automaton_t), intent(in) :: automaton
      character(*), intent(in) :: text
      logical :: res
   end function match_dense_dfa
   
end module forgex_dense_dfa_m 