! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_dense_dfa_m module is a part of Forgex.
!
!! This file contains procedures for building a fully compiled DFA for debugging and benchmarking.
#ifdef IMPURE
#define pure
#endif
!> This module defines procedures for building a fully compiled DFA for debugging and benchmarking.
module forgex_dense_dfa_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only:NFA_NULL_TRANSITION, DFA_INVALID_INDEX, DFA_NOT_INIT
   use :: forgex_automaton_m, only: automaton_t
   use :: forgex_nfa_state_set_m, only: nfa_state_set_t, check_nfa_state, init_state_set, &
            add_nfa_state, equivalent_nfa_state_set
   use :: forgex_lazy_dfa_node_m, only: dfa_transition_t, dfa_state_node_t

   implicit none
   private

   public :: construct_dense_dfa
   public :: match_dense_dfa_exactly
   public :: match_dense_dfa_including

contains


   !> This function calculates a set of possible NFA states from the current DFA state.
   !>
   !> It scans through the NFA states and finds the set of reachable states excluding ε-transitions.
   pure function compute_reachable_state(automaton, curr) result(state_set)
      use :: forgex_nfa_node_m, only: nfa_state_node_t, nfa_transition_t
      implicit none
      type(automaton_t), intent(in) :: automaton
      integer, intent(in) :: curr
      type(nfa_state_set_t) :: state_set

      type(nfa_state_set_t) :: current_set
      type(nfa_state_node_t) :: n_node
      type(nfa_transition_t) :: n_tra
      integer :: i, j, k

      call init_state_set(state_set, automaton%nfa%nfa_top)

      if (.not. allocated(automaton%dfa%nodes(curr)%nfa_set%vec)) return

      current_set = automaton%dfa%nodes(curr)%nfa_set

      outer: do i = 1, automaton%nfa%nfa_top

         if (check_nfa_state(current_set, i)) then
            n_node = automaton%nfa%nodes(i)
            if (.not. allocated(n_node%forward)) cycle

            middle: do j = 1, n_node%forward_top
               n_tra = n_node%forward(j)
               do k = 1, n_tra%c_top
                  if (n_tra%dst /= NFA_NULL_TRANSITION) then
                     call add_nfa_state(state_set, n_node%forward(j)%dst)
                  end if
               end do
            end do middle
         end if
      end do outer
   end function compute_reachable_state


   !> This subroutine gets the next DFA nodes index from current index,
   !> and stores the result in `next` and `next_set`.
   pure subroutine destination(automaton, curr, next, next_set)
      implicit none
      type(automaton_t), intent(in) :: automaton
      integer(int32), intent(in) :: curr
      integer(int32), intent(inout) :: next
      type(nfa_state_set_t), intent(inout) :: next_set

      integer :: i

      next_set = compute_reachable_state(automaton, curr)

      ! すでに登録されたDFAがある場合はその添字を返し、ない場合は`DFA_INVALID_INDEX`を返す。
      !! If the DFA state is already registered, it returns the index,
      !! otherwise it returns `DFA_INVALID_INDEX`.
      next = DFA_INVALID_INDEX
      do i = 1, automaton%dfa%dfa_top-1
         if (equivalent_nfa_state_set(next_set, automaton%dfa%nodes(i)%nfa_set)) then
            next = i
            return
         end if
      end do
   end subroutine destination



   !> This function returns the dfa transition object, that contains the destination index
   !> and the corresponding set of transitionable NFA state.
   pure function move(automaton, curr) result(res)
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


   !> This subroutine convert an NFA into a fully compiled DFA.
   pure subroutine construct_dense_dfa(automaton, curr_i)
      use :: forgex_segment_m, only: SEG_EPSILON, operator(/=)
      implicit none
      type(automaton_t), intent(inout) :: automaton
      integer(int32), intent(in) :: curr_i

      ! Already automaton is initialized
      type(dfa_transition_t) :: d_tra
      integer :: dst_i, i, j, k, ii

      i =  curr_i
      outer: do while (i < automaton%dfa%dfa_top)
         d_tra = move(automaton, i)
         call automaton%nfa%collect_epsilon_transition(d_tra%nfa_set)

         if (.not. any(d_tra%nfa_set%vec)) then
            i = i + 1
            cycle
         end if

         dst_i = automaton%dfa%registered(d_tra%nfa_set)

         if (dst_i == DFA_INVALID_INDEX) then
            call automaton%register_state(d_tra%nfa_set, dst_i)
         end if

         if (dst_i == DFA_INVALID_INDEX) error stop "DFA registration failed."

         middle: do ii = 1, automaton%nfa%nfa_top

            if (.not. allocated(automaton%nfa%nodes(ii)%forward))  cycle middle

            inner: do j = 1, automaton%nfa%nodes(ii)%forward_top

               if (automaton%nfa%nodes(ii)%forward(j)%dst == NFA_NULL_TRANSITION) cycle middle


               if (check_nfa_state(d_tra%nfa_set, automaton%nfa%nodes(ii)%forward(j)%dst)) then
                  core: do k = 1, automaton%nfa%nodes(ii)%forward(j)%c_top
                     if (automaton%nfa%nodes(ii)%forward(j)%c(k) /= SEG_EPSILON) then
                        call automaton%dfa%add_transition(d_tra%nfa_set, i, dst_i, &
                              automaton%nfa%nodes(ii)%forward(j)%c(k))
                     end if
                  end do core
               end if

            end do inner
         end do middle

         i = i + 1
      end do outer
   end subroutine construct_dense_dfa


   !> This function returns the index of the destination DFA state from the
   !> index of the current automaton DFA state array and the input symbol.
   pure function next_state_dense_dfa(automaton, curr_i, symbol) result(dst_i)
      use :: forgex_segment_m, only: symbol_to_segment, operator(.in.)
      implicit none
      type(automaton_t), intent(in) :: automaton
      integer(int32), intent(in) :: curr_i
      character(*), intent(in) :: symbol

      type(dfa_state_node_t) :: d_node
      type(dfa_transition_t) :: d_tra

      integer(int32) :: dst_i, j

      d_node = automaton%dfa%nodes(curr_i)
      dst_i = DFA_INVALID_INDEX
      do j = 1, d_node%get_tra_top()
         d_tra = d_node%transition(j)
         if (symbol_to_segment(symbol) .in. d_tra%c) then
            dst_i = d_tra%dst
            return
         end if
      end do
   end function next_state_dense_dfa


   !> This procedure reads a text, performs regular expression matching using compiled DFA,
   !> and returns `.true.` if it matches exactly.
   pure function match_dense_dfa_exactly(automaton, string) result(res)
      use :: forgex_utf8_m, only: idxutf8
      implicit none
      type(automaton_t), intent(in) :: automaton
      character(*), intent(in) :: string
      logical :: res

      integer :: cur_i, dst_i ! current and destination index of DFA nodes
      integer :: ci           ! character index
      integer :: next_ci      ! next character index
      integer :: max_match    !

      cur_i = automaton%initial_index

      if (cur_i == DFA_NOT_INIT) then
         error stop "DFA have not been initialized."
      end if

      if (len(string) == 0) then
         res = automaton%dfa%nodes(cur_i)%accepted
         return
      end if

      max_match = 0
      ci = 1
      do while(cur_i /= DFA_INVALID_INDEX)
         if (automaton%dfa%nodes(cur_i)%accepted) then
            max_match = ci
         end if

         if (ci > len(string)) exit

         next_ci = idxutf8(string, ci) + 1

         dst_i = next_state_dense_dfa(automaton, cur_i, string(ci:next_ci-1))

         cur_i = dst_i
         ci = next_ci
      end do

      if (max_match == len(string)+1) then
         res = .true.
      else
         res = .false.
      end if
   end function match_dense_dfa_exactly

   !> This procedure reads a text, performs regular expression matching using an automaton,
   !> and stores the string index in the argument if it contains a match.
   subroutine match_dense_dfa_including(automaton, string, from, to)
      use :: forgex_utf8_m, only: idxutf8
      implicit none
      type(automaton_t), intent(in) :: automaton
      character(*), intent(in) :: string
      integer, intent(inout) :: from, to

      integer :: cur_i, dst_i ! current and destination index of DFA nodes
      integer :: ci           ! character index
      integer :: next_ci      ! next character index
      integer :: max_match    ! maximum value of match attempts
      integer :: start        ! starting character index

      from = 0
      to = 0

      cur_i = automaton%initial_index
      if (cur_i == DFA_NOT_INIT) then
         error stop "DFA have not been initialized"
      end if

      if (string == char(10)//char(10)) then
         if (automaton%dfa%nodes(cur_i)%accepted) then
            from = 1
            to = 1
         end if
         return
      end if

      start = 1
      do while (start < len(string))
         max_match = 0
         ci = start
         cur_i = automaton%initial_index

         do while (cur_i /= DFA_INVALID_INDEX)

            if (automaton%dfa%nodes(cur_i)%accepted .and. ci /= start) then
               max_match = ci
            end if

            if (ci > len(string)) exit

            next_ci = idxutf8(string, ci) + 1

            dst_i = next_state_dense_dfa(automaton, cur_i, string(ci:next_ci-1))
            cur_i = dst_i
            ci = next_ci

         end do

         if (max_match > 1) then
            from = start
            to = max_match - 1
            return
         end if

         start = idxutf8(string, start) +1
      end do
   end subroutine match_dense_dfa_including

end module forgex_dense_dfa_m