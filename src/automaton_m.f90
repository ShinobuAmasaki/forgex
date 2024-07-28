module forgex_automaton_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m
   use :: forgex_segment_m
   use :: forgex_nfa_state_set_m
   use :: forgex_nfa_graph_m
   use :: forgex_lazy_dfa_graph_m
   implicit none
   private

   public :: automaton_t

   type, public :: automaton_t
      type(nfa_graph_t) :: nfa
      type(dfa_graph_t) :: dfa
      type(nfa_state_set_t) :: entry_set
      integer(int32) :: nfa_entry, nfa_exit
      integer(int32) :: initial_index
   contains
      procedure :: init => automaton__initialize
      procedure :: epsilon_closure => automaton__epsilon_closure
      procedure :: register => automaton__register
   end type automaton_t
      
contains


   pure subroutine automaton__initialize(self)
      implicit none
      class(automaton_t), intent(inout) :: self

      type(nfa_state_set_t) :: nfa_entry_set, initial_closure
      integer(int32) :: new_index

      call self%dfa%preprocess()

      call add_nfa_state(self%entry_set, self%nfa_exit)

      call self%epsilon_closure(self%entry_set, initial_closure)

      call self%register(initial_closure, new_index)

      self%initial_index = new_index
   end subroutine automaton__initialize


   pure subroutine automaton__epsilon_closure(self, state_set, closure)
      use :: forgex_nfa_node_m
      implicit none
      class(automaton_t), intent(inout) :: self
      type(nfa_state_set_t), intent(in) :: state_set
      type(nfa_state_set_t), intent(inout) :: closure

      type(nfa_transition_t) :: transition
      integer(int32) :: i, j, k

      closure = state_set

      !! もっと効率のよいループがあるはず

      ! すべてのNFA状態を走査する。
      do i = self%nfa%nfa_base + 1, self%nfa%nfa_top
         ! すべての順方向の遷移をスキャンする
         do j = 1, self%nfa%nodes(i)%forward_top

            ! 一時変数にコピー
            transition = self%nfa%nodes(i)%forward(j)

            if (transition%is_registered) then
               do k = 1, transition%c_top
                  
                  ! SEG_EMPTYの場合のみ処理を行う。
                  if ((transition%c(k) .in. SEG_EMPTY) .and. transition%dst /= NFA_NULL_TRANSITION) then
                     ! 
                     if (transition%own_j == self%nfa_entry) then ! これであっている？

                        call add_nfa_state(closure, transition%dst)

                     end if
                  end if
               end do
            end if
         end do
      end do

   end subroutine automaton__epsilon_closure


   pure subroutine automaton__register(self, state_set, res)
      implicit none
      class(automaton_t), intent(inout) :: self
      type(nfa_state_set_t), intent(in) :: state_set
      integer(int32), intent(inout) :: res ! resulting the new dfa index
      
      integer(int32) :: i, j, k

      ! 登録されている場合
      i = self%dfa%registered(state_set)
      if ( i /= DFA_INVALID_INDEX) then
         res = i
         return
      end if

      if (self%dfa%dfa_top >= self%dfa%dfa_limit) then
         error stop "Number of DFA states too large."
      end if

      k = self%dfa%dfa_top
      self%dfa%dfa_top = k + 1

      self%dfa%nodes(k)%nfa_set = state_set
      self%dfa%nodes(k)%accepted = check_nfa_state(state_set, self%nfa_exit)
      self%dfa%nodes(k)%registered = .true.
      allocate(self%dfa%nodes(k)%transition(DFA_TRANSITION_UNIT*1))
      self%dfa%nodes(k)%tra_top = 1
      res = k
   end subroutine automaton__register


   pure function automaton__compute_reachable_state(self, curr_i, symbol) result(res)
      use :: forgex_nfa_node_m
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32), intent(in) :: curr_i      ! current index of dfa
      character(*), intent(in) :: symbol
      type(nfa_state_set_t) :: res
      
      type(nfa_state_set_t) :: set
      type(nfa_state_node_t) :: n_node
      integer(int32) :: i, j, k

      set = self%dfa%nodes(curr_i)%nfa_set

      outer: do i = self%nfa%nfa_base+1, self%nfa%nfa_top
      if (check_nfa_state(set, i)) then

         n_node = self%nfa%nodes(i)

         middle: do j = 1, n_node%forward_top  
            
            if (.not. allocated(self%nfa%nodes(i)%forward)) cycle outer
            inner: do k = 1, n_node%forward(j)%c_top

               if (.not. (n_node%forward(j)%c(k) .in. [SEG_INIT, SEG_EMPTY, SEG_EPSILON])) then
                  call add_nfa_state(res, n_node%forward(j)%dst)
               end if
               
            end do inner
         end do middle
      end if
      end do outer


   end function automaton__compute_reachable_state



   

end module forgex_automaton_m