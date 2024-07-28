#ifdef PURE
#define pure
#endif
module forgex_automaton_m
   use, intrinsic :: iso_fortran_env, only: int32, stderr=>error_unit
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
      type(segment_t), allocatable :: all_segments(:)
      integer(int32) :: nfa_entry, nfa_exit
      integer(int32) :: initial_index
   contains
      procedure :: init            => automaton__initialize
      procedure :: epsilon_closure => automaton__epsilon_closure
      procedure :: register        => automaton__register
      procedure :: construct       => automaton__construct_dfa
      procedure :: get_reachable   => automaton__compute_reachable_state
      procedure :: move            => automaton__move
      procedure :: destination     => automaton__destination
#ifdef DEBUG
      procedure :: print           => automaton__print_info
#endif
   end type automaton_t

contains


   pure subroutine automaton__initialize(self, tree, tree_top)
      use :: forgex_syntax_tree_m, only: tree_node_t
      implicit none
      class(automaton_t), intent(inout) :: self
      type(tree_node_t), intent(in) :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32), intent(in) :: tree_top

      type(nfa_state_set_t) :: nfa_entry_set, initial_closure
      integer(int32) :: new_index

      !-- NFA building
      call self%nfa%build(tree, tree_top, self%nfa_entry, self%nfa_exit, self%all_segments)

#ifdef pure
#ifdef DEBUG
      call self%nfa%print()
#endif
#endif

      !-- DFA initialize
      call self%dfa%preprocess()

      call add_nfa_state(self%entry_set, self%nfa_entry)

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

      ! ! すべてのNFA状態を走査する。
      do i = self%nfa%nfa_base + 1, self%nfa%nfa_top
         ! すべての順方向の遷移をスキャンする
         do j = 1, self%nfa%nodes(i)%forward_top

            ! 一時変数にコピー
            transition = self%nfa%nodes(i)%forward(j)

            if (self%nfa%nodes(i)%forward(j)%is_registered) then
               do k = 1, transition%c_top

                  if ((transition%c(k) .in. SEG_EPSILON) .and. transition%dst /= NFA_NULL_TRANSITION) then
                  !
                     if (i == self%nfa_entry) then ! これであっている？

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


   !! WIP
   pure function automaton__compute_reachable_state(self, curr_i, symbol) result(state_set)
      use :: forgex_segment_m
      use :: forgex_nfa_node_m
      use :: forgex_lazy_dfa_node_m, only: dfa_transition_t
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32), intent(in) :: curr_i      ! current index of dfa
      character(*), intent(in) :: symbol


      type(nfa_state_set_t) :: state_set
      type(nfa_state_node_t) :: n_node
      type(nfa_transition_t) :: n_tra
      type(segment_t) :: symbol_belong
      integer(int32) :: i, j, k

      integer :: d_tra_top

      symbol_belong = SEG_INIT
      ! nfa状態をスキャン
      ! Scan through NFA states
      outer: do i = self%nfa%nfa_base+1, self%nfa%nfa_top

         ! 現在の状態集合のi番目が真ならば、i番NFAノードの処理を行う
         if (check_nfa_state(self%dfa%nodes(curr_i)%nfa_set, i)) then

            n_node = self%nfa%nodes(i)

            if (.not. allocated(n_node%forward)) cycle outer

            middle: do j = 1, n_node%forward_top

               ! temporary variables
               n_tra = n_node%forward(j)
               d_tra_top = self%dfa%nodes(curr_i)%tra_top


               if (.not. allocated(n_tra%c)) cycle middle

               do k = 1, n_tra%c_top
                  if (n_tra%c(k) /= SEG_EPSILON .and. n_tra%c(k) /= SEG_EMPTY .and. n_tra%c(k) /= SEG_INIT) then

                     if (.not. allocated(self%dfa%nodes(curr_i)%transition(d_tra_top)%c)) cycle

                     if ((self%dfa%nodes(curr_i)%transition(d_tra_top)%c(k) .in. n_tra%c) &
                      .and. self%dfa%nodes(curr_i)%transition(d_tra_top)%dst /= DFA_NULL_TRANSITION) then
                        call add_nfa_state(state_set, n_tra%dst)
                     endif

                  end if
               end do

               if (n_tra%dst /= NFA_NULL_TRANSITION) then

                  symbol_belong = which_segment_symbol_belong(self%all_segments, symbol)
                  call add_nfa_state(state_set, n_tra%dst)

               end if

            end do middle
         end if
      end do outer



   end function automaton__compute_reachable_state


   pure subroutine automaton__destination(self, curr, symbol, next, next_set)
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32), intent(in) :: curr
      character(*), intent(in) :: symbol
      integer(int32), intent(inout) :: next
      type(nfa_state_set_t), intent(inout) :: next_set

      next_set = self%get_reachable(curr, symbol)

   end subroutine automaton__destination



   pure function automaton__move(self, curr, symbol) result(res)
      use :: forgex_lazy_dfa_node_m, only: dfa_transition_t
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32), intent(in) :: curr    ! currnet
      character(*), intent(in) :: symbol

      type(dfa_transition_t) :: res

      integer :: i, j

      integer(int32) :: dst
      type(nfa_state_set_t) :: set

      call self%destination(curr, symbol, dst, set)
      if (dst /= DFA_INVALID_INDEX) then
            res%c = [symbol_to_segment(symbol)]
            res%dst = dst
            res%nfa_set = set
            res%own_j = self%dfa%dfa_top
      end if

   end function automaton__move


   pure subroutine automaton__construct_dfa (self, curr_i, dst_i, symbol)
      use :: forgex_lazy_dfa_node_m
      implicit none
      class(automaton_t), intent(inout) :: self
      integer(int32), intent(in) :: curr_i
      integer(int32), intent(inout) :: dst_i
      character(*), intent(in) :: symbol

      type(dfa_transition_t) :: x
      type(segment_t), allocatable :: segments(:)
      integer(int32) :: prev_i, res

      dst_i = DFA_INVALID_INDEX
      prev_i = curr_i
      segments = self%all_segments

      ! ε遷移を除いた行き先のstate_setを取得する
      x = self%move(prev_i, symbol)

      ! この実装ではリダクションの必要がない
      if (x%dst == DFA_INVALID_INDEX) return

      call self%nfa%collect_e_t(x%nfa_set)

      dst_i = self%dfa%registered(x%nfa_set)

      if (dst_i == DFA_INVALID_INDEX) then
         ! まだDFA状態が登録されていない場合
         call self%register(x%nfa_set, dst_i)
      end if

      if (dst_i == DFA_INVALID_INDEX) error stop "DFA registration failed."
      ! 遷移を追加する
      call self%dfa%add_transition(x%nfa_set, prev_i, dst_i, [which_segment_symbol_belong(self%all_segments, symbol)])


   end subroutine automaton__construct_dfa

#ifdef DEBUG

   subroutine automaton__print_info(self)
      use :: iso_fortran_env, only: stderr => error_unit
      implicit none
      class(automaton_t), intent(in) :: self

      write(stderr, *) "--- AUTOMATON INFO ---"
      write(stderr, *) "entry_set: ", self%entry_set%vec(1:self%nfa%nfa_top)
      write(stderr, *) "allocated(all_segments):", allocated(self%all_segments)
      write(stderr, *) "nfa_entry:     ", self%nfa_entry
      write(stderr, *) "nfa_exit:      ", self%nfa_exit
      write(stderr, *) "initial_index: ", self%initial_index

   end subroutine automaton__print_info

#endif

end module forgex_automaton_m