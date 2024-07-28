#ifdef PURE
#define pure
#endif

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
      type(segment_t), allocatable :: all_segments(:)
      integer(int32) :: nfa_entry, nfa_exit
      integer(int32) :: initial_index
   contains
      procedure :: init            => automaton__initialize
      procedure :: epsilon_closure => automaton__epsilon_closure
      procedure :: register        => automaton__register
      procedure :: construct       => automaton__construct_dfa
      procedure :: get_reachable   => automaton__compute_reachable_state
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


   !! WIP
   pure function automaton__compute_reachable_state(self, curr_i, symbol) result(res)
      use :: forgex_nfa_node_m
      use :: forgex_lazy_dfa_node_m, only: dfa_transition_t
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32), intent(in) :: curr_i      ! current index of dfa
      character(*), intent(in) :: symbol
      type(dfa_transition_t), allocatable:: res(:)
      
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
                  call add_nfa_state(set, n_node%forward(j)%dst)
               end if
               
            end do inner
         end do middle
      end if
      end do outer

   end function automaton__compute_reachable_state


   pure subroutine automaton__move(self, curr, symbol, res)
      use :: forgex_lazy_dfa_node_m, only: dfa_transition_t
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32), intent(in) :: curr
      character(*), intent(in) :: symbol

      type(dfa_transition_t), allocatable, intent(inout) :: res(:)
      
      integer :: i

      do i = DFA_INITIAL_INDEX, self%dfa%dfa_top
         res = self%get_reachable(curr, symbol)
         if (allocated(res)) return
      end do
      

   end subroutine automaton__move


   pure subroutine automaton__construct_dfa (self, curr_i, dst_i, symbol)
      implicit none
      class(automaton_t), intent(inout) :: self
      integer(int32), intent(in) :: curr_i
      integer(int32), intent(inout) :: dst_i
      character(*), intent(in) :: symbol
      
      type(segment_t), allocatable :: segments(:)
      integer(int32) :: prev_i

      prev_i = curr_i
      segments(:) = self%all_segments(:)

      ! ε遷移を除いた行き先のstate_setを取得する
      



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