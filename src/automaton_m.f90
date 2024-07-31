! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_automaton_m module is a part of Forgex.
!
!! This file contains the definition of `automaton_t` class and its type-bound procedures.
!
!> The `forgex_automaton_m` module contains `automaton_t` definition and its type-bound procedures.
!> 
#ifdef IMPURE
#define pure
#endif
module forgex_automaton_m
   use, intrinsic :: iso_fortran_env, only: int32, stderr=>error_unit
   use :: forgex_parameters_m, only: DFA_NOT_INIT, TREE_NODE_BASE, TREE_NODE_LIMIT, &
         NFA_NULL_TRANSITION, DFA_INVALID_INDEX, DFA_TRANSITION_UNIT, DFA_INITIAL_INDEX
   use :: forgex_segment_m
   use :: forgex_nfa_state_set_m
   use :: forgex_nfa_graph_m
   use :: forgex_lazy_dfa_graph_m
   implicit none
   private

   type, public :: automaton_t
      !! This type contains an NFA graph, and the DFA graph that are derived from it.
      type(nfa_graph_t)            :: nfa
      type(dfa_graph_t)            :: dfa
      type(nfa_state_set_t)        :: entry_set
      type(segment_t), allocatable :: all_segments(:)
      integer(int32)               :: nfa_entry, nfa_exit
      integer(int32)               :: initial_index = DFA_NOT_INIT
   contains
      procedure :: init            => automaton__initialize
      procedure :: epsilon_closure => automaton__epsilon_closure
      procedure :: register_state  => automaton__register_state
      procedure :: construct       => automaton__construct_dfa
      procedure :: get_reachable   => automaton__compute_reachable_state
      procedure :: move            => automaton__move
      procedure :: destination     => automaton__destination
#if  defined(IMPURE) && defined(DEBUG)
      procedure :: print           => automaton__print_info
      procedure :: print_dfa       => automaton__print_dfa
#endif
   end type automaton_t

contains

   !> This subroutine reads `tree` and `tree_top` variable, constructs the NFA graph,
   !> and then initializes the DFA graph. 
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


#if  defined(IMPURE) && defined(DEBUG)
      call self%nfa%print()
#endif


      !-- DFA initialize
      ! Invokes DFA preprocessing.
      call self%dfa%preprocess()

      ! Check if it has been initialized.
      if (self%dfa%dfa_top /= DFA_INITIAL_INDEX) then
         error stop "DFA graph initialization is failed."
      end if

      ! Constructing a DFA initial state from the NFA initial state.
      call add_nfa_state(self%entry_set, self%nfa_entry)

      ! Add an NFA node reachable by epsilon transitions to the entrance state set within DFA.
      call self%epsilon_closure(self%entry_set, initial_closure)

      ! Assign the computed initial closure into self%entry_set
      self%entry_set = initial_closure
      
      ! Register `entry_set` as a new DFA state in the graph.
      call self%register_state(self%entry_set, new_index)

      ! Assign the returned index to the `initial_index` of the graph.
      self%initial_index = new_index

   end subroutine automaton__initialize


   !> Compute the ε-closure for a set of NFA states.
   !>
   !> The ε-closure is the set of NFA states reachable from a given set of NFA states via ε-transition.
   !> This subroutine calculates the ε-closure and stores it in the `closure` parameter.
   pure subroutine automaton__epsilon_closure(self, state_set, closure)
      use :: forgex_nfa_node_m
      implicit none
      class(automaton_t), intent(inout) :: self
      type(nfa_state_set_t), intent(in) :: state_set
      type(nfa_state_set_t), intent(inout) :: closure

      type(nfa_transition_t) :: transition
      integer(int32) :: i, j, k

      closure = state_set

      i = self%nfa_entry
      ! ! すべての順方向の遷移をスキャンする
      do j = 1, self%nfa%nodes(i)%forward_top

         ! 一時変数にコピー
         transition = self%nfa%nodes(i)%forward(j)

         if (self%nfa%nodes(i)%forward(j)%is_registered) then
            do k = 1, transition%c_top

               if ((transition%c(k) .in. SEG_EPSILON) .and. transition%dst /= NFA_NULL_TRANSITION) then

                  call add_nfa_state(closure, transition%dst)
               end if
            end do
         end if
      end do

   end subroutine automaton__epsilon_closure


   !> This subroutine takes `nfa_state_set_t` as input and register the set as the DFA state in the DFA.  
   !> The result is returned as a pointer to the DFA state.
   pure subroutine automaton__register_state(self, state_set, res)
      implicit none
      class(automaton_t),    intent(inout) :: self
      type(nfa_state_set_t), intent(in)    :: state_set
      integer(int32),        intent(inout) :: res       ! resulting the new dfa index

      integer(int32) :: i, j, k

      ! If the set is already registered, returns the index of the corresponding DFA state. 
      i = self%dfa%registered(state_set)
      if ( i /= DFA_INVALID_INDEX) then
         res = i
         return
      end if

      ! Execute an error stop statement if the counter exceeds a limit. 
      if (self%dfa%dfa_top >= self%dfa%dfa_limit) then
         error stop "Number of DFA states too large."
      end if

      !> @note The processing here should reflect the semantic change of `dfa_top`.
      i = self%dfa%dfa_top
      self%dfa%dfa_top = i + 1 ! increment dfa_top

      self%dfa%nodes(i)%nfa_set    = state_set
      self%dfa%nodes(i)%accepted   = check_nfa_state(state_set, self%nfa_exit)
      self%dfa%nodes(i)%registered = .true.

      call self%dfa%nodes(i)%init_transition()
      call self%dfa%nodes(i)%increment_tra_top() ! Somehow this is necessary!

      res = i

   end subroutine automaton__register_state



   !> This function calculates a set of possible NFA states from the current DFA state by the input
   !> character `symbol`. 
   !>
   !> It scans through the NFA states and finds the set of reachable states by the given input `symbol`,
   !> excluding ε-transitions.
   pure function automaton__compute_reachable_state(self, curr_i, symbol) result(state_set)
      use :: forgex_segment_m
      use :: forgex_nfa_node_m
      use :: forgex_lazy_dfa_node_m, only: dfa_transition_t
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32),     intent(in) :: curr_i      ! current index of dfa
      character(*),       intent(in) :: symbol

      type(nfa_state_set_t)  :: state_set
      type(nfa_state_set_t)  :: current_set
      type(nfa_state_node_t) :: n_node       ! This variable simulates a pointer.
      integer, parameter :: TMP_NODE_SIZE = DFA_TRANSITION_UNIT * 10
      type(dfa_transition_t) :: transitions(TMP_NODE_SIZE)
      integer(int32)         :: i, j, k, jj

      integer :: num_nfa_states

      num_nfa_states = self%nfa%nfa_top

      current_set = self%dfa%nodes(curr_i)%nfa_set

      ! Scan the entire NFA states.
      outer: do i = 1, num_nfa_states
         ! If the i-th element of current state set is true, process the i-th NFA node.
         if (check_nfa_state(current_set, i)) then
            
            ! 
            n_node = self%nfa%nodes(i)

            if (.not. allocated(self%nfa%nodes(i)%forward)) cycle
            
            jj = 1 ! loop variable for temporary DFA nodes

            middle: do j = 1, n_node%forward_top

               if (jj > TMP_NODE_SIZE) then
                  ! reallocation
               end if

               if (n_node%forward(j)%dst /= NFA_NULL_TRANSITION) then

                  inner: do k = 1, n_node%forward(j)%c_top
                     if ( (symbol_to_segment(symbol) .in. n_node%forward(j)%c) &
                          .or. (n_node%forward(j)%c(k) == SEG_EPSILON)) then

                        transitions(jj)%c = which_segment_symbol_belong(self%all_segments, symbol)
                        call add_nfa_state(state_set, n_node%forward(j)%dst)

                        jj = jj + 1
                     end if
                  end do inner

               end if

            end do middle

         end if
      end do outer

   end function automaton__compute_reachable_state


   !> This subroutine gets the next DFA nodes index from current index and symbol,
   !> and stores the result in `next` and `next_set`.
   pure subroutine automaton__destination(self, curr, symbol, next, next_set)
      implicit none
      class(automaton_t),    intent(in) :: self
      integer(int32),        intent(in) :: curr
      character(*),          intent(in) :: symbol
      integer(int32),        intent(inout) :: next
      type(nfa_state_set_t), intent(inout) :: next_set

      integer :: i

      ! Get a set of NFAs for which current state can transition, excluding epsilon-transitions.
      next_set = self%get_reachable(curr, symbol)

      ! Initialize the next value
      next = DFA_INVALID_INDEX
      
      ! Scan the entire DFA nodes.
      do i = 1, self%dfa%dfa_top

         ! If there is an existing node corresponding to the NFA state set,
         ! return the index of that node.
         if (equivalent_nfa_state_set(next_set, self%dfa%nodes(i)%nfa_set)) then
            next = i
            return
         end if

      end do 
   end subroutine automaton__destination

   
   !> This function returns the dfa transition object, that contains the destination index
   !> and the corresponding set of transitionable NFA state.  
   pure function automaton__move(self, curr, symbol) result(res)
      use :: forgex_lazy_dfa_node_m, only: dfa_transition_t
      implicit none
      class(automaton_t), intent(in) :: self
      integer(int32),     intent(in) :: curr    ! current index
      character(*),       intent(in) :: symbol  ! input symbol
      type(dfa_transition_t)         :: res

      type(nfa_state_set_t) :: set
      integer(int32)        :: next

      call self%destination(curr, symbol, next, set)

      ! Set the value of each component of the returned object.
      res%dst = next                      ! valid index of DFA node or DFA_INVALID_INDEX
      res%nfa_set = set
      
      ! res%c = symbol_to_segment(symbol) ! this component would not be used.
      ! res%own_j = DFA_INITIAL_INDEX     ! this component would not be used.
   end function automaton__move


   !> This subroutine gets the destination index of DFA nodes from the current index with given symbol,
   !> adding a DFA node if necessary.
   !>
   !> It calculates the set of NFA states that can be reached from the `current` node for the given `symbol`, 
   !> excluding epsilon transitions, and then registers the new DFA state node if it has not already been registered.
   !> Finally, it adds the transition from the `current` node to the `destination` node in the DFA graph.
   pure subroutine automaton__construct_dfa (self, curr_i, dst_i, symbol)
      use :: forgex_lazy_dfa_node_m
      implicit none
      class(automaton_t), intent(inout) :: self
      integer(int32),     intent(in)    :: curr_i
      integer(int32),     intent(inout) :: dst_i
      character(*),       intent(in)    :: symbol

      type(dfa_transition_t) :: d_tra
      type(segment_t), allocatable :: segments(:)
      integer(int32) :: prev_i, res

      dst_i = DFA_INVALID_INDEX
      prev_i = curr_i
      segments = self%all_segments

      ! ε遷移を除いた行き先のstate_setを取得する。
      ! Get the state set for the destination excluding epsilon-transition.
      d_tra = self%move(prev_i, symbol)

      ! この実装ではリストのリダクションを計算する必要がない。
      !! In this implementation with array approach, array reduction is done in the reachable procedure. 

      ! ε遷移との和集合を取り、d_tra%nfa_setに格納する。
      ! Combine the state set with epsilon-transitions and store in `d_tra%nfa_set`.
      call self%nfa%collect_epsilon_transition(d_tra%nfa_set)

      ! 空のNFA状態集合の登録を禁止する
      if (.not. any(d_tra%nfa_set%vec)) then
         dst_i = DFA_INVALID_INDEX
         return
      end if

      dst_i = self%dfa%registered(d_tra%nfa_set)

      ! まだDFA状態が登録されていない場合は、新しく登録する。
      ! If the destination index is DFA_INVALID_INDEX, register a new DFA node.
      if (dst_i == DFA_INVALID_INDEX) then
         call self%register_state(d_tra%nfa_set, dst_i)
      end if

      ! If the destination index is DFA_INVALID_INDEX, the registration is failed.
      if (dst_i == DFA_INVALID_INDEX) error stop "DFA registration failed."

      ! 遷移を追加する
      ! Add a DFA transition from `prev` to `next` for the given `symbol`.
      call self%dfa%add_transition(d_tra%nfa_set, prev_i, dst_i, which_segment_symbol_belong(self%all_segments, symbol))
   end subroutine automaton__construct_dfa


!=====================================================================!


#if defined(IMPURE) && defined(DEBUG)

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

   subroutine automaton__print_dfa(self)
      use, intrinsic :: iso_fortran_env, only: stderr => error_unit
      use :: forgex_nfa_state_set_m
      use :: forgex_lazy_dfa_node_m
      implicit none
      class(automaton_t), intent(in) :: self
      type(dfa_transition_t) :: p
      integer(int32) :: i, j, k

      write(stderr,*) "--- PRINT DFA---"

      do i = 1, self%dfa%dfa_top -1

         if (self%dfa%nodes(i)%accepted) then
            write(stderr, '(i2,a, a)', advance='no') i, 'A', ": "
         else
            write(stderr, '(i2,a, a)', advance='no') i, ' ', ": "
         end if

         do j = 2, self%dfa%nodes(i)%get_tra_top()
            p = self%dfa%nodes(i)%transition(j)
            write(stderr, '(a, a, i0, 1x)', advance='no') p%c%print(), '=>', p%dst

         end do
         write(stderr, *) ""
      end do 

      do i = 1, self%dfa%dfa_top - 1
         if (self%dfa%nodes(i)%accepted) then
            write(stderr, '(a, i2, a)', advance='no') "state ", i, 'A = ( '
         else
            write(stderr, '(a, i2, a)', advance='no') "state ", i, '  = ( '
         end if

         call print_nfa_state_set(self%dfa%nodes(i)%nfa_set, self%nfa%nfa_top)

         write(stderr,'(a)') ")"
      end do
   end subroutine automaton__print_dfa

#endif

end module forgex_automaton_m