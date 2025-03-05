! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_lazy_dfa_graph_m module is a part of Forgex.
!
!! This file contains `dfa_graph_t` class definition and its type-bound procedures.
#ifdef IMPURE
#define pure
#endif
!> This module defines a derived-type `dfa_graph_t` that contains all the states of the DFA.
module forgex_lazy_dfa_graph_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only: DFA_STATE_BASE, DFA_STATE_UNIT, DFA_STATE_HARD_LIMIT, &
                                     DFA_INITIAL_INDEX, DFA_INVALID_INDEX
   use :: forgex_lazy_dfa_node_m, only: dfa_state_node_t, dfa_transition_t

   implicit none
   private

   type, public :: dfa_graph_t
      !! This type has the entire graph of DFA states.
      type(dfa_state_node_t), allocatable :: nodes(:)
      integer(int32) :: dfa_base  = DFA_STATE_BASE
      integer(int32) :: dfa_limit = DFA_STATE_UNIT
      integer(int32) :: dfa_top   = DFA_INVALID_INDEX
      integer(int32) :: alloc_count_node = 0
   contains
      procedure :: preprocess     => lazy_dfa__preprocess
      procedure :: registered     => lazy_dfa__registered_index
      procedure :: add_transition => lazy_dfa__add_transition
      procedure :: free           => lazy_dfa__deallocate
      procedure :: reallocate     => lazy_dfa__reallocate
   end type dfa_graph_t

contains

   !> This subroutine determines the number of DFA nodes the graph has
   !> and allocate the array.
   pure subroutine lazy_dfa__preprocess (self)
      implicit none
      class(dfa_graph_t), intent(inout) :: self

      integer(int32) :: i, base, limit

      ! Initialize DFA
      base = self%dfa_base
      limit = self%dfa_limit

      allocate(self%nodes(base:limit))

      self%alloc_count_node = 1

      self%nodes(:)%own_i = [(i, i=base, limit)]

      self%dfa_top = DFA_INITIAL_INDEX    ! Acts as an initialized flag

   end subroutine lazy_dfa__preprocess


   !> This subroutine performs reallocating array that represents the DFA graph.
   !>
   !> It evaluates the current upper limit for the array reallocation request call,
   !> and if the hard limit is not exceeded, performs the reallocation and updates the
   !> upper limit, otherwise the program stops with `ERROR STOP`.
   pure subroutine lazy_dfa__reallocate(self)
      implicit none
      class(dfa_graph_t), intent(inout) :: self
      type(dfa_state_node_t), allocatable :: tmp(:)

      integer :: siz, prev_count, i
      integer :: new_part_begin, new_part_end

      if (allocated(self%nodes)) then
         siz = size(self%nodes, dim=1) -1
         allocate(tmp(siz))
         call move_alloc(self%nodes, tmp)
      else
         siz = 0
      endif

      prev_count = self%alloc_count_node
      self%alloc_count_node = prev_count + 1

      new_part_begin = siz + 1
      new_part_end = siz*2

      if (new_part_end > DFA_STATE_HARD_LIMIT) then
         error stop "Too many DFA state nodes requested."
      end if

      allocate(self%nodes(0:new_part_end))

      self%nodes(1:siz) = tmp(1:siz)

      self%nodes(new_part_begin:new_part_end)%own_i = [(i, i=new_part_begin, new_part_end)]

      self%dfa_limit = new_part_end
   end subroutine lazy_dfa__reallocate


   !> This subroutine performs deallocation of the arrays representing 
   !> the DFA node transitions for every node in the DFA graph.
   pure subroutine lazy_dfa__deallocate(self)
      implicit none
      class(dfa_graph_t), intent(inout) :: self

      integer :: i
      if (.not. allocated(self%nodes)) return
      do i = 1, self%dfa_limit
         call self%nodes(i)%free()
      end do

   end subroutine lazy_dfa__deallocate


   ! DFA状態がすでに登録されているかを、添字で返す。登録されていなければDFA_INVALID_INDEXを返す。
   !> Returns whether the DFA state is already registered by index,
   !> or DFA_INVALID_INDEX if it is not registered.
   pure function lazy_dfa__registered_index(self, set) result(res)
      use :: forgex_nfa_state_set_m
      implicit none
      class(dfa_graph_t), intent(in) :: self
      type(nfa_state_set_t), intent(in) :: set
      integer(int32) :: res

      integer(int32) :: i
      logical :: is_registered

      ! Initialize the result variable.
      res = DFA_INVALID_INDEX

      do i = DFA_INITIAL_INDEX, self%dfa_top
         if (.not. allocated(self%nodes(i)%nfa_set%vec)) cycle
         is_registered = equivalent_nfa_state_set(self%nodes(i)%nfa_set, set)
         if (is_registered) then
            res = i
            return
         end if
      end do
   end function lazy_dfa__registered_index


   !> This subroutine construct an new transition object from the arguments,
   !> and invokes the type-bound procedure of `dfa_state_node_t` with it.
   pure subroutine lazy_dfa__add_transition(self, state_set, src, dst, seg)
      use :: forgex_segment_m
      use :: forgex_nfa_state_set_m
      implicit none
      class(dfa_graph_t),    intent(inout) :: self
      type(nfa_state_set_t), intent(in)    :: state_set
      integer,               intent(in)    :: src, dst
      type(segment_t),       intent(in)    :: seg

      type(dfa_transition_t) :: tra

      tra%c = seg
      tra%dst = dst

      tra%nfa_set = state_set

      call self%nodes(src)%add_transition(tra)

   end subroutine lazy_dfa__add_transition


end module forgex_lazy_dfa_graph_m