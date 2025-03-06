! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_lazy_dfa_m module is a part of Forgex.
!
!! This file contains definitions of `dfa_transition_t` type and `dfa_state_node_t` class,
!! and its type-bound procedures.
#ifdef IMPURE
#define pure
#endif
!> The `forgex_lazy_dfa_node_m` module defines the state nodes and transitions of DFA.
module forgex_lazy_dfa_node_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only: DFA_NOT_INIT, DFA_NOT_INIT_TRAENSITION_TOP, &
         DFA_TRANSITION_UNIT, DFA_INIT_TRANSITION_TOP, DFA_TRANSITION_BASE, &
         DFA_NOT_INIT_TRAENSITION_TOP, ALLOC_COUNT_INITTIAL
   use :: forgex_segment_m, only: segment_t
   use :: forgex_nfa_state_set_m, only: nfa_state_set_t
   implicit none
   private

   public :: copy_dfa_transition

   type, public :: dfa_transition_t
      type(segment_t)       :: c
      type(nfa_state_set_t) :: nfa_set
      integer(int32)        :: own_j = DFA_NOT_INIT ! Own index in the list of transitions
      integer(int32)        :: dst   = DFA_NOT_INIT ! The destination node index of DFA graph.
   end type dfa_transition_t


   type, public :: dfa_state_node_t
      integer(int32)                      :: own_i = DFA_NOT_INIT
      type(nfa_state_set_t)               :: nfa_set
      logical                             :: accepted = .false.
      type(dfa_transition_t), allocatable :: transition(:)
      integer(int32), private             :: tra_top = DFA_NOT_INIT_TRAENSITION_TOP
      integer(int32)                      :: alloc_count_f = ALLOC_COUNT_INITTIAL
      logical                             :: registered = .false.
      logical                             :: initialized = .false.
   contains
      procedure :: get_tra_top       => dfa_state_node__get_transition_top
      procedure :: init_tra_top      => dfa_state_node__initialize_transition_top
      procedure :: increment_tra_top => dfa_state_node__increment_transition_top
      procedure :: add_transition    => dfa_state_node__add_transition
      procedure :: realloc_f         => dfa_state_node__reallocate_transition_forward
      procedure :: is_registered_tra => dfa_state_node__is_registered_transition
      procedure :: free              => dfa_state_node__deallocate
   end type dfa_state_node_t

contains

   !> This function returns the index of top transition in the list dfa_state_node_t has.
   pure function dfa_state_node__get_transition_top (self) result(res)
      implicit none
      class(dfa_state_node_t), intent(in) :: self
      integer :: res

      res = self%tra_top
   end function dfa_state_node__get_transition_top


   !> This subroutine initialize the top index of the transition array of the dfa
   !> node with the value of the given argument.
   pure subroutine dfa_state_node__initialize_transition_top(self, top)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self
      integer, intent(in) :: top

      self%tra_top = top
   end subroutine dfa_state_node__initialize_transition_top


   !> This subroutine deallocates the transition array of a DFA state node.
   pure subroutine dfa_state_node__deallocate(self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self

      if (allocated(self%transition)) deallocate(self%transition)
   end subroutine dfa_state_node__deallocate


   !> This subroutine increments the value of top transition index.
   pure subroutine dfa_state_node__increment_transition_top (self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self

      self%tra_top = self%tra_top + 1
   end subroutine dfa_state_node__increment_transition_top


   !> This subroutine processes to add the given transition to the list which dfa_state_node_t has.
   pure subroutine dfa_state_node__add_transition (self, tra)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self
      type(dfa_transition_t), intent(in) :: tra

      integer :: j

      if (.not. self%initialized .or. .not. allocated(self%transition)) then
         call self%realloc_f()
      end if

      !== At this point, self%transition is definitely already assigned. ==!

      if (self%get_tra_top() == DFA_NOT_INIT_TRAENSITION_TOP) then
         error stop "ERROR: Invalid counting transitions"
      end if

      if (.not. allocated(self%transition)) then
         error stop "ERROR: Transition array not allocated."
      end if

      call self%increment_tra_top()
      j = self%get_tra_top()

      if (j >= size(self%transition, dim=1)) then
         call self%realloc_f()
      end if

      self%transition(j) = tra
   end subroutine dfa_state_node__add_transition


   !> This subroutine copies the data of a specified transition into the
   !> variables of another dfa_transition_t.
   pure subroutine copy_dfa_transition(src, dst)
      implicit none
      type(dfa_transition_t), intent(in) :: src
      type(dfa_transition_t), intent(inout) :: dst

      dst%c = src%c
      dst%dst = src%dst
      dst%nfa_set = src%nfa_set
      dst%own_j = src%own_j
   end subroutine copy_dfa_transition


   !> This subroutine performs allocating initial or additional transition arrays.
   !>
   pure subroutine dfa_state_node__reallocate_transition_forward(self)
      implicit none
      class(dfa_state_node_t), intent(inout) :: self

      type(dfa_transition_t), allocatable :: tmp(:)
      integer :: siz, j
      integer :: new_part_begin, new_part_end

      siz = 0

      !! @note Note that the return value of the `size` intrinsic function for an unallocated array is undefined.

      if (self%initialized .and. allocated(self%transition)) then
         ! If already initialized, copy the transitions to a temporary array `tmp`.
         siz = size(self%transition, dim=1)
         call move_alloc(self%transition, tmp)
      else
         ! If not yet initialized, call init_tra_top procedure.
         siz = 0
         call self%init_tra_top(DFA_INIT_TRANSITION_TOP)
      end if

      self%alloc_count_f = self%alloc_count_f + 1 ! Increment

      new_part_begin = siz + 1
      new_part_end = DFA_TRANSITION_UNIT * 2**self%alloc_count_f

      allocate(self%transition(DFA_TRANSITION_BASE:new_part_end))

      ! Copy registered data
      if(allocated(tmp)) self%transition(DFA_TRANSITION_BASE:siz) = tmp(DFA_TRANSITION_BASE:siz) 

      ! Initialize the new part of the array.
      self%transition(new_part_begin:new_part_end)%own_j = [(j, j=new_part_begin, new_part_end)]
      self%initialized = .true.
   end subroutine dfa_state_node__reallocate_transition_forward


   ! This function scans all transition of the node and returns true if a
   ! transition containing the given symbol is already registered.
   pure function dfa_state_node__is_registered_transition(self, dst, symbol) result(res)
      use :: forgex_segment_m, only: symbol_to_segment, operator(.in.)
      implicit none
      class(dfa_state_node_t), intent(in) :: self
      integer, intent(in) :: dst
      character(*), intent(in) :: symbol

      logical :: res

      integer :: j

      res = .false.
      do j = 1, self%get_tra_top()
         if (self%transition(j)%dst == dst) then
            if (symbol_to_segment(symbol) .in. self%transition(j)%c) then
               res = .true.
               return
            end if
         end if
      end do
   end function dfa_state_node__is_registered_transition

end module forgex_lazy_dfa_node_m