! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_nfa_node_m module is a part of Forgex.
!
!! This file contains `nfa_t` class and its type-bound procedures.

!> The `forgex_nfa_m` module defines the data structure of NFA.
!> The `nfa_t` is defined as a class representing NFA.
#ifdef IMPURE
#define pure
#define elemental
#endif
module forgex_nfa_node_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit, int32
   use :: forgex_parameters_m, only: NFA_NULL_TRANSITION, ALLOC_COUNT_INITTIAL, NFA_TRANSITION_UNIT
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_segment_m, only: segment_t, operator(/=), SEG_EPSILON, operator(==)
   use :: forgex_cube_m, only: cube_t
   implicit none
   private


   type, public :: nfa_transition_t
      type(cube_t) :: c
      integer(int32) :: dst = NFA_NULL_TRANSITION
      integer(int32) :: own_j = NFA_NULL_TRANSITION
      logical        :: is_registered = .false.
   end type nfa_transition_t

   type, public :: nfa_state_node_t
      integer(int32) :: own_i
      type(nfa_transition_t), allocatable :: forward(:)
      integer(int32) :: forward_top = 1
      integer(int32) :: alloc_count_f = ALLOC_COUNT_INITTIAL
   contains
      procedure :: nfa__add_transition, nfa__add_transition_cube
      generic :: add_transition => nfa__add_transition, nfa__add_transition_cube
      procedure :: realloc_forward => nfa__reallocate_transition_forward
      procedure :: merge_segment => nfa__merge_segments_of_transition
   end type nfa_state_node_t


contains

   pure subroutine nfa__add_transition (self, src, dst, seg)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      integer(int32), intent(in) :: src, dst
      type(segment_t), intent(in) :: seg(:)

      integer :: j, k

      j = NFA_NULL_TRANSITION
      if (allocated(self%forward)) then
         do k = 1, self%forward_top
            if (dst == self%forward(k)%dst) then
               j = k
            end if
         end do
      end if
      
      if (j == NFA_NULL_TRANSITION) then
         j = self%forward_top
      end if

      if (.not. allocated(self%forward)) then
         call self%realloc_forward()
      end if

      if (any(seg == SEG_EPSILON)) then
         call self%forward(j)%c%flag_epsilon()
      else
         call self%forward(j)%c%add(seg)
      end if

      self%forward(j)%dst = dst
      self%forward(j)%is_registered = .true.

      if (j == self%forward_top) self%forward_top = self%forward_top + 1

   end subroutine nfa__add_transition


   pure subroutine nfa__add_transition_cube(self, src, dst, cube)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      integer(int32), intent(in) :: src, dst
      type(cube_t), intent(in) :: cube

      integer :: j, k

      j = NFA_NULL_TRANSITION
      if (allocated(self%forward)) then
         do k = 1, self%forward_top
            if (dst==self%forward(k)%dst) then
               j = k
            end if
         end do
      end if

      if (j == NFA_NULL_TRANSITION) then
         j = self%forward_top
      end if

      if (.not. allocated(self%forward)) then
         call self%realloc_forward()
      end if

      call self%forward(j)%c%add(cube)
      self%forward(j)%dst = dst
      self%forward(j)%is_registered = .true.

      if (j==self%forward_top) self%forward_top = self%forward_top + 1

   end subroutine nfa__add_transition_cube


   pure subroutine nfa__reallocate_transition_forward (self)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self

      type(nfa_transition_t), allocatable :: tmp(:)
      integer :: siz, j
      integer :: prev_count, new_part_begin, new_part_end

      siz = 0
      prev_count = 0
      new_part_begin = 0
      new_part_end = 0

      if (allocated(self%forward)) then
         siz = size(self%forward, dim=1)
         call move_alloc(self%forward, tmp)
      else
         siz = 0
      end if

      prev_count = self%alloc_count_f
      self%alloc_count_f = prev_count + 1

      new_part_begin = siz + 1
      new_part_end = NFA_TRANSITION_UNIT * 2**self%alloc_count_f

      allocate(self%forward(1:new_part_end))

      if (allocated(tmp)) then
         if (siz > 0) self%forward(1:siz) = tmp(1:siz)
      end if

      self%forward(1:new_part_end)%own_j = [(j, j=1, new_part_end)]

   end subroutine nfa__reallocate_transition_forward


   pure elemental subroutine nfa__merge_segments_of_transition(self)
      use :: forgex_segment_m, only:seg__merge_segments=>merge_segments, seg__sort_segments=>sort_segment_by_min
      implicit none
      class(nfa_state_node_t), intent(inout) :: self

      integer :: j

      if (allocated(self%forward)) then
         do j = 1, self%forward_top
            if (allocated(self%forward(j)%c%sps)) then
               call seg__sort_segments(self%forward(j)%c%sps)
               call seg__merge_segments(self%forward(j)%c%sps)
            end if
         end do
      end if

   end subroutine nfa__merge_segments_of_transition

end module forgex_nfa_node_m