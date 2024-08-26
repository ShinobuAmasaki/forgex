! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_priority_queue_m module is a part of Forgex.
!
! (C) ue1221, 2021
!
! The original Fortran implementation of priority queue is by ue1221.
! cf. https://github.com/ue1221/fortran-utilities

!! This file defines the `priority_queue_t` derived-type.

!> The `forgex_priority_queue_m` module defines `priority_queue_t`.
!> This implementation was originally provided by ue1221.
module forgex_priority_queue_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_segment_m, only: segment_t
   implicit none
   private

   public :: priority_queue_t

   !> The `priority_queue_t` derived-type has an array containing segment data
   !> and the number of data. The array component is allocatable.
   type priority_queue_t
      integer(int32) :: number = 0
      type(segment_t), allocatable :: heap(:)
   contains
      procedure :: enqueue
      procedure :: dequeue
      procedure :: clear
   end type

contains

   !> The `enqueue` subroutine is responsible for allocating heap structure and
   !> holding the disjoined segment data with ascending priority order.
   pure subroutine enqueue(pq, seg)
      implicit none
      class(priority_queue_t), intent(inout) :: pq
      type(segment_t),         intent(in) :: seg

      type(segment_t)              :: t
      type(segment_t), allocatable :: tmp(:)
      integer(int32)               :: n, i

      if (.not.allocated(pq%heap)) allocate(pq%heap(1))

      !  Managing the size of array in the queue.
      !! @note This implementation shall be rewritten using the `move_alloc` statement.
      n = pq%number
      if (n == size(pq%heap)) then
         allocate(tmp(n))
         tmp(:) = pq%heap(:)
         deallocate(pq%heap)
         allocate(pq%heap(n*2))
         pq%heap(1:n) = tmp(1:n)
      end if

      pq%number = pq%number + 1
      pq%heap(pq%number) = seg

      ! Implementing a queue using arrays.
      ! The following loop ensures that the data structure is a heap:
      n = pq%number
      do while (n > 1)
         i = n/2
         if (pq%heap(n)%min < pq%heap(i)%min &
               .or. (pq%heap(n)%min == pq%heap(i)%min .and. pq%heap(n)%max < pq%heap(i)%max)) then
            t = pq%heap(n)
            pq%heap(n) = pq%heap(i)
            pq%heap(i) = t
         end if
         n = i
      end do
   end subroutine enqueue

   !> The `dequeue` function takes out and returns the prior segment from the queue.
   pure subroutine dequeue(pq, res)
      implicit none
      class(priority_queue_t), intent(inout) :: pq
      type(segment_t),         intent(inout) :: res

      type(segment_t) :: tmp
      integer :: n, i, j

      ! Hold the number of data in a temporary variable.
      n = pq%number

      ! The prior element of the array is returned.
      res = pq%heap(1)

      ! The tailing data is moved to the beginning.
      pq%heap(1) = pq%heap(n)

      ! Reduce the number of data by one.
      pq%number = pq%number - 1

      ! The following loop ensures that the data structure is a heap:
      i = 1
      do while (2*i < n)
         j = 2*i
         if (j+1 < n .and. pq%heap(j+1)%min < pq%heap(j)%min) j = j + 1
         if (pq%heap(j)%min < pq%heap(i)%min) then
            tmp = pq%heap(j)
            pq%heap(j) = pq%heap(i)
            pq%heap(i) = tmp
         end if
         i = j
      end do
   end subroutine dequeue

   !> The `clear` subroutine deallocates the queue.
   pure subroutine clear(pq)
      implicit none
      class(priority_queue_t), intent(inout) :: pq

      if (allocated(pq%heap)) deallocate(pq%heap)
      pq%number = 0
   end subroutine clear


end module forgex_priority_queue_m