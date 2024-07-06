!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     priority_queue_m module is a part of Forgex.
!! 
!! (C) ue1221, 2021
!! 
!! The original Fortran implementation of priority queue is by ue1221.
!! cf. https://github.com/ue1221/fortran-utilities

module forgex_priority_queue_m
   use, intrinsic :: iso_fortran_env
   use :: forgex_segment_m
   implicit none

   type priority_queue_t
      integer(int32) :: number = 0
      type(segment_t), pointer :: heap(:) => null()
   end type

contains
   
   subroutine enqueue(pq, seg)
      implicit none
      type(priority_queue_t), intent(inout) :: pq
      type(segment_t), intent(in) :: seg
      type(segment_t) :: t
      type(segment_t), allocatable :: tmp(:)
      integer(int32) :: n, i

      if (.not. associated(pq%heap)) allocate(pq%heap(1))

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


   function dequeue(pq) result(res)
      implicit none
      type(priority_queue_t), intent(inout) :: pq
      type(segment_t) :: res, tmp 

      integer :: n, i, j

      n = pq%number
      res = pq%heap(1)
      pq%heap(1) = pq%heap(n)
      pq%number = pq%number - 1

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

   end function dequeue

   
   subroutine clear(pq)
      implicit none
      type(priority_queue_t), intent(inout) :: pq
      
      if (associated(pq%heap)) deallocate(pq%heap)
      pq%number = 0
   end subroutine


end module forgex_priority_queue_m