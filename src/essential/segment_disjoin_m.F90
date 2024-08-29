! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_segment_disjoin_m module is a part of Forgex.
!
!! This file contains
#ifdef IMPURE
#define pure
#endif
module forgex_segment_disjoin_m
   use :: forgex_segment_m, only: segment_t, SEG_UPPER, SEG_INIT, operator(.in.), operator(/=)
   use :: forgex_priority_queue_m, only: priority_queue_t
   implicit none
   private

   public :: disjoin
   public :: is_prime_semgment
   public :: is_overlap_to_seg_list

   interface disjoin
      !! Interface for the procedure `disjoin_kernel`.
      module procedure :: disjoin_kernel
   end interface

contains

   !> Disjoins overlapping segments and creates a new list of non-overlapping segments.
   !>
   !> This subroutine takes a list of segments, disjoins any overlapping segments,
   !> and creates a new list of non-overlapping segments. It uses a priority queue
   !> to sort the segments and processes them to ensure they are disjoined.
   pure subroutine disjoin_kernel(list)
      use, intrinsic :: iso_fortran_env, only: int32
      implicit none
      type(segment_t), intent(inout), allocatable :: list(:)

      type(segment_t), allocatable :: old_list(:)
      type(priority_queue_t)       :: pqueue
      type(segment_t), allocatable :: buff(:)
      type(segment_t), allocatable :: cache(:)
      type(segment_t)              :: new
      integer(int32), allocatable  :: index_list(:)

      integer(int32) :: i, j, k, count, siz, top, bottom, real_size, m
      logical        :: flag

      ! If list is not allocated, it returns immediately.
      if (.not. allocated(list)) return

      siz = size(list, dim=1)
      if (siz <= 0) then
         return
      end if

      ! Move the currnet list to `old_list`
      call move_alloc(list, old_list)

      ! Sort segments using a priority queue (heap sort)
      block
         allocate(buff(siz))

         do j = 1, siz
            call pqueue%enqueue(old_list(j))
         end do

         do j = 1, siz
            call pqueue%dequeue(buff(j))  ! The `buff` is sorted array.
         end do
      end block

      ! Determine the bottom and top value from the segment array.
      block
         bottom = buff(1)%min
         top = 0
         do j = 1, siz
            top = max(top, buff(j)%max)
         end do
      end block

      allocate(list(siz*2))

      ! Generate a list of unique indices from the `old_list`.
      call index_list_from_segment_list(index_list, old_list)

      !==  At this point, index_list is allocated by index_list_from_segment_list procedure.

      ! Initialize
      new = SEG_UPPER   ! segment_t(2**21, 2**21)
      k = 1
      m = 1

      ! NOTE: this is a complex loop with multiple counters, so HANDLE WITH CARE.
      !
      do while(m <= size(index_list))
         i = index_list(m)    ! Get the current value of `index_list`.

         ! NOTE: the `index_list` is in ASCENDING order.

         ! Check if `i` is within any of the segments.
         !
         ! This loop iterates over each value in the `index_list` and checks if the current
         ! value `i` is present in any of the segments stored in the `buff` array.
         ! If it is present and less than the current minimum value of the new segment, it
         ! updates the new segment's minimum value.
         if (i .in. buff(1:siz)) then
            if (i < new%min) new%min = i
         else
            ! Otherwise, advance the index in `index_list` and move to the next cycle.
            m = m + 1
            cycle
         end if

         ! Check if `i+1` is the start of any segment.
         !
         ! This section checks if the value `i+1` is the starting point (`min`) of any segment
         ! in the `buff` array. If it is, then it sets the new segment's `max` value to `i` and
         ! registers the new segment.
         flag = .false.
         do j = 1, siz
            if (i+1 == buff(j)%min) flag = flag .or. .true.
               ! This `if` statement is redundant and should be fixed.
         end do
         if (flag) then
            new%max = i
            call register_seg_list(new, list, k)
            m = m + 1
            cycle
         end if

         ! Check for multiple segments starting at `i`.
         !
         ! This part counts how many segments start at the current value `i`. If more than
         ! one segment starts at `i`, it sets the new segment's max value to `i` and register
         ! the new segment.
         count = 0
         do j = 1, siz
            if (buff(j)%min == i) count = count + 1
         end do
         if (count > 1) then
            new%max = i
            call register_seg_list(new, list, k)
         end if

         ! Check for any segments ending at `i`.
         !
         ! This part counts how many segments end at the current value `i`.
         ! If any segment ends at `i`, it sets the new segment's max value to `i`
         ! and registers the new segment.
         count = 0
         do j = 1, siz
            if (buff(j)%max == i) count = count + 1
         end do
         if (count >0) then
            new%max = i
            call register_seg_list(new, list, k)
         end if

         m = m + 1
      end do

      ! Determine the real size of the new list.
      ! This loop calculates the actual number of non-empty segments in the new `list`.
      real_size = 0 
      do i = 1, size(list)
         if (list(i) /= SEG_INIT) real_size = real_size + 1
      end do

      ! Move `list` to `cache` and reallocate `list` to the real size.
      call move_alloc(list, cache)  ! list is now deallocated.
      allocate(list(real_size))
      list(:) = cache(1:real_size)

      ! Deallocate used arrays and clear the priority queue
      call pqueue%clear()
      deallocate(buff)
      deallocate(cache)
      deallocate(index_list)
   end subroutine disjoin_kernel


   !> Registers a new segment into a list if it is valid.
   !>
   !> This subroutine adds a new segment to a given list if the segment is valid.
   !> After registering, it sets the new segment to a predefined upper limit segment.
   pure subroutine register_seg_list(new, list, k)
      use, intrinsic :: iso_fortran_env, only: int32
      implicit none
      type(segment_t), intent(inout) :: new, list(:)
      integer(int32),  intent(inout) :: k

      ! If the `new` segment is valid, add it to the list and incremetn the count.
      !! @note This implementation is badly behaved and should be fixed as soon as possible.
      if (new%validate()) then
         list(k) = new
         k = k + 1
      end if
      new = SEG_UPPER
   end subroutine register_seg_list


   !> Checks if a segment is a prime segment within a disjoined list.
   !>
   !> This function determines whether the given segment `seg` is a prime
   !> segment, meaning it does not overlap with any segment in the `disjoined_list`.
   !
   !  この関数は、指定されたセグメント`seg`が、`disjoined_list`内の任意のセグメントと交差せずに
   !  独立しているかどうかを判定する。`disjoined_list`内のいずれかのセグメントについて、`seg`がその範囲内に
   !  完全に収まっているかどうかをチェックし、その結果を論理値`res`に格納して返す。
   pure function is_prime_semgment(seg, disjoined_list) result(res)
      implicit none
      type(segment_t), intent(in) :: seg, disjoined_list(:)

      logical :: res
      integer :: j

      ! Initialize the result.
      res = .false.

      ! リストのうちのいずれかと一致すれば、交差していない。
      ! Check if any segment in `disjoined_list` contains `seg`.
      do j = 1, size(disjoined_list)
         res = res .or. ( disjoined_list(j)%min <= seg%min .and. seg%max <= disjoined_list(j)%max)
      end do
   end function is_prime_semgment


   !> Checks if a segment overlaps with any segments in a list.
   !>
   !> This function determines whether the given segment `seg` overlaps with
   !> any of the segments in the provided `list`. It returns a logical array
   !> indicating the overlap status for each segment in the `list`.
   pure function is_overlap_to_seg_list(seg, list, len) result(res)
      use, intrinsic :: iso_fortran_env, only: int32
      implicit none
      integer(int32),  intent(in) :: len
      type(segment_t), intent(in) :: seg, list(:)
      logical :: res(len)

      integer :: i

      ! Initialize the result array.
      res(:) = .false.

      do i = 1, len
         res(i) = list(i) .in. seg  ! Check if each segment overlaps.
      end do
   end function is_overlap_to_seg_list


   !> Extracts a sorted list of unique indices from a list of segments.
   !>
   !> This subroutine takes a list of segments and generates a sorted list of
   !> unique indices from the `min` and `max` values of each segment, including
   !> values just before and after the `min` and `max`.
   pure subroutine index_list_from_segment_list(index_list, seg_list)
      use, intrinsic :: iso_fortran_env, only: int32
      use :: forgex_sort_m, only: insertion_sort
      implicit none
      type(segment_t), intent(in) :: seg_list(:)
      integer(int32), intent(out), allocatable :: index_list(:)
      integer(int32), allocatable :: cache(:)

      integer :: siz, i, k

      siz = size(seg_list, dim=1)   ! Get the size of the list.

      allocate(index_list(6*siz))   ! Allocate an `index_list` of the required size
      allocate(cache(6*siz))        ! Allocate an array for cache.

      do i = 1, siz
         ! Add the `min` and `max` values of each segment, as well as the values
         ! before and after them, to the index list.
         index_list(6*i-5) = seg_list(i)%min - 1
         index_list(6*i-4) = seg_list(i)%min
         index_list(6*i-3) = seg_list(i)%min + 1
         index_list(6*i-2) = seg_list(i)%max - 1
         index_list(6*i-1) = seg_list(i)%max
         index_list(6*i)   = seg_list(i)%max + 1
      end do

      call insertion_sort(index_list)  ! Sort the `index_list` in ascending order.

      ! Initialize
      cache(1) = index_list(1)
      k = 1
      ! Scan the entire `index_list`.
      do i = 2, siz*6
         if (index_list(i-1) /= index_list(i)) then
            ! Add only unique values to the `cache`.
            ! At the same time, count unique values.
            k = k + 1
            cache(k) = index_list(i)
         end if
      end do


      deallocate(index_list)     ! Deallocate the old `index_list`.
      allocate(index_list(k))    ! Allocate a new `index_list` based on the number of unique indices.
      index_list(:) = cache(1:k) ! Copy the data of `cahce(1:k)` into the `index_list(:)`.
   end subroutine index_list_from_segment_list

end module forgex_segment_disjoin_m