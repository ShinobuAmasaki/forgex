! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_segment_disjoin_m module is a part of Forgex.
!
!! This file contains procedures to disjoin overlapping segments.

!> The `forgex_segment_disjoin_m` module support to disjoin and split overlapping segments.
!> Without these procedures, we cannot building a valid DFA from NFA.
module forgex_segment_disjoin_m
   use :: forgex_segment_m
   use :: forgex_priority_queue_m
   private

   public :: disjoin
   public :: is_prime_semgment
   public :: is_overlap_to_seg_list

   type(segment_t), parameter :: SEG_UPPER = segment_t(UTF8_CODE_MAX+1, UTF8_CODE_MAX+1)

   interface disjoin
      module procedure :: disjoin_kernel
   end interface 

contains


   subroutine disjoin_kernel(new_list)
      implicit none
      type(segment_t), intent(inout), allocatable :: new_list(:)
      type(segment_t), allocatable :: old_list(:)
    
      type(priority_queue_t)       :: pqueue
      type(segment_t), allocatable :: buff(:)
      type(segment_t), allocatable :: cache(:)
      type(segment_t) :: new

      integer(int32) :: i, j, k, count, siz, top, bottom, real_size, m
      integer(int32), allocatable :: index_list(:)
      logical :: flag

      ! 最初にリストの長さを確認して0以下ならばreturnする。
      siz = size(new_list, dim=1)
      if (siz <= 0) then
         return
      end if

      ! 引数のリストをold_listへ割り付けを変更する。
      call move_alloc(new_list, old_list)

      ! 優先度付きキューにエンキューして、バッファを割り付けし、デキューしてソートされた結果バッファに格納をする。
      block ! heap sort with the `min` values order
         do j = 1, siz
            call enqueue(pqueue, old_list(j))
         end do

         allocate(buff(siz))
      
         do j = 1, siz
            buff(j) = dequeue(pqueue)
         end do
      end block

      ! 最低値をbottomに格納し、バッファを走査して最大値をtopに格納する。
      block ! get the bottom and top from the segment array.
         bottom = buff(1)%min
         top = 0
         do j = 1, siz
            top = max(top, buff(j)%max)
         end do
      end block

      ! new_listを改めて2倍のサイズで割り付ける
      allocate(new_list(siz*2))
      ! allocate(cache(siz*2))

      ! old_listを入力にindex_listを生成する。
      call index_list_from_segment_list(index_list, old_list)

      ! segment_t型の変数newに定数SEG_UPPERを代入する。
      new = SEG_UPPER

      ! インデックス変数に1を代入して初期化する
      k = 1
      m = 1
      do while(m <= size(index_list))
         i = index_list(m)

         ! i が範囲に含まれる場合
         if (i .in. buff(1:siz)) then 
            if (i < new%min) new%min =i
         else
            m = m + 1
            cycle
         end if

         ! i+1がいずれかのセグメントの始端の場合
         flag = .false.
         do j = 1, siz
            if (i+1 == buff(j)%min) flag = flag .or. .true.
         end do
         if (flag) then
            new%max = i
            call register_seg_list(new, new_list, k)
            m = m + 1
            cycle
         end if

         count = 0
         do j = 1, siz
            if (buff(j)%min == i) count = count + 1
         end do
         if (count > 1) then
            new%max = i
            call register_seg_list(new, new_list, k)
         end if 

         count = 0
         do j = 1, siz
            if (buff(j)%max == i) count = count + 1
         end do
         if (count >0) then
            new%max = i
            call register_seg_list(new, new_list, k)
         end if 

         m = m + 1
      end do

      real_size = 0
      do i = 1, size(new_list)
         if (new_list(i) /= SEG_EMPTY) real_size = real_size + 1
      end do

      call move_alloc(new_list, cache)  ! new_list is now deallocated.

      allocate(new_list(real_size))

      new_list(:) = cache(1:real_size)

      ! deallocate
      call clear(pqueue)
      deallocate(buff)
      deallocate(cache)
      deallocate(index_list)
   end subroutine disjoin_kernel


   subroutine register_seg_list(new, list, k)
      implicit none
      type(segment_t), intent(inout) :: new, list(:)
      integer(int32), intent(inout) :: k

      if (new%validate()) then
         list(k) = new
         k = k + 1
      end if
      new = SEG_UPPER
   end subroutine register_seg_list
         

   function is_prime_semgment(seg, disjoined_list) result(res)
      implicit none 
      type(segment_t), intent(in) :: seg, disjoined_list(:)
      logical :: res
      integer :: j

      ! リストのうちのいずれかと一致すれば、交差していない。
      res = .false. 
      do j = 1, size(disjoined_list)
         res = res .or. ( disjoined_list(j)%min <= seg%min .and. seg%max <= disjoined_list(j)%max)
      end do
   end function is_prime_semgment


   function is_overlap_to_seg_list(seg, list, len) result(res)
      implicit none
      integer(int32), intent(in) :: len
      type(segment_t), intent(in) :: seg, list(:)
      logical :: res(len)

      integer :: i
      
      res(:) = .false.

      do i = 1, len
         res(i) = list(i) .in. seg
      end do
   end function is_overlap_to_seg_list


   !| Extracts a sorted list of unique indices from a list of segments.
   !
   !  This subroutine takes a list of segments and generates a sorted list of
   !  unique indices from the `min` and `max` values of each segment, including
   !  values just before and after the `min` and `max`
   subroutine index_list_from_segment_list(index_list, seg_list)
      use :: forgex_sort_m, only: bubble_sort
      implicit none
      type(segment_t), intent(in) :: seg_list(:)
      integer(int32), intent(out), allocatable :: index_list(:)
      integer(int32), allocatable :: cache(:)

      integer :: siz, i, k

      siz = size(seg_list, dim=1)   ! Get the size of the input list.

      allocate(index_list(6*siz))   ! Allocate an index list of the required size.
      allocate(cache(6*siz))        ! Allocate an array for the cache.

      ! Scan the list of segments.
      do i = 1, siz
         ! Add the `min` and `max` values of the segment, as well as the values
         ! before and after them, to the `index_list`.
         index_list(6*i-5) = seg_list(i)%min - 1
         index_list(6*i-4) = seg_list(i)%min
         index_list(6*i-3) = seg_list(i)%min + 1
         index_list(6*i-2) = seg_list(i)%max - 1
         index_list(6*i-1) = seg_list(i)%max
         index_list(6*i)   = seg_list(i)%max + 1
      end do

      call bubble_sort(index_list) ! Sort the `index_list`.
      
      ! From the sorted index_list, remove duplicates and extract unique values,
      ! then add them to the `cache` array.
      ! At the same time, count the number of elements in the `cache` array.
      cache(1) = index_list(1)
      k = 1    ! Initialize the counter `k`.
      do i = 2, siz*6
         if (index_list(i-1) /= index_list(i)) then
            k = k + 1
            cache(k) = index_list(i)
         end if
      end do 

      deallocate(index_list)     ! Deallocate the old `index_list`.

      allocate(index_list(k))    ! Allocate a new index list based on the number of unique indices.
      index_list(:) = cache(1:k) ! Copy from the `cache(1:k)` into the new `index_list`. 
   end subroutine index_list_from_segment_list

end module forgex_segment_disjoin_m