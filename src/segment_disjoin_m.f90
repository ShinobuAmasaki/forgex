module segment_disjoin_m
   use :: segment_m
   use :: priority_queue_m
   private

   public :: disjoin
   public :: is_prime_semgment
   public :: is_overlap_to_seg_list

   interface disjoin
      module procedure :: disjoin_kernel
   end interface 

contains


   subroutine disjoin_kernel(new_list)

      implicit none
      type(segment_t), intent(inout), allocatable :: new_list(:)
      type(segment_t), allocatable :: old_list(:)
    
      type(priority_queue_t) :: pqueue
      type(segment_t), allocatable :: buff(:)
      type(segment_t), allocatable :: cache(:)
      type(segment_t) :: new
      type(segment_t), parameter :: SEG_UPPER = segment_t(UTF8_CODE_MAX+1, UTF8_CODE_MAX+1)

      integer(int32) :: i, j, k, count, siz, top, bottom, real_size, m
      integer(int32), allocatable :: index_list(:)
      logical :: flag

      siz = size(new_list, dim=1)

      call move_alloc(new_list, old_list)

      block ! heap sort
         do j = 1, siz
            call enqueue(pqueue, old_list(j))
         end do

         allocate(buff(siz))
      
         do j = 1, siz
            buff(j) = dequeue(pqueue)
         end do
      end block

      block ! get the bottom and top from the segment array.
         bottom = buff(1)%min
         top = 0
         do j = 1, siz
            top = max(top, buff(j)%max)
         end do
      end block

      allocate(new_list(siz*2))
      ! allocate(cache(siz*2))

      call index_list_from_segment_list(index_list, old_list)

      new = SEG_UPPER

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

   contains

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
         

   end subroutine disjoin_kernel


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


   subroutine index_list_from_segment_list(index_list, seg_list)
      use :: sort_m, only: bubble_sort
      implicit none
      type(segment_t), intent(in) :: seg_list(:)
      integer(int32), intent(out), allocatable :: index_list(:)
      integer(int32), allocatable :: cache(:)

      integer :: siz, i, k

      siz = size(seg_list, dim=1)

      allocate(index_list(6*siz))
      allocate(cache(6*siz))

      do i = 1, siz
         index_list(6*i-5) = seg_list(i)%min - 1
         index_list(6*i-4) = seg_list(i)%min
         index_list(6*i-3) = seg_list(i)%min + 1
         index_list(6*i-2) = seg_list(i)%max - 1
         index_list(6*i-1) = seg_list(i)%max
         index_list(6*i)   = seg_list(i)%max + 1
      end do

      call bubble_sort(index_list)
      
      cache(1) = index_list(1)
      k = 1
      do i = 2, siz*6
         if (index_list(i-1) /= index_list(i)) then
            k = k + 1
            cache(k) = index_list(i)
         end if
      end do 

      deallocate(index_list)
      allocate(index_list(k))
      index_list(:) = cache(1:k)

   end subroutine index_list_from_segment_list

end module segment_disjoin_m