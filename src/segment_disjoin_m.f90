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

      subroutine disjoin_kernel(seg_list)
      implicit none
      type(segment_t), intent(inout), allocatable :: seg_list(:)
      type(priority_queue_t) :: pqueue
      type(segment_t), allocatable :: new_list(:)
      type(segment_t) :: new, SEG_UPPER = segment_t(UTF8_CODE_MAX+1, UTF8_CODE_MAX+1)

      integer(int32) :: i, j, k, count, siz, top, bottom, real_size
      logical :: flag

      siz = size(seg_list, dim=1)

      do j = 1, siz
         call enqueue(pqueue, seg_list(j))
      end do

      do j = 1, siz
         seg_list(j) = dequeue(pqueue)
      end do

      bottom = seg_list(1)%min
      top = 0
      do j = 1, siz
         top = max(top, seg_list(j)%max)
      end do

      allocate(new_list(top-bottom+1))

      k = 1
      new = SEG_UPPER
      do i = bottom, top

         ! i が範囲に含まれる場合
         if (i .in. seg_list(1:siz)) then 
            if (i < new%min) new%min =i
         else
            cycle
         end if

         ! i+1がいずれかのセグメントの始端の場合
         flag = .false.
         do j = 1, siz
            if (i+1 == seg_list(j)%min) flag = flag .or. .true.
         end do
         if (flag) then
            new%max = i
            call register_seg_list(new, new_list, k)
            cycle
         end if

         count = 0
         do j = 1, siz
            if (seg_list(j)%min == i) count = count + 1
         end do
         if (count > 1) then
            new%max = i
            call register_seg_list(new, new_list, k)
         end if 

         count = 0
         do j = 1, siz
            if (seg_list(j)%max == i) count = count + 1
         end do
         if (count >0) then
            new%max = i
            call register_seg_list(new, new_list, k)
         end if 

      end do

      real_size = 0
      do i = 1, size(new_list)
         if (new_list(i) /= SEG_EMPTY) real_size = real_size + 1
      end do

      deallocate(seg_list)
      allocate(seg_list(real_size))

      seg_list = new_list(1:siz)


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
         res = res .or. seg == disjoined_list(j)
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


end module segment_disjoin_m