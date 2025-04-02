! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_cube_m module is a part of Forgex.
!
#ifdef IMPURE
#define pure
#endif
module forgex_cube_m
   use, intrinsic :: iso_fortran_env, only: int64, int32
   use :: forgex_parameters_m, only: BMP_SIZE, BMP_SIZE_BIT, bits_64, INVALID_CODE_POINT, UTF8_CODE_MAX, ASCII_SIZE, ASCII_SIZE_BIT
   use :: forgex_bitmap_m, only: bmp_t, ascii_t
   use :: forgex_segment_m, only: segment_t, symbol_to_segment, &
      operator(.in.), SEG_INIT, SEG_EPSILON, operator(==), width_of_segment, invert_segment_list
   use :: forgex_utf8_m, only: ichar_utf8
   implicit none
   private


   type, public :: cube_t ! contains all planes (ascii, BMP and SPs) of Unicode.
      logical :: epsilon_flag = .false.
      logical :: single_flag = .true.
      logical :: is_switched_to_bmp = .false.
      type(ascii_t) :: ascii                 ! for  U+0000 .. U+007F ASCII
      type(bmp_t), allocatable :: bmp        ! for  U+0000 .. U+FFFF BMP
      type(segment_t), allocatable :: sps(:) ! for U+10000 .. U+10FFFF SPs (SIP, SMP, etc.)
   contains
      procedure :: flag_epsilon => cube_flag__epsilon
      procedure :: is_flagged_epsilon => cube_flag__is_flagged_epsilon
      procedure :: cube_add__symbol
      procedure :: cube_add__segment
      procedure :: cube_add__segment_list
      procedure :: cube_add__cube
      procedure :: switch_bmp => cube__switch_ascii_to_bmp
      procedure :: cube2seg => cube__bmp2seg
      procedure :: print_sps => cube__dump_sps
      procedure :: invert => cube__invert
      procedure :: num => cube__number_of_flagged_bits
      procedure :: first => cube__first_codepoint
      generic :: add => cube_add__symbol, cube_add__segment, cube_add__segment_list, cube_add__cube
   end type cube_t

   interface operator(.in.)
      module procedure :: cube_t__symbol_in_cube
   end interface

   interface assignment(=)
      module procedure :: cube_t__cube_assign
   end interface

   interface operator(==)
      module procedure :: cube_t__equal
   end interface

   public :: operator(.in.)
   public :: assignment(=)
   public :: operator(==)

   integer :: q
   type(bmp_t), parameter, public :: white_bmp = bmp_t([(0_int64, q=0, BMP_SIZE-1)])

contains

   pure subroutine cube_t__cube_assign(a, b)
      implicit none
      type(cube_t), intent(inout) :: a
      type(cube_t), intent(in) :: b

      integer :: num

      a%is_switched_to_bmp = b%is_switched_to_bmp
      a%epsilon_flag = b%epsilon_flag
      if (b%is_switched_to_bmp) then
         call a%switch_bmp()
         a%bmp%b(:) = b%bmp%b(:)
      else
         a%ascii%a(:) = b%ascii%a(:)
      end if

      if (.not. allocated(b%sps)) return
      num = ubound(b%sps, dim=1)
      a%sps = b%sps ! implicit reallocation

      a%single_flag = a%num() == 1

   end subroutine cube_t__cube_assign


   pure function cube_t__symbol_in_cube (symbol, cube) result(ret)
      implicit none
      character(*), intent(in) :: symbol
      type(cube_t), intent(in) :: cube
      logical :: ret

      integer :: cp

      cp = ichar_utf8(symbol)

      if (cp < ASCII_SIZE_BIT .and. .not. cube%is_switched_to_bmp) then
         ret = iand(cube%ascii%a(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64

      else if (cp < BMP_SIZE_BIT .and. cube%is_switched_to_bmp) then
         ret = iand(cube%bmp%b(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64

      else if (cp >= ASCII_SIZE_BIT .and. cp < BMP_SIZE_BIT .and. .not. cube%is_switched_to_bmp) then
            ret = .false.
      else
         if (allocated(cube%sps)) then
            ret = symbol_to_segment(symbol) .in. cube%sps(:)
         else
            ret = .false.
         end if
      end if

   end function cube_t__symbol_in_cube


   pure function cube_t__equal(a, b) result(ret)
      use :: forgex_segment_m, only: segment_t, merge_segments, sort_segment_by_min
      implicit none
      type(cube_t), intent(in) :: a, b

      type(segment_t), allocatable :: a_sps(:), b_sps(:)
      logical :: ret, candi

      integer :: i

      ret = .false.

      if (a%is_switched_to_bmp .neqv. b%is_switched_to_bmp) return
      if (.not. all(a%ascii%a(:) == b%ascii%a(:))) return

      if (allocated(a%bmp) .and. allocated(b%bmp)) then
         if (any(a%bmp%b(:) /= b%bmp%b(:))) return
      end if


      if (allocated(a%sps) .and. allocated(b%sps)) then
         a_sps = a%sps
         b_sps = b%sps
         call sort_segment_by_min(a_sps)
         call merge_segments(a_sps)
         call sort_segment_by_min(b_sps)
         call merge_segments(b_sps)

         if (size(a_sps, dim=1) == size(b_sps, dim=1)) then
            candi = .true. 
            do i = 1, size(a_sps, dim=1)
               candi = candi .and. a_sps(i) == b_sps(i)
            end do
            ret = candi
            return

         end if
      end if

      ret = .true.

   end function cube_t__equal


   pure function cube_t__codepoint_in_cube (cp, cube) result(ret)
      implicit none
      integer(int32), intent(in) :: cp
      type(cube_t), intent(in) :: cube
      logical :: ret

      if (cp < ASCII_SIZE_BIT .and. .not. cube%is_switched_to_bmp) then
         ret = iand(cube%ascii%a(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64
      else if (cp < BMP_SIZE_BIT) then
         ret = iand(cube%bmp%b(cp/bits_64), ishft(1_int64, mod(cp, bits_64))) /= 0_int64
      else
         ret = cp .in. cube%sps(:)
      end if

   end function cube_t__codepoint_in_cube

!=====================================================================!

   pure subroutine cube_flag__epsilon(self)
      implicit none
      class(cube_t), intent(inout) :: self
      self%epsilon_flag = .true.
   end subroutine cube_flag__epsilon

   pure logical function cube_flag__is_flagged_epsilon(self)
      implicit none
      class(cube_t), intent(in) :: self
      cube_flag__is_flagged_epsilon = self%epsilon_flag
   end function cube_flag__is_flagged_epsilon

!=====================================================================!

   pure subroutine cube_add__symbol(self, symbol)
      implicit none
      class(cube_t), intent(inout) :: self
      character(*), intent(in) :: symbol

      integer :: cp
      cp = ichar_utf8(symbol)
      if (cp == -1) return ! WARNING: magic nubmer
      if (cp < ASCII_SIZE_BIT .and. .not. self%is_switched_to_bmp) then
         call self%ascii%add(cp)

      else if (cp < BMP_SIZE_BIT) then
         call self%switch_bmp()
         call self%bmp%add(cp)

      else
         call self%switch_bmp()
         call cube_add__segment(self, segment_t(cp, cp))
      end if

      if (self%single_flag) self%single_flag = self%num() == 1

   end subroutine cube_add__symbol

   pure subroutine cube_add__segment(self, segment)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: segment

      integer :: cp_min, cp_max, sps_size, i, j
      type(segment_t), allocatable :: tmp(:)
      type(segment_t) :: what_to_add

      if (segment == SEG_EPSILON) then
         call self%flag_epsilon()
         return
      end if

      cp_min = segment%min
      cp_max = segment%max

      if (cp_max < ASCII_SIZE_BIT .and. .not. self%is_switched_to_bmp) then
         call self%ascii%add(cp_min, cp_max)
         if (self%single_flag) self%single_flag = self%num() == 1
         return
      else
         call self%switch_bmp()
         call self%bmp%add(cp_min, cp_max)
      end if

      if (cp_max > BMP_SIZE_BIT) then

         what_to_add = segment_t(max(cp_min, BMP_SIZE_BIT), cp_max)

         if (allocated(self%sps)) then
            sps_size = size(self%sps, dim=1) + 1
            allocate(tmp(sps_size))
            j = 0
            do i = 1, size(self%sps)
               j = j + 1
               if (self%sps(i)%min < what_to_add%min) then
                  tmp(j) = self%sps(i)
               else
                  tmp(j) = what_to_add
               end if
            end do
            self%sps = tmp(1:sps_size) ! implicit reallocation

         else
            self%sps = [segment]
         end if

      end if

      if (self%single_flag) self%single_flag = self%num() == 1

   end subroutine cube_add__segment


   pure subroutine cube_add__segment_list(self, seglist)
      implicit none
      class(cube_t), intent(inout) :: self
      type(segment_t), intent(in) :: seglist(:)

      integer :: cp_min, cp_max, siz, i, j, k, m, n, p

      type(segment_t), allocatable :: tmp(:), ret(:)
      type(segment_t) :: what_to_add

      integer :: upper

      if (allocated(self%sps)) then
         m = size(self%sps)
      else
         m = 0
      end if
      n = size(seglist, dim=1)

      if (any(seglist == SEG_EPSILON)) then
         self%epsilon_flag = .true.
      end if

      ! Scan all segments in the list to find max value of them.
      upper = 0
      do i = 1, n
         upper = max(seglist(i)%max, upper)
      end do

      ! If all values of the list is within the range of ASCII, register them to self%ascii and retrun.
      if (upper < ASCII_SIZE_BIT .and. .not. self%is_switched_to_bmp) then
         do i = 1, n
            cp_min = seglist(i)%min
            cp_max = seglist(i)%max
            call self%ascii%add(cp_min, cp_max)
         end do
         if (self%single_flag) self%single_flag = self%num() == 1
         return
      end if

      if (.not. self%is_switched_to_bmp) call self%switch_bmp()

      siz = m + n
      allocate(tmp(n))
      allocate(ret(siz+1))

      k = 0 ! for tmp
      j = 1 ! for segments to add
      do while ( j <= n)
         cp_min = seglist(j)%min
         cp_max = seglist(j)%max
         call self%bmp%add(cp_min, cp_max)
         if (cp_max > BMP_SIZE_BIT) then
            k = k + 1
            what_to_add = segment_t(max(cp_min, BMP_SIZE_BIT), cp_max)
            tmp(k) = what_to_add
         end if

         j = j + 1
      end do

      if (k /= 0) then
         joint: block
            type(segment_t), allocatable :: cache(:)
            if (allocated(self%sps)) then
               p = ubound(self%sps, dim=1)
               cache(1:p) = self%sps(1:p)
               deallocate(self%sps)

               allocate(self%sps(1:p+k))
               self%sps(1:p) = cache(1:p)
               self%sps(p+1:p+k) = tmp(1:k)
            else
               allocate(self%sps(1:k))
               self%sps(1:k) = tmp(1:k)
            end if

         end block joint
      end if

      if (self%single_flag) self%single_flag = self%num() == 1
   end subroutine cube_add__segment_list


   pure subroutine cube_add__cube(self, cube)
      implicit none
      class(cube_t), intent(inout) :: self
      type(cube_t), intent(in) :: cube

      integer :: i

      if (.not. self%is_switched_to_bmp .and. .not. cube%is_switched_to_bmp ) then
         do concurrent (i=0:ASCII_SIZE-1)
            self%ascii%a(i) = ior(self%ascii%a(i), cube%ascii%a(i))
         end do
         return
      else
         call self%switch_bmp()
      end if

      do concurrent (i = 0:BMP_SIZE-1)
         self%bmp%b(i) = ior(self%bmp%b(i), cube%bmp%b(i))
      end do

      if (allocated(cube%sps)) then
         call cube_add__segment_list(self, cube%sps)
      end if

      if (self%single_flag) self%single_flag = self%num() == 1
   end subroutine cube_add__cube


   pure subroutine cube__switch_ascii_to_bmp(self)
      implicit none
      class(cube_t), intent(inout) :: self

      self%is_switched_to_bmp = .true.
      if (.not. allocated(self%bmp)) then
         allocate(self%bmp)
         self%bmp%b(0:1) = self%ascii%a(0:1)
      end if

   end subroutine cube__switch_ascii_to_bmp

!=====================================================================!

   pure subroutine cube__invert(self)
      implicit none
      class(cube_t), intent(inout) :: self
      integer :: i
      integer(int64) :: mask

      if (.not. self%is_switched_to_bmp) then
         call self%switch_bmp()
      end if

      mask = not(shiftl(1_int64, 32) -1)
      self%bmp%b(0) = iand(not(self%bmp%b(0)), mask)

      do concurrent (i = 1:BMP_SIZE-1)
         self%bmp%b(i) = not(self%bmp%b(i))
      end do

      if (.not. allocated(self%sps)) then
         self%sps = [segment_t(BMP_SIZE_BIT, UTF8_CODE_MAX)]
      else
         call invert_segment_list(self%sps)
      end if

      self%single_flag = self%num() == 1
   end subroutine cube__invert


   pure function cube__number_of_flagged_bits(self) result(ret)
      implicit none
      class(cube_t), intent(in) :: self
      integer :: ret

      integer :: i
      integer :: partial_sum(0:BMP_SIZE-1)

      if (.not. self%is_switched_to_bmp) then
         ret = popcnt(self%ascii%a(0)) + popcnt(self%ascii%a(1))
         return
      end if

      ret = 0
      do concurrent (i = 0:BMP_SIZE-1)
         partial_sum(i) = popcnt(self%bmp%b(i))
      end do

      ret = sum(partial_sum)

      if (allocated(self%sps)) then
         do i = 1, size(self%sps)
            ret = ret + width_of_segment(self%sps(i))
         end do
      end if

   end function cube__number_of_flagged_bits


   pure function cube__first_codepoint(self) result(ret)
      use :: forgex_parameters_m, only: UTF8_CODE_MAX
      implicit none
      class(cube_t), intent(in) :: self

      integer :: i, num, pos, ret, candi

      if (.not. self%is_switched_to_bmp) then
         do i = 0, ASCII_SIZE-1
            if (self%ascii%a(i) /= 0) then
               pos = trailz(self%ascii%a(i))
               ret = i*bits_64+pos
               return
            end if
         end do
         ret = INVALID_CODE_POINT
         return
      end if

      do i = 0, BMP_SIZE-1
         if (self%bmp%b(i) /= 0) then
            pos = trailz(self%bmp%b(i))
            ret = i*bits_64 + pos
            return
         end if
      end do

      ret = INVALID_CODE_POINT
      if (.not. allocated(self%sps)) return

      candi = UTF8_CODE_MAX
      do i = 1, size(self%sps)
         candi = min(candi, self%sps(i)%min)
      end do

      if (candi /= UTF8_CODE_MAX) then
         ret = candi
      else
         ret = -1
      end if

   end function cube__first_codepoint


!=====================================================================!

   pure subroutine cube__bmp2seg(self, segments)
      implicit none
      class(cube_t), intent(in) :: self
      type(segment_t), allocatable, intent(inout) :: segments(:)

      type(segment_t), allocatable :: tmp(:)

      integer :: m, n

      if (allocated(segments)) deallocate(segments)

      if (.not. self%is_switched_to_bmp) then
         call self%ascii%ascii2seg(segments)
         return
      end if

      if (allocated(self%bmp)) then
         call self%bmp%bmp2seg(tmp)
         m = size(tmp, dim=1)
      else
         m = 0
      end if

      if (allocated(self%sps)) then
         n = size(self%sps, dim=1)
      else
         n = 0
      end if

      if (m+n > 0) then
         allocate(segments(m+n))
         segments(1:m) = tmp(1:m)
         if (n > 0) segments(m+1:m+n) = self%sps(1:n)
      end if

   end subroutine cube__bmp2seg


   subroutine cube__dump_sps(self)
      class(cube_t), intent(in) :: self

      integer :: i
      if (.not. allocated(self%sps)) return

      do i = 1, ubound(self%sps, dim=1)
         write(0,*) self%sps(i)%print()
      end do

   end subroutine cube__dump_sps

end module forgex_cube_m
