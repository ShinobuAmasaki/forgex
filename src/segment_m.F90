! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_segment_m module is a part of Forgex.
!
!! This file defines `segment_t` representing subset of UTF-8 character codeset
!! and contains procedures for that.
module forgex_segment_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only: UTF8_CODE_MIN, UTF8_CODE_MAX, UTF8_CODE_EMPTY
   implicit none
   private

   public :: operator(==)
   public :: operator(/=)
   public :: operator(.in.)
   public :: invert_segment_list
   public :: which_segment_symbol_belong
   public :: symbol_to_segment

   public :: sort_segment_by_min
   public :: merge_segments

   !> This derived-type represents a contiguous range of the Unicode character set
   !> as a `min` and `max` value, providing an effective way to represent ranges of characters
   !> when building automata where a range characters share the same transition destination.
   type, public :: segment_t
      integer(int32) :: min = UTF8_CODE_MAX+2 ! = 2097153
      integer(int32) :: max = UTF8_CODE_MAX+2 ! = 2097153
   contains
      procedure :: print => segment_for_print
      procedure :: validate => segment_is_valid
   end type

   ! See ASCII code set
   type(segment_t), parameter, public :: SEG_INIT  = segment_t(UTF8_CODE_MAX+2, UTF8_CODE_MAX+2)
   type(segment_t), parameter, public :: SEG_EPSILON = segment_t(-1, -1)
   type(segment_t), parameter, public :: SEG_EMPTY = segment_t(UTF8_CODE_EMPTY, UTF8_CODE_EMPTY)
   type(segment_t), parameter, public :: SEG_ANY   = segment_t(UTF8_CODE_MIN, UTF8_CODE_MAX)
   type(segment_t), parameter, public :: SEG_TAB   = segment_t(9, 9)     ! Horizontal Tab
   type(segment_t), parameter, public :: SEG_LF    = segment_t(10, 10)   ! Line Feed
   type(segment_t), parameter, public :: SEG_FF    = segment_t(12, 12)   ! Form Feed
   type(segment_t), parameter, public :: SEG_CR    = segment_t(13, 13)   ! Carriage Return
   type(segment_t), parameter, public :: SEG_SPACE = segment_t(32, 32)   ! White space
   type(segment_t), parameter, public :: SEG_UNDERSCORE = segment_t(95, 95)
   type(segment_t), parameter, public :: SEG_DIGIT = segment_t(48, 57)   ! 0-9
   type(segment_t), parameter, public :: SEG_UPPERCASE = segment_t(65, 90)   ! A-Z
   type(segment_t), parameter, public :: SEG_LOWERCASE = segment_t(97, 122)  ! a-z
   type(segment_t), parameter, public :: SEG_ZENKAKU_SPACE = segment_t(12288, 12288) ! '　' U+3000 全角スペース
   type(segment_t), parameter, public :: SEG_UPPER = segment_t(UTF8_CODE_MAX+1, UTF8_CODE_MAX+1)

   interface operator(==)
      !! This interface block provides a equal operator for comparing segments.
      module procedure :: segment_equivalent
   end interface

   interface operator(/=)
      !! This interface block provides a not equal operator for comparing segments.
      module procedure :: segment_not_equiv
   end interface

   interface operator(.in.)
      !! This interface block provides the `.in.` operator, which checks whether
      !! an integer and a segment, an integer and a list of segments, or a segment
      !! and a segment, is contained in the latter, respectively.
      module procedure :: arg_in_segment
      module procedure :: arg_in_segment_list
      module procedure :: seg_in_segment
      module procedure :: seg_in_segment_list
      !! @note Note that this is unrelated to the `.in.` operator provided by `forgex` module,
      !! which is intended to be used only by backend modules that implement Forgex (i.e. only
      !! if the `use forgex_segment_m` statement is declared in some module).
   end interface

   !! @note Support for handling many Unicode whitespace characters is currently not
   !! available, but will be added in the future.

   !! @note We would like to add a procedure to merge adjacent segments with the same transition
   !! destination into a single segment.

contains

   !| Checks if the given integer is within the specified segment.
   !
   !  This function determines whether the integer `a` falls within the
   !  range defined by the `min` and `max` values of the `segment_t` type.
   pure elemental function arg_in_segment(a, seg) result(res)
      implicit none
      integer(int32),  intent(in) :: a
      type(segment_t), intent(in) :: seg
      logical :: res

      res = seg%min <= a .and. a <= seg%max
   end function arg_in_segment

   !| Check if the ginve integer is within any of specified segments in a list.
   !
   !  This function determins whether the integer `a` falls within any of the
   !  ranges defined by the `min` and `max` value of the `segment_t` type
   !  in the provided list of segments.
   pure function arg_in_segment_list(a, seg_list) result(res)
      implicit none
      integer(int32),  intent(in) :: a
      type(segment_t), intent(in) :: seg_list(:)
      logical :: res
      integer :: i

      ! Initialize
      res = .false.

      ! Scan the list of segments
      do i = 1, ubound(seg_list, dim=1)
         res = res .or. (seg_list(i)%min <= a .and. a <= seg_list(i)%max)
      end do
   end function arg_in_segment_list


   !| Check if the one segment is completely within another segment.
   !
   !  This function determines whether the segment `a` is entirely within the
   !  range specified by the segment `b`.
   pure elemental function seg_in_segment(a, b) result(res)
      implicit none
      type(segment_t), intent(in) :: a, b
      logical :: res

      res =  b%min <= a%min .and. a%max <= b%max
   end function seg_in_segment


   pure function seg_in_segment_list(seg, list) result(res)
      implicit none
      type(segment_t), intent(in) :: seg
      type(segment_t), intent(in) :: list(:)
      logical :: res

      res = any(seg_in_segment(seg, list(:)))

   end function seg_in_segment_list


   !| Check if the one segment is exactly equal to another segment.
   !
   !  This function determines wheter the segment `a` is equivalent to the
   !  segment `b`, meaning both their `min` and `max` values are identical.
   pure elemental function segment_equivalent(a, b) result(res)
      implicit none
      type(segment_t), intent(in) :: a, b
      logical :: res

      res = a%max == b%max .and. a%min == b%min
   end function segment_equivalent


   !| Check if two segments are not equivalent.
   !
   !  This function determines whether the segment `a` is not equivalent to the
   !  segment `b`, meaning their `min` or `max` values are different.
   pure elemental function segment_not_equiv(a, b) result(res)
      implicit none
      type(segment_t), intent(in) :: a, b
      logical :: res

      res = a%max /= b%max .or. a%min /= b%min
   end function segment_not_equiv


   !| Checks if a segment is valid.
   !
   !  This function determines whether the segment is valid by ensuring that
   !  the `min` value is less than or equal to the `max` value.
   pure elemental function segment_is_valid(self) result(res)
      implicit none
      class(segment_t), intent(in) :: self
      logical :: res

      res = self%min <= self%max
   end function segment_is_valid


   !> This subroutine inverts a list of segment ranges representing Unicode characters.
   !> It compute the complement of the given ranges and modifies the list accordingly.
   !>
   pure subroutine invert_segment_list(list)
      implicit none
      type(segment_t), intent(inout), allocatable :: list(:)
      type(segment_t), allocatable :: new_list(:)

      integer :: i, n, count
      integer :: current_min

      if (.not. allocated(list)) return

      ! sort and merge segments
      call sort_segment_by_min(list)
      call merge_segments(list)

      ! Count the number of new segments
      count = 0
      current_min = UTF8_CODE_EMPTY+1
      n = size(list, dim=1)

      do i = 1, n
         if (current_min < list(i)%min) then
            count = count + 1
         end if
         current_min = list(i)%max + 1
      end do

      if (current_min <= UTF8_CODE_MAX) then
         count = count + 1
      end if

      ! Allocate new list
      allocate(new_list(count))

      ! Fill the new list with the component segments
      count = 1
      current_min = UTF8_CODE_MIN

      do i = 1, n
         if (current_min < list(i)%min) then
            new_list(count)%min = current_min
            new_list(count)%max = list(i)%min - 1
            count = count + 1
         end if
         current_min = list(i)%max + 1
      end do

      if (current_min <= UTF8_CODE_MAX) then
         new_list(count)%min = current_min
         new_list(count)%max = UTF8_CODE_MAX
      end if

      ! Deallocate old list and reassign new list
      deallocate(list)
      list = new_list
   end subroutine invert_segment_list

   !> This function takes an array of segments and a character as arguments,
   !> and returns the segment as rank=1 array to which symbol belongs
   !> (included in the segment interval).
   pure function which_segment_symbol_belong (segments, symbol) result(res)
      use :: forgex_utf8_m
      implicit none
      type(segment_t), intent(in) :: segments(:)
      character(*),    intent(in) :: symbol
      type(segment_t)             :: res

      integer         :: i, i_end, j
      type(segment_t) :: target_for_comparison

      ! If `symbol` is a empty character, return SEG_EMPTY
      if (symbol == '') then
         res = SEG_EMPTY
         return
      end if

      ! Initialize indices.
      i = 1
      i_end = idxutf8(symbol, i)

      ! The target to check for inclusion.
      target_for_comparison = symbol_to_segment(symbol(i:i_end))

      ! Scan the segments array.
      do j = 1, size(segments)
         ! Compare segments and return the later element of the segments, which contains the target segment.
         if (target_for_comparison .in. segments(j)) then
            res = segments(j)
            return
         end if
      end do

      ! If not found, returns SEG_EMPTY.
      res = SEG_EMPTY
   end function which_segment_symbol_belong


   !> This function convert an input symbol into the segment corresponding it.
   pure function symbol_to_segment(symbol) result(res)
      use :: forgex_utf8_m
      implicit none
      character(*), intent(in) :: symbol
      type(segment_t)          :: res

      integer(int32) :: i, i_end, code

      ! If `symbol` is a empty character, return SEG_EMPTY
      if (symbol == char(0)) then
         res = SEG_EMPTY
         return
      else if (symbol == char(32)) then
         res = SEG_SPACE
         return
      end if

      ! Initialize indices
      i = 1
      i_end = idxutf8(symbol, i)

      ! Get the code point of the input character.
      code = ichar_utf8(symbol(i:i_end))

      ! Create a segment corresponding to the code, and return it.
      res = segment_t(code, code)
   end function symbol_to_segment

!====================================================================-!
!  Helper procedures
   pure subroutine sort_segment_by_min(segments)
      implicit none
      type(segment_t), allocatable, intent(inout) :: segments(:)

      integer :: i, j, n
      type(segment_t) :: temp ! temporary variable

      if (.not. allocated(segments)) return

      n = size(segments)
      do i = 1, n-1
         do j = i+1, n
            if (segments(i)%min > segments(j)%min) then
               temp = segments(i)
               segments(i) = segments(j)
               segments(j) = temp
            end if
         end do
      end do
   end subroutine sort_segment_by_min


   pure subroutine merge_segments(segments)
      implicit none
      type(segment_t), allocatable, intent(inout) :: segments(:)
      integer :: i, j, n, m

      if (.not. allocated(segments)) return
      n = size(segments)

      m = 1
      do i = 2, n
         if (segments(i) == SEG_INIT) exit
         m = m+1
      end do
      n = m

      if (n <= 1) then
         segments = segments(:n)
         return
      end if

      j = 1
      do i = 2, n
         if (segments(j)%max >= segments(i)%min-1) then
            segments(j)%max = max(segments(j)%max, segments(i)%max)
         else
            j = j + 1
            segments(j) = segments(i)
         endif
      end do

      if (j <= n) then
         segments = segments(:j)    ! reallocation implicitly.
      end if

   end subroutine merge_segments


   !| Converts a segment to a printable string representation.
   !
   !  This function generates a string representation of the segment `seg` for
   !  printing purposes. It converts special segments to predefined strings
   !  like `<ANY>`, `<LF>`, etc., or generates a character range representation
   !  for segments with defined `min` and `max` values.
   function segment_for_print (seg) result(res)
      use :: forgex_utf8_m
      implicit none
      class(segment_t), intent(in) :: seg
      character(:), allocatable :: res
      character(:), allocatable :: cache

      if (seg == SEG_ANY) then
         res = "<ANY>"

      else if (seg == SEG_TAB) then
         res = "<TAB>"
      else if (seg == segment_t(9, 10)) then
         res = "<TAB, LF>"
      else if (seg == segment_t(9, 11)) then
         res = "<TAB, LF, VT>"   
      else if (seg == segment_t(9, 12)) then
         res = "<TAB, LF, VT, FF>"   
      else if (seg == segment_t(9, 13)) then
         res = "<TAB, LF, VT, FF, CR>"

      else if (seg == SEG_LF) then
         res = "<LF>"
      else if (seg == segment_t(10, 11)) then
         res = "<LF, VT>"   
      else if (seg == segment_t(10, 12)) then
         res = "<LF, VT, FF>"   
      else if (seg == segment_t(10, 13)) then
         res = "<LF, VT, FF, CR>"  

      else if (seg == segment_t(11, 11)) then
         res = "<VT>"   
      else if (seg == segment_t(11, 12)) then
         res = "<VT, FF>"   
      else if (seg == segment_t(11, 13)) then
         res = "<VT, FF, CR>"

      else if (seg == SEG_FF) then
         res = "<FF>"
      else if (seg == segment_t(12, 13)) then
         res = "<FF, CR>"

      else if (seg == SEG_CR) then
         res = "<CR>"
      else if (seg == SEG_SPACE) then
         res = "<SPACE>"
      else if (seg == SEG_ZENKAKU_SPACE) then
         res = "<ZENKAKU SPACE>"
      else if (seg == SEG_EPSILON) then
         res = "?"
      else if (seg == SEG_INIT) then
         res = "<INIT>"
      else if (seg == SEG_EMPTY) then
         res = "<EMPTY>"
      else if (seg%min == seg%max) then
         res = char_utf8(seg%min)
      else if (seg%max == UTF8_CODE_MAX) then
         if (seg%min == ichar(' ')) then
            cache = "<SPACE>"
         else
            cache = '"'//char_utf8(seg%min)//'"'
         end if
         res = '['//cache//'-'//"<U+1FFFFF>"//']'
      else
         if (seg%min == ichar(' ')) then
            cache = "<SPACE>"
         else
            cache = '"'//char_utf8(seg%min)//'"'
         end if
         res = '['//cache//'-"'//char_utf8(seg%max)//'"]'
      end if

      !!
      !! @note This function contains magic strings, so in the near future we would like
      !! to extract it to `forgex_parameter_m` module and remove the magic strings.

   end function segment_for_print

end module forgex_segment_m