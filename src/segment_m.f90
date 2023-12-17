module segment_m
   use, intrinsic :: iso_fortran_env
   use :: utf8_m
   implicit none
   
   type, public :: segment_t
      integer(int32) :: min = UTF8_CODE_EMPTY ! = 0
      integer(int32) :: max = UTF8_CODE_EMPTY ! = 0
   contains
      procedure :: print => segment_for_print
   end type

   ! See ASCII code set
   type(segment_t), parameter, public :: SEG_EMPTY = segment_t(UTF8_CODE_EMPTY, UTF8_CODE_EMPTY)
   type(segment_t), parameter, public :: SEG_ANY   = segment_t(UTF8_CODE_MIN, UTF8_CODE_MAX)
   type(segment_t), parameter, public :: SEG_TAB   = segment_t(9, 9)     ! Horizontal Tab
   type(segment_t), parameter, public :: SEG_LF    = segment_t(10, 10)   ! Line Feed 
   type(segment_t), parameter, public :: SEG_FF    = segment_t(12, 12)   ! Form Feed
   type(segment_t), parameter, public :: SEG_CR    = segment_t(13, 13)   ! Carriage Return
   type(segment_t), parameter, public :: SEG_SPACE = segment_t(32, 32)
   type(segment_t), parameter, public :: SEG_UNDERSCORE = segment_t(95, 95)
   type(segment_t), parameter, public :: SEG_DIGIT = segment_t(48, 57)   ! 0-9
   type(segment_t), parameter, public :: SEG_UPPERCASE = segment_t(65, 90)   ! A-Z
   type(segment_t), parameter, public :: SEG_LOWERCASE = segment_t(97, 122)  ! a-z
   type(segment_t), parameter, public :: SEG_ZENKAKU_SPACE = segment_t(12288, 12288) ! '　' U+3000 全角スペース

   interface operator(==)
      module procedure :: segment_equivalent
   end interface

   interface operator(/=)
      module procedure :: segment_not_equiv
   end interface


contains

   function segment_equivalent(a, b) result(res)
      implicit none
      type(segment_t), intent(in) :: a, b
      logical :: res

      res = a%max == b%max .and. a%min == b%min
   end function segment_equivalent


   function segment_not_equiv(a, b) result(res)
      implicit none
      type(segment_t), intent(in) :: a, b
      logical :: res

      res = a%max /= b%max .or. a%min /= b%min

   end function segment_not_equiv

   function segment_for_print (seg) result(res)
      implicit none
      class(segment_t), intent(in) :: seg
      character(:), allocatable :: res

      if (seg == SEG_ANY) then
         res = "<ANY>"
      else if (seg == SEG_LF) then
         res = "<LF>"
      else if (seg == SEG_CR) then   
         res = "<CR>"
      else if (seg == SEG_FF) then
         res = "<FF>"
      else if (seg == SEG_TAB) then
         res = "<TAB>"
      else if (seg == SEG_SPACE) then
         res = "<SPACE>"
      else if (seg == SEG_ZENKAKU_SPACE) then
         res = "<ZENKAKU SPACE>"
      else if (seg == SEG_EMPTY) then
         res = "?"

      else if (seg%min == seg%max) then
         res = char_utf8(seg%min)
      else if (seg%max == UTF8_CODE_MAX) then
         res = '["'//char_utf8(seg%min)//'"-'//"<U+1FFFFF>"//']'
      else
         res = '["'//char_utf8(seg%min)//'"-"'//char_utf8(seg%max)//'"]'
      end if

   end function segment_for_print


end module segment_m