module segment_m
   use, intrinsic :: iso_fortran_env
   use :: utf8_m
   implicit none
   
   type, public :: segment_t
      integer(int32) :: min = UTF8_CODE_MIN ! = 0
      integer(int32) :: max = UTF8_CODE_MIN ! = 0
   end type

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



end module segment_m