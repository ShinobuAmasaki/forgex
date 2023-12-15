!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     utf8_m module is a part of Forgex.

module utf8_m
   implicit none

   integer, parameter :: UTF8_CODE_MAX = 2**21-1
   integer, parameter :: UTF8_CODE_MIN = 0

contains

   ! INDEX OF UTF8
   ! This function returns the index of the end of the (multibyte) character,
   ! given the string str and the current index curr.
   pure function idxutf8 (str, curr) result(tail) 
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: str
      integer(int32), intent(in) :: curr
      integer(int32)  :: tail
      integer(int32) :: i
      integer(int8) :: byte, shift_3, shift_4, shift_5, shift_6, shift_7

      tail = curr

      do i = 0, 3

         byte = ichar(str(curr+i:curr+i))

         shift_3 = ishft(byte, -3)
         shift_4 = ishft(byte, -4)
         shift_5 = ishft(byte, -5)
         shift_6 = ishft(byte, -6)
         shift_7 = ishft(byte, -7)

         if (shift_6 == 2) cycle

         if (i == 0) then

            if (shift_3 == 30 ) then ! 11110_2
               tail = curr + 4 - 1
               return
            end if

            if (shift_4 == 14) then ! 1110_2
               tail = curr + 3 - 1
               return 
            end if

            if (shift_5 == 6) then  ! 110_2
               tail = curr + 2 - 1
               return
            end if

            if (shift_7 == 0) then ! 0_2
               tail = curr + 1 - 1
               return
            end if 

         else 
            
            if (shift_3 == 30 .or. shift_4 == 14 .or. shift_5 == 6 .or. shift_7 == 0) then
               tail = curr + i - 1
               return 
            end if 

         end if

      end do

   end function idxutf8

   ! Take a UTF-8 character as an argument and
   ! return the integer representing its Unicode code point. 
   function ichar_utf8 (chara) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: chara
      integer(int32) :: res
      integer(int8) :: byte(4), shift_3, shift_4, shift_5, shift_7
      integer(int8) ::  mask_2_bit, mask_3_bit, mask_4_bit, mask_5_bit
      integer(int32) :: buf

      character(8) :: binary

      binary = '00111111'
      read(binary, '(b8.8)') mask_2_bit

      binary = '00011111'
      read(binary, '(b8.8)') mask_3_bit  ! for 2-byte character 
      
      binary = '00001111'
      read(binary, '(b8.8)') mask_4_bit  ! for 3-byte character 

      binary = '00000111'
      read(binary, '(b8.8)') mask_5_bit

      res = 0

      if (len(chara) > 4)  then
         res = -1
         return
      end if

      byte(1) = ichar(chara(1:1))
      if (len(chara) >= 2) byte(2) = ichar(chara(2:2))
      if (len(chara) >= 3) byte(3) = ichar(chara(3:3))
      if (len(chara) >= 4) byte(4) = ichar(chara(4:4))

      shift_3 = ishft(byte(1), -3)
      shift_4 = ishft(byte(1), -4)
      shift_5 = ishft(byte(1), -5)
      shift_7 = ishft(byte(1), -7)

      ! 1-byte character 
      if (shift_7 == 0) then

         res = byte(1)
         return

      ! 4-byte character
      else if (shift_3 == 30) then

         res = and(byte(1), mask_5_bit)

         res = ishft(res, 6)
         buf = and(byte(2), mask_2_bit)
         res = or(res, buf)

         res = ishft(res, 6)
         buf = and(byte(3), mask_2_bit)
         res = or(res, buf)

         res = ishft(res, 6)
         buf = and(byte(4), mask_2_bit)
         res = or(res, buf)

      ! 3-byte character
      else if (shift_4 == 14) then
         
         res = and(byte(1), mask_4_bit)

         res = ishft(res, 6)
         buf = and(byte(2), mask_2_bit)
         res = or(res, buf)

         res = ishft(res, 6)
         buf = and(byte(3), mask_2_bit)
         res = or(res, buf)

      ! 2-byte character
      else if (shift_5 == 6) then

         res = and(byte(1), mask_3_bit)

         res = ishft(res, 6)
         buf = and(byte(2), mask_2_bit)
         res = or(res, buf)

      end if 

   end function ichar_utf8

   
   pure function is_first_byte_of_character(chara) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(1), intent(in) :: chara
      logical :: res
      integer(int8) :: byte, shift_6

      byte = ichar(chara)

      res = .true. 

      shift_6 = ishft(byte, -6)

      if (shift_6 == 2) res = .false.

   end function is_first_byte_of_character


   subroutine is_first_byte_of_character_array (str, array, length)
      use, intrinsic :: iso_fortran_env
      implicit none
      logical, allocatable, intent(inout) :: array(:)
      integer(int32), intent(in) :: length
      character(len=length), intent(in) :: str
      integer :: i

      if (allocated(array)) deallocate(array)

      allocate(array(length), source=.false.)

      do concurrent (i = 1:length)
         array(i) = is_first_byte_of_character(str(i:i))
      end do

   end subroutine
      

end module utf8_m