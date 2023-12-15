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
   integer, parameter :: UTF8_CODE_MIN = 33 ! = 0x21: '!'
   integer, parameter :: UTF8_CODE_EMPTY = 0

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


   function char_utf8 (code) result(str)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: code
      character(:), allocatable :: str

      character(:), allocatable :: bin
      integer(int32) :: buf, mask
      integer(int8) :: byte(4)
      
      str = ''
      buf = code

      bin = '0000000000000000000000000111111' ! lower 6-bit mask
      read(bin, '(b32.32)') mask

      byte(1) = and(ishft(buf, -18), mask)
      
      buf = code
      byte(2) = and(ishft(buf, -12), mask)
      
      buf = code
      byte(3) = and(ishft(buf, -6), mask)
      
      buf = code
      byte(4) = and(buf, mask)

      if (code > 2**7-1) then
        
         if (2**16 -1 < code) then
            ! the first byte of 4-byte character
            byte(1) = ibset(byte(1),7)
            byte(1) = ibset(byte(1),6)
            byte(1) = ibset(byte(1),5)
            byte(1) = ibset(byte(1),4)
            byte(1) = ibclr(byte(1),3)
            byte(2) = set_continuation_byte(byte(2))
            byte(3) = set_continuation_byte(byte(3))
            byte(4) = set_continuation_byte(byte(4))
            
         ! the first byte of 3-byte character
         else if (2**11 - 1 < code) then
            byte(1) = 0 
            byte(2) = ibset(byte(2), 7)
            byte(2) = ibset(byte(2), 6)
            byte(2) = ibset(byte(2), 5)
            byte(2) = ibclr(byte(2), 4)
            byte(3) = set_continuation_byte(byte(3))
            byte(4) = set_continuation_byte(byte(4))

         ! the first byte of 2-byte character
         else if (2**7 -1 < code) then
            byte(1) = 0
            byte(2) = 0
            byte(3) = ibset(byte(3), 7)
            byte(3) = ibset(byte(3), 6)
            byte(3) = ibclr(byte(3), 5)
            byte(4) = set_continuation_byte(byte(4))
         end if

         str = char(byte(1))//char(byte(2))//char(byte(3))//char(byte(4))
         str = trim(adjustl(str))
      
      else
         str = char(code)
      end if

            
   contains
      
      function set_continuation_byte(byte) result(res)
         implicit none
         integer(int8), intent(in) :: byte
         integer(int8) :: res

         res = ibset(byte, 7)
         res = ibclr(res, 6)

      end function set_continuation_byte      

   end function char_utf8

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

   function len_trim_utf8(str) result(count)
      implicit none
      character(*), intent(in) :: str
      integer :: i, inext, count

      i = 1
      count = 0
      do while(i <= len_trim(str))
         inext = idxutf8(str, i) + 1
         count = count + 1
         i = inext
      end do
      
   end function len_trim_utf8

   
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
      

   function count_token(str, token) result(count)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: str
      character(1), intent(in) :: token
      integer :: count, i, siz

      count = 0
      siz = len(str)
      do i = 1, siz
         if (str(i:i) == token) count = count + 1
      end do 

   end function count_token



end module utf8_m