module utf8_m
   implicit none
   
contains

   ! INDEX OF UTF8
   ! This function returns the index of the end of the (multibyte) character,
   ! given the string str and the current index curr.
   pure function iutf8 (str, curr) result(tail) 
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: str
      integer(int32), intent(in) :: curr
      integer(int32)  :: tail
      integer(int32) :: i
      integer(int8) :: byte, shift_4, shift_5, shift_6, shift_7

      tail = curr

      do i = 0, 3

         byte = ichar(str(curr+i:curr+i))

         shift_4 = ishft(byte, -4)
         shift_5 = ishft(byte, -5)
         shift_6 = ishft(byte, -6)
         shift_7 = ishft(byte, -7)

         if (shift_6 == 2) cycle

         if (i == 0) then

            if (shift_4 == 14) then
               tail = curr + 3 - 1
               return 
            end if

            if (shift_5 == 6) then
               tail = curr + 2 - 1
               return
            end if

            if (shift_7 == 0) then
               tail = curr + 1 - 1
               return
            end if 

         else 
            
            if (shift_4 == 14 .or. shift_5 == 6 .or. shift_7 == 0) then
               tail = curr + i - 1
               return 
            end if 

         end if

      end do

   end function iutf8

end module utf8_m