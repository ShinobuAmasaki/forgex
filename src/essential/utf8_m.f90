! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_utf8_m module is a part of Forgex.

!! This file contains procedures to handle UTF-8 character set.

!> The `forgex_utf8_m` module processes a byte-indexed character strings type as UTF-8 strings.
module forgex_utf8_m
   use :: iso_fortran_env, only: int8
   use :: iso_c_binding
   implicit none
   private

   public :: idxutf8
   public :: next_idxutf8  ! for processing pattern
   public :: next_idxutf8_strict  ! for processing text (Cannot be assumed to be UTF-8 character)
   public :: char_utf8, ichar_utf8
   public :: make_replacement_char
   public :: count_token
   public :: is_first_byte_of_character
   public :: is_first_byte_of_character_array
   public :: len_trim_utf8, len_utf8
   public :: is_valid_multiple_byte_character
   public :: adjustl_multi_byte
   public :: trim_invalid_utf8_byte
   public :: reverse_utf8

   integer(int8), parameter, public :: fullbit = -1                ! 11111111
   integer(int8), parameter, public :: ascii_mask= 127             ! 01111111
   integer(int8), parameter, public :: lead_2_mask = -33           ! 11011111
   integer(int8), parameter, public :: lead_3_mask = -17           ! 11101111
   integer(int8), parameter, public :: lead_4_mask= -9             ! 11110111
   integer(int8), parameter, public :: continuation_mask = -65     ! 10111111

contains

   ! INDEX OF UTF8
   !> This function returns the index of the end of the (multibyte) character,
   !> given the string str and the current index curr.
   pure function idxutf8 (str, curr) result(tail)
      use, intrinsic :: iso_fortran_env
      use :: forgex_parameters_m
      implicit none
      character(*),   intent(in) :: str      ! Input string, a multibyte character is expected.
      integer(int32), intent(in) :: curr     ! Current index.
      integer(int32)             :: tail     ! Resulting index of the end of the character.
      integer(int32)             :: i        ! Loop variable.
      integer(int8)              :: byte     ! Variable to hold the byte value of the 1-byte part of the character
      integer(int8) :: shift_3, shift_4, shift_5, shift_6, shift_7
         ! Shifted byte values.


      ! If the index exceeds the length of str, return the invalid value.
      if (curr > len(str)) then
         tail = INVALID_CHAR_INDEX
         return
      end if

      tail = curr    ! Initialize tail to the current index.

      !! Class of invalid UTF-8 characters
      !! 1. invalid lead byte
      !! 2. invalid trail byte
      !! 3. overrun
      !! 4. over long encoding
      !! 5. incomplete multibyte sequence
      !! 6. invalid character range (U+D800-U+DFFF)
      !! 7. BOM appears in the middle
      !! 8. isolated trail byte
      !
      !! In the above case, `idxutf8` will returns `curr`.
      !! Then, you should call `is_valid_multiple_byte_character` at a higher level to validate the substring.

      outer: do i = 0, 3    ! Loop over the next four bytes to determine the byte-length of the character.

         ! for terminated incomplete multibyte character
         if (curr+i > len(str)) then
            tail = curr
            return
         end if

         byte = int(ichar(str(curr+i:curr+i)), kind(byte))
            ! Get the byte value of the character at position `curr+1`.

         shift_3 = ishft(byte, -3)  ! Right shift the byte by 3 bits
         shift_4 = ishft(byte, -4)  ! Right shift the byte by 3 bits
         shift_5 = ishft(byte, -5)  ! Right shift the byte by 5 bits
         shift_6 = ishft(byte, -6)  ! Right shift the byte by 6 bits
         shift_7 = ishft(byte, -7)  ! Right shift the byte by 7 bits

         if (shift_6 == 2) cycle    ! Continue to the next iteration if the `byte` is a continuation byte (10xxxxxx_2).

         if (i == 0) then   ! Check the first byte to determine the character length.

            if (shift_3 == 30 ) then ! If the byte starts with 11110_2 (4-byte character).
               tail = curr + 4 - 1
               exit outer
            end if

            if (shift_4 == 14) then ! If the byte starts witth 1110_2 (3-byte character).
               tail = curr + 3 - 1
               exit outer
            end if

            if (shift_5 == 6) then  ! If the byte starts with 110_2 (2-byte character).
               tail = curr + 2 - 1
               exit outer
            end if

            if (shift_7 == 0) then ! If then byte starts with 0_2 (1-byte character).
               tail = curr + 1 - 1
               exit outer
            end if

         else     ! Check continuation byptes

            if (shift_3 == 30 .or. shift_4 == 14 .or. shift_5 == 6 .or. shift_7 == 0) then
               tail = curr + i - 1
               exit outer
            end if

         end if
      end do outer

      if (tail <= len(str)) then
         if (.not. is_valid_multiple_byte_character(str(curr:tail))) then
            tail = curr
         else
            return
         end if
      else
         tail = curr
      end if


   end function idxutf8


   !> This function returns the index of the next character,
   !> given the string str and the current index curr.
   !> If the current index is for the last character, it returns the invalid value.
   pure function next_idxutf8(str, curr) result(res)
      use :: forgex_parameters_m
      implicit none
      character(*), intent(in) :: str
      integer, intent(in) :: curr
      
      integer :: res
      integer :: curr_end

      curr_end = idxutf8(str, curr)

      if (curr_end /= INVALID_CHAR_INDEX) then
         res = curr_end + 1
      else
         res = INVALID_CHAR_INDEX
      end if
      
   end function next_idxutf8


   !> This subroutine returns the index of the next UTF-8 character conteined in `str`.
   !> This is used to handle strings that may not be encoded by UTF-8.
   pure subroutine next_idxutf8_strict(str, curr, next, is_valid)
      use :: forgex_parameters_m
      implicit none
      character(*), intent(in)    :: str
      integer,      intent(in)    :: curr
      integer,      intent(inout) :: next
      logical,      intent(inout) :: is_valid

      integer :: ib, ie

      ! initialize
      is_valid = .false.
      ib = curr
      ie = idxutf8(str, ib)

      if (ie /= INVALID_CHAR_INDEX) then
         is_valid = is_valid_multiple_byte_character(str(ib:ie))
         next = ie + 1
      else
         next = curr+1
         is_valid = .false.
      end if

   end subroutine next_idxutf8_strict
      

   !> This function checks the input byte string is valid as a single UTF-8 character.
   pure function is_valid_multiple_byte_character(chara) result(res)
      use, intrinsic :: iso_fortran_env, only: int32, int8
      implicit none
      character(*), intent(in) :: chara
      logical :: res

      integer :: siz, i, expected_siz
      integer(int8) :: shift_3, shift_4, shift_5, shift_6, shift_7
      integer(int8) :: byte
      
      res = .true.
      siz = len(chara)

      byte = ichar(chara(1:1), kind=int8)
      shift_3 = ishft(byte, -3)  ! Right shift the byte by 3 bits
      shift_4 = ishft(byte, -4)  ! Right shift the byte by 4 bits
      shift_5 = ishft(byte, -5)  ! Right shift the byte by 5 bits
      shift_6 = ishft(byte, -6)  ! Right shift the byte by 6 bits
      shift_7 = ishft(byte, -7)  ! Right shift the byte by 7 bits

      ! 1st byte
      if (shift_3 == 31) then  ! 5-byte character (invalid) 11111xxx_2
         res = .false.
         return
      else if (shift_3 == 30) then  ! 4-byte character `11110xxx_2`
         expected_siz = 4
      else if (shift_4 == 14)then   ! 3 byte character `1110xxxx_2`
         expected_siz = 3 
      else if (shift_5 == 6) then   ! 2-byte character `110xxxxx_2`
         expected_siz = 2
      else if (shift_7 == 0) then   ! for 1-byte character `0xxxxxxx`
         expected_siz = 1         
      else
         res = .false.
         return
      end if

      if (expected_siz /= siz) then
         res = .false.
         return
      end if

      do i = 2, expected_siz
         byte = ichar(chara(i:i), kind=int8)
         shift_6 = ishft(byte, -6)  ! Right shift the byte by 6 bits such as `10xxxxxx_2`
         if (shift_6 /= 2) then
            res = .false.
            return
         end if
      end do

   end function is_valid_multiple_byte_character
      

   !> The `char_utf8` function takes a code point as integer in Unicode character set,
   !> and returns the corresponding character as UTF-8 binary string.
   !>
   !> This function is like an extension of char() for the UTF-8 codeset.
   pure function char_utf8 (code) result(str)
      use, intrinsic :: iso_fortran_env
      implicit none
      integer(int32), intent(in) :: code        ! Input Unicode code point.

      character(:), allocatable  :: str         ! Resulting one UTF-8 character.
      character(32), allocatable :: bin         ! A 32-digit number expressed in character format for masking.
      integer(int32)             :: buf, mask   ! Buffer and mask for bit operations.
      integer(int8)              :: byte(4)     ! Array to hold up 4 bytes of the UTF-8 character.

      str = ''    ! Initialize result string.
      buf = code  ! Initialize buffer with input `code` point.

      bin = '0000000000000000000000000111111' ! Lower 6-bit mask
      read(bin, '(b32.32)') mask              ! Read the `mask` from the `bin` character string.

      byte(1) = int(iand(ishft(buf, -18), mask),kind(byte))    ! First byte

      buf = code
      byte(2) = int(iand(ishft(buf, -12), mask), kind(byte))   ! Second byte

      buf = code
      byte(3) = int(iand(ishft(buf, -6), mask), kind(byte))    ! Third byte

      buf = code
      byte(4) = int(iand(buf, mask), kind(byte))               ! Fourth byte

      if (code > 2**7-1) then    ! Check if the `code` point is greater than 127 (non-ASCII character).

         if (2**16 -1 < code) then     ! 4-byte character
            byte(1) = ibset(byte(1),7)
            byte(1) = ibset(byte(1),6)
            byte(1) = ibset(byte(1),5)
            byte(1) = ibset(byte(1),4)
            byte(1) = ibclr(byte(1),3)
            byte(2) = set_continuation_byte(byte(2))  ! Set continuation bytes.
            byte(3) = set_continuation_byte(byte(3))
            byte(4) = set_continuation_byte(byte(4))

         else if (2**11 - 1 < code) then  ! 3-byte character
            byte(1) = 32
            byte(2) = ibset(byte(2), 7)
            byte(2) = ibset(byte(2), 6)
            byte(2) = ibset(byte(2), 5)
            byte(2) = ibclr(byte(2), 4)
            byte(3) = set_continuation_byte(byte(3))
            byte(4) = set_continuation_byte(byte(4))

         else if (2**7 -1 < code) then    ! 2-byte character
            byte(1) = 32
            byte(2) = 32
            byte(3) = ibset(byte(3), 7)
            byte(3) = ibset(byte(3), 6)
            byte(3) = ibclr(byte(3), 5)
            byte(4) = set_continuation_byte(byte(4))
         end if

         str = char(byte(1)) //char(byte(2)) //char(byte(3)) //char(byte(4))  ! Concatenate bytes into a string.
         str = trim(adjustl(str))   ! Trim leading and tailing space.

      else
         str = char(code)  ! For ASCII characters.
      end if

   end function char_utf8


   !> This function take one byte, set the first two bits to 10, and
   !> returns one byte of the continuation part.
   pure function set_continuation_byte(byte) result(res)
      use, intrinsic :: iso_fortran_env, only: int8
      implicit none
      integer(int8), intent(in) :: byte
      integer(int8) :: res

      res = ibset(byte, 7) ! 1xxxxxxx
      res = ibclr(res, 6)  ! 10xxxxxx
   end function set_continuation_byte


   !> Take a UTF-8 character as an argument and
   !> return the integer (also known as "code point" in Unicode) representing
   !> its UTF-8 binary string.
   !>
   !> This function is like an extension of char() for the UTF-8 codeset.
   pure function ichar_utf8 (chara) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(*), intent(in) :: chara   ! Input one UTF-8 character

      integer(int32) :: res         ! Resulting integer representing an UTF-8 binary string.
      integer(int8)  :: byte(4)     ! Byte array (32bit)
      integer(int8)  :: shift_3, shift_4, shift_5, shift_7              ! Shift values
      integer(int8)  :: mask_2_bit, mask_3_bit, mask_4_bit, mask_5_bit  ! Masks for bit operations
      integer(int32) :: buf         ! Buffer for bit operations

      character(8) :: binary        ! 8-byte character string representing binary.

      binary = '00111111'           ! 6-bit mask for continuation bytes.
      read(binary, '(b8.8)') mask_2_bit

      binary = '00011111'           ! 5-bit mask for 2-byte characters.
      read(binary, '(b8.8)') mask_3_bit

      binary = '00001111'           ! 4-bit mask for 3-byte characters.
      read(binary, '(b8.8)') mask_4_bit

      binary = '00000111'           ! 3-bit mask for 4-byte characters.
      read(binary, '(b8.8)') mask_5_bit

      res = 0     ! Initialize result

      if (len(chara) > 4)  then     ! Check if the length of input character is more than 4 bytes.
         res = -1                   ! Invalid UTF-8 character.
         return
      end if

      ! Convert a multi-byte character to thier integer byte representation.
      byte(1) = int(ichar(chara(1:1)),kind(byte))
      if (len(chara) >= 2) byte(2) = int(ichar(chara(2:2)), kind(byte))
      if (len(chara) >= 3) byte(3) = int(ichar(chara(3:3)), kind(byte))
      if (len(chara) >= 4) byte(4) = int(ichar(chara(4:4)), kind(byte))

      ! Perform bit shifts to determine character's byte-length.
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

         ! First 1 byte
         res =  iand(byte(1), mask_5_bit)

         ! Continuation bytes
         res = ishft(res, 6)     ! Left shift by 6 bits and store into res
         buf =  iand(byte(2), mask_2_bit) ! Mask `byte(2)` with `mask_2_bit` and store the result into `buf`.
         res =   ior(res, buf)   ! Take the bitwise OR of `res` and `buf`. The same applies below.

         res = ishft(res, 6)
         buf =  iand(byte(3), mask_2_bit)
         res =   ior(res, buf)

         res = ishft(res, 6)
         buf =  iand(byte(4), mask_2_bit)
         res =   ior(res, buf)

      ! 3-byte character
      else if (shift_4 == 14) then

         res =  iand(byte(1), mask_4_bit)

         res = ishft(res, 6)
         buf =  iand(byte(2), mask_2_bit)
         res =   ior(res, buf)

         res = ishft(res, 6)
         buf =  iand(byte(3), mask_2_bit)
         res =   ior(res, buf)

      ! 2-byte character
      else if (shift_5 == 6) then

         res =  iand(byte(1), mask_3_bit)

         res = ishft(res, 6)
         buf =  iand(byte(2), mask_2_bit)
         res =   ior(res, buf)

      end if
   end function ichar_utf8


   pure function make_replacement_char() result(replace)
      implicit none
      character(3) :: replace

      replace = char_utf8(65535)  ! U+FFFF
   end function make_replacement_char

   !> This function calculates the length of a UTF-8 string excluding tailing spaces.
   !>
   !> It takes a UTF-8 string as input and returns the number of characters in the string,
   !> ignoring any tailing whitespace characters.
   pure function len_trim_utf8(str) result(count)
      implicit none
      character(*), intent(in) :: str
      integer :: i, inext, count

      ! Initialize
      i = 1
      count = 0

      ! Loop through the string until the end of the trimed string is reached.
      do while(i <= len_trim(str))
         inext = idxutf8(str, i) + 1   ! Get the index of the next UTF-8 character.
         count = count + 1             ! Increment the character count.
         i = inext                     ! Move to the next character.
      end do

   end function len_trim_utf8


   !> This function calculates the length of a UTF-8 string.
   !>
   !> It takes a UTF-8 string as input and returns the number of characters in the string.
   pure function len_utf8(str) result(count)
      implicit none
      character(*), intent(in) :: str
      integer :: i, inext, count

      ! Initialize
      i = 1
      count = 0

      ! Loop through the string until the end of the string is reached.
      do while(i <= len(str))
         inext = idxutf8(str, i) + 1   ! Get the index of the next UTF-8 character.
         count = count + 1             ! Increment the character count.
         i = inext                     ! Move to the next character.
      end do
   end function len_utf8


   !> This function determines if a given character is the first byte of
   !> a UTF-8 multibyte character. It takes a 1-byte character as input
   !> and returns a logical value indicating if it is the first byte of
   !> an UTF-8 binary string.
   pure function is_first_byte_of_character(chara) result(res)
      use, intrinsic :: iso_fortran_env
      implicit none
      character(1), intent(in) :: chara   ! Input single byte character

      logical       :: res             ! Result indicating if it is the first byte of a multibyte character.
      integer(int8) :: byte, shift_6   ! Integer representation of the character and shifted value.

      ! Convert the character to its integer representation
      byte = int(ichar(chara), kind(byte))

      ! Initialize the result to `.true.` (assume it is the first byte).
      res = .true.

      ! Shift the byte 6 bits to the right.
      shift_6 = ishft(byte, -6)

      ! If the shifted value equals 2 (10_2), it is a continuation byte, not the first byte.
      if (shift_6 == 2) res = .false.

   end function is_first_byte_of_character


   !> This subroutine determines if each character in a given string is the first byte of a UTF-8 multibyte character.
   !> It takes a UTF-8 string and return a logical array indicating for each position if it is the first byte.
   pure subroutine is_first_byte_of_character_array (str, array, length)
      use, intrinsic :: iso_fortran_env, only: int32
      implicit none
      logical, allocatable,  intent(inout) :: array(:)   ! Output logical array indicating first byte status.
      integer(int32),        intent(in)    :: length     ! Length of the input string
      character(len=length), intent(in)    :: str        ! Input UTF-8 string

      integer :: i      ! Loop index variable

      ! Deallocate the array if it is already allocated.
      if (allocated(array)) deallocate(array)

      ! Allocate the array with the same length as the input string and initialize to `.false.`
      allocate(array(length), source=.false.)

      ! Loop through each character in the string concurrently.
      ! do concurrent (i = 1:length)
      do i = 1, length
         ! Call the `is_first_byte_of_character` function for each character and store the result in the `array`.
         array(i) = is_first_byte_of_character(str(i:i))
      end do

   end subroutine

   !> This function counts the occurrence of a spcified character(token) in a given string.
   pure function count_token(str, token) result(count)
      implicit none
      character(*), intent(in) :: str     ! Input string to be searched.
      character(1), intent(in) :: token   ! Character to be counted in the input string.

      integer :: count     ! Result: number of occurrences of the `token`.
      integer :: i         ! Loop index variable.
      integer :: siz       ! Length of the input string.

      ! Initialize the count to zero.
      count = 0

      ! Get the length of the input string.
      siz = len(str)

      ! Loop through each character in the string.
      do i = 1, siz
         ! If the current character matches the `token`, increment the `count`.
         if (str(i:i) == token) count = count + 1
      end do
   end function count_token


   pure function adjustl_multi_byte(chara) result(res)
      implicit none
      character(*), intent(in) :: chara
      character(:), allocatable :: res

      integer :: i
      res = ''
      i = 1
      do while (i <= len(chara))
         if (chara(i:i) == char(0)) then
            i = i + 1
            cycle
         else
            exit
         end if
      end do

      res = chara(i:len(chara))

   end function adjustl_multi_byte

   pure function trim_invalid_utf8_byte(chara) result(res)
      implicit none
      character(*), intent(in) :: chara
      character(:), allocatable :: res
      
      if (is_valid_multiple_byte_character(chara)) then
         res = chara
      else
         res = ''
      end if
   
   end function trim_invalid_utf8_byte


   pure function reverse_utf8(str) result(retval)
      use :: forgex_parameters_m
      implicit none
      character(*), intent(in) :: str
      character(:), allocatable :: retval

      integer :: i, ie

      ! Initialize
      ie = 1
      i = 1
      retval = ''
      do while (i /= INVALID_CHAR_INDEX)
         ie = idxutf8(str, i)
         retval = str(i:ie)//retval
         i = next_idxutf8(str, i)
      end do 
   end function reverse_utf8


end module forgex_utf8_m