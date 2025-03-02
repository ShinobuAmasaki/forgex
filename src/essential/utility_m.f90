! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_utility_m module is a part of Forgex.
!
module forgex_utility_m
   implicit none
   private

   public :: is_there_caret_at_the_top
   public :: is_there_dollar_at_the_end
   public :: get_index_list_forward
   public :: get_index_comma
   public :: is_integer

contains

   !> This function returns .true. if the pattern contains the caret character
   !> at the top that matches the beginning of a line.
   pure function is_there_caret_at_the_top(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff
      logical :: res

      res = .false.

      buff = adjustl(pattern)
      if (len(buff) == 0) return

      res = buff(1:1) == '^'
   end function is_there_caret_at_the_top


   !> This funciton returns .true. if the pattern contains the doller character
   !> at the end that matches the ending of a line.
   pure function is_there_dollar_at_the_end(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff

      logical :: res

      res = .false.
      
      buff = trim(pattern)
      if (len(buff) == 0) return

      res = buff(len_trim(buff):len_trim(buff)) == '$'
   end function is_there_dollar_at_the_end


   !> This subroutine creates an array containing a list of the positions of the
   !> `prefix`es that exist in the `text`
   pure subroutine get_index_list_forward(text, prefix, suffix, index_array)
      use, intrinsic :: iso_fortran_env, only: int32
      use :: forgex_parameters_m
      implicit none
      character(*), intent(in) :: text, prefix, suffix
      integer(int32), allocatable, intent(inout) :: index_array(:)
      integer(int32), allocatable :: tmp(:)

      integer :: offset, idx, len_pre, len_suf, i, siz, suf_idx

      !! If the length of `prefix` equals to zero, return immediately.
      len_pre = len(prefix)
      len_suf = len(suffix)
      if (len_pre == 0) then
         return
      end if

      ! Intialize
      if (allocated(index_array)) deallocate(index_array)
      allocate(index_array(LIT_OPTS_INDEX_UNIT), source=INVALID_CHAR_INDEX)
      siz = LIT_OPTS_INDEX_UNIT

      ! Get the first position with the `index` intrinsic function.
      idx = index(text, prefix)
      suf_idx = index(text, suffix, back=.true.)
      if (suf_idx == 0) suf_idx = INVALID_CHAR_INDEX

      if (idx <= 0) then
         return
      else if (suf_idx /= INVALID_CHAR_INDEX) then
         if (idx <= suf_idx) index_array(1) = idx
      else
         index_array(1) = idx
      end if

      ! Calculate the offset to specify a substring.
      offset = idx + len_pre -1
      
      i = 2      
      do while (offset < len(text))

         ! Get the position and store it in the `idx` variable. 
         idx = index(text(offset+1:), prefix)
         if (idx <= 0) exit
         index_array(i) = idx + offset
         i = i + 1

         ! Reallocate
         if (i > siz) then
            call move_alloc(index_array, tmp)
            allocate(index_array(2*siz), source=INVALID_CHAR_INDEX)
            index_array(1:siz) = tmp(1:siz)
            siz = siz*2
         end if

         ! Update the offset to specify the next substring.
         offset = offset + idx + len_pre -1
         if (suf_idx /= INVALID_CHAR_INDEX .and. offset > suf_idx) exit
      end do
   end subroutine get_index_list_forward


   !> This procedure find first comma and number of it in the given pattern.
   !> It aims to parse repetation times of the expression.
   pure subroutine get_index_comma (str, i, count)
      use :: iso_fortran_env, only: int32
      use :: forgex_parameters_m, only: comma => SYMBOL_COMMA, EMPTY_CHAR
      implicit none
      character(*), intent(in) :: str
      integer(int32), intent(inout) :: i, count

      integer(int32) :: j
      character(:), allocatable :: buf

      i = 0
      j = 1
      count = 0
      buf = str
      do while (.true.)
         j = index(buf, comma)
         if (i == 0) i = j
         if (j == 0) exit
         buf = buf(1:j-1)//'.'//buf(j+1:len(buf))
         count = count + 1
      end do

   end subroutine get_index_comma

   !> This function determines whether the input character string can be
   !> parsed into an integer.
   pure function is_integer (chara) result(res)
      use :: iso_fortran_env, only: int64
      use :: forgex_parameters_m
      implicit none
      character(*), intent(in) :: chara

      integer :: ios, i, count
      integer(int64) :: val1
      logical :: res

      i = index(chara, SYMBOL_COMMA)
      i = max(i, index(chara, SYMBOL_WS))

      if (i /= 0) then
         res = .false.
         return
      end if

      read(chara, fmt='(1i19)', iostat=ios) val1
      if (ios /= 0) then
         res = .false.
      else
         res = .true.
      end if
   end function is_integer
   

end module forgex_utility_m