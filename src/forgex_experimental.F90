! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_experimental module is a part of Forgex.
!
!! This file includes the experimental API module of Forgex.
!! The APIs defined in this file are written with the aim of being integrated into the `forgex` module,
!! but users should be aware that it may be changed, added, or removed without notice.
module forgex_experimental
   use :: forgex, only: regex
   private

   ! The following two procedures must be functions that return variable-length strings.
   ! If the argument with `intent(out)` is a fixed-length string, the length of
   ! the resulting string after substitution may not fit within the length of the argument.
   public :: replace
   public :: all_replace


contains

   !> This subroutine performs 
   function replace(pattern, text, replacement, ierr) result(ret)
      implicit none
      character(*), intent(in) :: pattern, text, replacement
      integer, intent(out), optional :: ierr

      character(:), allocatable :: ret

      integer :: idx_begin, idx_end
      character(:), allocatable :: unused
      integer :: icode

      call regex(pattern, text, unused, from=idx_begin, to=idx_end, status=icode)

      ret = text(1:idx_begin-1) // replacement // text(idx_end+1:)

   end function replace


   function all_replace(pattern, text, replacement, ierr) result(ret)
      use :: forgex_error_m
      use :: forgex_parameters_m
      implicit none
      character(*),intent(in) :: pattern, text, replacement
      integer, intent(out), optional :: ierr

      character(:), allocatable :: ret

      integer :: idx_begin, idx_end
      character(:), allocatable :: unused, buf
      integer :: icode

      idx_begin = INVALID_CHAR_INDEX
      idx_end = INVALID_CHAR_INDEX

      ret = text
      call regex(pattern, text, unused, from=idx_begin, to=idx_end, status=icode)

      ! 以下のコードはループを引き起こす可能性があるので、要修正
      ! 例えば、replacementにpatternが含まれる場合

      ! 修正は、具体的には、マッチしたすべてのインデックスを取得してから結果文字列を構成するアプローチ、
      ! もしくは置換された分だけtextの部分文字列のインデックスを進めるアプローチなど
      do while (idx_begin /= 0 .and. idx_end /= 0)

         if (icode /= SYNTAX_VALID) then
            if (allocated(ret)) deallocate(ret)
            ierr = icode
            return
         end if

         if (idx_end+1 > len(ret)) then
            ret = text(1:idx_begin-1) // replacement
            exit
         else
            ret = text(1:idx_begin-1) // replacement // text(idx_end+1:)
         end if

         call regex(pattern, ret, unused, from=idx_begin, to=idx_end, status=icode)
      end do
      
   end function all_replace


end module forgex_experimental