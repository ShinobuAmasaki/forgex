! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_test_m module is a part of Forgex.
!
!! This file contains helper procedures for testing the engine.

!> The `forgex_test_m` module provides helper procedures to unit testing for Forgex.
module forgex_test_m
   use, intrinsic :: iso_fortran_env
   use :: forgex
   implicit none
   private

   public :: is_valid__in
   public :: is_valid__match
   public :: is_valid__regex
   public :: runner_in
   public :: runner_match
   public :: runner_regex

contains

   !> This function checks if a pattern is found within a string and
   !> compares the result to the `correct_answer`.
   function is_valid__in(pattern, str, correct_answer) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      logical,      intent(in) :: correct_answer

      logical :: res

      res = (pattern .in. str) .eqv. correct_answer
   end function is_valid__in


   !> This function checks if a pattern matches exactly a string and
   !> compares the result to the correct answer.
   function is_valid__match(pattern, str, correct_answer) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      logical,      intent(in) :: correct_answer

      logical :: res

      res = (pattern .match. str) .eqv. correct_answer
   end function is_valid__match


   !> This function checks if a pattern matches a string using the `regex`
   !> function and compares the result to the expected answer.
   function is_valid__regex(pattern, str, answer, substr) result(res)
      implicit none
      character(*),              intent(in)    :: pattern, str
      character(*),              intent(in)    :: answer
      character(:), allocatable, intent(inout) :: substr

      character(:), allocatable :: local
      integer(int32)            :: length
      logical                   :: res

      call regex(pattern, str, local, length)
      substr = local

      res = local == answer

   end function is_valid__regex


   !> This subroutine runs the `is_valid__in` function and prints the result.
   subroutine runner_in(pattern, str, answer, result)
      implicit none
      character(*), intent(in)    :: pattern, str
      logical,      intent(in)    :: answer
      logical,      intent(inout) :: result
      logical :: res

      res = is_valid__in(pattern, str, answer)

      if (res) then
         write(error_unit, '(a, a, a)') 'result(in   ): Success', ' '//trim(pattern)
      else
         write(error_unit, '(a, a, a)') 'result(in   ): FAILED ', ' '//trim(pattern), ' '//trim(str)
      end if

      result = result .and. res
   end subroutine runner_in


   !> This subroutine runs the `is_valid__match` function and prints the result.
   subroutine runner_match(pattern, str, answer, result)
      implicit none
      character(*), intent(in)    :: pattern, str
      logical,      intent(in)    :: answer
      logical,      intent(inout) :: result
      logical :: res

      res = is_valid__match(pattern, str, answer)


      if (res) then
         if (answer) then
            write(error_unit, '(a, a, a)') 'result(match): Success', ' '//trim(pattern), ' "'//trim(str)//'"'
         else
            write(error_unit, '(a, a, a)') 'result(match): Success', ' '//trim(pattern)
         end if
      else
         write(error_unit, '(a, a, a)') 'result(match): FAILED ' , ' '//trim(pattern),' "'//trim(str)//'"'
      end if

      result = result .and. res

   end subroutine runner_match


   !> This subroutine runs the `is_valid__regex` function and prints the result.
   subroutine runner_regex(pattern, str, answer, result)
      implicit none
      character(*), intent(in)    :: pattern, str
      character(*), intent(in)    :: answer
      logical,      intent(inout) :: result

      character(:), allocatable :: substr
      logical                   :: res

      res = is_valid__regex(pattern, str, answer, substr)

      if (res) then
         if (answer == substr) then
            write(error_unit, '(a, a, a)') 'result(regex): Success', ' '//trim(pattern), ' "'//trim(substr)//'"'
         else
            write(error_unit, '(a, a, a)') 'result(regex): Success', ' '//trim(pattern)
         end if
      else
         write(error_unit, '(a, a, a)') 'result(regex): FAILED ', ' '//trim(pattern), ' "'//trim(substr)//'"'
      end if

      result = result .and. res
   end subroutine runner_regex


end module forgex_test_m