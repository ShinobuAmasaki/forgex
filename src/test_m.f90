module test_m
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

   function is_valid__in(pattern, str, correct_answer) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      logical, intent(in) :: correct_answer
      logical :: res
      
      res = (pattern .in. str) .eqv. correct_answer

   end function is_valid__in

   function is_valid__match(pattern, str, correct_answer) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      logical, intent(in) :: correct_answer
      logical :: res

      res = (pattern .match. str) .eqv. correct_answer
   end function is_valid__match

   function is_valid__regex(pattern, str, answer, substr) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      character(*), intent(in) :: answer
      character(:), allocatable, intent(inout) :: substr
      character(:), allocatable :: local
      integer(int32) :: length
      logical :: res 


      local = regex(pattern, str, length)

      substr = local

      res = trim(local) == trim(answer)

   end function is_valid__regex

   subroutine runner_in(pattern, str, answer, result)
      implicit none
      character(*), intent(in) :: pattern, str
      logical, intent(in) :: answer
      logical, intent(inout) :: result

      logical :: res
      

      res = is_valid__in(pattern, str, answer)

      if (res) then
         write(error_unit, '(a, a, a)') 'result(in   ): Success', ' '//trim(pattern)
      else
         write(error_unit, '(a, a, a)') 'result(in   ): FAILED ', ' '//trim(pattern), ' '//trim(str)
      end if

      result = result .and. res
      
   end subroutine runner_in

   subroutine runner_match(pattern, str, answer, result)
      implicit none
      character(*), intent(in) :: pattern, str
      logical, intent(in) :: answer
      logical, intent(inout) :: result

      logical :: res
      

      res = is_valid__match(pattern, str, answer)

      ! write(error_unit, '(a)', advance='no') '                                          '//char(13)
      if (res) then
         write(error_unit, '(a, a, a)') 'result(match): Success', ' '//trim(pattern), ' "'//trim(str)//'"'
      else
         write(error_unit, '(a, a, a)') 'result(match): FAILED ' , ' '//trim(pattern),' "'//trim(str)//'"'
      end if

      result = result .and. res

   end subroutine runner_match


   subroutine runner_regex(pattern, str, answer, result)
      implicit none
      character(*), intent(in) :: pattern, str
      character(*), intent(in) :: answer
      logical, intent(inout) :: result

      character(:), allocatable :: substr

      logical :: res

      res = is_valid__regex(pattern, str, answer, substr)

      ! write(error_unit, '(a)', advance='no') '                                          '//char(13)

      if (res) then
         write(error_unit, '(a, a, a)') 'result(regex): Success', ' '//trim(pattern), ' "'//trim(substr)//'"'
      else
         write(error_unit, '(a, a, a)') 'result(regex): FAILED ', ' '//trim(pattern), ' "'//trim(substr)//'"'
      end if

      result = result .and. res

   end subroutine runner_regex


end module test_m