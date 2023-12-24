module test_m
   use, intrinsic :: iso_fortran_env
   use :: forgex
   implicit none
   private

   public :: is_valid__in
   public :: is_valid__match
   public :: runner_in
   public :: runner_match
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

   subroutine runner_in(pattern, str, answer, result)
      implicit none
      character(*), intent(in) :: pattern, str
      logical, intent(in) :: answer
      logical, intent(inout) :: result

      logical :: res
      

      res = is_valid__in(pattern, str, answer)

      write(error_unit, '(a)', advance='no') '                                          '//char(13)
      if (res) then
         write(error_unit, '(a, l, a, a)', advance='no') 'result: ', res, ' '//trim(pattern),  char(13)
      else
         write(error_unit, '(a, l, a, a)') 'result: ', res, ' '//trim(pattern), ' '//trim(str)
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

      write(error_unit, '(a)', advance='no') '                                          '//char(13)
      if (res) then
         write(error_unit, '(a, l, a, a)', advance='no') 'result: ', res, ' '//trim(pattern), char(13)
      else
         write(error_unit, '(a, l, a, a)') 'result: ', res, ' '//trim(pattern),' '//trim(str)
      end if

      result = result .and. res

   end subroutine runner_match


end module test_m