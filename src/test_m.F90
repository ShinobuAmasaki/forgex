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
   use, intrinsic :: iso_fortran_env, only: int8, int32, error_unit, output_unit
   use :: forgex, only: operator(.in.), operator(.match.), regex, is_valid_regex
   use :: forgex_syntax_tree_graph_m, only: tree_t
   implicit none
   private

   public :: is_valid__pattern
   public :: is_valid__in
   public :: is_valid__match
   public :: is_valid__regex
   public :: is_valid__prefix
   public :: is_valid__suffix
   ! public :: is_valid__middle
   public :: is_valid__error


   public :: runner_validate
   public :: runner_in
   public :: runner_match
   public :: runner_regex
   public :: runner_prefix
   public :: runner_suffix
   ! public :: runner_middle
   public :: runner_error
   public :: nchar         ! negative char


contains

   !> This function checks if the given pattern is valid as a regex pattern
   !> and compares the result to the `correct_answer`.
   function is_valid__pattern(pattern, correct_answer) result(res)
      implicit none
      character(*), intent(in) :: pattern
      logical,      intent(in) :: correct_answer

      logical :: res

      res = is_valid_regex(pattern) .eqv. correct_answer
   end function is_valid__pattern

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

   !> This function checks whether the correct prefix is extracted
   !> for a given pattern.
   function is_valid__prefix(pattern, expected_prefix) result(res)
      use :: forgex_syntax_tree_optimize_m, only: get_prefix_literal
      use :: forgex_utf8_m, only: len_utf8
      implicit none
      character(*), intent(in) :: pattern, expected_prefix
      logical :: res

      character(:), allocatable :: resulting
      type(tree_t) :: tree

      call tree%build(pattern)
      resulting = get_prefix_literal(tree)

      if (len_utf8(expected_prefix) == len_utf8(resulting)) then
         res = expected_prefix == resulting
         return
      end if
      res = .false. 

   end function is_valid__prefix

   !> This function checks whether the correct suffix is extracted
   !> for a given pattern.
   function is_valid__suffix(pattern, expected_suffix) result(res)
      use :: forgex_syntax_tree_optimize_m, only: get_suffix_literal
      use :: forgex_utf8_m, only: len_utf8
      implicit none
      character(*), intent(in) :: pattern, expected_suffix
      logical :: res

      character(:), allocatable :: resulting
      type(tree_t) :: tree

      call tree%build(pattern)
      resulting = get_suffix_literal(tree)

      if (len_utf8(expected_suffix) == len_utf8(resulting)) then
         res = expected_suffix == resulting
         return
      end if
      res = .false.

   end function is_valid__suffix

   ! function is_valid__middle(pattern, expected, middle) result(res)
   !    use :: forgex_syntax_tree_optimize_m
   !    use :: forgex_utf8_m
   !    implicit none
   !    character(*), intent(in) :: pattern, expected
   !    character(:), allocatable :: middle
   !    logical :: res

   ! !    character(:), allocatable :: resulting
   ! !    type(tree_t) :: tree
   ! !    ! call tree%build(pattern)
   ! !    ! resulting = get_middle_literal(tree)
   ! !    ! middle = resulting
   ! !    ! if (len_utf8(expected) == len_utf8(resulting)) then
   ! !    !    res = expected == resulting
   ! !    !    return
   ! !    ! end if
   ! !    ! res = .false.

   ! end function is_valid__middle


   !> This function checks whether it returns the correct error for a given pattern and text.
   function is_valid__error (pattern, text, expected_err_code, return_code) result(res)
      use :: forgex_error_m
      implicit none
      character(*), intent(in) :: pattern
      character(*), intent(in) :: text
      integer, intent(in)      :: expected_err_code
      integer, intent(inout)   :: return_code

      integer(int32)           :: status
      character(256)           :: err_msg
      character(:), allocatable :: substr
      logical                  :: res

      call regex(pattern, "", substr, status=status, err_msg=err_msg)
      return_code = status

      res = (status == expected_err_code) .and. (trim(err_msg) == trim(get_error_message(expected_err_code)))

   end function is_valid__error

!=====================================================================!

   !> This subroutine runs the `is_valid__pattern` function and prints the result.
   subroutine runner_validate(pattern, answer, result)
      implicit none
      character(*), intent(in)    :: pattern
      logical,      intent(in)    :: answer
      logical,      intent(inout) :: result
      
      logical :: res

      res = is_valid__pattern(pattern, answer)

      if (res) then
#ifndef FAILED
         write(output_unit, '(a,a,l1)') 'result(validate): Success', ' '//trim(pattern)//' ', answer
#endif
      else
         write(error_unit, '(a,a,l1)') 'result(validate): FAILED ', ' '//trim(pattern)//' ', answer
      end if

      result = result .and. res
   end subroutine runner_validate

   !> This subroutine runs the `is_valid__in` function and prints the result.
   subroutine runner_in(pattern, str, answer, result)
      implicit none
      character(*), intent(in)    :: pattern, str
      logical,      intent(in)    :: answer
      logical,      intent(inout) :: result
      logical :: res

      res = is_valid__in(pattern, str, answer)

      if (res) then
         continue
#ifndef FAILED
         write(output_unit, '(a, a, a)') 'result(in   ): Success', ' '//pattern
#endif
      else
         write(error_unit, '(a, a, a)') 'result(in   ): FAILED ', ' '//pattern, ' '//trim(str)
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
#ifndef FAILED
         if (answer) then
            write(output_unit, '(a, a, a)') 'result(match): Success', ' '//trim(pattern), ' "'//trim(str)//'"'
         else
            write(output_unit, '(a, a, a)') 'result(match): Success', ' '//trim(pattern)
         end if
#endif
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
#ifndef FAILED
         if (answer == substr) then
            write(output_unit, '(a, a, a)') 'result(regex): Success', ' '//trim(pattern), ' "'//trim(substr)//'"'
         else
            write(output_unit, '(a, a, a)') 'result(regex): Success', ' '//trim(pattern)
         end if
#endif
      else
         write(error_unit, '(a, a, a)') 'result(regex): FAILED ', ' '//trim(pattern), ' "'//trim(substr)//'"'
      end if

      result = result .and. res
   end subroutine runner_regex


   !> This subroutine runs the `is_valid_prefix` function and prints the result.
   subroutine runner_prefix(pattern, prefix, result)
      implicit none
      character(*), intent(in) :: pattern, prefix
      logical, intent(inout) :: result
      logical :: res

      res = is_valid__prefix(pattern, prefix)

      if (res) then
#ifndef FAILED
         write(output_unit, '(a,a,a)') 'result(prefix): Success', ' '//trim(pattern), ' "'//trim(prefix)//'"'
#endif
      else
         write(error_unit, '(a,a,a)') 'result(prefix): FAILED ', ' '//trim(pattern), ' "'//trim(prefix)//'"'
      end if
      result = result .and. res
   end subroutine runner_prefix


   !> This function runs the `is_valid_suffix` function and prints the result.
   subroutine runner_suffix(pattern, suffix, result)
      implicit none
      character(*), intent(in) :: pattern, suffix
      logical, intent(inout) :: result
      logical :: res

      res = is_valid__suffix(pattern, suffix)

      if (res) then
#ifndef FAILED
         write(output_unit, '(a,a,a)') 'result(suffix): Success', ' '//trim(pattern), ' "'//trim(suffix)//'"'
#endif
      else
         write(error_unit, '(a,a,a)') 'result(suffix): FAILED ', ' '//trim(pattern), ' "'//trim(suffix)//'"'
      end if
      result = result .and. res
   end subroutine runner_suffix


   ! subroutine runner_middle(pattern, middle, result)
   !    implicit none
   !    character(*), intent(in) :: pattern, middle
   !    logical, intent(inout) :: result
   !    character(:),allocatable :: resulting
   !    logical :: res

   !    ! res = is_valid__middle(pattern, middle, resulting)

   !    ! if (res) then
   !    !    write(output_unit, '(a,a,a)') 'result(middle): Success', ' '//trim(pattern), ' "'//trim(middle)//'"'
   !    ! else
   !    !    write(error_unit, '(a,a,a a)') 'result(middle): FAILED ', ' '//trim(pattern), ': got "'//resulting//'"', &
   !    !                                   ', "'//trim(middle)//'" is expected.'
   !    ! end if
   !    ! result = result .and. res
   ! end subroutine runner_middle


   !> This subroutine runs `is_valid_error` function and prints its result.
   subroutine runner_error(pattern, text, code, result)
      use :: forgex_error_m
      implicit none
      character(*), intent(in) :: pattern, text
      integer, intent(in) :: code
      logical, intent(inout) :: result
      
      character(10) :: cache
      character(:), allocatable :: fmt, fmt_with_code
      logical :: res
      integer :: returned_code, width

      cache = ''
      width = max(len_trim(pattern), 15)
      write(cache, '(i0)') width
      fmt = '(a, a'//trim(adjustl(cache))//', a)'
      fmt_with_code = '(a, a'//trim(adjustl(cache))//', a, i3)'

      res = is_valid__error(pattern, text, code, returned_code)

      if (res) then
#ifndef FAILED
         write(output_unit, fmt) 'result(error): Success: ', pattern, ': "'//trim(get_error_message(returned_code))//'" '
#endif
      else
         write(error_unit, fmt_with_code) 'result(error): FAILED:  ', pattern, &
             ': "'//trim(get_error_message(returned_code))//'" error code =', returned_code
      end if

      result = result .and. res

   end subroutine runner_error

   !> nchar means 'negative char'.
   pure function nchar(i) result(chara)
      implicit none
      integer(int8), intent(in) :: i
      character(1) :: chara

      if (i < 0) then
         chara = char(i+256)
      else
         chara = char(i)
      end if
   end function nchar


end module forgex_test_m