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
   use :: forgex_syntax_tree_graph_m, only: tree_t
   implicit none
   private

   public :: is_valid__in
   public :: is_valid__match
   public :: is_valid__regex
   public :: is_valid__prefix
   public :: is_valid__postfix
   public :: is_valid__middle
   public :: runner_in
   public :: runner_match
   public :: runner_regex
   public :: runner_prefix
   public :: runner_postfix
   public :: runner_middle


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

   function is_valid__prefix(pattern, expected_prefix) result(res)
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_utf8_m
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

   
   function is_valid__postfix(pattern, expected_postfix) result(res)
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_utf8_m
      implicit none
      character(*), intent(in) :: pattern, expected_postfix
      logical :: res
      character(:), allocatable :: resulting

      type(tree_t) :: tree
      call tree%build(pattern)
      resulting = get_postfix_literal(tree)

      if (len_utf8(expected_postfix) == len_utf8(resulting)) then
         res = expected_postfix == resulting
         return
      end if
      res = .false.

   end function is_valid__postfix

   function is_valid__middle(pattern, expected, middle) result(res)
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_utf8_m
      implicit none
      character(*), intent(in) :: pattern, expected
      character(:), allocatable :: middle
      logical :: res

      character(:), allocatable :: resulting
      type(tree_t) :: tree
      ! call tree%build(pattern)
      ! resulting = get_middle_literal(tree)
      ! middle = resulting
      ! if (len_utf8(expected) == len_utf8(resulting)) then
      !    res = expected == resulting
      !    return
      ! end if
      ! res = .false.

   end function is_valid__middle



!=====================================================================!

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


   subroutine runner_prefix(pattern, prefix, result)
      implicit none
      character(*), intent(in) :: pattern, prefix
      logical, intent(inout) :: result
      logical :: res

      res = is_valid__prefix(pattern, prefix)

      if (res) then
         write(error_unit, '(a,a,a)') 'result(prefix): Success', ' '//trim(pattern), ' "'//trim(prefix)//'"'
      else
         write(error_unit, '(a,a,a)') 'result(prefix): FAILED ', ' '//trim(pattern), ' "'//trim(prefix)//'"'
      end if
      result = result .and. res
   end subroutine runner_prefix

   subroutine runner_postfix(pattern, postfix, result)
      implicit none
      character(*), intent(in) :: pattern, postfix
      logical, intent(inout) :: result
      logical :: res

      res = is_valid__postfix(pattern, postfix)

      if (res) then
         write(error_unit, '(a,a,a)') 'result(postfix): Success', ' '//trim(pattern), ' "'//trim(postfix)//'"'
      else
         write(error_unit, '(a,a,a)') 'result(postfix): FAILED ', ' '//trim(pattern), ' "'//trim(postfix)//'"'
      end if
      result = result .and. res
   end subroutine runner_postfix


   subroutine runner_middle(pattern, middle, result)
      implicit none
      character(*), intent(in) :: pattern, middle
      logical, intent(inout) :: result
      character(:),allocatable :: resulting
      logical :: res

      ! res = is_valid__middle(pattern, middle, resulting)

      ! if (res) then
      !    write(error_unit, '(a,a,a)') 'result(middle): Success', ' '//trim(pattern), ' "'//trim(middle)//'"'
      ! else
      !    write(error_unit, '(a,a,a a)') 'result(middle): FAILED ', ' '//trim(pattern), ': got "'//resulting//'"', &
      !                                   ', "'//trim(middle)//'" is expected.'
      ! end if
      ! result = result .and. res
   end subroutine runner_middle

end module forgex_test_m