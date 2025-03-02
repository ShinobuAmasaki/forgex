program test_010
   use, intrinsic :: iso_fortran_env
   use :: forgex_test_m
   use :: forgex_utf8_m
   implicit none

   logical :: res = .true.

   character(:), allocatable :: text, bad
   character(1) :: c1, c2, c3

   ! Test case #10.
   ! Tests for input with bad utf-8 string.

   print *, "=== TEST CASE 10 BEGIN ==="
!=====================================================================!

   ! UTF-8 characte of the correct lenght.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_4_mask)//c1//c1//c1
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)

   ! Invalid Leader bytes
   c1 = nchar(continuation_mask)
   c2 = nchar(lead_4_mask)
   c3 = nchar(lead_2_mask)
   bad = c2//c3//c1
   text = bad//'a'
   call runner_in("[a-z]", text, .true., res)

   ! Contenuation bytes are shorter than indicated by the leader byte.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_4_mask)//c1//c1
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)

   ! Contenuation bytes are shorter than indicated by the leader byte.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_3_mask)//c1
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   
   ! Contenuation bytes are shorter than indicated by the leader byte.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_2_mask)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)

   ! Continuation bytes without a leader byte.
   c1 = nchar(continuation_mask)
   bad = c1
   text = bad//bad//'a'//bad
   call runner_in("\w", text, .true., res)
   
   ! Continuation bytes without a leader byte.
   c1 = nchar(continuation_mask)
   bad = c1
   text = bad//'a'//bad
   call runner_in("[a-z]+", text, .true., res)

!=====================================================================!
   if (res) then
      print *, "=== TEST CASE 10 END ==="
      stop
   else
      error stop
   end if
end program test_010