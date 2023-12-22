program test_002
   use, intrinsic :: iso_fortran_env
   use :: test_m
   implicit none

   ! Test case #2. 
   ! Tests for UTF-8 Character set.

   call runner_in('[い]{7,7}', 'いろはにほへとち', .false.)
   call runner_match("[い]{6}", 'いいいいいい ', .false.)
   call runner_in("[^さ-ん]{6}",  'あいうえおか', .true.)

   print *, "=== TEST CASE 2 END ===          "

end program test_002