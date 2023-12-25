program test_002
   use, intrinsic :: iso_fortran_env
   use :: test_m
   implicit none

   logical :: res = .true.
   
   ! Test case #2. 
   ! Tests for UTF-8 Character set.

   call runner_in('[い]{7,7}', 'いろはにほへとち', .false., res)
   call runner_match("[い]{6}", 'いいいいいい ', .false., res)
   call runner_in("[^さ-ん]{6}",  'あいうえおか', .true., res)

   call runner_match("\s", '　', .true., res)
   call runner_match('\s*', '　　', .true., res)


   if (res) then
      print *, "=== TEST CASE 2 END ===          "
      stop
   else
      error stop
   end if 


end program test_002