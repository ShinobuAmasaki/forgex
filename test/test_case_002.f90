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

   call runner_regex('[ぁ-ん]{1,7}', 'いろはにほへとちりぬるを', 'いろはにほへと', res)

   call runner_regex('[ぁ-ん]{,7}', 'いろはにほへとちりぬるを', 'いろはにほへと', res)


   call runner_in("夢.{1,7}胡蝶", "昔者莊周夢爲胡蝶　栩栩然胡蝶也", .true., res)
   call runner_regex('夢.{1,7}胡蝶', "昔者莊周夢爲胡蝶　栩栩然胡蝶也", "夢爲胡蝶　栩栩然胡蝶", res )

   ! Zenkaku space
   call runner_match("\s", '　', .true., res)
   call runner_match('\s*', '　　', .true., res)
100 continue

   if (res) then
      print *, "=== TEST CASE 2 END ===          "
      stop
   else
      error stop
   end if 


end program test_002