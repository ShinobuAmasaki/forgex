program test_011
   use :: iso_fortran_env
   use :: forgex_test_m
   implicit none

   logical :: res = .true.

   ! Stress-test cases

   print *, '=== TEST CASE 11 BEGIN ==='

   call runner_match('a{1001}', repeat('a', 1000), .false., res)
   call runner_match('a{1000}', repeat('a', 1000), .true., res)
   call runner_match('a{999}', repeat('a', 1000), .false., res)
   
   call runner_match('(a|b)*c', repeat('ab', 500)//'c', .true., res)
   call runner_match('(a|b)*c', repeat('ab', 500), .false., res)
   call runner_match('(a+)+', 'aaaa', .true. , res)
   call runner_match('(a+)+', 'bbbb', .false. , res)
   call runner_match('(a|aa|aaa|aaaa)*', repeat('a', 1000), .true., res)
   call runner_match('(a|aa|aaa|aaaa)*', repeat('a', 1000)//'b', .false., res)
   call runner_match('((a*)b*)*', 'aaabbb', .true., res)
   call runner_match('((a*)b*)*', 'bbbb', .true., res)
   call runner_match('((a*)b*)*', 'aaa', .true., res)
   call runner_match('((a*)b*)*', 'aaabbb', .true., res)
   call runner_match('((a*)b*)*', 'ababab', .true., res)
   call runner_match('((a*)b*)*', '', .true., res)
   call runner_match('a*a*a*a*b', repeat('a', 1000)//'b', .true., res)
   call runner_match('a*a*a*a*b', repeat('a', 1000)//'c', .false., res)
   call runner_match('(.+)+', 'abc', .true., res)
   call runner_match('(.+)+', '', .false., res)
   call runner_match('(.*.*.*.*.*.*.*.*.*.*)', 'x', .true., res)
   call runner_match('(a|b|c|d|e)*z', repeat('abcde', 200) // 'z', .true., res)
   call runner_match('(.+)*a', repeat('x', 1000) // 'a', .true., res)
   call runner_match('(.+)*a', repeat('x', 1000), .false., res)
   call runner_match('((ab)+c)*', 'abababc', .true., res)
   call runner_match('((ab)+c)*', 'ababab', .false., res)
   call runner_match('((a|b)*c)*', 'abcabcabc', .true., res)
   call runner_match('((a|b)*c)*', 'abcabcab', .false., res)
   
!+-------------------------------------------------------------------------+! 
   call runner_match('(a|b|c|d|e){0,1000}', repeat('a', 1000), .true., res)
   call runner_match('(a|b|c|d|e){0,1000}', repeat('a', 1001), .false., res)
   call runner_match('((a|b)*c){0,1000}', repeat('ab', 500)//'c', .true., res)
   call runner_match('((a|b)*c){0,1000}', repeat('ab', 500)//'d', .false., res)
   call runner_match('((a|b)*c){1000}', repeat('ab', 499)//'c', .false., res)

   call runner_match('((a|b)*c){1}', repeat('abc', 1), .true., res)

   call runner_match('((a|b)*c){2}', repeat('abc', 2), .true., res)  ! Failed v4.3
   call runner_match('((a|b)*c){3}', repeat('abc', 3), .true., res)  ! Failed v4.3
   call runner_match('((a|b)*c){4}', repeat('abc', 4), .true., res)  ! Failed v4.3
   call runner_match('((a|b)*c){5}', repeat('abc', 5), .true., res)  ! Failed v4.3
   call runner_match('((a|b)*c){6}', repeat('abc', 6), .true., res)  ! Failed v4.3
   call runner_match('((a|b)*c){7}', repeat('abc', 7), .true., res)  ! Failed v4.3
   call runner_match('((a|b)*c){8}', repeat('abc', 8), .true., res)  ! Failed v4.3

   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)
   ! call runner_match('', repeat('', ), , res)


!=====================================================================!
   if (res) then
      write(error_unit, *) '=== TEST CASE 11 END ==='
      stop
   else
      error stop
   end if
         

end program test_011