program test_001
   use, intrinsic :: iso_fortran_env
   use :: test_m
   implicit none

   logical :: res = .true.

   ! Test case #1. 
   ! Tests for ASCII Character set.
   call runner_match('[a]{1,4}', 'aaaaa', .false., res)
   call runner_match('[a]{1,4}', 'aaaa', .true., res)
   call runner_match('[a]{1,4}', 'aaa', .true., res)
   call runner_match('[a]{1,4}', 'aa', .true., res)
   call runner_match('[a]{1,4}', 'a', .true., res)
   call runner_match('[a]{1,4}', '', .false., res)

   call runner_match('[a]{,7}', 'aaaaaaaa', .false., res)

   call runner_match('[a]{1,2}', 'aaa', .false., res)
   call runner_match('[a]{1,2}', 'aa', .true., res)
   call runner_match('[a]{1,2}', 'a', .true., res)
   call runner_match('[a]{1,2}', '', .false., res)
   
   call runner_match('[a]{,2}', 'aaa', .false., res)
   call runner_match('[a]{,2}', 'aa', .true., res)
   call runner_match('[a]{,2}', 'a', .true., res)
   call runner_match('[a]{,2}', '', .true., res)
! goto 100

   call runner_match('[a]{,4}', 'aaaaa', .false., res)
   call runner_match('[a]{,4}', 'aaaa', .true., res)
   call runner_match('[a]{,4}', 'aaa', .true., res)
   call runner_match('[a]{,4}', 'aa', .true., res)
   call runner_match('[a]{,4}', 'a', .true., res)
   call runner_match('[a]{,4}', '', .true., res)


   
   call runner_match('[a]{2, 4}', "aaaaa", .false., res)
   call runner_match('[a]{2, 4}', "aaaa", .true., res)
   call runner_match('[a]{2, 4}', "aaa", .true., res)
   call runner_match('[a]{2, 4}', "aa", .true., res)
   call runner_match('[a]{2, 4}', "a", .false., res)
   call runner_match('[a]{2, 4}', "", .false., res)
   

   call runner_match('[a]{7,7}', 'aaaaaa', .false., res)
   call runner_match("hoge(fuga|piyo)", 'hogepiyo', .true., res)
   call runner_match("f[a-zA-Z]*z", 'fooBARbaz', .true., res)
 
   call runner_match("(@|@)(a|a)*", "aa", .false., res)
   call runner_match("aaa", "aa", .false., res)
   call runner_match("(a|b)", "b", .true., res)

   call runner_in("^[a-z]{3}", "0abc", .false., res)
   call runner_in("^abc", "0abc", .false., res)
   call runner_in("^abc$", "abc", .true., res)

   call runner_in("^abc$", "foo"//char(10)//"abc", .true., res)
   call runner_in("^[a-z][A-Z]$", "aT", .true., res)
   call runner_in("^[a-z]$foo$", "a"//char(10)//"foo", .true., res)
   
   call runner_in("[^a-z]", "A", .true., res)
   call runner_match("[^a-z]", "B", .true., res)

100 continue

   if (res) then
      print *, "=== TEST CASE 1 END ==="
      stop
   else
      error stop
   end if 


end program test_001