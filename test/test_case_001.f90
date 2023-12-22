program test_001
   use, intrinsic :: iso_fortran_env
   use :: test_m
   implicit none

   ! Test case #1. 
   ! Tests for ASCII Character set.

   call runner_match('[a]{7,7}', 'aaaaaa', .false.)
   call runner_match("hoge(fuga|piyo)", 'hogepiyo', .true.)
   call runner_match("f[a-zA-Z]*z", 'fooBARbaz', .true.)
 
   call runner_match("(@|@)(a|a)*", "aa", .false.)
   call runner_match("aaa", "aa", .false.)
   call runner_match("(a|b)", "b", .true.)

   call runner_in("^[a-z]{3}", "0abc", .false.)
   call runner_in("^abc", "0abc", .false.)
   call runner_in("^abc$", "abc", .true.)

   call runner_in("^abc$", "foo"//char(10)//"abc", .true.)
   call runner_in("^[a-z][A-Z]$", "aT", .true.)
   call runner_in("^[a-z]$foo$", "a"//char(10)//"foo", .true.)
   
   call runner_in("[^a-z]", "A", .true.)
   call runner_match("[^a-z]", "B", .true.)

   print *, "=== TEST CASE 1 END ==="

end program test_001