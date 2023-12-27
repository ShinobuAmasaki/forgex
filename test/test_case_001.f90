program test_001
   use, intrinsic :: iso_fortran_env
   use :: test_m
   implicit none

   logical :: res = .true.

print *, 'hoge'
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

   call runner_match('[a]{,4}', 'aaaaa', .false., res)
   call runner_match('[a]{,4}', 'aaaa', .true., res)
   call runner_match('[a]{,4}', 'aaa', .true., res)
   call runner_match('[a]{,4}', 'aa', .true., res)
   call runner_match('[a]{,4}', 'a', .true., res)
   call runner_match('[a]{,4}', '', .true., res)

   call runner_match('[a-z]{,7}', 'aaaaaaaa', .false., res)
   call runner_match('[a-z]{,7}', 'aaaaaaa', .true., res)
   call runner_match('[a-z]{,7}', 'aaaaaa', .true., res)
   call runner_match('[a-z]{,7}', 'aaaaa', .true., res)
   call runner_match('[a-z]{,7}', 'aaaa', .true., res)
   call runner_match('[a-z]{,7}', 'aaa', .true., res)
   call runner_match('[a-z]{,7}', 'aa', .true., res)
   call runner_match('[a-z]{,7}', 'a', .true., res)
   call runner_match('[a-z]{,7}', '', .true., res)

   call runner_match('[a-z]{2,7}', 'aaaaaaaa', .false., res)
   call runner_match('[a-z]{2,7}', 'aaaaaaa', .true., res)
   call runner_match('[a-z]{2,7}', 'aaaaaa', .true., res)
   call runner_match('[a-z]{2,7}', 'aaaaa', .true., res)
   call runner_match('[a-z]{2,7}', 'aaaa', .true., res)
   call runner_match('[a-z]{2,7}', 'aaa', .true., res)
   call runner_match('[a-z]{2,7}', 'aa', .true., res)
   call runner_match('[a-z]{2,7}', 'a', .false., res)
   call runner_match('[a-z]{2,7}', '', .false., res)

   call runner_regex('[a-z]{1,7}', 'aaaaaaab', 'aaaaaaa', res)
   call runner_regex('[a-z]{1,6}', 'aaaaaaab', 'aaaaaa', res)
   call runner_regex('[a-z]{1,5}', 'aaaaaaab', 'aaaaa', res)
   call runner_regex('[a-z]{1,4}', 'aaaaaaab', 'aaaa', res)
   call runner_regex('[a-z]{1,3}', 'aaaaaaab', 'aaa', res)
   call runner_regex('[a-z]{1,2}', 'aaaaaaab', 'aa', res)
   call runner_regex('[a-z]{1,1}', 'aaaaaaab', 'a', res)

   call runner_regex('[a-z]{,7}', 'aaaaaaab', 'aaaaaaa', res)
   call runner_regex('[a-z]{,6}', 'aaaaaaab', 'aaaaaa', res)
   call runner_regex('[a-z]{,5}', 'aaaaaaab', 'aaaaa', res)
   call runner_regex('[a-z]{,4}', 'aaaaaaab', 'aaaa', res)
   call runner_regex('[a-z]{,3}', 'aaaaaaab', 'aaa', res)
   call runner_regex('[a-z]{,2}', 'aaaaaaab', 'aa', res)
   call runner_regex('[a-z]{,1}', 'aaaaaaab', 'a', res)
   
   call runner_match('[a]{2, 4}', "aaaaa", .false., res)
   call runner_match('[a]{2, 4}', "aaaa", .true., res)
   call runner_match('[a]{2, 4}', "aaa", .true., res)
   call runner_match('[a]{2, 4}', "aa", .true., res)
   call runner_match('[a]{2, 4}', "a", .false., res)
   call runner_match('[a]{2, 4}', "", .false., res)
   
   call runner_match("a.{1,5}g", "abgdefg", .true., res)

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