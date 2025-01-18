program main
   use :: forgex
   implicit none

   character(:), allocatable :: res
   character(64) :: err_msg

   err_msg = ""

   print *, is_valid_regex("abc(")
   print *, "abc\[]" .in. "abc[]" !!! invalid pattern
   print *, is_valid_regex("abc\[]")

   err_msg = ""
   call regex("abc\[]", "abc[]", res, err_msg=err_msg)
   print *, err_msg

   err_msg = ""
   call regex("abc[333", "abc[333]", res, err_msg=err_msg)
   print *, err_msg

   err_msg = ""
   call regex("abc]333", "abc", res, err_msg=err_msg)
   print *, err_msg

   err_msg = ""
   call regex("abc(333", "abc", res, err_msg=err_msg)
   print *, err_msg
   
   err_msg = "" 
   call regex(")", "a", res, err_msg=err_msg)
   print *, err_msg

   err_msg = "" 
   call regex("(", "a", res, err_msg=err_msg)
   print *, err_msg

   err_msg = "" 
   call regex("a)", "a", res, err_msg=err_msg)
   print *, err_msg

   err_msg = "" 
   call regex("a([)", "a", res, err_msg=err_msg)
   print *, err_msg
   ! print *, "abc)333" .in. "abc)333"

   err_msg = ""
   call regex("a(a)", "aab", res, err_msg=err_msg)
   print *, err_msg
   
   err_msg = ""
   call regex("((ab)|(de)+)+", "ab", res, err_msg=err_msg)
   print *, err_msg

end program main