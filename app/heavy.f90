program main
   use :: forgex
   use :: forgex_time_measurement_omp_m
   implicit none

   character(:), allocatable :: text, pattern
   logical :: res, entire, answer
   integer :: i

   pattern = ".*a(a|b){500}c{20}"
   text =             "ababababababababababababababababababababababababab" ! (ab) x25
   text = trim(text)//"ababababababababababababababababababababababababab" !      x50
   text = trim(text)//"ababababababababababababababababababababababababab" !      x75
   text = trim(text)//"ababababababababababababababababababababababababab" !     x100
   text = trim(text)//"ababababababababababababababababababababababababab" !     x125
   text = trim(text)//"ababababababababababababababababababababababababab" !     x150
   text = trim(text)//"ababababababababababababababababababababababababab" !     x175
   text = trim(text)//"ababababababababababababababababababababababababab" !     x200
   text = trim(text)//"ababababababababababababababababababababababababab" !     x225
   text = trim(text)//"ababababababababababababababababababababababababab" !     x250
   text = "akkkkkkkksscga"//trim(text)
   text = trim(text)//"ccccccccccccccccccc"
   answer = .false.

   call time_begin()
   do i = 1, 10
      res = pattern .match. text
      entire = entire .and. res
      print *, "i = ", i
   end do

   call time_lap("")
   call time_end(" matching by .match. operator")

   print *, entire .eqv. answer

end program main