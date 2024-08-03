! Fortran Regular Expression (Forgex)
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     benchmark_heavy is a part of Forgex.
!
!! This file includes a heavy test case of regular expression matching.
program benchmark_heavy
   use :: iso_fortran_env, only: stderr=>error_unit
   use :: forgex_time_measurement_m
   use :: forgex
   implicit none

   integer, parameter :: siz = 16
      !! With 4 threads, it requires approximately 500MB of RAM.
   
   integer :: i   
   logical :: res(siz), answer
   character(:), allocatable :: pattern, text

   write(stderr, *) "============ FORGEX BENCHMARKING TEST: heavy ============"

   res(:) = .false.
   pattern = ".*a(a|b){500}c{20}"
   text = "akkkkkkkksscga"
   text = trim(text)//"ababababababababababababababababababababababababab" ! ab   x25
   text = trim(text)//"ababababababababababababababababababababababababab" !      x50
   text = trim(text)//"ababababababababababababababababababababababababab" !      x75
   text = trim(text)//"ababababababababababababababababababababababababab" !     x100
   text = trim(text)//"ababababababababababababababababababababababababab" !     x125
   text = trim(text)//"ababababababababababababababababababababababababab" !     x150
   text = trim(text)//"ababababababababababababababababababababababababab" !     x175
   text = trim(text)//"ababababababababababababababababababababababababab" !     x200
   text = trim(text)//"ababababababababababababababababababababababababab" !     x225
   text = trim(text)//"ababababababababababababababababababababababababab" !     x250
   text = trim(text)//"cccccccccccccccccccc"
   answer = .true.

   ! Time measurement of ordinary DO loop
   call time_begin()
   do i = 1, siz
      res(i) = pattern .match. text
   end do
   call time_end("ordinary DO loop")

   ! Time measurement of DO CONCURRENT loop
   call time_begin()
   do concurrent (i = 1:siz)
      res(i) = pattern .match. text
   end do
   call time_end("DO CONCURRENT loop")

   write(stderr, *) "---------------------------------------------------------"
   if (all(res) .eqv. answer) then
      write(stderr, *) "result: Success"
   else
      write(stderr, *) "result: FAILED"
   endif

end program benchmark_heavy