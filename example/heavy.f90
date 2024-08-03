! Fortran Regular Expression (Forgex)
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     benchmark_heavy is a part of Forgex.
!
!! This file includes a heavy test case of regular expression matching.
!> This program performs some heavyweight tests to verify that Forgex `.match.` operator 
!< is enable to be used in `do concurrent`. Do execute followings:  
!> `fpm run heavy --example --profile release --compiler ifx --flag "/Qopenmp  /Qopenmp-target-do-concurrent"` for Windows.
!> `fpm run heavy --example --profile release --compiler ifx --flag "-fopenmp -fopenmp-target-do-concurrent" for Linux.
!> @note This command may not be parallelized depending on the CPU or compiler environment.
program benchmark_heavy
   use :: iso_fortran_env, only: stderr=>error_unit
   !$ use :: omp_lib
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

   ! !$ call omp_set_num_threads(4)

      write(stderr, *) "=== Information ==="
      write(stderr, *)              "Pattern    : ", pattern
      write(stderr, *)              "Text       : ", text
      write(stderr, "(1x, a, i0)")  "Loop size  : ", siz
      write(stderr, *)              "Operator   : ", ".match."
      write(stderr, "(1x, a, l1)")  "Answer     : ", answer
   ! !$ write(stderr, "(1x, a, i0)")  "NUM_THREADS: ", omp_get_num_threads()

   !! WARNING: Do NOT use the `.in.` operator for this test, as it will take too long to wait
   !! on currently version.

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

      ! Time measurement of OPENMP PARALLEL DO
   call time_begin()
   !$omp parallel do
   do i = 1, siz
      res(i) = pattern .match. text
   end do
   !$omp end parallel do
   call time_end("OpenMP parallel do loop")


   write(stderr, *) "---------------------------------------------------------"
   if (all(res) .eqv. answer) then
      write(stderr, *) "result: Success"
   else
      write(stderr, *) "result: FAILED"
   endif

end program benchmark_heavy