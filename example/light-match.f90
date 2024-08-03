! Fortran Regular Expression (Forgex)
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     benchmark_light is a part of Forgex.
!
!! This file includes a light test case of regular expression matching.

!> This program performs some lightweight tests to verify that Forgex operators
!> are enable to be used in `do concurrent`. Do execute followings:
!> `fpm run light-match --example --profile release --compiler ifx --flag "/Qopenmp  /Qopenmp-target-do-concurrent"` for Windows.
!> `fpm run light-match --example --profile release --compiler ifx --flag "-fopenmp -fopenmp-target-do-concurrent" for Linux.
!> @note This command may not be parallelized depending on the CPU or compiler environment.
program benchmark_light_match
   use :: iso_fortran_env, only: stderr=>error_unit
   !$ use :: omp_lib
   use :: forgex_time_measurement_m
   use :: forgex
   implicit none

   integer, parameter :: siz = 1000

   integer :: i
   logical :: res(siz), answer, entire
   character(:), allocatable :: pattern, text

   write(stderr, *) "============ FORGEX BENCHMARKING TEST: light ============"

   pattern = "a[^x]{20}b"
   text = 'abbbbccccddddeeeeffffb'
   answer = .true.

   ! Initialize
   res(:) = .false.

   ! !$ call omp_set_num_threads(4)

   write(stderr, *) "=== Information ==="
   write(stderr, *)              "Pattern    : ", pattern
   write(stderr, *)              "Text       : ", text
   write(stderr, "(1x, a, i0)")  "Loop size  : ", siz
   write(stderr, *)              "Operator   : ", ".match."
   write(stderr, "(1x, a, l1)")  "Answer     : ", answer
   ! !$ write(stderr, "(1x, a, i0)")  "NUM_THREADS: ", omp_get_num_threads()

   ! Time measurement of ordinary DO loop
   call time_begin()
   do i = 1, siz
      res(i) = pattern .match. text
   end do
   call time_end("ordinary DO loop")
   entire = all(res)

#ifndef IMPURE
   ! Time measurement of DO CONCURRENT loop
   res(:) = .false.
   call time_begin()
   do concurrent (i = 1:siz)
      res(i) = pattern .match. text
   end do
   call time_end("DO CONCURRENT loop")
   entire = entire .and. all(res)
#endif

   ! Time measurement of OPENMP PARALLEL DO
   res(:) = .false.
   call time_begin()
   !$omp parallel do
   do i = 1, siz
      res(i) = pattern .match. text
   end do
   !$omp end parallel do
   call time_end("OpenMP parallel do loop")
   entire = entire .and. all(res)

   write(stderr, *) "---------------------------------------------------------"
   if (entire .eqv. answer) then
      write(stderr, *) "result     : Success"
   else
      write(stderr, *) "result     : FAILED"
   endif

end program benchmark_light_match
