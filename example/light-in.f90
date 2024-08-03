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
!> `fpm run light-in --example --profile release --compiler ifx --flag "/Qopenmp  /Qopenmp-target-do-concurrent"` for Windows.
!> `fpm run light-in --example --profile release --compiler ifx --flag "-fopenmp -fopenmp-target-do-concurrent" for Linux.
!> @note This command may not be parallelized depending on the CPU or compiler environment.
program benchmark_light_in
   use :: iso_fortran_env, only: stderr=>error_unit
   use :: forgex_time_measurement_m
   use :: forgex
   implicit none

   integer, parameter :: siz = 1000

   integer :: i
   logical :: res(siz), answer, entire
   character(:), allocatable :: pattern, text
   character(*), parameter :: pat="a[^x]{20}b", txt='aaaaabbbbccccddddeeeeffffb'

   write(stderr, *) "============ FORGEX BENCHMARKING TEST: light ============"

   pattern = "a[^x]{20}b"
   text = 'aaaaabbbbccccddddeeeeffffb'
   answer = .true.

   res(:) = .false.

   write(stderr, *) "=== Information ==="
   write(stderr, *)              "Pattern    : ", pattern
   write(stderr, *)              "Text       : ", text
   write(stderr, "(1x, a, i0)")  "Loop size  : ", siz
   write(stderr, *)              "Operator   : ", ".in."
   write(stderr, "(1x, a, l1)")  "Answer     : ", answer
   ! !$ write(stderr, "(1x, a, i0)")  "NUM_THREADS: ", omp_get_num_threads()

   ! Time measurement of ordinary DO loop
   call time_begin()
   do i = 1, siz
      res(i) = pattern .in. text
   end do
   call time_end("ordinary DO loop")
   entire = all(res)

#ifndef IMPURE
   ! Time measurement of DO CONCURRENT loop
   res(:) = .false.
   call time_begin()
   do concurrent (i = 1:siz)
      res(i) = pattern .in. text
   end do
   call time_end("DO CONCURRENT loop")
   entire = entire .and. all(res)
#endif

   write(stderr, *) "---------------------------------------------------------"
   if (entire .eqv. answer) then
      write(stderr, *) "result     : Success"
   else
      write(stderr, *) "result     : FAILED"
   endif

end program benchmark_light_in
