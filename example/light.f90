! Fortran Regular Expression (Forgex)
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     benchmark_light is a part of Forgex.
!
!! This file includes a light test case of regular expression matching.
!! do execute followings: 
!! `fpm run light --example --compiler ifx --flag "/Qopenmp"` for Windows.
!! `fpm run light --example --compiler ifx --flag "" for Linux.
program benchmark_light
   use :: iso_fortran_env, only: stderr=>error_unit
   !$ use :: omp_lib
   use :: forgex_time_measurement_m
   use :: forgex
   implicit none

   integer, parameter :: siz = 1000
   
   integer :: i   
   logical :: res(siz), answer
   character(:), allocatable :: pattern, text

   write(stderr, *) "============ FORGEX BENCHMARKING TEST: light ============"

   res(:) = .false.
   pattern = "'a[^x]{20}b'"
   text = 'aaakkkkkkkkkkkkkkkkkkkb'
   answer = .false.
   !$ call omp_set_num_threads(4)

      write(stderr, *) "=== Information ==="
      write(stderr, *)              "Pattern    : ", pattern
      write(stderr, *)              "Text       : ", text
      write(stderr, "(1x, a, i0)")  "Loop size  : ", siz
      write(stderr, *)              "Operator   : ", ".match."
      write(stderr, "(1x, a, l1)")  "Answer     : ", answer
   !$ write(stderr, "(1x, a, i0)")  "NUM_THREADS: ", omp_get_num_threads()

   ! Time measurement of ordinary DO loop
   call time_begin()
   do i = 1, siz
      res(i) = pattern .in. text
   end do
   call time_end("ordinary DO loop")

   ! Time measurement of DO CONCURRENT loop
   call time_begin()
   do concurrent (i = 1:siz)
      res(i) = pattern .in. text
   end do
   call time_end("DO CONCURRENT loop")

   write(stderr, *) "---------------------------------------------------------"
   if (all(res) .eqv. answer) then
      write(stderr, *) "result     : Success"
   else
      write(stderr, *) "result     : FAILED"
   endif
   
end program benchmark_light
