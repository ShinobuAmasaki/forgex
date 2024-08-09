! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_time_measurement_m module is a part of Forgex.
!
!! This file provides procedures for time measurement.
!
!> This module provides procedures to measure the time it takes to execute.
module forgex_time_measurement_m
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit
   use, intrinsic :: iso_c_binding, only: c_long_long, c_bool
   !$ use :: omp_lib
   use :: forgex_cli_parameters_m
   implicit none
   private

   public :: time_begin, time_lap
   public :: get_lap_time_in_appropriate_unit

   real(real64) :: begin_s, last_s, end_s
   integer :: i, ii

   
   !! cf. https://qiita.com/implicit_none/items/86c9117990798c1e8b3b
#if defined(_WIN32) || defined(_WIN64)

   integer(c_long_long) :: time_begin_qhc, time_end_qhc, frequency
   logical(c_bool) :: is_supported, is_succeeded

   interface 
      function QueryPerformanceCounter(lPerformanceCount_count) result(is_succeeded) &
            bind(c, name="QueryPerformanceCounter")
         use, intrinsic :: iso_c_binding
         implicit none
         integer(c_long_long), intent(out) :: lPerformanceCount_count
         logical(c_bool) :: is_succeeded
      end function QueryPerformanceCounter
   end interface
   interface
      function QueryPerformanceFrequency(lFrequency_countPerSec) result(is_supported) &
            bind(c, name="QueryPerformanceFrequency")
         use, intrinsic :: iso_c_binding
         implicit none
         integer(c_long_long), intent(out) :: lFrequency_countPerSec
         logical(c_bool) :: is_supported
      end function QueryPerformanceFrequency
   end interface
#endif

contains

#if defined(_WIN32) || defined(_WIN64)

   subroutine time_begin()
      implicit none
      is_supported = QueryPerformanceFrequency(frequency)
      if (is_supported) then
         is_succeeded = QueryPerformanceCounter(time_begin_qhc)
      else
         begin_s = 0d0
         last_s = 0d0
         end_s = 0d0
         call cpu_time(begin_s)
         last_s = begin_s
      end if       

   end subroutine time_begin

   function time_lap() result(res)
      implicit none
      real(real64) :: res
      if (is_supported) then
         is_succeeded = QueryPerformanceCounter(time_end_qhc)
         res = dble(time_end_qhc - time_begin_qhc)/dble(frequency)
      else
         call cpu_time(end_s)
         res = end_s - last_s
         last_s = end_s
      end if
   end function time_lap

#else

   subroutine time_begin()
      implicit none

      begin_s = 0d0
      last_s = 0d0
      end_s = 0d0

      call cpu_time(begin_s)
      !$ begin_s = omp_get_wtime()

      last_s = begin_s

   end subroutine time_begin


   function time_lap() result(res)
      implicit none
      real(real64) :: res

      call cpu_time(end_s)
      !$ end_s = omp_get_wtime()

      res = end_s - last_s
      last_s = end_s

   end function time_lap
#endif

   function get_lap_time_in_appropriate_unit(lap_time) result(res)
      implicit none
      real(real64), intent(in) :: lap_time
      character(NUM_DIGIT_TIME) :: res

      character(3) :: unit
      real(real64) :: multiplied

      unit = 's'

      if (lap_time >= 6d1) then
         unit = 'm'
         multiplied = lap_time / 6d1
      else if (lap_time >= 1d0) then
         unit = 's'
         multiplied = lap_time
      else if (lap_time >= 1d-3) then
         unit = 'ms'
         multiplied = lap_time * 1d3
      else if (lap_time >= 1d-6) then
         unit = 'μs'
         multiplied = lap_time * 1d6
      end if

      write(res, '(f6.1, a)') multiplied, unit

   end function get_lap_time_in_appropriate_unit

end module forgex_time_measurement_m