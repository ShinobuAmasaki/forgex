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
module forgex_cli_time_measurement_m
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit
   use, intrinsic :: iso_c_binding, only: c_long_long, c_bool
   !$ use :: omp_lib
   use :: forgex_cli_parameters_m, only: NUM_DIGIT_TIME
   use :: forgex_cli_utils_m, only: get_os_type
   use :: forgex_enums_m, only: OS_WINDOWS
   implicit none
   private

   public :: time_begin, time_lap
   public :: get_lap_time_in_appropriate_unit

   real(real64) :: begin_s, last_s, end_s

   integer(c_long_long) :: time_begin_qhc, time_end_qhc, frequency
   logical(c_bool) :: is_supported = .false.
   logical(c_bool) :: is_succeeded = .false.

   !> For Windows, use high-resolution system call for timing.
   interface
      function QueryPerformanceCounter(PerformanceCount_count) result(is_succeeded_c) &
            bind(c, name="QueryPerformanceCounter")
         use, intrinsic :: iso_c_binding
         implicit none
         integer(c_long_long), intent(out) :: PerformanceCount_count
         logical(c_bool) :: is_succeeded_c
      end function QueryPerformanceCounter
      function QueryPerformanceFrequency(Frequency_countPerSec) result(is_supported_c) &
            bind(c, name="QueryPerformanceFrequency")
         use, intrinsic :: iso_c_binding
         implicit none
         integer(c_long_long), intent(out) :: Frequency_countPerSec
         logical(c_bool) :: is_supported_c
      end function QueryPerformanceFrequency
   end interface
   !! cf. https://qiita.com/implicit_none/items/86c9117990798c1e8b3b


contains


   !> This subroutine is for timing purpose and starts a stopwatch.
   subroutine time_begin()
      implicit none

      if (get_os_type() == OS_WINDOWS) then
         is_supported = QueryPerformanceFrequency(frequency)
         if (is_supported) then
            is_succeeded = QueryPerformanceCounter(time_begin_qhc)
         else
            !$ begin_s = omp_get_wtime()
            !$ last_s = begin_s
            !$ return
            call use_cpu_time_begin
         end if
      else
         !$ begin_s = omp_get_wtime()
         !$ last_s = begin_s
         !$ return
         call use_cpu_time_begin
      end if

   contains

      subroutine use_cpu_time_begin
         implicit none
         begin_s = 0d0
         last_s = 0d0
         end_s = 0d0
         call cpu_time(begin_s)
         last_s = begin_s
      end subroutine use_cpu_time_begin

   end subroutine time_begin


   !> This function is for timing purposes and returns the lap time
   !> since the last call of `time_begin` or `time_lap`.
   function time_lap() result(res)
      implicit none
      real(real64) :: res

      if (get_os_type()== OS_WINDOWS) then
         if (is_supported) then
            is_succeeded = QueryPerformanceCounter(time_end_qhc)
            res = dble(time_end_qhc - time_begin_qhc)/dble(frequency)
            time_begin_qhc = time_end_qhc
         else
            !$ end_s = omp_get_wtime()
            !$ res = end_s - last_s
            !$ last_s = end_s
            !$ return
            call use_cpu_time_end
         end if
      else
         !$ end_s = omp_get_wtime()
         !$ res = end_s - last_s
         !$ last_s = end_s
         !$ return
         call use_cpu_time_end
      end if
   contains

      subroutine use_cpu_time_end
         implicit none
         call cpu_time(end_s)
         res = end_s - last_s
         last_s = end_s
      end subroutine use_cpu_time_end

   end function time_lap


   !> This function takes a real number of seconds, converts it to the appropriate
   !> units, and returns a string with the unit for output.
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
         if (get_os_type() == OS_WINDOWS) then
            unit = 'us'
         else
            unit = 'μs'
         end if
         multiplied = lap_time * 1d6
      else
         unit = 'ns'
         multiplied = lap_time * 1d9
      end if

      write(res, '(f10.1, a)') multiplied, unit

   end function get_lap_time_in_appropriate_unit

end module forgex_cli_time_measurement_m