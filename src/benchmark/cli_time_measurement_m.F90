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
   use :: forgex_cli_parameters_m
   use :: forgex_cli_utils_m
   use :: forgex_enums_m
   implicit none
   private

   public :: time_begin, time_lap
   public :: get_lap_time_in_appropriate_unit

   real(real64) :: begin_s, last_s, end_s
   integer :: i, ii

   integer(c_long_long) :: time_begin_qhc, time_end_qhc, frequency
   logical(c_bool) :: is_supported = .false.
   logical(c_bool) :: is_succeeded = .false.

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


   subroutine time_begin()
      implicit none

      if (get_os_type() == OS_WINDOWS) then
         is_supported = QueryPerformanceFrequency(frequency)
         if (is_supported) then
            is_succeeded = QueryPerformanceCounter(time_begin_qhc)
         else
            call use_cpu_time_begin
         end if            
      else
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

   function time_lap() result(res)
      implicit none
      real(real64) :: res

      if (get_os_type()== OS_WINDOWS) then
         if (is_supported) then
            is_succeeded = QueryPerformanceCounter(time_end_qhc)
            res = dble(time_end_qhc - time_begin_qhc)/dble(frequency)
         else
            call use_cpu_time_end
         end if
      else
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
      ! else if (lap_time >= 1d-6) then
      else
         if (get_os_type() == OS_WINDOWS) then
            unit = 'us'
         else
            unit = 'μs'
         end if
         multiplied = lap_time * 1d6
      end if

      write(res, '(f10.1, a)') multiplied, unit

   end function get_lap_time_in_appropriate_unit

end module forgex_cli_time_measurement_m