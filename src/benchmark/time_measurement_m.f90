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
   !$ use :: omp_lib
   implicit none
   private

   public time_begin, time_end, time_lap

   integer, parameter :: record_siz = 200
   real(real64) :: begin_s, last_s, end_s
   integer :: i, ii
   type record
      real(real64) :: lap_time
      character(:), allocatable :: description
   end type record

   type(record) :: records(record_siz)

contains

   subroutine time_begin()
      implicit none

      begin_s = 0d0
      last_s = 0d0
      end_s = 0d0
      i = 1

      call cpu_time(begin_s)
      !$ begin_s = omp_get_wtime()
      last_s = begin_s

   end subroutine time_begin


   subroutine time_lap(description)
      implicit none
      character(*), intent(in) :: description


      call cpu_time(end_s)
      !$ end_s = omp_get_wtime()

      records(i)%lap_time = end_s - last_s
      records(i)%description = trim(description)
      i = i + 1
      last_s = end_s

   end subroutine time_lap

   subroutine time_end(description)
      implicit none
      character(*), intent(in) :: description

      call cpu_time(end_s)
      !$ end_s = omp_get_wtime()

      records(i)%lap_time = end_s - begin_s
      records(i)%description = "sec. : TOTAL: "//trim(description)

      do ii = 1, i-1
         write(stderr, "(f20.13, a, a)") records(ii)%lap_time, "sec. : "//records(ii)%description
      end do


      write(stderr,*) "---------------------------------------------------------"
      write(stderr,"(f20.13, a, a)") end_s - begin_s, " sec. : TOTAL: ", trim(description)
   end subroutine time_end
end module forgex_time_measurement_m