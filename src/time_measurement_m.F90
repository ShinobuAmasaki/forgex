module forgex_time_measurement_m
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit
   use :: omp_lib
   implicit none

   real(real64) :: begin_s, last_s, end_s
   integer :: i_cpu = 1
   integer :: i, ii
   type record
      real(real64) :: lap_time
      character(:), allocatable :: description
   end type record

   type(record) :: records(200)

contains

   subroutine time_begin()
      implicit none

      begin_s = 0d0
      last_s = 0d0
      end_s = 0d0

      begin_s = omp_get_wtime()
      last_s = begin_s

   end subroutine time_begin


   subroutine time_lap(description)
      implicit none
      character(*), intent(in) :: description



      end_s = omp_get_wtime()

      records(i)%lap_time = end_s - last_s
      records(i)%description = trim(description)
      i = i + 1
      last_s = end_s

   end subroutine time_lap

   subroutine time_end(description)
      implicit none
      character(*), intent(in) :: description


      end_s = omp_get_wtime()

      records(i)%lap_time = end_s - begin_s
      records(i)%description = "sec. : TOTAL: "//trim(description)

      do ii = 1, i-1
         write(stderr, *) records(ii)%lap_time, "sec. : "//records(ii)%description
      end do


      write(stderr,*) "-------------------------------"
      write(stderr,*) end_s - begin_s, "sec. : TOTAL: ", trim(description)
   end subroutine time_end
   
end module forgex_time_measurement_m