#ifdef IMPURE
#define pure
#endif

module forgex_time_measurement_m
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit
   use :: omp_lib
   implicit none

   real(real64) :: begin_s, last_s, end_s
   integer :: i = 1, ii
   type record
      real(real64) :: lap_time
      character(:), allocatable :: description
   end type record

   type(record) :: records(200)

contains

   pure subroutine time_begin()
      implicit none
#if defined(IMPURE) && defined(BENCH)
      begin_s = 0d0
      last_s = 0d0
      end_s = 0d0

      begin_s = omp_get_wtime()
      last_s = begin_s
#endif
   end subroutine time_begin


   pure subroutine time_lap(description)
      implicit none
      character(*), intent(in) :: description

#if defined(IMPURE) && defined(BENCH)

      end_s = omp_get_wtime()

      records(i)%lap_time = end_s - last_s
      records(i)%description = trim(description)
      i = i + 1
      last_s = end_s
#endif
   end subroutine time_lap

   pure subroutine time_end(description)
      implicit none
      character(*), intent(in) :: description

#if defined(IMPURE) && defined(BENCH)
      end_s = omp_get_wtime()

      records(i)%lap_time = end_s - begin_s
      records(i)%description = "sec. : TOTAL: "//trim(description)

      do ii = 1, i-1
         write(stderr, *) records(ii)%lap_time, "sec. : "//records(ii)%description
      end do


      write(stderr,*) "-------------------------------"
      write(stderr,*) end_s - begin_s, "sec. : TOTAL: ", trim(description)
#endif
   end subroutine time_end

   subroutine time_begin_cpu()
      implicit none
      
      begin_s = 0d0
      last_s = 0d0
      end_s = 0d0

      call cpu_time(begin_s)
      last_s = begin_s
   end subroutine time_begin_cpu

   subroutine time_lap_cpu(description)
      implicit none
      character(*), intent(in) :: description

      call cpu_time(end_s)

      records(i)%lap_time = end_s - last_s
      records(i)%description = trim(description)
      i = i + 1
      last_s = end_s
   end subroutine time_lap_cpu

   subroutine time_end_cpu(description)
      implicit none
      character(*), intent(in) :: description
      call cpu_time(end_s)

      records(i)%lap_time = end_s - begin_s
      records(i)%description = "sec. : TOTAL: "//trim(description)

      do ii = 1, i-1
         write(stderr, *) records(ii)%lap_time, "sec. : "//records(ii)%description
      end do


      write(stderr,*) "-------------------------------"
      write(stderr,*) end_s - begin_s, "sec. : TOTAL: ", trim(description)
      
   end subroutine time_end_cpu
   
end module forgex_time_measurement_m