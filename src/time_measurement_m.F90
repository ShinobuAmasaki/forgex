#ifdef IMPURE
#define pure
#endif

module forgex_time_measurement_m
   use, intrinsic :: iso_fortran_env, only: real64, stderr => error_unit
   use :: omp_lib
   implicit none

   real(real64) :: begin_s, last_s, end_s


contains

   pure subroutine time_begin()
#if defined(IMPURE) && defined(BENCH)
      implicit none

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

      write(stderr, *) end_s - last_s, " sec. :", trim(description)
      last_s = end_s
#endif
   end subroutine time_lap

end module forgex_time_measurement_m