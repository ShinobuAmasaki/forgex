program main
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex
   implicit none

   character(:), allocatable :: pattern



   pattern = 'h[a-z]*d'

   print *, pattern .match. "hzd"


 

end program main