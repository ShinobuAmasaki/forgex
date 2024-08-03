!! This program provides a simple example to check the debug message display.
program main
   use :: forgex
   implicit none
   
   character(:), allocatable :: pattern, text

   pattern = "0[a-z]+b"
   text = "0abcdb"

   print *, pattern .match. text    !=> T
end program main