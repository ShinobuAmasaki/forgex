module c_func_debug_m
   use, intrinsic :: iso_c_binding
   implicit none
   private

#ifdef DEBUG

   public :: message
   public :: message_char


   interface
      pure subroutine message(i, j) bind(c)
         import c_int
         implicit none
         integer(c_int), intent(in), value :: i, j
      end subroutine message
   end interface

   interface
      pure subroutine message_char(i, str) bind(c)
         import c_int, c_char
         implicit none
         integer(c_int), intent(in), value :: i
         character(1, kind=c_char), intent(in) :: str(*)
      end subroutine
   end interface
#endif

end module c_func_debug_m