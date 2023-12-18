module stack_m
   use, intrinsic :: iso_fortran_env, stderr=> error_unit
   implicit none
   private

   public :: stack_t

   integer, parameter, private :: STACK_UPPER_LIMIT = 1024
   integer, parameter, private :: STACK_LOWER_LIMIT = 0

   type :: stack_t
      integer(int32) :: num = 0
      integer(int32) :: stack(STACK_UPPER_LIMIT)  = 0
   contains
      procedure :: get_num
      procedure :: push
      procedure :: pop
      procedure :: peek
   end type

contains 

   function get_num(self) result(res)
      implicit none
      class(stack_t) :: self
      integer(int32) :: res

      res = self%num

   end function get_num

   subroutine push(self, item)
      implicit none
      class(stack_t) :: self
      integer(int32), intent(in) :: item

      if (self%num == STACK_UPPER_LIMIT) then
         write(stderr, *) "stack_m: stack overflow. Operation 'push' failed. "
         return
      end if

      self%num = self%num + 1
      self%stack(self%num) = item

   end subroutine push

   function peek(self) result(res)
      implicit none
      class(stack_t) :: self
      integer(int32) :: res

      res = -1 
      if (self%num == STACK_LOWER_LIMIT) then
         write(stderr, *) "stack_m: stack is empty."
         return
      end if

      res = self%stack(self%num)

   end function peek
   

   function pop(self) result(res)
      implicit none
      class(stack_t) :: self
      integer(int32) :: res

      res = -1
      if (self%num == STACK_LOWER_LIMIT) then
         write(stderr, *) "stack_m: stack underflow. Operation 'pop' failed."
         return
      end if
      
      res = self%stack(self%num)
      self%num = self%num - 1
   
   end function pop

end module stack_m