module forgex_cli_type_m
   use, intrinsic :: iso_fortran_env, only: int32, stderr => error_unit
   implicit none
   private

   public :: get_arg_command_line
   public :: operator(.in.)
   public :: register_flag
   public :: register_subc


   integer, parameter, public :: NUM_FLAGS = 1
   integer, parameter, public :: NUM_SUB_C = 1

   type, public :: argv_t
      character(:), allocatable :: v
   end type argv_t

   type, public :: arg_t
      integer :: argc
      type(argv_t), allocatable :: arg(:)
      character(:), allocatable :: entire
   end type arg_t

   type :: subsubc_t
      character(16) :: name = ''
   end type subsubc_t

   type, public :: subc_t  ! sub command type
      character(16) :: name = ''
      type(subsubc_t), allocatable :: subsubc(:)
   end type subc_t

   ! option flags, such as '--help', '-h'
   type, public :: flag_t
      character(32) :: name
      character(:), allocatable :: long_f, short_f
   end type flag_t

   interface operator(.in.)
      module procedure :: does_flag_exist
      module procedure :: does_subcommand_exist
      module procedure :: does_sub_subcommand_exist
   end interface


contains

   subroutine get_arg_command_line(argc, arg, entire)
      implicit none
      integer(int32), intent(inout) :: argc  ! argc
      type(argv_t), allocatable, intent(inout) :: arg(:)
      character(:), allocatable, intent(inout) :: entire

      integer :: i, len_ith, entire_len

      argc = command_argument_count()

      call get_command(length=entire_len)
      allocate(character(entire_len) :: entire)
      call get_command(command=entire)
      
      allocate(arg(0:argc))

      do i = 0, argc
         ! Get length of i-th command line argmuemnt.
         call get_command_argument(number=i, length=len_ith)

         ! Allocate str(i)%v of the same length as the i-th argument.
         allocate (character(len_ith) :: arg(i)%v)

         ! Get the value of the i-th argument as a string.
         call get_command_argument(number=i, value=arg(i)%v)

      end do
   end subroutine get_arg_command_line


   pure function does_sub_subcommand_exist(arg, subsubc_list) result(res)
      implicit none
      character(*), intent(in) :: arg
      type(subsubc_t), intent(in) :: subsubc_list(:)
      logical :: res

      integer :: i

      res = .false.
      do i = lbound(subsubc_list, dim=1), ubound(subsubc_list, dim=1)
         res = res .or. trim(arg) == trim(subsubc_list (i)%name)
      end do
   end function does_sub_subcommand_exist


   pure function does_subcommand_exist(arg, subc_list) result(res)
      implicit none
      character(*), intent(in) :: arg
      type(subc_t), intent(in) :: subc_list(:)
      logical :: res
      integer :: i

      res = .false.
      do i = lbound(subc_list, dim=1), ubound(subc_list, dim=1)
         res = res .or. trim(arg) == trim(subc_list(i)%name)
         if (res) return
      end do
   end function does_subcommand_exist


   pure function does_flag_exist(arg, flag_list) result(res)
      implicit none
      character(*), intent(in) :: arg
      type(flag_t), intent(in) :: flag_list(:)
      logical :: res
      integer :: i
      res = .false.

      do i = lbound(flag_list, dim=1), ubound(flag_list, dim=1)
         res = res & 
                  .or. trim(arg) == trim(flag_list(i)%short_f) &
                  .or. trim(arg) == trim(flag_list(i)%long_f)
         if (res) return
      end do
   end function does_flag_exist
      


   subroutine register_flag(flag, name, long, short, has_value)
      implicit none
      type(flag_t), intent(inout) :: flag
      character(*), intent(in) :: name
      character(*), intent(in) :: long, short
      logical, intent(in) :: has_value

      flag%name = name
      flag%long_f = long
      flag%short_f = short
   end subroutine


   subroutine register_subc(subc, name)
      implicit none
      type(subc_t), intent(inout) :: subc
      character(*), intent(in) :: name

      subc%name = name
   end subroutine register_subc

end module forgex_cli_type_m