module forgex_cli_type_m
   use, intrinsic :: iso_fortran_env, only: int32, real64, stderr => error_unit
   use :: forgex_cli_parameters_m
   implicit none
   private

   type, public :: arg_element_t
      character(:), allocatable :: v
   end type arg_element_t

   type, public :: arg_t
      integer :: argc
      type(arg_element_t), allocatable :: arg(:)
      character(:), allocatable :: entire
   end type arg_t

   type, public :: pattern_t
      character(:), allocatable :: p
   end type pattern_t

   type :: subsubc_t
      character(16) :: name = ''
   end type subsubc_t

   type, public :: subc_t  ! sub command type
      character(16) :: name = ''
      type(subsubc_t), allocatable :: subsubcmd(:)
   end type subc_t

   ! option flags, such as '--help', '-h'
   type, public :: flag_t
      character(32) :: name
      character(:), allocatable :: long_f, short_f
   end type flag_t

   type(flag_t), public :: all_flags(NUM_FLAGS)
   type(subc_t), public :: all_sub_cmds(NUM_SUB_C)

   public :: get_arg_command_line
   public :: get_flag_index
   public :: operator(.in.)
   public :: register_flag
   public :: register_subc


   interface operator(.in.)
      module procedure :: does_flag_exist
      module procedure :: does_subcommand_exist
      module procedure :: does_sub_subcommand_exist
      module procedure :: is_arg_contained_in_flags
   end interface



contains

   function get_flag_index(arg, flags) result(res)
      implicit none
      type(arg_element_t), intent(in) :: arg
      type(flag_t), intent(in) :: flags(:)
      integer :: res
      integer :: i

      res = -1
      do i = 1, NUM_FLAGS
         if (arg%v == flags(i)%long_f .or. arg%v == flags(i)%short_f) then
            res = i
            return
         end if
      end do

   end function get_flag_index


   function is_arg_contained_in_flags(arg, flags) result(res)
      implicit none
      type(arg_element_t), intent(in) :: arg
      type(flag_t), intent(in) :: flags(:)
      logical :: res

      integer :: i

      res = .false.
      do i = 1, ubound(flags, dim=1)
         res = res  &
                  .or. flags(i)%long_f == arg%v &
                  .or. flags(i)%short_f == arg%v
         if (res) return
      end do
   end function is_arg_contained_in_flags


   subroutine get_arg_command_line(argc, arg, entire)
      implicit none
      integer(int32), intent(inout) :: argc  ! argc
      type(arg_element_t), allocatable, intent(inout) :: arg(:)
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



   subroutine register_flag(flag, name, long, short)
      implicit none
      type(flag_t), intent(inout) :: flag
      character(*), intent(in) :: name
      character(*), intent(in) :: long
      character(*), intent(in), optional :: short

      flag%name = name
      flag%long_f = long
      if (present(short)) then
         flag%short_f = short
      else
         flag%short_f = INVALID_FLAG
      end if
   end subroutine


   subroutine register_subc(subc, name)
      implicit none
      type(subc_t), intent(inout) :: subc
      character(*), intent(in) :: name

      subc%name = name
   end subroutine register_subc


end module forgex_cli_type_m