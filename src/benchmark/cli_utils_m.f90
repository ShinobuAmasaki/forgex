! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_utils_m module is a part of Forgex.
!
module forgex_cli_utils_m
   use, intrinsic :: iso_fortran_env, only: int32, real64, stderr => error_unit
   use :: forgex_cli_parameters_m, only: LEN_ENV_VAR, NUM_FLAGS, INVALID_FLAG
   use forgex_cli_type_m, only: arg_element_t, flag_t, subc_t, subsubc_t
   implicit none
   private
   public :: right_justify

   public :: operator(.in.)
   interface operator(.in.)
      module procedure :: does_flag_exist
      module procedure :: does_subcommand_exist
      module procedure :: does_sub_subcommand_exist
      module procedure :: is_arg_contained_in_flags
   end interface

   public :: get_arg_command_line
   public :: get_flag_index
   public :: register_flag
   public :: register_subc
   public :: get_os_type
   public :: is_there_caret_at_the_top
   public :: is_there_dollar_at_the_end

contains

   function get_os_type() result(res)
      use :: forgex, only: operator(.in.)
      use :: forgex_enums_m
      implicit none
      integer :: res
      integer, save :: res_save
      logical, save :: is_first = .true.
      
      character(LEN_ENV_VAR) :: val1, val2
      integer :: len1, len2, stat1, stat2

      if (.not. is_first) then
         res = res_save
         return
      end if

      res = OS_UNKNOWN

      call get_environment_variable(name='OS', value=val1, length=len1, status= stat1)

      if (stat1 == 0 .and. len1 > 0) then
         if ("Windows_NT" .in. val1) then
            res_save = OS_WINDOWS
            res = res_save
            is_first = .false.
            return
         end if
      end if

      call get_environment_variable(name='OSTYPE', value=val2, length=len2, status= stat2)
      if (stat2 == 0 .and. len2 > 0) then
         !! @todo 
      end if
   end function get_os_type


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
         res = res .or. trim(arg) == trim(subsubc_list(i)%name)
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


   subroutine right_justify(array)
      use :: forgex_cli_parameters_m, only: NUM_DIGIT_KEY
      implicit none
      character(NUM_DIGIT_KEY), intent(inout) :: array(:)

      character(NUM_DIGIT_KEY), allocatable :: buff(:)
      integer :: i, max_len


      allocate(buff(size(array, dim=1)))
      buff(:) = array(:)

      max_len = 0
      do i = 1, size(buff)
         max_len = max(max_len, len_trim(adjustl(buff(i))))
      end do

      ! right justify
      do i = 1, size(buff)
         buff(i) = adjustl(array(i))
         buff(i) = repeat(' ', max_len- len_trim(buff(i)))// buff(i)
      end do

      array(:) = buff(:)
   end subroutine

   !> This function returns .true. if the pattern contains the caret character
   !> at the top that matches the beginning of a line.
   pure function is_there_caret_at_the_top(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff
      logical :: res

      res = .false.

      buff = adjustl(pattern)
      if (len(buff) == 0) return

      res = buff(1:1) == '^'
   end function is_there_caret_at_the_top


   !> This funciton returns .true. if the pattern contains the doller character
   !> at the end that matches the ending of a line.
   pure function is_there_dollar_at_the_end(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff

      logical :: res

      res = .false.
      
      buff = trim(pattern)
      if (len(buff) == 0) return

      res = buff(len_trim(buff):len_trim(buff)) == '$'
   end function is_there_dollar_at_the_end


end module forgex_cli_utils_m