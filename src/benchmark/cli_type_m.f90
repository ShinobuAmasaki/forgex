! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_type_m module is a part of Forgex.
!
module forgex_cli_type_m
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

   type, public :: cmd_t  ! command type
      character(LEN_CMD), private :: name = ''
      character(LEN_CMD), allocatable :: subc(:)   ! sub-command
   contains
      procedure :: get_name => cmd__get_name
      procedure :: set_name => cmd__set_name
   end type cmd_t

   ! option flags, such as '--help', '-h'
   type, public :: flag_t
      character(32) :: name
      character(:), allocatable :: long_f, short_f
   end type flag_t

contains

   pure function cmd__get_name(self) result(res)
      implicit none
      class(cmd_t), intent(in) :: self
      character(:), allocatable :: res

      res = trim(self%name)
   end function cmd__get_name

   pure subroutine cmd__set_name(self, name)
      implicit none
      class(cmd_t), intent(inout) :: self
      character(*), intent(in) :: name

      self%name = name
   end subroutine cmd__set_name


end module forgex_cli_type_m