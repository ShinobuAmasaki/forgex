! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_type_m module is a part of Forgex.
!
module forgex_cli_type_m
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

   type, public :: sub_subc_t
      character(16) :: name = ''
   end type sub_subc_t

   type, public :: subc_t
      character(16) :: name = ''
      type(sub_subc_t), allocatable :: subsub(:)
   end type subc_t

   type, public :: cmd_t  ! command type
      character(16) :: name = ''
      type(subc_t), allocatable :: sub_cmd(:)   ! sub commands
   end type cmd_t

   ! option flags, such as '--help', '-h'
   type, public :: flag_t
      character(32) :: name
      character(:), allocatable :: long_f, short_f
   end type flag_t

end module forgex_cli_type_m