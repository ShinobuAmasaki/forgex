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

   type, public :: subsubc_t
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

end module forgex_cli_type_m