! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_character_array_m module is a part of Forgex.
!
#ifdef IMPURE
#define pure
#endif
module forgex_character_array_m
   implicit none
   private

   type :: character_array_t
   !! This derived-type contains single UTF-8 character and two flags.
   !! It will be used to parse character class patterns enclosed in square brackets. 
   !! `is_escaped` is true when the character has preceding backslash.
   !! `is_hyphenated` is true when the character has following hyphen except for First character.
   !! `seg_size` is the number of segments the component represents, which is 1 for normal characters
   !! and greater than 1 for most shorthand escape sequences.
      character(:), allocatable :: c
      logical :: is_escaped = .false.
      logical :: is_hyphenated = .false.
      integer :: seg_size = 0
   end type

   public :: character_array_t
   public :: character_string_to_array
   public :: parse_backslash_and_hyphen_in_char_array
#ifdef IMPURE
   public :: dump_character_array_t_list
#endif

contains

   !> This subroutine parses a pattern string for character class,
   !> and outputs `character_array_t` type array.
   !> When it encounters invalid value along the way, it returns.
   pure subroutine character_string_to_array(str, array)
      use :: forgex_parameters_m, only: INVALID_CHAR_INDEX
      use :: forgex_utf8_m, only: len_utf8, idxutf8
      implicit none
      character(*), intent(in) :: str
      type(character_array_t), intent(inout), allocatable :: array(:)

      integer :: siz, ib, ie, j

      siz = len_utf8(str)
      if (siz < 1) return

      if (allocated(array)) deallocate(array)
      allocate(array(siz))

      ib = 0
      ie = 0
      do j = 1, siz
         ib = ie + 1
         ie = idxutf8(str, ib)
         if (ib == INVALID_CHAR_INDEX .or. ie == INVALID_CHAR_INDEX) return
         array(j)%c = str(ib:ie)
      end do

   end subroutine character_string_to_array
      
   !> This subroutine processes a character array, and outputs the corresponding
   !> flagged array. It removes backslash and hyphen characters, and then flags 
   !> the current element in `character_array_t` type array.
   pure subroutine parse_backslash_and_hyphen_in_char_array(array)
      use :: forgex_parameters_m
      implicit none
      type(character_array_t), intent(inout), allocatable :: array(:)
      type(character_array_t), allocatable :: temp(:)
      integer :: i, k, siz

      if (.not. allocated(array)) return
      if (size(array, dim=1) < 1) return 

      allocate(temp(size(array, dim=1)))

      k = 1 ! actual size counter to output.

      ! Main loop
      do i = 1, size(array, dim=1) ! i is array's index

         if (array(i)%c == SYMBOL_BSLH .and. .not. temp(k)%is_escaped) then
            ! If the current character is backslash
            ! except the `is_escaped` of `temp(k)` is true.
            temp(k)%is_escaped = .true.
         
         else if (array(i)%c == SYMBOL_HYPN .and. .not. i == 1) then
            ! If the current character is hyphen,
            ! except for the first character. 
            temp(k-1)%is_hyphenated = .true.
         
         else
            ! For characters has no special meaning.
            temp(k)%c = array(i)%c
            k = k + 1
         end if
      end do

      ! Copy from local array to the arguemnt array.
      siz = k - 1
      if (allocated(array)) deallocate(array)
      allocate(array(siz))
      array(:) = temp(1:siz)
      
   end subroutine parse_backslash_and_hyphen_in_char_array

!=====================================================================!

#ifdef IMPURE
   subroutine dump_character_array_t_list(list)
      use :: iso_fortran_env, only: stderr => error_unit
      implicit none
      type(character_array_t), intent(in) :: list(:)

      integer :: i

      write(stderr, *) '== character_array_t output ==' 
      write(stderr, *) '====+====+====+====+====+====+====+' 
      do i = 1, size(list, dim=1)
         write(stderr, *) list(i)%c, list(i)%is_escaped, list(i)%is_hyphenated, list(i)%seg_size
      end do
      write(stderr, *) '====+====+====+====+====+====+====+' 

   end subroutine dump_character_array_t_list
#endif

end module forgex_character_array_m