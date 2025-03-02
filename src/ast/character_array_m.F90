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
      logical :: is_subtract = .false.
      integer :: seg_size = 0
   end type

   public :: character_array_t
   public :: character_string_to_array
   public :: parse_backslash_and_hyphen_in_char_array
   public :: parse_segment_width_in_char_array

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
   pure subroutine parse_backslash_and_hyphen_in_char_array(array, ierr)
      use :: forgex_parameters_m
      use :: forgex_error_m
      implicit none
      type(character_array_t), intent(inout), allocatable :: array(:)
      type(character_array_t), allocatable :: temp(:)
      integer, intent(inout) :: ierr

      integer :: i, k, siz
      logical :: is_already_subtraction_zone

      if (.not. allocated(array)) return
      if (size(array, dim=1) < 1) return 

      allocate(temp(size(array, dim=1)))

      k = 1 ! actual size counter to output.
      is_already_subtraction_zone = .false.

      ! Main loop
      do i = 1, size(array, dim=1) ! i is array's index
         if (1 < i .and. i < size(array,dim=1)) then
           
            ! Handling subtract expression
            if (.not. is_already_subtraction_zone) then
               if (array(i)%c == SYMBOL_HYPN .and. array(i+1)%c == SYMBOL_HYPN) then
                  temp(k:size(temp))%is_subtract = .true.
                  is_already_subtraction_zone = .true.
                  cycle
               end if
            else
               if (array(i)%c == SYMBOL_HYPN .and. array(i+1)%c == SYMBOL_HYPN) then
                  ierr = SYNTAX_ERR_MISPLACED_SUBTRACTION_OPERATOR
                  return
               end if
            end if

            if (array(i-1)%c == SYMBOL_HYPN .and. array(i)%c == SYMBOL_HYPN) then
               cycle
            end if
         end if

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


      !> This subroutine assigns the expected segment size from the character `c` of
   !> the current array element to its `seg_size`.
   pure subroutine parse_segment_width_in_char_array (array)
      use :: forgex_parameters_m
      use :: forgex_utf8_m
      use :: forgex_segment_m
      implicit none
      type(character_array_t), intent(inout) :: array(:)
      type(segment_t), allocatable :: seg(:)
      integer :: k, n

      n = 0
      do k = 1, size(array, dim=1)
         if (array(k)%is_escaped) then
            select case(array(k)%c)
            case (ESCAPE_T)
               n = width_of_segment(SEG_TAB)
            case (ESCAPE_N)
               n = width_of_segment(SEG_LF) + width_of_segment(SEG_CR)
            case (ESCAPE_R)
               n = width_of_segment(SEG_CR)
            case (ESCAPE_D)
               n = width_of_segment(SEG_DIGIT)
            case (ESCAPE_D_CAPITAL)
               allocate(seg(1))
               seg(1) = SEG_DIGIT
               call invert_segment_list(seg)
               n = total_width_of_segment(seg)

            case (ESCAPE_W)
               allocate(seg(4))
               seg(1) = SEG_LOWERCASE
               seg(2) = SEG_UPPERCASE
               seg(3) = SEG_DIGIT
               seg(4) = SEG_UNDERSCORE
               n = total_width_of_segment(seg)

            case (ESCAPE_W_CAPITAL)
               allocate(seg(4))
               seg(1) = SEG_LOWERCASE
               seg(2) = SEG_UPPERCASE
               seg(3) = SEG_DIGIT
               seg(4) = SEG_UNDERSCORE
               call invert_segment_list(seg)
               n = total_width_of_segment(seg)

            case (ESCAPE_S)
               n = 6
            case (ESCAPE_S_CAPITAL)
               allocate(seg(6))
               seg(1) = SEG_SPACE
               seg(2) = SEG_TAB
               seg(3) = SEG_CR
               seg(4) = SEG_LF
               seg(5) = SEG_FF
               seg(6) = SEG_ZENKAKU_SPACE
               call invert_segment_list(seg)
               n = total_width_of_segment(seg)
            case (SYMBOL_BSLH)
               n = 1
            case (SYMBOL_LCRB)
               n = 1
            case (SYMBOL_RCRB)
               n = 1
            case (SYMBOL_LSBK)
               n = 1
            case (SYMBOL_RSBK)
               n = 1
            case default
               n = INVALID_SEGMENT_SIZE
            end select
         else
            n = 1
         end if
         array(k)%seg_size = n
      end do

   end subroutine parse_segment_width_in_char_array

!=====================================================================!

#ifdef IMPURE
   subroutine dump_character_array_t_list(list)
      use :: iso_fortran_env, only: stderr => error_unit
      implicit none
      type(character_array_t), intent(in) :: list(:)
      
      character(:), allocatable :: fmt
      integer :: i

      fmt = "('|', a6, 1x, '|', l8, 1x, '|', l11 ,1x, '|', l9, 1x, '|', i7, 1x, '|')"
      write(stderr, '(a)') '+=========== character_array_t output ==============' 
      write(stderr, '(a)') '| chara | escaped | hyphenated | subtract |  size  |' 
      do i = 1, size(list, dim=1)
         write(stderr, fmt) list(i)%c, list(i)%is_escaped, list(i)%is_hyphenated, &
            list(i)%is_subtract, list(i)%seg_size
      end do
      write(stderr, '(a)') '+=======+=========+============+==========+========+' 

   end subroutine dump_character_array_t_list
#endif

end module forgex_character_array_m