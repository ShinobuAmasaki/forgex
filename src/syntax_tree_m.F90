! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!    `forgex_syntax_tree_m` module is a part of Forgex.
!
!! This file defines syntactic parsing.

module forgex_syntax_tree_m
   use, intrinsic :: iso_fortran_env
   use :: forgex_parameters_m
   use :: forgex_segment_m
   implicit none
   private

   public :: tree_node_t

   character(UTF8_CHAR_SIZE), parameter, public :: EMPTY = char(0)

   type :: tree_node_t
      integer(int32) :: op
      type(segment_t), allocatable :: c(:)
      integer(int32) :: left, right
   end type

   type :: tape_t
      character(:), allocatable :: str
      integer(int32) :: current_token
      character(UTF8_CHAR_SIZE) :: token_char = EMPTY
   end type

contains

   !> Copies the input pattern to `tape_t` type and builds a syntax tree.
   !> The result returns a pointer to the root of the tree.
   !> Expected to be used by the forgex module.
   pure subroutine build_syntax_tree(str, tape, tree, idx)
      implicit none
      character(*), intent(in)    :: str
      type(tape_t), intent(inout) :: tape
      type(tree_node_t), allocatable, intent(inout) :: tree(:)
      integer(int32), intent(inout) :: idx

      allocate(tree(0:TREE_MAX_SIZE)) ! 0-based 

   end subroutine build_syntax_tree

   
   pure subroutine deallocate_tree(tree)
      implicit none
      type(tree_node_t), allocatable, intent(inout) :: tree

      if (allocated(tree)) deallocate(tree)
   end subroutine deallocate_tree


   pure subroutine get_token(self, idx, class_flag)
      use :: forgex_enums_m
      use :: forgex_utf8_m
      implicit none
      class(tape_t), intent(inout) :: self
      logical, optional, intent(in) :: class_flag
      integer(int32), intent(inout) :: idx 

      character(UTF8_CHAR_SIZE) :: c
      integer(int32) :: ib, ie

      ib = idx
      if (ib > len(self%str)) then
         self%current_token = tk_end
         self%token_char = ''
      else

         ie = idxutf8(self%str, ib)
         
         c = self%str(ib:ie)

         if (present(class_flag)) then
            if (class_flag) then
               select case (trim(c))
               case (SYMBOL_RSBK)
                  self%current_token = tk_rsbracket
               case (SYMBOL_HYPN)
                  self%current_token = tk_hyphen
                  self%token_char = c
               case default
                  self%current_token = tk_char
                  self%token_char = c
               end select
            end if
         else
            select case(trim(c))
            case (SYMBOL_VBAR)
               self%current_token = tk_union
            case (SYMBOL_LPAR)
               self%current_token = tk_lpar
            case (SYMBOL_RPAR)
               self%current_token = tk_rpar
            case (SYMBOL_STAR)
               self%current_token = tk_star
            case (SYMBOL_PLUS)
               self%current_token = tk_plus
            case (SYMBOL_QUES)
               self%current_token = tk_question
            case (SYMBOL_BSLH)
               self%current_token = tk_backslash

               ib = ie +1
               ie = idxutf8(self%str, ib)

               self%token_char = self%str(ib:ie)
            case (SYMBOL_LSBK)
               self%current_token = tk_lsbracket
            case (SYMBOL_RSBK)
               self%current_token = tk_rsbracket
            case (SYMBOL_LCRB)
               self%current_token = tk_lcurlybrace
            case (SYMBOL_RCRB)
               self%current_token = tk_rcurlybrace
            case (SYMBOL_DOT)
               self%current_token = tk_dot
            case (SYMBOL_CRET)
               self%current_token = tk_caret
            case (SYMBOL_DOLL)
               self%current_token = tk_dollar
            case default
               self%current_token = tk_char
               self%token_char = c
            end select
         end if

         idx = ie + 1

      end if
   end subroutine get_token

end module forgex_syntax_tree_m
