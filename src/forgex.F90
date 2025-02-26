! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex module is a part of Forgex.
!
!! This file includes the API module of Forgex.
#ifdef IMPURE
#define elemental
#define pure
#endif
module forgex
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_syntax_tree_optimize_m, only: get_prefix_literal, get_suffix_literal, get_entire_literal
   use :: forgex_automaton_m, only: automaton_t
   use :: forgex_api_internal_m, only: do_matching_exactly, do_matching_including
   use :: forgex_utility_m, only: is_there_caret_at_the_top, is_there_dollar_at_the_end
   implicit none
   private

   public :: is_valid_regex
   public :: operator(.in.)
   public :: operator(.match.)
   public :: regex
   public :: regex_f

   interface is_valid_regex
      !! The generic name for the `is_valid_regex` function implemented as `is_valid_regex_pattern`.
      module procedure :: is_valid_regex_pattern
   end interface


   interface operator(.in.)
      !! Interface for user-defined operator of `.in.`
      module procedure :: operator__in
   end interface

   interface operator(.match.)
   !! Interface for user-defined operator of `.match.`
      module procedure :: operator__match
   end interface

   interface regex
      !! The generic name for the `regex` subroutine implemented as `procedure__regex`.
      module procedure :: subroutine__regex
   end interface

   interface regex_f
      !! The generic name for the `regex_f` function implemented as `function__regex`.
      module procedure :: function__regex
   end interface regex_f

contains

   pure elemental function is_valid_regex_pattern (pattern) result(res)
      !! The function validating a given regex patten.
      implicit none
      character(*), intent(in)  :: pattern
      logical                   :: res

      character(:), allocatable :: buff
      type(tree_t)              :: tree

      buff = trim(pattern)
      call tree%build(buff)

      res = tree%is_valid_pattern
   end function is_valid_regex_pattern


   pure elemental function operator__in(pattern, str) result(res)
      use :: forgex_parameters_m, only: ACCEPTED_EMPTY, INVALID_CHAR_INDEX
      !! The function implemented for the `.in.` operator.
      implicit none
      character(*), intent(in)  :: pattern, str
      logical                   :: res

      character(:), allocatable :: buff
      type(tree_t)              :: tree
      type(automaton_t)         :: automaton
      integer                   :: from, to

      character(:), allocatable :: prefix, suffix, entirely_fixed_string
      logical :: unused

      prefix = ''
      suffix = ''
      entirely_fixed_string = ''
      from = INVALID_CHAR_INDEX
      to = INVALID_CHAR_INDEX

      buff = trim(pattern)

      ! Build a syntax tree from buff, and store the result in tree and root.
      call tree%build(buff)

      ! Reterns .false. if the given pattern is invalid.
      if (.not. tree%is_valid_pattern) then
         res = .false.
         return
      end if

      ! If the whole pattern is a fixed string, get it.
      entirely_fixed_string = get_entire_literal(tree)
      
      ! If the pattern consists only of fixed character string,
      if (entirely_fixed_string /= '') then

         ! from_l stores the position of the literal in input text.
         from = index(str, entirely_fixed_string)

          ! If the literal is included,
         if (from > 0) then
            ! to_l stores the position of the end of the literal in the text.
            to = from + len(entirely_fixed_string) -1
         end if

         ! If the pattern is contained, it returns true, otherwise it returns false.
         if (from > 0 .and. to > 0) then
            res = .true.
         else
            res = .false.
         end if
   
         return
      end if

    != From here on, we will deal with cases where an entire pattern is not a fixed string.

      ! Extract a prefix and a suffix from the tree.
      prefix = get_prefix_literal(tree)
      suffix = get_suffix_literal(tree)

      ! Initialize automaton with tree.
      call automaton%preprocess(tree)
      call automaton%init()

      ! Call the internal procedure to match string, and store the result in logical `res`.
      call do_matching_including(automaton, str, from, to, prefix, suffix, unused)
   
       ! Handle when it matches an empty string.
      if (from == ACCEPTED_EMPTY .and. to == ACCEPTED_EMPTY) then
         res = .true.
         return
      end if

      ! If the pattern is contained in the text, it returns true, otherwise it returns false.
      if (from > 0 .and. to > 0) then
         res = .true.
      else
         res = .false.
      end if

      ! Free the automaton instance.
      call automaton%free()
   end function operator__in


   pure elemental function operator__match(pattern, str) result(res)
      !! The function implemented for the `.match.` operator.
      implicit none
      character(*), intent(in)  :: pattern, str
      logical                   :: res

      character(:), allocatable :: buff
      type(tree_t)              :: tree
      type(automaton_t)         :: automaton
      character(:), allocatable :: prefix, suffix, entirely_fixed_string
      logical :: unused

      ! Initalize
      prefix = ''
      suffix = ''
      entirely_fixed_string = ''

      ! If the pattern begins with a caret character and ends with
      ! a doller character, they are removed and assigned to the string buffer.
      if (is_there_caret_at_the_top(pattern)) then
         buff = pattern(2:len(pattern))
      else
         buff = pattern(1:len(pattern))
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         buff = buff(1:len_trim(pattern)-1)
      end if

      ! Build a syntax tree from buff, and store the result in tree and root.
      ! call build_syntax_tree(buff, tape, tree, root)
      call tree%build(buff)

      ! Reterns .false. if the given pattern is invalid.
      if (.not. tree%is_valid_pattern) then
         res = .false.
         return
      end if

      ! If the whole pattern is a fixed string, get it.
      entirely_fixed_string = get_entire_literal(tree)

      ! If the pattern consists only of fixed character string,
      if (entirely_fixed_string /= '') then
         if (len(str) == len(entirely_fixed_string)) then
            ! return true if the lengths are equal and the strings are equal.
            res = str == entirely_fixed_string
            return
         end if
      end if

   != From here on, we will deal with cases where an entire pattern is not a fixed string.
      
      ! Get the prefix contained in the AST.
      prefix = get_prefix_literal(tree)
      suffix = get_suffix_literal(tree)

      ! Initialize automaton with tree and root.
      call automaton%preprocess(tree)
      call automaton%init()

      ! Call the internal procedure to match string, and store the result in logical `res`.
      call do_matching_exactly(automaton, str, res, prefix, suffix, unused)

      ! Free the automaton instance.
      call automaton%free()

   end function operator__match


   !> The function implemented for the `regex` subroutine.
   pure subroutine subroutine__regex(pattern, text, res, length, from, to, status, err_msg)
      use :: forgex_parameters_m, only: ACCEPTED_EMPTY, INVALID_CHAR_INDEX
      use :: forgex_syntax_tree_error_m, only: get_error_message
      implicit none
      character(*),              intent(in)    :: pattern, text
      character(:), allocatable, intent(inout) :: res
      integer, optional,         intent(inout) :: length, from, to, status
      character(*), optional,    intent(inout) :: err_msg

      character(:),      allocatable :: buff
      type(tree_t)                   :: tree
      type(automaton_t)              :: automaton
      integer                        :: from_l, to_l

      character(:), allocatable :: prefix, suffix, entirely_fixed_string
      logical :: unused

      ! Initialize
      prefix = ''
      suffix = ''
      entirely_fixed_string = ''
      from_l = INVALID_CHAR_INDEX
      to_l = INVALID_CHAR_INDEX

      buff = trim(pattern)

      ! Build tree from regex pattern in the buff variable.
      call tree%build(buff)

      ! Assigns invalid value of arguments if the given pattern is invalid.
      if (.not. tree%is_valid_pattern) then
         res = ""
         if (present(length)) length = 0
         if (present(from))   from = INVALID_CHAR_INDEX
         if (present(to))     to = INVALID_CHAR_INDEX
         if (present(err_msg)) err_msg = get_error_message(tree%code)
         if (present(status)) status = tree%code
         return
      end if

      ! If the whole pattern is a fixed string, get it.
      entirely_fixed_string = get_entire_literal(tree)

      ! If the pattern consists only of fixed character string,
      if (entirely_fixed_string /= '') then

         ! from_l stores the position of the literal in input text.
         from_l = index(text, entirely_fixed_string)

         ! If the literal is included, 
         if (from_l > 0) then
            ! to_l stores the position of the end of the literal in the text.
            to_l = from_l + len(entirely_fixed_string) -1
         end if

         ! If the pattern is contained,
         if (from_l > 0 .and. to_l > 0) then
            ! assign the value of the local variable to the argument,
            if (present(from)) from = from_l
            if (present(to)) to = to_l
            if (present(length)) length = len(entirely_fixed_string)
            res = text(from_l:to_l)
         else
            ! otherwise return the empty string.
            if (present(from)) from = 0
            if (present(to)) to = 0
            if (present(length)) length = 0
            res = ''
         end if
         return
      end if

   != From here on, we will deal with cases where an entire pattern is not a fixed string.

      ! Extract a prefix and a suffix from the tree.
      prefix = get_prefix_literal(tree)
      suffix = get_suffix_literal(tree)

      ! Initialize automaton
      call automaton%preprocess(tree)
      call automaton%init()

      ! Perform a match for whether a pattern is included. 
      call do_matching_including(automaton, text, from_l, to_l, prefix, suffix, unused)

      ! Handle when it matches an empty string.
      if (from_l == ACCEPTED_EMPTY .and. to_l == ACCEPTED_EMPTY) then
         res = ''
         if (present(from)) from = 0
         if (present(to)) to = 0
         if (present(length)) length = 0
         return
      end if

      ! Process if the pattern is contained in text,
      if (from_l > 0 .and. to_l > 0) then
         res = text(from_l:to_l)
         if (present(length)) length = to_l - from_l + 1
         if (present(from)) from = from_l
         if (present(to)) to = to_l
      else
         ! otherwise return an empty string and 0. 
         res = ''
         if (present(length)) length = 0
         if (present(from)) from = 0
         if (present(to)) to = 0
      end if

      ! Free the automaton instance.
      call automaton%free()
   end subroutine subroutine__regex


   !> The function implemented for the `regex_f` function.
   pure function function__regex(pattern, text) result(res)
      implicit none
      character(*), intent(in)  :: pattern, text
      character(:), allocatable :: res

      call subroutine__regex(pattern, text, res)

   end function function__regex

end module forgex