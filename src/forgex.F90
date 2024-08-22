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
   use :: forgex_syntax_tree_optimize_m, only: get_prefix_literal, get_postfix_literal, get_entire_literal
   use :: forgex_automaton_m, only: automaton_t
   use :: forgex_api_internal_m, only: do_matching_exactly, do_matching_including
   use :: forgex_utility_m, only: is_there_caret_at_the_top, is_there_dollar_at_the_end
   implicit none
   private

   public :: operator(.in.)
   public :: operator(.match.)
   public :: regex
   public :: regex_f

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

   pure elemental function operator__in(pattern, str) result(res)
      use :: forgex_parameters_m, only: ACCEPTED_EMPTY, INVALID_CHAR_INDEX
      !! The function implemented for the `.in.` operator.
      implicit none
      character(*), intent(in)       :: pattern, str
      logical                        :: res

      character(:),      allocatable :: buff
      type(tree_t)                   :: tree
      type(automaton_t)              :: automaton
      integer                        :: from, to

      character(:), allocatable :: prefix, postfix, entirely_fixed_string
      logical :: unused

      prefix = ''
      postfix = ''
      entirely_fixed_string = ''
      from = INVALID_CHAR_INDEX
      to = INVALID_CHAR_INDEX

      buff = trim(pattern)

      ! Build a syntax tree from buff, and store the result in tree and root.
      ! call build_syntax_tree(buff, tape, tree, root)
      call tree%build(buff)

      entirely_fixed_string = get_entire_literal(tree)
      if (entirely_fixed_string /= '') then
         from = index(str, entirely_fixed_string)
         if (from > 0) then
            to = from + len(entirely_fixed_string) -1
         end if
         if (from > 0 .and. to > 0) then
            res = .true.
         else
            res = .false.
         end if
         return
      end if

      prefix = get_prefix_literal(tree)
      postfix = get_postfix_literal(tree)
   
      call automaton%preprocess(tree)

      ! Initialize automaton with tree and root.
      call automaton%init()

      ! Call the internal procedure to match string, and store the result in logical `res`.
      call do_matching_including(automaton, char(0)//str//char(0), from, to, prefix, postfix, unused)
         ! キャレットとダラーへの対応するために、strの前後に改行文字を追加する。

      if (from == ACCEPTED_EMPTY .and. to == ACCEPTED_EMPTY) then
         res = .true.
         return
      end if

      if (is_there_caret_at_the_top(pattern)) then
         from = from
      else
         from = from -1
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         to = to - 2
      else
         to = to - 1
      end if

      if (from > 0 .and. to > 0) then
         res = .true.
      else
         res = .false.
      end if

      call automaton%free()
   end function operator__in


   pure elemental function operator__match(pattern, str) result(res)
      !! The function implemented for the `.match.` operator.
      implicit none
      character(*), intent(in)       :: pattern, str
      logical                        :: res

      character(:),      allocatable :: buff
      type(tree_t)                   :: tree
      type(automaton_t)              :: automaton
      character(:), allocatable  :: prefix, postfix, entirely_fixed_string
      logical :: unused


      prefix = ''
      postfix = ''
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

      entirely_fixed_string = get_entire_literal(tree)
      if (entirely_fixed_string /= '') then
         if (len(str) == len(entirely_fixed_string)) then
            res = str == entirely_fixed_string
            return
         end if
      end if

      prefix = get_prefix_literal(tree)
      ! postfix = get_postfix_literal(tree)

      ! Initialize automaton with tree and root.
      call automaton%preprocess(tree)

      call automaton%init()

      ! Call the internal procedure to match string, and store the result in logical `res`.
      call do_matching_exactly(automaton, str, res, prefix, postfix, unused)

      call automaton%free()

   end function operator__match

   !> The function implemented for the `regex` subroutine.
   pure subroutine subroutine__regex(pattern, text, res, length, from, to)
      use :: forgex_parameters_m, only: ACCEPTED_EMPTY, INVALID_CHAR_INDEX
      implicit none
      character(*),              intent(in)    :: pattern, text
      character(:), allocatable, intent(inout) :: res
      integer, optional,         intent(inout) :: length, from, to

      character(:),      allocatable :: buff
      type(tree_t)                   :: tree
      type(automaton_t)              :: automaton
      integer                        :: from_l, to_l

      character(:), allocatable :: prefix, postfix, entirely_fixed_string
      logical :: unused

      prefix = ''
      postfix = ''
      entirely_fixed_string = ''
      from_l = INVALID_CHAR_INDEX
      to_l = INVALID_CHAR_INDEX
   
      buff = trim(pattern)

      ! call build_syntax_tree(buff, tape, tree, root)
      call tree%build(buff)


      entirely_fixed_string = get_entire_literal(tree)
      if (entirely_fixed_string /= '') then
         from_l = index(text, entirely_fixed_string)
         if (from_l > 0) then
            to_l = from_l + len(entirely_fixed_string) -1
         end if
         if (from_l > 0 .and. to_l > 0) then
            if (present(from)) from = from_l
            if (present(to)) to = to_l
            if (present(length)) length = len(entirely_fixed_string)
            res = text(from_l:to_l)
         else
            res = ''
         end if
         return
      end if

      prefix = get_prefix_literal(tree)
      postfix = get_postfix_literal(tree)

      call automaton%preprocess(tree)
      call automaton%init()

      call do_matching_including(automaton, char(0)//text//char(0), from_l, to_l, prefix, postfix, unused)

      if (from_l == ACCEPTED_EMPTY .and. to_l == ACCEPTED_EMPTY) then
         res = ''
         if (present(from)) from = 0
         if (present(to)) to = 0
         if (present(length)) length = 0
         return
      end if

      if (is_there_caret_at_the_top(pattern)) then
         from_l = from_l
      else
         from_l = from_l - 1
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         to_l = to_l - 2
      else
         to_l = to_l - 1
      end if


      if (from_l > 0 .and. to_l > 0) then
         res = text(from_l:to_l)
         if (present(length)) length = to_l - from_l + 1
         if (present(from)) from = from_l
         if (present(to)) to = to_l
      else
         res = ''
         if (present(length)) length = 0
         if (present(from)) from = 0
         if (present(to)) to = 0
      end if

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