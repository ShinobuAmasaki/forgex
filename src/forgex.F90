! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!
!! This file includes the API module of Forgex.
module forgex
!! The `forgex` module defines APIs of Forgex.
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: forgex_syntax_tree_m
   use :: forgex_nfa_m
   use :: forgex_lazy_dfa_m

   implicit none
   private

   public :: operator(.in.)
   public :: operator(.match.)
   public :: regex

   interface operator(.in.)
      !! Interface for user-defined operator of `.in.`
      module procedure :: in__matching
   end interface

   interface operator(.match.)
      !! Interface for user-defined operator of `.match.`
      module procedure :: match__matching
   end interface

   interface regex
      !! The generic name for the `regex` function implemented as `regex__matching`.
      module procedure :: regex__matching
   end interface

   ! Module variables
   type(nfa_t), target :: nfa

   type(dfa_t) :: dfa

   character(:), allocatable :: pattern_cache

contains


   function in__matching (pattern, str) result(res)
      !! The function implemented for the `.in.` operator.
      implicit none
      character(*), intent(in) :: pattern, str

      character(:), allocatable :: buff
      integer(int32) :: from, to
      logical :: res

      type(tree_t), pointer :: root
      type(tape_t) :: tape

      from = 0
      to = 0

      if (.not. allocated(pattern_cache)) call initialize_pattern_cache

      if (pattern /= pattern_cache .or. pattern == '') then
        !!@note We will add code later to handle the case where the cache string
        !! exists but the automatons are no longer there.

         buff = pattern
         root => build_syntax_tree(tape, buff)

#ifdef DEBUG
         call print_tree(root)
#endif
         call build_automaton(root, pattern)

         ! Once the NFA is constructed, forget the unnecessary syntax tree.
         call deallocate_tree()

      end if
      call dfa%matching(char(10)//str//char(10), from, to)

      call free_dlist

#ifdef DEBUG
      call nfa%print()
      call dfa%print()
#endif

      if (is_there_caret_at_the_top(pattern)) then
         from = from
      else
         from = from - 1
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         to = to - 2
      else
         to = to - 1
      end if

      ! res = .true.
      if (from  > 0 .and. to > 0) then
         res = .true.

      else
         res = .false.
      end if
   end function in__matching


   function match__matching(pattern, str) result(res)
       !! The function implemented for the `.match.` operator.
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32) :: from, to
      character(:), allocatable :: buff
      logical :: res

      type(tree_t), pointer :: root
      type(tape_t) :: tape

      from = 0
      to = 0

      ! If the pattern_cache variable haven't been initialized,
      ! allocate and assign the empty character
      if (.not. allocated(pattern_cache)) call initialize_pattern_cache

      ! If pattern is not equivalent to pattern_cache, build its syntax-tree and automatons.
      if (pattern /= pattern_cache .or. pattern == '') then
         !!@note We will add code later to handle the case where the cache string
         !! exists but the automatons are no longer there.

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

         ! build the syntax tree from buff and tape,
         ! and assign the result to root pointer
         root => build_syntax_tree(tape, buff)

#ifdef DEBUG
         call print_tree(root)
#endif

         call build_automaton(root, pattern)

         ! Once the NFA is constructed, forget the syntax tree, we don't need them anymore.
         call deallocate_tree()

      end if

      res = dfa%matching_exactly(str)

#ifdef DEBUG
         call nfa%print()
         call dfa%print()
#endif
   end function match__matching


   function regex__matching (pattern, str, length, from, to) result(res)
      !! The function implemented for the `regex` function.
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32), intent(inout), optional :: length
      integer(int32), intent(inout), optional :: from, to
      character(:), allocatable :: res

      character(:), allocatable :: buff
      integer(int32) :: from_l, to_l

      type(tree_t), pointer :: root
      type(tape_t) :: tape

      from_l = 0
      to_l = 0

      if (.not. allocated(pattern_cache)) call initialize_pattern_cache

      if (pattern /= pattern_cache .or. pattern == '') then
         !!@note We will add code later to handle the case where the cache string
         !! exists but the automatons are no longer there.

         buff = pattern
         root => build_syntax_tree(tape, buff)

#ifdef DEBUG
         call print_tree(root)
#endif
         call build_automaton(root, pattern)

         ! Once the NFA is constructed, forget the unnecessary syntax tree.
         call deallocate_tree()

      end if

      call dfa%matching(char(10)//str//char(10), from_l, to_l)

#ifdef DEBUG
      call nfa%print()
      call dfa%print()
#endif

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
         res = str(from_l:to_l)
         if (present(length)) length = to_l - from_l + 1
         if (present(from)) from = from_l
         if (present(to)) to = to_l
      else
         res = ''
         if (present(length)) length = 0
         if (present(from)) from = 0
         if (present(to)) to = 0
      end if
   end function regex__matching

!---------------------------------------------------------------------!
! Private procedures
!

   !> This function returns .true. if the pattern contains the caret character
   !> at the top that matches the beginning of a line.
   function is_there_caret_at_the_top(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff
      logical :: res

      buff = adjustl(pattern)
      if (len(buff) == 0) return

      res = buff(1:1) == '^'
   end function is_there_caret_at_the_top


   !> This funciton returns .true. if the pattern contains the doller character
   !> at the end that matches the ending of a line.
   function is_there_dollar_at_the_end(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff

      logical :: res

      buff = trim(pattern)
      if (len(buff) == 0) return

      res = buff(len_trim(buff):len_trim(buff)) == '$'
   end function is_there_dollar_at_the_end


   !> This subroutine initializes the `pattern_cache` variable that remembers
   !> the pattern of the previous matching.
   subroutine initialize_pattern_cache()
      implicit none
      pattern_cache = ''

      !! Without this initialization, the Intel's compiler `ifx` will complain
      !! about comparison with unallocated character variable.
   end subroutine initialize_pattern_cache


   !> This subroutine performs the common tasks for the three public procedures:
   !> freeing, initializing, and constructing the NFA and DFA.
   !> Also, an assignment to the `pattern_cache` variable is done here.
   subroutine build_automaton(syntax_root, pattern)
      implicit none
      type(tree_t), intent(in) :: syntax_root
      character(*), intent(in) :: pattern

      call nfa%free()
      call nfa%init()

      call nfa%build(syntax_root)

      ! Initialize DFA.
      call dfa%free()
      call dfa%init(nfa)

      ! Remember the pattern.
      pattern_cache = pattern
   end subroutine build_automaton


end module forgex
