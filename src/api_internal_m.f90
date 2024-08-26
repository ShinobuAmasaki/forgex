! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_api_internal_m module is a part of Forgex.
!
!! This file defines the back-end processing of the APIs.

!> The `forgex_api_internal_m` defines the procedures that the API call directly.
!> Currently, it contains two procedures: `do_matching_including` and `do_matching_exactly`.
#ifdef IMPURE
#define pure
#endif
module forgex_api_internal_m
   use, intrinsic :: iso_fortran_env, only: stderr => error_unit
   use :: forgex_parameters_m, only: DFA_NOT_INIT, DFA_INVALID_INDEX
   use :: forgex_automaton_m, only: automaton_t
   use :: forgex_utf8_m, only: idxutf8
   implicit none
   private

   public :: do_matching_including
   public :: do_matching_exactly

contains

   !> This procedure reads a text, performs regular expression matching using an automaton,
   !> and stores the string index in the argument if it contains a match.
   pure subroutine do_matching_including (automaton, string, from, to, prefix, suffix, runs_engine)
      use :: forgex_utility_m, only: get_index_list_forward
      use :: forgex_parameters_m, only: INVALID_CHAR_INDEX, ACCEPTED_EMPTY
      implicit none
      type(automaton_t), intent(inout) :: automaton
      character(*),      intent(in)    :: string
      integer,           intent(inout) :: from, to
      character(*),      intent(in)    :: prefix, suffix
      logical,           intent(inout) :: runs_engine

      integer :: cur_i, dst_i ! current and destination index of DFA nodes
      integer :: ci           ! character index
      integer :: next_ci      ! next character index
      integer :: max_match    ! maximum value of match attempts
      integer :: start        ! starting character index
      integer :: i
      integer :: suf_idx      ! right-most suffix index
      character(:), allocatable :: str
      integer, allocatable :: index_list(:)
      logical :: do_brute_force

      do_brute_force = .false.
      runs_engine = .false.

      str = char(0)//string//char(0)

      from = 0
      to = 0
      do_brute_force = prefix == ''
      suf_idx = INVALID_CHAR_INDEX

      cur_i = automaton%initial_index

      if (cur_i == DFA_NOT_INIT) then
         error stop "DFA have not been initialized."
      end if

      if (len(string) <= 1 .and. string == '') then
         if (automaton%dfa%nodes(cur_i)%accepted) then
            from = ACCEPTED_EMPTY
            to = ACCEPTED_EMPTY
         end if
         return
      end if

      if (.not. do_brute_force) then
         call get_index_list_forward(str, prefix, suffix, index_list)
         if (.not. allocated(index_list)) return
         if (index_list(1) == INVALID_CHAR_INDEX) then
            do_brute_force = .true.
         end if
      end if

      loop_init: block
         if (do_brute_force) then
            i = 1
            start = i
         else
            ! indexリストの先頭が2の場合、NULL文字を考慮してstart=1, i=0にする。
            if (index_list(1) == 2) then
               start = 1
               i = 0
            else
               i = 1
               start = index_list(i)
            end if

            if (suffix /= '') then
               suf_idx = index(string, suffix, back=.true.)
               if (suf_idx == 0) return
            end if

         end if
      end block loop_init


      do while (start < len(str))
         max_match = 0
         ci = start
         cur_i = automaton%initial_index
         runs_engine = .true.

         if (suf_idx /= INVALID_CHAR_INDEX) then
            if (suf_idx < ci) exit
         end if

         ! Traverse the DFA with the input string from the current starting position of ``cur_i`.
         do while (cur_i /= DFA_INVALID_INDEX)

            if (automaton%dfa%nodes(cur_i)%accepted .and. ci /= start) then
               max_match = ci
            end if

            if (ci > len(str)) exit

            next_ci = idxutf8(str, ci) + 1

            call automaton%construct(cur_i, dst_i, str(ci:next_ci-1))

            cur_i = dst_i
            ci = next_ci
         end do

         ! Update match position if a match is found.
         if (max_match > 0) then
            from = start-1
            if (from == 0) from = 1 ! handle leading NULL character.
            if (max_match >= len(str)) then
               to = len(string)
            else
               to = max_match-2
            end if
            return
         end if

         if (do_brute_force) then
            start = idxutf8(str, start) + 1 ! Bruteforce searching
            cycle
         endif

         i = i + 1
         if (i <= size(index_list)) then
            start = index_list(i)
            if (start == INVALID_CHAR_INDEX) return
         else
            return
         end if
      end do


   end subroutine do_matching_including


   !> This subroutine is intended to be called from the `forgex` API module.
   pure subroutine do_matching_exactly(automaton, string, res, prefix, suffix, runs_engine, entire_fixed_string)
      implicit none
      type(automaton_t),      intent(inout) :: automaton
      character(*),           intent(in)    :: string
      logical,                intent(inout) :: res
      character(*),           intent(in)    :: prefix, suffix
      logical,                intent(inout) :: runs_engine
      character(*), optional, intent(inout) :: entire_fixed_string

      integer :: cur_i, dst_i ! current and destination index of DFA nodes
      integer :: ci           ! character index
      integer :: next_ci      ! next character index
      integer :: max_match    !
      character(:), allocatable :: str

      integer :: len_pre, len_suf, n
      logical :: empty_pre, empty_post, matches_pre, matches_post

      runs_engine = .false.

      if (present(entire_fixed_string)) then
         if (entire_fixed_string /= '') then
            res = entire_fixed_string == string
            return
         end if
      end if

      len_pre = len(prefix)
      len_suf = len(suffix)
      n = len(string)
      matches_pre = .true.
      matches_post = .true.

      ! Returns true immediately if the given prefix exactly matches the string.
      if (len(string) > 0 .and. len(prefix) >0 ) then
         if (prefix == string .and. len_pre == n) then
            res = .true.
            return
         end if
      end if
      
      ! Returns false if the prefix or suffix is ​​longer than the input string.
      if (len_pre > len(string) .or. len_suf > len(string)) then
            res = .false.
            return
      end if
      
      empty_pre   = prefix == ''
      empty_post  = suffix == ''

      if (.not. empty_pre) matches_pre = string(1:len_pre) == prefix
      if (.not. empty_post) matches_post = string(n-len_suf+1:n) == suffix

      runs_engine = any([(matches_pre .and. matches_post), &
                         (empty_pre .and. matches_post), &
                         (empty_post .and. matches_pre), &
                         (empty_pre .and. empty_post), matches_pre])


      if (.not. runs_engine) then
         res = .false.
         return
      end if

      ! Initialize `cur_i` with automaton's initial index.
      cur_i = automaton%initial_index

      ! If the DFA have not been initialized, abort the program.
      if (cur_i == DFA_NOT_INIT) then
         error stop "DFA have not been initialized."
      end if

      ! If the input string is an empty string, returns a logical value
      ! indicating whether the current state is accepting or not.
      if (len(string) == 0) then
         res = automaton%dfa%nodes(cur_i)%accepted
         return
      end if

      ! Initialize counter variables.
      max_match = 0
      ci = 1
      str = char(0)//string//char(0)

      ! Loop and proceed with matching unless the current index is DFA_INVALID_INDEX.
      do while (cur_i /= DFA_INVALID_INDEX)

         ! If the current state acceptable, the value of `max_match` is updated with `i`.
         if (automaton%dfa%nodes(cur_i)%accepted) then
            max_match = ci
         end if

         if (ci > len(str)) exit

         ! Get the index of the next character and assign it to `next_ci`.
         next_ci = idxutf8(str, ci) + 1

         ! Lazy evaluation is performed by calling this procedure here.
         ! The index of destination DFA node is stored in the `dst_i` variable.
         call automaton%construct(cur_i, dst_i, str(ci:next_ci-1))

         ! If there is mismatch in the first byte of the NULL character, try again with the second byte.
         if (dst_i == DFA_INVALID_INDEX .and. ci == 1) then
            ci = 2
            next_ci = idxutf8(str, ci) + 1
            call automaton%construct(cur_i, dst_i, str(ci:next_ci-1))
         end if

         ! update counters
         cur_i = dst_i
         ci = next_ci

      end do
      ! If the maximum index of the match is one larger than length of the string,
      ! this function returns true, otherwise it returns false.
      if (max_match >= len(string)+2) then
         res = .true.
      else
         res = .false.
      end if
   end subroutine do_matching_exactly


end module forgex_api_internal_m