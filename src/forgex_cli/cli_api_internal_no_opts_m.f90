! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_api_internal_no_opts_m module is a part of Forgex.
!
module forgex_cli_api_internal_no_opts_m
   use :: forgex_automaton_m
   use :: forgex_parameters_m
   use :: forgex_utf8_m
   implicit none

contains

   !> This procedure reads a text, performs regular expression matching using an automaton,
   !> and stores the string index in the argument if it contains a match.
   subroutine do_matching_including_no_literal_opts (automaton, string, from, to)
      use :: forgex_utility_m
      implicit none
      type(automaton_t), intent(inout) :: automaton
      character(*),      intent(in)    :: string
      integer,           intent(inout) :: from, to

      integer :: cur_i, dst_i ! current and destination index of DFA nodes
      integer :: ci           ! character index
      integer :: next_ci      ! next character index
      integer :: max_match    ! maximum value of match attempts
      integer :: start        ! starting character index
      integer :: i
      character(:), allocatable :: str

      str = string
      from = 0
      to = 0

      str = char(0)//string//char(0)

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

      loop_init: block
         i = 1
         start = i
      end block loop_init

      do while (start < len(str))
         max_match = 0
         ci = start
         cur_i = automaton%initial_index

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
            if (max_match == len(str)) then
               to = len(string)
            else
               to = max_match-2
            end if
            return
         end if

         start = idxutf8(str, start) + 1 ! Bruteforce searching

      end do
   end subroutine do_matching_including_no_literal_opts


   !> This subroutine is intended to be called from the `forgex_cli_find_m` module.
   subroutine do_matching_exactly_no_literal_opts(automaton, string, res)
      implicit none
      type(automaton_t), intent(inout) :: automaton
      character(*),      intent(in)    :: string
      logical,           intent(inout) :: res

      integer :: cur_i, dst_i ! current and destination index of DFA nodes
      integer :: ci           ! character index
      integer :: next_ci      ! next character index
      integer :: max_match    !
      character(:), allocatable :: str

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
   end subroutine do_matching_exactly_no_literal_opts


end module forgex_cli_api_internal_no_opts_m