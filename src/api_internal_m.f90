#ifdef PURE
#define pure
#endif
module forgex_api_internal_m
   use, intrinsic :: iso_fortran_env, only: stderr => error_unit
   use :: forgex_parameters_m
   use :: forgex_automaton_m
   use :: forgex_utf8_m
   implicit none
   private
   public :: do_matching_exactly
contains

   subroutine do_matching_exactly(automaton, string, res)
   ! pure subroutine do_matching_exactly(automaton, string, res)
      implicit none
      type(automaton_t), intent(inout) :: automaton
      character(*), intent(in) :: string
      logical, intent(inout) :: res

      integer :: cur_i, dst_i
      integer :: ci        ! character index
      integer :: next_ci   ! next character index
      integer :: max_match

      cur_i = automaton%initial_index

      if (cur_i == DFA_NOT_INIT) then
         error stop "DFA have not been initialized."
      end if

      if (len(string) == 0) then
         res = automaton%dfa%nodes(cur_i)%accepted
         return
      end if

      max_match = 0
      ci = 1
      do while (cur_i /= DFA_INVALID_INDEX)
         ! If the current state acceptable, the value of `max_match` is updated with `i`.
         if (automaton%dfa%nodes(cur_i)%accepted) then
            max_match = ci
         end if

         if (ci > len(string)) exit

         next_ci = idxutf8(string, ci) + 1

         call automaton%construct(cur_i, dst_i, string(ci:next_ci-1))

         cur_i = dst_i

         ci = next_ci

      end do


      if (max_match == len(string)+1) then
         res = .true.
      else
         res = .false.
      end if

   end subroutine do_matching_exactly


end module forgex_api_internal_m