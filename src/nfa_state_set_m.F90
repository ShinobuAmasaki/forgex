! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_nfa_state_set_m module is a part of Forgex.
!
!! This file contains `nfa_state_set_t` class.

!> `forgex_nfa_m` module defines a derived-type which is the set of NFA nodes.
!> `nfa_state_set_t` represents a set of NFA nodes for the power set construction method.
#ifdef IMPURE
#define pure
#endif
module forgex_nfa_state_set_m
   use :: iso_fortran_env, only: int32
   use :: forgex_parameters_m, only: NFA_STATE_LIMIT, NFA_STATE_BASE, NFA_STATE_LIMIT, NFA_NULL_TRANSITION
   implicit none
   private

   public :: add_nfa_state
   public :: check_nfa_state
   public :: equivalent_nfa_state_set
   public :: init_state_set

   public :: print_nfa_state_set


   !> The `nfa_state_set_t` type represents set of NFA states.
   type, public :: nfa_state_set_t
      logical, allocatable :: vec(:)
   end type

contains

   pure subroutine init_state_set(state_set, ntop)
      implicit none
      type(nfa_state_set_t), intent(inout) :: state_set
      integer(int32), intent(in) :: ntop

      if (.not. allocated(state_set%vec)) then
         allocate(state_set%vec(ntop))
         state_set%vec(:) = .false.
      end if

   end subroutine init_state_set


   !> This function checks if the arguement 'state' (set of NFA state) includes state 's'.
   pure logical function check_nfa_state(state_set, state_index)
      implicit none
      type(nfa_state_set_t), intent(in) :: state_set
      integer(int32),        intent(in) :: state_index

      if (state_index /= 0) then
         check_nfa_state = state_set%vec(state_index)

      else
         check_nfa_state = .false.
      end if
   end function check_nfa_state


   !> This subroutine adds a specified state (`s`) to an NFA state set `state_set`
   !> by setting the corresponding element in `state%vec` to true.
   pure subroutine add_nfa_state(state_set, s)
      implicit none
      type(nfa_state_set_t), intent(inout) :: state_set  ! NFA state set to modify.
      integer(int32),        intent(in)    :: s          ! State index to add to the state set

      ! Set the state `s` in the `state_set` to `.true.`
      state_set%vec(s) = .true.
   end subroutine add_nfa_state


   !> This function determines if two NFA state sets (logical vectors) are equivalent.
   !>
   !> It takes two NFA state sets, compares all elements of a logical vector, perform a
   !> logical AND, and returns it.
   pure elemental function equivalent_nfa_state_set(a, b) result(res)
      implicit none
      type(nfa_state_set_t), intent(in) :: a, b

      logical        :: res

      ! If all elements match, set the result `res` to `.true.` indicating equivalence.
      res = all(a%vec .eqv. b%vec)

   end function equivalent_nfa_state_set


   ! This subroutine is for debugging, print_lazy_dfa and automaton__print_dfa use this procedure.
   subroutine print_nfa_state_set(set, top, uni)
      use, intrinsic :: iso_fortran_env, only:stderr => error_unit
      implicit none
      type(nfa_state_set_t), intent(in) :: set
      integer(int32),        intent(in) :: top
      integer(int32),        intent(in) :: uni
 
      integer(int32) :: i

      do i = 1, top
         if (check_nfa_state(set, i)) write(uni, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine print_nfa_state_set

end module forgex_nfa_state_set_m