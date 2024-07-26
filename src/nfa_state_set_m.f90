! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_nfa_m module is a part of Forgex.
!
!! This file contains `nfa_state_set_t` class.

!> `forgex_nfa_m` module defines a derived-type which is the set of NFA nodes.
!> `nfa_state_set_t` represents a set of NFA nodes for the power set construction method.
module forgex_nfa_state_set_m
   use :: forgex_parameters_m
   implicit none
   private

   !> The `nfa_state_set_t` type represents set of NFA states.
   type, public :: nfa_state_set_t
      logical :: vec(NFA_STATE_BASE:NFA_STATE_LIMIT) = .false.
   end type

contains


   !> This function checks if the arguement 'state' (set of NFA state) includes state 's'.
   logical function check_nfa_state(state_set, state_index)
      implicit none
      type(nfa_state_set_t), intent(in) :: state_set

      integer(int32) :: state_index

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
   !> It takes a pointer to NFA state set and NFA state set, and returns logical result
   !> indicating equivalent (`.true.` if equivalent, `.false.` otherwise).
   pure function equivalent_nfa_state_set(a, b) result(res)
      implicit none
      type(nfa_state_set_t), intent(in) :: a, b

      integer(int32) :: i
      logical        :: res

      ! Compare each element of the logical vectors in a and b.
      do i = NFA_STATE_BASE, NFA_STATE_LIMIT
         if (a%vec(i) .neqv. b%vec(i)) then
            res = .false.
            return
         end if
      end do
      
      ! If all elements match, set the result `res` to `.true.` indicating equivalence. 
      res = .true.
   end function equivalent_nfa_state_set
   
end module forgex_nfa_state_set_m