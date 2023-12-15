!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     nfa_m module is a part of Forgex.

module nfa_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: segment_m 
   use :: syntax_tree_m, only: EMPTY, op_char, op_concat, op_union, op_closure, &
                               op_empty, tree_t
   use :: utf8_m
   implicit none
   private 

   public :: check_NFA_state
   public :: build_nfa
   public :: print_nfa

   integer(int32), parameter, public :: NFA_STATE_MAX = 1024
   integer(int32), parameter, public :: NFA_VECTOR_SIZE = NFA_STATE_MAX

   type, public :: nlist_t
      ! character(3) :: c = EMPTY
      type(segment_t) :: c
      integer(int32) :: to
      type(nlist_t), pointer :: next => null()  
   end type 


   type, public :: NFA_state_set_t
      logical :: vec(NFA_VECTOR_SIZE) = .false.
   end type 

   type(segment_t) :: SEG_EMPTY = segment_t(UTF8_CODE_MIN, UTF8_CODE_MIN)

   type(nlist_t), public, target :: nfa(NFA_STATE_MAX)
   integer(int32), public :: nfa_entry, nfa_exit
   integer(int32), public :: nfa_nstate = 0

contains

!=====================================================================!

   function generate_node()
      implicit none
      integer(int32) :: generate_node

      if (nfa_nstate >= NFA_STATE_MAX) then
         write(stderr, *) "Number of NFA states too large."
         error stop
      end if

      nfa_nstate = nfa_nstate + 1 
      generate_node = nfa_nstate
      
   end function generate_node


   subroutine add_transition(from, to, c)
      implicit none
      integer(int32), intent(in) :: from, to
      ! character(3), intent(in) :: c
      type(segment_t) :: c

      type(nlist_t), pointer :: p
      
      p => null()
      allocate(p)
      
      p = nfa(from)


      nfa(from)%c%min = c%min
      nfa(from)%c%max = c%max
      nfa(from)%to = to
      nfa(from)%next => p

   end subroutine add_transition
      

   recursive subroutine generate_nfa(tree, entry, way_out)
      implicit none
      type(tree_t), intent(in) :: tree
      integer(int32), intent(in) :: entry, way_out

      integer :: a1, a2, j

      select case (tree%op)
      case (op_char)
         do j = 1, size(tree%c, dim=1)
            call add_transition(entry, way_out, tree%c(j))
         end do 
     
      case (op_empty)
         call add_transition(entry, way_out, SEG_EMPTY)

      case (op_union)
         call generate_nfa(tree%left, entry, way_out)
         call generate_nfa(tree%right, entry, way_out)
      
      case (op_closure)
         a1 = generate_node()
         a2 = generate_node()
         call add_transition(entry, a1, SEG_EMPTY)
         call generate_nfa(tree%left, a1, a2)
         call add_transition(a2, a1, SEG_EMPTY)
         call add_transition(a1, way_out, SEG_EMPTY)
      
      case (op_concat)
         a1 = generate_node()
         call generate_nfa(tree%left, entry, a1)
         call generate_nfa(tree%right, a1, way_out)
      
      case default
         write(stderr, *) "This will not happen in 'generate_nfa'."
         error stop
      end select

   end subroutine generate_nfa


   subroutine build_nfa(tree)
      implicit none
      type(tree_t), intent(in)  :: tree

      nfa_entry = generate_node()

      nfa_exit = generate_node()

      call generate_nfa(tree, nfa_entry, nfa_exit)

   end subroutine build_nfa
      
   subroutine print_nfa()
      implicit none
      integer :: i, j
      type(nlist_t), pointer :: p
      character(4) :: chara(2)
      character(9) :: cache

      print *, "--- PRINT NFA ---"

      do i = 1, NFA_STATE_MAX
         if (i <= nfa_nstate) then
            write(*, '(a, i3, a)', advance='no') "state ", i, ": "
            p => nfa(i)
            do while (associated(p))
               if (p%to /= 0 ) then
                  chara(1) = char_utf8(p%c%min)
                  chara(2) = char_utf8(p%c%max)

                  if (chara(1) == chara(2))  then
                     cache = chara(1)
                  else
                     cache = trim(chara(1))//'-'//trim(chara(2))
                  end if

                  if (chara(1) == char(0) .and. chara(2) == char(0)) cache = '?'
                  
                  write(*, "(a, a, a2, i0, a1)", advance='no') "(", trim(cache),", ", p%to, ")"
               end if
               p => p%next
            end do
            write(*, *) ''
         end if
      end do

   end subroutine print_nfa
         
   logical function check_NFA_state(state, s)
      implicit none
      type(NFA_state_set_t), intent(in) :: state
      integer(int32) :: s
      
      if (s /= 0) then 
         check_NFA_state = state%vec(s)
      else
         check_NFA_state = .false. 
      end if
      
   end function check_NFA_state

end module nfa_m