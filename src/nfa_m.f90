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
   public :: disjoin_nfa

   ! Upper limit of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_MAX = 1024
   
   ! Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_VECTOR_SIZE = NFA_STATE_MAX

   ! nlist_t is a type represents a transition on NFA.
   ! It transit to state 'to' by character segment 'c'. 
   type, public :: nlist_t
      type(segment_t) :: c = SEG_EMPTY
      integer(int32) :: to = 0
      type(nlist_t), pointer :: next => null()  
   end type 

   ! NFA_state_set_t represents set of NFA states.
   type, public :: NFA_state_set_t
      logical :: vec(NFA_VECTOR_SIZE) = .false.
   end type 

   ! A table of transition on NFA.
   type(nlist_t), public, target :: nfa(NFA_STATE_MAX)

   ! Initial and accepting state on NFA.
   integer(int32), public :: nfa_entry, nfa_exit

   ! Number of NFA states.
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
      type(segment_t), intent(in) :: c

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
      character(:), allocatable :: cache

      print *, "--- PRINT NFA ---"

      do i = 1, NFA_STATE_MAX
         if (i <= nfa_nstate) then

            write(*, '(a, i3, a)', advance='no') "state ", i, ": "
            p => nfa(i)

            do while (associated(p))
               if (p%to /= 0 ) then

                  cache = p%c%print()

                  if (p%c == SEG_EMPTY) cache = '?'
                  
                  write(*, "(a, a, a2, i0, a1)", advance='no') "(", trim(cache),", ", p%to, ")"
               end if
               p => p%next
            end do
            write(*, *) ''
         end if
      end do



   end subroutine print_nfa

   ! Is the arguement 'state' (set of NFA state) includes state 's'?
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


   subroutine disjoin_nfa()
      use :: priority_queue_m
      use :: segment_disjoin_m
      implicit none
      type(nlist_t), pointer :: p, q
      integer(int32) :: i, j, k 
      type(priority_queue_t) :: queue
      type(segment_t), allocatable :: seg_list(:)
      integer :: num


      num = 0
      p => null()

      do i = 1,nfa_nstate
         if (i <= nfa_nstate) then

            p => nfa(i)

            do while (associated(p))
               if (p%to /= 0 ) then

                  if (p%c /= SEG_EMPTY) call enqueue(queue, p%c)
               end if
               p => p%next
            end do

         end if
      end do

      num = queue%number

      allocate(seg_list(num))
      do j = 1, num
         seg_list(j) = dequeue(queue)
      end do

      call disjoin(seg_list)

      do i = 1, nfa_nstate 
         if (i <= nfa_nstate) then

            p => nfa(i)

            do while (associated(p))
               if (p%to /= 0 .and. p%c /= SEG_EMPTY) then
                  if (.not. is_prime_semgment(p%c, seg_list)) then
                     call disjoin_nfa_state(p, seg_list)
                  end if
               end if
               p => p%next
            end do

         end if
      end do

   end subroutine disjoin_nfa

   subroutine disjoin_nfa_state(state, seg_list)
      use :: segment_disjoin_m
      implicit none
      type(nlist_t), pointer, intent(inout) ::state 

      type(segment_t), intent(inout) :: seg_list(:)
      
      integer :: j, k, siz
      siz = size(seg_list, dim=1)

      block
         logical :: flag(siz)
         flag = is_overlap_to_seg_list(state%c, seg_list, siz)

         k = 1

         do j = 1, siz
            if (flag(j)) then
               block
                  type(nlist_t), pointer :: ptr
                  ptr => null() 
            
                  if (j == 1) then
                     state%c = seg_list(j)
                  else
                     allocate(ptr)
                     ptr = state 
                     state%c = seg_list(j)
                     state%to = ptr%to
                     state%next => ptr
                  end if

               end block
            end if 
         end do 
      end block



   end subroutine disjoin_nfa_state 


end module nfa_m