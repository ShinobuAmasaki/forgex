!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023-2024
!!     A regular expression engine for Fortran.
!!     forgex_nfa_m module is a part of Forgex.
!!
module forgex_nfa_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: forgex_segment_m
   use :: forgex_enums_m
   use :: forgex_syntax_tree_m
   use :: forgex_utf8_m
   implicit none
   private

   public :: equivalent_NFA_state_set
   public :: check_NFA_state
   public :: add_NFA_state

   ! Upper limit of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_MAX = 1024

   ! Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_VECTOR_SIZE = NFA_STATE_MAX

   ! Initial and accepting state on NFA.
   integer(int32), public :: nfa_entry
   integer(int32), public :: nfa_exit

   ! nlist_t is a type represents a transition on NFA.
   ! It transits to state 'to' by character segument 'c'.
   type, public :: nlist_t
      type(segment_t) :: c = SEG_EMPTY
      integer(int32)  :: to = 0
      type(nlist_t), pointer :: next => null()
      integer(int32) :: index
   end type

   ! nfa_state_set_t represents set of NFA states.
   type, public :: nfa_state_set_t
      logical :: vec(NFA_VECTOR_SIZE) = .false.
   end type

   type, public :: nfa_t
      character(:), allocatable :: pattern
      integer(int32) :: nfa_nstate = 0    ! Number of NFA state
      type(nlist_t), pointer :: states(:)
      type(segment_t), allocatable :: all_segments(:)
   contains
      procedure :: init           => nfa__init
      procedure :: generate_node  => nfa__generate_node
      procedure :: generate_nfa   => nfa__generate_nfa
      procedure :: build          => nfa__build
      procedure :: add_transition => nfa__add_transition
      procedure :: disjoin        => nfa__disjoin
      procedure :: print          => nfa__print
      procedure :: print_state_set=> nfa__print_state_set
      procedure :: free           => nfa__deallocate
      procedure :: mark_empty_transition
      procedure :: collect_empty_transition
   end type 

   type :: nlist_pointer_list_t
      type(nlist_t), pointer :: node
   end type

   ! private variables
   type(nlist_pointer_list_t) :: nlist_node_list(NFA_STATE_MAX)
   integer(int32) :: nlist_node_count  = 0

contains

   subroutine nfa__init(self)
      implicit none
      class(nfa_t) :: self
      integer :: i

      self%nfa_nstate = 0

      allocate(self%states(NFA_STATE_MAX))

      do i = 1, size(self%states, dim=1)
         self%states(i)%index = i
      end do

   end subroutine nfa__init


   function nfa__generate_node(self)
      implicit none
      class(nfa_t) :: self
      integer(int32) :: nfa__generate_node

      if (self%nfa_nstate >= NFA_STATE_MAX) then
         write(stderr, *) "Number of NFA states too large."
         error stop
      end if

      self%nfa_nstate = self%nfa_nstate + 1 
      nfa__generate_node = self%nfa_nstate
      
   end function nfa__generate_node

   
   subroutine nfa__add_transition(self, from, to, c)
      implicit none
      class(nfa_t) :: self
      integer(int32), intent(in) :: from, to
      type(segment_t), intent(in) :: c

      type(nlist_t), pointer :: p
      
      p => null()
      allocate(p)

      nlist_node_count = nlist_node_count + 1
      nlist_node_list(nlist_node_count)%node => p
      
      p = self%states(from)

      self%states(from)%c%min = c%min
      self%states(from)%c%max = c%max
      self%states(from)%to = to
      self%states(from)%next => p

   end subroutine nfa__add_transition


   recursive subroutine nfa__generate_nfa(self, tree, entry, way_out)
      implicit none
      class(nfa_t) :: self
      type(tree_t), intent(in) :: tree
      integer(int32), intent(in) :: entry, way_out

      integer :: a1, a2, j

      select case (tree%op)
      case (op_char)
         do j = 1, size(tree%c, dim=1)
            call self%add_transition(entry, way_out, tree%c(j))
         end do 
     
      case (op_empty)
         call self%add_transition(entry, way_out, SEG_EMPTY)

      case (op_union)
         call self%generate_nfa(tree%left, entry, way_out)
         call self%generate_nfa(tree%right, entry, way_out)
      
      case (op_closure)
         a1 = self%generate_node()
         a2 = self%generate_node()
         call self%add_transition(entry, a1, SEG_EMPTY)
         call self%generate_nfa(tree%left, a1, a2)
         call self%add_transition(a2, a1, SEG_EMPTY)
         call self%add_transition(a1, way_out, SEG_EMPTY)
      
      case (op_concat)
         a1 = self%generate_node()
         call self%generate_nfa(tree%left, entry, a1)
         call self%generate_nfa(tree%right, a1, way_out)
      
      case default
         write(stderr, *) "This will not happen in 'generate_nfa'."
         error stop
      end select

   end subroutine nfa__generate_nfa

   subroutine nfa__disjoin(self)
      use :: forgex_priority_queue_m
      use :: forgex_segment_disjoin_m
      implicit none
      class(nfa_t) :: self
      type(nlist_t), pointer :: p
      integer(int32) :: i, j
      type(priority_queue_t) :: queue
      type(segment_t), allocatable :: seg_list(:)
      integer :: num


      num = 0
      p => null()

      block ! enqueue
         do i = 1, self%nfa_nstate
            p => self%states(i)

            do while (associated(p))
               if (p%to /= 0 ) then

                  if (p%c /= SEG_EMPTY) call enqueue(queue, p%c)
               end if
               p => p%next
            end do
         end do
      end block ! enqueue

      num = queue%number

      allocate(seg_list(num))
      do j = 1, num
         seg_list(j) = dequeue(queue)
      end do

      !-- seg_list array is sorted.

      call disjoin(seg_list)
      self%all_segments = seg_list ! all_segments are one of the module array-variables.

      do i = 1, self%nfa_nstate
         p => self%states(i)

         if (.not. is_prime_semgment(p%c, seg_list)) then
            call disjoin_nfa_state(p, seg_list)
         end if
      end do

      do i = 1, self%nfa_nstate 

         p => self%states(i)%next

         inner: do while (associated(p))

            if (.not. is_prime_semgment(p%c, seg_list)) then                     
               call disjoin_nfa_state(p, seg_list)
            end if

            if (p%index > 0) exit inner

            p => p%next

         end do inner
      end do

      !-- deallocate
      call clear(queue)
      deallocate(seg_list)

   end subroutine nfa__disjoin


   subroutine nfa__build(self, tree)
      implicit none
      class(nfa_t) :: self
      type(tree_t), intent(in)  :: tree

      nfa_entry = self%generate_node()

      nfa_exit = self%generate_node()

      call self%generate_nfa(tree, nfa_entry, nfa_exit)

      call self%disjoin()

   end subroutine nfa__build


   subroutine nfa__deallocate(self)
      implicit none
      class(nfa_t) :: self
      integer :: j, max 
      
      max = nlist_node_count
      if (max < 1) return

      do j = 1, max
         if (associated(nlist_node_list(j)%node)) then
            deallocate(nlist_node_list(j)%node)
            nlist_node_count = nlist_node_count -1
         end if
      end do

      if (associated(self%states)) nullify (self%states)

   end subroutine nfa__deallocate
   

   subroutine nfa__print(self)
      implicit none
      class(nfa_t) :: self 
      integer :: i
      type(nlist_t), pointer :: p
      character(:), allocatable :: cache

      write(stderr, *) "--- PRINT NFA ---"

      do i = 1, self%nfa_nstate
         if (i <= self%nfa_nstate) then

            write(stderr, '(a, i3, a)', advance='no') "state ", i, ": "
            p => self%states(i)

            do while (associated(p))
               if (p%to /= 0 ) then

                  cache = p%c%print()

                  if (p%c == SEG_EMPTY) cache = '?'
                  
                  write(stderr, "(a, a, a2, i0, a1)", advance='no') "(", trim(cache),", ", p%to, ")"
               end if
               p => p%next
            end do
            write(stderr, *) ''
         end if
      end do

   end subroutine nfa__print
   

   subroutine nfa__print_state_set (self, p)
      implicit none
      class(nfa_t) :: self 
      type(NFA_state_set_t), intent(in), target :: p

      integer(int32) :: i

      do i = 1, self%nfa_nstate
         if (check_NFA_state(p, i)) write(stderr, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine nfa__print_state_set

   

!==========================================================================================!

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


   subroutine disjoin_nfa_state(state, seg_list)
      use :: forgex_segment_disjoin_m
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

                     nlist_node_count = nlist_node_count + 1
                     nlist_node_list(nlist_node_count)%node => ptr

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

   
   subroutine add_NFA_state(state, s)
      implicit none
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in):: s

      state%vec(s) = .true.
   end subroutine add_NFA_state


   recursive subroutine mark_empty_transition(self, state, idx)
      implicit none
      class(nfa_t) :: self
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in) :: idx 

      type(nlist_t), pointer :: p

! write(stderr, *) "L407 i =", idx
      call add_NFA_state(state, idx)
   
! write(stderr, *) "L410 state =", state%vec(1:6)
      nullify(p)

      p => self%states(idx)
      do while (associated(p))

         if (p%c == SEG_EMPTY .and. .not. check_NFA_state(state, p%to) ) then
            if (p%to /= 0) call self%mark_empty_transition(state, p%to)
         end if 

         p => p%next

      enddo 

   end subroutine mark_empty_transition


   subroutine collect_empty_transition (self, state)
      implicit none
      class(nfa_t) :: self
      type(NFA_state_set_t), intent(inout):: state
      integer(int32) :: i 

! write(stderr, *) "L430", state%vec(1:6)
      do i = 1, self%nfa_nstate
         if (check_NFA_state(state, i)) then
            call self%mark_empty_transition(state, i)       
         end if
      end do
! write(stderr, *) "L432", state%vec(1:6) 

   end subroutine collect_empty_transition



   function equivalent_NFA_state_set(a, b) result(res)
      implicit none
      type(NFA_state_set_t), intent(in), pointer  :: a
      type(NFA_state_set_t), intent(in)  :: b
      integer(int32) :: i
      logical :: res


      do i = 1, NFA_VECTOR_SIZE
         if (a%vec(i) .neqv. b%vec(i)) then
            res = .false.
            return
         end if
      end do
      res = .true.

   end function equivalent_NFA_state_set


end module forgex_nfa_m