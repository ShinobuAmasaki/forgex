! Fortran Regular Expression (Forgex)
! 
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_nfa_m module is a part of Forgex.
!
!! This file contains `nfa_t` class and its type-bound procedures.

!> The `forgex_nfa_m` module defines the data structure of NFA.
!> The `nfa_t` is defined as a class representing NFA.
module forgex_nfa_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: forgex_segment_m
   use :: forgex_enums_m
   use :: forgex_syntax_tree_m
   use :: forgex_utf8_m
   implicit none
   private

   public :: equivalent_nfa_state_set
   public :: check_nfa_state
   public :: add_nfa_state

   !> Upper limit of NFA state instance
   integer(int32), parameter, public :: NFA_STATE_MAX = 1024

   !> Upper limit of NFA transition instance
   integer(int32), parameter, public :: NFA_VECTOR_SIZE = NFA_STATE_MAX
   
   !> Initial state on NFA.
   integer(int32), public :: nfa_entry
   !> Accepting state on NFA. 
   integer(int32), public :: nfa_exit

   !| The `nlist_t` type represents a transition on NFA.
   !  It transits to state 'to' by character segument 'c'.
   ! 
   type, public :: nlist_t
      type(segment_t) :: c = SEG_EMPTY
      integer(int32)  :: to = 0
      type(nlist_t), pointer :: next => null()
      integer(int32) :: index
   end type

   !> The `nfa_state_set_t` type represents set of NFA states.
   type, public :: nfa_state_set_t
      logical :: vec(NFA_VECTOR_SIZE) = .false.
   end type

   !> The `nfa_t` class represents a single automaton as a set of NFA states.
   !> An NFA is built from the input syntax-tree.
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
#ifdef DEBUG
      procedure :: print          => nfa__print
      procedure :: print_state_set=> nfa__print_state_set
#endif
      procedure :: free           => nfa__deallocate
      procedure :: mark_empty_transition
      procedure :: collect_empty_transition
   end type 

   !> An derived-type definition for element that make up the pointer array
   !> for the monitor of the `nlist_t` type.
   type :: nlist_pointer_list_t
      type(nlist_t), pointer :: node
   end type

   !> The monitor array of the `nlist` type.
   type(nlist_pointer_list_t) :: nlist_node_list(NFA_STATE_MAX)
   !> The number of nodes registered in the monitor array of the `nlist_node_list`.
   integer(int32) :: nlist_node_count  = 0

contains

   !> The `nfa__init`  subroutine initialize an `nfa_t` type instance.
   !> This procedure belongs to the class of `nfa_t` derived-type and is called as `init`. 
   subroutine nfa__init(self)
      implicit none
      class(nfa_t), intent(inout) :: self
      
      integer :: i

      ! Initialize the counter of an instance. 
      self%nfa_nstate = 0

      allocate(self%states(NFA_STATE_MAX))

      ! Initialize the index of states conteined in an instance.
      do i = 1, size(self%states, dim=1)
         self%states(i)%index = i
      end do
   end subroutine nfa__init

   
   !> This subroutine builds an NFA from a given syntax tree.
   !> 
   !> This procedure is intended to be used in procedures that define the API of the forgex module.
   subroutine nfa__build(self, tree)
      implicit none
      class(nfa_t), intent(inout) :: self    ! The NFA object to modify.
      type(tree_t), intent(in)    :: tree    ! The syntax tree representing the regular expression.

      ! Generate the entry state for the NFA.
      nfa_entry = self%generate_node()

      ! Generate the exit state for the NFA.
      nfa_exit = self%generate_node()

      ! Recursively generate the NFA from the syntax tree.
      call self%generate_nfa(tree, nfa_entry, nfa_exit)

      ! Disjoin the NFA states to ensure proper transition for a DFA construction.
      call self%disjoin()
   end subroutine nfa__build


   !> The `nfa__generate_node` function generates an node and counts `nfa_state` in an instance of the class. 
   function nfa__generate_node(self)
      implicit none
      class(nfa_t), intent(inout) :: self
      integer(int32) :: nfa__generate_node

      !! If the counter exceeds NFA_STATE_MAX, an error stop will occur. 
      if (self%nfa_nstate >= NFA_STATE_MAX) then
         write(stderr, *) "Number of NFA states too large."
         error stop
      end if

      ! Increment the component of `tyep(nfa_t)` for counter.
      self%nfa_nstate = self%nfa_nstate + 1

      ! Return the value of the `nfa_nstate` component of the `self`.
      nfa__generate_node = self%nfa_nstate
   end function nfa__generate_node

   
   !> This subroutine adds a transition to the NFA state from state `from` to state `to` with character 
   !> segment `c`.
   subroutine nfa__add_transition(self, from, to, c)
      implicit none
      class(nfa_t),    intent(inout) :: self
      integer(int32),  intent(in)    :: from, to
      type(segment_t), intent(in)    :: c

      type(nlist_t), pointer :: p
      
      p => null()
      allocate(p)
      ! Increment the node count and store the new node in the global monitor array `nlist_node_list`. 
      nlist_node_count = nlist_node_count + 1
      nlist_node_list(nlist_node_count)%node => p
      
      ! Deep copy the current node's information to `p`.
      p = self%states(from)

      ! Register the given information in an array node and places it at the beginning of the linked list. 
      self%states(from)%c%min = c%min
      self%states(from)%c%max = c%max
      self%states(from)%to = to
      self%states(from)%next => p
   end subroutine nfa__add_transition


   !> This subroutine recursively generates an NFA from a syntax tree.
   recursive subroutine nfa__generate_nfa(self, tree, entry, way_out)
      implicit none
      class(nfa_t),   intent(inout) :: self           ! The NFA object to modify.
      type(tree_t),   intent(in)    :: tree           ! The syntax tree representing the regular expression.
      integer(int32), intent(in)    :: entry          ! The entry state of the NFA
      integer(int32), intent(in)    :: way_out        ! The exit state of NFA
      
      integer :: a1, a2, j

      ! Determine the operation type of the current node in the syntax tree.
      select case (tree%op)
      case (op_char)
         ! Handle character operations by adding transitions for each character.
         do j = 1, size(tree%c, dim=1)
            call self%add_transition(entry, way_out, tree%c(j))
         end do 
     
      case (op_empty)
         ! Handle empty operation by adding an empty transition.
         call self%add_transition(entry, way_out, SEG_EMPTY)

      case (op_union)
         ! Handle union operation by recursively generating NFAs for left and right subtrees.
         call self%generate_nfa(tree%left, entry, way_out)
         call self%generate_nfa(tree%right, entry, way_out)
      
      case (op_closure)
         ! Handle closure (Kleene star) operations by creating new node and adding appropriate transition.
         a1 = self%generate_node()
         a2 = self%generate_node()
         call self%add_transition(entry, a1, SEG_EMPTY)

         call self%generate_nfa(tree%left, a1, a2)

         call self%add_transition(a2, a1, SEG_EMPTY)
         call self%add_transition(a1, way_out, SEG_EMPTY)
      
      case (op_concat)
         ! Handle concatenation operations by recursively generating NFAs for left and right subtrees.
         a1 = self%generate_node()
         call self%generate_nfa(tree%left, entry, a1)
         call self%generate_nfa(tree%right, a1, way_out)
      
      case default
         ! Handle unexpected cases.
         ! If this case is encountered, it prints an error message and stop execution
         write(stderr, *) "This will not happen in 'generate_nfa'."
         error stop
      end select
   end subroutine nfa__generate_nfa


   !> This subroutine disjoins the segments of an NFA.
   !> It ensures that the segments of the NFA are disjoined, meacning no two segemnts overlap.
   !> 
   subroutine nfa__disjoin(self)
      use :: forgex_priority_queue_m
      use :: forgex_segment_disjoin_m
      implicit none
      class(nfa_t), intent(inout) :: self
 
      type(nlist_t), pointer       :: p
      type(priority_queue_t)       :: queue
      type(segment_t), allocatable :: seg_list(:)
      integer(int32)               :: i, j, num

      num = 0
      p => null()

      ! Traverse through all states and enqueue their segments into a priority queue.
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

      ! Allocate memory for the segment list and dequeue all segments for the priority queue.
      num = queue%number
      allocate(seg_list(num))
      do j = 1, num
         seg_list(j) = dequeue(queue)
      end do
      !--- The seg_list array is now sorted.

      ! Disjoin the segments to ensure no overlaps.
      call disjoin(seg_list)
      self%all_segments = seg_list ! Store the disjoined segments in the NFA.

      ! Traverse through all states and disjoin their segments if necessary.
      do i = 1, self%nfa_nstate
         p => self%states(i)

         if (.not. is_prime_semgment(p%c, seg_list)) then
            call disjoin_nfa_state(p, seg_list)
         end if
      end do

      ! Repeat the process for the rest of the states.
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

      ! Deallocate memory resources. 
      call clear(queue)
      deallocate(seg_list)
   end subroutine nfa__disjoin



   !> This subroutine deallocates all the nodes in the monitor array and their associated 
   !> linked-list nodes.
   subroutine nfa__deallocate(self)
      implicit none
      class(nfa_t), intent(inout) :: self

      integer :: j, max 
      
      max = nlist_node_count
      if (max < 1) return

      do j = 1, max
         if (associated(nlist_node_list(j)%node)) then
            deallocate(nlist_node_list(j)%node)
            nlist_node_count = nlist_node_count -1
         end if
      end do

      if (associated(self%states)) then
         deallocate(self%states)
      end if
   end subroutine nfa__deallocate


!==========================================================================================!


   ! This function checks if the arguement 'state' (set of NFA state) includes state 's'.
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


   !> This subroutine updates the NFA state transitions by disjoining the segments.
   !>
   !> It breaks down overlapping segments into non-overlapping segments,
   !>  and creates new transitions accordingly.
   subroutine disjoin_nfa_state(state, seg_list)
      use :: forgex_segment_disjoin_m
      implicit none
      type(nlist_t),   intent(inout), pointer :: state         ! Pointer to the NFA state list.
      type(segment_t), intent(inout)          :: seg_list(:)   ! Array of segment structures.
      
      integer :: j, k, siz
      siz = size(seg_list, dim=1)

      block
         logical :: flag(siz)    ! Check which segments in seg_list overlap with the current state's segment.

         flag = is_overlap_to_seg_list(state%c, seg_list, siz)

         k = 1

         do j = 1, siz
            if (flag(j)) then
               block
                  type(nlist_t), pointer :: ptr
                  ptr => null() 
            
                  if (j == 1) then
                     ! If it is the first segment, update the current `state`'s segment.
                     state%c = seg_list(j)
                  else
                     ! For subsequent segments, create a new transition.
                     allocate(ptr)

                     ! Increment the global node count and associate the new node with the 
                     ! global list.
                     nlist_node_count = nlist_node_count + 1
                     nlist_node_list(nlist_node_count)%node => ptr

                     ! Store the current top node (node of `ptr`) information with a deep copy. 
                     ptr = state 
                     ! Update the current `state`'s segment to the new segment.
                     state%c = seg_list(j)

                     ! Preserve the `to` state of the current transition.
                     state%to = ptr%to

                     ! Link the new node next to the current top node in the transition list.
                     state%next => ptr
                  end if

               end block
            end if 
         end do 
      end block
   end subroutine disjoin_nfa_state 


   !> This subroutine adds a specified state (`s`) to an NFA state set `state_set`
   !> by setting the corresponding element in `state%vec` to true.
   subroutine add_nfa_state(state_set, s)
      implicit none
      type(nfa_state_set_t), intent(inout) :: state_set  ! NFA state set to modify.
      integer(int32),        intent(in)    :: s          ! State index to add to the state set

      ! Set the state `s` in the `state_set` to `.true.`
      state_set%vec(s) = .true.
   end subroutine add_nfa_state


   !> Recursively marks empty transitions from a given state in an NFA.
   recursive subroutine mark_empty_transition(self, state_set, idx)
      implicit none
      class(nfa_t),          intent(in)    :: self       ! Instance of the NFA class
      type(nfa_state_set_t), intent(inout) :: state_set  ! NFA state set to mark empty transition
      integer(int32),        intent(in)    :: idx        ! Index of the current state in self%states

      type(nlist_t), pointer :: p

      nullify(p)

      ! Add the current state to the state set.
      call add_nfa_state(state_set, idx)

      ! Traverse the linked list of transition from the `idx`-th state.
      p => self%states(idx)
      do while (associated(p))
         if (p%c == SEG_EMPTY .and. .not. check_nfa_state(state_set, p%to) ) then
            ! Recursively mark empty transitions from state `p%to`.
            if (p%to /= 0) call self%mark_empty_transition(state_set, p%to)
         end if 

         p => p%next
      enddo
   end subroutine mark_empty_transition


   !> This subroutine collects all states reachable by empty transition starting from a given
   !> state set in an NFA.
   subroutine collect_empty_transition (self, state)
      implicit none
      class(nfa_t),          intent(in)   :: self
      type(nfa_state_set_t), intent(inout):: state

      integer(int32) :: i 
      
      ! Iterate over all states in the NFA.
      do i = 1, self%nfa_nstate
         if (check_NFA_state(state, i)) then
            ! If state `i` is in the state set `state_set`, mark empty transitions from state `i`.
            call self%mark_empty_transition(state, i)       
         end if
      end do
   end subroutine collect_empty_transition


   !> This function determines if two NFA state sets (logical vectors) are equivalent.
   !>
   !> It takes a pointer to NFA state set and NFA state set, and returns logical result
   !> indicating equivalent (`.true.` if equivalent, `.false.` otherwise).
   function equivalent_nfa_state_set(a, b) result(res)
      implicit none
      type(nfa_state_set_t), intent(in), pointer  :: a
      type(nfa_state_set_t), intent(in)           :: b

      integer(int32) :: i
      logical        :: res

      ! Compare each element of the logical vectors in a and b.
      do i = 1, NFA_VECTOR_SIZE
         if (a%vec(i) .neqv. b%vec(i)) then
            res = .false.
            return
         end if
      end do
      
      ! If all elements match, set the result `res` to `.true.` indicating equivalence. 
      res = .true.
   end function equivalent_nfa_state_set

!=====================================================================!
!  Procedure for debugging

#ifdef DEBUG
   subroutine nfa__print(self)
      implicit none
      class(nfa_t), intent(in) :: self

      type(nlist_t), pointer    :: p
      character(:), allocatable :: cache
      integer                   :: i

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
      class(nfa_t),          intent(in)         :: self 
      type(NFA_state_set_t), intent(in), target :: p

      integer(int32) :: i

      do i = 1, self%nfa_nstate
         if (check_NFA_state(p, i)) write(stderr, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine nfa__print_state_set
#endif

end module forgex_nfa_m
