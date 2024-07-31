#ifdef IMPURE
#define pure
#endif
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
module forgex_nfa_node_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit, int32
   use :: forgex_parameters_m, only: TREE_NODE_BASE, TREE_NODE_LIMIT, &
       NFA_NULL_TRANSITION, NFA_STATE_BASE, NFA_STATE_LIMIT, NFA_TRANSITION_SIZE, NFA_C_SIZE
   use :: forgex_segment_m
   use :: forgex_syntax_tree_m

   implicit none
   private
   
   public :: build_nfa_graph
   public :: disjoin_nfa
   public :: nfa_deallocate
   public :: make_nfa_node
   public :: generate_nfa

   type, public :: nfa_transition_t
      type(segment_t), allocatable :: c(:)
      integer(int32)  :: c_top = 0
      integer(int32)  :: dst = NFA_NULL_TRANSITION
      integer(int32)  :: own_j = NFA_NULL_TRANSITION
      logical         :: is_registered = .false.
   end type

   type, public :: nfa_state_node_t
      integer(int32) :: own_i
      type(nfa_transition_t), allocatable :: forward(:)
      type(nfa_transition_t), allocatable :: backward(:)
      integer(int32) :: forward_top = 0
      integer(int32) :: backward_top = 0
      ! type(segment_t), allocatable :: all_segments(:)
   contains
      procedure :: add_transition => nfa__add_transition
   end type

contains

   pure subroutine build_nfa_graph (tree, root_i, nfa, nfa_entry, nfa_exit, nfa_top, all_segments)
      implicit none
      type(tree_node_t),      intent(in)                 :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      integer(int32),         intent(in)                 :: root_i
      type(nfa_state_node_t), intent(inout), allocatable :: nfa(:)
      integer(int32),         intent(inout)              :: nfa_entry
      integer(int32),         intent(inout)              :: nfa_exit
      integer(int32),         intent(inout)              :: nfa_top
      type(segment_t),        intent(inout), allocatable :: all_segments(:)
      

      integer(int32) :: i, i_begin, i_end ! index for states array
      integer(int32) :: j                 ! index for transitions array

      i_begin = NFA_STATE_BASE
      i_end   = NFA_STATE_LIMIT

      ! initialize
      nfa_top = 0

      allocate(nfa(i_begin:i_end))

      ! Initialize
      nfa(i_begin:i_end)%own_i = [(i, i =i_begin, i_end)]

      do i = i_begin, i_end
         allocate(nfa(i)%forward(1:NFA_TRANSITION_SIZE))
         allocate(nfa(i)%backward(1:NFA_TRANSITION_SIZE))
      end do

      do i = i_begin, i_end
         do j = 1, NFA_TRANSITION_SIZE
            nfa(i)%forward(j)%own_j = j
            nfa(i)%backward(j)%own_j = j
         end do
      end do

      nfa(:)%forward_top = 1
      nfa(:)%backward_top = 1


      call make_nfa_node(nfa_top)
      nfa_entry = nfa_top

      call make_nfa_node(nfa_top)
      nfa_exit = nfa_top

      call generate_nfa(tree, root_i, nfa, nfa_top, nfa_entry, nfa_exit)

      call disjoin_nfa(nfa, nfa_top, all_segments)

   end subroutine build_nfa_graph


   pure subroutine nfa_deallocate(nfa)
      implicit none
      type(nfa_state_node_t), allocatable, intent(inout) :: nfa(:)
      integer :: i

      if (.not. allocated(nfa)) return

      do i = NFA_STATE_BASE, NFA_STATE_LIMIT
         if (allocated(nfa(i)%forward)) deallocate(nfa(i)%forward)
         if (allocated(nfa(i)%backward)) deallocate(nfa(i)%backward)
         ! if (allocated(nfa(i)%all_segments)) deallocate(nfa(i)%all_segments)
      end do

      deallocate(nfa)
   end subroutine nfa_deallocate


   pure subroutine make_nfa_node(nfa_top)
      implicit none
      integer(int32), intent(inout) :: nfa_top

      nfa_top = nfa_top + 1

   end subroutine make_nfa_node


   pure recursive subroutine generate_nfa(tree, tree_idx, nfa_graph, nfa_top, entry, exit)
      use :: forgex_enums_m
      implicit none
      type(tree_node_t),       intent(in)    :: tree(TREE_NODE_BASE:TREE_NODE_LIMIT)
      type(nfa_state_node_t),  intent(inout) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32), intent(in) :: tree_idx
      integer(int32), intent(inout) :: nfa_top
      integer(int32), intent(in) :: entry
      integer(int32), intent(in) :: exit

      integer :: i
      integer :: k
      integer :: node1
      integer :: node2

      i = tree_idx

      select case(tree(i)%op)
      case (op_char)
         ! Handle character operations by adding transition for each character.
         do k = 1, size(tree(i)%c, dim=1)
            call nfa_graph(entry)%add_transition(nfa_graph, entry, exit, tree(i)%c(k))
         end do
      
      case (op_empty)
         ! Handle empty opration by adding an epsilon transition
         call nfa_graph(entry)%add_transition(nfa_graph, entry, exit, SEG_EPSILON)
      
      case (op_union)
         ! Handle union operation by recursively generating NFA for left and right subtrees.
         call make_nfa_node(nfa_top)
         call generate_nfa(tree, tree(i)%left_i, nfa_graph, nfa_top, entry, exit)
         call generate_nfa(tree, tree(i)%right_i, nfa_graph, nfa_top, entry, exit)
      
      case (op_closure)
         ! Handle closure (Kleene star) operations by creating new node and adding appropriate transition
         call make_nfa_node(nfa_top)
         node1 = nfa_top
         call make_nfa_node(nfa_top)
         node2 = nfa_top

         call nfa_graph(entry)%add_transition(nfa_graph, entry, node1, SEG_EPSILON)

         call generate_nfa(tree, tree(i)%left_i, nfa_graph, nfa_top, node1, node2)
         
         call nfa_graph(node2)%add_transition(nfa_graph, node2, node1, SEG_EPSILON)
         call nfa_graph(node1)%add_transition(nfa_graph, node1, exit, SEG_EPSILON)

      case (op_concat)
         ! Handle concatenation operations by recursively generating NFA for left and right subtrees.
         call make_nfa_node(nfa_top)
         node1 = nfa_top
         call generate_nfa(tree, tree(i)%left_i, nfa_graph, nfa_top, entry, node1)
         call generate_nfa(tree, tree(i)%right_i, nfa_graph, nfa_top, node1, exit)

      case default ! for case (op_not_init)
         ! Handle unexpected cases.
         error stop "This will not heppen in 'generate_nfa'."
      end select
   end subroutine generate_nfa


   pure subroutine nfa__add_transition(self,nfa_graph, src, dst, c)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      type(nfa_state_node_t), intent(inout) :: nfa_graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer(int32), intent(in) :: src, dst
      type(segment_t) ,intent(in) :: c

      integer(int32) :: j, k

      j = self%forward_top
      if (j >= size(self%forward, dim=1)) then
         ! reallocate
      endif
      if (.not. allocated(self%forward(j)%c))  allocate(self%forward(j)%c(NFA_C_SIZE))
      
      self%forward(j)%c_top = self%forward(j)%c_top + 1
      k = self%forward(j)%c_top

      self%forward(j)%c(k) = c
      self%forward(j)%dst = dst
      self%forward(j)%is_registered = .true.
      
      self%forward_top = j + 1


      j = nfa_graph(dst)%backward_top
      if (j >= size(nfa_graph(dst)%backward, dim=1)) then
         ! reallocate
      endif
      if (.not. allocated(nfa_graph(dst)%backward(j)%c))  allocate(nfa_graph(dst)%backward(j)%c(NFA_C_SIZE))
      
      nfa_graph(dst)%backward(j)%c_top = nfa_graph(dst)%backward(j)%c_top + 1
      k = nfa_graph(dst)%backward(j)%c_top

      nfa_graph(dst)%backward(j)%c(k) = c
      nfa_graph(dst)%backward(j)%dst = src
      nfa_graph(dst)%backward(j)%is_registered = .true.
      
      nfa_graph(dst)%backward_top = j + 1
   end subroutine nfa__add_transition


   pure subroutine disjoin_nfa(graph, nfa_top, seg_list)
      use :: forgex_priority_queue_m
      use :: forgex_segment_m
      use :: forgex_segment_disjoin_m
      implicit none
      type(nfa_state_node_t), intent(inout) :: graph(NFA_STATE_BASE:NFA_STATE_LIMIT)
      integer, intent(in) :: nfa_top
      type(segment_t), allocatable, intent(inout) :: seg_list(:)

      type(priority_queue_t) :: queue_f
      type(nfa_transition_t) :: ptr
      
      integer :: i, j, k, num_f

      ! Enqueue
      ! Traverse through all states and enqueue their segments into a priority queue.
      block
         do i = NFA_STATE_BASE, nfa_top ! Do not subtract 1 from nfa_top.
      
            do j = 1, graph(i)%forward_top -1
               
               ptr = graph(i)%forward(j)
               if (ptr%dst /= NFA_NULL_TRANSITION) then
                  do k = 1, graph(i)%forward(j)%c_top
                     if (ptr%c(k) /= SEG_EPSILON .and. ptr%c(k) /= SEG_INIT) then
                     ! if (ptr%c(k) /= SEG_EMPTY .and. ptr%c(k) /= SEG_EPSILON .and. ptr%c(k) /= SEG_INIT) then
                        call queue_f%enqueue(ptr%c(k))
                     end if
                  end do
               end if
            end do
         end do
      end block

      ! Dequeue
      ! Allocate memory for the segment list and dequeue all segments for the priority queue.
      num_f = queue_f%number
      allocate(seg_list(num_f))
      do j = 1, num_f
         call queue_f%dequeue(seg_list(j))
      end do

      !-- The seg_list arrays are now sorted.

      ! Disjoin the segment lists to ensure no over laps
      call disjoin(seg_list)


      ! Apply disjoining to all transitions over the NFA graph.

      ! do concurrent (i = NFA_STATE_BASE:nfa_top)
      !    do concurrent (j = 1:graph(1)%forward_top)
      do i = NFA_STATE_BASE, nfa_top
         do j = 1, graph(1)%forward_top
            call disjoin_nfa_each_transition(graph(i)%forward(j), seg_list)
         end do
         do j = 1, graph(i)%backward_top
            call disjoin_nfa_each_transition(graph(i)%backward(j), seg_list)
         end do
      end do

      ! deallocate the used priority queue.
      call queue_f%clear()
   end subroutine disjoin_nfa


   !> This subroutine updates the NFA state transitions by disjoining the segments.
   !>
   !> It breaks down overlapping segments into non-overlapping segments,
   !>  and creates new transitions accordingly.
   pure subroutine disjoin_nfa_each_transition(transition, seg_list)
      use :: forgex_segment_disjoin_m
      implicit none
      type(nfa_transition_t), intent(inout) :: transition
      type(segment_t),        intent(in) :: seg_list(:)

      type(segment_t), allocatable ::  tmp(:)

      integer :: k, m, n, siz

      if (.not. allocated(transition%c)) return

      siz = size(seg_list, dim=1)
      allocate(tmp(siz))

      block
         logical :: flag(siz)
         
         n = 0 ! to count valid disjoined segments.
         do k = 1, transition%c_top

            flag(:) = is_overlap_to_seg_list(transition%c(k), seg_list, siz)

            do m = 1, siz
               if (flag(m)) then
                  n = n + 1
                  tmp(n) = seg_list(m)
               end if
            end do

         end do
      end block

      if (size(transition%c, dim=1) < n) then
         deallocate(transition%c)
         allocate(transition%c(n))
      end if

      ! Deep copy the result into the arguemnt's component
      do k = 1, n
         transition%c(k) = tmp(k)
      end do

      call update_c_top(transition)

      deallocate(tmp)
   end subroutine disjoin_nfa_each_transition

   
   !> Update c_top, which has become outdated by disjoin, to new information.
   pure subroutine update_c_top(transition)
      implicit none
      type(nfa_transition_t), intent(inout) :: transition

      integer :: j, k
      if (.not. allocated(transition%c)) return
         
      k = 1
      do while(transition%c(k) /= SEG_INIT)
         k = k + 1
      end do
      transition%c_top = k
   
   end subroutine update_c_top


   pure subroutine transition_to_seg_list(transition_list, top_idx, segment_list)
      implicit none
      type(nfa_transition_t),       intent(in)    :: transition_list(:)
      integer(int32),               intent(in)    :: top_idx
      type(segment_t), allocatable, intent(inout) :: segment_list(:)

      integer :: j, k

      allocate(segment_list(top_idx))

      do j = 1, top_idx
         do k = 1, size(transition_list(j)%c, dim=1)
            segment_list(j) = transition_list(j)%c(k)
         end do
      end do
   end subroutine transition_to_seg_list


end module forgex_nfa_node_m