! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_nfa_graph_m module is a part of Forgex.
!
!! This file contains a derived-type which represents the NFA graph using an array. 
#ifdef IMPURE
#define pure
#endif
!> This module defines the `nfa_graph_t` derived-type which represents the NFA graph.
module forgex_nfa_graph_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_nfa_node_m, only: nfa_state_node_t, nfa_transition_t
   use :: forgex_nfa_state_set_m, only: nfa_state_set_t, add_nfa_state, check_nfa_state
   use :: forgex_parameters_m, only: NFA_STATE_BASE, NFA_STATE_UNIT, NFA_STATE_LIMIT, NFA_NULL_TRANSITION
   use :: forgex_segment_m, only: SEG_EPSILON
   use :: forgex_cube_m, only: cube_t, assignment(=)
   implicit none

   type, public :: nfa_graph_t
      type(nfa_state_node_t), allocatable :: graph(:)
      integer(int32) :: top = 0
      integer(int32) :: entry = 0
      integer(int32) :: exit = 0
      integer(int32) :: nfa_base = NFA_STATE_BASE
      integer(int32) :: nfa_limit = NFA_STATE_LIMIT
   contains
      procedure :: new_nfa_node => nfa_graph__new_node
      procedure :: is_exceeded => nfa_graph__is_exceeded
      procedure :: reallocate => nfa_graph__reallocate
      procedure :: build => nfa_graph__build
      procedure :: collect_epsilon_transition => nfa_graph__collect_epsilon_transition
      procedure :: mark_epsilon_transition => nfa_graph__mark_epsilon_transition
      procedure :: disjoin => nfa_graph__disjoin
      procedure :: print => nfa_graph__print
   end type nfa_graph_t

contains

!=====================================================================!
   pure subroutine nfa_graph__new_node(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self

      self%top = self%top + 1
   end subroutine nfa_graph__new_node


   pure function nfa_graph__is_exceeded(self) result(ret)
      implicit none
      class(nfa_graph_t), intent(in) :: self
      logical :: ret

      ret = ubound(self%graph, dim=1) < self%top

   end function nfa_graph__is_exceeded


   pure subroutine nfa_graph__reallocate(self)
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(nfa_state_node_t), allocatable :: tmp(:)
      integer :: n

      n = ubound(self%graph, dim=1)
      call move_alloc(self%graph, tmp)

      allocate(self%graph(NFA_STATE_BASE:n*2))
      
      self%graph(NFA_STATE_BASE:n) = tmp(NFA_STATE_BASE:n)

      self%graph(n+1:n*2)%forward_top = 1
   end subroutine nfa_graph__reallocate


   pure subroutine nfa_graph__build(self, tree, entry_i, exit_i, entire)
      implicit none
      type(tree_t), intent(in) :: tree
      class(nfa_graph_t), intent(inout) :: self
      integer(int32), intent(inout) :: entry_i, exit_i
      type(cube_t), intent(inout) :: entire

      integer(int32) :: i, ib, ie ! index for states array

      ib = NFA_STATE_BASE
      ie = NFA_STATE_UNIT

      ! initialize
      self%top = 0
      allocate(self%graph(ib:ie))
      self%graph(ib:ie)%own_i = [(i, i=ib, ie)]
      self%graph(:)%alloc_count_f = 0
      self%graph(:)%forward_top = 1

      call self%new_nfa_node()
      entry_i = self%top
      call self%new_nfa_node()
      exit_i = self%top

      call generate_nfa(tree, tree%top, self, entry_i, exit_i)  ! ~0.8ms TOO SLOW

      do i = 1, self%top
         call self%graph(i)%merge_segment()
      end do

      call self%disjoin(entire)

   end subroutine nfa_graph__build


   pure recursive subroutine nfa_graph__mark_epsilon_transition(self, state_set, idx)
      use :: forgex_segment_m, only: operator(==)
      implicit none
      class(nfa_graph_t), intent(in) :: self
      type(nfa_state_set_t), intent(inout) :: state_set
      integer, intent(in) :: idx

      integer :: j, dst

      call add_nfa_state(state_set, idx)

      if (.not. allocated(self%graph(idx)%forward)) return

      do j = 1, self%graph(idx)%forward_top

         dst = self%graph(idx)%forward(j)%dst

         if (dst == NFA_NULL_TRANSITION) cycle

         if (self%graph(idx)%forward(j)%c%is_flagged_epsilon() .and. &
            .not. check_nfa_state(state_set, dst)) then

            if (dst /= NFA_NULL_TRANSITION) then
                call self%mark_epsilon_transition(state_set, dst)
            end if
            
         end if
      end do

   end subroutine nfa_graph__mark_epsilon_transition


   pure subroutine nfa_graph__collect_epsilon_transition(self, state_set)
      implicit none
      class(nfa_graph_t), intent(in) :: self
      type(nfa_state_set_t), intent(inout) :: state_set

      integer :: i

      do i = NFA_STATE_BASE, self%top
         if (check_nfa_state(state_set, i)) then
            call self%mark_epsilon_transition(state_set, i)
         end if
      end do
   end subroutine nfa_graph__collect_epsilon_transition


   pure subroutine nfa_graph__disjoin(self, cube)
      use :: forgex_priority_queue_m, only : priority_queue_t
      use :: forgex_segment_m, only: SEG_INIT, segment_t, operator(/=)
      use :: forgex_segment_disjoin_m, only: disjoin
      implicit none
      class(nfa_graph_t), intent(inout) :: self
      type(cube_t), intent(inout) :: cube 

      type(nfa_transition_t) :: node

      type(priority_queue_t) :: queue
      type(nfa_transition_t) :: tra

      integer :: i, j, k

      enqueue : block
         do i = NFA_STATE_BASE, self%top
            do j = 1, self%graph(i)%forward_top-1

               node = self%graph(i)%forward(j)

               if (node%dst /= NFA_NULL_TRANSITION) then
                  if (allocated(node%c%sps)) then
                     do k = 1,  size(node%c%sps, dim=1)

                        if (node%c%sps(k) /= SEG_INIT) then
                           call queue%enqueue(self%graph(i)%forward(j)%c%sps(k))
                        end if                     
               
                     end do
                  end if
               end if

            end do
         end do
      end block enqueue

      dequeue: block
         integer :: m, n
         type(segment_t) :: cache
         n = queue%number

         allocate(cube%sps(n))
         m = 0
         do j = 1, n
            if (j == 1) then
               m = m + 1
               call queue%dequeue(cube%sps(j))
               cycle
            end if

            call queue%dequeue(cache)
            if (cube%sps(m) /= cache) then
               m = m + 1
               cube%sps(m) = cache
            end if
         end do 
         if (m > 0) cube%sps(1:m) = cube%sps(1:m) ! reallocation implicitly

      end block dequeue

      call disjoin(cube%sps)

      if (.not. allocated(cube%sps)) then
         error stop "ERROR: Array that should have been disjoined is not allocated."
      end if

      ! Apply disjoining to all transitions over the NFA graph.

      do i = NFA_STATE_BASE, self%top
         if (allocated(self%graph(i)%forward)) then
            do j = 1, self%graph(i)%forward_top
               call disjoin_nfa_each_transition(self%graph(i)%forward(j), cube%sps)
            end do
         end if
      end do

      call queue%clear()

   end subroutine nfa_graph__disjoin


   !> This subroutine updates the NFA state transitions by disjoining the segments.
   !>
   !> It breaks down overlapping segments into non-overlapping segments,
   !>  and creates new transitions accordingly.
   pure subroutine disjoin_nfa_each_transition(transition, seg_list)
      use :: forgex_segment_m, only: segment_t
      use :: forgex_segment_disjoin_m
      implicit none
      type(nfa_transition_t), intent(inout) :: transition
      type(segment_t),        intent(in) :: seg_list(:)

      type(segment_t), allocatable ::  tmp(:)

      integer :: k, m, n, siz

      if (.not. allocated(transition%c%sps)) return

      siz = size(seg_list, dim=1)

      allocate(tmp(siz))

      block
         logical :: flag(siz)

         n = 0 ! to count valid disjoined segments.
         do k = 1, size(transition%c%sps, dim=1)

            flag(:) = is_overlap_to_seg_list(transition%c%sps(k), seg_list, siz)

            do m = 1, siz
               if (flag(m)) then
                  n = n + 1
                  tmp(n) = seg_list(m)
               end if
            end do

         end do
      end block

      if (n == 0) return

      if (allocated(transition%c%sps)) then
         if (size(transition%c%sps, dim=1) < n) then
            deallocate(transition%c%sps)
            allocate(transition%c%sps(n))
         end if
      end if

      ! Deep copy the result into the arguemnt's component
      do k = 1, n
         transition%c%sps(k) = tmp(k)
      end do

      deallocate(tmp)
   end subroutine disjoin_nfa_each_transition


   subroutine nfa_graph__print(self, uni, nfa_exit)
      use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
      use :: forgex_segment_m, only: segment_t, SEG_INIT, operator(==)
      implicit none
      class(nfa_graph_t), intent(in) :: self
      integer(int32), intent(in) :: uni
      integer(int32), intent(in) :: nfa_exit

      type(nfa_state_node_t) :: node
      type(nfa_transition_t) :: transition
      character(:), allocatable :: buf
      integer(int32) :: i, j, k

      type(segment_t), allocatable :: seglist(:)

      do i = self%nfa_base, self%top

         write(uni, '(a, i4, a)', advance='no') "state ", i, ": "
         ! node = self%graph(i)
         if (i == nfa_exit) then
            write(uni, '(a)') "<Accepted>"
            cycle
         end if

         do j = 1, self%graph(i)%forward_top
            if (.not. allocated(self%graph(i)%forward)) cycle

            ! transition = self%graph(i)%forward(j)
            if (self%graph(i)%forward(j)%c%is_flagged_epsilon()) then
               write(uni, '(a,a,a2,i0,a1)', advance='no') "(", "?", ", ", self%graph(i)%forward(j)%dst, ")"
            end if

            call self%graph(i)%forward(j)%c%cube2seg(seglist)

            if (self%graph(i)%forward(j)%dst /= NFA_NULL_TRANSITION .and. allocated(seglist)) then
               do k = 1, size(seglist, dim=1)
                  if (seglist(k) == SEG_INIT) cycle

                  buf = seglist(k)%print()
                  write(uni, '(a,a,a2,i0,a1)', advance='no') "(", trim(buf), ", ", self%graph(i)%forward(j)%dst, ")"

               enddo
            end if
         end do

         write(uni, '(a)') ""
      end do
   end subroutine nfa_graph__print
!=====================================================================!

   pure recursive subroutine generate_nfa(tree, idx, nfa, entry_i, exit_i)
      use :: forgex_enums_m, only: op_char, op_empty, op_closure, op_concat, op_repeat, op_union
      use :: forgex_parameters_m, only: INFINITE, INVALID_INDEX

      implicit none
      type(tree_t), intent(in) :: tree
      integer(int32), intent(in) :: idx
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(in) :: entry_i, exit_i

      integer :: i
      integer :: k
      integer :: node1, node2, entry_local

      if (idx == INVALID_INDEX) return
      i = idx
      entry_local = entry_i

      select case(tree%nodes(i)%op)
      case (op_char)
         ! Handle character operations by adding transition for each character.
         call nfa%graph(entry_i)%add_transition(entry_i, exit_i, tree%nodes(i)%c)
      
      case (op_empty)
         ! Handle empty opration by adding an epsilon transition
         call nfa%graph(entry_i)%add_transition(entry_i, exit_i, [SEG_EPSILON])
      
      case (op_closure)
         ! Handle closure (Kleene star) operations by creating new node and adding appropriate transition
         call generate_nfa_closure(tree, idx, nfa, entry_i, exit_i)
      case (op_union)
         ! Handle union operation by recursively generating NFA for left and right subtrees.
         call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_i, exit_i)
         call generate_nfa(tree, tree%nodes(i)%right_i, nfa, entry_i, exit_i)

      case (op_concat)
         ! Handle concatenation operations by recursively generating NFA for left and right subtrees.
         call generate_nfa_concatenate(tree, idx, nfa, entry_i, exit_i)
      
      case (op_repeat)
         block
            integer(int32) :: min_repeat, max_repeat, j
            integer(int32) :: num_1st_repeat, num_2nd_repeat
            min_repeat = tree%nodes(i)%min_repeat
            max_repeat = tree%nodes(i)%max_repeat

            num_1st_repeat = min_repeat-1
            if (max_repeat == INFINITE) then
               num_1st_repeat = num_1st_repeat +1
            end if

            do j = 1, num_1st_repeat
               call nfa%new_nfa_node()

               if (nfa%is_exceeded()) call nfa%reallocate()

               node1 = nfa%top
               call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_local, node1)
               entry_local = node1
            end do

            if (min_repeat == 0) then
               num_2nd_repeat = max_repeat - 1
            else
               num_2nd_repeat = max_repeat - min_repeat
            end if

            do j = 1, num_2nd_repeat
               call nfa%new_nfa_node()
               if (nfa%is_exceeded()) call nfa%reallocate()
               node2 = nfa%top

               call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_local, node2)
               call nfa%graph(node2)%add_transition(node2, exit_i, [SEG_EPSILON])

               entry_local = node2
            end do
            

            if (min_repeat == 0) then
               call nfa%graph(entry_i)%add_transition(entry_i, exit_i, [SEG_EPSILON])
            end if

            if (max_repeat == INFINITE) then
               call generate_nfa_closure(tree, idx, nfa, entry_local, exit_i)
            else
               call generate_nfa(tree, tree%nodes(i)%left_i, nfa, entry_local, exit_i)
            end if

         end block
      case default ! for case (op_not_init)
         ! Handle unexpected cases.
         error stop "This will not heppen in 'generate_nfa'."
         ! error stop tree%nodes(i)%op
      end select
   end subroutine generate_nfa


   pure recursive subroutine generate_nfa_concatenate(tree, idx, nfa, entry_i, exit_i)
      implicit none
      type(tree_t), intent(in) :: tree
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(in) :: idx
      integer(int32), intent(in) :: entry_i, exit_i

      integer(int32) :: node1

      call nfa%new_nfa_node()
      if (nfa%is_exceeded()) call nfa%reallocate()

      node1 = nfa%top

      call generate_nfa(tree, tree%nodes(idx)%left_i, nfa, entry_i, node1)
      call generate_nfa(tree, tree%nodes(idx)%right_i, nfa, node1, exit_i)

   end subroutine generate_nfa_concatenate


   pure recursive subroutine generate_nfa_closure(tree, idx, nfa, entry_i, exit_i)
      implicit none
      type(tree_t), intent(in) :: tree
      type(nfa_graph_t), intent(inout) :: nfa
      integer(int32), intent(in) :: idx
      integer(int32), intent(in) :: entry_i, exit_i

      integer(int32) :: node1, node2

      call nfa%new_nfa_node()
      if (nfa%is_exceeded()) call nfa%reallocate()
      node1 = nfa%top

      call nfa%new_nfa_node()
      if (nfa%is_exceeded()) call nfa%reallocate()
      node2 = nfa%top

      call nfa%graph(entry_i)%add_transition(entry_i, node1, [SEG_EPSILON])
      call generate_nfa(tree, tree%nodes(idx)%left_i, nfa, node1, node2)

      call nfa%graph(node2)%add_transition(node2, node1, [SEG_EPSILON])
      call nfa%graph(node1)%add_transition(node1, exit_i, [SEG_EPSILON])

   end subroutine generate_nfa_closure


end module forgex_nfa_graph_m
   