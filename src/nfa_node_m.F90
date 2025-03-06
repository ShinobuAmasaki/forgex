! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_nfa_node_m module is a part of Forgex.
!
!! This file contains `nfa_t` class and its type-bound procedures.

!> The `forgex_nfa_m` module defines the data structure of NFA.
!> The `nfa_t` is defined as a class representing NFA.
#ifdef IMPURE
#define pure
#endif
module forgex_nfa_node_m
   use, intrinsic :: iso_fortran_env, only: stderr=>error_unit, int32
   use :: forgex_parameters_m, only: TREE_NODE_BASE, TREE_NODE_LIMIT, ALLOC_COUNT_INITTIAL, &
      NFA_NULL_TRANSITION, NFA_STATE_BASE, NFA_TRANSITION_UNIT, NFA_STATE_UNIT, NFA_STATE_LIMIT, &
      NFA_C_SIZE, INFINITE
   use :: forgex_segment_m, only: segment_t, SEG_INIT, SEG_EPSILON, operator(/=), operator(==), &
      seg__merge_segments=>merge_segments, seg__sort_segments=>sort_segment_by_min

   use :: forgex_syntax_tree_graph_m, only: tree_t

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
      integer(int32) :: alloc_count_f = ALLOC_COUNT_INITTIAL
      integer(int32) :: alloc_count_b = ALLOC_COUNT_INITTIAL
      ! type(segment_t), allocatable :: all_segments(:)
   contains
      procedure :: add_transition => nfa__add_transition
      procedure :: realloc_f      => nfa__reallocate_transition_forward
      procedure :: realloc_b      => nfa__reallocate_transition_backward
      procedure :: merge_segments => nfa__merge_segments_of_transition
   end type

contains

   pure subroutine build_nfa_graph (tree, nfa, nfa_entry, nfa_exit, nfa_top, all_segments)
      use :: forgex_parameters_m, only: NFA_TRANSITION_UNIT
      implicit none
      type(tree_t),         intent(in)                :: tree
      type(nfa_state_node_t), intent(inout), allocatable :: nfa(:)
      integer(int32),         intent(inout)              :: nfa_entry
      integer(int32),         intent(inout)              :: nfa_exit
      integer(int32),         intent(inout)              :: nfa_top
      type(segment_t),        intent(inout), allocatable :: all_segments(:)


      integer(int32) :: i, i_begin, i_end ! index for states array

      i_begin = NFA_STATE_BASE
      i_end   = NFA_STATE_UNIT

      ! initialize
      nfa_top = 0

      allocate(nfa(i_begin:i_end))

      ! Initialize
      nfa(i_begin:i_end)%own_i = [(i, i =i_begin, i_end)]

      nfa(:)%alloc_count_f = 0
      nfa(:)%alloc_count_b = 0

      nfa(:)%forward_top = 1
      nfa(:)%backward_top = 1


      call make_nfa_node(nfa_top)
      nfa_entry = nfa_top

      call make_nfa_node(nfa_top)
      nfa_exit = nfa_top

      call generate_nfa(tree, tree%top, nfa, nfa_top, nfa_entry, nfa_exit)

      do i = 1, nfa_top
         call nfa(i)%merge_segments()
      end do

      call disjoin_nfa(nfa, nfa_top, all_segments)

   end subroutine build_nfa_graph


   pure subroutine nfa_deallocate(nfa)
      implicit none
      type(nfa_state_node_t), allocatable, intent(inout) :: nfa(:)
      integer :: i

      if (.not. allocated(nfa)) return

      do i = NFA_STATE_BASE, ubound(nfa, dim=1)
         if (allocated(nfa(i)%forward)) deallocate(nfa(i)%forward)
         if (allocated(nfa(i)%backward)) deallocate(nfa(i)%backward)
      end do

      deallocate(nfa)
   end subroutine nfa_deallocate


   pure subroutine make_nfa_node(nfa_top)
      implicit none
      integer(int32), intent(inout) :: nfa_top

      nfa_top = nfa_top + 1

   end subroutine make_nfa_node


   pure function is_exceeded (nfa_top, nfa_graph) result(res)
      implicit none
      integer(int32), intent(in) :: nfa_top
      type(nfa_state_node_t), intent(in) :: nfa_graph(:)
      logical :: res

      res = ubound(nfa_graph, dim=1) < nfa_top

   end function is_exceeded


   pure subroutine reallocate_nfa(nfa_graph)
      implicit none
      type(nfa_state_node_t), allocatable, intent(inout) :: nfa_graph(:)

      type(nfa_state_node_t), allocatable :: tmp(:)
      integer :: siz


      siz = ubound(nfa_graph, dim=1)

      call move_alloc(nfa_graph, tmp)

      allocate(nfa_graph(NFA_STATE_BASE:siz*2))

      nfa_graph(NFA_STATE_BASE:siz) = tmp(NFA_STATE_BASE:siz)

      nfa_graph(siz+1:siz*2)%forward_top = 1
      nfa_graph(siz+1:siz*2)%backward_top = 1

   end subroutine

   pure recursive subroutine generate_nfa(tree, idx, nfa_graph, nfa_top, entry, exit)
      use :: forgex_enums_m
      use :: forgex_parameters_m
      implicit none
      type(tree_t),  intent(in) :: tree
      type(nfa_state_node_t), allocatable, intent(inout) :: nfa_graph(:)
      integer(int32), intent(in) :: idx
      integer(int32), intent(inout) :: nfa_top
      integer(int32), intent(in) :: entry
      integer(int32), intent(in) :: exit

      integer :: i
      integer :: k
      integer :: node1
      integer :: node2
      integer :: entry_local

      if (idx == INVALID_INDEX) return
      i = idx
      entry_local = entry

      select case(tree%nodes(i)%op)
      case (op_char)
         if (.not. allocated(tree%nodes(i)%c)) then
            error stop "ERROR: Character node of the AST do not have actual character list."
         end if
         ! Handle character operations by adding transition for each character.
         do k = 1, size(tree%nodes(i)%c, dim=1)
            call nfa_graph(entry)%add_transition(nfa_graph, entry, exit, tree%nodes(i)%c(k))
         end do

      case (op_empty)
         ! Handle empty opration by adding an epsilon transition
         call nfa_graph(entry)%add_transition(nfa_graph, entry, exit, SEG_EPSILON)

      case (op_union)
         ! Handle union operation by recursively generating NFA for left and right subtrees.
         call generate_nfa(tree, tree%nodes(i)%left_i, nfa_graph, nfa_top, entry, exit)
         call generate_nfa(tree, tree%nodes(i)%right_i, nfa_graph, nfa_top, entry, exit)

      case (op_closure)
         ! Handle closure (Kleene star) operations by creating new node and adding appropriate transition
         call generate_nfa_closure(tree, idx, nfa_graph, nfa_top, entry, exit)


      case (op_concat)
         ! Handle concatenation operations by recursively generating NFA for left and right subtrees.
         call generate_nfa_concatenate(tree, idx, nfa_graph, nfa_top, entry, exit)

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
               call make_nfa_node(nfa_top)
               if (is_exceeded(nfa_top, nfa_graph)) call reallocate_nfa(nfa_graph)
               node1 = nfa_top
               call generate_nfa(tree, tree%nodes(i)%left_i, nfa_graph, nfa_top, entry_local, node1)
               entry_local = node1
            end do

            if (min_repeat == 0) then
               num_2nd_repeat = max_repeat - 1
            else
               num_2nd_repeat = max_repeat - min_repeat
            end if

            do j = 1, num_2nd_repeat
               call make_nfa_node(nfa_top)
               if (is_exceeded(nfa_top, nfa_graph)) call reallocate_nfa(nfa_graph)
               node2 = nfa_top

               call generate_nfa(tree, tree%nodes(i)%left_i, nfa_graph, nfa_top, entry_local, node2)
               call nfa_graph(node2)%add_transition(nfa_graph, node2, exit, SEG_EPSILON)
               entry_local = node2
            end do
            

            if (min_repeat == 0) then
               call nfa_graph(entry)%add_transition(nfa_graph, entry, exit, SEG_EPSILON)
            end if

            if (max_repeat == INFINITE) then
               call generate_nfa_closure(tree, idx, nfa_graph, nfa_top, entry_local, exit)
            else
               call generate_nfa(tree, tree%nodes(i)%left_i, nfa_graph, nfa_top, entry_local, exit)
            end if

         end block
      case default ! for case (op_not_init)
         ! Handle unexpected cases.
         error stop "This will not heppen in 'generate_nfa'."
      end select
   end subroutine generate_nfa


   pure recursive subroutine generate_nfa_concatenate(tree, idx, nfa_graph, nfa_top, entry, exit)
      implicit none
      type(tree_t),  intent(in) :: tree
      type(nfa_state_node_t), allocatable, intent(inout) :: nfa_graph(:)
      integer(int32), intent(in) :: idx
      integer(int32), intent(inout) :: nfa_top
      integer(int32), intent(in) :: entry
      integer(int32), intent(in) :: exit

      integer(int32) :: node1

      call make_nfa_node(nfa_top)
      if (is_exceeded(nfa_top, nfa_graph)) then
         call reallocate_nfa(nfa_graph)
      end if
      node1 = nfa_top

      call generate_nfa(tree, tree%nodes(idx)%left_i, nfa_graph, nfa_top, entry, node1)
      call generate_nfa(tree, tree%nodes(idx)%right_i, nfa_graph, nfa_top, node1, exit)

   end subroutine generate_nfa_concatenate

   pure recursive subroutine generate_nfa_closure(tree, idx, nfa_graph, nfa_top, entry, exit)
      implicit none
      type(tree_t),  intent(in) :: tree
      type(nfa_state_node_t), allocatable, intent(inout) :: nfa_graph(:)
      integer(int32), intent(in) :: idx
      integer(int32), intent(inout) :: nfa_top
      integer(int32), intent(in) :: entry
      integer(int32), intent(in) :: exit

      integer(int32) :: node1, node2

      call make_nfa_node(nfa_top)
      if (is_exceeded(nfa_top, nfa_graph)) then
         call reallocate_nfa(nfa_graph)
      end if
      node1 = nfa_top

      call make_nfa_node(nfa_top)
      if (is_exceeded(nfa_top, nfa_graph)) then
         call reallocate_nfa(nfa_graph)
      end if
      node2 = nfa_top

      call nfa_graph(entry)%add_transition(nfa_graph, entry, node1, SEG_EPSILON)

      call generate_nfa(tree, tree%nodes(idx)%left_i, nfa_graph, nfa_top, node1, node2)

      call nfa_graph(node2)%add_transition(nfa_graph, node2, node1, SEG_EPSILON)
      call nfa_graph(node1)%add_transition(nfa_graph, node1, exit, SEG_EPSILON)

   end subroutine generate_nfa_closure

   pure subroutine nfa__add_transition(self,nfa_graph, src, dst, c)
      use :: forgex_parameters_m, only: NFA_TRANSITION_UNIT
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      type(nfa_state_node_t), intent(inout) :: nfa_graph(:)
      integer(int32), intent(in) :: src, dst
      type(segment_t) ,intent(in) :: c

      integer(int32) :: j, jj, k

      !== Forward transition process
      j = NFA_NULL_TRANSITION
      if (allocated(self%forward) .and. c /= SEG_EPSILON) then
         ! ε遷移でない場合、同じ行き先の遷移があるかどうか検索する
         do jj = 1, self%forward_top
            if ( dst == self%forward(jj)%dst .and. self%forward(jj)%c_top < NFA_C_SIZE) then
               ! セグメント配列のサイズを超える場合には新しい遷移にセグメントを追加する
               j = jj
            end if
         end do
      end if

      if (j == NFA_NULL_TRANSITION) then
         j = self%forward_top
      end if

      !> @note Note that the return value of the size function on an unallocated array is undefined.
      if (.not. allocated(self%forward)) then
         ! Reallocate the forward array component.
         call self%realloc_f()
      endif

      if (j >= size(self%forward, dim=1)) then
         ! Reallocate the forward array component.
         call self%realloc_f()
      endif

      if (.not. allocated(self%forward(j)%c))  then
         allocate(self%forward(j)%c(1:NFA_C_SIZE))
      end if

      self%forward(j)%c_top = self%forward(j)%c_top + 1  ! Increment
      k = self%forward(j)%c_top

      self%forward(j)%c(k) = c
      self%forward(j)%dst = dst
      self%forward(j)%is_registered = .true.

      if (j == self%forward_top) self%forward_top = self%forward_top + 1

      !== Backward transition process
      j = NFA_NULL_TRANSITION
      if (allocated(nfa_graph(dst)%backward) .and. c /= SEG_EPSILON) then
         do jj = 1, nfa_graph(dst)%backward_top
            if (src == nfa_graph(dst)%backward(jj)%dst .and. nfa_graph(dst)%backward(jj)%c_top < NFA_C_SIZE) j = jj
               ! セグメント配列のサイズを超える場合には新しい遷移にセグメントを追加する
         end do
      end if

      if (j == NFA_NULL_TRANSITION) then
         j = nfa_graph(dst)%backward_top
      end if

      if (.not. allocated(nfa_graph(dst)%backward)) then
         ! Reallocate backward array component.
         call nfa_graph(dst)%realloc_b
      end if

      if (j >= size(nfa_graph(dst)%backward, dim=1)) then
         ! Reallocate backward array component.
         call nfa_graph(dst)%realloc_b
      endif

      if (.not. allocated(nfa_graph(dst)%backward(j)%c))  allocate(nfa_graph(dst)%backward(j)%c(NFA_C_SIZE))

      nfa_graph(dst)%backward(j)%c_top = nfa_graph(dst)%backward(j)%c_top + 1
      k = nfa_graph(dst)%backward(j)%c_top

      nfa_graph(dst)%backward(j)%c(k) = c
      nfa_graph(dst)%backward(j)%dst = src
      nfa_graph(dst)%backward(j)%is_registered = .true.

      if(j == nfa_graph(dst)%backward_top) nfa_graph(dst)%backward_top = nfa_graph(dst)%backward_top + 1
   end subroutine nfa__add_transition


   pure subroutine disjoin_nfa(graph, nfa_top, seg_list)
      use :: forgex_priority_queue_m
      use :: forgex_segment_m
      use :: forgex_segment_disjoin_m
      implicit none
      type(nfa_state_node_t), intent(inout) :: graph(:)
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
                     if (ptr%c(k) /= SEG_INIT) then
                        call queue_f%enqueue(ptr%c(k))
                     end if
                  end do
               end if
            end do
         end do
      end block

      ! Dequeue
      ! Allocate memory for the segment list and dequeue all segments for the priority queue.
      block
         integer :: m
         type(segment_t) :: cache
         num_f = queue_f%number

         allocate(seg_list(num_f))
         m = 0
         do j = 1, num_f
            if (j == 1) then
               m = m + 1
               call queue_f%dequeue(seg_list(j))
               cycle
            end if

            call queue_f%dequeue(cache)
            if (seg_list(m) /= cache) then
               m = m + 1
               seg_list(m) = cache
            end if
         end do

         !-- The seg_list arrays are now sorted.
         seg_list = seg_list(:m) ! reallocation implicitly
      end block

      !==  At this point, seg_list is always allocated.  ==!

      ! Disjoin the segment lists to ensure no over laps
      call disjoin(seg_list)

      if (.not. allocated(seg_list)) then
         error stop "ERROR: Array that should have been disjoined is not allocated."
      end if

      ! Apply disjoining to all transitions over the NFA graph.

      ! do concurrent (i = NFA_STATE_BASE:nfa_top)
      !    do concurrent (j = 1:graph(1)%forward_top)
      do i = NFA_STATE_BASE, nfa_top

         if (allocated(graph(i)%forward)) then
            do j = 1, graph(i)%forward_top
               call disjoin_nfa_each_transition(graph(i)%forward(j), seg_list)
            end do
         end if

         if (allocated(graph(i)%backward)) then
            do j = 1, graph(i)%backward_top
               call disjoin_nfa_each_transition(graph(i)%backward(j), seg_list)
            end do
         end if

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

      integer :: k
      if (.not. allocated(transition%c)) return

      k = 0
      do while(k+1 <= size(transition%c, dim=1))
         k = k + 1
         if (transition%c(k) == SEG_INIT) exit
      end do
      transition%c_top = k

   end subroutine update_c_top


   ! pure subroutine transition_to_seg_list(transition_list, top_idx, segment_list)
   !    implicit none
   !    type(nfa_transition_t),       intent(in)    :: transition_list(:)
   !    integer(int32),               intent(in)    :: top_idx
   !    type(segment_t), allocatable, intent(inout) :: segment_list(:)

   !    integer :: j, k

   !    allocate(segment_list(top_idx))

   !    do j = 1, top_idx
   !       do k = 1, size(transition_list(j)%c, dim=1)
   !          segment_list(j) = transition_list(j)%c(k)
   !       end do
   !    end do
   ! end subroutine transition_to_seg_list


   pure subroutine nfa__reallocate_transition_forward (self)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      type(nfa_transition_t), allocatable :: tmp(:)
      integer :: siz, j
      integer :: prev_count, new_part_begin, new_part_end

      siz = 0
      prev_count = 0
      new_part_begin = 0
      new_part_end = 0

      if (allocated(self%forward)) then
         siz = size(self%forward, dim=1)
         call move_alloc(self%forward, tmp)
      else
         siz = 0
      end  if

      prev_count = self%alloc_count_f
      self%alloc_count_f = prev_count + 1

      new_part_begin = (siz) + 1
      new_part_end = NFA_TRANSITION_UNIT * 2**self%alloc_count_f

      allocate(self%forward(1:new_part_end))

      if (allocated(tmp)) then
         do j = 1, siz
            self%forward(j) = tmp(j)
         end do
      end if

      self%forward(1:new_part_end)%own_j = &
         [(j, j= 1, new_part_end)]

   end subroutine nfa__reallocate_transition_forward


   pure subroutine nfa__reallocate_transition_backward (self)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self
      type(nfa_transition_t), allocatable :: tmp(:)
      integer :: siz, jj
      integer :: prev_count, new_part_begin, new_part_end

      siz = 0
      prev_count = 0
      new_part_begin = 0
      new_part_end = 0

      if (allocated(self%backward)) then
         siz = size(self%backward, dim=1)
         call move_alloc(self%backward, tmp)
      else
         siz = 0
      end  if

      prev_count = self%alloc_count_b
      self%alloc_count_b = prev_count + 1

      new_part_begin = (siz) + 1
      new_part_end =  NFA_TRANSITION_UNIT * 2**self%alloc_count_b

      allocate(self%backward(1:new_part_end))

      if(allocated(tmp)) self%backward(1:siz) = tmp(1:siz)

      self%backward(new_part_begin:new_part_end)%own_j = &
         [(jj, jj= new_part_begin, new_part_end)]

   end subroutine nfa__reallocate_transition_backward


   pure elemental subroutine nfa__merge_segments_of_transition(self)
      implicit none
      class(nfa_state_node_t), intent(inout) :: self

      integer :: j

      if(allocated(self%forward)) then
         do j = 1, self%forward_top
            if (allocated(self%forward(j)%c)) then
               call seg__sort_segments(self%forward(j)%c)
               call seg__merge_segments(self%forward(j)%c)
               self%forward(j)%c_top = size(self%forward(j)%c, dim=1)
            end if
         end do
      end if

      if (allocated(self%backward)) then
         do j = 1, self%backward_top
            if (allocated(self%backward(j)%c)) then
               call seg__sort_segments(self%backward(j)%c)
               call seg__merge_segments(self%backward(j)%c)
               self%backward(j)%c_top = size(self%backward(j)%c, dim=1)
            end if
         end do
      end if
   end subroutine nfa__merge_segments_of_transition

end module forgex_nfa_node_m