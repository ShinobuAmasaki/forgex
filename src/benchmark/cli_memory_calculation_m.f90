! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_memory_calculation_m module is a part of Forgex.
!
module forgex_cli_memory_calculation_m
   use :: forgex_parameters_m, only: NFA_STATE_BASE
   implicit none
   private

   public :: mem_tape
   public :: mem_tree
   public :: mem_nfa_graph
   public :: mem_dfa_graph

contains

   function mem_tape(tape) result(res)
      use :: forgex_syntax_tree_m
      implicit none
      type(tape_t), intent(in) :: tape
      integer :: res

      res = len(tape%str)
      res = res + 12
   end function mem_tape


   function mem_tree(tree) result(res)
      use :: forgex_syntax_tree_m
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer :: res, sum_c, i

      res = size(tree, dim=1) * 6 * 4 ! 5 int32, 1 logical

      sum_c = 0
      do i = lbound(tree, dim=1), ubound(tree,dim=1)
         if (allocated(tree(i)%c)) then
            sum_c = sum_c + size(tree(i)%c) * 8 ! 8bytes per segment
         end if
      end do
      res = res + sum_c

   end function mem_tree


   function mem_nfa_graph(graph) result(res)
      use :: forgex_nfa_graph_m
      implicit none
      type(nfa_graph_t), intent(in) :: graph
      integer :: res, sum_node, sum_tra, i, j

      res = 12 ! 3 int32

      sum_node = 0
      do i = NFA_STATE_BASE, graph%nfa_top
         sum_node = sum_node + 5*4 ! 5 int32
         sum_tra = 0
         if (.not. allocated(graph%nodes(i)%forward)) cycle

         b: do j = lbound(graph%nodes(i)%forward, dim=1), ubound(graph%nodes(i)%forward, dim=1)
            if (.not. allocated(graph%nodes(i)%forward)) cycle b
            sum_tra = sum_tra + 4*4 ! 3 int32, 1 logical
            if (allocated(graph%nodes(i)%forward(j)%c)) then
               sum_tra = sum_tra + 8*size(graph%nodes(i)%forward(j)%c)
            end if
         end do b

         sum_node = sum_node + sum_tra*2 ! forward and backward
      end do
      res = res + sum_node
      res = res + (ubound(graph%nodes, dim=1) - graph%nfa_top)*5 ! 5 int32

   end function mem_nfa_graph


   function mem_dfa_graph(graph) result(res)
      use :: forgex_lazy_dfa_graph_m
      implicit none
      type(dfa_graph_t), intent(in) :: graph
      integer :: res, sum_node, sum_tra, i, j

      res = 16 ! 4 int32

      sum_node = 0
      do i = 1, graph%dfa_top-1
         sum_node = sum_node + 6 * 4 ! 3 int32, 3 logical
         if (allocated(graph%nodes(i)%nfa_set%vec)) then
            sum_node = sum_node + size(graph%nodes(i)%nfa_set%vec)*4 ! logical vector
         end if

         sum_tra = 0
         inner: do j = 1, graph%nodes(i)%get_tra_top()
            sum_tra = sum_tra + 8 + 4*2 ! segment + 2 int32
            if (.not. allocated(graph%nodes(i)%transition)) cycle inner
            if ( allocated(graph%nodes(i)%transition(j)%nfa_set%vec)) then
               sum_tra = sum_tra + size(graph%nodes(i)%transition(j)%nfa_set%vec)*4
            end if
         end do inner
         sum_node = sum_node + sum_tra
      end do
      res = res + sum_node
      res = res + (ubound(graph%nodes, dim=1) - graph%dfa_top)*6*4 ! 3 int32, 3 logical
   end function mem_dfa_graph

end module forgex_cli_memory_calculation_m