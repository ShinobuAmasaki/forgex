module forgex_cli_memory_measurement_m
   use :: forgex_parameters_m
   implicit none
   
contains

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
         do j = lbound(graph%nodes(i)%forward, dim=1), ubound(graph%nodes(i)%forward, dim=1)
            if (.not. allocated(graph%nodes(i)%forward)) cycle
            sum_tra = sum_tra + 4*4 ! 3 int32, 1 logical
            if (allocated(graph%nodes(i)%forward(j)%c)) then
               sum_tra = sum_tra + 8*size(graph%nodes(i)%forward(j)%c)
            end if
         end do
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
      do i = DFA_STATE_BASE, ubound(graph%nodes, dim=1)
         sum_node = sum_node + 6 * 4 ! 3 int32, 3 logical
         sum_tra = 0
      end do

   end function mem_dfa_graph

end module forgex_cli_memory_measurement_m