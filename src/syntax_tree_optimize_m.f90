module forgex_syntax_tree_optimize_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_syntax_tree_m
   use :: forgex_utf8_m
   use :: forgex_enums_m
   implicit none

contains

   pure function is_literal_tree_node(node) result(res)
      implicit none
      type(tree_node_t), intent(in) :: node
      logical :: res

      res = .false.
      if (node%op == op_char .and. size(node%c) == 1) then
         if (node%c(1)%min == node%c(1)%max) then
            res = .true.
         end if
      end if
   end function is_literal_tree_node


   pure recursive subroutine all_literals(tree, idx, literal)
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32), intent(in) :: idx
      character(:), allocatable, intent(inout) :: literal
      type(tree_node_t) :: node

      node = tree(idx)

      if (node%op == op_concat) then
         call all_literals(tree, node%left_i, literal) 
         if (literal == '') return
         call all_literals(tree, node%right_i, literal)
         if (literal == '') return
      else if (is_literal_tree_node(node)) then
         literal = literal//char_utf8(node%c(1)%min)
      else
         literal = ''
         return
      end if

   end subroutine all_literals
   
   


end module forgex_syntax_tree_optimize_m