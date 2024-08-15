module forgex_syntax_tree_optimize_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_syntax_tree_m
   use :: forgex_utf8_m
   use :: forgex_enums_m
   implicit none
   private

   public :: get_prefix_literal
   ! public :: get_suffix_literal
   public :: all_literals
contains

   function get_prefix_literal(tree, root) result(chara)
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32), intent(in) :: root
      character(:), allocatable :: chara
      logical :: each_res

      chara = ''

      call get_prefix_literal_internal(tree, root, chara, each_res, 0)

   end function get_prefix_literal

   
   ! pure function get_suffix_literal(tree, root) result(chara)
   !    implicit none
   !    type(tree_node_t), intent(in) :: tree(:)
   !    integer(int32), intent(in) :: root
   !    character(:), allocatable :: chara
   !    logical :: terminal_flag

   !    chara = ''

   !    call get_suffix_literal_internal(tree, root, chara, terminal_flag)

   ! end function get_suffix_literal
      

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
      end if

   end subroutine all_literals
   

   recursive subroutine get_prefix_literal_internal(tree, idx, prefix, res, parent)
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32), intent(in) :: idx, parent
      character(:), allocatable, intent(inout) :: prefix
      logical, intent(inout) :: res

      logical :: res_left, res_right
      type(tree_node_t) :: node
      
      
      node = tree(idx)
      res_left = .false.
      res_right = .false.


      ! print *,"L 97", idx, node%op, res, "|", res_left, res_right, "| ", prefix

      if (node%op == op_concat) then

         call get_prefix_literal_internal(tree, node%left_i, prefix, res_left, idx)
        
         if (res_left) call get_prefix_literal_internal(tree, node%right_i, prefix, res_right, idx)

         res = res_left .and. res_right
 
      else if (is_literal_tree_node(node)) then
         prefix = prefix//char_utf8(node%c(1)%min)
         res = .true.

      else
         res = .false.
      end if

      ! print *,"L133", idx, parent, node%op, res, "|", res_left, res_right, "| ", prefix

   end subroutine get_prefix_literal_internal


   ! pure recursive subroutine get_suffix_literal_internal(tree, idx, suffix, res)
   !    implicit none
   !    type(tree_node_t), intent(in) :: tree(:)
   !    integer(int32), intent(in) :: idx
   !    character(:), allocatable, intent(inout) :: suffix
   !    logical, intent(inout) :: res
   !    type(tree_node_t) :: node

   !    node = tree(idx)

   !    if (node%op == op_concat) then
   !       call get_suffix_literal_internal(tree, node%right_i, suffix, res)
   !       if (.not. res) return
   !       call get_suffix_literal_internal(tree, node%left_i, suffix, res)
   !       if (.not. res) return
   !    else if (is_literal_tree_node(node)) then
   !       suffix = char_utf8(node%c(1)%min)//suffix
   !       res = .true.
   !    else
   !       res = .false.
   !    end if
   ! end subroutine get_suffix_literal_internal



end module forgex_syntax_tree_optimize_m