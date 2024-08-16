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
      logical :: is_left_contains_union

      chara = ''
      is_left_contains_union = .false.

      call get_prefix_literal_internal(tree, root, chara, each_res, 0, is_left_contains_union)

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
   

   recursive subroutine get_prefix_literal_internal(tree, idx, prefix, res, parent, contains_union)
      use :: forgex_parameters_m
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32), intent(in) :: idx, parent
      character(:), allocatable, intent(inout) :: prefix
      logical, intent(inout) :: res, contains_union

      logical :: res_left, res_right
      type(tree_node_t) :: node, next_l, next_r
      character(:), allocatable :: cache_l, cache_r

      integer :: ci
      
      if (idx < 1) return
      node = tree(idx)
      res_left = .false.
      res_right = .false.

 
      ! print *,"L 97", idx, node%op, res, "|", res_left, res_right, "| ", prefix

      if (node%op == op_concat) then

         call get_prefix_literal_internal(tree, node%left_i, prefix, res_left, idx, contains_union)

         next_r = tree(node%right_i)

         if (res_left) then
            if (.not. contains_union) then
                call get_prefix_literal_internal(tree, node%right_i, prefix, res_right, idx, contains_union)
         
            else if (next_r%op == op_char) then
                call get_prefix_literal_internal(tree, node%right_i, prefix, res_right, idx, contains_union)
            end if
         end if

         res = res_left .and. res_right

      else if (node%op == op_union) then
         if (tree(parent)%right_i == idx) then
            contains_union = .true.
            res = .true.
            return
         end if
         contains_union = .true.
         call get_prefix_literal_internal(tree, node%left_i, prefix, res_left, idx, contains_union)
         res = .true.

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

   pure function extract_same_part_prefix (a, b) result(res)
      implicit none
      character(*), intent(in) :: a, b
      character(:), allocatable :: res

      integer :: i, n
      res = ''

      n = min(len(a), len(b))
      do i = 1, n
         if (a(i:i) == b(i:i)) then
            res = res//a(i:i)
         else
            return
         end if
      end do

   end function extract_same_part_prefix


end module forgex_syntax_tree_optimize_m