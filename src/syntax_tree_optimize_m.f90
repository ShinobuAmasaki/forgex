module forgex_syntax_tree_optimize_m
   use, intrinsic :: iso_fortran_env, only: int32
   use :: forgex_syntax_tree_node_m, only: tree_node_t
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_utf8_m
   use :: forgex_enums_m
   implicit none
   private

   public :: get_prefix_literal
   ! public :: get_suffix_literal
   public :: all_literals
contains

   function get_prefix_literal(tree) result(chara)
      implicit none
      type(tree_t), intent(in) :: tree
      character(:), allocatable :: chara
      logical :: each_res
      logical :: is_left_contains_union

      chara = ''
      is_left_contains_union = .false.

      call get_prefix_literal_internal(tree%nodes, tree%top, chara, each_res, 0, is_left_contains_union)

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
      use :: forgex_syntax_tree_node_m
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

      integer :: ci
      
      node = tree(idx)
      res_left = .false.
      res_right = .false.

      select case (node%op)
      case (op_concat)

         call get_prefix_literal_internal(tree, node%left_i, prefix, res_left, idx, contains_union)

         next_r = tree(node%right_i)

         if (res_left) then
            if (.not. contains_union) then
                call get_prefix_literal_internal(tree, node%right_i, prefix, res_right, idx, contains_union)
            end if
         end if

         res = res_left .and. res_right

      case(op_union)
         block
            character(:), allocatable :: candidate1, candidate2
            candidate1 = ''
            candidate2 = ''
            call get_prefix_literal_internal(tree, node%left_i, candidate1, res_left, idx, contains_union)
            call get_prefix_literal_internal(tree, node%right_i, candidate2, res_right, idx, contains_union)
            prefix = extract_same_part_prefix(candidate1, candidate2)
         end block
         contains_union = .true.
         res = .true.

      case(op_repeat)
         block
            integer :: j, n
            n = node%min_repeat
            do j = 1, n
               call get_prefix_literal_internal(tree, node%left_i, prefix, res_right, idx, contains_union)
            end do
         end block

      case default
         if (is_literal_tree_node(node)) then
            prefix = prefix//char_utf8(node%c(1)%min)
            res = .true.
         else
            res = .false.
         end if
      end select
   end subroutine get_prefix_literal_internal


   pure recursive subroutine get_postfix_literal_internal(tree, idx, postfix, res, parent, contains_union)
      implicit none
      type(tree_node_t), intent(in) :: tree(:)
      integer(int32), intent(in) :: idx
      character(:), allocatable, intent(inout) :: postfix
      integer(int32), intent(in) :: parent
      logical, intent(inout) :: res, contains_union
      
      logical :: res_left, res_right, unused
      type(tree_node_t) :: node, next_l, next_r
      character(:), allocatable :: candidate1, candidate2

      node = tree(idx)
      res_left = .false.
      res_right = .false.
      candidate1 = ''
      candidate2 = ''

      select case (node%op)
      case (op_concat)
         call get_postfix_literal_internal(tree, node%right_i, postfix, res, idx, contains_union)
         next_l = tree(node%left_i)

         if (res_right) then
            if (.not. contains_union) then
               call get_postfix_literal_internal(tree, node%left_i, postfix, res, idx, contains_union)
            end if
         end if 

         res = res_left .and. res_right
      case (op_union)
         call get_postfix_literal_internal(tree, node%right_i, candidate1, unused, idx, contains_union)
         call get_postfix_literal_internal(tree, node%left_i, candidate2, unused, idx, contains_union)
         postfix = extract_same_part_postfix(candidate1, candidate2)
         contains_union = .true.
         res = .true.
      case default
         if (is_literal_tree_node(node)) then
            postfix = char_utf8(node%c(1)%min)//postfix
            res = .true.
         else
            res = .false.
         end if
      end select

   end subroutine get_postfix_literal_internal

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

   pure function extract_same_part_postfix (a, b) result(res)
      implicit none
      character(*), intent(in) :: a, b
      character(:), allocatable :: res
   end function extract_same_part_postfix
end module forgex_syntax_tree_optimize_m