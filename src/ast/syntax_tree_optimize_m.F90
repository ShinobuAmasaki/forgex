! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_syntax_tree_optimize_m module is a part of Forgex.
!
#ifdef IMPURE
#define pure
#endif
module forgex_syntax_tree_optimize_m
   use :: forgex_syntax_tree_node_m, only: tree_node_t
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_utf8_m, only: char_utf8, reverse_utf8
   use :: forgex_parameters_m, only: INVALID_INDEX, INVALID_CHAR_INDEX
   use :: forgex_segment_m, only: width_of_segment
   use :: forgex_enums_m
   implicit none
   private

   !> This type is wrapper to make a allocatable character array with variable length.
   type character_array_element_t
      character(:), allocatable :: c
   end type character_array_element_t

   !> This type contains a character variables that represents each literal:
   !> all, pref, suff, and fact.
   type literal_t
      type(character_array_element_t) :: all, pref, suff, fact
      logical :: flag_closure = .false.
      logical :: flag_class = .false.
   end type literal_t

   character(0), parameter :: theta = ''

   public :: extract_literal

contains

   !> This is the public procedure of this module to obtain each literal from AST.
   pure subroutine extract_literal(tree, all, prefix, suffix, factor)
      implicit none
      type(tree_t), intent(in) :: tree
      character(:), allocatable, intent(inout) :: all, prefix, suffix, factor

      type(literal_t) :: literal

      literal = get_literal(tree)

      all = literal%all%c
      prefix = literal%pref%c
      suffix = literal%suff%c
      factor = literal%fact%c
   end subroutine extract_literal


   !> Wrapping function to retrieve literals: all, prefix, suffix, factor.
   pure function get_literal(tree) result(literal)
      implicit none
      type(tree_t), intent(in) :: tree
      type(literal_t) :: literal

      ! Recursive procedure calls start here.
      call best_factor(tree%nodes, tree%top, literal)

   end function get_literal


   !> This is recursive procedure to tour a given syntax tree.
   pure recursive subroutine best_factor(nodes, idx, lit)
      implicit none
      type(tree_node_t), intent(in) :: nodes(:)
      integer, intent(in) :: idx
      type(literal_t), intent(inout) :: lit

      type(literal_t) :: lit_l, lit_r
      type(tree_node_t) :: curr
      integer :: i

      curr = nodes(idx)
      lit%all%c = theta
      lit%pref%c = theta
      lit%suff%c = theta
      lit%fact%c = theta

      if (curr%op == op_union .or. curr%op == op_concat) then
         call best_factor(nodes, curr%left_i, lit_l)
         call best_factor(nodes, curr%right_i, lit_r)
      end if

      select case (curr%op)

      case(op_union)
         lit%pref%c = same_part_of_prefix(lit_l%pref%c, lit_r%pref%c)
         lit%suff%c = same_part_of_suffix(lit_l%suff%c, lit_r%suff%c)
         lit%flag_closure = .true.

      case(op_concat)
! #ifdef IMPURE
! write(0,*) "L103: ", lit_l%flag_class, lit_r%flag_class, lit_l%flag_closure, lit_r%flag_closure
! #endif
         lit%flag_class = lit_l%flag_class .or. lit_r%flag_class
         lit%flag_closure = lit_l%flag_closure .or. lit_r%flag_closure

         select case (return_class_closure(lit_l%flag_class, lit_r%flag_class, lit_l%flag_closure, lit_r%flag_closure))
         case (lt_N_class_N_closure)
            lit%all%c = lit_l%all%c//lit_r%all%c
            lit%pref%c = best(lit_l%pref%c, lit_l%all%c//lit_r%pref%c)
            lit%suff%c = best(lit_r%suff%c, lit_l%suff%c//lit_r%all%c)

         case (lt_N_class_R_closure)
            lit%pref%c = lit_l%all%c//lit_r%pref%c
            lit%suff%c = lit_r%suff%c           

         case (lt_N_class_L_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_l%suff%c//lit_r%all%c

         case (lt_N_class_LR_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c

         ! following 12 cases are not tested enough.
         !===========================================================================!
         case (lt_R_class_N_closure)
            lit%pref%c = best(lit_l%pref%c, lit_l%all%c//lit_r%pref%c)
            lit%suff%c = lit_r%suff%c

         case (lt_R_class_R_closure)
            lit%pref%c = best(lit_l%pref%c, lit_l%all%c//lit_r%pref%c)
            lit%suff%c = lit_r%suff%c

         case (lt_R_class_L_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            ! lit%fact%c = best(lit_r%suff%c, lit_l%suff%c//lit_r%all%c)

         case (lt_R_class_LR_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c

         !===========================================================================!
         case (lt_L_class_N_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = best(lit_r%suff%c, lit_l%suff%c//lit_r%all%c)

         case (lt_L_class_R_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            continue
         case (lt_L_class_L_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = best(lit_r%suff%c, lit_l%suff%c//lit_r%all%c)
            continue
         case (lt_L_class_LR_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            continue
         !===========================================================================!
         case (lt_LR_class_N_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            continue
         case (lt_LR_class_R_closure)
            lit%pref%c = lit_l%pref%c
            lit%pref%c = lit_l%pref%c
            continue
         case (lt_LR_class_L_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            continue
         case (lt_LR_class_LR_closure)
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            continue
         end select

         !== Intermediate literals (factors) are not implemented and tested yet.
         ! lit%fact%c = best(best(lit_l%fact%c, lit_r%fact%c), lit_l%suff%c//lit_r%pref%c)

      case (op_closure)
         lit%flag_closure = .true.

      case (op_char)
         if (allocated(curr%c)) then
            if (size(curr%c) == 1) then
               if (width_of_segment(curr%c(1)) == 1) then
                  lit%all%c = char_utf8(curr%c(1)%min)
                  lit%pref%c = char_utf8(curr%c(1)%min)
                  lit%suff%c =  char_utf8(curr%c(1)%min)
                  lit%fact%c =  char_utf8(curr%c(1)%min)
               else
                  lit%flag_class = .true.
               end if
            else
               lit%flag_class = .true.
            end if

         end if
      case (op_repeat)
         block
            type(tree_node_t) :: next_l

            ! Is the flag_class of the next node flagged? 
            call best_factor(nodes, curr%left_i, lit_l)
            lit%flag_class = lit_l%flag_class

            do i = 1, curr%min_repeat       
               call best_factor(nodes, curr%left_i, lit_l)
               lit%all%c = lit%all%c//lit_l%all%c
               lit%pref%c = lit%pref%c//lit_l%pref%c
               lit%suff%c = lit%suff%c//lit_l%suff%c
               lit%fact%c = lit%fact%c//lit_l%fact%c
               lit%flag_class = lit%flag_class .or. lit_l%flag_class
               if (lit_l%flag_closure) exit
            end do

            lit%flag_closure = curr%min_repeat /= curr%max_repeat
            lit%flag_closure = lit%flag_closure .or. lit_l%flag_closure
         end block

      case default
         lit%flag_closure = .true.
      end select
   end subroutine best_factor


   pure function best(c1, c2) result(res)
      implicit none
      character(*), intent(in) :: c1, c2
      character(:), allocatable :: res

      integer :: max_len
      res = theta
      if (len_trim(c1) > len_trim(c2)) then
         res = trim(adjustl(c1))
      else
         res = trim(adjustl(c2))
      end if
   end function best


   pure function same_part_of_prefix (c1, c2) result(res)
      use :: forgex_utf8_m
      implicit none
      character(*), intent(in) :: c1, c2

      character(:), allocatable :: res, part1, part2
      logical :: flag_return
      integer :: i
      res = ''

      i = 1
      flag_return = .false.
      do while(.not. flag_return)

         part1 = c1(i:idxutf8(c1, i))
         part2 = c2(i:idxutf8(c2, i))
         flag_return = next_idxutf8(c1, i) == INVALID_CHAR_INDEX .or. next_idxutf8(c2,i) == INVALID_CHAR_INDEX

         if (flag_return) return

         if (part1 == part2) then
            res = res//part1
         else
            exit
         end if

         i = next_idxutf8(c1, i)
      end do

   end function same_part_of_prefix


   pure function same_part_of_suffix(c1,c2) result(retval)
      character(*), intent(in) :: c1, c2
      character(:), allocatable :: retval

      character(:), allocatable :: rc1, rc2
      integer :: n, i
      n = max(len_trim(c1), len_trim(c2))

      allocate(character(n):: rc1, rc2)

      rc1 = reverse_utf8(c1)   
      rc2 = reverse_utf8(c2)
      retval = same_part_of_prefix(rc1, rc2)
      retval = reverse_utf8(retval)

   end function same_part_of_suffix


   pure function return_class_closure(f_L_class, f_R_class, f_L_closure, f_R_closure) result(retval)
      implicit none
      logical, intent(in) :: f_L_class, f_r_class, f_l_closure, f_r_closure
      integer :: retval

      if (.not. f_L_class) then
         if (.not. f_r_class) then
            if (.not. f_l_closure) then
               if (.not. f_r_closure) then
                  retval = lt_N_class_N_closure
               else
                  retval = lt_N_class_R_closure
               end if
            else
               if (.not. f_r_closure) then
                  retval = lt_N_class_L_closure
               else
                  retval = lt_N_class_LR_closure
               end if
            end if
         else
            if (.not. f_l_closure) then
               if (.not. f_r_closure) then
                  retval = lt_R_class_N_closure
               else
                  retval = lt_R_class_R_closure
               endif
            else
               if (.not. f_r_closure) then
                  retval = lt_R_class_L_closure
               else
                  retval = lt_R_class_LR_closure
               end if
            end if
         end if
      else
         if (.not. f_r_class) then
            if (.not. f_l_closure) then
               if (.not. f_r_closure) then
                  retval = lt_L_class_N_closure
               else
                  retval = lt_L_class_R_closure
               end if
            else
               if (.not. f_r_closure) then
                  retval = lt_L_class_L_closure
               else
                  retval = lt_L_class_LR_closure
               end if
            end if
         else
            if (.not. f_l_closure) then
               if (.not. f_r_closure) then
                  retval = lt_LR_class_N_closure
               else
                  retval = lt_LR_class_R_closure
               endif
            else
               if (.not. f_r_closure) then
                  retval = lt_LR_class_L_closure
               else
                  retval = lt_LR_class_LR_closure
               end if
            end if
         end if   
      end if
   end function return_class_closure

end module forgex_syntax_tree_optimize_m