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
module forgex_syntax_tree_optimize_exp_m
   use :: forgex_syntax_tree_node_m, only: tree_node_t
   use :: forgex_syntax_tree_graph_m, only: tree_t
   use :: forgex_utf8_m, only: char_utf8
   use :: forgex_enums_m
   use :: forgex_parameters_m
   implicit none
   private

   type character_array_element_t
      character(:), allocatable :: c
   end type character_array_element_t

   type literal_t
      type(character_array_element_t) :: all, pref, suff, fact
      logical :: flag_closure = .false.
   end type literal_t

   character(0), parameter :: theta = ''

   public :: extract_literal

contains

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


   pure function get_literal(tree) result(literal)
      implicit none
      type(tree_t), intent(in) :: tree
      type(literal_t) :: literal

      call best_factor(tree%nodes, tree%top, literal)
   
   end function get_literal

   pure recursive subroutine best_factor(nodes, idx, lit)
      implicit none
      type(tree_node_t), intent(in) :: nodes(:)
      integer, intent(in) :: idx
      type(literal_t), intent(inout) :: lit

      type(literal_t) :: lit_l, lit_r
      type(tree_node_t) :: curr

      curr = nodes(idx)

      lit_l%all%c = theta
      lit_l%pref%c = theta
      lit_l%suff%c = theta
      lit_l%fact%c = theta

      lit_r%all%c = theta
      lit_r%pref%c = theta
      lit_r%suff%c = theta
      lit_r%fact%c = theta
      

      if (curr%op == op_union .or. curr%op == op_concat) then
         call best_factor(nodes, curr%left_i, lit_l)
         call best_factor(nodes, curr%right_i, lit_r)
      end if

      select case (curr%op)
      case(op_union)
         lit%all%c =  theta
         lit%pref%c = theta
         lit%suff%c = theta
         lit%fact%c = theta
         lit%flag_closure = .true.

      case(op_concat)

         if (lit_l%flag_closure .and. lit_r%flag_closure) then 
            lit%all%c = theta
            lit%pref%c = lit_l%pref%c
            lit%suff%c = lit_r%suff%c
            lit%flag_closure = .true.
         else if (lit_l%flag_closure) then
            lit%all%c =  theta
            lit%pref%c = lit_l%pref%c
            lit%suff%c = best(lit_r%suff%c, lit_l%suff%c//lit_r%all%c)
            lit%flag_closure = .true.
         else if (lit_r%flag_closure) then
            lit%all%c = theta
            lit%pref%c = best(lit_l%all%c//lit_r%pref%c, lit_l%pref%c)
            lit%suff%c = lit_r%suff%c
            lit%flag_closure = .true.
         else
            lit%all%c = lit_l%all%c//lit_r%all%c
            lit%pref%c = best(lit_l%pref%c, lit_l%all%c//lit_r%pref%c)
            lit%suff%c = best(lit_r%suff%c, lit_l%suff%c//lit_r%all%c)
         end if 

         lit%fact%c = best(best(lit_l%fact%c, lit_r%fact%c), lit_l%suff%c//lit_r%pref%c)

      case (op_closure)
         lit%all%c = theta
         lit%pref%c = theta
         lit%suff%c = theta
         lit%fact%c = theta
         lit%flag_closure = .true.

      case (op_char)
         lit%all%c = char_utf8(curr%c(1)%min)
         lit%pref%c = char_utf8(curr%c(1)%min)
         lit%suff%c =  char_utf8(curr%c(1)%min)
         lit%fact%c =  char_utf8(curr%c(1)%min)

      case (op_repeat)
         lit%all%c =  theta
         lit%pref%c = theta
         lit%suff%c = theta
         lit%fact%c = theta
         lit%flag_closure = .true.
      case default
         lit%all%c =  theta
         lit%pref%c = theta
         lit%suff%c = theta
         lit%fact%c = theta
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

end module forgex_syntax_tree_optimize_exp_m