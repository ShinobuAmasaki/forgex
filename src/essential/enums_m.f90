! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_enums_m module is a part of Forgex.
!
!! This file contains enumeratorsl for syntactic parsing and building a syntax-tree.

!> The `forgex_enums_m` defines enumerators of tokens and operators for syntax-tree building.
!> @note These enums will be rewritten in Fortran 2023's enumerator in the future.
module forgex_enums_m
   implicit none

   enum, bind(c)
      enumerator :: tk_char = 0
      enumerator :: tk_union        ! 1
      enumerator :: tk_lpar         ! 2
      enumerator :: tk_rpar         ! 3
      enumerator :: tk_backslash    ! 4
      enumerator :: tk_question     ! 5
      enumerator :: tk_star         ! 6
      enumerator :: tk_plus         ! 7
      enumerator :: tk_lsbracket    ! 8  left square bracket
      enumerator :: tk_rsbracket    ! 9  right square bracket
      enumerator :: tk_lcurlybrace  ! 10 left curly brace
      enumerator :: tk_rcurlybrace  ! 11 right curly brace
      enumerator :: tk_dot          ! 12
      enumerator :: tk_hyphen       ! 13
      enumerator :: tk_caret        ! 14
      enumerator :: tk_dollar       ! 15
      enumerator :: tk_end          ! 16
   end enum

   enum, bind(c)
      enumerator :: op_not_init = 0 ! 0
      enumerator :: op_char         ! 1
      enumerator :: op_concat       ! 2
      enumerator :: op_union        ! 3
      enumerator :: op_closure      ! 4
      enumerator :: op_repeat       ! 5
      enumerator :: op_empty        ! 6  for epsilon transition
   end enum

   enum, bind(c)
      enumerator :: lt_N_class_N_closure
      enumerator :: lt_N_class_R_closure
      enumerator :: lt_N_class_L_closure
      enumerator :: lt_N_class_LR_closure
      enumerator :: lt_R_class_N_closure
      enumerator :: lt_R_class_R_closure
      enumerator :: lt_R_class_L_closure
      enumerator :: lt_R_class_LR_closure
      enumerator :: lt_L_class_N_closure
      enumerator :: lt_L_class_R_closure
      enumerator :: lt_L_class_L_closure
      enumerator :: lt_L_class_LR_closure
      enumerator :: lt_LR_class_N_closure
      enumerator :: lt_LR_class_R_closure
      enumerator :: lt_LR_class_L_closure
      enumerator :: lt_LR_class_LR_closure
   end enum

   enum, bind(c)
      enumerator :: FLAG_INVALID = 0
      enumerator :: FLAG_HELP
      enumerator :: FLAG_VERBOSE
      enumerator :: FLAG_NO_TABLE
      enumerator :: FLAG_TABLE_ONLY
      enumerator :: FLAG_NO_LITERAL
   end enum

   enum, bind(c)
      enumerator :: OS_UNKNOWN
      enumerator :: OS_WINDOWS
      enumerator :: OS_UNIX
   end enum

end module forgex_enums_m