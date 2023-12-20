module enums_m
   implicit none
   
     ! These enums will be rewritten in Fortran 2023's enumerator feature. 
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
      enumerator :: tk_end          ! 14
   end enum

   enum, bind(c)
      enumerator :: op_char = 0
      enumerator :: op_concat
      enumerator :: op_union
      enumerator :: op_closure
      enumerator :: op_empty
   end enum
end module enums_m