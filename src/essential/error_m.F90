! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2025
!     A regular expression engine for Fortran.
!     forgex_error_m module is a part of Forgex.
!
module forgex_error_m
   implicit none
   
   enum, bind(c)
      enumerator :: SYNTAX_VALID
      enumerator :: SYNTAX_ERR
      enumerator :: SYNTAX_ERR_PARENTHESIS_MISSING
      enumerator :: SYNTAX_ERR_PARENTHESIS_UNEXPECTED
      enumerator :: SYNTAX_ERR_BRACKET_MISSING
      enumerator :: SYNTAX_ERR_BRACKET_UNEXPECTED
      enumerator :: SYNTAX_ERR_CURLYBRACE_MISSING
      enumerator :: SYNTAX_ERR_CURLYBRACE_UNEXPECTED
      enumerator :: SYNTAX_ERR_INVALID_TIMES
      enumerator :: SYNTAX_ERR_ESCAPED_SYMBOL_MISSING
      enumerator :: SYNTAX_ERR_ESCAPED_SYMBOL_INVALID
      enumerator :: SYNTAX_ERR_EMPTY_CHARACTER_CLASS
      enumerator :: SYNTAX_ERR_RANGE_WITH_ESCAPE_SEQUENCES
      enumerator :: SYNTAX_ERR_MISPLACED_SUBTRACTION_OPERATOR
      enumerator :: SYNTAX_ERR_INVALID_CHARACTER_RANGE
      enumerator :: SYNTAX_ERR_CHAR_CLASS_SUBTRANCTION_NOT_IMPLEMENTED
      
      enumerator :: SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
      
      
      enumerator :: ALLOCATION_ERR
   end enum


   ! Error message lines.
   character(*), parameter :: err_is_nothing    = "Given pattern is valid."
   character(*), parameter :: err_generic                  = "ERROR: Pattern includes some syntax error."
   character(*), parameter :: err_r_parenthesis_missing    = "ERROR: Closing parenthesis is expected."
   character(*), parameter :: err_r_parenthesis_unexpected = "ERROR: Unexpected closing parenthesis error."
   character(*), parameter :: err_r_bracket_missing        = "ERROR: Closing square bracket is expected."
   character(*), parameter :: err_r_bracket_unexpected     = "ERROR: Unexpected closing square bracket error."
   character(*), parameter :: err_r_curlybrace_missing     = "ERROR: Closing right curlybrace is expected."
   character(*), parameter :: err_r_curlybrace_unexpected  = "ERROR: Unexpected closing right curlybrace error."
   character(*), parameter :: err_invalid_quantifier       = "ERROR: Given quantifier range is invalid."
   character(*), parameter :: err_escaped_symbol_missing   = "ERROR: Pattern cannot end with a trailing unescaped backslash."
   character(*), parameter :: err_escaped_symbol_invalid   = "ERROR: This token has no special meaning."
   character(*), parameter :: err_empty_character_class    = "ERROR: Given class has no character."
   character(*), parameter :: err_range_with_escape_sequences = "ERROR: Cannot create a range with shorthand escape sequence"
   character(*), parameter :: err_misplaced_subtraction_operator &
                                                  = "ERROR: Subtraction operator is misplaced in the given character class."
   character(*), parameter :: err_invalid_character_range  = "ERROR: Given character range is invalid."
   character(*), parameter :: err_character_class_subtraction = "ERROR: Character class subtraction hasn't implemented yet."

   character(*), parameter :: err_allocation               = "ERROR: Allocation is failed."
   
   character(*), parameter :: err_this_should_not_happen   = "ERROR: Fatal error is happened."
   


contains

   !> This function accepts integer error status code and then return error
   !> messagge corresponding to it.
   pure function get_error_message(code) result(msg)
      implicit none
      integer, intent(in)       :: code
      character(:), allocatable :: msg

      select case (code)
      case (SYNTAX_VALID)
         msg = err_is_nothing
   
      case (SYNTAX_ERR)
         msg = err_generic
      case (SYNTAX_ERR_PARENTHESIS_MISSING)
         msg = err_r_parenthesis_missing

      case (SYNTAX_ERR_PARENTHESIS_UNEXPECTED)
         msg = err_r_parenthesis_unexpected

      case (SYNTAX_ERR_BRACKET_MISSING)
         msg = err_r_bracket_missing

      case (SYNTAX_ERR_BRACKET_UNEXPECTED)
         msg = err_r_bracket_unexpected

      case (SYNTAX_ERR_CURLYBRACE_MISSING)
         msg = err_r_curlybrace_missing
   
      case (SYNTAX_ERR_CURLYBRACE_UNEXPECTED)
         msg = err_r_curlybrace_unexpected

      case (SYNTAX_ERR_INVALID_TIMES)
         msg = err_invalid_quantifier

      case (SYNTAX_ERR_ESCAPED_SYMBOL_MISSING)
         msg = err_escaped_symbol_missing

      case (SYNTAX_ERR_ESCAPED_SYMBOL_INVALID)
         msg = err_escaped_symbol_invalid
      
      case (SYNTAX_ERR_EMPTY_CHARACTER_CLASS)
         msg = err_empty_character_class
      
      case (SYNTAX_ERR_RANGE_WITH_ESCAPE_SEQUENCES)
         msg = err_range_with_escape_sequences

      case (SYNTAX_ERR_MISPLACED_SUBTRACTION_OPERATOR)
         msg = err_misplaced_subtraction_operator

      case (SYNTAX_ERR_INVALID_CHARACTER_RANGE)
         msg = err_invalid_character_range
      
      case (SYNTAX_ERR_CHAR_CLASS_SUBTRANCTION_NOT_IMPLEMENTED)
         msg = err_character_class_subtraction
      
      case (ALLOCATION_ERR)
         msg = err_allocation

      !!!
      case (SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN)
         msg = err_this_should_not_happen

      case default
         msg = err_this_should_not_happen

      end select

   end function get_error_message


end module forgex_error_m