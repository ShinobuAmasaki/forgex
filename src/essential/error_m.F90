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
      enumerator :: SYNTAX_ERR_STAR_INCOMPLETE
      enumerator :: SYNTAX_ERR_PLUS_INCOMPLETE
      enumerator :: SYNTAX_ERR_QUESTION_INCOMPLETE
      enumerator :: SYNTAX_ERR_INVALID_HEXADECIMAL
      enumerator :: SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH
      enumerator :: SYNTAX_ERR_UNICODE_EXCEED
      enumerator :: SYNTAX_ERR_UNICODE_PROPERTY_NOT_IMPLEMENTED
      enumerator :: SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
      enumerator :: ALLOCATION_ERR
   end enum


   ! Error message lines.
   ! SYNTAX_VALID
   character(*), parameter :: err_is_nothing    = "Given pattern is valid."
   
   ! SYNTAX_ERR
   character(*), parameter :: err_generic = "ERROR: Pattern includes some syntax error."
   
   ! SYNTAX_ERR_PARENTHESIS_MISSING
   character(*), parameter :: err_r_parenthesis_missing = "ERROR: Closing parenthesis is expected."
   
   ! SYNTAX_ERR_PARENTHESIS_UNEXPECTED
   character(*), parameter :: err_r_parenthesis_unexpected = "ERROR: Unexpected closing parenthesis error."
   
   ! SYNTAX_ERR_BRACKET_MISSING
   character(*), parameter :: err_r_bracket_missing = "ERROR: Closing square bracket is expected."
   
   ! SYNTAX_ERR_BRACKET_UNEXPECTED
   character(*), parameter :: err_r_bracket_unexpected = "ERROR: Unexpected closing square bracket error."
   
   ! SYNTAX_ERR_CURLYBRACE_MISSING
   character(*), parameter :: err_r_curlybrace_missing = "ERROR: Closing right curlybrace is expected."
   
   ! SYNTAX_ERR_CURLYBRACE_UNEXPECTED
   character(*), parameter :: err_r_curlybrace_unexpected = "ERROR: Unexpected closing right curlybrace error."
   
   ! SYNTAX_ERR_INVALID_TIMES
   character(*), parameter :: err_invalid_quantifier = "ERROR: Given quantifier range is invalid."
   
   ! SYNTAX_ERR_ESCAPED_SYMBOL_MISSING
   character(*), parameter :: err_escaped_symbol_missing = "ERROR: Pattern cannot end with a trailing unescaped backslash."
   
   ! SYNTAX_ERR_ESCAPED_SYMBOL_INVALID
   character(*), parameter :: err_escaped_symbol_invalid = "ERROR: This token has no special meaning."
   
   ! SYNTAX_ERR_EMPTY_CHARACTER_CLASS
   character(*), parameter :: err_empty_character_class = "ERROR: Given class has no character."
   
   ! SYNTAX_ERR_RANGE_WITH_ESCAPE_SEQUENCES
   character(*), parameter :: err_range_with_escape_sequences = "ERROR: Cannot create a range with shorthand escape sequence"
   
   ! SYNTAX_ERR_MISPLACED_SUBTRACTION_OPERATOR
   character(*), parameter :: err_misplaced_subtraction_operator &
                                                  = "ERROR: Subtraction operator is misplaced in the given character class."
   ! SYNTAX_ERR_INVALID_CHARACTER_RANGE
   character(*), parameter :: err_invalid_character_range = "ERROR: Given character range is invalid."
   
   ! SYNTAX_ERR_CHAR_CLASS_SUBTRANCTION_NOT_IMPLEMENTED
   character(*), parameter :: err_character_class_subtraction = "ERROR: Character class subtraction hasn't implemented yet."
   
   ! SYNTAX_ERR_STAR_INCOMPLETE
   character(*), parameter :: err_star_incomplete = "ERROR: Not quantifiable; star '*' operator is missing operand."
   
   ! SYNTAX_ERR_PLUS_INCOMPLETE
   character(*), parameter :: err_plus_incomplete = "ERROR: Not quantifiable; plus '+' operator is missing operand."
   
   ! SYNTAX_ERR_QUESTION_INCOMPLETE
   character(*), parameter :: err_question_incomplete = "ERROR: Not quantifiable; question '?' operator is missing operand."
   
   ! SYNTAX_ERR_TOKEN_INCOMPLETE
   character(*), parameter :: err_token_incomplete = "ERROR: The token is incomplete."

   ! SYNTAX_ERR_INVALID_HEXADECIMAL
   character(*), parameter :: err_invalid_hexadecimal_value = &
                "ERROR: Invalid characters detected. Ensure all characters are 0-9, A-F/a-f."

   ! SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH
   character(*), parameter :: err_hex_is_not_enought_digit = &
                "ERROR: At least 2 hexadecimal digits are required (e.g., '0A' instead of 'A')."

   ! SYNTAX_ERR_UNICODE_EXCEED
   character(*), parameter :: err_exceed_unicode_limit = "ERROR: Given hex number exceeds the range of unicode codepoint."

   ! SYNTAX_ERR_UNICODE_PROPERTY_NOT_IMPLEMENTED
   character(*), parameter :: err_unicode_property = "ERROR: Unicode property escape hasn't implemented yet."

   ! SYNTAX_ERR_THIS_SHOULD_NOT_HAPPEN
   character(*), parameter :: err_this_should_not_happen = "ERROR: Fatal error is happened."
   
   ! ALLOCATION_ERR
   character(*), parameter :: err_allocation = "ERROR: Allocation is failed."


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
      
      case (SYNTAX_ERR_STAR_INCOMPLETE)
         msg = err_star_incomplete
      
      case (SYNTAX_ERR_PLUS_INCOMPLETE)
         msg = err_plus_incomplete
      
      case (SYNTAX_ERR_QUESTION_INCOMPLETE)
         msg = err_question_incomplete

      case (SYNTAX_ERR_INVALID_HEXADECIMAL)
         msg = err_invalid_hexadecimal_value
      
      case (SYNTAX_ERR_HEX_DIGITS_NOT_ENOUGH)
         msg = err_hex_is_not_enought_digit

      case (SYNTAX_ERR_UNICODE_EXCEED)
         msg = err_exceed_unicode_limit
      
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