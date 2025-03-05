program test_010
   use, intrinsic :: iso_fortran_env
   use :: forgex
   use :: forgex_test_m
   use :: forgex_utf8_m
   implicit none

   logical :: res = .true.

   character(:), allocatable :: text, bad, bad1, bad2, ans, pattern
   character(1) :: c1, c2, c3
   character(2) :: fmt
   integer :: i

   ! Test case #10.
   ! Tests for input with bad utf-8 string.

   print *, "=== TEST CASE 10 BEGIN ==="
!=====================================================================!

   !=== UTF-8 characte of the correct lenght.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_4_mask)//c1//c1//c1
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Invalid Leader bytes
   c1 = nchar(continuation_mask)
   c2 = nchar(lead_4_mask)
   c3 = nchar(lead_2_mask)
   bad = c2//c3//c1
   text = bad//'a'
   call runner_in("[a-z]", text, .true., res)
   call print_hex(text)

   !=== Contenuation bytes are shorter than indicated by the leader byte.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_4_mask)//c1//c1
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Contenuation bytes are shorter than indicated by the leader byte.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_3_mask)//c1
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)   

   !=== Contenuation bytes are shorter than indicated by the leader byte.
   c1 = nchar(continuation_mask)
   bad = nchar(lead_2_mask)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Continuation bytes without a leader byte.
   c1 = nchar(continuation_mask)
   bad = c1
   text = bad//bad//'a'//bad
   call runner_in("\w", text, .true., res)
   call print_hex(text)   

   !=== Continuation bytes without a leader byte.
   c1 = nchar(continuation_mask)
   bad = c1
   text = bad//'a'//bad
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Overlong encoding for ASCII character '/' (U+002F)
   ! Valid encoding: 0x2F, Overlong: 0xC0 0xAF
   bad = char(192) // char(175)  ! Overlong for '/'
   text = bad//'a'
   call runner_in("/", text, .false., res)
   call print_hex(text)

   !=== Invalid UTF-16 surrogate pair (U+D800, 0xED 0xA0 0x80)
   bad = char(237) // char(160) // char(128)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Invalid leading bytes (0xFE, 0xFF)
   bad = char(254) // char(255)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Invalid 5-byte UTF-8 sequence (0xF8 0x88 0x80 0x80 0x80)
   bad = char(248) // char(136) // char(128) // char(128) // char(128)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Missing continuation byte (lead byte with insufficient continuation)
   bad = nchar(lead_3_mask) // nchar(continuation_mask)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Excess continuation bytes (too many continuations after a leader byte)
   bad = nchar(lead_2_mask) // nchar(continuation_mask) // nchar(continuation_mask)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Non-shortest form encoding (U+0000 as 0xC0 0x80)
   bad = char(192) // char(128)
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)
   call print_hex(text)

   !=== Invalid sequence between ASCII and 2-byte character boundary
   ! ASCII "a" (0x61) followed by an invalid 2-byte leader (0xC2)
   bad = char(97) // nchar(lead_2_mask)  ! "a" + invalid leader
   text = bad//'b'
   call runner_in("[a-z]+", text, .true., res)  ! Expected: Match "ab"
   call print_hex(text)

   !=== 2-byte leader (0xC2) followed by an invalid ASCII character
   ! Instead of a valid continuation byte (0x80-0xBF), "A" (0x41) is used
   bad = char(194) // char(65)  ! Invalid: 0xC2 0x41
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)  ! Expected: Match "a"
   call print_hex(text)

   !=== ASCII character between 2-byte leader and valid continuation byte
   ! "a" (0x61) as a continuation byte is not valid
   bad = char(194) // char(97)  ! Invalid: 0xC2 0x61 ("a")
   text = bad//'b'
   call runner_in("[a-z]+", text, .true., res)  ! Expected: Match "b"
   call print_hex(text)

   !=== Boundary value 0x7F (max ASCII) + invalid 2-byte sequence
   bad = char(127) // char(194) // char(128)  ! 0x7F + 0xC2 0x80
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)  ! Expected: Match "a"
   call print_hex(text)

   !=== Boundary value 0x80 (min continuation byte) + invalid leader byte
   bad = char(128) // char(194)  ! Invalid: 0x80 + 0xC2
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)  ! Expected: Match "a"
   call print_hex(text)

   !=== Boundary value 0xC2 (2-byte leader) + invalid byte 0xFF
   bad = char(194) // char(255)  ! Invalid: 0xC2 + 0xFF
   text = bad//'a'
   call runner_in("[a-z]+", text, .true., res)  ! Expected: Match "a"
   call print_hex(text)

!+-------------------------------------------------------------------+!

   ! Simple match with valid input
   pattern = '[a-z]+'
   ans = 'hello'
   text = 'hello'
   call runner_regex(pattern, text, ans, res)
   call print_hex(text)

   ! Invalid leader byte with not enough continuation byte
   pattern = '[a-z]+'
   ans = 'a'
   bad = nchar(lead_3_mask) // nchar(continuation_mask)
   text = bad//ans//bad
   call runner_regex(pattern, text, ans, res)
   call print_hex(text)

   ! Invalid continuation byte without a leader byte
   pattern = '[a-z]+'
   ans = 'test'
   bad = nchar(continuation_mask)
   text = bad//ans//bad
   call runner_regex(pattern, text, ans, res)
   call print_hex(text)

   ! Multiple invalid sequences surrounding valid match
   pattern = '[a-z]+'
   ans = 'xyz'
   bad1 = nchar(lead_3_mask) // nchar(continuation_mask) // nchar(continuation_mask) // nchar(continuation_mask)
   bad2 = nchar(lead_2_mask) // nchar(continuation_mask) // nchar(continuation_mask)
   text = bad1//ans//bad2
   call runner_regex(pattern, text, ans, res)
   call print_hex(text)

!=====================================================================!
   if (res) then
      write(error_unit, *) "=== TEST CASE 10 END ==="
      stop
   else
      error stop
   end if

end program test_010