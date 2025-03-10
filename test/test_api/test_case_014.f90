program test_case_014
   use :: iso_fortran_env
   use :: forgex_utf8_m
   use :: forgex_test_m
   implicit none
   logical :: res

   ! \x escape sequences in character classes
   res = .true.
   print *, '=== TEST CASE 13 BEGIN ==='

   call runner_match('[\x{3040}]',char_utf8(12352), .true., res)   ! before Hiragana
   call runner_match('[\x{3041}]','ぁ', .true., res) ! Hiragana minimum
   call runner_match('[\x{3042}]','あ', .true., res) 
   call runner_match('[\x{309E}]','ゞ', .true., res) 
   call runner_match('[\x{309F}]','ゟ', .true., res) ! HIRAGANA DIGRAPH YORI
   call runner_match('[\x{30A0}]','゠', .true., res) ! KATAKANA-HIRAGANA DOUBLE HYPHEN
   call runner_match('[\x{30A1}]','ァ', .true., res)
   call runner_match('[\x{30FF}]','ヿ', .true., res) ! Katakana maximum, KATAKANA DIGRAPH KOTO
   call runner_match('[\x{3100}]','㄀', .true., res)

   call runner_match('[\x{4DFF}]',char_utf8(19967), .true., res)
   call runner_match('[\x{4E00}]','一', .true., res) ! Kanji minimum
   call runner_match('[\x{4E01}]','丁', .true., res)
   call runner_match('[\x{9FBE}]','龾', .true., res)
   call runner_match('[\x{9FBF}]','龿', .true., res) ! Kanji maximum
   call runner_match('[\x{A000}]','ꀀ', .true., res)

   call runner_match('[\x{036F}]','ͯ', .true., res) ! COMBINING LATIN SMALL LETTER X
   call runner_match('[\x{0370}]','Ͱ', .true., res) ! Greek minimum
   call runner_match('[\x{0371}]','ͱ', .true., res)
   call runner_match('[\x{03FE}]','Ͼ', .true., res)
   call runner_match('[\x{03FF}]','Ͽ', .true., res) ! Greek maximu
   call runner_match('[\x{0400}]','Ѐ', .true., res) ! Cyrillic minimum
   call runner_match('[\x{0401}]','Ё', .true., res) 
   call runner_match('[\x{04FE}]','Ӿ', .true., res) 
   call runner_match('[\x{04FF}]','ӿ', .true., res) ! Cyrillic maximum
   call runner_match('[\x{0500}]','Ԁ', .true., res) ! キリル文字範囲外（直後）

   call runner_match('[\x{1F300}]','🌀', .true., res) !  Cyclone
   call runner_match('[\x{1F301}]','🌁', .true., res) !
   call runner_match('[\x{1F5FE}]','🗾', .true., res) !
   call runner_match('[\x{1F5FF}]','🗿', .true., res) !  Moai
   call runner_match('[\x{1F600}]','😀', .true., res) ! Grinning Face
   call runner_match('[\x{1F601}]','😁', .true., res) !
   call runner_match('[\x{1F64E}]','🙎', .true., res) !
   call runner_match('[\x{1F64F}]','🙏', .true., res) ! Person with Folded Hands
   call runner_match('[\x{1F650}]','🙐', .true., res) !
   call runner_match('[\x{1F6FF}]','🛿', .true., res) !
   call runner_match('[\x{1F700}]','🜀', .true., res) ! Alchemy Symbol for Quintessence
   call runner_match('[\x{1F701}]','🜁', .true., res) !
   call runner_match('[\x{1F77E}]','🝾', .true., res) !
   call runner_match('[\x{1F77F}]','🝿', .true., res) !  Alchemy Symbol for Mercury Sublimate

   call runner_match('[\x{1F780}]','�', .false., res) !  undefined
   
   call runner_match("[\x{3042}-\x{309f}]", "う", .true., res)
   call runner_match("[\x{3042}-\x{309f}]+", "あいうえお", .true., res)
   call runner_match("[\x{3042}-\x{309f}]+", "アイウエオ", .false., res)

   call runner_match("[\x{3042}-\x{309f}]{5}", "あいうえお", .true., res)
   call runner_match("[\x{3042}-\x{309f}]{1,5}", "かきくけこ", .true., res)

   if (res) then
      write(error_unit, *) '=== TEST CASE 13 END ==='
      stop
   else
      error stop
   end if
         
end program test_case_014
