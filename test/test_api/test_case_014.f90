program test_case_014
   use :: iso_fortran_env
   use :: forgex_utf8_m
   use :: forgex_test_m
   implicit none
   logical :: res

   ! \x escape sequences in character classes
   res = .true.
   print *, '=== TEST CASE 14 BEGIN ==='

   call runner_match('[\x21]','!', .true., res)
   call runner_match('[\x22]','"', .true., res)
   call runner_match('[\x23]','#', .true., res)
   call runner_match('[\x24]','$', .true., res)
   call runner_match('[\x25]','%', .true., res)
   call runner_match('[\x26]','&', .true., res)
   call runner_match('[\x27]',"'", .true., res)
   call runner_match('[\x28]','(', .true., res)
   call runner_match('[\x29]',')', .true., res)
   call runner_match('[\x2A]','*', .true., res)
   call runner_match('[\x2B]','+', .true., res)
   call runner_match('[\x2C]',',', .true., res)
   call runner_match('[\x2D]','-', .true., res)
   call runner_match('[\x2E]','.', .true., res)
   call runner_match('[\x2F]','/', .true., res)
   call runner_match('[\x30]','0', .true., res)
   call runner_match('[\x31]','1', .true., res)
   call runner_match('[\x32]','2', .true., res)
   call runner_match('[\x33]','3', .true., res)
   call runner_match('[\x34]','4', .true., res)
   call runner_match('[\x35]','5', .true., res)
   call runner_match('[\x36]','6', .true., res)
   call runner_match('[\x37]','7', .true., res)
   call runner_match('[\x38]','8', .true., res)
   call runner_match('[\x39]','9', .true., res)
   call runner_match('[\x3A]',':', .true., res)
   call runner_match('[\x3B]',';', .true., res)
   call runner_match('[\x3C]','<', .true., res)
   call runner_match('[\x3D]','=', .true., res)
   call runner_match('[\x3E]','>', .true., res)
   call runner_match('[\x3F]','?', .true., res)
   call runner_match('[\x40]','@', .true., res)
   call runner_match('[\x41]','A', .true., res)
   call runner_match('[\x42]','B', .true., res)
   call runner_match('[\x43]','C', .true., res)
   call runner_match('[\x44]','D', .true., res)
   call runner_match('[\x45]','E', .true., res)
   call runner_match('[\x46]','F', .true., res)
   call runner_match('[\x47]','G', .true., res)
   call runner_match('[\x48]','H', .true., res)
   call runner_match('[\x49]','I', .true., res)
   call runner_match('[\x4A]','J', .true., res)
   call runner_match('[\x4B]','K', .true., res)
   call runner_match('[\x4C]','L', .true., res)
   call runner_match('[\x4D]','M', .true., res)
   call runner_match('[\x4E]','N', .true., res)
   call runner_match('[\x4F]','O', .true., res)
   call runner_match('[\x50]','P', .true., res)
   call runner_match('[\x51]','Q', .true., res)
   call runner_match('[\x52]','R', .true., res)
   call runner_match('[\x53]','S', .true., res)
   call runner_match('[\x54]','T', .true., res)
   call runner_match('[\x55]','U', .true., res)
   call runner_match('[\x56]','V', .true., res)
   call runner_match('[\x57]','W', .true., res)
   call runner_match('[\x58]','X', .true., res)
   call runner_match('[\x59]','Y', .true., res)
   call runner_match('[\x5A]','Z', .true., res)
   call runner_match('[\x61]','a', .true., res)
   call runner_match('[\x62]','b', .true., res)
   call runner_match('[\x63]','c', .true., res)
   call runner_match('[\x64]','d', .true., res)
   call runner_match('[\x65]','e', .true., res)
   call runner_match('[\x66]','f', .true., res)
   call runner_match('[\x67]','g', .true., res)
   call runner_match('[\x68]','h', .true., res)
   call runner_match('[\x69]','i', .true., res)
   call runner_match('[\x6A]','j', .true., res)
   call runner_match('[\x6B]','k', .true., res)
   call runner_match('[\x6C]','l', .true., res)
   call runner_match('[\x6D]','m', .true., res)
   call runner_match('[\x6E]','n', .true., res)
   call runner_match('[\x6F]','o', .true., res)
   call runner_match('[\x70]','p', .true., res)
   call runner_match('[\x71]','q', .true., res)
   call runner_match('[\x72]','r', .true., res)
   call runner_match('[\x73]','s', .true., res)
   call runner_match('[\x74]','t', .true., res)
   call runner_match('[\x75]','u', .true., res)
   call runner_match('[\x76]','v', .true., res)
   call runner_match('[\x77]','w', .true., res)
   call runner_match('[\x78]','x', .true., res)
   call runner_match('[\x79]','y', .true., res)
   call runner_match('[\x7A]','z', .true., res)

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
   call runner_match('[\x21-\x7a]+','!abc', .true., res)


   if (res) then
      write(error_unit, *) '=== TEST CASE 14 END ==='
      stop
   else
      error stop
   end if
         
end program test_case_014
