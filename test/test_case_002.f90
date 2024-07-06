program test_002
   use, intrinsic :: iso_fortran_env
   use :: forgex_test_m
   implicit none

   logical :: res = .true.
   
   ! Test case #2. 
   ! Tests for UTF-8 Character set.

   call runner_regex('[ぁ-ん]{,7}', 'あいうえおかきくけこ', 'あいうえおかき', res )
   call runner_regex('[ぁ-ん]{,6}', 'あいうえおかきくけこ', 'あいうえおか', res )
   call runner_regex('[ぁ-ん]{,5}', 'あいうえおかきくけこ', 'あいうえお', res )
   call runner_regex('[ぁ-ん]{,4}', 'あいうえおかきくけこ', 'あいうえ', res )
   call runner_regex('[ぁ-ん]{,3}', 'あいうえおかきくけこ', 'あいう', res )
   call runner_regex('[ぁ-ん]{,2}', 'あいうえおかきくけこ', 'あい', res )
   call runner_regex('[ぁ-ん]{,1}', 'あいうえおかきくけこ', 'あ', res )

   call runner_regex('[ぁ-んァ-ン]*', 'あイうエお', 'あイうエお', res)
   call runner_regex('[ぁ-んァ-ン]{,5}', 'あイうエお', 'あイうエお', res)
   call runner_regex('[ぁ-んァ-ン]{,4}', 'あイうエお', 'あイうエ', res)
   call runner_regex('[ぁ-んァ-ン]{,3}', 'あイうエお', 'あイう', res)
   call runner_regex('[ぁ-んァ-ン]{,2}', 'あイうエお', 'あイ', res)
   call runner_regex('[ぁ-んァ-ン]{,1}', 'あイうエお', 'あ', res)

   ! 一 U+4e00
   ! 鿿 U+9fff

   call runner_regex('[ぁ-んァ-ン一-鿿]*', 'あいうえおアイウエオ安伊宇衣於', &
    'あいうえおアイウエオ安伊宇衣於', res) 
   call runner_regex('[一-鿿]*', 'あいうえおアイウエオ安伊宇衣於', &
    '安伊宇衣於', res) 
    call runner_regex('[一-鿿]{1,10}', 'あいうえおアイウエオ安伊宇衣於', &
    '安伊宇衣於', res) 
    call runner_regex('[一-鿿]{1,7}', 'あいうえおアイウエオ安伊宇衣於', &
    '安伊宇衣於', res) 
    call runner_regex('[一-鿿]{1,5}', 'あいうえおアイウエオ安伊宇衣於', &
    '安伊宇衣於', res) 
    call runner_regex('[一-鿿]{1,3}', 'あいうえおアイウエオ安伊宇衣於', &
    '安伊宇', res) 
    call runner_regex('[一-鿿]{,2}', 'あいうえおアイウエオ安伊宇衣於', &
    '安伊', res) 
    call runner_regex('[一-鿿]{,1}', 'あいうえおアイウエオ安伊宇衣於', &
    '安', res) 

   


   call runner_in('[い]{7,7}', 'いろはにほへとち', .false., res)

   call runner_match("[い]{6}", 'いいいいいい ', .false., res)
   call runner_match("[^さ-ん]{6}",  'あいうえおか', .true., res)

   call runner_regex('[ぁ-ん]{1,7}', 'いろはにほへとちりぬるを', 'いろはにほへと', res)

   call runner_regex('[ぁ-ん]{,7}', 'いろはにほへとちりぬるを', 'いろはにほへと', res)


   call runner_in("夢.{1,7}胡蝶", "昔者莊周夢爲胡蝶　栩栩然胡蝶也", .true., res)
   call runner_regex('夢.{1,7}胡蝶', "昔者莊周夢爲胡蝶　栩栩然胡蝶也", "夢爲胡蝶　栩栩然胡蝶", res )

   ! Zenkaku space
   call runner_match("\s", '　', .true., res)
   call runner_match('\s*', '　　', .true., res)

   if (res) then
      print *, "=== TEST CASE 2 END ===          "
      stop
   else
      error stop
   end if 


end program test_002