program main
   use :: forgex
   implicit none
   
   block
      character(:), allocatable :: pattern, str
      integer :: length
      
      pattern = "夢.{1,7}胡蝶"
      str = "昔者莊周夢爲胡蝶　栩栩然胡蝶也"

      
      print *, pattern .in. str      ! T
      print *, regex(pattern, str, length)   ! 胡蝶　栩栩然胡蝶
      print *, length
      
      pattern = "a.{1,5}g"
      str = "abcdefg"
      print *, pattern .in. str      ! T
      print *, regex(pattern, str, length)   ! 胡蝶　栩栩然胡蝶
      print *, length

   end block
end program main  