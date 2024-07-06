!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!
!!     forgex_m defines APIs of Forgex.
!!  
module forgex
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   
   use :: forgex_syntax_tree_m
   use :: forgex_automaton_m
   implicit none
   private

   public :: operator(.in.)
   public :: operator(.match.)

   public :: regex

   interface operator(.in.)
      module procedure :: in__matching
   end interface

   interface operator(.match.)
      module procedure :: match__matching
   end interface 

   interface regex
      module procedure :: regex__matching
   end interface

   type(automaton_t) :: cache
   character(:), allocatable :: pattern_cache

contains

   function in__matching (pattern, str) result(res)
      implicit none
      character(*), intent(in) :: pattern, str

      character(:), allocatable :: buff
      integer(int32) :: from, to
      logical :: res

      type(tree_t), pointer :: root
      type(tape_t) :: tape


      from = 0
      to = 0

      if (pattern /= pattern_cache) then

         ! initialize
         call cache%free()
         call cache%init()

         buff = pattern
         root => build_syntax_tree(tape, buff)

         ! call print_tree(root)
         
         call cache%build_nfa(root)

         ! call cache%print_nfa()
         
         call cache%convert_NFA_to_DFA()

         ! call cache%print_dfa()
         pattern_cache = pattern
         call deallocate_tree()

      end if
      
      call cache%matching(char(10)//str//char(10), from, to)


      if (is_there_caret_at_the_top(pattern)) then
         from = from
      else 
         from = from - 1
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         to = to - 2
      else
         to = to - 1
      end if

      ! res = .true.
      if (from  > 0 .and. to > 0) then
         res = .true.

      else
         res = .false.
      end if

   end function in__matching
   

   function match__matching(pattern, str) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32) :: from, to
      character(:), allocatable :: buff
      logical :: res

      type(tree_t), pointer :: root
      type(tape_t) :: tape

      from = 0
      to = 0

      if (pattern /= pattern_cache) then

         if (is_there_caret_at_the_top(pattern)) then
            buff = pattern(2:len(pattern))
         else
            buff = pattern(1:len(pattern))
         end if

         if (is_there_dollar_at_the_end(pattern)) then
            buff = buff(1:len_trim(pattern)-1)
         end if

         root => build_syntax_tree(tape, buff)  

         ! call print_tree(root)

         call cache%free()
         call cache%init()
         call cache%build_nfa(root)

         ! call cache%print_nfa()

         call cache%convert_NFA_to_DFA()

         ! call cache%print_dfa()
         pattern_cache = pattern
         call deallocate_tree()
         
      end if
      
      res = cache%matching_exactly(str)

      ! write(stderr, *) from, to
      
   end function match__matching


   function regex__matching (pattern, str, length, from, to) result(res)
      implicit none
      character(*), intent(in) :: pattern, str
      integer(int32), intent(inout), optional :: length
      integer(int32), intent(inout), optional :: from, to
      character(:), allocatable :: res

      character(:), allocatable :: buff
      integer(int32) :: from_l, to_l

      type(tree_t), pointer :: root
      type(tape_t) :: tape
   
      from_l = 0
      to_l = 0

      if (pattern /= pattern_cache) then
         buff = pattern
         root => build_syntax_tree(tape, buff)


         ! call print_tree(root)
         call cache%free() 
         call cache%init()

         call cache%build_nfa(root)

         ! call cache%print_nfa()
         
         call cache%convert_NFA_to_DFA()

         ! call cache%print_dfa()
         pattern_cache = pattern
         call deallocate_tree()

      end if 

      call cache%matching(char(10)//str//char(10), from_l, to_l)

      if (is_there_caret_at_the_top(pattern)) then
         from_l = from_l
      else 
         from_l = from_l - 1
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         to_l = to_l - 2
      else
         to_l = to_l - 1
      end if

      if (from_l > 0 .and. to_l > 0) then
         res = str(from_l:to_l)
         if (present(length)) length = to_l - from_l + 1
         if (present(from)) from = from_l
         if (present(to)) to = to_l
      else
         res = ''
         if (present(length)) length = 0
         if (present(from)) from = 0
         if (present(to)) to = 0
      end if

   end function regex__matching

!---------------------------------------------------------------------!

   function is_there_caret_at_the_top(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff
      logical :: res

      buff = adjustl(pattern)

      res = buff(1:1) == '^'

   end function is_there_caret_at_the_top


   function is_there_dollar_at_the_end(pattern) result(res)
      implicit none
      character(*), intent(in) :: pattern
      character(:), allocatable :: buff

      logical :: res

      buff = trim(pattern)

      res = buff(len_trim(buff):len_trim(buff)) == '$'

   end function is_there_dollar_at_the_end

   

end module forgex