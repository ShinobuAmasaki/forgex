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
   
   use :: syntax_tree_m
   use :: automaton_m
   implicit none
   private

   public :: operator(.in.)
   public :: operator(.match.)

   interface operator(.in.)
      module procedure :: in__matching
   end interface

   interface operator(.match.)
      module procedure :: match__matching
   end interface 


   type(automaton_t) :: automaton
   type(automaton_t) :: cache
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

      if (is_there_caret_at_the_top(pattern)) then
         buff = pattern(2:len(pattern))
      else
         buff = pattern(1:len(pattern))
      end if

      if (is_there_dollar_at_the_end(pattern)) then
         buff = buff(1:len_trim(buff)-1)
      end if

      root => build_syntax_tree(tape, buff)
! call print_tree(root)
      call cache%init()
      call cache%build_nfa(root)
! call cache%print_nfa()
      call cache%convert_NFA_to_DFA()
! call cache%print_dfa()
      call cache%matching(str, from, to)

      call deallocate_tree(root)

      res = .true.
      if (from  > 0 .and. to > 0) then

         if (is_there_caret_at_the_top(pattern)) then
            
            ! 先頭にマッチした場合
            if (from == 1) then
               res = res .and. .true.
   
            ! 前の1文字が改行文字の場合
            else if (str(from-1:from-1) == char(10) .or. str(from-1:from-1) == char(12)) then
               res = res .and. .true.
            
            else
               res = .false.
            end if
         else
            res = res .and. .true.
         end if

         if (is_there_dollar_at_the_end(pattern)) then
            ! 末尾が最後にマッチした場合
            if (to == len(str)) then
               res = res .and. .true.

            ! 末尾の次の文字が改行文字の場合
            else if (str(to+1:to+1) == char(10) &          ! LF
                     .or. str(to+1:to+1) == char(13) &     ! CR
                     .or. str(to+1:to+1) == char(12)) then ! FF
               res = res .and. .true.
            
            else
               res = .false.
            end if
         else
            res = res .and. .true.
         end if 
 
      else
         res = .false.
      end if

      ! call cache%deallocate_automaton()

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
      call cache%init()
      call cache%build_nfa(root)
! call cache%print_nfa()
      call cache%convert_NFA_to_DFA()
! call cache%print_dfa()
      res = cache%matching_exactly(str)

! write(stderr, *) from, to

      call deallocate_tree(root)
      ! call cache%deallocate_automaton()

   end function match__matching


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