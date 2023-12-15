!! Fortran Regular Expression (Forgex)
!! 
!! MIT License
!!
!! (C) Amasaki Shinobu, 2023
!!     A regular expression engine for Fortran.
!!     dfa_m module is a part of Forgex.

module dfa_m
   use, intrinsic :: iso_fortran_env, stderr=>error_unit
   use :: syntax_tree_m, only: EMPTY
   use :: nfa_m, only: NFA_state_set_t, check_NFA_state, nlist_t, nfa_nstate, nfa, &
                       NFA_VECTOR_SIZE, nfa_entry, nfa_exit, build_nfa, print_nfa
   implicit none
   private

   public :: convert_NFA_to_DFA
   public :: print_dfa
   public :: matching

   public :: D_state_t
   
   integer(int32), parameter :: DFA_STATE_MAX = 1024


   type :: D_list_t
      character(3) :: c = EMPTY
      type(NFA_state_set_t) :: to
      type(D_list_t), pointer :: next => null()
   end type


   type :: D_slist_t
      character(3) :: c = EMPTY
      type(D_state_t), pointer :: to => null()
      type(D_slist_t), pointer :: next => null()
   end type


   type :: D_state_t
      integer(int32) :: index
      type(NFA_state_set_t), pointer :: state => null() 
      logical :: visited
      logical :: accepted
      type(D_slist_t), pointer :: next => null() 
   end type


   integer(int32) :: dfa_nstate = 0
   type(D_state_t), target :: dfa(DFA_STATE_MAX)
   type(D_state_t), pointer :: initial_dfa_state => null()


contains 

   subroutine dfa_initialize()
      implicit none
      integer :: i, siz

      siz = size(dfa, dim=1)

      do i = 1, siz
         dfa(i)%index = i
      end do

   end subroutine dfa_initialize


   subroutine print_NFA_state_set (p)
      implicit none
      type(NFA_state_set_t), intent(in) :: p

      integer(int32) :: i

      do i = 1, nfa_nstate
         if (check_NFA_state(p, i)) write(*, '(i0, a)', advance='no') i, ' '
      end do
   end subroutine print_NFA_state_set
   

   subroutine print_dfa()
      implicit none

      type(D_slist_t), pointer :: l
      integer(int32) :: i, j
      character(1) :: c_Accepted

      print *, "--- PRINT DFA---"

      do i = 1, dfa_nstate
         if (dfa(i)%accepted) then
            write(*, '(i2,a, a)', advance='no') i, 'A', ": "
         else
            write(*, '(i2,a, a)', advance='no') i, ' ', ": "
         end if

         l => dfa(i)%next
         do while (associated(l))
            write(*, '(a, a, i0, 1x)', advance='no') trim(l%c), '=>', l%to%index
            l => l%next
         end do
         write(*, *) ""
      end do 

      do i = 1, dfa_nstate
         if (dfa(i)%accepted) then
            write(*, '(a, i2, a)', advance='no') "state ", i, 'A = ( '
         else
            write(*, '(a, i2, a)', advance='no') "state ", i, '  = ( '
         end if
         call print_NFA_state_set(dfa(i)%state)
         write(*,'(a)') ")"
      end do

   end subroutine print_dfa


   subroutine add_NFA_state(state, s)
      implicit none
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in):: s

      state%vec(s) = .true.
   end subroutine add_NFA_state


   recursive subroutine mark_empty_transition(state, s)
      use, intrinsic :: iso_c_binding
      implicit none
      type(NFA_state_set_t), intent(inout) :: state
      integer(int32), intent(in) :: s 

      type(nlist_t), pointer :: p => null() 

      call add_NFA_state(state, s)
      
      p => nfa(s)
      do while (associated(p))

         if (p%c == EMPTY .and. .not. check_NFA_state(state, p%to) ) then
            if (p%to /= 0) call mark_empty_transition(state, p%to)
         end if 

         if (.not. associated(p)) exit
         p => p%next

      enddo 

   end subroutine mark_empty_transition


   subroutine collect_empty_transition (state)
      use, intrinsic :: iso_c_binding
      implicit none
      type(NFA_state_set_t), intent(inout), target :: state
      integer(int32) :: i

      do i = 1, nfa_nstate
         if (check_NFA_state(state, i)) call mark_empty_transition(state, i)
      end do
   end subroutine collect_empty_transition


   function equivalent_NFA_state_set(a, b) result(res)
      implicit none
      type(NFA_state_set_t), intent(in), pointer  :: a
      type(NFA_state_set_t), intent(in)  :: b
      integer(int32) :: i
      logical :: res


      do i = 1, NFA_VECTOR_SIZE
         if (a%vec(i) .neqv. b%vec(i)) then
            res = .false.
            return
         end if
      end do
      res = .true.

   end function equivalent_NFA_state_set


   function register_D_state(s) result(res)
      implicit none
      type(NFA_state_set_t), intent(in), target :: s
      integer(int32) :: i
      type(D_state_t), pointer :: res 

      res => null() 

      do i = 1, dfa_nstate

         if (equivalent_NFA_state_set(dfa(i)%state, s)) then
            res => dfa(i)
            return
         end if

      end do

      if (dfa_nstate >= DFA_STATE_MAX) then
         write(stderr, '(a)') "Number of DFA states too large.."
         error stop
      end if

      dfa_nstate = dfa_nstate + 1 

      dfa(dfa_nstate)%state => s
      dfa(dfa_nstate)%visited = .false.
      dfa(dfa_nstate)%accepted = check_NFA_state(s, nfa_exit)
      dfa(dfa_nstate)%next => null()

      res => dfa(dfa_nstate)

   end function register_D_state


   function fetch_unvisited_D_state() result(res)
      implicit none
      type(D_state_t), pointer :: res
      integer(int32) :: i

      res => null()

      do i = 1, dfa_nstate
         if (dfa(i)%visited .eqv. .false.) then
            res => dfa(i)
            return
         end if
      end do
   end function fetch_unvisited_D_state


   function compute_reachable_N_state(dstate) result(res)
      use, intrinsic :: iso_c_binding
      implicit none
      type(D_state_t), intent(in) :: dstate

      type(D_list_t), pointer :: res
      integer(int32):: i
      type(NFA_state_set_t), pointer :: state
      type(nlist_t), pointer :: p
      type(D_list_t), pointer :: b
      type(D_list_t), pointer :: a

      p => null()
      a => null()
      b => null()
      res => null() 
      state => dstate%state

      ! Iterate the all NFA states
      outer: do i = 1, nfa_nstate

         ! If NFA state <i> is included in DFA state <dstate>, perform the following processing.
         if (check_NFA_state(state, i)) then

            ! Examine all NFA states reachable from NFA state <i> and list them. 
            p => nfa(i)
            middle: do while (associated(p))

               ! Except for ε-transition.
               if (p%c /= EMPTY) then

                  a => res
                  inner: do while(associated(a))
                     
                     if (a%c == p%c) then
                        call add_NFA_state(a%to, p%to)
                        
                        ! Move to next NFA state
                        p => p%next
                        cycle middle

                     end if
                     a => a%next

                  end do inner

                  allocate(b)
                  b%c = p%c
                  call add_NFA_state(b%to, p%to)
                  b%next => res
                  res => b
                  
               end if

               p => p%next

            end do middle
         
         end if 
      end do outer

   end function compute_reachable_N_state


   subroutine convert_NFA_to_DFA()
      implicit none

      type(NFA_state_set_t), target :: initial_state
      type(D_state_t), pointer :: t
      type(D_list_t), pointer :: x
      type(D_slist_t), pointer :: p

      t => null()
      x => null()
      p => null()

      call dfa_initialize

      call add_NFA_state(initial_state, nfa_entry)
      call collect_empty_transition(initial_state)

      initial_dfa_state => register_D_state(initial_state)

      t => fetch_unvisited_D_state()
      do while (associated(t))


         t%visited = .true.

         x => compute_reachable_N_state(t)
         do while (associated(x))

            call collect_empty_transition(x%to)

            allocate(p)
            p%c = x%c

            p%to => register_D_state(x%to)
            p%next => t%next
            t%next => p

            x => x%next
         end do 

         t => fetch_unvisited_D_state()
      end do


   end subroutine convert_NFA_to_DFA


   function next_state_dfa(state, chara) result(res)
      use :: utf8_m
      implicit none
      type(D_state_t), intent(in) :: state
      character(*), intent(in) :: chara
      type(D_slist_t), pointer :: ptr
      type(D_state_t), pointer :: res

      ptr => state%next
      do while (associated(ptr))
         if (chara(1:idxutf8(chara, 1)) == ptr%c) then
            res => ptr%to
            return
         end if
         ptr => ptr%next
      end do

      res => null()
   end function next_state_dfa


   subroutine matching (str, from, to)
      use :: utf8_m
      implicit none
      character(*), intent(in) :: str
      integer(int32), intent(inout) :: from, to
      type(D_state_t), pointer :: state

      integer(int32) :: start, next
      integer(int32) :: max_match, i

      ! logical, allocatable :: is_first(:)
      ! call is_first_byte_of_character_array(str, is_first, len(str))

      ! Match the pattern by shifting one character from the begining of string str.
      ! This loop should be parallelized.
      start = 1
      do while (start < len(str))

         ! Initialize DFA
         max_match = 0
         i = start
         state => initial_dfa_state

         ! 
         do while( associated(state))

            if (state%accepted) then
               max_match = i
            end if

            if (i > len(str)) exit

            next = idxutf8(str, i) + 1

            state => next_state_dfa(state, str(i:next-1))

            i = next

         end do

         if (max_match /= 0) then
            from = start
            to = max_match -1
            return
         end if

         start = idxutf8(str, start) + 1
      end do

   end subroutine


end module dfa_m