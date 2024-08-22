! Fortran Regular Expression (Forgex)
!
! MIT License
!
! (C) Amasaki Shinobu, 2023-2024
!     A regular expression engine for Fortran.
!     forgex_cli_find_m module is a part of Forgex.
!
module forgex_cli_find_m
   use, intrinsic :: iso_fortran_env, stdout => output_unit
   use :: forgex_cli_parameters_m
   use :: forgex_enums_m
   use :: forgex_cli_time_measurement_m
   use :: forgex_cli_help_messages_m
   use :: forgex_cli_utils_m, only: right_justify
   implicit none
   private

   public :: do_find_match_forgex
   public :: do_find_match_lazy_dfa
   public :: do_find_match_dense_dfa

contains

   subroutine do_find_match_forgex(flags, pattern, text, is_exactly)
      use :: forgex, only: regex, operator(.in.), operator(.match.)
      use :: forgex_parameters_m, only: INVALID_CHAR_INDEX
      use :: forgex_cli_time_measurement_m
      use :: forgex_cli_utils_m, only: text_highlight_green
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern, text
      logical, intent(in) :: is_exactly

      real(real64) :: lap
      logical :: res
      character(:), allocatable :: res_string
      integer :: from, to, unused

      res_string = ''
      from = INVALID_CHAR_INDEX
      to = INVALID_CHAR_INDEX

      call time_begin()
      if (is_exactly) then
         res = pattern .match. text
      else
         res = pattern .in. text
      end if
      lap = time_lap()

      ! Invoke regex subroutine to highlight matched substring.
      call regex(pattern, text, res_string, unused, from, to)

      output: block
         character(NUM_DIGIT_KEY) :: pattern_key, text_key
         character(NUM_DIGIT_KEY) :: total_time, matching_result
         character(NUM_DIGIT_KEY) :: buf(4)

         pattern_key = "pattern:"
         text_key = "text:"
         total_time = "time:"
         matching_result = "result:"
         if (flags(FLAG_NO_TABLE)) then
            write(stdout, *) res
         else
            buf = [pattern_key, text_key, total_time, matching_result]
            call right_justify(buf)
            write(stdout, '(a, 1x, a)') trim(buf(1)), trim(adjustl(pattern))
            write(stdout, '(a, 1x, a)') trim(buf(2)), '"'//text_highlight_green(text, from, to)//'"'
            write(stdout, fmt_out_time) trim(buf(3)), get_lap_time_in_appropriate_unit(lap)
            write(stdout, fmt_out_logi) trim(buf(4)), res
         end if
      end block output

   end subroutine do_find_match_forgex


   subroutine do_find_match_lazy_dfa(flags, pattern, text, is_exactly)
      use :: forgex_automaton_m
      use :: forgex_syntax_tree_graph_m
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_cli_memory_calculation_m
      use :: forgex_api_internal_m
      use :: forgex_nfa_state_set_m
      use :: forgex_cli_utils_m
      use :: forgex_utility_m, only: is_there_caret_at_the_top, is_there_dollar_at_the_end
      use :: forgex_parameters_m, only: ACCEPTED_EMPTY
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern
      character(*), intent(in) :: text
      logical, intent(in) :: is_exactly

      type(tree_t) :: tree
      type(automaton_t) :: automaton

      integer :: uni, ierr, i
      character(:), allocatable :: dfa_for_print, prefix, suffix, entire
      character(256) :: line
      real(real64) :: lap1, lap2, lap3, lap4, lap5
      logical :: res, flag_runs_engine, flag_fixed_string
      integer :: from, to

      dfa_for_print = ''
      lap1 = 0d0
      lap2 = 0d0
      lap3 = 0d0
      lap4 = 0d0
      lap5 = 0d0
      from = 0
      to = 0
      prefix = ''
      suffix = ''
      entire = ''
      flag_fixed_string = .false.
      flag_runs_engine = .false.

      if (flags(FLAG_HELP) .or. pattern == '') call print_help_find_match_lazy_dfa
      

      call time_begin()
      call tree%build(trim(pattern))
      lap1 = time_lap()

      ! 
      call time_begin()
      if (.not. flags(FLAG_NO_LITERAL)) then
         entire = get_entire_literal(tree)
         if (entire /= '') flag_fixed_string = .true.

         if (.not. flag_fixed_string) then
            prefix = get_prefix_literal(tree)
            suffix = get_suffix_literal(tree)
         end if
      end if
      lap5 = time_lap()

      if (.not. flag_fixed_string) then
         call automaton%preprocess(tree)
         lap2 = time_lap()

         call automaton%init()
         lap3 = time_lap()
      end if

      if (is_exactly) then

         if (flag_fixed_string) then
            if (len(text) == len(entire)) then
               res = text == entire
            end if
         else
            call runner_do_matching_exactly(automaton, text, res, prefix, suffix, flags(FLAG_NO_LITERAL), flag_runs_engine)
         end if

         lap4 = time_lap()
         if (res) then
            from = 1
            to = len(text)
         end if
      else
         block
            if (flag_fixed_string) then
               from = index(text, entire)
               if (from > 0 ) to = from + len(entire) -1
            else
               call runner_do_matching_including(automaton, char(0)//text//char(0), from, to, &
                     prefix, suffix, flags(FLAG_NO_LITERAL), flag_runs_engine)
            
               if (from == ACCEPTED_EMPTY .and. to == ACCEPTED_EMPTY) then
                  from = 0
                  to = 0
                  res = .true.
               end if


               if (is_there_caret_at_the_top(pattern)) then
                  from = from
               else
                  from = from -1
               end if

               if (is_there_dollar_at_the_end(pattern)) then
                  to = to - 2
               else
                  to = to - 1
               end if
            end if

            if (from > 0 .and. to > 0) then
               res = .true.
            else
               res = .false.
            end if
            lap4 = time_lap()
         end block
      end if

      open(newunit=uni, status='scratch')
      write(uni, fmta) HEADER_NFA
      call automaton%nfa%print(uni, automaton%nfa_exit)
      write(uni, fmta) HEADER_DFA
      call automaton%print_dfa(uni)

      rewind(uni)
      ierr = 0
      do while (ierr == 0)
         read(uni, fmta, iostat=ierr) line
         if (ierr/=0) exit
         if (get_os_type() == OS_WINDOWS) then
            dfa_for_print = dfa_for_print//trim(line)//CRLF
         else
            dfa_for_print = dfa_for_print//trim(line)//LF
         end if
      end do
      close(uni)

      output: block
         character(NUM_DIGIT_KEY) :: pattern_key, text_key
         character(NUM_DIGIT_KEY) :: parse_time, extract_time
         character(NUM_DIGIT_KEY) :: nfa_time, dfa_init_time, matching_time, memory
         character(NUM_DIGIT_KEY) :: runs_engine_key
         character(NUM_DIGIT_KEY) :: tree_count
         character(NUM_DIGIT_KEY) :: nfa_count
         character(NUM_DIGIT_KEY) :: dfa_count, matching_result
         character(NUM_DIGIT_KEY) :: cbuff(13) = ''
         integer :: memsiz

         pattern_key    = "pattern:"
         text_key       = "text:"
         parse_time     = "parse time:"
         extract_time   = "extract literal time:"
         runs_engine_key= "runs engine:"

         nfa_time       = "compile nfa time:"
         dfa_init_time  = "dfa initialize time:"
         matching_time  = "search time:"
         memory         = "memory (estimated):"
         matching_result= "matching result:"

         tree_count     = "tree node count:"
         nfa_count      = "nfa states:"
         dfa_count      = "dfa states:"

         if (flag_fixed_string) then
            memsiz = mem_tape(tree%tape) + mem_tree(tree%nodes)
         else
            memsiz = mem_tape(tree%tape) + mem_tree(tree%nodes) + mem_nfa_graph(automaton%nfa) &
                      + mem_dfa_graph(automaton%dfa) + 4*3
         end if

         if (allocated(automaton%entry_set%vec)) then
            memsiz = memsiz + size(automaton%entry_set%vec, dim=1)
         end if
         if (allocated(automaton%all_segments)) then
            memsiz = memsiz + size(automaton%all_segments, dim=1)*8
         end if

         if (flags(FLAG_VERBOSE)) then
            cbuff = [pattern_key, text_key, parse_time, extract_time, runs_engine_key, &
                     nfa_time, dfa_init_time, matching_time, matching_result, memory, tree_count, &
                     nfa_count, dfa_count]
            call right_justify(cbuff)

            write(stdout, '(a, 1x, a)') trim(cbuff(1)), trim(adjustl(pattern))
            ! write(stdout, '(a, 1x, a)') trim(cbuff(2)), '"'//text//'"'
            write(stdout, '(a, 1x, a)') trim(cbuff(2)), '"'//text_highlight_green(text, from, to)//'"'
            write(stdout, fmt_out_time) trim(cbuff(3)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(4)), get_lap_time_in_appropriate_unit(lap5)
            write(stdout, fmt_out_logi) trim(cbuff(5)), flag_runs_engine
            
            if (flag_runs_engine .or. .not. flag_fixed_string) then
               write(stdout, fmt_out_time) trim(cbuff(6)), get_lap_time_in_appropriate_unit(lap2)
               write(stdout, fmt_out_time) trim(cbuff(7)), get_lap_time_in_appropriate_unit(lap3)
            else
               write(stdout, fmt_out_char) trim(cbuff(6)), not_running
               write(stdout, fmt_out_char) trim(cbuff(7)), not_running
            end if

            write(stdout, fmt_out_time) trim(cbuff(8)), get_lap_time_in_appropriate_unit(lap4)            
            write(stdout, fmt_out_logi) trim(cbuff(9)), res
            write(stdout, fmt_out_int)  trim(cbuff(10)), memsiz

            write(stdout, fmt_out_ratio) trim(cbuff(11)), tree%top, size(tree%nodes, dim=1)
            write(stdout, fmt_out_ratio) trim(cbuff(12)), automaton%nfa%nfa_top, automaton%nfa%nfa_limit
            write(stdout, fmt_out_ratio) trim(cbuff(13)), automaton%dfa%dfa_top, automaton%dfa%dfa_limit
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff(:) = [pattern_key, text_key, parse_time, extract_time, runs_engine_key, nfa_time, dfa_init_time, &
                        matching_time, matching_result, memory, (repeat(" ", NUM_DIGIT_KEY), i = 1, 3)]
            call right_justify(cbuff)
            write(stdout, '(a,1x,a)') trim(cbuff(1)), pattern
            ! write(stdout, '(a,1x,a)') trim(cbuff(2)), "'"//text//"'"
            write(stdout, '(a,1x,a)') trim(cbuff(2)), "'"//text_highlight_green(text, from, to)//"'"
            write(stdout, fmt_out_time) trim(cbuff(3)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(4)), get_lap_time_in_appropriate_unit(lap5)
            write(stdout, fmt_out_logi) trim(cbuff(5)), flag_runs_engine

            if (flag_runs_engine .or. .not. flag_fixed_string) then
               write(stdout, fmt_out_time) trim(cbuff(6)), get_lap_time_in_appropriate_unit(lap2)
               write(stdout, fmt_out_time) trim(cbuff(7)), get_lap_time_in_appropriate_unit(lap3)
            else
               write(stdout, fmt_out_char) trim(cbuff(6)), not_running
               write(stdout, fmt_out_char) trim(cbuff(7)), not_running
            end if

            write(stdout, fmt_out_time) trim(cbuff(8)), get_lap_time_in_appropriate_unit(lap4)
            write(stdout, fmt_out_logi) trim(cbuff(9)), res
            write(stdout, fmt_out_int)  trim(cbuff(10)), memsiz
         end if

         if (flags(FLAG_TABLE_ONLY) .or. .not. flag_runs_engine .or. flag_fixed_string) then
            call automaton%free
            return
         end if

         write(stdout, *) ""

         write(stdout, fmta, advance='no') trim(dfa_for_print)
         write(stdout, fmta) FOOTER

      end block output
      call automaton%free
   end subroutine do_find_match_lazy_dfa


   subroutine do_find_match_dense_dfa(flags, pattern, text, is_exactly)
      use :: forgex_automaton_m
      use :: forgex_syntax_tree_graph_m
      use :: forgex_cli_memory_calculation_m
      use :: forgex_cli_time_measurement_m
      use :: forgex_dense_dfa_m
      use :: forgex_nfa_state_set_m
      use :: forgex_cli_utils_m
      use :: forgex_utility_m
      implicit none
      logical, intent(in) :: flags(:)
      character(*), intent(in) :: pattern
      character(*), intent(in) :: text
      logical, intent(in) :: is_exactly

      type(tree_t) :: tree
      type(automaton_t) :: automaton

      integer :: uni, ierr, i
      character(:), allocatable :: dfa_for_print
      character(256) :: line
      real(real64) :: lap1, lap2, lap3, lap4, lap5
      logical :: res
      integer :: from, to
      from = 0
      to = 0

      if (flags(FLAG_HELP) .or. pattern == '') call print_help_find_match_dense_dfa
      if (flags(FLAG_NO_LITERAL)) call info("No literal search optimization is implemented in dense DFA.")
      call time_begin()
      ! call build_syntax_tree(trim(pattern), tape, tree, root)
      call tree%build(trim(pattern))
      lap1 = time_lap()

      call automaton%preprocess(tree)
      lap2 = time_lap() ! build nfa

      call automaton%init()
      lap3 = time_lap() ! automaton initialize

      call construct_dense_dfa(automaton, automaton%initial_index)
      lap4 = time_lap() ! compile nfa to dfa

      if (is_exactly) then
         res = match_dense_dfa_exactly(automaton, text)
         if (res) then
            from = 1
            to = len(text)
         end if
      else
         block
            call match_dense_dfa_including(automaton, char(10)//text//char(10), from, to)
            if (is_there_caret_at_the_top(pattern)) then
               from = from
            else
               from = from -1
            end if

            if (is_there_dollar_at_the_end(pattern)) then
               to = to -2
            else
               to = to -1
            end if

            if (from>0 .and. to>0) then
               res = .true.
            else
               res = .false.
            end if
         end block
      end if
      lap5 = time_lap() ! search time

      open(newunit=uni, status='scratch')
      write(uni, fmta) HEADER_NFA
      call automaton%nfa%print(uni, automaton%nfa_exit)
      write(uni, fmta) HEADER_DFA
      call automaton%print_dfa(uni)

      rewind(uni)
      ierr = 0
      dfa_for_print = ''
      do while (ierr == 0)
         read(uni, fmta, iostat=ierr) line
         if (ierr/=0) exit
         if (get_os_type() == OS_WINDOWS) then
            dfa_for_print = dfa_for_print//trim(line)//CRLF
         else
            dfa_for_print = dfa_for_print//trim(line)//LF
         end if
      end do
      close(uni)

      output: block
         character(NUM_DIGIT_KEY) :: pattern_key, text_key
         character(NUM_DIGIT_KEY) :: parse_time, nfa_time, dfa_init_time, dfa_compile_time, matching_time
         character(NUM_DIGIT_KEY) :: memory
         character(NUM_DIGIT_KEY) :: tree_count, nfa_count, dfa_count
         character(NUM_DIGIT_KEY) :: matching_result
         character(NUM_DIGIT_KEY) :: cbuff(12) = ''
         integer :: memsiz

         pattern_key    = "pattern:"
         text_key       = "text:"
         parse_time     = "parse time:"
         nfa_time       = "compile nfa time:"
         dfa_init_time  = "dfa initialize time:"
         dfa_compile_time = "compile dfa time:" 
         matching_time  = "search time:"
         memory         = "memory (estimated):"
         matching_result= "matching result:"

         tree_count     = "tree node count:"
         nfa_count      = "nfa states:"
         dfa_count      = "dfa states:"
         
         memsiz = mem_tape(tree%tape) + mem_tree(tree%nodes) + mem_nfa_graph(automaton%nfa) &
            + mem_dfa_graph(automaton%dfa) + 4*3
         if (allocated(automaton%entry_set%vec)) then
            memsiz = memsiz + size(automaton%entry_set%vec, dim=1)
         end if
         if (allocated(automaton%all_segments)) then
            memsiz = memsiz + size(automaton%all_segments, dim=1)*8
         end if

         if (flags(FLAG_VERBOSE)) then
            cbuff = [pattern_key, text_key, parse_time, nfa_time, dfa_init_time, dfa_compile_time, matching_time,&
                     matching_result, memory, tree_count, nfa_count, dfa_count]
            call right_justify(cbuff)

            write(stdout, '(a, 1x, a)') trim(cbuff(1)), trim(adjustl(pattern))
            write(stdout, '(a, 1x, a)') trim(cbuff(2)), "'"//text_highlight_green(text,from,to)//"'"
            write(stdout, fmt_out_time) trim(cbuff(3)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(4)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_time) trim(cbuff(5)), get_lap_time_in_appropriate_unit(lap3)
            write(stdout, fmt_out_time) trim(cbuff(6)), get_lap_time_in_appropriate_unit(lap4)
            write(stdout, fmt_out_time) trim(cbuff(7)), get_lap_time_in_appropriate_unit(lap5)
            write(stdout, fmt_out_logi) trim(cbuff(8)), res
            write(stdout, fmt_out_int) trim(cbuff(9)), memsiz
            write(stdout, fmt_out_ratio) trim(cbuff(10)), tree%top, size(tree%nodes, dim=1)
            write(stdout, fmt_out_ratio) trim(cbuff(11)), automaton%nfa%nfa_top, automaton%nfa%nfa_limit
            write(stdout, fmt_out_ratio) trim(cbuff(12)), automaton%dfa%dfa_top, automaton%dfa%dfa_limit
         else if (flags(FLAG_NO_TABLE)) then
            continue
         else
            cbuff = [pattern_key, text_key, parse_time, nfa_time, dfa_init_time, dfa_compile_time, matching_time,&
            matching_result, memory, (repeat(" ", NUM_DIGIT_KEY), i = 1, 3)]
            call right_justify(cbuff)

            write(stdout, '(a, 1x, a)') trim(cbuff(1)), trim(adjustl(pattern))
            write(stdout, '(a, 1x, a)') trim(cbuff(2)), "'"//text_highlight_green(text,from,to)//"'"
            write(stdout, fmt_out_time) trim(cbuff(3)), get_lap_time_in_appropriate_unit(lap1)
            write(stdout, fmt_out_time) trim(cbuff(4)), get_lap_time_in_appropriate_unit(lap2)
            write(stdout, fmt_out_time) trim(cbuff(5)), get_lap_time_in_appropriate_unit(lap3)
            write(stdout, fmt_out_time) trim(cbuff(6)), get_lap_time_in_appropriate_unit(lap4)
            write(stdout, fmt_out_time) trim(cbuff(7)), get_lap_time_in_appropriate_unit(lap5)
            write(stdout, fmt_out_logi) trim(cbuff(8)), res
            write(stdout, fmt_out_int) trim(cbuff(9)), memsiz
         end if
         
         if (flags(FLAG_TABLE_ONLY))  then
            call automaton%free()
            return
         end if

         write(stdout, *) ""
         write(stdout, fmta, advance='no') trim(dfa_for_print)
         write(stdout, fmta) FOOTER
      end block output
      
      call automaton%free()
      
   end subroutine do_find_match_dense_dfa

   subroutine runner_do_matching_exactly(automaton, text, res, prefix, suffix, flag_no_literal_optimize, runs_engine)
      use :: forgex_automaton_m
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_cli_api_internal_no_opts_m
      use :: forgex_api_internal_m
      implicit none
      type(automaton_t), intent(inout) :: automaton
      character(*), intent(in) :: text
      logical, intent(inout) :: res
      logical, intent(inout) :: runs_engine
      logical, intent(in) :: flag_no_literal_optimize
      character(*), intent(in) :: prefix, suffix



      if (flag_no_literal_optimize) then
         call do_matching_exactly_no_literal_opts(automaton, text, res)
         runs_engine = .true.
      else
         call do_matching_exactly(automaton, text, res, prefix, suffix, runs_engine)
      end if

   end subroutine runner_do_matching_exactly


   subroutine runner_do_matching_including(automaton, text, from, to, prefix, suffix, flag_no_literal_optimize, runs_engine)
      use :: forgex_syntax_tree_optimize_m
      use :: forgex_automaton_m
      use :: forgex_api_internal_m
      use :: forgex_cli_api_internal_no_opts_m
      implicit none
      type(automaton_t), intent(inout) :: automaton
      character(*), intent(in) :: text
      integer(int32), intent(inout) :: from, to
      character(*), intent(in) :: prefix, suffix
      logical,intent(in) :: flag_no_literal_optimize
      logical, intent(inout) :: runs_engine

      if (flag_no_literal_optimize) then
         call do_matching_including_no_literal_opts(automaton, text, from, to)
         runs_engine = .true.
      else
         call do_matching_including(automaton, text, from, to, prefix, suffix, runs_engine)
      end if
   end subroutine runner_do_matching_including


end module forgex_cli_find_m