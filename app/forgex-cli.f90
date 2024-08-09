program forgex_cli
#if defined(IMPURE) && defined(DEBUG)
   use :: forgex_cli_m
   implicit none






#else
   implicit none
   print *, "forgex-cli is a simple benchmark program. To use this, you need to"
   print *, "build the program with IMPURE and DEBUG macros enabled."
   print *, 'Specifically, specify "-DIMPURE -DDEBGU" for the --flag of the fpm'
   print *, "command, or change [preprocess] in fpm.toml as follows:"
   print *, "..."
   print *, "[preprocess]"
   print *, "[preprocess.cpp]"
   print *, 'macros = ["IMPURE", "DEBUG"]'
   print *, ""
#endif
end program forgex_cli