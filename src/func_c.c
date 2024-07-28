#include <stdio.h>
#ifdef DEBUG
void message(int l, int n) {
   printf("%d: %d\n", l, n);
   return;
}

void message2(int line, int i, int j){
   printf("%d: %d, %d\n", line, i, j);
   return;
}

void message_char(int l, const char* str) {
   printf("%d: %s\n", l, str);
   return;
}

#endif