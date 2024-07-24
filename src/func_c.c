#include <stdio.h>

void message(int l, int n) {
   printf("%d: %d\n", l, n);
   return;
}

void message_char(int l, const char* str) {
   printf("%d: %s\n", l, str);
   return;
}