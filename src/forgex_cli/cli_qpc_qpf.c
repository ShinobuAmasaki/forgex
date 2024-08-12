// This file is a stub to building on Unix-like systems.
#if defined(_WIN32) || defined(_WIN64)
#else
#include <stdbool.h>
bool QueryPerformanceCounter(long long PerformanceCount_count) {
   return false;
}
bool QueryPerformanceFrequency(long long Frequency_countPerSec) {
   return false;
}
#endif