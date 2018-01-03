#include <stdio.h>

int main() {
  long b = 106700;
  long c = 123700;
  int h = 0;

  while (1)
  { 
    printf("h = %d\n", h);
    long f = 1;
    for (long d = 2; d - b != 0; d++)
    {
      for (long e = 2; e - b != 0; e++)
      {
        if (d * e > b)
          break;

        if (d * e - b == 0) 
          f = 0;
      } 
    } 
    
    if (f == 0) h++;
   
    if (b - c != 0) 
    {
      b += 17;
    }
    else
    {
      printf("h = %d\n", h);
      return 0;
    }
  }
}
