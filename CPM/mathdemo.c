#include <stdio.h>
#include <math.h>

int main()
{
    int i;

    printf("%s", " i    sqrt      sin       cos\n");
    printf("%s", "-- --------- --------- ---------\n");

    for (i = 0; i <= 45; i++) {
        printf("%2d %8.7f %8.7f %8.7f\n",
               i,
               sqrt(i),
               sin(2*M_PI*i/360),
               cos(2*M_PI*i/360)
               );
    }

    return 0;
}
