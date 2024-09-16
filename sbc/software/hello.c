/*

  Simple z88dk C program example for Z80 SBC.

  Jeff Tranter <tranter@pobox.com>

*/

#include <stdio.h>
#include <math.h>

int main()
{
    int i;

    /* Print a message */
    printf("\nHello from z88dk!\n");

    for (i = 0; i <= 100; i++) {
        printf("%d %d %f\n", i, i*i, sqrt(i));
    }

    printf("\nPress a key to echo, 'Q' to quit\n");

    /* Get characters and echo back. Exit if 'q', or 'Q' pressed. */
    while (1) {
        int c = getchar();
        printf("%c", c);
        if (c == 'q' || c == 'Q')
            break;
    }

    printf("\nGoodbye!\n");

    return 0;
}
