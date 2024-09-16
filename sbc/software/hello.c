/*

  Simple z88dk C program example for Z80 SBC.

  Jeff Tranter <tranter@pobox.com>

*/

#include <stdio.h>

int main()
{
    int i;

    /* Print a message */
    printf("\nHello from z88dk!\n");

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
