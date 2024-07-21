/*

  Simple z88dk C program example for Heathkit H89.

  Jeff Tranter <tranter@pobox.com>

*/


/* Tells C run-time to return to caller on exit */
#pragma-define CRT_ON_EXIT=0x10002

#include <stdio.h>

void init();

int main()
{
    init();

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
