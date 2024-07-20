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
    printf("Hello from z88dk!\r\n");

    /* Get characters and echo back. Exit if Control-Z pressed. */
    while (1) {
        int c = getchar();
        printf("%c\r\n", c);
        if (c == 26)
            break; /* Control-Z */
    }

    printf("\r\nGoodbye!\r\n");

    return 0;
}
