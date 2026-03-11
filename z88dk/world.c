/*

  Simple z88dk C program example for Heathkit H89.

  Jeff Tranter <tranter@pobox.com>

*/

#include <stdio.h>
#include <unistd.h>

int main()
{

/* Test for execv(). For now, need to move stack pointer back to
   default value in order for .LINK system call to succeed. */

 /*
#asm
    ld sp,0x2280
#endasm
    execv("SY0:BASIC.ABS", 0);
 */

    /* Print a message */
    printf("\nHello from z88dk!\n");

    /* Get characters and echo back. Exit if 'q', or 'Q' pressed. */
    printf("\nType characters to echo, 'q' or 'Q' to quit:\n");

    while (1) {
        int c = getchar();
        printf("%c", c);
        if (c == 'q' || c == 'Q')
            break;
    }

    printf("\nGoodbye!\n");

    return 0;
}
