#include <conio.h>
#include <stdio.h>
#include <string.h>
#include "hdosutil.h"

#define SYSCALL_VERS 011

int main()
{
    int rc;
    uint8_t a;
    uint16_t bc;
    uint16_t de;
    uint16_t hl;
    static char s[80];

    clrscr();

    printf("clock() returned %d\n", clock());
    printf("clock() returned %d\n", clock());
    printf("clock() returned %d\n", clock());
    printf("clock() returned %d\n", clock());

    printf("delay(1000)\n");
    delay(1000);

    printf("beep(1000)\n");
    beep(200);

    rddate(s);
    printf("Date: %s\n", s);

    rdtime(s);
    printf("Time: %s\n", s);

    a = 0; bc = 0; de = 0; hl = 0;
    printf("\nCalling .VERS\n");
    rc = scall(SYSCALL_VERS, &a, &bc, &de, &hl);

    printf("Returned %d, a=%02x bc=%04x de=%04x hl=%04x\n", rc, a, bc, de, hl);

    return 0;
}
