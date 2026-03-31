/* Test/demo of HDOS support */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <arch/hdos.h>

/* Return H89 clock speed, using 2/4MHz speed mod. Returns speed in
   MHz, i.e. 2 or 4, or -1 if unknown */

int cpu_speed()
{
    switch (bpeek(0x2036) & 0x10) {
    case 0x00:
        return 2;
        break;
    case 0x10:
        return 4;
    default:
        return -1;
    }
}

int main(int argc, char *argv[])
{
    int ver, s, e, c;
    struct tm *t;
    time_t tt;

    tt = time(0);
    t = localtime(&tt);
    ver = hdos_ver();
    c = cpu_speed();

    printf("HDOS version: %d.%d\n", ver / 16, ver % 16);
    printf("Default device: '%s'\n", default_device());
    printf("CPU speed: %d MHz\n", c);
    printf("Sleeping for 10 seconds...\n");

    s = clock();
    if (c == 4) {
        sleep(20);
    } else {
        sleep(10);
    }
    e = clock();

    printf("Clock ticks: %d\n", e - s);
    printf("Elapsed time: %d seconds\n", (e - s) / CLOCKS_PER_SEC);

    return 0;
}
