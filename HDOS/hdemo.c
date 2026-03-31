/* Test/demo of HDOS support */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <arch/hdos.h>

int main(int argc, char *argv[])
{
    int ver, s, e;
    struct tm *t;
    time_t tt;

    tt = time(0);
    t = localtime(&tt);
    printf("tm_hour  = %d\n", t->tm_hour);
    printf("tm_min   = %d\n", t->tm_min);
    printf("tm_sec   = %d\n", t->tm_sec);
    printf("tm_year  = %d\n", t->tm_year);
    printf("tm_mon   = %d\n", t->tm_mon);
    printf("tm_mday  = %d\n", t->tm_mday);
    printf("tm_wday  = %d\n", t->tm_wday);
    printf("tm_yday  = %d\n", t->tm_yday);
    printf("tm_isdst = %d\n", t->tm_isdst);
    printf("asctime  = %s", asctime(t));

    ver = hdos_ver();
    printf("HDOS version is: %d.%d\n", ver / 16, ver % 16);

    printf("Default device is: '%s'\n", default_device());

    printf("Sleeping for 10 seconds...\n");
    s = clock();
    sleep(10);
    e = clock();

    printf("clock ticks: %d\n", e - s);
    printf("elapsed time: %d seconds\n", (e - s) / CLOCKS_PER_SEC);

    return 0;
}
