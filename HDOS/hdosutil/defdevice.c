#include <string.h>
#include "hdosutil.h"

/* Return default disk device, e.g. "SY0" */
char *defaultDevice()
{
    static char s[4];

    /* Stored as 3 bytes from $006A-$006C (HDOS 3 only) */
    memcpy(s, 0x6a, 3);
    s[3] = 0; // Null terminate.

    return s;
}
