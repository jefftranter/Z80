#include <string.h>
#include "hdosutil.h"

/* Return default file extension, e.g. "ABS" */
char *defaultExtension()
{
    static char s[4];

    /* Stored as 3 bytes from $006D-$006F (HDOS 3 only) */
    memcpy(s, 0x6d, 3);
    s[3] = 0; // Null terminate.

    return s;
}
