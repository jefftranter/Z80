/*

   Demonstrate expanding filenames with wildcards under CP/M.
   Compiles using z88dk compiler.

 */

#include <stdio.h>
#include <string.h>
#include <cpm.h>

#define MAXWILDCARDS 40
#define FILENAMELEN 13    // 8 + 3 + '.' + terminating null

/* Return a list of strings matching a wildcard pattern. Returns the
   number of matches. */
int wildcard(char *pattern, char matches[MAXWILDCARDS][FILENAMELEN])
{
    int i, rc;
    char *p;
    struct fcb *file;

    //fprintf(stderr, "wildcard: '%s'\n", pattern);

    /* Get FCB. */
    file = getfcb();

    /* Put pattern in FCB. */
    rc = setfcb(file, pattern);
    //fprintf(stderr, "setfcb() returned %d\n", rc);

    for (i = 0; i < MAXWILDCARDS; i++) {
        if (i == 0) {        
            /* First call Find First to get first match of filename pattern. */
            rc = bdos(CPM_FFST, file);
            //fprintf(stderr, "CPM_FFST returned %d\n", rc);
            if (rc == -1)
                break;
        } else {
            /* Then call Find Next for any subsequent matches */
            rc = bdos(CPM_FNXT, file);
            //fprintf(stderr, "CPM_FNXT returned %d\n", rc);
            if (rc == -1)
                break;
        }

        /* Directory entry will be at DMA + rc * 32 (where rc is 0-3).
           File name (8 chars) starts at offset 1 and file extension
           (3 chars) at offset 9. */       
        p = (char *) 0x80 + rc * 32;
        memcpy(&matches[i][0], p + 1, 8);
        matches[i][8] = '.';
        memcpy(&matches[i][9], p + 9, 3);
        matches[i][12] = 0;
        //fprintf(stderr, "wildcard match %d: '%s'\n", i, matches[i]);
    }

    clearfcb(file);
    return i;
}

int main(int argc, char **argv)
{
    int i, j, k;
    char matches[MAXWILDCARDS][FILENAMELEN];
    char patterns[MAXWILDCARDS][FILENAMELEN];

    if (argc < 2) {
        fprintf(stderr, "usage: %s <filename>...\n", argv[0]);
        return 1;
    }

    /* Need to save the command line arguments before making any BDOS
       calls as they overwrite the same DMA buffer area. */   
    for (i = 0; i < argc - 1; i++) {
        strcpy(patterns[i], argv[i+1]);
        //fprintf(stderr, "Argument '%s'\n", patterns[i]); 
    }

    /* Expand wildcard arguments. */
    for (i = 0; i < argc - 1; i++) {
        k = wildcard(patterns[i], matches);
        fprintf(stderr, "Pattern '%s' matches:\n", patterns[i]);
        for (j = 0; j < k; j++) {
            fprintf(stderr, "%s\n", matches[j]);
        }
    }

    return 0;
}
