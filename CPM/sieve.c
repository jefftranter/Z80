/* From http://www.scriptol.com/programming/sieve.php */

/* Sieve Of Erathosthenes by Denis Sureau */

#include <stdlib.h> 
#include <stdio.h>

int all[10000];

void eratosthenes(int top)
{
    int idx = 0;
    int prime = 3;
    int x, j;
		
    printf("1\n");
	
    while (prime <= top) {
        for (x = 0; x < top; x++) {
            if (all[x] == prime) goto skip; 
        }

        printf("%d\n", prime);
        j = prime;
        while (j <= (top / prime)) {
            all[idx++] = prime * j;
            j += 1;
        }

skip:
        prime+=2;
    }
    puts("");
    return;
}

int main(int argc, char *argv[])
{
    int n;
    if (argc == 2) {
        n = atoi(argv[1]);
    } else {
        n = 1000;
    }

    printf("\nPrime numbers up to %d:\n", n);
    eratosthenes(n);
    printf("Done.\n");
    return 0;
}
