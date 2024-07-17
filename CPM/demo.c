#include <stdio.h>
#include <string.h>

int main()
{
    printf ("C Program Demo\n");
    printf ("--------------\n");

    printf("size of char: %d\n", sizeof(char));
    printf("size of short: %d\n", sizeof(short));
    printf("size of int: %d\n", sizeof(int));
    printf("size of long: %d\n", sizeof(long));
    printf("size of float: %d\n", sizeof(float));
    printf("size of double: %d\n", sizeof(double));

    printf("\nEnter some text: ");
    char buffer[80];
    fgets(buffer, sizeof(buffer)-1, stdin);
    printf("Length of entered text is %d.\n", strlen(buffer));
    printf("\nYou entered: ");
    printf(buffer);

    return 0;
}
