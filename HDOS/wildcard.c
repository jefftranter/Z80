#include <libgen.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
    unsigned char s;

    //unsigned char *glob(const char *s, const char *pattern);
    //unsigned char *glob_dos(const char *s, const char *pattern);
    //unsigned char *glob_fat(const char *s, const char *pattern);

    s = glob("FILE1.TXT", "FILE1.*");
    printf("s = %d\n", s);
    s = glob("FILE1.TXT", "*.TXT");
    printf("s = %d\n", s);
    s = glob("FILE1.TXT", "FILE*.*");
    printf("s = %d\n", s);
    s = glob("FILE1.TXT", "*.BAK");
    printf("s = %d\n", s);
    s = glob("FILE1.TXT", "FILE2.*");
    printf("s = %d\n", s);
    s = glob("FILE1.TXT", "FILE1.???");
    printf("s = %d\n", s);

    s = glob_dos("FILE1.TXT", "FILE1.*");
    printf("s = %d\n", s);
    s = glob_dos("FILE1.TXT", "*.TXT");
    printf("s = %d\n", s);
    s = glob_dos("FILE1.TXT", "FILE*.*");
    printf("s = %d\n", s);

    s = glob_fat("FILE1.TXT", "FILE1.*");
    printf("s = %d\n", s);
    s = glob_fat("FILE1.TXT", "*.TXT");
    printf("s = %d\n", s);
    s = glob_fat("FILE1.TXT", "FILE*.*");
    printf("s = %d\n", s);

    return 0;
}

