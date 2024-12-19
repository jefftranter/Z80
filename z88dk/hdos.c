/*

  Input/output Routines for HDOS

  Jeff Tranter <tranter@pobox.com>

  Calling tree (for debuggging):
fopen()
  open()
fprintf()
  writebyte()
    write()
feof() Internal to z88dk, when read returns -1.
fgets()
  readbyte()
    read()
cgetc()
fclose()
  close();

*/

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <hdos.h>

__asm
SCALL   macro   call            ; SYSCALL macro
        rst     $38
        db      call
        endm
EXIT    equ     0               ; HDOS System calls
SCIN    equ     1
SCOUT   equ     2
CONSL   equ     6
CSLMD   equ     0               ; Index for console mode
CSLECH  equ     %10000000       ; Bit for suppress echo
CSLCHR  equ     %00000001       ; Bit for update in character mode
__endasm

/* Global Variables */
char default[7]; // Default device and extension
char fname[20]; // File name buffer

int fputc_cons_native(char c) __naked
{
__asm
        pop     bc              ; Return address
        pop     hl              ; Character to print in l
        push    hl
        push    bc
        ld      a,l             ; Get char to print
        cmp     '\n'            ; Is it newline?
        jr      nz,pr1          ; Branch if not
        ld      a,'\r'          ; If so, first print return
        SCALL   SCOUT
        ld      a,'\n'          ; Now print newline
pr1:    SCALL   SCOUT           ; System call for System Console Output
        ret
__endasm
}

int fgetc_cons() __naked
{
__asm
nr:     SCALL   SCIN            ; System call for System Console Input
        jr      c,nr            ; Try again if no character ready
        ld      l,a             ; Return the result in hl
        ld      h,0
        ret
__endasm
}

/* File i/o functions */

int open(const char *name, int flags, mode_t mode)
{
    uint8_t request, a;
    uint16_t bc, de, hl;
    int channel, rc;

    printk("open(%s, %d, %d)=5\n", name, flags, mode);

    printk("flags:");
    if (flags & O_RDONLY)
        printk(" O_RDONLY");
    if (flags & O_WRONLY)
        printk(" O_WRONLY");
    if (flags & O_RDWR)
        printk(" O_RDWR");
    if (flags & O_TRUNC)
        printk(" O_TRUNC");
    if (flags & O_CREAT)
        printk(" O_CREAT");
    if (flags & O_APPEND)
        printk(" O_APPEND");
    if (flags & O_BINARY)
        printk(" O_BINARY");
    printk("\nmode:");
    if (mode & _IOUSE)
        printk(" _IOUSE");
    if (mode & _IOREAD)
        printk(" _IOREAD");
    if (mode & _IOWRITE)
        printk(" _IOWRITE");
    if (mode & _IOEOF)
        printk(" _IOEOF");
    if (mode & _IOSYSTEM)
        printk(" _IOSYSTEM");
    if (mode & _IOEXTRA)
        printk(" _IOEXTRA");
    if (mode & _IOTEXT)
        printk(" _IOTEXT");
    if (mode & _IOSTRING)
        printk(" _IOSTRING");
    printk("\n");

    // Channel can be 0 to 5 (-1 is the running program). Initially
    // hardcode to only channel 1 for now.
    channel = 5;

    strcpy(default, "SY0TMP");
    strcpy(fname, "SY0:TEXT.TXT");

    if (mode & _IOREAD) {
        request = SYSCALL_OPENR;
    } else if (mode & _IOWRITE) {
        request = SYSCALL_OPENW;
    }

    a = channel;
    bc = 0; // Not used
    de = default;
    hl = fname;

    printk("Calling syscall(%d, %d, %d, %d, %d)\n", request, a, bc, de, hl);
    rc = scall(request, &a, &bc, &de, &hl);
    printk("return code = %d\n", rc);
    printk("Returned with a=%d bc=%d de=%d hl=%d\n", a, bc, de, hl);

    return channel;
}

int creat(const char *name, mode_t mode)
{
    printk("creat(%s, %d)\n", name, mode);
    printk("mode:");
    if (mode & _IOUSE)
        printk(" _IOUSE");
    if (mode & _IOREAD)
        printk(" _IOREAD");
    if (mode & _IOWRITE)
        printk(" _IOWRITE");
    if (mode & _IOEOF)
        printk(" _IOEOF");
    if (mode & _IOSYSTEM)
        printk(" _IOSYSTEM");
    if (mode & _IOEXTRA)
        printk(" _IOEXTRA");
    if (mode & _IOTEXT)
        printk(" _IOTEXT");
    if (mode & _IOSTRING)
        printk(" _IOSTRING");
    printk("\n");

    return 10;
}

int close(int fd)
{
    uint8_t a;
    uint16_t bc, de, hl;
    int rc;

    printk("close(%d)\n", fd);

    a = 5;
    bc = 0; de = 0; hl = 0;
    printk("Calling scall(%d, %d, %d, %d, %d)\n", SYSCALL_CLOSE, a, bc, de, hl);
    rc = scall(SYSCALL_CLOSE, &a, &bc, &de, &hl);
    printk("return code = %d\n", rc);
    printk("Returned with a=%d bc=%d de=%d hl=%d\n", a, bc, de, hl);

    return 0;
}

ssize_t read(int fd, void *ptr, size_t len)
{
    printk("read(%d, %ld, %d)\n", fd, ptr, len);
    return -1; /* EOF */
}

ssize_t write(int fd, void *ptr, size_t len)
{
    // Uncommenting the line below will cause infinite recursion
    //printk("write(%d, %ld, %d)\n", fd, ptr, len);

    return len;
}

long lseek(int fd, long posn, int whence)
{
    printk("lseek(%d, %ld, %d)\n", fd, posn, whence);
    return posn;
}

/* This function reads a byte from filehandle fd (which is supplied in
    the register pair hl), if an error occurred it should return EOF
    (-1) and return with carry set. Otherwise it should return with
    carry reset and hl holding the byte just read
*/

int readbyte(int fd)
{
    unsigned char buffer;

    printk("readbyte(%d)\n", fd);

    if (read(fd, &buffer, 1) <= 0) {
        return_c -1;
    }

    return_nc buffer;
}

/*  This function writes byte c to filehandle fd, once more if an
    error occurs it should return EOF and carry set, otherwise hl
    holds the byte just written and carry is reset */

int writebyte(int fd, int byte)
{
    return(write(fd, &byte, 1));
}

/* Abandon file with the handle fd - this is called by the system on
   program exit should it not be able to close a file. This function
   is a dummy function on the z88 but for example on the Spectrum +3
   this function would be of use. */

void fabandon(FILE *fp)
{
    printk("fabandon(%d)\n", fp);
}

/* Not sure if this needs to be implemented. */
int fsync(int fd)
{
    printk("fsync(%d)\n", fd);
    return 0;
}

/* Wrapper for HDOS system call (scall). Pass in scall number and register values.
   Returns carry status (normally 1 for error, 0 for success. */
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl)
{
    Z80_registers regs;
    extern uint8_t *CALLNO;

    // Write system call number after RST instruction
    z80_bpoke(&CALLNO, request);

    // Set up register parameters.
    regs.Bytes.A = *a;
    regs.UWords.BC = *bc;
    regs.UWords.DE = *de;
    regs.UWords.HL = *hl;

    // Call SYSCALL routine below.
    AsmCall(&SYSCALL, &regs, REGS_MAIN, REGS_MAIN);

    // Get back register values to return.
    *a = regs.Bytes.A;
    *bc = regs.Words.BC;
    *de = regs.Words.DE;
    *hl = regs.Words.HL;

    // Return carry status
    return regs.Flags.C;
}

#asm
_SYSCALL:
        rst     $38     ; System call
_CALLNO:
        db      $00     ; Call # goes here
        ret
#endasm
