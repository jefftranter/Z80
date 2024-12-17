/*

  Input/output Routines for HDOS

  Jeff Tranter <tranter@pobox.com>

  Calling tree (for debuggging):

fopen()
  open()
fprintf()
  writebyte()
feof() Internal to z88dk, when read returns -1.
fgets()
  readbyte()
    read()
cgetc()
fclose()
  close();
WHAT CAUSES SY0: TO BE UNMOUNTED? (TRY SELECTIVELY REMOVING CODE)

*/

#include <stdio.h>
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
    printk("open(%s, %d, %d)=5\n", name, flags, mode);
    return 5;
}

int creat(const char *name, mode_t mode)
{
    printk("creat(%s, %d)\n", name, mode);
    return 10;
}

int close(int fd)
{
    printk("close(%d)\n", fd);
    return 0;
}

// If fd is stdin, stdout, or stderr, use console...
ssize_t read(int fd, void *ptr, size_t len)
{
    int i;

    printk("read(%d, %ld, %d)\n", fd, ptr, len);

    if ((fd == 0) || (fd == *stdin.desc.fd)) {
        for (i = 0; i < len; i++) {
            ptr[i] = fgetc_cons();
        }
        return len;
    }

    return -1;
}


// If fd is stdin, stdout, or stderr, use console...
ssize_t write(int fd, void *ptr, size_t len)
{
    int i;

    //printk("write(%d, %ld, %d)\n", fd, ptr, len);

    if ((fd == 1) || (fd ==2) || (fd == *stdout.desc.fd) || (fd == *stderr.desc.fd)) {
        for (i = 0; i < len; i++) {
            fputc_cons_native(ptr[i]);
        }
    }

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

    if ((fd == 0) || (fd == *stdin.desc.fd)) {
        return_nc fgetc_cons();
    }

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
    printk("writebyte(%d, '%c')\n", fd, byte);

    if ((fd == *stdout.desc.fd) || (fd == *stderr.desc.fd)) {
        fputc_cons_native(byte);
        return_nc 0;
    }

    return_nc write(fd, &byte, 1);
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
