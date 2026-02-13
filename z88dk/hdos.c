/*

  Platform Support Routines for HDOS

  Jeff Tranter <tranter@pobox.com>

  TODO:
  - Add support for opening multiple files using different channel numbers
  - Implement system()?
  - Implement lseek()?


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


/* Define this if you want file i/o support. Disable it to save memory
   when it is not needed. */
#define FS_SUPPORT 1

#include <fcntl.h>
#include <hdos.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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


/* Defines and macros */
#define BLOCK_SIZE 256
#define TIKCNT 0x201B
#define HDOS_DATE   ((volatile unsigned char *)0x20BF)
#define HDOS_TIME   ((volatile unsigned char *)0x20CA)

/* Disable/enable interrupts */
#define di() __asm__("di")
#define ei() __asm__("ei")

/* Global Variables */
#ifdef FS_SUPPORT
static char          default[7]; // Default device and extension
static char          fname[20]; // File name buffer
static unsigned char buf[BLOCK_SIZE];
static unsigned int  count;
static int           hfd;
static int           error;
static unsigned int  pos;      /* Current position in buffer */
static unsigned int  limit;    /* Valid bytes in buffer */
static int           eof_flag;
#endif

// Console output routine
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

// Console input routine
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

#ifdef FS_SUPPORT

// Initialize buffered writer
int bw_open(int fd)
{
    count = 0;
    hfd = fd;
    error = 0;
    pos = 0;
    limit = 0;
    eof_flag = 0;
    return 0;
}

// Initialize buffered reader
int br_open(int fd)
{
    count = 0;
    hfd   = fd;
    error = 0;
    pos = 0;
    limit = 0;
    eof_flag = 0;
    return 0;
}

// Read one 256-byte record
int hdos_read()
{
    static int rc;
    static uint8_t request, a;
    static uint16_t bc, de, hl;

    // Read buffer using .READ system call
    request = SYSCALL_READ; a = hfd; bc = BLOCK_SIZE; de = buf; hl = 0;
    rc = scall(request, &a, &bc, &de, &hl);

    // Will return non-zero on EOF
    return rc;
}

// Read a single byte
int br_getc()
{
    int rc;

    if (error)
        return -1;

    if (eof_flag)
        return -1;   /* EOF */

    /* Refill buffer if empty */
    if (pos >= limit) {

        rc = hdos_read();
        if (rc != 0) {
            eof_flag = 1;
            return -1;
        }

        pos = 0;
        limit = BLOCK_SIZE;
    }

    return buf[pos++];
}

// Write one 256-byte record
int hdos_write()
{
    static int rc;
    static uint8_t request, a;
    static uint16_t bc, de, hl;

    // Write buffer using .WRITE system call
    request = SYSCALL_WRITE; a = hfd; bc = BLOCK_SIZE; de = buf; hl = 0;
    rc = scall(request, &a, &bc, &de, &hl);

    return rc;
}

// Write a single byte
int bw_putc(int c)
{
    if (error)
        return -1;

    buf[count++] = (unsigned char)c;

    if (count == BLOCK_SIZE) {
        if (hdos_write(hfd, buf) != 0) {
            error = 1;
            return -1;
        }
        count = 0;
    }

    return c;
}

// Flush partial block (pad to 256 bytes)
int bw_flush()
{
    static unsigned int i;

    if (error) {
        return -1;
    }

    if (count == 0) {
        return 0;
    }

    /* Pad remainder of block */
    for (i = count; i < BLOCK_SIZE; i++) {
        buf[i] = 0;
    }

    if (hdos_write(hfd, buf) != 0) {
        error = 1;
        return -1;
    }

    count = 0;
    return 0;
}

int open(const char *name, int flags, mode_t mode)
{
    static uint8_t request, a;
    static uint16_t bc, de, hl;
    static int channel, rc;

    // Channel can be 0 to 5 (-1 is the running program). Hardcoded to
    // channel 3 for now.
    channel = 3;

    strcpy(default, "SY0TXT");
    strcpy(fname, name);

    if (mode & _IOREAD) {
        request = SYSCALL_OPENR;
    } else if (mode & _IOWRITE) {
        request = SYSCALL_OPENW;
    } else {
        return -1; // Error, no mode specified
    }

    a = channel;
    bc = 0; // Not used
    de = default;
    hl = fname;

    rc = scall(request, &a, &bc, &de, &hl);

    bw_open(channel);

    return channel;
}

int creat(const char *name, mode_t mode)
{
    /* A call to creat() is equivalent to calling open() with flags
       equal to O_CREAT|O_WRONLY|O_TRUNC. */

    return open(name, O_CREAT|O_WRONLY|O_TRUNC, mode);
}

int close(int fd)
{
    static uint8_t request, a;
    static uint16_t bc, de, hl;
    static int rc;

    bw_flush();

    a = 3; // Channel number; hardcoded to 3 for now.
    request = SYSCALL_CLOSE; bc = 0; de = 0; hl = 0;
    rc = scall(request, &a, &bc, &de, &hl);

    return rc;
}

ssize_t read(int fd, void *ptr, size_t len)
{
    static int i, c;

    for (i = 0; i < len; i++) {
        c = br_getc();
        if (c == -1) {
            return -1; /* EOF */
            break;
        }
        ptr[i] = c;
    }

    if (i == len) {
        return i;
    } else {
        return -1; /* EOF */
    }
}

ssize_t write(int fd, void *ptr, size_t len)
{
    static int i;

    for (i = 0; i < len; i++) {
        bw_putc(ptr[i]);
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
    static unsigned char buffer;

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
}

/* Not sure if this needs to be implemented. */
int fsync(int fd)
{
    return 0;
}

/* Remove a file */
int remove(const char *name)
{
    static uint8_t request, a;
    static uint16_t bc, de, hl;
    static int rc;

    strcpy(default, "SY0TXT");
    strcpy(fname, name);

    a = 0; // Not used
    bc = 0; // Not used
    de = default;
    hl = fname;
    request = SYSCALL_DELETE;

    rc = scall(request, &a, &bc, &de, &hl);

    return rc;
}

/* Rename a file from s to d */
int rename(const char *s, const char *d)
{
    static uint8_t request, a;
    static uint16_t bc, de, hl;
    static int rc;

    strcpy(default, "SY0TXT");

    a = 0; // Not used
    bc = d; // New name
    de = default; // Default block
    hl = s; // Old name
    request = SYSCALL_RENAME;

    rc = scall(request, &a, &bc, &de, &hl);

    return rc;
}
#endif

/* Beep the H-89/H-19 speaker for ms milliseconds. */
void beep(unsigned int ms)
{
    while (ms--) {
        putchar('\a');
    }
}

/* Delay for specified milliseconds. Uses 2 ms clock interrupt. */
void delay(unsigned int ms)
{
    unsigned int t;

    ms >>= 1; // Divide by 2 to get interrupts
    t = wpeek(TIKCNT) + ms + 1;
    di();
    wpoke(TIKCNT, 0);
    ei();
    while (ms > wpeek(TIKCNT))
        ;
    di();
    wpoke(TIKCNT, t);
    ei();
}

/* Convert packed BCD byte to integer */
static unsigned char bcd_to_int(unsigned char bcd)
{
    return ((bcd >> 4) * 10) + (bcd & 0x0F);
}

/* Convert integer (0-99) to packed BCD */
static unsigned char int_to_bcd(unsigned char val)
{
    return ((val / 10) << 4) | (val % 10);
}

/* Return HDOS date in format dd-Mmm-yy */
void rddate(char *date)
{
    int i;

    for (i = 0; i < 9; i++)
        date[i] = HDOS_DATE[i];
    date[9] = '\0';
}

/* Return HDOS time in format hh:mm:ss */
void rdtime(char *time)
{
    unsigned char hh, mm, ss;

    hh = bcd_to_int(HDOS_TIME[0]);
    mm = bcd_to_int(HDOS_TIME[1]);
    ss = bcd_to_int(HDOS_TIME[2]);

    sprintf(time, "%02u:%02u:%02u", hh, mm, ss);
}

/* Set HDOS date in format dd-Mmm-yy */
void wrdate(char *date)
{
    int i;

    if (strlen(date) != 9) {
        printf("Error: date must be exactly 9 characters.\n");
        return;
    }

    for (i = 0; i < 9; i++)
        HDOS_DATE[i] = date[i];
}

/* Set HDOS time in format hh:mm:ss */
void wrtime(char *time)
{
    unsigned int hh, mm, ss;

    if (sscanf(time, "%u:%u:%u", &hh, &mm, &ss) != 3) {
        printf("Error: time format must be hh:mm:ss\n");
        return;
    }

    if (hh > 23 || mm > 59 || ss > 59) {
        printf("Error: invalid time value\n");
        return;
    }

    HDOS_TIME[0] = int_to_bcd((unsigned char)hh);
    HDOS_TIME[1] = int_to_bcd((unsigned char)mm);
    HDOS_TIME[2] = int_to_bcd((unsigned char)ss);
}

/* Wrapper for HDOS system call (scall). Pass in scall number and register values.
   Returns carry status (normally 1 for error, 0 for success. */
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl)
{
    static Z80_registers regs;
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
