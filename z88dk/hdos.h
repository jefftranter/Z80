/*

  Utility Routines for HDOS

  Jeff Tranter <tranter@pobox.com>

*/

#include <arch/z80.h>

// System calls
#define SYSCALL_EXIT   00
#define SYSCALL_SCIN   01
#define SYSCALL_SCOUT  02
#define SYSCALL_PRINT  03
#define SYSCALL_READ   04
#define SYSCALL_WRITE  05
#define SYSCALL_CONSL  06
#define SYSCALL_CLRCO  07
#define SYSCALL_LOADO  010
#define SYSCALL_VERS   011
#define SYSCALL_GDA    012
#define SYSCALL_CRC16  013
#define SYSCALL_LINK   040
#define SYSCALL_CTLC   041
#define SYSCALL_OPENR  042
#define SYSCALL_OPENW  043
#define SYSCALL_OPENU  044
#define SYSCALL_OPENC  045
#define SYSCALL_CLOSE  046
#define SYSCALL_POSIT  047
#define SYSCALL_DELETE 050
#define SYSCALL_RENAME 051
#define SYSCALL_SETTOP 052
#define SYSCALL_DECODE 053
#define SYSCALL_NAME   054
#define SYSCALL_CLEAR  055
#define SYSCALL_CLEARA 056
#define SYSCALL_ERROR  057
#define SYSCALL_CHFLG  060
#define SYSCALL_DISMT  061
#define SYSCALL_LOADD  062
#define SYSCALL_TASK   0101
#define SYSCALL_TDU    0122
#define SYSCALL_LOG    0177
#define SYSCALL_MOUNT  0200
#define SYSCALL_MONMS  0202
#define SYSCALL_DMNMS  0203
#define SYSCALL_RESET  0204
#define SYSCALL_RESNMS 0205
#define SYSCALL_DAD    0206

/* Wrapper for HDOS system call (scall). Pass in scall number and register values.
   Returns carry status (normally 1 for error, 0 for success. */
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl);

/* Beep the H-89/H-19 speaker for ms milliseconds. */
void beep(unsigned int ms);

/* Delay for specified milliseconds. Uses 2 ms clock interrupt. */
void delay(unsigned int ms);

/* Return HDOS date in format dd-Mmm-yy */
void rddate(char *date);

/* Return HDOS time in format hh:mm:ss */
void rdtime(char *time);

/* Set HDOS date in format dd-Mmm-yy */
void wrdate(char *date);

/* Set HDOS time in format hh:mm:ss */
void wrtime(char *time);

/* Return HDOS version number in BCD (e.g. $21 for 2.1) */
int hdosversion();
