/*

  Library of Utility Routines for HDOS.

  Jeff Tranter <tranter@pobox.com>

*/

#include <stdint.h>

/* Clock rate, as returned by clock(), is 2 ms */
#ifdef CLOCKS_PER_SEC
#undef CLOCKS_PER_SEC
#endif
#define CLOCKS_PER_SEC 500

/* Clear the H89/H19 screen. */
#ifdef clrscr
#undef clrscr
#endif
#define	clrscr() cprintf("\eE")

/* Beep the H89/H19 speaker for ms milliseconds. */
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

/* Return clock ticks. Increases at the rate of CLOCKS_PER_SEC. Not
   guaranteed to be monotonic. */
clock_t clock();

/* Return default disk device, e.g. "SY0" */
char *defaultDevice();

/* Return default file extension, e.g. "ABS" */
char *defaultExtension();

/* Wrapper for HDOS system call (scall). Pass in scall number and register values.
   Returns carry status (normally 1 for error, 0 for success. */
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl);
