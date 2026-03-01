/* ay_scale.c
 *
 * Play a C major scale on AY-3-8910 using z88dk.
 *
 */

#include <stdint.h>
#include <stdlib.h>     /* for outp() */
#include <stdio.h>

/* --- Hardware configuration --- */
#define PSG_REG_PORT  0xA0   /* Register select port (example: MSX) */
#define PSG_DATA_PORT 0xA1   /* Data port */

/* AY clock frequency */
#define PSG_CLOCK 1789773UL  /* ~1.79 MHz */

/* Delay loop (simple busy wait) */
void delay_ms(uint16_t ms)
{
    uint16_t i, j;
    for (i = 0; i < ms; ++i)
        for (j = 0; j < 30; ++j) {
            /* crude timing delay */
            __asm
                nop
            __endasm;
        }
}

/* Write to AY register */
void ay_write(uint8_t reg, uint8_t value)
{
    outp(PSG_REG_PORT, reg);
    outp(PSG_DATA_PORT, value);
}

/* Set tone period for channel A */
void ay_set_tone(uint16_t period)
{
    ay_write(0, period & 0xFF);        /* Fine tune */
    ay_write(1, (period >> 8) & 0x0F); /* Coarse tune */
}

/* Enable tone on channel A only */
void ay_enable_channel_a(void)
{
    /* Reg 7 mixer:
       bit0=1 disables tone A, so clear bit0 to enable
       disable noise and other channels */
    ay_write(7, 0b00111110);
}

/* Set volume for channel A (0-15) */
void ay_set_volume(uint8_t vol)
{
    ay_write(8, vol & 0x0F);
}

/* Convert frequency to AY period */
uint16_t freq_to_period(uint16_t freq)
{
    return (uint16_t)(PSG_CLOCK / (16UL * freq));
}

int main(void)
{
    /* C major scale frequencies (Hz) */
    uint16_t scale[] = {
        262, /* C4 */
        294, /* D4 */
        330, /* E4 */
        349, /* F4 */
        392, /* G4 */
        440, /* A4 */
        494, /* B4 */
        523  /* C5 */
    };

    uint8_t i;

    ay_enable_channel_a();
    ay_set_volume(15); /* max volume */

    for (i = 0; i < 8; ++i) {
        uint16_t period = freq_to_period(scale[i]);
        ay_set_tone(period);
        delay_ms(300);
    }
    
    ay_set_volume(0); /* min volume */

    return 0;
}
