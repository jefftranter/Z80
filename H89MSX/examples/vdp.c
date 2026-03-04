#include <stdlib.h>

/* VDP ports */
#define VDP_DATA    0x98
#define VDP_CTRL    0x99

/* Graphics I layout */
#define NAME_TABLE      0x1800
#define PATTERN_TABLE   0x0000
#define COLOR_TABLE     0x2000

/* -------------------------------------------------- */
/* Basic VDP access using outp()                     */
/* -------------------------------------------------- */

void vdp_write(uint8_t value)
{
    outp(VDP_DATA, value);
}

void vdp_set_write_address(uint16_t addr)
{
    outp(VDP_CTRL, addr & 0xFF);
    outp(VDP_CTRL, (addr >> 8) | 0x40);
}

void vdp_write_register(uint8_t reg, uint8_t value)
{
    outp(VDP_CTRL, value);
    outp(VDP_CTRL, 0x80 | reg);
}

/* -------------------------------------------------- */
/* Initialize Graphics I Mode                        */
/* -------------------------------------------------- */

void vdp_init_graphics1(void)
{
    vdp_write_register(0, 0x00);   /* Mode bits */
    vdp_write_register(1, 0xE0);   /* Display on, 16K VRAM */

    vdp_write_register(2, NAME_TABLE >> 10);
    vdp_write_register(3, COLOR_TABLE >> 6);
    vdp_write_register(4, PATTERN_TABLE >> 11);

    vdp_write_register(5, 0x00);
    vdp_write_register(6, 0x00);

    vdp_write_register(7, 0xF1);   /* White on black */
}

/* -------------------------------------------------- */
/* 8×8 ASCII Font (32–127)                           */
/* Public domain 8x8 font                             */
/* -------------------------------------------------- */

const uint8_t font8x8_basic[96][8] = {
#include "font8x8_basic.inl"  
};

/* -------------------------------------------------- */
/* Download Font                                     */
/* -------------------------------------------------- */

void download_font(void)
{
    uint16_t ch;
    uint8_t row;

    /* Clear entire pattern table */
    vdp_set_write_address(PATTERN_TABLE);
    for (ch = 0; ch < 256 * 8; ch++)
        vdp_write(0x00);

    /* Load printable ASCII 32–127 */
    for (ch = 32; ch < 128; ch++)
    {
        vdp_set_write_address(PATTERN_TABLE + ch * 8);

        for (row = 0; row < 8; row++)
        {
            vdp_write(font8x8_basic[ch - 32][row]);
        }
    }

    /* Set all colors white on black */
    vdp_set_write_address(COLOR_TABLE);
    for (ch = 0; ch < 32; ch++)
        vdp_write(0xF1);
}

/* -------------------------------------------------- */
/* Clear Screen                                      */
/* -------------------------------------------------- */

void clear_screen(void)
{
    uint16_t i;

    vdp_set_write_address(NAME_TABLE);

    for (i = 0; i < 32 * 24; i++)
        vdp_write(32);   /* space */
}

/* -------------------------------------------------- */
/* Write Text                                        */
/* -------------------------------------------------- */

void write_text(uint8_t row, uint8_t col, const char *s)
{
    uint16_t addr = NAME_TABLE + row * 32 + col;

    vdp_set_write_address(addr);

    while (*s)
        vdp_write(*s++);
}

/* -------------------------------------------------- */
/* Main                                              */
/* -------------------------------------------------- */

void main(void)
{
    vdp_init_graphics1();
    download_font();
    clear_screen();

    write_text(0, 0, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    write_text(2, 0, "0123456789");
    write_text(4, 0, "GRAPHICS I MODE - 8x8 ASCII");

    while (1)
        ;
}
