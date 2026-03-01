/* vdp_colorbars_direct.c
 *
 * Direct TMS9918 programming example for MSX (z88dk).
 * Displays 8 vertical color bars using Graphics II–style layout.
 *
 */

#include <stdlib.h>

#define VDP_DATA 0x98
#define VDP_CTRL 0x99

/* Write VDP register */
void vdp_set_reg(uint8_t reg, uint8_t value)
{
    outp(VDP_CTRL, value);
    outp(VDP_CTRL, 0x80 | reg);
}

/* Set VRAM write address */
void vdp_set_write(uint16_t addr)
{
    outp(VDP_CTRL, addr & 0xFF);
    outp(VDP_CTRL, 0x40 | ((addr >> 8) & 0x3F));
}

/* Write byte to VRAM */
void vdp_write(uint8_t value)
{
    outp(VDP_DATA, value);
}

/* Initialize VDP for Graphics II mode (SCREEN 2–like) */
void vdp_init_screen2(void)
{
    /* Register setup for 256x192 Graphics II */
    vdp_set_reg(0, 0x02);  /* M3=1 (Graphics II) */
    vdp_set_reg(1, 0xE0);  /* Display ON, no IRQ */
    vdp_set_reg(2, 0x06);  /* Name table at 0x1800 */
    vdp_set_reg(3, 0xFF);  /* Color table at 0x2000 (full mask) */
    vdp_set_reg(4, 0x00);  /* Pattern table at 0x0000 */
    vdp_set_reg(5, 0x36);  /* Sprite attr table (unused here) */
    vdp_set_reg(6, 0x07);  /* Sprite pattern table (unused) */
    vdp_set_reg(7, 0x01);  /* Background color = blue */
}

int main(void)
{
    uint16_t i, j;
    uint8_t color;

    vdp_init_screen2();

    /* --- Pattern table (0x0000) ---
       Create 8 solid 8x8 tiles */
    vdp_set_write(0x0000);
    for (i = 0; i < 8; ++i) {
        for (j = 0; j < 8; ++j) {
            vdp_write(0xFF);  /* all pixels on */
        }
    }

    /* --- Color table (0x2000) ---
       Each tile gets a unique color */
    vdp_set_write(0x2000);
    for (i = 0; i < 8; ++i) {
        color = ((i + 1) << 4) | 0x01; /* FG color, blue BG */
        for (j = 0; j < 8; ++j) {
            vdp_write(color);
        }
    }

    /* --- Name table (0x1800) ---
       32x24 tile map -> 8 vertical bars */
    vdp_set_write(0x1800);
    for (i = 0; i < 24; ++i) {
        for (j = 0; j < 32; ++j) {
            vdp_write(j / 4); /* 8 bars across screen */
        }
    }

    return 0;
}
