#include <math.h>
#include <stdint.h>
#include "ay8910.h"

/* Hardware dependent ports */
#define AY_ADDR_PORT  0xA0
#define AY_DATA_PORT  0xA1

#define AY_CLOCK 1789773

static uint8_t mixer_state = 0x3F;

extern void outp(uint16_t port, uint8_t val);
extern uint8_t inp(uint16_t port);

void ay_write(uint8_t reg, uint8_t value)
{
    outp(AY_ADDR_PORT, reg);
    outp(AY_DATA_PORT, value);
}

uint8_t ay_read(uint8_t reg)
{
    outp(AY_ADDR_PORT, reg);
    return inp(AY_DATA_PORT);
}

void ay_init(void)
{
    for (int i = 0; i < 16; i++)
        ay_write(i, 0);
}

void ay_set_tone(ay_channel_t ch, uint16_t period)
{
    uint8_t reg = ch * 2;

    ay_write(reg, period & 0xFF);
    ay_write(reg + 1, (period >> 8) & 0x0F);
}

void ay_set_volume(ay_channel_t ch, uint8_t volume)
{
    ay_write(AY_REG_VOL_A + ch, volume & 0x1F);
}

void ay_enable_tone(ay_channel_t ch, uint8_t enable)
{
    if (enable)
        mixer_state &= ~(1 << ch);
    else
        mixer_state |= (1 << ch);

    ay_write(AY_REG_MIXER, mixer_state);
}

void ay_enable_noise(ay_channel_t ch, uint8_t enable)
{
    uint8_t bit = ch + 3;

    if (enable)
        mixer_state &= ~(1 << bit);
    else
        mixer_state |= (1 << bit);

    ay_write(AY_REG_MIXER, mixer_state);
}

void ay_set_noise(uint8_t period)
{
    ay_write(AY_REG_NOISE_PERIOD, period & 0x1F);
}

void ay_set_envelope(uint16_t period, uint8_t shape)
{
    ay_write(AY_REG_ENV_FINE, period & 0xFF);
    ay_write(AY_REG_ENV_COARSE, period >> 8);
    ay_write(AY_REG_ENV_SHAPE, shape);
}

void ay_set_portA(uint8_t value)
{
    ay_write(AY_REG_IOA, value);
}

uint8_t ay_get_portA(void)
{
    return ay_read(AY_REG_IOA);
}

void ay_set_portB(uint8_t value)
{
    ay_write(AY_REG_IOB, value);
}

uint8_t ay_get_portB(void)
{
    return ay_read(AY_REG_IOB);
}

static float note_base_freq[12] =
{
    261.63, /* C */
    277.18,
    293.66,
    311.13,
    329.63,
    349.23,
    369.99,
    392.00,
    415.30,
    440.00,
    466.16,
    493.88
};

uint16_t ay_freq_to_period(uint32_t clock, float freq)
{
    if (freq <= 0.0)
        return 0;

    return (uint16_t)(clock / (16.0 * freq));
}

void ay_play_note(ay_channel_t ch, ay_note_t note, int octave, uint8_t volume)
{
    float freq;
    uint16_t period;

    freq = note_base_freq[note];

    /* octave adjustment */
    while (octave > 4)
    {
        freq *= 2.0;
        octave--;
    }

    while (octave < 4)
    {
        freq /= 2.0;
        octave++;
    }

    period = ay_freq_to_period(AY_CLOCK, freq);

    ay_set_tone(ch, period);
    ay_set_volume(ch, volume);
    ay_enable_tone(ch, 1);
}
