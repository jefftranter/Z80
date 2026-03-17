#ifndef AY8910_H
#define AY8910_H

#include <stdint.h>

/* AY register numbers */
#define AY_REG_CH_A_FINE     0
#define AY_REG_CH_A_COARSE   1
#define AY_REG_CH_B_FINE     2
#define AY_REG_CH_B_COARSE   3
#define AY_REG_CH_C_FINE     4
#define AY_REG_CH_C_COARSE   5
#define AY_REG_NOISE_PERIOD  6
#define AY_REG_MIXER         7
#define AY_REG_VOL_A         8
#define AY_REG_VOL_B         9
#define AY_REG_VOL_C         10
#define AY_REG_ENV_FINE      11
#define AY_REG_ENV_COARSE    12
#define AY_REG_ENV_SHAPE     13
#define AY_REG_IOA           14
#define AY_REG_IOB           15

/* Channel identifiers */
typedef enum
{
    AY_CH_A = 0,
    AY_CH_B = 1,
    AY_CH_C = 2
} ay_channel_t;

/* Envelope shapes */
typedef enum
{
    AY_ENV_HOLD        = 0x00,
    AY_ENV_ATTACK      = 0x04,
    AY_ENV_ALTERNATE   = 0x02,
    AY_ENV_CONTINUE    = 0x08
} ay_env_flags_t;

typedef enum
{
    AY_NOTE_C = 0,
    AY_NOTE_CS,
    AY_NOTE_D,
    AY_NOTE_DS,
    AY_NOTE_E,
    AY_NOTE_F,
    AY_NOTE_FS,
    AY_NOTE_G,
    AY_NOTE_GS,
    AY_NOTE_A,
    AY_NOTE_AS,
    AY_NOTE_B
} ay_note_t;

/* Public API */

void ay_init(void);

void ay_write(uint8_t reg, uint8_t value);
uint8_t ay_read(uint8_t reg);

void ay_set_tone(ay_channel_t ch, uint16_t period);
void ay_set_volume(ay_channel_t ch, uint8_t volume);

void ay_enable_tone(ay_channel_t ch, uint8_t enable);
void ay_enable_noise(ay_channel_t ch, uint8_t enable);

void ay_set_noise(uint8_t period);

void ay_set_envelope(uint16_t period, uint8_t shape);

void ay_set_portA(uint8_t value);
uint8_t ay_get_portA(void);

void ay_set_portB(uint8_t value);
uint8_t ay_get_portB(void);

uint16_t ay_freq_to_period(uint32_t clock, float freq);

void ay_play_note(ay_channel_t ch, ay_note_t note, int octave, uint8_t volume);

#endif
