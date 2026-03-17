#include "ay8910.h"

void delay(void)
{
    for (volatile long i = 0; i < 50000; i++);
}


int main(void)
{
    ay_init();

    ay_set_tone(AY_CH_A, 500);
    ay_set_volume(AY_CH_A, 15);
    delay();
    
    ay_enable_tone(AY_CH_A, 1);
    ay_enable_noise(AY_CH_A, 0);
    delay();

    ay_play_note(AY_CH_A, AY_NOTE_C, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_D, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_E, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_F, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_G, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_A, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_B, 4, 15);
    delay();
    ay_play_note(AY_CH_A, AY_NOTE_C, 5, 15);
    delay();
}
