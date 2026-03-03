/* Example of reading status of joysticks.

I/O port bits for each joystick:
+---------+---+---+---+---+---+---+---+---+
|     Bit:| 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
+---------+---+---+---+---+---+---+---+---+
|Function:| 3 | 2 | 1 | X | R | L | D | U |
+---------+---+---+---+---+---+---+---+---+

X = Unused
R = Right
L = Left
D = Down
U = Up
1 = Fire 1 (not on original board)
2 = Fire 2 (not on original board)
3 = Fire 3 (not on original board)

1 = not pressed, 0 = pressed
Joystick 1 is register 14
Joystick 2 is register 15
  
 */

#include <stdio.h>
#include <stdlib.h>

#define AY_REG_PORT   0xA0
#define AY_DATA_PORT  0xA1

#define AY_REG_PORTA   14
#define AY_REG_PORTB   15
#define AY_REG_ENABLE   7

/* Write to AY register */
void ay_write_reg(unsigned int reg, unsigned int value)
{
    outp(AY_REG_PORT, reg);
    outp(AY_DATA_PORT, value);
}

unsigned int ay_read_reg(unsigned int reg)
{
    outp(AY_REG_PORT, reg);
    return inp(AY_REG_PORT);
}

/* Configure ports as input (bits 6 and 7 = 0) */
void ay_ports_input(void)
{
    unsigned int enable;

    /* Read current enable register */
    enable = ay_read_reg(AY_REG_ENABLE);

    /* Clear bits 6 and 7 (Port A and B input) */
    enable &= ~0xC0;

    ay_write_reg(AY_REG_ENABLE, enable);
}

int main(void)
{
    unsigned int porta, portb;

    printf("Joystick demo: Press Fire 1 and Fire 2 to quit.\n\n");
    
    while (1) {
        porta = ay_read_reg(AY_REG_PORTA);
        portb = ay_read_reg(AY_REG_PORTB);

        if (!(porta & 0x80))
            printf("Joystick 1: Fire 3\n");
        if (!(porta & 0x40))
            printf("Joystick 1: Fire 2\n");
        if (!(porta & 0x20))
            printf("Joystick 1: Fire 1\n");
        if (!(porta & 0x08))
            printf("Joystick 1: Right\n");
        if (!(porta & 0x04))
            printf("Joystick 1: Left\n");
        if (!(porta & 0x02))
            printf("Joystick 1: Down\n");
        if (!(porta & 0x01))
            printf("Joystick 1: Up\n");

        if (!(portb & 0x80))
            printf("Joystick 1: Fire 3\n");
        if (!(portb & 0x40))
            printf("Joystick 1: Fire 2\n");
        if (!(portb & 0x20))
            printf("Joystick 1: Fire 1\n");
        if (!(portb & 0x08))
            printf("Joystick 1: Right\n");
        if (!(portb & 0x04))
            printf("Joystick 1: Left\n");
        if (!(portb & 0x02))
            printf("Joystick 1: Down\n");
        if (!(portb & 0x01))
            printf("Joystick 1: Up\n");

        if (porta == 0x9f || portb == 0x9f)
            break;
    }
    
    return 0;
}
