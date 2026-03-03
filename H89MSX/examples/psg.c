/*

AY-3-8910 Register Demo

*/

#include <stdio.h>
#include <stdlib.h>

#define AY_REG_PORT   0xA0
#define AY_DATA_PORT  0xA1

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

void commands()
{
    printf("\nAY-3-8910 Register Demo\n");
    printf("-----------------------\n");
    printf("Select Function:\n");
    printf("1. Read Register.\n");
    printf("2. Write Register.\n");
    printf("3. Display all Registers.\n");
    printf("4. Display Register Reference.\n");
    printf("5. Quit.\n");
    printf("Function: ");
}

void readRegister()
{
    int n;

    printf("Read Register:\n");
    printf("Register number (in hex, 00 to FF): ");
    scanf("%02X", &n);

    printf("Register %02X: %02X\n", n, ay_read_reg(n));
}

void writeRegister()
{
    int n, d;
    
    printf("Write Register:\n");
    printf("Register number (in hex, 00 to FF): ");
    scanf("%02X", &n);
    printf("Value to write (in hex, 00 to FF): ");
    scanf("%02X", &d);
    ay_write_reg(n, d);
    printf("Wrote %02X to register %02X\n", d, n);
}

void displayRegisters()
{
    int i;

    printf("Display all Registers:\n");
    printf("Register:");

    for (i = 0; i <= 15; i++) {       
        printf(" %02X", i);
    }

    printf("\nValue:   ");
    for (i = 0; i <= 15; i++) {       
        printf(" %02X", ay_read_reg(i));
    }

    printf("\n");
}

void registerReference()
{
    printf("Register Reference:\n");
    printf("00 Channel A Tone Period (fine)   B7 B6 B5 B4 B3 B2 B1 B0\n");
    printf("01 Channel A Tone Period (coarse) XX XX XX XX B3 B2 B1 B0\n");
    printf("02 Channel B Tone Period (fine)   B7 B6 B5 B4 B3 B2 B1 B0\n");
    printf("03 Channel B Tone Period (coarse) XX XX XX XX B3 B2 B1 B0\n");
    printf("04 Channel C Tone Period (fine)   B7 B6 B5 B4 B3 B2 B1 B0\n");
    printf("05 Channel D Tone Period (coarse) XX XX XX XX B3 B2 B1 B0\n");
    printf("06 Noise Period                   XX XX XX B4 B3 B2 B1 B0\n");
    printf("07 Enable (active low)            IB IA NC NB NA TV TB TA\n");
    printf("08 Channel A Amplitude            XX XX XX M  L3 L2 L1 L0\n");
    printf("09 Channel B Amplitude            XX XX XX M  L3 L2 L1 L0\n");
    printf("10 Channel C Amplitude            XX XX XX M  L3 L2 L1 L0\n");
    printf("11 Envelope Period (fine)         B7 B6 B5 B4 B3 B2 B1 B0\n");
    printf("12 Envelope Period (coarse)       B7 B6 B5 B4 B3 B2 B1 B0\n");
    printf("13 Envelope Shape/Cycle           XX XX XX XX CO AT AL HL\n");
    printf("15 I/O Port A Data                B7 B6 B5 B4 B3 B2 B1 B0\n");
    printf("15 I/O Port B Data                B7 B6 B5 B4 B3 B2 B1 B0\n");               
}

int main()
{
    int cmd;

    while (1) {
        commands();

        scanf("%d", &cmd);

        switch (cmd) {
        case 1:
            readRegister();
            break;
        case 2:
            writeRegister();
            break;
        case 3:
            displayRegisters();
            break;
        case 4:
            registerReference();
            break;
        case 5:
            printf("Quitting\n");
            return 0;
        default:
            printf("Invalid command\n");
        } 

    }

    return 0;
}
