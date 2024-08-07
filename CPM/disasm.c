/*
 * Simple 8080 disasssembler. Reads and disassembles a binary (COM)
 * file passed on the command line.
 *
 * Note that this will compile natively under CP/M using the z88dk C
 * compiler.
 *
 * Copyright 2023-2024 Jeff Tranter <tranter@pobox.com>
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef CPM
#include <conio.h>
#endif

/* Data structures */

/* Addressing modes */
enum {
    implied, rega, regb, regc, regd, rege, regh, regl, regm, regsp,
    regbb, regbc, regbd, regbe, regbh, regbl, regbm, regba, regcb, regcc,
    regcd, regce, regch, regcl, regcm, regca, regdb, regdc, regdd, regde,
    regdh, regdl, regdm, regda, regeb, regec, reged, regee, regeh, regel,
    regem, regea, reghb, reghc, reghd, reghe, reghh, reghl, reghm, regha,
    reglb, reglc, regld, regle, reglh, regll, reglm, regla, regmb, regmc,
    regmd, regme, regmh, regml, regma, regab, regac, regad, regae, regah,
    regal, regam, regaa, regpsw, imm, imma, immb, immc, immd, imme,
    immh, imml, immm, immxb, immxd, immxh, immxsp, direct, m0, m1,
    m2, m3, m4, m5, m6, m7
} addressingMode_t;

/* Mnemonic names. Matches entries in table of mnemonic strings. */
enum {
    invalid, aci, adc, add, adi, ana, ani, call, cc, cm,
    cma, cmc, cmp, cnc, cnz, cp, cpe, cpi, cpo, cz,
    daa, dad, dcr, dcx, di, ei, hlt, in, inr, inx,
    jc, jm, jmp, jnc, jnz, jp, jpe, jpo, jz, lda,
    ldax, lhld, lxi, mov, mvi, nop, ora, ori, out, pchl,
    pop, push, ral, rar, rc, ret, rlc, rm, rnc, rnz,
    rp, rpe, rpo, rrc, rst, rz, sbb, sbi, shld, sphl,
    sta, stax, stc, sub, sui, xchg, xra, xri, xthl
} mnemonic_t;

/* Table of format strings, indexed by addressing mode */
char *formatString[96] = {
    "", "A", "B", "C", "D", "E", "H", "L", "M", "SP",
    "B,B", "B,C", "B,D", "B,E", "B,H", "B,L", "B,M", "B,A","C,B", "C,C",
    "C,D", "C,E", "C,H", "C,L", "C,M", "C,A", "D,B", "D,C", "D,D", "D,E",
    "D,H", "D,L", "D,M", "D,A", "E,B", "E,C", "E,D", "E,E", "E,H", "E,L",
    "E,M", "E,A", "H,B", "H,C", "H,D", "H,E", "H,H", "H,L", "H,M", "H,A",
    "L,B", "L,C", "L,D", "L,E", "L,H", "L,L", "L,M", "L,A", "M,B", "M,C",
    "M,D", "M,E", "M,H", "M,L", "M,A", "A,B", "A,C", "A,D", "A,E",  "A,H",
    "A,L", "A,M", "A,A", "PSW", "0%02XH", "A,0%02XH", "B,0%02XH", "C,0%02XH",
    "D,0%02XH", "E,0%02XH", "H,0%02XH", "L,0%02XH", "M,0%02XH", "B,0%02X%02XH",
    "D,0%02X%02XH", "H,0%02X%02XH", "SP,0%02X%02XH", "0%02X%02XH", "0", "1",
    "2", "3", "4", "5", "6", "7"
};

/* Table of instruction lengths, indexed by addressing mode */
int instructionLength[96] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 3, 3, 3, 3, 3, 1, 1,
    1, 1, 1, 1, 1, 1
};

/* Table of mnemonic strings. */
char *mnemonicString[] = {
    "???",  "ACI",  "ADC",  "ADD",  "ADI",  "ANA",  "ANI",  "CALL", "CC",   "CM",
    "CMA",  "CMC",  "CMP",  "CNC",  "CNZ",  "CP",   "CPE",  "CPI",  "CPO",  "CZ",
    "DAA",  "DAD",  "DCR",  "DCX",  "DI",   "EI",   "HLT",  "IN",   "INR",  "INX",
    "JC",   "JM",   "JMP",  "JNC",  "JNZ",  "JP",   "JPE",  "JPO",  "JZ",   "LDA",
    "LDAX", "LHLD", "LXI",  "MOV",  "MVI",  "NOP",  "ORA",  "ORI",  "OUT",  "PCHL",
    "POP",  "PUSH", "RAL",  "RAR",  "RC",   "RET",  "RLC",  "RM",   "RNC",  "RNZ",
    "RP",   "RPE",  "RPO",  "RRC",  "RST",  "RZ",   "SBB",  "SBI",  "SHLD", "SPHL",
    "STA",  "STAX", "STC",  "SUB",  "SUI",  "XCHG", "XRA",  "XRI",  "XTHL"
};

/* Table of mnemonics, indexed by op code. */
int mnemonic[256] = {
    /* 00 */
    nop, lxi, stax, inx, inr, dcr, mvi, rlc,
    invalid, dad, ldax, dcx, inr, dcr, mvi, rrc,
    /* 10 */
    invalid, lxi, stax, inx, inr, dcr, mvi, ral,
    implied, dad, ldax, dcx, inr, dcr, mvi, rar,
    /* 20 */
    invalid, lxi, shld, inx, inr, dcr, mvi, daa,
    invalid, dad, lhld, dcx, inr, dcr, mvi, cma,
    /* 30 */
    invalid, lxi, sta, inx, inr, dcr, mvi, stc,
    invalid, dad, lda, dcx, inr, dcr, mvi, cmc,
    /* 40 */
    mov, mov, mov, mov, mov, mov, mov, mov,
    mov, mov, mov, mov, mov, mov, mov, mov,
    /* 50 */
    mov, mov, mov, mov, mov, mov, mov, mov,
    mov, mov, mov, mov, mov, mov, mov, mov,
    /* 60 */
    mov, mov, mov, mov, mov, mov, mov, mov,
    mov, mov, mov, mov, mov, mov, mov, mov,
    /* 70 */
    mov, mov, mov, mov, mov, mov, hlt, mov,
    mov, mov, mov, mov, mov, mov, mov, mov,
    /* 80 */
    add, add, add, add, add, add, add, add,
    adc, adc, adc, adc, adc, adc, adc, adc,
    /* 90 */
    sub, sub, sub, sub, sub, sub, sub, sub,
    sbb, sbb, sbb, sbb, sbb, sbb, sbb, sbb,
    /* A0 */
    ana, ana, ana, ana, ana, ana, ana, ana,
    xra, xra, xra, xra, xra, xra, xra, xra,
    /* B0 */
    ora, ora, ora, ora, ora, ora, ora, ora,
    cmp, cmp, cmp, cmp, cmp, cmp, cmp, cmp,
    /* C0 */
    rnz, pop, jnz, jmp, cnz, push, adi, rst,
    rz, ret, jz, invalid, cz, call, aci, rst,
    /* D0 */
    rnc, pop, jnc, out, cnc, push, sui, rst,
    rc, invalid, jc, in, cc, invalid, sbi, rst,
    /* E0 */
    rpo, pop, jpo, xthl, cpo, push, ani, rst,
    rpe, pchl, jpe, xchg, cpe, invalid, xri, rst,
    /* F0 */
    rp, pop, jp, di, cp, push, ori, rst,
    rm, sphl, jm, ei, cm, invalid, cpi, rst
};

/* Table of addressing modes, indexed by op code. */
int addressMode[256]  = {
    /* 00 */
    implied, immxb, regb, regb, regb, regb, immb, implied,
    implied, regb, regb, regb, regc, regc, immc, implied,
    /* 10 */
    implied, immxd, regd, regd, regd, regd, immd, implied,
    implied, regd, regd, regd, rege, rege, imme, implied,
    /* 20 */
    implied, immxh, direct, regh, regh, regh, immh, implied,
    implied, regh, direct, regh, regl, regl, imml, implied,
    /* 30 */
    implied, immxsp, direct, regsp, regm, regm, immm, implied,
    implied, regsp, direct, regsp, rega, rega, imma, implied,
    /* 40 */
    regbb, regbc, regbd, regbe, regbh, regbl, regbm, regba,
    regcb, regcc, regcd, regce, regch, regcl, regcm, regca,
    /* 50 */
    regdb, regdc, regdd, regde, regdh, regdl, regdm, regda,
    regeb, regec, reged, regee, regeh, regel, regem, regea,
    /* 60 */
    reghb, reghc, reghd, reghe, reghh, reghl, reghm, regha,
    reglb, reglc, regld, regle, reglh, regll, reglm, regla,
    /* 70 */
    regmb, regmc, regmd, regme, regmh, regml, implied, regma,
    regab, regac, regad, regae, regah, regal, regam, regaa,
    /* 80 */
    regb, regc, regd, rege, regh, regl, regm, rega,
    regb, regc, regd, rege, regh, regl, regm, rega,
    /* 90 */
    regb, regc, regd, rege, regh, regl, regm, rega,
    regb, regc, regd, rege, regh, regl, regm, rega,
    /* A0 */
    regb, regc, regd, rege, regh, regl, regm, rega,
    regb, regc, regd, rege, regh, regl, regm, rega,
    /* B0 */
    regb, regc, regd, rege, regh, regl, regm, rega,
    regb, regc, regd, rege, regh, regl, regm, rega,
    /* C0 */
    implied, regb, direct, direct, direct, regb, imm, m0,
    implied, implied, direct, invalid, direct, direct, imm, m1,
    /* D0 */
    implied, regd, direct, imm, direct, regd, imm, m2,
    implied, invalid, direct, imm, direct, invalid, imm, m3,
    /* E0 */
    implied, regh, direct, implied, direct, regh, imm, m4,
    implied, implied, direct, implied, direct, invalid, imm, m5,
    /* F0 */
    implied, regpsw, direct, implied, direct, regpsw, imm, m6,
    implied, implied, direct, implied, direct, implied, imm, m7
};

int main(int argc, char *argv[])
{
    char *filename;
    FILE *f;
    unsigned int address = 0x100;
    unsigned int end = 0xffff;
    int i, len;
    int sourceMode = 0;
    int op, am, mnem, op1, op2;
    int lines = 0;
    char c;

    if (argc < 2 || argc > 5) {
        printf("Usage: disasm [-a<start address>] [-e<end address> [-s] <filename>\n");
        return 1;
    }

    /* Parse any command line options. */
    for (i = 1; i < argc; i++) {
        if (!strcasecmp(argv[i], "-s")) {
            sourceMode = 1;
        } else if (argv[i][0] == '-' && (argv[i][1] == 'a' || argv[i][1] == 'A')) {
            address = strtol(argv[i]+2, NULL, 16);
        } else if (argv[i][0] == '-' && (argv[i][1] == 'e' || argv[i][1] == 'E')) {
            end = strtol(argv[i]+2, NULL, 16);
        }
    }

    filename = argv[argc - 1];
    f = fopen(filename, "rb");
    if (f == NULL) {
        printf("Error: unable to open '%s'\n", filename);
        return 1;
    }

    if (sourceMode) {
        printf("        ORG     0%04XH\n", address);
    }

    while ((op = getc(f)) != EOF) {
        mnem = mnemonic[op];
        am = addressMode[op];
        len = instructionLength[am];

        if (feof(f))
            break;

        if (address > end)
            break;

        switch (len) {
        case 1:
            if (sourceMode) {
                if (mnem == invalid) {
                    printf("        DB      0%02XH\n", op);
                } else {
                    printf("        %-4s    %s\n", mnemonicString[mnem], formatString[am]);
                }
            } else {
                printf("%04X  %02X        %-4s  %s\n", address, op, mnemonicString[mnem], formatString[am]);
            }
            break;
        case 2:
            op1 = getc(f);
            if (feof(f))
                break;
            if (sourceMode) {
                printf("        %-4s    ", mnemonicString[mnem]);
            } else {
                printf("%04X  %02X %02X     %-4s  ", address, op, op1, mnemonicString[mnem]);
            }
            printf(formatString[am], op1);
            printf("\n");
            break;
        case 3:
            op1 = getc(f);
            if (feof(f))
                break;
            op2 = getc(f);
            if (feof(f))
                break;
            if (sourceMode) {
                printf("        %-4s    ", mnemonicString[mnem]);
            } else {
                printf("%04X  %02X %02X %02X  %-4s  ", address, op, op1, op2, mnemonicString[mnem]);
            }
            printf(formatString[am], op2, op1);
            printf("\n");
            break;
        }
        address += len;

        lines++;
        if (lines >= 23) {
            printf("Press <space> to continue, Q to quit ");
#ifdef CPM
            while (!kbhit())
                ;
            c = getch();
            printf("\n");
#else
            c = getchar();
#endif
            if (c == 'q' || c == 'Q')
                break;
            lines = 0;
        }
    }

    if (sourceMode) {
        printf("        END\n");
    }

    fclose (f);
    return 0;
}
