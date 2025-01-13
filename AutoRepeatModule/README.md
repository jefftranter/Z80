# Heathkit H89/H19 Automatic Key Repeat

## Summary

Automatic Key Repeat board for the Heathkit H19 terminal or H89
computer. Based on the circuit used in a commercial product sold by
Analytical Products in the 1980s. Adds automatic keyboard key repeat
as on more modern computers, avoiding the need to press the REPEAT
key. Does not require any modifications to the computer: plugs in
between the keyboard and ribbon cable to computer. The original manual
describes the assembly and operation.

## Introduction

The lack of automatic key repeat on the H89 and H19 is a little
annoying when you are used to this feature on modern computers. In the
1980s Analytical Products sold a kit for a little board that connected
between the keyboard and terminal logic board that added automatic key
repeat. I've never seen an original unit, but the manual has a
schematic and instructions that were enough to reproduce the board. I
designed a PCB and assembled it and it works very well. It goes inline
between the keyboard and ribbon cable so no modifications to the
computer are needed and it should work with the Heathkit H89, H89A,
H19, H19A and Zenith Z-19 and Z-89.

## Parts List

| Qty | Des    | Description                                                  |
| --- |------- | -----------                                                  |
| 1   | J1     | 2x17 PC104 connector SAMTEC ESQ-117-14-G-D or ESQ-117-39-L-F |
| 1   | U1     | CD4013 IC                                                    |
| 1   | U2     | CD4020 IC                                                    |
| 1   | Q1     | 2N3904 transistor                                            |
| 1   | C1     | 22uF 10V or higher tantalum  or electrolytic capacitor       |
| 4   | R1-R4  | 27K 1/4W resistor                                            |
| 16  | D1-D16 | 1N4148 diode                                                 |
| 1   | n/a    | 14-pin IC socket                                             |
| 1   | n/a    | 16-pin IC socket                                             |
| 1   | n/a    | PCB                                                          |
| 1   | Z      | 1K resistor (optional)                                       |
| 1   | Cx     | 1000 pF capacitor (optional)                                 |
| 1   | S      | Header for wires to defeat switch (optional)                 |

The connector is a Samtec PC104 connector ESQ-117-14-G-D (e.g Mouser
part no. 200-ESQ11714GD).
 
I ordered my PCBs from jlcpcb.com and they were very inexpensive and
of high quality.

## Construction

Assembly is straightforward and uses all through-hole components. All
parts are commonly available.

Recommended order of construction is from lowest profile components to
highest, i.e

1. Resistors.
2. Diodes.
3. IC sockets.
4. Transistor.
5. Capacitor.
6. Connector.
7. ICs.

Note the orientation of the diodes, capacitor C1, and the IC sockets
and ICs.

Parts Cx, Z, and header S are optional. Z is an optional 10K resistor
to add if operation is intermittent on an H89A or H19A. Cx is an
optional 1000pF capacitor to be installed if repeat is not desired for
the top row of function keys. Point S is for adding an external defeat
switch to turn off autorepeat. See the Analytical Products manual for
more details.

Note the orientation of the connector -- it goes on the non-component
side. Leave some space between the connector and the PCB. It is
recommended to install the board on the keyboard and then solder a few
of the pins when it is in place to get the alignment correct. Solder
on the component side and avoid getting solder on the pins where the
connector attaches. Note the orientation of the board and the ribbon
cable.

# Images

![Assembled board](image1.png)

![Side view showing connector](image2.png)

![Plugged into keyboard](image3.png)

![Ready to plug in ribbon cable](image4.png)

## Operation

When installed, keyboard keys will autorepeat after a short delay. The
REPEAT key can still be used if you want an immediate repeat function.

## Other

There is a third party replacement available for the keyboard encoder
IC used on the H19/H89. It implements autorepeat internally. It is
unknown if the autorepeat module will work this part, but in any case
it would serve no purpose.

The hardware design is Open Source Hardware, licensed under the The TAPR
Open Hardware License. You are welcome to build the circuit and use my
PCB layout.
See https://web.tapr.org/OHL/TAPR_Open_Hardware_License_v1.0.txt

Documentation written by me is licensed under a Creative Commons
Attribution 4.0 International License.
See https://creativecommons.org/licenses/by/4.0/

## References

1. https://oshwlab.com/tranter/h89-h19-automatic-key-repeat
2. https://easyeda.com/editor#id=|f389a216686142f587bb133ffbe48b7c|0499826e9c3442c9a9fe8f3af5bf9ad2
3. https://sebhc.github.io/sebhc/documentation/supplemental/AnaPro_AutoKeyRepeat.pdf
