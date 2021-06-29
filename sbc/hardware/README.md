This is a Z80 single board computer designed base on Grant Searle's
Minimal Chip Count Z80 Computer (32K version). A few change were made:

- Uses an FTDI USB to serial converter rather than full RS-232 port.
- Full schematic including bypass and filter caps and unused gates.
- Powered by USB (if jumper connected).
- 36 pin expansion header connector.
- Power on LED.
- PCB layout was designed.

For the EasyEDA design, see https://oshwlab.com/tranter/z80-single-board-computer

Assembly Notes:

- Recommend using sockets for all ICs.
- Observe correct polarity when installing capacitor C7 and LED D1.
- EPROM/EEPROM must be programmed with firmware.
- Connect jumper at J1 if powered from USB.
- Connect FTDI USB/serial adaptor at H1.
- Connect FTDI to USB port on laptop/desktop computer and run terminal
  emulator set to 115200bps 8N1.
- Pressing reset button should show boot message.

In Case of Problems:

- Check for 5V power coming from FTDI.
- Check for 7.3728 MHz clock signal at CPU CLK pin.
- Check that /RESET goes low when button pressed.
- Ensure all ICs have proper orientation and no bent pins.
- Check for soldering opens or shorts.
