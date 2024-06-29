#! /usr/bin/env python3
#
# bin2abs: Binary to HDOS executable file converter.
#
# Converts a pure binary binary file, such as that produced by an
# assembler, to the ABS file format used for HDOS executables.
#
# usage: bin2abs [-a <load address>] [-s <start address>] <infile> <outfile>
#
# Load address defaults to USERFWA: 0x2280 or 0o21200.
# Start address defaults to load address.
# Addresses can be in decimal, octal, or hex.
#
# Copyright (c) 2024 by Jeff Tranter <tranter@pobox.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import sys
import argparse
import signal


def auto_int(x):
    return int(x, 0)


# Avoids an error when output piped, e.g. to "less"
signal.signal(signal.SIGPIPE, signal.SIG_DFL)


# Parse command line options
parser = argparse.ArgumentParser()
parser.add_argument("infilename", help="Binary input file to read")
parser.add_argument("outfilename", help="ABS output file to write")
parser.add_argument("-a", "--load-address", help="Specify load address (defaults to 0x2280)", default=0x2280, type=auto_int)
parser.add_argument("-s", "--start-address", help="Specify starting address (defaults to load address)", default=-1, type=auto_int)

args = parser.parse_args()

# Get command line arguments.
infile = args.infilename
outfile = args.outfilename
start_address = args.start_address
load_address = args.load_address

if start_address == -1:
    start_address = load_address

# Enforce addresses  to be in valid range.
start_address = start_address & 0xffff
load_address = load_address & 0xffff

# Open input file for reading.
# Display error and exit if filename does not exist.
try:
    fin = open(infile, "rb")
except FileNotFoundError:
    print(("error: input file '{}' not found.".format(infile)), file=sys.stderr)
    sys.exit(1)

# Open output file for writing
# Display error and exit if filename can not be opened or created.
try:
    fout = open(outfile, "wb")
except (PermissionError, IsADirectoryError):
    print(("error: could not open output file '{}'".format(outfile)), file=sys.stderr)
    sys.exit(1)

# Get input file size
size = os.stat(infile).st_size

# Output header
fout.write(b'\xff')  # ID byte = FFH
fout.write(b'\0')  # FT byte = 00H
fout.write(load_address.to_bytes(2, 'little'))  # Load address (low byte, high byte)
fout.write(size.to_bytes(2, 'little'))  # Program length (low byte, high byte)
fout.write(start_address.to_bytes(2, 'little'))  # Start address (low byte, high byte)

# Now output contents of the input file.
while True:
    try:
        b = fin.read(1)  # Get binary byte from file
        if not b:  # handle EOF
            break
        fout.write(b)    # Write to output file

    except KeyboardInterrupt:
        print("Interrupted by Control-C", file=sys.stderr)
        break

# Finally, pad the file with zeroes out to the next 256 byte boundary.
pad = 256 - (size + 8) % 256
for p in range(0, pad):
    fout.write(b'\0')
