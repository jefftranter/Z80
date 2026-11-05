#!/usr/bin/env python3
"""Create standalone, nonbootable HDOS 3 H8D data disks.

Supported nominal sizes: 100, 200, 400 and 1800 KB.

The 100/200/400 KB layouts follow INIT-formatted floppy references. The
1800 KB layout follows the working 7200-sector HDOS volume recovered from an
H89 CF card and successfully mounted through FujiNet. It uses 30 sectors per
allocation group (240 groups), not 32.

The output is a raw sequence of 256-byte logical sectors. Boot sectors are
zero-filled; these are data volumes, not bootable system disks.
"""
import argparse
from dataclasses import dataclass
from pathlib import Path

SECTOR_SIZE = 256

@dataclass(frozen=True)
class Format:
    sectors: int
    spg: int
    rgt: int
    grt: int
    directory: tuple[int, ...]
    flags: int
    active_blocks: int
    sys_block: int
    default_label: str

FORMATS = {
    100: Format(400, 2, 10, 148,
                (132,136,130,134,138,142,146,140,144), 0, 3, 1,
                'empty disk image:  1 side,  40 tracks'),
    200: Format(800, 4, 12, 280,
                (264,266,260,262,268,270,276,278,272,274), 2, 4, 3,
                'empty disk image:  1 side,  80 tracks'),
    400: Format(1600, 8, 16, 544,
                (528,530,532,534,520,522,524,526,536,538,540,542), 3, 4, 3,
                'empty disk image: 2 sides,  80 cylinders (160 tracks)'),
    # EXPERIMENTAL linear FujiNet volume, not an INIT-verified HDOS format.
    # Geometry recovered from a working H89 CF HDOS 3.02 volume:
    # 7200 sectors / 30 sectors per group = 240 allocation groups.
    # RGT is in group 2, DIRECT.SYS in group 79, and GRT in group 80.
    # DIRECT.SYS reserves one complete 30-sector group, represented by
    # fifteen 512-byte directory blocks at sectors 2370, 2372, ... 2398.
    1800: Format(7200, 30, 60, 2400,
                 tuple(range(2370, 2400, 2)), 0, 9, 1,
                 'HDOS 3.02 1800K DATA VOLUME'),
}

def put16(buf, offset, n):
    buf[offset:offset+2] = n.to_bytes(2, 'little')

def system_entry(stem, flags, first, last, count, field13=0, field15=0):
    entry = bytearray(23)
    entry[:8] = stem.encode('ascii').ljust(8, b'\0')
    entry[8:11] = b'SYS'
    entry[13] = field13
    entry[14] = flags
    entry[15] = field15
    entry[16:19] = bytes((first, last, count))
    return entry

def make_image(size=100, volume=None, label=None):
    fmt = FORMATS[size]
    if volume is None:
        volume = {100: 17, 200: 18, 400: 34, 1800: 35}[size]
    if not 1 <= volume <= 255:
        raise ValueError('volume must be 1..255')
    if label is None:
        label = fmt.default_label
    label_bytes = label.encode('ascii')
    if len(label_bytes) > 60:
        raise ValueError('label must be at most 60 ASCII bytes')

    # Match the INIT media fill convention, without copying any reference.
    disk = bytearray(b'SP' * (fmt.sectors * SECTOR_SIZE // 2))
    def write_sector(n, contents):
        if not (0 <= n < fmt.sectors and len(contents) == SECTOR_SIZE):
            raise ValueError('invalid sector write')
        disk[n*SECTOR_SIZE:(n+1)*SECTOR_SIZE] = contents

    for n in range(9):
        write_sector(n, bytes(SECTOR_SIZE))

    volume_sector = bytearray(SECTOR_SIZE)
    volume_sector[0] = volume
    put16(volume_sector, 3, fmt.directory[0])
    put16(volume_sector, 5, fmt.grt)
    volume_sector[7] = fmt.spg
    # The recovered CF volume uses the HDOS 3.0 label format marker.
    # Smaller floppy formats retain the INIT 2.x marker used previously.
    if size == 1800:
        volume_sector[8] = 1
        volume_sector[9] = 0x30
    else:
        volume_sector[8] = 0
        volume_sector[9] = 0x20
    put16(volume_sector, 10, fmt.rgt)
    put16(volume_sector, 12, fmt.sectors)
    put16(volume_sector, 14, SECTOR_SIZE)
    volume_sector[16] = fmt.flags
    volume_sector[17:77] = label_bytes.ljust(60, b' ')
    volume_sector[79] = 10
    write_sector(9, volume_sector)

    # RGT byte meanings observed in INIT images: 0 unavailable, 0xff
    # reserved, 1 normal; entries beyond the end of disk are 0xff.
    rgt = bytearray([1] * 256)
    rgt[0] = 0
    # Reserved boot/loader groups, as observed in INIT-created disks.
    if size == 100:
        rgt[1] = 0
        rgt[2:5] = b'\xff' * 3
    elif size == 200:
        rgt[1:3] = b'\xff' * 2
    elif size == 1800:
        # Exact prefix convention in the recovered CF volume's RGT.SYS:
        # groups 0 and 1 are zero; normal entries begin at group 2.
        rgt[1] = 0
    else:
        rgt[1] = 0xff
    total_groups = fmt.sectors // fmt.spg
    rgt[total_groups:] = b'\xff' * (256 - total_groups)
    write_sector(fmt.rgt, rgt)

    dir_groups = list(dict.fromkeys(n // fmt.spg for n in fmt.directory))
    grt_group = fmt.grt // fmt.spg
    rgt_group = fmt.rgt // fmt.spg
    reserved = set(range(rgt_group + 1)) | {grt_group} | set(dir_groups)
    free_groups = [n for n in range(1, total_groups) if n not in reserved]
    grt = bytearray([0xff] * 256)
    grt[0] = free_groups[0]
    grt[rgt_group] = 0
    grt[grt_group] = 0
    for a, b in zip(free_groups, free_groups[1:]):
        grt[a] = b
    grt[free_groups[-1]] = 0
    for a, b in zip(dir_groups, dir_groups[1:]):
        grt[a] = b
    grt[dir_groups[-1]] = 0
    # Group 1 is unallocated in the 100K image, and reserved in larger disks.
    if size == 100:
        grt[1] = 0
    write_sector(fmt.grt, grt)

    if size == 1800:
        # These directory-entry attribute bytes are taken from the working
        # CF volume's RGT.SYS, GRT.SYS and DIRECT.SYS entries.
        entries = (
            system_entry('RGT', 0xf2, rgt_group, rgt_group, 1, 0x03, 0xff),
            system_entry('GRT', 0xf2, grt_group, grt_group, 1, 0x02, 0xff),
            system_entry('DIRECT', 0xe2, dir_groups[0], dir_groups[-1],
                         fmt.spg, 0xff, 0xff),
        )
    else:
        entries = (
            system_entry('RGT', 0xf0, rgt_group, rgt_group, 1),
            system_entry('GRT', 0xf0, grt_group, grt_group, 1),
            system_entry('DIRECT', 0xe0, dir_groups[0], dir_groups[-1], fmt.spg),
        )
    for index, sector in enumerate(fmt.directory):
        block = bytearray(512)
        for slot in range(22):
            block[slot*23] = 0xff if index < fmt.active_blocks else 0xfe
        if index == fmt.sys_block:
            # The CF-derived 1800K directory has ordinary free-entry space
            # after the three system entries; the floppy layouts carry the
            # owner/comment marker used by the earlier generator.
            if size != 1800:
                block[21*23] = 0xfe
                block[21*23+1:21*23+23] = b"Steven Parker (/AIWZ/)"
            for slot, entry in zip((18, 19, 20), entries):
                block[slot*23:slot*23+23] = entry
        block[507] = 23
        put16(block, 508, sector)
        put16(block, 510, fmt.directory[index+1] if index+1 < len(fmt.directory) else 0)
        write_sector(sector, block[:256])
        write_sector(sector+1, block[256:])
    return bytes(disk)

def validate(data, size):
    fmt = FORMATS[size]
    assert len(data) == fmt.sectors * SECTOR_SIZE
    sector = lambda n: data[n*SECTOR_SIZE:(n+1)*SECTOR_SIZE]
    vol = sector(9)
    assert int.from_bytes(vol[3:5], 'little') == fmt.directory[0]
    assert int.from_bytes(vol[5:7], 'little') == fmt.grt
    assert int.from_bytes(vol[10:12], 'little') == fmt.rgt
    assert int.from_bytes(vol[12:14], 'little') == fmt.sectors
    assert vol[7] == fmt.spg
    for i, n in enumerate(fmt.directory):
        b = sector(n) + sector(n+1)
        assert int.from_bytes(b[508:510], 'little') == n
        assert int.from_bytes(b[510:512], 'little') == (fmt.directory[i+1] if i+1<len(fmt.directory) else 0)
    grt = sector(fmt.grt)
    groups = list(dict.fromkeys(n // fmt.spg for n in fmt.directory))
    for i, group in enumerate(groups):
        assert grt[group] == (groups[i+1] if i+1<len(groups) else 0)
    assert grt[fmt.grt//fmt.spg] == 0


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('output', type=Path)
    p.add_argument('--size', type=int, choices=sorted(FORMATS), default=100)
    p.add_argument('--volume', type=int, default=None)
    p.add_argument('--label', default=None)
    args = p.parse_args()
    data = make_image(args.size, args.volume, args.label)
    validate(data, args.size)
    args.output.write_bytes(data)
    print(f'Created {args.output}: {len(data)} bytes ({args.size} KiB-class, nonbootable)')
    if args.size == 1800:
        print('1800K geometry: 7200 sectors, 30 sectors/group, 240 groups')
        print('Note: the geometry is derived from a working CF volume; test the newly initialized image on your HDOS/FujiNet setup.')

if __name__ == '__main__':
    main()
