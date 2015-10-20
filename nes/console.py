import logging
import struct
import time
logger = logging.getLogger("nes.console")

from pygame import Color
import apu
import cpu
import memory
import ppu
from .util import enum

buttons = enum([
    "a",
    "b",
    "select",
    "start",
    "up",
    "down",
    "left"
    "right",
])

palette = list(Color("0x%06xff" % (c,)) for c in (
        0x666666, 0x002a88, 0x1412a7, 0x3b00a4,
        0x5c007e, 0x6e0040, 0x6c0600, 0x561d00,
        0x333500, 0x0b4800, 0x005200, 0x004f08,
        0x00404d, 0x000000, 0x000000, 0x000000,
        0xadadad, 0x155fd9, 0x4240ff, 0x7527fe,
        0xa01acc, 0xb71e7b, 0xb53120, 0x994e00,
        0x6b6d00, 0x388700, 0x0c9300, 0x008f32,
        0x007c8d, 0x000000, 0x000000, 0x000000,
        0xfffeff, 0x64b0ff, 0x9290ff, 0xc676ff,
        0xf36aff, 0xfe6ecc, 0xfe8170, 0xea9e22,
        0xbcbe00, 0x88d800, 0x5ce430, 0x45e082,
        0x48cdde, 0x4f4f4f, 0x000000, 0x000000,
        0xfffeff, 0xc0dfff, 0xd3d2ff, 0xe8c8ff,
        0xfbc2ff, 0xfec4ea, 0xfeccc5, 0xf7d8a5,
        0xe4e594, 0xcfef96, 0xbdf4ab, 0xb3f3cc,
        0xb5ebf2, 0xb8b8b8, 0x000000, 0x000000,
))

MAGIC_INES = 0x4e45531a

# iNES File Header (16 bytes)
# -- magic number (4 bytes)
# -- number of PRG-ROM 16kB banks
# -- number of CHR-ROM 8kB banks
# -- control bits (2 bytes)
# -- PRG-RAM size in 8kB blocks (ignored here)
# -- other ignored data and padding (7 bytes)
iNESFileHeader = struct.Struct(">IBBHx7x")


class Cartridge(object):

    def __init__(self, prg, chr, mapper, mirror, battery):
        self.prg = prg
        self.chr = chr
        self.mapper = mapper
        self.mirror = mirror
        self.battery = battery
        self.sram = bytearray(8 << 10)
        logger.debug("loaded rom")
        logger.debug("prg size: %d", len(self.prg))
        logger.debug("chr size: %d", len(self.chr))

    @classmethod
    def fromfile(cls, path):
        """Read an iNES file (.nes) and return a Cartridge.

        http://wiki.nesdev.com/w/index.php/INES

        """
        with open(path, "rb") as f:
            header = f.read(iNESFileHeader.size)
            magic, numprg, numchr, ctrl = iNESFileHeader.unpack(header)
            if magic != MAGIC_INES:
                raise ValueError("invalid .nes file")

            mapper = (ctrl & 0xf0) | ((ctrl >> 12) & 0x0f)
            mirror = ((ctrl & 0x100) >> 8) | ((ctrl & 0x800) >> 10)
            battery = bool(ctrl & 0x200)

            if ctrl & 0x400:
                trainer = bytearray(f.read(512))

            prg = bytearray(f.read(numprg * 16 << 10))
            chr = bytearray(f.read(numchr * 8 << 10) if numchr else 8 << 10)

            return cls(prg, chr, mapper, mirror, battery)


class Controller(object):

    buttons = list(False for _ in buttons)
    index = 0
    strobe = 0

    def _strobe_index(self):
        if self.strobe & 1:
            self.index = 0

    def read(self):
        v = int(self.index < 8 and self.buttons[self.index])
        self.index += 1
        self._strobe_index()
        if self.strobe & 1:
            self.index = 0
        return v

    def write(self, v):
        self.strobe = v & 0xff
        self._strobe_index()


class Console(object):

    def __init__(self):
        self.ram = bytearray(2048)
        self.controllers = (Controller(), Controller())

    @classmethod
    def load(cls, cartridge):
        self = cls() if isinstance(cls, type) else cls
        self.cartridge = cartridge
        self.mapper = memory.Mapper.fromcart(self.cartridge)
        self.cpu = cpu.CPU(self)
        self.apu = apu.APU(self)
        self.ppu = ppu.PPU(self)
        return self

    @property
    def background_color(self):
        return palette[self.ppu.read_palette(0) % 0x40]

    @property
    def buffer(self):
        return self.ppu.front

    def reset(self):
        self.cpu.reset()

    def step(self):
        pstep = self.ppu.step
        mstep = self.mapper.step
        astep = self.apu.step
        cycles = self.cpu.step()
        for _ in xrange(cycles):
            pstep()
            mstep()
            pstep()
            mstep()
            pstep()
            mstep()
            astep()
        return cycles

    def step_frame(self):
        frame = self.ppu.frame
        frame_steps = iter((lambda: self.ppu.frame == frame), False)
        return sum(self.cpu.step() for _ in frame_steps)

    def step_seconds(self, seconds):
        cycles = cpu.CPUFREQ * seconds
        #last_time = start_time = time.time()
        while cycles > 0:
            cycles -= self.step()
            #this_time = time.time()
            #if this_time - last_time > 0.2:
            #    logger.debug("cycles: %s", cycles)
            #    logger.debug("elapsed: %s", this_time - start_time)
            #    last_time = this_time
