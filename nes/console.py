import logging
logger = logging.getLogger("nes.console")

from pygame import Color
from .apu import APU
from .cpu import CPU
from .ppu import PPU
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

    cartridge = None
    mapper = None

    def __init__(self):
        self.ram = bytearray(2048)
        self.controllers = (Controller(), Controller())
        self.apu = APU(self)
        self.cpu = CPU()
        self.ppu = PPU(self)

    @classmethod
    def load(cls, cartridge):
        self = cls() if isinstance(cls, type) else cls
        self.cartrige = cartridge

    @property
    def background_color(self):
        return palette[self.ppu.read_palette(0) % 0x40]

    @property
    def buffer(self):
        return self.ppu.front

    @property
    def mapper(self):
        return self.cartridge.mapper

    def reset(self):
        self.cpu.reset()

    def step(self):
        cycles = self.cpu.step()
        for _ in xrange(cycles * 3):
            self.ppu.step()
            self.mapper.step()
        for _ in xrange(cycles):
            self.apu.step()
        return cycles

    def step_frame(self):
        frame = self.ppu.frame
        frame_steps = iter((lambda: self.ppu.frame == frame), False)
        return sum(self.cpu.step() for _ in frame_steps)

    def step_seconds(self, seconds):
        cycles = CPUFREQ * seconds
        while cycles > 0:
            cycles -= self.step()
