from .controller import Controller
from .apu import APU
from .cpu import CPU
from .ppu import PPU

class Console(object):

    cartridge = None
    mapper = None

    def __init__(self):
        self.ram = bytearray(2048)
        self.controllers = (Controller(), Controller())
        self.apu = APU(self)
        self.cpu = CPU(self)
        self.ppu = PPU(self)

    @classmethod
    def load(cls, cartridge):
        self = cls() if isinstance(cls, type) else cls
        self.cartrige = cartridge
        self.mapper = Mapper(self.cartridge)

    @property
    def buffer(self):
        return self.ppu.front

    @property
    def background_color(self):
        return palette[self.ppu.read_palette(0) % 0x40]

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
