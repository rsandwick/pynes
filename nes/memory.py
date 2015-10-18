import logging
logger = logging.getLogger("nes.memory")

from .util import enum

Mirror = enum([
    "HORIZONTAL",
    "VERTICAL",
    "SINGLE0",
    "SINGLE1",
    "FOUR",
])

def mirror_address(mode, addr):
    addr &= 0xffff
    table = addr / 0x0400
    offset = addr & 0x03ff
    page = ((0, 0, 1, 1),
            (0, 1, 0, 1),
            (0, 0, 0, 0),
            (1, 1, 1, 1),
            (0, 1, 2, 3))[mode][table] * 0x0400
    return 0x2000 + page + offset


class Memory(object):
    def __init__(self, console):
        self._console = console


class CPUMemory(Memory):

    def read(self, addr):
        addr &= 0xffff
        if addr < 0x2000:
            return self._console.ram[addr % 0x0800]
        elif addr < 0x4000:
            return self._console.ppu.read_register(0x2000 + (addr & 7))
        elif addr == 0x4014:
            return self._console.ppu.read_register(addr)
        elif addr == 0x4015:
            return self._console.apu.read_register(addr)
        elif addr == 0x4016:
            return self._console.controllers[0].read()
        elif addr == 0x4017:
            return self._console.controllers[1].read()
        elif addr < 0x6000:
            #TODO: i/o registers
            pass
        elif addr >= 0x6000:
            return self._console.mapper.read(addr)
        else:
            raise MemoryError(
                "unhandled cpu memory read at address: 0x%04x" % (addr,))

    def write(self, addr, v):
        addr &= 0xffff
        v &= 0xff
        if addr < 0x2000:
            self._console.ram[addr % 0x0800]
        elif addr < 0x4000:
            self._console.ppu.write_register(0x2000 + (addr & 7), v)
        elif addr < 0x4014:
            self._console.apu.write_register(addr, v)
        elif addr == 0x4014:
            self._console.ppu.write_register(addr, v)
        elif addr == 0x4015:
            self._console.apu.write_register(addr, v)
        elif addr == 0x4016:
            self._console.controllers[0].write(v)
        elif addr == 0x4017:
            self._console.controllers[1].write(v)
        elif addr < 0x6000:
            #TODO: i/o registers
            pass
        elif addr >= 0x6000:
            self._console.mapper.write(addr, v)
        else:
            raise MemoryError(
                "unhandled cpu memory write at address: 0x%04x" % (addr,))


class PPUMemory(Memory):

    def read(self, addr):
        addr &= 0xffff
        if addr < 0x2000:
            return self._console.mapper.read(addr)
        elif addr < 0x3f00:
            mode = self._console.cartridge.mirror
            addr = mirror_address(mode, addr) & 0x07ff
            return self._console.ppu.name_table_data[addr]
        elif addr < 0x4000:
            return self._console.ppu.read_palette(addr & 0x1f)
        else:
            raise MemoryError(
                "unhandled ppu memory read at address: 0x%04x" % (addr,))

    def write(self, addr, v):
        addr &= 0xffff
        v &= 0xff
        if addr < 0x2000:
            self._console.mapper.write(addr, v)
        elif addr < 0x3f00:
            mode = self._console.cartridge.mirror
            addr = mirror_address(mode, addr) & 0x07ff
            self._console.ppu.name_table_data[addr] = v
        elif addr < 0x4000:
            self._console.ppu.write_palette(addr & 0x1f, v)
        else:
            raise MemoryError(
                "unhandled ppu memory write at address: 0x%04x" % (addr,))
