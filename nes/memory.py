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
    addr = (addr - 0x2000) & 0x0fff
    table = addr >> 10
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
            return self._console.ram[addr & 0x07ff]
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


class Mapper(object):

    _bad_read = "unhandled %s read at address: 0x%04x"
    _bad_write = "unhandled %s write at address: 0x%04x"
    _mappers = {}

    def __init__(self, cartridge):
        self._cartridge = cartridge

    @classmethod
    def fromcart(cls, cartridge):
        k = cartridge.mapper
        try:
            return cls._mappers[k or 2](cartridge)
        except KeyError:
            msg = "unsupported mapper: %d"
            logger.error(msg, k)
            raise ValueError(msg % (k,))

    def step(self): pass


class Mapper1(Mapper):

    _control = 0
    _shift_register = 0x10

    _chr_bank0 = 0
    _chr_bank1 = 0
    _chr_mode = 0
    _chr_offsets = [0, 0]

    _prg_mode = 0
    _prg_bank = 0
    _prg_offsets = [0, 0]

    def __init__(self, cartridge):
        super(Mapper1, self).__init__(cartridge)
        self._prg_offsets[1] = self._prg_bank_offset(-1)

    def read(self, addr):
        addr &= 0xffff
        if addr < 0x2000:
            bank = self._chr_offsets[addr / 0x1000]
            offset = addr % 0x1000
            return self._cartridge.chr[bank + offset]
        elif addr >= 0x8000:
            addr -= 0x8000
            bank = self._prg_offsets[addr / 0x4000]
            offset = addr % 0x4000
            return self._cartridge.prg[bank + offset]
        elif addr >= 0x6000:
            return self._cartridge.sram[addr - 0x6000]
        else:
            raise MemoryError(self._bad_read % (type(self), addr))

    def write(self, addr, v):
        addr &= 0xffff
        v &= 0xff
        if addr < 0x2000:
            bank = self._chr_offsets[addr / 0x1000]
            offset = addr % 0x1000
            self._cartridge.chr[bank + offset] = v
        elif addr >= 0x8000:
            self._load_register(addr, v)
        elif addr >= 0x6000:
            self._cartridge._sram[addr - 0x6000] = v
        else:
            raise MemoryError(self._bad_write % (type(self), addr))

    def _load_register(self, addr, v):
        if v & 0x80:
            self._shift_register = 0x10
            self._write_control(self._control | 0x0c)
        else:
            complete = bool(self._shift_register & 1)
            self._shift_register >>= 1
            self._shift_register |= (v & 1) << 4
            if complete:
                self._write_register(addr, self._shift_register)
                self._shift_register = 0x10

    def _write_register(self, addr, v):
        if addr <= 0x9fff:
            self._write_control(v)
        elif addr <= 0xbfff:
            self._write_chr_bank0(v)
        elif addr <= 0xdfff:
            self._write_chr_bank1(v)
        elif addr <= 0xbfff:
            self._write_prg_bank(v)

    def _write_control(self, v):
        self._control = v
        self._chr_mode = (v >> 4) & 1
        self._prg_mode = (v >> 2) & 3
        mirror = v & 3
        self._cartridge.mirror = (
                Mirror.SINGLE0, Mirror.SINGLE1,
                Mirror.VERTICAL, Mirror.HORIZONTAL
                )[mirror]
        self._update_offsets()

    def _write_chr_bank0(self, v):
        self._chr_bank0 = v & 0xff
        self._update_offsets()

    def _write_chr_bank1(self, v):
        self._chr_bank1 = v & 0xff
        self._update_offsets()

    def _write_prg_bank(self, v):
        self._prg_bank = v & 0x0f
        self._update_offsets()

    def _chr_bank_offset(self, i):
        if i >= 0x80:
            i -= 0x100
        i %= len(self._cartridge.chr) / 0x1000
        offset = i * 0x1000
        if offset < 0:
            offset += len(self._cartridge.chr)
        return offset

    def _prg_bank_offset(self, i):
        if i >= 0x80:
            i -= 0x100
        i %= len(self._cartridge.prg) / 0x4000
        offset = i * 0x4000
        if offset < 0:
            offset += len(self._cartridge.prg)
        return offset

    def _update_offsets(self):
        """Update PRG and CHR offset values based on current modes.

        PRG ROM bank mode --
        0, 1 -- switch 32kB as $8000, ignore low bit of bank number
        2 -- fix first bank at $8000, switch 16kB bank at $C000
        3 -- fix last bank at $C000, switch 16kB bank at $8000

        CHR ROM bank mode --
        0 -- switch full 8kB bank
        1 -- switch two separate 4kB banks

        """
        bank, offset = self._prg_bank, self._prg_bank_offset
        if self._prg_mode in (0, 1):
            self._prg_offsets = [offset(bank & 0xfe), offset(bank | 1)]
        elif self._prg_mode == 2:
            self._prg_offsets = [0, offset(bank)]
        elif self._prg_mode == 3:
            self._prg_offsets = [offset(bank), offset(-1)]

        offset = self._chr_bank_offset
        if self._chr_mode == 0:
            self._chr_offsets = [
                offset(self._chr_bank0 & 0xfe), offset(self._chr_bank0 | 1)]
        elif self._chr_mode == 1:
            self._chr_offsets = [
                offset(self._chr_bank0), offset(self._chr_bank1)]


class Mapper2(Mapper):

    _prg_banks = 0
    _prg_bank1 = 0
    _prg_bank2 = 0

    def __init__(self, cartridge):
        super(Mapper2, self).__init__(cartridge)
        self._prg_banks = len(self._cartridge.prg) / 0x4000
        self._prg_bank2 = self._prg_banks - 1

    def read(self, addr):
        addr &= 0xffff
        if addr < 0x2000:
            return self._cartridge.chr[addr]
        elif addr >= 0xc000:
            i = (self._prg_bank2 * 0x4000) + (addr - 0xc000)
            return self._cartridge.prg[i]
        elif addr >= 0x8000:
            i = (self._prg_bank1 * 0x4000) + (addr - 0x8000)
            return self._cartridge.prg[i]
        elif addr >= 0x6000:
            return self._cartridge.sram[addr - 0x6000]
        else:
            raise MemoryError(self._bad_read % (type(self), addr))

    def write(self, addr, v):
        addr &= 0xffff
        v &= 0xff
        if addr < 0x2000:
            self._cartridge.chr[addr] = v
        elif addr >= 0x8000:
            self._prg_bank1 = v % self._prg_banks
        elif addr >= 0x6000:
            self._cartridge.sram[addr - 0x6000] = v
        else:
            raise MemoryError(self._bad_write % (type(self), addr))


class Mapper3(Mapper):

    _chr_bank = 0
    _prg_bank1 = 0
    _prg_bank2 = 0

    def __init__(self, cartridge):
        super(Mapper2, self).__init__(cartridge)
        self._prg_bank2 = (len(self._cartridge.prg) / 0x4000) - 1

    def read(self, addr):
        addr &= 0xffff
        if addr < 0x2000:
            return self._cartridge.chr[(self._chr_bank * 0x2000) + addr]
        elif addr >= 0xc000:
            i = (self._prg_bank2 * 0x4000) + (addr - 0xc000)
            return self._cartridge.prg[i]
        elif addr >= 0x8000:
            i = (self._prg_bank1 * 0x4000) + (addr - 0x8000)
            return self._cartridge.prg[i]
        elif addr >= 0x6000:
            return self._cartridge.sram[addr - 0x6000]
        else:
            raise MemoryError(self._bad_read % (type(self), addr))

    def write(self, addr, v):
        addr &= 0xffff
        v &= 0xff
        if addr < 0x2000:
            self._cartridge.chr[(self._chr_bank * 0x2000) + addr] = v
        elif addr >= 0x8000:
            self._chr_bank = v & 3
        elif addr >= 0x6000:
            self._cartridge.sram[addr - 0x6000] = v
        else:
            raise MemoryError(self._bad_write % (type(self), addr))


#TODO: mapper 4 is kinda big -- come back to it a little later
#class Mapper4(Mapper): pass

class Mapper7(Mapper):

    _prg_bank = 0

    def read(self, addr):
        addr &= 0xffff
        if addr < 0x2000:
            return self._cartridge.chr[addr]
        elif addr >= 0x8000:
            i = (self._prg_bank * 0x8000) + (addr - 0x8000)
            return self._cartridge.prg[i]
        elif addr >= 0x6000:
            return self._cartridge.sram[addr - 0x6000]
        else:
            raise MemoryError(self._bad_read % (type(self), addr))

    def write(self, addr, v):
        addr &= 0xffff
        v &= 0xff
        if addr < 0x2000:
            self._cartridge.chr[(self._chr_bank * 0x2000) + addr] = v
        elif addr >= 0x8000:
            self._prg_bank = v & 7
            self._cartridge.mirror = (
                    Mirror.SINGLE1 if v & 0x10 else Mirror.SINGLE0)
        elif addr >= 0x6000:
            self._cartridge.sram[addr - 0x6000] = v
        else:
            raise MemoryError(self._bad_write % (type(self), addr))


Mapper._mappers.update({
    1: Mapper1,
    2: Mapper2,
    3: Mapper3,
    #4: Mapper4,
    7: Mapper7,
})
