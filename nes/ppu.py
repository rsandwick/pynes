import logging
import pygame
logger = logging.getLogger("nes.ppu")

import console
from . import memory

class PPU(memory.PPUMemory):

    #TODO: come back to rest of struct (registers, flags, etc.)
    # registers
    v = t = x = w = f = 0
    register = 0

    # nmi flags
    nmi_occurred = 0
    nmi_output = 0
    nmi_previous = 0
    nmi_delay = 0

    # background temp vars
    name_table_byte = 0
    attribute_table_byte = 0
    low_tile_byte = 0
    high_tile_byte = 0
    tile_data = 0

    # sprite temp vars
    sprite_count = 0
    sprite_patterns = [0] * 8
    sprite_positions = [0] * 8
    sprite_priorities = [0] * 8
    sprite_indexes = [0] * 8

    # $2000 PPUCTRL
    flag_name_table = 0
    flag_increment = 0
    flag_sprite_table = 0
    flag_background_table = 0
    flag_sprite_size = 0
    flag_master_slave = 0

    # $2001 PPUMASK
    flag_grayscale = 0
    flag_show_left_background = 0
    flag_show_left_sprites = 0
    flag_show_background = 0
    flag_show_sprites = 0
    flag_red_tint = 0
    flag_green_tint = 0
    flag_blue_tint = 0

    # $2002 PPUSTATUS
    flag_sprite_zero_hit = 0
    flag_sprite_overflow = 0

    # $2003 OAMADDR
    oam_addr = 0

    # $2007 PPUDATA
    buffered = 0

    def __init__(self, console):
        super(PPU, self).__init__(console)
        self.palette = bytearray(32)
        self.nametable = bytearray(2048)
        self.oam = bytearray(256)
        self.front = pygame.Surface((256, 240))
        self.back = pygame.Surface((256, 240))
        self.reset()

    def reset(self):
        self.cycle = 340
        self.scanline = 240
        self.frame = 0
        self.CTRL = 0
        self.MASK = 0
        self.OAMADDR = 0

    def read_palette(self, addr):
        if addr >= 16 and not addr & 0x03:
            addr -= 16
        return self.palette[addr]

    def write_palette(self, addr, v):
        if addr >= 16 and not addr & 0x03:
            addr -= 16
        self.palette[addr] = v & 0xff

    @property
    def CTRL(self):
        return 0
    @CTRL.setter
    def CTRL(self, v):
        # $2000: PPUCTRL
        v = self.register = v & 0xff
        self.flag_name_table = (v >> 0) & 3
        self.flag_increment = (v >> 2) & 1
        self.flag_sprite_table = (v >> 3) & 1
        self.flag_background_table = (v >> 4) & 1
        self.flag_sprite_size = (v >> 5) & 1
        self.flag_master_slave = (v >> 6) & 1
        self.nmi_output = (v >> 7) & 1
        self.nmi_change()
        self.t = (self.t & 0xf3ff) | ((v & 0x03) << 10)

    @property
    def MASK(self):
        return 0
    @MASK.setter
    def MASK(self, v):
        # $2001: PPUMASK
        v = self.register = v & 0xff
        self.flag_grayscale = (v >> 0) & 1
        self.flag_show_left_background = (v >> 1) & 1
        self.flag_show_left_sprites = (v >> 2) & 1
        self.flag_show_background = (v >> 3) & 1
        self.flag_show_sprites = (v >> 4) & 1
        self.flag_red_tint = (v >> 5) & 1
        self.flag_green_tint = (v >> 6) & 1
        self.flag_blue_tint = (v >> 7) & 1

    @property
    def STATUS(self):
        # $2002: PPUSTATUS
        result = self.register & 0x1f
        result |= self.flag_sprite_overflow << 5
        result |= self.flag_sprite_zero_hit << 6
        if self.nmi_occurred:
            result |= 1 << 7
        self.nmi_occurred = 0
        self.nmi_change()
        self.w = 0
        return result
    @STATUS.setter
    def STATUS(self, v):
        self.register = v & 0xff

    @property
    def OAMADDR(self):
        return 0
    @OAMADDR.setter
    def OAMADDR(self, v):
        # $2003 OAMADDR
        self.oam_addr = self.register = v & 0xff

    @property
    def OAMDATA(self):
        # $2004 OAMDATA (read)
        return self.oam[self.oam_addr] & 0xff
    @OAMDATA.setter
    def OAMDATA(self, v):
        # $2004 OAMDATA (write)
        self.oam[self.oam_addr] = self.register = v & 0xff
        self.oam_addr = (self.oam_addr + 1) & 0xff

    @property
    def PPUSCROLL(self):
        return 0
    @PPUSCROLL.setter
    def PPUSCROLL(self, v):
        # $2005: PPUSCROLL
        v = self.register = v & 0xff
        if self.w == 0:
            # t: hgfedcba -> ........ ...hgfed
            self.t = (self.t & 0xffe0) | (v >> 3)
            self.x = v & 0x07
            self.w = 1
        else:
            # t: hgfedcba -> .cba..hg fed.....
            self.t = (self.t & 0x8fff) | ((v & 0x07) << 12)
            self.t = (self.t & 0xfc1f) | ((v & 0xf8) << 2)
            self.w = 0

    @property
    def PPUADDR(self):
        return 0
    @PPUADDR.setter
    def PPUADDR(self, v):
        # $2006: PPUADDR
        v = self.register = v & 0xff
        if self.w == 0:
            # t: hgfedcba -> .0fedcba ........
            self.t = (self.t & 0x80ff) | ((v & 0x3f) << 8)
            self.w = 1
        else:
            # t: hgfedcba -> ........ hgfedcba
            self.t = (self.t & 0xff00) | v
            self.v = self.t
            self.w = 0

    @property
    def PPUDATA(self):
        # $2007: PPUDATA (read)
        v = self.read(self.v)
        if self.v & 0x3fff < 0x3f00:
            v, self.buffered = self.buffered, v
        else:
            self.buffered = self.read(self.v - 0x1000)
        # increment address
        self.v += 1 if self.flag_increment == 0 else 32
        return v
    @PPUDATA.setter
    def PPUDATA(self, v):
        # $2007: PPUDATA (write)
        v = self.register = v & 0xff
        self.write(self.v, v)
        self.v += 1 if self.flag_increment == 0 else 32

    @property
    def OAMDMA(self):
        return 0
    @OAMDMA.setter
    def OAMDMA(self, v):
        # $4014: OAMDMA
        cpu = self._console.cpu
        v = self.register = v & 0xff
        if v < 0x20:
            addr = (v & 0x07) << 8
            data = cpu.ram[addr:addr + 256]
        elif v < 0x40:
            data = cpu.ram[0x2000:0x2007] * 32
        else:
            addr = v << 8
            data = cpu.ram[addr:addr + 256]
        self.oam[self.oam_addr:] = data[:256 - self.oam_addr]
        self.oam[:self.oam_addr] = data[256 - self.oam_addr:]
        cpu.stall += 513 + (cpu.cycles & 1)

    ### ntsc timing helpers ###

    def increment_x(self):
        if self.v & 0x1f == 0x1f:
            # zero out coarse x
            self.v &= 0xffe0
            # switch horizontal name table
            self.v ^= 0x0400
        else:
            # increment coarse x
            self.v += 1

    def increment_y(self):
        if self.v & 0x7000 != 0x7000:
            # increment fine y
            self.v += 0x1000
        else:
            # zero out fine y
            self.v &= 0x8fff
            # y = coarse y
            y = (self.v & 0x03e0) >> 4
            if y == 31:
                y = 0
            elif y == 29:
                y = 0
                # switch vertical name table
                self.v ^= 0x0800
            else:
                y += 1
            self.v = (self.v & 0xfc1f) | (y << 5)

    def copy_x(self):
        """Copy bits 1-5 and 11 from register t to register v."""
        self.v = (self.v & 0xfbe0) | (self.t & 0x041f)

    def copy_y(self):
        """Copy bits 6-10 and 12-15 from register t to register v."""
        self.v = (self.v & 0x841f) | (self.t & 0x7be0)

    def nmi_change(self):
        nmi = self.nmi_output and self.nmi_occurred
        if nmi and not self.nmi_previous:
            #TODO: this fixes some games but the delay shouldn't have
            #TODO: to be so long, so the timings are off somewhere
            self.nmi_delay = 15
        self.nmi_previous = nmi

    def set_vblank(self):
        self.front, self.back = self.back, self.front
        self.nmi_occurred = 1
        self.nmi_change()

    def clear_vblank(self):
        self.nmi_occurred = 0
        self.nmi_change()

    def fetch_name_table_byte(self):
        self.name_table_byte = self.read(0x2000 | (self.v & 0x0fff))

    def fetch_attr_table_byte(self):
        v = self.v & 0xffff
        addr = 0x23c0 | (v & 0x0c00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
        shift = ((v >> 4) & 4) | (v & 2)
        self.attr_table_byte = ((self.read(addr) >> shift) & 3) << 2

    def fetch_low_tile_byte(self):
        y = ((self.v & 0xffff) >> 12) & 7
        table = self.flag_background_table
        tile = self.name_table_byte
        addr = (table << 12) + (tile << 4) + y
        self.low_tile_byte = self.read(addr)

    def fetch_high_tile_byte(self):
        y = ((self.v & 0xffff) >> 12) & 7
        table = self.flag_background_table
        tile = self.name_table_byte
        addr = (table << 12) + (tile << 4) + y
        self.low_tile_byte = self.read(addr + 8)

    def store_tile_data(self):
        data = 0
        a = self.attribute_table_byte
        for _ in xrange(8):
            p1 = (self.low_tile_byte & 0x80) >> 7
            p2 = (self.high_tile_byte & 0x80) >> 6
            self.low_tile_byte <<= 1
            self.high_tile_byte <<= 1
            data <<= 4
            data |= (a | p1 | p2) & 0xffffffff
        self.tile_data |= data

    def fetch_tile_data(self):
        return (self.tile_data >> 32) & 0xffffffff

    def background_pixel(self):
        if not self.flag_show_background:
            return 0
        data = self.fetch_tile_data() >> ((7 - self.x) << 2)
        return data & 0x0f

    def sprite_pixel(self):
        if not self.flag_show_sprites:
            return (0, 0)
        for i in xrange(self.sprite_count):
            offset = (self.cycle - 1) - self.sprite_positions[i]
            if not (0 <= offset < 8):
                continue
            offset = 7 - offset
            color = (self.sprite_patterns[i] >> (offset << 2)) & 0x0f
            if not color & 3:
                continue
            return (i, color)
        return (0, 0)

    def render_pixel(self):
        x = self.cycle - 1
        y = self.scanline
        background = self.background_pixel()
        i, sprite = self.sprite_pixel()
        if x < 8:
            if not self.flag_show_left_background:
                background = 0
            if not self.flag_show_left_sprites:
                sprite = 0
        b = bool(background & 3)
        s = bool(sprite & 3)
        color = 0
        if sprite & 3:
            color = sprite & 0x10
            if background & 3:
                if self.sprite_indexes[i] == 0 and x < 255:
                    self.flag_sprite_zero_hit = 1
                if self.sprite_priorities[i]:
                    color = background
        elif background & 3:
            color = background
        c = console.palette[self.read_palette(color) & 0x3f]
        self.back.set_at((x, y), c)

    def fetch_sprite_pattern(self, i, y):
        tile = self.oam[(i << 2) + 1]
        attr = self.oam[(i << 2) + 2]
        if not self.flag_sprite_size:
            if attr & 0x80:
                y = 7 - y
            table = self.flag_sprite_table
        else:
            if attr & 0x80:
                y = 15 - y
            table = tile & 1
            tile &= 0xfe
            if y > 7:
                tile += 1
                y -= 8
        addr = (table << 12) + (tile << 4) + y
        a = (attr & 3) << 2
        low, high = self.read(addr), self.read(addr + 8)
        data = 0
        for _ in xrange(8):
            if attr & 0x40:
                p1, p2 = low & 1, (high & 1) << 1
                low >>= 1
                high >>= 1
            else:
                p1, p2 = (low & 0x80) >> 7, (high & 0x80) >> 6
                low <<= 1
                high <<= 1
            data <<= 4
            data |= a | p1 | p2
        return data

    def evaluate_sprites(self):
        h = 16 if self.flag_sprite_size else 8
        j = 0
        for i in xrange(64):
            y = self.oam[i << 2]
            a = self.oam[(i << 2) + 2]
            x = self.oam[(i << 2) + 3]
            row = self.scanline - y
            if not (0 <= row < h):
                continue
            if j < 8:
                self.sprite_patterns[j] = self.fetch_sprite_pattern(i, row)
                self.sprite_positions[j] = x
                self.sprite_priorities[j] = (a >> 5) & 1
                self.sprite_indexes[j] = i & 0xff
            j += 1
        if j > 8:
            j = 8
            self.flag_sprite_overflow = 1
        self.sprite_count = j

    def tick(self):
        """Update cycle, scanline, and frame counters."""
        if self.nmi_delay > 0:
            self.nmi_delay -= 1
            if self.nmi_delay == 0 and self.nmi_output and self.nmi_occurred:
                self._console.cpu.trigger_nmi()

        if self.flag_show_background and self.flag_show_sprites:
            #TODO: what are these magicks?
            if self.f == 1 and self.scanline == 261 and self.cycle == 339:
                self.cycle = 0
                self.scanline = 0
                self.frame += 1
                self.f ^= 1
                return

        self.cycle += 1
        if self.cycle > 340:
            self.cycle = 0
            self.scanline += 1
            if self.scanline > 261:
                self.scanline = 0
                self.frame += 1
                self.f ^= 1

    def step(self):
        """Execute a single PPU cycle."""
        self.tick()

        #render_enable = self.flag_show_background and self.flag_show_sprites
        render_enable = True
        pre_line = self.scanline == 261
        visible_line = self.scanline < 240
        #post_line = self.scanline == 240
        render_line = pre_line or visible_line
        prefetch_cycle = (321 <= self.cycle <= 336)
        visible_cycle = (0 < self.cycle <= 256)
        fetch_cycle = prefetch_cycle or visible_cycle

        if render_enable:
            # background logic
            if visible_line and visible_cycle:
                self.render_pixel()
            if render_line and fetch_cycle:
                self.tile_data <<= 4
                c = self.cycle & 7
                if c == 1:
                    self.fetch_name_table_byte()
                elif c == 3:
                    self.fetch_attr_table_byte()
                elif c == 5:
                    self.fetch_low_tile_byte()
                elif c == 7:
                    self.fetch_high_tile_byte()
                elif c == 0:
                    self.store_tile_data()
            if pre_line and (280 <= self.cycle <= 304):
                self.copy_y()
            if render_line:
                if fetch_cycle and not self.cycle & 7:
                    self.increment_x()
                if self.cycle == 256:
                    self.increment_y()
                if self.cycle == 257:
                    self.copy_x()

            # sprite logic
            if self.cycle == 257:
                if visible_line:
                    self.evaluate_sprites()
                else:
                    self.sprite_count = 0

        # vblank logic
        if self.scanline == 241 and self.cycle == 1:
            self.set_vblank()
        if pre_line and self.cycle == 1:
            self.clear_vblank()
            self.flag_sprite_zero_hit = 0
            self.flag_sprite_overflow = 0
