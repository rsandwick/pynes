import logging
import struct
logger = logging.getLogger("nes.cartridge")

MAGIC_INES = 0x4e45531a

# iNES File Header (16 bytes)
# -- magic number (4 bytes)
# -- number of PRG-ROM 16kB banks
# -- number of CHR-ROM 8kB banks
# -- control bits (2 bytes)
# -- PRG-RAM size in 8kB blocks (ignored here)
# -- other ignored data and padding (7 bytes)
iNESFileHeader = struct.Struct(">IBBHx7x")

class CartridgeError(StandardError):
    """Base class for cartridge-related errors."""
class InvalidNESFileError(CartridgeError):
    """Cartridge file has invalid contents."""

class Cartridge(object):

    def __init__(self, prg, chr, mapper, mirror, battery):
        self.prg = prg
        self.chr = chr
        self.mapper = mapper
        self.mirror = mirror
        self.battery = battery
        self.sram = bytearray(8 << 10)

    @classmethod
    def fromfile(cls, path):
        """Read an iNES file (.nes) and return a Cartridge.

        http://wiki.nesdev.com/w/index.php/INES

        """
        with open(path, "rb") as f:
            header = f.read(iNESFileHeader.size)
            magic, numprg, numchr, ctrl = iNESFileHeader.unpack(header)
            if magic != MAGIC_INES:
                raise InvalidNESFileError("invalid .nes file")

            mapper = (ctrl & 0xf0) | ((ctrl >> 12) & 0x0f)
            mirror = ((ctrl & 0x100) >> 8) | ((ctrl & 0x800) >> 10)
            battery = bool(ctrl & 0x200)

            if ctrl & 0x400:
                trainer = bytearray(f.read(512))

            prg = bytearray(f.read(numprg * 16 << 10))
            chr = bytearray(f.read(numchr * 8 << 10) if numchr else 8 << 10)

            return cls(prg, chr, mapper, mirror, battery)
