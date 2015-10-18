import logging

from .util import enum
from .instructions import instructions

logger = logging.getLogger("nes.cpu")

CPUFREQ = 1789773.0

# interrupt types
Interrupt = enum([
    "NONE",
    "NMI",
    "IRQ",
])

# addressing modes
Mode = enum([
    "Absolute",
    "AbsoluteX",
    "AbsoluteY",
    "Accumulator",
    "Immediate",
    "Implied",
    "IndexedIndirect",
    "Indirect",
    "IndirectIndexed",
    "Relative",
    "ZeroPage",
    "ZeroPageX",
    "ZeroPageY",
])

def pages_differ(a, b):
    """Test if two addresses reference different pages."""
    return a & 0xff00 != b & 0xff00

class CPU(object):

    # cpu internal data
    _pc = 0 # program counter
    _sp = 0 # stack pointer
    _a = 0 # accumulator
    _x = 0 # x register
    _y = 0 # y register

    # 64kB memory area
    memory = None
    # interrupt type to perform
    interrupt = Interrupt.NONE
    # cycle count (for platform timing quirks)
    cycles = 0
    # number of cycles to stall
    stall = 0

    def __init__(self):
        # instruction (+ data) queue
        self.memory = bytearray(65536)
        self.reset()

    _flags = "czidbuvn"
    @property
    def flags(self):
        """Flags bitfield, representing additional CPU state.

        In order of least to most significant bit, the flags are:

        (c)arry --
        (z)ero --
        (i)nterrupt Disable --
        (d)ecimal Mode --
        (b)reak Command --
        (u)nused --
        O(v)erflow --
        (n)egative --

        """
        return sum(getattr(self, k) << i for i, k in enumerate(self._flags))
    @flags.setter
    def flags(self, flags):
        for i, k in enumerate(self._flags):
            setattr(self, k, bool((flags >> i) & 1))

    def __getattr__(self, k):
        _k = "_%s" % (k,)
        if k == "pc":
            return getattr(self, _k) & 0xffff
        elif k in ("sp", "a", "x", "y"):
            return getattr(self, _k) & 0xff
        elif k in self._flags:
            return bool(getattr(self, _k))
        else:
            return super(CPU, self).__getattr__(k)
            #raise AttributeError("CPU object has no attribute '%s'" % (k,))

    def __setattr__(self, k, v):
        _k = "_%s" % (k,)
        if k == "pc":
            setattr(self, _k, v & 0xffff)
        elif k == "sp":
            setattr(self, _k, v & 0xff)
        elif k in ("a", "x", "y"):
            setattr(self, _k, v & 0xff)
            self.set_zn(v & 0xff)
        elif k in self._flags:
            setattr(self, _k, bool(v))
        else:
            super(CPU, self).__setattr__(k, v)
            #raise AttributeError("CPU object has no attribute '%s'" % (k,))

    def reset(self):
        self.pc = self.read16(0xfffc)
        self.sp = 0xfd
        self.flags = 0x24

    def print_instruction(self):
        op = instructions[self.read()]
        w = tuple("%02x" % (self.read(offset=i),) for i in xrange(op.size))
        w0, w1, w2 = (w + ("  ", "  ", "  "))[:3]
        logger.debug(
                "%04x %s %s %s %s "
                "a:%02x x:%02x y:%02x p:%02x sp:%02x cyc:%3d",
                self.pc, w0, w1, w2, op.name.ljust(32, " "),
                self.a, self.x, self.y, self.flags, self.sp,
                (self.cycles * 3) % 341)

    def compare(self, a, b):
        self.set_zn(a - b)
        self.c = (a >= b)

    def read(self, addr=None, offset=0):
        return self.memory[(self.pc if addr is None else addr) + offset]

    def read16(self, addr=None, offset=0):
        """Read two bytes as a 16-bit word."""
        addr = (self.pc if addr is None else addr) + offset
        return self.memory[addr] + (self.memory[addr + 1] << 8)

    def read16bug(self, addr=None, offset=0):
        """Read two bytes as a 16-bit word, while also emulating a 6502 bug.

        The bug caused the low byte to wrap w/o incrementing the high byte.

        """
        addr = (self.pc if addr is None else addr) + offset
        bug = (addr & 0xff00) | ((addr & 0xff) + 1)
        return self.memory[addr] + (self.memory[bug] << 8)

    def push(self, v):
        """Push one byte onto the stack."""
        self.memory[(self.sp & 0xffff) | 0x100] = v & 0xff
        self.sp -= 1

    def push16(self, v):
        """Push a 16-bit word onto the stack as two bytes."""
        self.push((v >> 8) & 0xff)
        self.push(v & 0xff)

    def pull(self):
        """Pop one byte from the stack."""
        self.sp += 1
        return self.memory[(self.sp & 0xffff) | 0x100]

    def pull16(self):
        """Pop two bytes from the stack as a 16-bit word."""
        return self.pull() | (self.pull() << 8)

    def set_z(self, v):
        self.z = (v == 0)

    def set_n(self, v):
        self.n = ((v & 0x80) != 0)

    def set_zn(self, v):
        self.set_z(v)
        self.set_n(v)

    def trigger_nmi(self):
        self.interrupt = Interrupt.NMI

    def trigger_irq(self):
        if not self.I:
            self.interrupt = Interrupt.IRQ

    ### interrupts ###

    def nmi(self):
        """Perform a non-maskable interrupt."""
        self.push16(self.pc)
        self.php(None, None)
        self.pc = self.read16(addr=0xfffa)
        self.i = 1
        self.cycles += 7

    def irq(self):
        """Perform an IRQ interrupt."""
        self.push16(self.pc)
        self.php()
        self.pc = self.read16(addr=0xfffe)
        self.i = 1
        self.cycles += 7

    ### base execution op ###

    def step(self):
        """Execute a CPU instruction."""
        if self.stall > 0:
            self.stall -= 1
            return 1

        cycles = self.cycles

        if self.interrupt == Interrupt.NMI:
            self.nmi()
        elif self.interrupt == Interrupt.IRQ:
            self.irq()
        self.interrupt = Interrupt.NONE

        op = instructions[self.read()]
        mode = op.mode
        page_crossed = False

        if mode == Mode.Absolute:
            addr = self.read16(offset=1)
        elif mode == Mode.AbsoluteX:
            addr = (self.read16(offset=1) + (self.x)) & 0xffff
            page_crossed = pages_differ(addr - self.x, addr)
        elif mode == Mode.AbsoluteY:
            addr = (self.read16(offset=1) + (self.y)) & 0xffff
            page_crossed = pages_differ(addr - self.y, addr)
        elif mode == Mode.Accumulator:
            addr = 0
        elif mode == Mode.Immediate:
            addr = self.pc + 1
        elif mode == Mode.Implied:
            addr = 0
        elif mode == Mode.IndexedIndirect:
            addr = self.read16bug(offset=self.read(offset=1) + self.x)
        elif mode == Mode.Indirect:
            addr = self.read16bug(offset=self.read(offset=1))
        elif mode == Mode.IndirectIndexed:
            addr = self.read16bug(offset=self.read(offset=1)) + self.y
            page_crossed = pages_differ(addr - self.y, addr)
        elif mode == Mode.Relative:
            offset = self.read(offset=1)
            addr = self.pc + 2 + offset - (0 if offset < 0x80 else 0x100)
        elif mode == Mode.ZeroPage:
            addr = self.read(offset=1)
        elif mode == Mode.ZeroPageX:
            addr = self.read(offset=1) + self.x
        elif mode == Mode.ZeroPageY:
            addr = self.read(offset=1) + self.y
        addr &= 0xffff

        self.pc += op.size & 0xffff
        self.cycles += op.cycles + (op.pagecycles if page_crossed else 0)

        getattr(self, op.name)(addr, mode)
        return self.cycles - cycles

    ### base branching op ###

    def _branch(self, condition, addr):
        """Branch if a condition is met."""
        if condition:
            # add 1 cycle for taking a branch, 2 if it jumps to a new page
            self.cycles += 2 if pages_differ(self.pc, addr) else 1
            self.pc = addr

    ### simple instructions (one-liners) ###

    # logical AND
    def AND(self, addr, mode): self.a = self.a & self.read(addr=addr)

    # branch if carry clear
    def BCC(self, addr, mode): self._branch(not self.c, addr)

    # branch if carry set
    def BCS(self, addr, mode): self._branch(self.c, addr)

    # branch if equal
    def BEQ(self, addr, mode): self._branch(self.z, addr)

    # branch if minus
    def BMI(self, addr, mode): self._branch(self.n, addr)

    # branch if not equal
    def BNE(self, addr, mode): self._branch(not self.z, addr)

    # branch if positive
    def BPL(self, addr, mode): self._branch(not self.n, addr)

    # branch if overflow clear
    def BVC(self, addr, mode): self._branch(not self.v, addr)

    # branch if overflow set
    def BVS(self, addr, mode): self._branch(self.v, addr)

    # clear carry flag
    def CLC(self, addr, mode): self.c = False

    # clear decimal mode
    def CLD(self, addr, mode): self.d = False

    # clear interrupt disable
    def CLI(self, addr, mode): self.i = False

    # clear overflow flag
    def CLC(self, addr, mode): self.v = False

    # compare accumulator
    def CMP(self, addr, mode): self.compare(self.a, self.read(addr=addr))

    # compare register x
    def CPX(self, addr, mode): self.compare(self.x, self.read(addr=addr))

    # compare register y
    def CPY(self, addr, mode): self.compare(self.y, self.read(addr=addr))

    # decrement register x
    def DEX(self, addr, mode): self.x -= 1

    # decrement register y
    def DEY(self, addr, mode): self.y -= 1

    # exclusive OR
    def EOR(self, addr, mode): self.a ^= self.read(addr=addr)

    # increment register x
    def INX(self, addr, mode): self.x += 1

    # increment register y
    def INY(self, addr, mode): self.y += 1

    # jump
    def JMP(self, addr, mode): self.pc = addr & 0xffff

    # load accumulator
    def LDA(self, addr, mode): self.a = self.read(addr=addr)

    # load register x
    def LDX(self, addr, mode): self.x = self.read(addr=addr)

    # load register y
    def LDY(self, addr, mode): self.y = self.read(addr=addr)

    # no operation
    def NOP(self, addr, mode): pass

    # inclusive OR
    def ORA(self, addr, mode): self.a |= self.read(addr=addr) & 0xff

    # push accumulator
    def PHA(self, addr, mode): self.push(self.a)

    # push processor status
    def PHP(self, addr, mode): self.push(self.flags & 0x10)

    # pull accumulator
    def PLA(self, addr, mode): self.a = self.pull()

    # pull processor status
    def PLP(self, addr, mode): self.flags = (self.pull() & 0xef) | 0x20

    # return from subroutine
    def RTS(self, addr, mode): self.pc = (self.pull16() + 1) & 0xffff

    # set carry flag
    def SEC(self, addr, mode): self.c = True

    # set decimal flag
    def SED(self, addr, mode): self.d = True

    # set interrupt disable
    def SEI(self, addr, mode): self.i = True

    # store accumulator
    def STA(self, addr, mode): self.write(addr, self.a)

    # store register x
    def STX(self, addr, mode): self.write(addr, self.x)

    # store register y
    def STY(self, addr, mode): self.write(addr, self.y)

    # transfer accumulator to register x
    def TAX(self, addr, mode): self.x = self.a

    # transfer accumulator to register y
    def TAY(self, addr, mode): self.y = self.a

    # transfer stack pointer to register x
    def TSX(self, addr, mode): self.x = self.sp

    # transfer register x to accumulator
    def TXA(self, addr, mode): self.a = self.x

    # transfer register x to stack pointer
    def TXS(self, addr, mode): self.sp = self.x

    # transfer register y to accumulator
    def TXY(self, addr, mode): self.a = self.y

    ### complex instructions ###

    def ADC(self, addr, mode):
        """Add w/ carry."""
        a, b, c = self.a, self.read(addr=addr), self.c
        self.a = a + b + c
        self.c = (a + b + c > 0xff)
        self.v = ((a ^ b) & 0x80 == 0) and ((a ^ self.a) & 0x80 != 0)

    def ASL(self, addr, mode):
        """Arithmetic shift left."""
        if mode == ADDRMODE.Accumulator:
            self.c = (self.a >> 7) & 1
            self.a <<= 1
        else:
            value = self.read(addr=addr)
            self.c = (v >> 7) & 1
            v <<= 1
            self.write(addr, v)
            self.set_zn(v)

    def BIT(self, addr, mode):
        """Bit test."""
        v = self.read(addr=addr)
        self.v = (v >> 6) & 1
        self.set_z(v & self.a)
        self.set_n(v)

    def BRK(self, addr, mode):
        """Force interrupt."""
        self.push16(self.pc)
        self.php(None, None)
        self.sei(None, None)
        self.pc = self.read16(addr=0xfffe)

    def DEC(self, addr, mode):
        """Decrement memory."""
        v = (self.read(addr=addr) - 1) & 0xff
        self.write(addr, v)
        self.set_zn(v)

    def INC(self, addr, mode):
        """Increment memory."""
        v = (self.read(addr=addr) + 1) & 0xff
        self.write(addr, v)
        self.set_zn(v)

    def JSR(self, addr, mode):
        """Jump to subroutine."""
        self.push16(self.pc - 1)
        self.pc = addr & 0xffff

    def LSR(self, addr, mode):
        """Logical shift right."""
        if mode == ADDRMODE.Accumulator:
            self.c = self.a & 1
            self.a >>= 1
        else:
            v = self.read(addr=addr) & 0xff
            self.c = v & 1
            v >>= 1
            self.write(addr, v)
            self.set_zn(v)

    def ROL(self, addr, mode):
        """Rotate left."""
        c = self.c
        if mode == ADDRMODE.Accumulator:
            self.c = (self.a >> 7) & 1
            sefl.a = (self.a << 1) | c
        else:
            v = self.read(addr=addr)
            self.c = (v >> 7) & 1
            v = (v << 1) | c
            self.write(addr, v)
            self.set_zn(v)

    def ROR(self, addr, mode):
        """Rotate left."""
        c = self.c
        if mode == ADDRMODE.Accumulator:
            self.c = self.a & 1
            self.a = (self.a >> 1) | (c << 7)
        else:
            v = self.read(addr=addr)
            self.c = v & 1
            v = (v >> 1) | (c << 7)
            self.write(addr, v)
            self.set_zn(v)

    def RTI(self, addr, mode):
        """Return from interrupt."""
        self.flags = (self.pull() & 0xef) | 0x20
        self.pc = self.pull16()

    def SBC(self, addr, mode):
        """Subtract w/ carry."""
        a, b, c = self.a, self.read(addr=addr), self.c
        v = a - b - (1 - c)
        self.a = v
        self.c = (v >= 0)
        self.v = ((a ^ b) & 0x80 != 0) and ((a ^ self.a) & 0x80 != 0)

    ### disallowed instructions ###
    def AHX(self, addr, mode): pass
    def ALR(self, addr, mode): pass
    def ANC(self, addr, mode): pass
    def ARR(self, addr, mode): pass
    def AXS(self, addr, mode): pass
    def DCP(self, addr, mode): pass
    def ISC(self, addr, mode): pass
    def KIL(self, addr, mode): pass
    def LAS(self, addr, mode): pass
    def LAX(self, addr, mode): pass
    def RLA(self, addr, mode): pass
    def RRA(self, addr, mode): pass
    def SAX(self, addr, mode): pass
    def SHX(self, addr, mode): pass
    def SHY(self, addr, mode): pass
    def SLO(self, addr, mode): pass
    def SRE(self, addr, mode): pass
    def TAS(self, addr, mode): pass
    def XAA(self, addr, mode): pass
