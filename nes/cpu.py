import logging
import pprint
logger = logging.getLogger("nes.cpu")

import memory
from .util import enum
from .instructions import addressing_modes, instructions, pages_differ

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

class CPU(memory.CPUMemory):

    # cpu internal data
    p = 0 # program counter
    s = 0 # stack pointer
    # -- underlying data for properties .a, .x, .y
    _a = 0 # accumulator
    _x = 0 # x register
    _y = 0 # y register

    # cpu flags
    c = False # carry
    z = False # zero
    i = False # interrupt disable
    d = False # decimal mode
    b = False # break command
    u = False # unused
    v = False # overflow
    n = False # negative

    # 64kB memory area
    memory = None
    # interrupt type to perform
    interrupt = Interrupt.NONE
    # cycle count (for platform timing quirks)
    cycles = 0
    # number of cycles to stall
    stall = 0

    def __init__(self, console):
        self._console = console
        # instruction (+ data) queue
        self.mem = bytearray(65536)
        self.reset()

    _flags = "czidbuvn"
    @property
    def flags(self):
        """Flags bitfield, representing additional CPU state."""
        return (self.c +
                self.z << 1 +
                self.i << 2 +
                self.d << 3 +
                self.b << 4 +
                self.u << 5 +
                self.v << 6 +
                self.n << 7)
    @flags.setter
    def flags(self, flags):
        self.c = bool(flags & 0x01)
        self.z = bool(flags & 0x02)
        self.i = bool(flags & 0x04)
        self.d = bool(flags & 0x08)
        self.b = bool(flags & 0x10)
        self.u = bool(flags & 0x20)
        self.v = bool(flags & 0x40)
        self.n = bool(flags & 0x80)

    @property
    def a(self):
        return self._a
    @a.setter
    def a(self, v):
        self._a = v & 0xff
        self.set_zn(v)

    @property
    def x(self):
        return self._x
    @x.setter
    def x(self, v):
        self._x = v & 0xff
        self.set_zn(v)

    @property
    def y(self):
        return self._y
    @y.setter
    def y(self, v):
        self._y = v & 0xff
        self.set_zn(v)

    def reset(self):
        self.p = self.read16(0xfffc)
        self.s = 0xfd
        self.flags = 0x24

    def print_instruction(self):
        op = instructions[self.read(self.p)]
        w = tuple("%02x" % (self.read(self.p + i),) for i in xrange(op.size))
        w0, w1, w2 = (w + ("  ", "  ", "  "))[:3]
        logger.debug(
                "%04x %s %s %s %s "
                "a:%02x x:%02x y:%02x flags:%02x s:%02x cyc:%3d",
                self.p, w0, w1, w2, op.name.ljust(32, " "),
                self.a, self.x, self.y, self.flags, self.s,
                (self.cycles * 3) % 341)

    def compare(self, a, b):
        self.set_zn(a - b)
        self.c = (a >= b)

    def read16(self, addr):
        """Read two bytes as a 16-bit word."""
        return self.read(addr) + (self.read(addr + 1) << 8)

    def read16bug(self, addr):
        """Read two bytes as a 16-bit word, while also emulating a 6502 bug.

        The bug caused the low byte to wrap w/o incrementing the high byte.

        """
        bug = (addr & 0xff00) | ((addr & 0xff) + 1)
        return self.read(addr) + (self.read(bug) << 8)

    def push(self, v):
        """Push one byte onto the stack."""
        self.write((self.s & 0xffff) | 0x100, v & 0xff)
        self.s -= 1

    def push16(self, v):
        """Push a 16-bit word onto the stack as two bytes."""
        self.push((v >> 8) & 0xff)
        self.push(v & 0xff)

    def pull(self):
        """Pop one byte from the stack."""
        self.s += 1
        return self.read((self.s & 0xffff) | 0x100)

    def pull16(self):
        """Pop two bytes from the stack as a 16-bit word."""
        return self.pull() | (self.pull() << 8)

    def set_z(self, v):
        self.z = (v == 0)

    def set_n(self, v):
        self.n = ((v & 0x80) != 0)

    def set_zn(self, v):
        # inline copy rather than another function call
        self.z = (v == 0)
        self.n = ((v & 0x80) != 0)

    def trigger_nmi(self):
        self.interrupt = Interrupt.NMI

    def trigger_irq(self):
        if not self.I:
            self.interrupt = Interrupt.IRQ

    ### interrupts ###

    def NMI(self):
        """Perform a non-maskable interrupt."""
        self.push16(self.p)
        self.PHP(None, None)
        self.p = self.read16(0xfffa)
        self.i = 1
        self.cycles += 7

    def IRQ(self):
        """Perform an IRQ interrupt."""
        self.push16(self.p)
        self.PHP()
        self.p = self.read16(0xfffe)
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
            self.NMI()
        elif self.interrupt == Interrupt.IRQ:
            self.IRQ()
        self.interrupt = Interrupt.NONE

        opcode = self.read(self.p)
        op = instructions[opcode]
        mode = op.mode
        addr, page_crossed = addressing_modes[mode](self)

        self.p = (self.p + op.size) & 0xffff
        self.cycles += op.cycles + (op.pagecycles if page_crossed else 0)

        self._instructions[opcode](self, addr, mode)
        return self.cycles - cycles

    ### base branching op ###

    def _branch(self, condition, addr):
        """Branch if a condition is met."""
        if condition:
            # add 1 cycle for taking a branch, 2 if it jumps to a new page
            self.cycles += 2 if pages_differ(self.p, addr) else 1
            self.p = addr

    ### simple instructions (one-liners) ###

    # logical AND
    def AND(self, addr, mode): self.a = self.a & self.read(addr)

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
    def CLV(self, addr, mode): self.v = False

    # compare accumulator
    def CMP(self, addr, mode): self.compare(self.a, self.read(addr))

    # compare register x
    def CPX(self, addr, mode): self.compare(self.x, self.read(addr))

    # compare register y
    def CPY(self, addr, mode): self.compare(self.y, self.read(addr))

    # decrement register x
    def DEX(self, addr, mode): self.x -= 1

    # decrement register y
    def DEY(self, addr, mode): self.y -= 1

    # exclusive OR
    def EOR(self, addr, mode): self.a ^= self.read(addr)

    # increment register x
    def INX(self, addr, mode): self.x += 1

    # increment register y
    def INY(self, addr, mode): self.y += 1

    # jump
    def JMP(self, addr, mode): self.p = addr & 0xffff

    # load accumulator
    def LDA(self, addr, mode): self.a = self.read(addr)

    # load register x
    def LDX(self, addr, mode): self.x = self.read(addr)

    # load register y
    def LDY(self, addr, mode): self.y = self.read(addr)

    # no operation
    def NOP(self, addr, mode): pass

    # inclusive OR
    def ORA(self, addr, mode): self.a |= self.read(addr) & 0xff

    # push accumulator
    def PHA(self, addr, mode): self.push(self.a)

    # push processor status
    def PHP(self, addr, mode): self.push(self.flags & 0x10)

    # pull accumulator
    def PLA(self, addr, mode): self.a = self.pull()

    # pull processor status
    def PLP(self, addr, mode): self.flags = (self.pull() & 0xef) | 0x20

    # return from subroutine
    def RTS(self, addr, mode): self.p = (self.pull16() + 1) & 0xffff

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
    def TSX(self, addr, mode): self.x = self.s

    # transfer register x to accumulator
    def TXA(self, addr, mode): self.a = self.x

    # transfer register x to stack pointer
    def TXS(self, addr, mode): self.s = self.x

    # transfer register y to accumulator
    def TYA(self, addr, mode): self.a = self.y

    ### complex instructions ###

    def ADC(self, addr, mode):
        """Add w/ carry."""
        a, b, c = self.a, self.read(addr), self.c
        self.a = a + b + c
        self.c = (a + b + c > 0xff)
        self.v = ((a ^ b) & 0x80 == 0) and ((a ^ self.a) & 0x80 != 0)

    def ASL(self, addr, mode):
        """Arithmetic shift left."""
        if mode == ADDRMODE.Accumulator:
            self.c = (self.a >> 7) & 1
            self.a <<= 1
        else:
            value = self.read(addr)
            self.c = (v >> 7) & 1
            v <<= 1
            self.write(addr, v)
            self.set_zn(v)

    def BIT(self, addr, mode):
        """Bit test."""
        v = self.read(addr)
        self.v = (v >> 6) & 1
        self.set_z(v & self.a)
        self.set_n(v)

    def BRK(self, addr, mode):
        """Force interrupt."""
        self.push16(self.p)
        self.PHP(None, None)
        self.SEI(None, None)
        self.p = self.read16(0xfffe)

    def DEC(self, addr, mode):
        """Decrement memory."""
        v = (self.read(addr) - 1) & 0xff
        self.write(addr, v)
        self.set_zn(v)

    def INC(self, addr, mode):
        """Increment memory."""
        v = (self.read(addr) + 1) & 0xff
        self.write(addr, v)
        self.set_zn(v)

    def JSR(self, addr, mode):
        """Jump to subroutine."""
        self.push16(self.p - 1)
        self.p = addr & 0xffff

    def LSR(self, addr, mode):
        """Logical shift right."""
        if mode == ADDRMODE.Accumulator:
            self.c = self.a & 1
            self.a >>= 1
        else:
            v = self.read(addr) & 0xff
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
            v = self.read(addr)
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
            v = self.read(addr)
            self.c = v & 1
            v = (v >> 1) | (c << 7)
            self.write(addr, v)
            self.set_zn(v)

    def RTI(self, addr, mode):
        """Return from interrupt."""
        self.flags = (self.pull() & 0xef) | 0x20
        self.p = self.pull16()

    def SBC(self, addr, mode):
        """Subtract w/ carry."""
        a, b, c = self.a, self.read(addr), self.c
        v = a - b - (1 - c)
        self.a = v
        self.c = (v >= 0)
        self.v = ((a ^ b) & 0x80 != 0) and ((a ^ self.a) & 0x80 != 0)

    ### unofficial instructions ###
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

    _instructions = [locals()[i.name] for i in instructions]
