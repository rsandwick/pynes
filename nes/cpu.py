import collections
import logging
logger = logging.getLogger("cpu")

CPUFREQ = 1789773

#TODO: put this somewhere else?
class enum(tuple): __getattr__ = tuple.index

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


# container for information that the instruction functions use
StepInfo = collections.namedtuple("StepInfo", ["address", "pc", "mode"])

# container for op details
Instruction = collections.namedtuple("Instruction",
        ["name", "mode", "size", "cycles", "pagecycles"])

# mapping of opcodes to op details
instructions = dict(enumerate(Instruction(op) for op in [
        ("BRK", 6, 1, 7, 0),
        ("ORA", 7, 2, 6, 0),
        ("KIL", 6, 0, 2, 0),
        ("SLO", 7, 0, 8, 0),
        ("NOP", 11, 2, 3, 0),
        ("ORA", 11, 2, 3, 0),
        ("ASL", 11, 2, 5, 0),
        ("SLO", 11, 0, 5, 0),
        ("PHP", 6, 1, 3, 0),
        ("ORA", 5, 2, 2, 0),
        ("ASL", 4, 1, 2, 0),
        ("ANC", 5, 0, 2, 0),
        ("NOP", 1, 3, 4, 0),
        ("ORA", 1, 3, 4, 0),
        ("ASL", 1, 3, 6, 0),
        ("SLO", 1, 0, 6, 0),
        ("BPL", 10, 2, 2, 1),
        ("ORA", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("SLO", 9, 0, 8, 0),
        ("NOP", 12, 2, 4, 0),
        ("ORA", 12, 2, 4, 0),
        ("ASL", 12, 2, 6, 0),
        ("SLO", 12, 0, 6, 0),
        ("CLC", 6, 1, 2, 0),
        ("ORA", 3, 3, 4, 1),
        ("NOP", 6, 1, 2, 0),
        ("SLO", 3, 0, 7, 0),
        ("NOP", 2, 3, 4, 1),
        ("ORA", 2, 3, 4, 1),
        ("ASL", 2, 3, 7, 0),
        ("SLO", 2, 0, 7, 0),
        ("JSR", 1, 3, 6, 0),
        ("AND", 7, 2, 6, 0),
        ("KIL", 6, 0, 2, 0),
        ("RLA", 7, 0, 8, 0),
        ("BIT", 11, 2, 3, 0),
        ("AND", 11, 2, 3, 0),
        ("ROL", 11, 2, 5, 0),
        ("RLA", 11, 0, 5, 0),
        ("PLP", 6, 1, 4, 0),
        ("AND", 5, 2, 2, 0),
        ("ROL", 4, 1, 2, 0),
        ("ANC", 5, 0, 2, 0),
        ("BIT", 1, 3, 4, 0),
        ("AND", 1, 3, 4, 0),
        ("ROL", 1, 3, 6, 0),
        ("RLA", 1, 0, 6, 0),
        ("BMI", 10, 2, 2, 1),
        ("AND", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("RLA", 9, 0, 8, 0),
        ("NOP", 12, 2, 4, 0),
        ("AND", 12, 2, 4, 0),
        ("ROL", 12, 2, 6, 0),
        ("RLA", 12, 0, 6, 0),
        ("SEC", 6, 1, 2, 0),
        ("AND", 3, 3, 4, 1),
        ("NOP", 6, 1, 2, 0),
        ("RLA", 3, 0, 7, 0),
        ("NOP", 2, 3, 4, 1),
        ("AND", 2, 3, 4, 1),
        ("ROL", 2, 3, 7, 0),
        ("RLA", 2, 0, 7, 0),
        ("RTI", 6, 1, 6, 0),
        ("EOR", 7, 2, 6, 0),
        ("KIL", 6, 0, 2, 0),
        ("SRE", 7, 0, 8, 0),
        ("NOP", 11, 2, 3, 0),
        ("EOR", 11, 2, 3, 0),
        ("LSR", 11, 2, 5, 0),
        ("SRE", 11, 0, 5, 0),
        ("PHA", 6, 1, 3, 0),
        ("EOR", 5, 2, 2, 0),
        ("LSR", 4, 1, 2, 0),
        ("ALR", 5, 0, 2, 0),
        ("JMP", 1, 3, 3, 0),
        ("EOR", 1, 3, 4, 0),
        ("LSR", 1, 3, 6, 0),
        ("SRE", 1, 0, 6, 0),
        ("BVC", 10, 2, 2, 1),
        ("EOR", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("SRE", 9, 0, 8, 0),
        ("NOP", 12, 2, 4, 0),
        ("EOR", 12, 2, 4, 0),
        ("LSR", 12, 2, 6, 0),
        ("SRE", 12, 0, 6, 0),
        ("CLI", 6, 1, 2, 0),
        ("EOR", 3, 3, 4, 1),
        ("NOP", 6, 1, 2, 0),
        ("SRE", 3, 0, 7, 0),
        ("NOP", 2, 3, 4, 1),
        ("EOR", 2, 3, 4, 1),
        ("LSR", 2, 3, 7, 0),
        ("SRE", 2, 0, 7, 0),
        ("RTS", 6, 1, 6, 0),
        ("ADC", 7, 2, 6, 0),
        ("KIL", 6, 0, 2, 0),
        ("RRA", 7, 0, 8, 0),
        ("NOP", 11, 2, 3, 0),
        ("ADC", 11, 2, 3, 0),
        ("ROR", 11, 2, 5, 0),
        ("RRA", 11, 0, 5, 0),
        ("PLA", 6, 1, 4, 0),
        ("ADC", 5, 2, 2, 0),
        ("ROR", 4, 1, 2, 0),
        ("ARR", 5, 0, 2, 0),
        ("JMP", 8, 3, 5, 0),
        ("ADC", 1, 3, 4, 0),
        ("ROR", 1, 3, 6, 0),
        ("RRA", 1, 0, 6, 0),
        ("BVS", 10, 2, 2, 1),
        ("ADC", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("RRA", 9, 0, 8, 0),
        ("NOP", 12, 2, 4, 0),
        ("ADC", 12, 2, 4, 0),
        ("ROR", 12, 2, 6, 0),
        ("RRA", 12, 0, 6, 0),
        ("SEI", 6, 1, 2, 0),
        ("ADC", 3, 3, 4, 1),
        ("NOP", 6, 1, 2, 0),
        ("RRA", 3, 0, 7, 0),
        ("NOP", 2, 3, 4, 1),
        ("ADC", 2, 3, 4, 1),
        ("ROR", 2, 3, 7, 0),
        ("RRA", 2, 0, 7, 0),
        ("NOP", 5, 2, 2, 0),
        ("STA", 7, 2, 6, 0),
        ("NOP", 5, 0, 2, 0),
        ("SAX", 7, 0, 6, 0),
        ("STY", 11, 2, 3, 0),
        ("STA", 11, 2, 3, 0),
        ("STX", 11, 2, 3, 0),
        ("SAX", 11, 0, 3, 0),
        ("DEY", 6, 1, 2, 0),
        ("NOP", 5, 0, 2, 0),
        ("TXA", 6, 1, 2, 0),
        ("XAA", 5, 0, 2, 0),
        ("STY", 1, 3, 4, 0),
        ("STA", 1, 3, 4, 0),
        ("STX", 1, 3, 4, 0),
        ("SAX", 1, 0, 4, 0),
        ("BCC", 10, 2, 2, 1),
        ("STA", 9, 2, 6, 0),
        ("KIL", 6, 0, 2, 0),
        ("AHX", 9, 0, 6, 0),
        ("STY", 12, 2, 4, 0),
        ("STA", 12, 2, 4, 0),
        ("STX", 13, 2, 4, 0),
        ("SAX", 13, 0, 4, 0),
        ("TYA", 6, 1, 2, 0),
        ("STA", 3, 3, 5, 0),
        ("TXS", 6, 1, 2, 0),
        ("TAS", 3, 0, 5, 0),
        ("SHY", 2, 0, 5, 0),
        ("STA", 2, 3, 5, 0),
        ("SHX", 3, 0, 5, 0),
        ("AHX", 3, 0, 5, 0),
        ("LDY", 5, 2, 2, 0),
        ("LDA", 7, 2, 6, 0),
        ("LDX", 5, 2, 2, 0),
        ("LAX", 7, 0, 6, 0),
        ("LDY", 11, 2, 3, 0),
        ("LDA", 11, 2, 3, 0),
        ("LDX", 11, 2, 3, 0),
        ("LAX", 11, 0, 3, 0),
        ("TAY", 6, 1, 2, 0),
        ("LDA", 5, 2, 2, 0),
        ("TAX", 6, 1, 2, 0),
        ("LAX", 5, 0, 2, 0),
        ("LDY", 1, 3, 4, 0),
        ("LDA", 1, 3, 4, 0),
        ("LDX", 1, 3, 4, 0),
        ("LAX", 1, 0, 4, 0),
        ("BCS", 10, 2, 2, 1),
        ("LDA", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("LAX", 9, 0, 5, 1),
        ("LDY", 12, 2, 4, 0),
        ("LDA", 12, 2, 4, 0),
        ("LDX", 13, 2, 4, 0),
        ("LAX", 13, 0, 4, 0),
        ("CLV", 6, 1, 2, 0),
        ("LDA", 3, 3, 4, 1),
        ("TSX", 6, 1, 2, 0),
        ("LAS", 3, 0, 4, 1),
        ("LDY", 2, 3, 4, 1),
        ("LDA", 2, 3, 4, 1),
        ("LDX", 3, 3, 4, 1),
        ("LAX", 3, 0, 4, 1),
        ("CPY", 5, 2, 2, 0),
        ("CMP", 7, 2, 6, 0),
        ("NOP", 5, 0, 2, 0),
        ("DCP", 7, 0, 8, 0),
        ("CPY", 11, 2, 3, 0),
        ("CMP", 11, 2, 3, 0),
        ("DEC", 11, 2, 5, 0),
        ("DCP", 11, 0, 5, 0),
        ("INY", 6, 1, 2, 0),
        ("CMP", 5, 2, 2, 0),
        ("DEX", 6, 1, 2, 0),
        ("AXS", 5, 0, 2, 0),
        ("CPY", 1, 3, 4, 0),
        ("CMP", 1, 3, 4, 0),
        ("DEC", 1, 3, 6, 0),
        ("DCP", 1, 0, 6, 0),
        ("BNE", 10, 2, 2, 1),
        ("CMP", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("DCP", 9, 0, 8, 0),
        ("NOP", 12, 2, 4, 0),
        ("CMP", 12, 2, 4, 0),
        ("DEC", 12, 2, 6, 0),
        ("DCP", 12, 0, 6, 0),
        ("CLD", 6, 1, 2, 0),
        ("CMP", 3, 3, 4, 1),
        ("NOP", 6, 1, 2, 0),
        ("DCP", 3, 0, 7, 0),
        ("NOP", 2, 3, 4, 1),
        ("CMP", 2, 3, 4, 1),
        ("DEC", 2, 3, 7, 0),
        ("DCP", 2, 0, 7, 0),
        ("CPX", 5, 2, 2, 0),
        ("SBC", 7, 2, 6, 0),
        ("NOP", 5, 0, 2, 0),
        ("ISC", 7, 0, 8, 0),
        ("CPX", 11, 2, 3, 0),
        ("SBC", 11, 2, 3, 0),
        ("INC", 11, 2, 5, 0),
        ("ISC", 11, 0, 5, 0),
        ("INX", 6, 1, 2, 0),
        ("SBC", 5, 2, 2, 0),
        ("NOP", 6, 1, 2, 0),
        ("SBC", 5, 0, 2, 0),
        ("CPX", 1, 3, 4, 0),
        ("SBC", 1, 3, 4, 0),
        ("INC", 1, 3, 6, 0),
        ("ISC", 1, 0, 6, 0),
        ("BEQ", 10, 2, 2, 1),
        ("SBC", 9, 2, 5, 1),
        ("KIL", 6, 0, 2, 0),
        ("ISC", 9, 0, 8, 0),
        ("NOP", 12, 2, 4, 0),
        ("SBC", 12, 2, 4, 0),
        ("INC", 12, 2, 6, 0),
        ("ISC", 12, 0, 6, 0),
        ("SED", 6, 1, 2, 0),
        ("SBC", 3, 3, 4, 1),
        ("NOP", 6, 1, 2, 0),
        ("ISC", 3, 0, 7, 0),
        ("NOP", 2, 3, 4, 1),
        ("SBC", 2, 3, 4, 1),
        ("INC", 2, 3, 7, 0),
        ("ISC", 2, 0, 7, 0),
]))

def pages_differ(a, b):
    """Test if two addresses reference different pages."""
	return a & 0xff00 != b & 0xff00

class CPU(object):

	pc = 0 # program counter
	sp = 0 # stack pointer
	a = 0 # accumulator
	x = 0 # x register
	y = 0 # y register

	interrupt = Interrupt.NONE # interrupt type to perform
    cycles = 0 # cycle count (for platform timing quirks)
	stall = 0 # number of cycles to stall

    def __init__(self):
        # instruction (+ data) queue
        self.memory = bytearray()
        # instruction function table
        #self.table = self._create_table()
        self.reset()

    #def _create_table(self)
    #    return list(getattr(self, op.name.lower()) for op in instructions)

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

    def reset(self):
        self.pc = cpu.read16(0xfffc)
        self.sp = 0xfd
        self.flags = 0x24

    _instruction_fmt = (
            "{0.PC:4x} {2:02x} {3:02x} {4:02x} {1.name: >32s}" +
            "{0.A:02x} {0.X:02x} {0.Y:02x} {0.flags:02x} {0.A:02x} {5:3d}")

    def print_instruction(self):
        op = instructions[self.read()]
        w = tuple(self.read(offset=i) for i in xrange(op.size))
        w0, w1, w2 = (w + ("", "", ""))[:3]
        logger.debug(self._instruction_fmt.format(
                self, op, w0, w1, w2, (self.cycles * 3) % 341))

    def add_branch_cycles(self, info):
        """Add 1 cycle for taking a branch + 1 if it jumps to a new page."""
        self.cycles += 2 if pages_differ(info.pc, info.address) else 1

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
        self.z = (value == 0)

    def set_n(self, v):
        self.n = ((value & 0x80) != 0)

    def set_zn(self, v):
        self.set_z(v)
        self.set_n(v)

    def trigger_nmi(self):
        self.interrupt = Interrupt.NMI

    def trigger_irq(self):
        if not self.I:
            self.interrupt = Interrupt.IRQ

    def step(self):
        """Execute a CPU instruction."""
        if self.stall > 0:
            self.stall -= 1
            return 1

        cycles = self.cycles

        if self.interrupt == Interrupt.NMI: self.nmi()
        elif self.interrupt == Interrupt.IRQ: self.irq()
        self.interrupt = Interrupt.NONE

        opcode = self.read()
        address = 0
        page_crossed = False
        mode = instructions[opcode].mode

        if mode == Mode.Absolute:
            address = self.read16(offset=1)
        elif mode == Mode.AbsoluteX:
            address = cpu.Read16(cpu.PC+1) + uint16(cpu.X)
            pageCrossed = pages_differ(address-uint16(cpu.X), address)
        elif mode == Mode.AbsoluteY:
            address = cpu.Read16(cpu.PC+1) + uint16(cpu.Y)
            pageCrossed = pages_differ(address-uint16(cpu.Y), address)
        elif mode == Mode.Accumulator:
            address = 0
        elif mode == Mode.Immediate:
            address = cpu.PC + 1
        elif mode == Mode.Implied:
            address = 0
        elif mode == Mode.IndexedIndirect:
            address = cpu.read16bug(uint16(cpu.Read(cpu.PC+1) + cpu.X))
        elif mode == Mode.Indirect:
            address = cpu.read16bug(cpu.Read16(cpu.PC + 1))
        elif mode == Mode.IndirectIndexed:
            address = cpu.read16bug(uint16(cpu.Read(cpu.PC+1))) + uint16(cpu.Y)
            pageCrossed = pages_differ(address-uint16(cpu.Y), address)
        elif mode == Mode.Relative:
            offset := uint16(cpu.Read(cpu.PC + 1))
            if offset < 0x80 {
                address = cpu.PC + 2 + offset
            } else {
                address = cpu.PC + 2 + offset - 0x100
            }
        elif mode == Mode.ZeroPage:
            address = uint16(cpu.Read(cpu.PC + 1))
        elif mode == Mode.ZeroPageX:
            address = uint16(cpu.Read(cpu.PC+1) + cpu.X)
        elif mode == Mode.ZeroPageY:
            address = uint16(cpu.Read(cpu.PC+1) + cpu.Y)
        }

        cpu.PC += uint16(instructionSizes[opcode])
        cpu.Cycles += uint64(instructionCycles[opcode])
        if pageCrossed {
            cpu.Cycles += uint64(instructionPageCycles[opcode])
        }
        info := &stepInfo{address, cpu.PC, mode}
        cpu.table[opcode](info)

        return int(cpu.Cycles - cycles)
}

"""

# NMI - Non-Maskable Interrupt
func (cpu *CPU) nmi() {
	cpu.push16(cpu.PC)
	cpu.php(nil)
	cpu.PC = cpu.Read16(0xFFFA)
	cpu.I = 1
	cpu.Cycles += 7
}

# IRQ - IRQ Interrupt
func (cpu *CPU) irq() {
	cpu.push16(cpu.PC)
	cpu.php(nil)
	cpu.PC = cpu.Read16(0xFFFE)
	cpu.I = 1
	cpu.Cycles += 7
}

# ADC - Add with Carry
func (cpu *CPU) adc(info *stepInfo) {
	a := cpu.A
	b := cpu.Read(info.address)
	c := cpu.C
	cpu.A = a + b + c
	cpu.setZN(cpu.A)
	if int(a)+int(b)+int(c) > 0xFF {
		cpu.C = 1
	} else {
		cpu.C = 0
	}
	if (a^b)&0x80 == 0 && (a^cpu.A)&0x80 != 0 {
		cpu.V = 1
	} else {
		cpu.V = 0
	}
}

# AND - Logical AND
func (cpu *CPU) and(info *stepInfo) {
	cpu.A = cpu.A & cpu.Read(info.address)
	cpu.setZN(cpu.A)
}

# ASL - Arithmetic Shift Left
func (cpu *CPU) asl(info *stepInfo) {
	if info.mode == ADDRMODE.Accumulator {
		cpu.C = (cpu.A >> 7) & 1
		cpu.A <<= 1
		cpu.setZN(cpu.A)
	} else {
		value := cpu.Read(info.address)
		cpu.C = (value >> 7) & 1
		value <<= 1
		cpu.Write(info.address, value)
		cpu.setZN(value)
	}
}

# BCC - Branch if Carry Clear
func (cpu *CPU) bcc(info *stepInfo) {
	if cpu.C == 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BCS - Branch if Carry Set
func (cpu *CPU) bcs(info *stepInfo) {
	if cpu.C != 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BEQ - Branch if Equal
func (cpu *CPU) beq(info *stepInfo) {
	if cpu.Z != 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BIT - Bit Test
func (cpu *CPU) bit(info *stepInfo) {
	value := cpu.Read(info.address)
	cpu.V = (value >> 6) & 1
	cpu.setZ(value & cpu.A)
	cpu.setN(value)
}

# BMI - Branch if Minus
func (cpu *CPU) bmi(info *stepInfo) {
	if cpu.N != 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BNE - Branch if Not Equal
func (cpu *CPU) bne(info *stepInfo) {
	if cpu.Z == 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BPL - Branch if Positive
func (cpu *CPU) bpl(info *stepInfo) {
	if cpu.N == 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BRK - Force Interrupt
func (cpu *CPU) brk(info *stepInfo) {
	cpu.push16(cpu.PC)
	cpu.php(info)
	cpu.sei(info)
	cpu.PC = cpu.Read16(0xFFFE)
}

# BVC - Branch if Overflow Clear
func (cpu *CPU) bvc(info *stepInfo) {
	if cpu.V == 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# BVS - Branch if Overflow Set
func (cpu *CPU) bvs(info *stepInfo) {
	if cpu.V != 0 {
		cpu.PC = info.address
		cpu.addBranchCycles(info)
	}
}

# CLC - Clear Carry Flag
func (cpu *CPU) clc(info *stepInfo) {
	cpu.C = 0
}

# CLD - Clear Decimal Mode
func (cpu *CPU) cld(info *stepInfo) {
	cpu.D = 0
}

# CLI - Clear Interrupt Disable
func (cpu *CPU) cli(info *stepInfo) {
	cpu.I = 0
}

# CLV - Clear Overflow Flag
func (cpu *CPU) clv(info *stepInfo) {
	cpu.V = 0
}

# CMP - Compare
func (cpu *CPU) cmp(info *stepInfo) {
	value := cpu.Read(info.address)
	cpu.compare(cpu.A, value)
}

# CPX - Compare X Register
func (cpu *CPU) cpx(info *stepInfo) {
	value := cpu.Read(info.address)
	cpu.compare(cpu.X, value)
}

# CPY - Compare Y Register
func (cpu *CPU) cpy(info *stepInfo) {
	value := cpu.Read(info.address)
	cpu.compare(cpu.Y, value)
}

# DEC - Decrement Memory
func (cpu *CPU) dec(info *stepInfo) {
	value := cpu.Read(info.address) - 1
	cpu.Write(info.address, value)
	cpu.setZN(value)
}

# DEX - Decrement X Register
func (cpu *CPU) dex(info *stepInfo) {
	cpu.X--
	cpu.setZN(cpu.X)
}

# DEY - Decrement Y Register
func (cpu *CPU) dey(info *stepInfo) {
	cpu.Y--
	cpu.setZN(cpu.Y)
}

# EOR - Exclusive OR
func (cpu *CPU) eor(info *stepInfo) {
	cpu.A = cpu.A ^ cpu.Read(info.address)
	cpu.setZN(cpu.A)
}

# INC - Increment Memory
func (cpu *CPU) inc(info *stepInfo) {
	value := cpu.Read(info.address) + 1
	cpu.Write(info.address, value)
	cpu.setZN(value)
}

# INX - Increment X Register
func (cpu *CPU) inx(info *stepInfo) {
	cpu.X++
	cpu.setZN(cpu.X)
}

# INY - Increment Y Register
func (cpu *CPU) iny(info *stepInfo) {
	cpu.Y++
	cpu.setZN(cpu.Y)
}

# JMP - Jump
func (cpu *CPU) jmp(info *stepInfo) {
	cpu.PC = info.address
}

# JSR - Jump to Subroutine
func (cpu *CPU) jsr(info *stepInfo) {
	cpu.push16(cpu.PC - 1)
	cpu.PC = info.address
}

# LDA - Load Accumulator
func (cpu *CPU) lda(info *stepInfo) {
	cpu.A = cpu.Read(info.address)
	cpu.setZN(cpu.A)
}

# LDX - Load X Register
func (cpu *CPU) ldx(info *stepInfo) {
	cpu.X = cpu.Read(info.address)
	cpu.setZN(cpu.X)
}

# LDY - Load Y Register
func (cpu *CPU) ldy(info *stepInfo) {
	cpu.Y = cpu.Read(info.address)
	cpu.setZN(cpu.Y)
}

# LSR - Logical Shift Right
func (cpu *CPU) lsr(info *stepInfo) {
	if info.mode == ADDRMODE.Accumulator {
		cpu.C = cpu.A & 1
		cpu.A >>= 1
		cpu.setZN(cpu.A)
	} else {
		value := cpu.Read(info.address)
		cpu.C = value & 1
		value >>= 1
		cpu.Write(info.address, value)
		cpu.setZN(value)
	}
}

# NOP - No Operation
func (cpu *CPU) nop(info *stepInfo) {
}

# ORA - Logical Inclusive OR
func (cpu *CPU) ora(info *stepInfo) {
	cpu.A = cpu.A | cpu.Read(info.address)
	cpu.setZN(cpu.A)
}

# PHA - Push Accumulator
func (cpu *CPU) pha(info *stepInfo) {
	cpu.push(cpu.A)
}

# PHP - Push Processor Status
func (cpu *CPU) php(info *stepInfo) {
	cpu.push(cpu.Flags() | 0x10)
}

# PLA - Pull Accumulator
func (cpu *CPU) pla(info *stepInfo) {
	cpu.A = cpu.pull()
	cpu.setZN(cpu.A)
}

# PLP - Pull Processor Status
func (cpu *CPU) plp(info *stepInfo) {
	cpu.SetFlags(cpu.pull()&0xEF | 0x20)
}

# ROL - Rotate Left
func (cpu *CPU) rol(info *stepInfo) {
	if info.mode == ADDRMODE.Accumulator {
		c := cpu.C
		cpu.C = (cpu.A >> 7) & 1
		cpu.A = (cpu.A << 1) | c
		cpu.setZN(cpu.A)
	} else {
		c := cpu.C
		value := cpu.Read(info.address)
		cpu.C = (value >> 7) & 1
		value = (value << 1) | c
		cpu.Write(info.address, value)
		cpu.setZN(value)
	}
}

# ROR - Rotate Right
func (cpu *CPU) ror(info *stepInfo) {
	if info.mode == ADDRMODE.Accumulator {
		c := cpu.C
		cpu.C = cpu.A & 1
		cpu.A = (cpu.A >> 1) | (c << 7)
		cpu.setZN(cpu.A)
	} else {
		c := cpu.C
		value := cpu.Read(info.address)
		cpu.C = value & 1
		value = (value >> 1) | (c << 7)
		cpu.Write(info.address, value)
		cpu.setZN(value)
	}
}

# RTI - Return from Interrupt
func (cpu *CPU) rti(info *stepInfo) {
	cpu.SetFlags(cpu.pull()&0xEF | 0x20)
	cpu.PC = cpu.pull16()
}

# RTS - Return from Subroutine
func (cpu *CPU) rts(info *stepInfo) {
	cpu.PC = cpu.pull16() + 1
}

# SBC - Subtract with Carry
func (cpu *CPU) sbc(info *stepInfo) {
	a := cpu.A
	b := cpu.Read(info.address)
	c := cpu.C
	cpu.A = a - b - (1 - c)
	cpu.setZN(cpu.A)
	if int(a)-int(b)-int(1-c) >= 0 {
		cpu.C = 1
	} else {
		cpu.C = 0
	}
	if (a^b)&0x80 != 0 && (a^cpu.A)&0x80 != 0 {
		cpu.V = 1
	} else {
		cpu.V = 0
	}
}

# SEC - Set Carry Flag
func (cpu *CPU) sec(info *stepInfo) {
	cpu.C = 1
}

# SED - Set Decimal Flag
func (cpu *CPU) sed(info *stepInfo) {
	cpu.D = 1
}

# SEI - Set Interrupt Disable
func (cpu *CPU) sei(info *stepInfo) {
	cpu.I = 1
}

# STA - Store Accumulator
func (cpu *CPU) sta(info *stepInfo) {
	cpu.Write(info.address, cpu.A)
}

# STX - Store X Register
func (cpu *CPU) stx(info *stepInfo) {
	cpu.Write(info.address, cpu.X)
}

# STY - Store Y Register
func (cpu *CPU) sty(info *stepInfo) {
	cpu.Write(info.address, cpu.Y)
}

# TAX - Transfer Accumulator to X
func (cpu *CPU) tax(info *stepInfo) {
	cpu.X = cpu.A
	cpu.setZN(cpu.X)
}

# TAY - Transfer Accumulator to Y
func (cpu *CPU) tay(info *stepInfo) {
	cpu.Y = cpu.A
	cpu.setZN(cpu.Y)
}

# TSX - Transfer Stack Pointer to X
func (cpu *CPU) tsx(info *stepInfo) {
	cpu.X = cpu.SP
	cpu.setZN(cpu.X)
}

# TXA - Transfer X to Accumulator
func (cpu *CPU) txa(info *stepInfo) {
	cpu.A = cpu.X
	cpu.setZN(cpu.A)
}

# TXS - Transfer X to Stack Pointer
func (cpu *CPU) txs(info *stepInfo) {
	cpu.SP = cpu.X
}

# TYA - Transfer Y to Accumulator
func (cpu *CPU) tya(info *stepInfo) {
	cpu.A = cpu.Y
	cpu.setZN(cpu.A)
}

"""
    # illegal opcodes --
    def ahx(self, info): pass
    def alr(self, info): pass
    def anc(self, info): pass
    def arr(self, info): pass
    def axs(self, info): pass
    def dcp(self, info): pass
    def isc(self, info): pass
    def kil(self, info): pass
    def las(self, info): pass
    def lax(self, info): pass
    def rla(self, info): pass
    def rra(self, info): pass
    def sax(self, info): pass
    def shx(self, info): pass
    def shy(self, info): pass
    def slo(self, info): pass
    def sre(self, info): pass
    def tas(self, info): pass
    def xaa(self, info): pass
