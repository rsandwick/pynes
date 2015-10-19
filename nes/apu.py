import collections
import logging
logger = logging.getLogger("nes.apu")

from .cpu import CPUFREQ
from .util import Record

FRAMECOUNTERRATE = CPUFREQ / 240.0
SAMPLERATE = CPUFREQ / 44100.0 / 2

length = [
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
    12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
]

duty = [
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
]

triangle = range(15, -1, -1) + range(16)

noise = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
]

dmc = [
    214, 190, 170, 160, 143, 127, 113, 107, 95, 80, 71, 64, 53, 42, 36, 27,
]

pulse = list(95.52 / (8128.0 / (i + 100)) for i in xrange(31))
tnd = list(163.67 / (24329.0 / (i + 100)) for i in xrange(203))


class APU(object):

    channel = None
    _pulse1 = None
    _pulse2 = None
    _triangle = None
    _noise = None
    _dmc = None
    _cycle = 0
    _frame_period = 0
    _frame_value = 0
    _frame_irq = False

    def __init__(self, console):
        self._console = console
        self._pulse1 = Pulse()
        self._pulse2 = Pulse()
        self._triangle = Triangle()
        self._noise = Noise()
        self._dmc = DMC()
        self._noise.shift_register = 1
        self._pulse1.channel = 1
        self._pulse2.channel = 2
        self._dmc.cpu = console.cpu

    @property
    def _drivers(self):
        return (self._pulse1,
                self._pulse2,
                self._triangle,
                self._noise,
                self._dmc)

    def step(self):
        cycle1 = self.cycle
        self.cycle += 1
        cycle2 = self.cycle
        self.step_timer()
        f1, f2 = (float(c) / FRAMECOUNTERRATE for c in (cycle1, cycle2))
        if f1 != f2:
            self.step_frame_counter()
        s1, s2 = (float(c) / SAMPLERATE for c in (cycle1, cycle2))
        if s1 != s2:
            self.send_sample()

    def send_sample(self):
        #TODO: what does this channel stuff do?
        self.channel.send(self.output())

    def output(self):
        outputs = list(driver.output() for driver in self._drivers)
        return pulse[sum(outputs[:2])] + tnd[sum(outputs[2:])]

    def _step_frame_counter(self):
        # mode 0:    mode 1:       function
        # ---------  -----------  -----------------------------
        #  - - - f    - - - - -    IRQ (if bit 6 is clear)
        #  - l - l    l - l - -    Length counter and sweep
        #  e e e e    e e e e -    Envelope and linear counter
        if self.frame_period == 4:
            self.frame_value = (self.frame_value + 1) & 3
            self.step_envelope()
            if self.frame_value & 1:
                self.step_sweep()
                self.step_length()
            if self.frame_value == 3:
                self.fire_irq()
        elif self.frame_period == 5:
            self.frame_value = (self.frame_value + 1) % 5
            self.step_envelope()
            if self.frame_value in (0, 2):
                self.step_sweep()
                self.step_length()

    def step_timer(self):
        if not self.cycle & 1:
            self._pulse1.step_timer()
            self._pulse2.step_timer()
            self._noise.step_timer()
            self._dmc.step_timer()
        self._triangle.step_timer()

    def step_envelope(self):
        self._pulse1.step_envelope()
        self._pulse2.step_envelope()
        self._triangle.step_counter()
        self._noise.step_envelope()

    def step_sweep(self):
        self._pulse1.step_sweep()
        self._pulse2.step_sweep()

    def step_length(self):
        self._pulse1.step_length()
        self._pulse2.step_length()
        self._triangle.step_length()
        self._dmc.step_length()

    def fire_irq(self):
        if self._frame_irq:
            self._console.cpu.trigger_irq()

    def read_register(self, addr):
        if addr == 0x4015:
            return self.read_status()
        #else:
        #    raise MemoryError(
        #        "unhandled apu register read at address: 0x%04x" % (addr,))


    def write_register(addr, v):
        v &= 0xff
        if addr == 0x4000:
            self._pulse1.write_control(v)
        elif addr == 0x4001:
            self._pulse1.write_sweep(v)
        elif addr == 0x4002:
            self._pulse1.write_timer_low(v)
        elif addr == 0x4003:
            self._pulse1.write_timer_high(v)
        elif addr == 0x4004:
            self._pulse2.write_control(v)
        elif addr == 0x4005:
            self._pulse2.write_sweep(v)
        elif addr == 0x4006:
            self._pulse2.write_timer_low(v)
        elif addr == 0x4007:
            self._pulse2.write_timer_high(v)
        elif addr == 0x4008:
            self._triangle.write_control(v)
        elif addr in (0x4009, 0x4010):
            self._dmc.write_control(v)
        elif addr == 0x400a:
            self._triangle.write_timer_low(v)
        elif addr == 0x400b:
            self._triangle.write_timer_high(v)
        elif addr == 0x400c:
            self._noise.write_control(v)
        elif addr in (0x400d, 0x400e):
            self._noise.write_period(v)
        elif addr == 0x400f:
            self._noise.write_length(v)
        elif addr == 0x4011:
            self._dmc.write_v(v)
        elif addr == 0x4012:
            self._dmc.write_address(v)
        elif addr == 0x4013:
            self._dmc.write_length(v)
        elif addr == 0x4015:
            self._write_control(v)
        elif addr == 0x4017:
            self._write_frame_counter(v)
        #else:
        #    raise MemoryError(
        #        "unhandled apu register write at address: 0x%04x" % (addr,))

    def read_status(self):
        return sum(
                ((1 << i) if driver.length_value > 0 else 0)
                for (i, driver) in enumerate(self._drivers))

    def write_control(self, v):
        for i, driver in enumerate(self._drivers):
            driver.enabled = bool(v & (1 << i))
            if not driver.enabled:
                driver.length_value = 0
        if self._dmc.enabled:
            if self._dmc.length_value == 0:
                self._dmc.restart()

    def write_frame_counter(self, v):
        self._frame_period = 4 + ((v >> 7) & 1)
        self._frame_irq = not v & 0x40
        #self._frame_value = 0
        #if self._frame_period == 5:
        #    self._step_envelope()
        #    self._step_sweep()
        #    self._step_length()


class Envelope(Record):
    __slots__ = ("enabled", "period", "value", "loop", "start", "volume")

class Sweep(Record):
    __slots__ = ("enabled", "period", "value", "negate", "reload", "shift")

class Periodic(Record):
    __slots__ = ("period", "value", "reload")

class LengthMixIn(object):

    length_enabled = False
    length_value = 0

    def step_length(self):
        if self.length_enabled and self.length_value > 0:
            self.length_value -= 1

    def write_control(self, v):
        v &= 0xff

        e = self.envelope
        e.enabled = not v & 0x10
        e.loop = bool(v & 0x20)
        e.period = v & 0x0f
        e.start = True

        #TODO: is this right (same value as envelope period)?
        self.constant_volume = v & 0x0f
        #TODO: is this right (negated envelope loop)?
        self.length_enabled = not bool(v & 0x20)


class Pulse(LengthMixIn):

    channel = None
    constant_volume = 0
    duty_mode = 0
    duty_value = 0
    enabled = False
    envelope = Envelope(False, 0, 0, False, False, 0)
    sweep = Sweep(False, 0, 0, False, False, 0)
    timer = Periodic(0, 0, False)

    def write_control(self, v):
        self.duty_mode = (v >> 6) & 0x03
        super(Pulse, self).write_control(v)

    def write_sweep(self, v):
        s = self.sweep
        s.enabled = bool(v & 0x80)
        s.negate = bool(v & 0x08)
        s.period = (v >> 4) & 0x07
        s.reload = True
        s.shift = v & 0x07

    def write_timer_low(self, v):
        self.timer.period = (self.timer.period & 0xff00) | (v & 0xff)

    def write_timer_high(self, v):
        self.length_value = length[v >> 0x03]
        self.timer.period = (self.timer.period & 0xff) | ((v & 0x07) << 8)
        self.envelope.start = True
        self.duty_value = 0

    def step_timer(self):
        t = self.timer
        if t.value <= 0:
            t.value = t.period
            self.duty_value = (self.duty_value + 1) & 0x07
        else:
            t.value -= 1

    def step_envelope(self):
        e = self.envelope
        if e.start:
            e.volume = 0x0f
            e.value = e.period
            e.start = False
        elif e.value > 0:
            e.value -= 1
        else:
            if e.volume > 0:
                e.volume -= 1
            elif e.loop:
                e.volume = 0x0f
            e.value = e.period

    def step_sweep(self):
        s = self.sweep
        if s.reload:
            if s.enabled and s.value == 0:
                self.sweep()
            s.value = s.period
            s.reload = False
        elif s.value > 0:
            s.value -= 1
        else:
            if s.enabled:
                self.sweep()
            s.value = s.period

    def sweep(self):
        s, t = self.sweep, self.timer
        delta = t.period >> s.shift
        if s.negate:
            t.period -= delta
            if self.channel == 1:
                t.period -= 1
        else:
            t.period += delta

    def output(self):
        if ((not self.enabled) or
                (self.length_value == 0) or
                (duty[self.duty_mode][self.duty_value] == 0) or
                (not (8 <= self.timer.period <= 0x7ff))):
            return 0
        e, s, t = self.envelope, self.sweep, self.timer
        #if ((not s.negate) and
        #        (t.period + (t.period >> s.shift)) > 0x07ff):
        #    return 0
        return e.volume if e.enabled else self.constant_volume


class Triangle(LengthMixIn):

    counter = Periodic(0, 0, False)
    duty_value = 0
    enabled = False
    timer = Periodic(0, 0, False)

    def write_control(self, v):
        self.length_enabled = not (value & 0x80)
        self.counter.period = v & 0x7f

    def write_timer_low(self, v):
        self.timer.period = (self.timer.period & 0xff00) | (v & 0xff)

    def write_timer_high(self, v):
        self.length_value = length[v >> 0x03]
        self.timer.period = (self.timer.period & 0xff) | ((v & 0x07) << 8)
        self.timer.value = self.timer.period
        self.counter.reload = True

    def step_timer(self):
        t = self.timer
        if t.value <= 0:
            t.value = t.period
            if self.length_value > 0 and self.counter.value > 0:
                self.duty_value = (self.duty_value + 1) & 0x1f
        else:
            t.value -= 1

    def step_counter(self):
        c = self.counter
        if c.reload:
            c.value = c.period
        elif c.value > 0:
            c.value -= 1
        if self.length_enabled:
            c.reload = False

    def output(self):
        if ((not self.enabled) or
                self.length_value == 0 or
                self.counter.value == 0):
            return 0
        return triangle[self.duty_value]


class Noise(LengthMixIn):

    constant_volume = 0
    enabled = False
    envelope = Envelope(False, 0, 0, False, False, 0)
    mode = False
    shift_register = 0
    timer = Periodic(0, 0, False)

    def write_period(v):
        self.mode = bool(v & 0x80)
        self.timer.period = noise[v & 0x0f]

    def write_length(v):
        v &= 0xff
        self.length_value = length[v >> 3]
        self.envelope.start = True

    def step_timer(self):
        t = self.timer
        if t.value == 0:
            t.value = t.period
            b1 = self.shift_register & 0x01
            b2 = (self.shift_register >> (6 if self.mode else 1)) & 0x01
            self.shift_register >>= 1
            self.shift_register |= (b1 ^ b2) << 14
        else:
            t.value -= 1

    def step_envelope(self):
        e = self.envelope
        if e.start:
            e.volume = 0x0f
            e.value = e.period
            e.start = False
        elif e.value > 0:
            e.value -= 1
        else:
            if e.volume > 0:
                e.volume -= 1
            elif e.loop:
                e.volume = 0x0f
            e.value = e.period

    def output(self):
        if ((not self.enabled) or
                self.length_value == 0 or
                self.shift_register & 0x01):
            return 0
        e = self.envelope
        return e.volume if e.enabled else self.constant_volume


class DMC(object):

    bit_count = 0
    cpu = None
    current_address = 0
    length_value = 0
    enabled = False
    irq = False
    loop = False
    sample_address = 0
    sample_length = 0
    shift_register = 0
    tick = Periodic(0, 0, False)
    value = 0

    def write_control(self, v):
        self.irq = bool(v & 0x80)
        self.loop = bool(v & 0x40)
        self.tick.period = dmc[v & 0x0f]

    def write_value(self, v):
        self.value = v & 0x7f

    def write_address(self, v):
        self.sample_address = 0xc000 | ((v & 0xff) << 6)

    def write_length(self, v):
        self.sample_length = ((v & 0xff) << 4) | 1

    def restart(self):
        self.current_address = self.sample_address
        self.length_value = self.sample_length

    def step_timer(self):
        if not self.enabled:
            return
        self.step_reader()
        t = self.tick
        if t.value <= 0:
            t.value = t.period
            self.step_shifter()
        else:
            t.value -= 1

    def step_reader(self):
        if self.length_value > 0 and self.bit_count == 0:
            self.cpu.stall += 4
            self.shift_register = self.cpu.read(self.current_address)
            self.bit_count = 8
            self.current_address += 1
            if self.current_address == 0:
                self.current_address = 0x8000
            self.length_value -= 1
            if self.length_value == 0 and self.loop:
                self.restart()

    def step_shifter(self):
        if self.bit_count == 0:
            return
        if self.shift_register & 1:
            if self.value <= 125:
                self.value += 2
        else:
            if self.value >= 2:
                self.value -= 2
        self.shift_regitster >>= 1
        self.bit_count -= 1

    def output(self):
        return self.value
