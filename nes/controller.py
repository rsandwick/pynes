from ..util import enum

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
