import logging
logging.basicConfig(level=logging.DEBUG)

from nes.console import Cartridge, Console

cartridge = Cartridge.fromfile("loz.nes")
console = Console.load(cartridge)
console.reset()
console.step()
