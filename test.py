import logging
import os
import pygame
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("test")

from nes.console import Cartridge, Console

cartridge = Cartridge.fromfile("loz.nes")
console = Console.load(cartridge)
console.reset()

os.environ["SDL_VIDEODRIVER"] = "directx"

pygame.display.init()
pygame.display.set_caption("test -- the legend of zelda")

screen = pygame.display.set_mode((256, 240))
screen.fill((0, 0, 0, 0))
pygame.display.flip()
done = False
while not done:
    console.step_seconds(0.1)
    #screen.blit(console.buffer, (0, 0))
    screen.blit(console.ppu.back, (0, 0))
    pygame.display.flip()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            done = True
        elif event.type == pygame.KEYUP:
            if event.key == pygame.K_ESCAPE:
                done = True
