# GBPong
A pong clone written in assembly for the Nintendo Game Boy. This was largely written in my spare time, and this was my first time working with an assembly language and this particular workflow. Definitely don't expect particularly good practices here.

You can get the Game Boy .rom file through the releases. Alternatively, you can clone the repository and build from source using RGBDS. See https://rgbds.gbdev.io/ for installation instructions and the newest release. This project runs on RGBDS v0.5.2, but should compile on later versions as well unless something drastic changes.

Once RGBDS is installed, the included makefile should build the ROM properly (you may need to make some directories though). It also outputs a .map and .sym file for debugging with BGB, not that you'd need that.

The ROM should run on a real Game Boy just fine, if you have a flashcart. For emulation, I recommend Sameboy or BGB. The latest version of either will run the game just fine. I'm not familiar with Sameboy and link cable functionality, but I have tested multiplayer on BGB. I have not yet tested multiplayer on real hardware.

Some notes:

- The documentation is pretty messy, and there's a very long comment block on the serial communication protocol. I left that block in the hopes it makes the following code easier to understand, though I'm not sure if it actually does.
- From the title screen, hit any button to go to the main menu. Solo will play a game versus yourself (you move both paddles). Duel will prompt you for a serial connection, and the game will start once it finds another Game Boy (or emulator) trying to run a Duel game as well. The options screen is just a hard reset at the moment.
- Behind each paddle are three targets, each with three hit points. They get darker as they take more damage. Once they're black, they're destroyed. Destroy all three of your opponent's targets to win. On a win, the Game Boy will reset.
- The serial communication is still quite buggy, mostly at startup. If, after the match starts, one or both Game Boys freeze or desync, just reset both consoles and try again. It usually doesn't take more than a couple tries to get it working. (There's a problem with how the Game Boys start communication with a "handshake" that isn't resolved yet.)
- This project is not being actively maintained.
