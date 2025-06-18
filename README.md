# Snake-Xenzia-Game-in-Assembly-Nasm
Snake Xenzia is a classic Snake game implementation written in x86 assembly language for DOS environments. This nostalgic game brings back the simple yet addictive gameplay of the original Snake, where players control a growing snake to collect food while avoiding collisions with walls and itself.
# Description
Snake Xenzia is a classic Snake game implementation written in x86 assembly language for DOS environments. This nostalgic game brings back the simple yet addictive gameplay of the original Snake, where players control a growing snake to collect food while avoiding collisions with walls and itself.

# Features
- Traditional Snake Gameplay: Move the snake using arrow keys to eat food and grow longer
- Score Tracking: Real-time score display that increases as you collect food
- Collision Detection: Game ends if snake hits walls or itself
- Smooth Movement: Optimized snake movement with proper delay timing
- Text-based Graphics: Uses DOS text mode with colored characters for display
- Random Food Generation: Food appears at random locations on the playfield

# Technical Details
Written in x86 Assembly Language
- Designed for DOS environment (can run in DOSBox or similar emulators)
- Uses BIOS interrupts for keyboard input and screen manipulation
- Implements efficient memory management for snake body tracking
- Includes collision detection algorithms

# How to Play
Run the game in a DOS environment or emulator like DOSBox
Use arrow keys (↑ ↓ ← →) to control the snake's direction
Eat the food (displayed as ♥) to grow longer and increase your score
Avoid hitting the walls or your own tail
The game ends when you collide with a wall or yourself

# Requirements
DOS environment (or DOSBox emulator)
x86 compatible processor
Minimum 640KB RAM (standard DOS requirement)
# Installation
Assemble the code using NASM or similar x86 assembler:
# text:
-- write nasm snake.asm -o snake.com
-- Run the resulting .com file in DOS/DOSBox

# Controls
- Up Arrow: Move snake upward
- Down Arrow: Move snake downward
- Left Arrow: Move snake left
- Right Arrow: Move snake right
- ESC: Exit game

# Contributing
Contributions are welcome! Feel free to fork the repository and submit pull requests with improvements or new features.
