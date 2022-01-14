## Sprites

- https://tcrf.net/The_3-D_Battles_of_World_Runner
  - As I watch [this video](https://www.youtube.com/watch?v=jTbH25YNxE4) frame by frame, I notice that each square is some proportion to the next one behind it. Everything takes 8 frames. Starting from the very back:
    - A single pixel high, all 8 frames to move down one pixel
    - ~2 pixels high, takes 4 frames to move down one pixel (twice)
    - ~3 pixels high, takes 2 frames to move down one pixel (four times)
    - ~5 pixels high, moves each frame
    - ~12 pixels high
    - ~24 pixels high
  - The squares as they get larger expand throughout the 8 frames of change
  - Maybe there are two palette sets for the ground, each with 8 L/R grounds that change depending on the X position of the guy, modulo'd to 8.
- Space harrier used multiple floors and cycled them: https://www.spriters-resource.com/nes/spaceharrier/sheet/167250/
  - Three sprite cycles imply motion
  - What about this but with some kind of math to shift LR?


## Hypothesis

- 8 distinct background floor sprites

## PLAN

Store six 64 x 21 floors in two pages. Pull in each chunk for each animation step
