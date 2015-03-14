# Project Description #
## What kind of game are you planning to build? ##
an awesome one...
A Third-person shooter/platformer...in SPACE!
## What are the goals of the game, how do players win, how do they lose? ##
The players are divided into two teams: the humans and the ghosts. The humans win by reaching the 'treasure'. The ghosts win by killing the humans.  You lose if the other team wins :)
## What are the interesting or unique aspects to your game? ##
  * Written in lisp
  * Areas with gravity, without gravity
  * Space Ghosts
  * Platformer with guns
  * Improved salty flavor
## List of Features ##
### Must Have ###
  * Graphics
    * skybox
    * animations
    * simple particle effects
    * minimap
  * Game Engine
    * 3rd person camera
    * health
    * ray intersections for guns
    * melee combat
    * ground controls
    * flying controls (maybe in nice-to-have)
    * platforms
  * physics
    * gravity + ability to turn off
    * collision + fun
  * Input
    * SDL working with XBox controller
    * good camera control
  * Sound
    * music
    * sound effects
  * Networking
  * <other stuff here>
### Would Be Really Nice ###
  * Graphics
    * Shadows
    * Normal maps
    * post-processing effects
      * HDR + tone-mapping
    * proceedural textures
  * Game Engine
  * Physics
  * Input
  * Sound
  * Networking

### Cool But Only If Ahead Of Schedule ###
  * Graphics
    * motion blur
  * Game Engine
  * Physics
  * Input
  * Sound
    * Event-based dialog
  * Networking

# Group Management #
## What are the major roles in your group's management? ##

## How will decisions be made? By leader, consensus? ##
By consensus. Failing consensus, democratic vote. Failing democracy, magic-eight ball

## How will you communicate? Email, meetings in the lab, discussion board? ##
Google group mailing list

In person in lab

## How will you know when you're off schedule, and how will you deal with schedule slips? ##
We'll know we're off schedule when we're behind milestones (and have not revised them :P )

## Who will produce the weekly group status reports? ##
Everyone will together, or taking turns when not.

# Project Development #
## What are the development roles and who will handle them? ##
| |**Graphics**|**Game Logic / Gameplay**|**Physics**|**Art Assets**|**Sound**|**Networking**|**Input**|
|:|:-----------|:------------------------|:----------|:-------------|:--------|:-------------|:--------|
|Primary| Robert | Chris, Michael | Michael | TBD | Chris | Elliott | Chris |
|Secondary| William | Elliott |  | Robert, William | Elliott | Chris | Elliott |

## What tools will you use? ##
  * Emacs w/ SLIME
  * Clozure Lisp compiler
  * 3DS Max or Maya
  * Mercurial
  * Subversion
## How will you do testing? ##
Unit testing where applicable

Frequent in-game testing.

## How will you do documentation (both internal group documentation as well as external player documentation)? ##
> All exported functions, macros and classes should have documentation to aid other group members in using the package. These will also be on the wiki. Group members should take care to also comment difficult code.  William will draw pictures for players. What else do they need?

# Project Schedule #
## Integration Milestones ##
### Milestone 1 ###
end of week 2
  * First person control with 360 controller with simple models/lighting

### Milestone 2 ###
end of week 3
  * 3rd person control, basic networking, basic models import and per-pixel lighting, some physics (probably collisions)

### Milestone 3 ###
end of week 4
  * game scripting, hopefully some real models, graphics

### Milestone 4 ###
beginning week 5
  * Game logic and Level Design, Animations?
  * gameplay testing
  * user interface

### Design Freeze ###
week 8

### Milestone 5 ###
end of quarter
  * Game is awesome