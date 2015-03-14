# Status Report 05/31/2011 #
(Accidentally missed)

# Status Report 05/24/2011 #

  1. what were your concrete goals for the week?
    * Implement rooms and room linking (aka portals)
    * Implement gravity for the player
      * Ideally direction would be determined by a property of the current room
    * Implement player health, death, and make the monster attack hurt instead of disconnect the player.
    * Help Michael with the octree, possibly
  1. what goals were you able to accomplish?
    * Started work on rooms and portals.
      * Wrote a mechanism for edge-triggered behavior
      * Wrote a room class and was able to get clients placed in it when they connect
      * Still need to do a bunch of work to get collisions and collision queries on a per-room basis
      * Client side or network protocol work needs to be done to handle changing the current level geometry (this is probably not too difficult, but I need to talk to Robert about it first)
    * Implemented a really crappy form of gravity and jumping
      * Jumping is disabled though while we rethink motion (it's rather annoying to stick to the ceiling when you're testing other things...)
      * Gravity has you fall at a constant speed, rather than accelerate
  1. if there were goals you were unable to meet, what were the reasons?
    * There is a lot of tricky work that still needs to be done in room/portal handling and basic player motion physics, so I haven't gotten to health, attacks, and other game scripting.
  1. what are your specific goals for the next week?
    * Finish my existing goals
    * More gameplay scripting for specific monsters, the ghosts, and other game elements
  1. what is your individual morale?
    * Very tired, but I have a pretty clear idea of what still needs to be done

# Status Report 05/17/2011 #

  1. what were your concrete goals for the week?
    * Improvements to input system
      * Dead-zone for xbox controller
      * Signaling of other buttons, triggers on controller
      * Customizable keyboard controls (through code)
    * Generalize my resource tracker so we can reuse it for other types of resources
      * Probably can do this pretty quickly with a macro or two
    * Refactor the message system
      * We need a more coherent processing scheme on the client
      * Need an easier way to define messages
      * Need help from Elliott on this
    * Simplify entity movement (split this with Michael)
      * Probably need at least the following:
        * (move entity x y z)
          * Simple wrapper to move by relative offset (x, y, z)
          * Should be a 5 minute fix
        * (walk entity vector)
          * Update desired movement velocity based on input vector
          * This will have to work with physics step and object's properties to work properly
        * (jump entity)
        * (move-towards entity point velocity)
          * move entity towards point at velocity (naively)
        * (simple-physics-step entity)
          * This should update position based on velocity, gravity, forces, etc.
          * Call it in the update method of an object that should fall, not walk through walls, etc.
      * Maybe some higher-level AI movement commands, if there's time?
        * (navigate-towards ...) -- move towards goal, avoid obstacles
        * (chase ...) -- follow an entity; naive version already written, just need to extract pattern from my monster
        * (wander ...) -- move randomly within some bounding volume
    * Start working on playing sounds, and get a background song to play
  1. what goals were you able to accomplish?
    * Dead-zone for controller implemented
    * Slightly simpler keyboard configuration for existing key handling
    * Elliott changed the message protocol -- it's much easier to create and handle messages now
    * Factored out some of the monster logic for chase, which might be reusable eventually
    * Started a standard-physics-step where we move objects by velocity, and used it for the player.
  1. if there were goals you were unable to meet, what were the reasons?
    * Elliott's taking on sound instead, so I did not work on that
    * Didn't make as much progress as I'd like with basic game scripting/physics -- tried to help Robert out with static collisions, but I don't understand the low-level details of the approach he's using.
  1. what are your specific goals for the next week?
    * Implement rooms and room linking (aka portals)
    * Implement gravity for the player
      * Ideally direction would be determined by a property of the current room
    * Implement player health, death, and make the monster attack hurt instead of disconnect the player.
    * Help Michael with the octree, possibly
  1. what is your individual morale?
    * Other parts of my life have made the last few days kinda rough, but I'm still feeling ok about the game engine.

# Status Report 05/10/2011 #

  1. what were your concrete goals for the week?
    * Refactor server.lisp
    * Fix entity removal
    * Integrate some basic physics w/ Michael
    * Script a simple monster and a simple player attack
  1. what goals were you able to accomplish?
    * Some cleanup of server.lisp
    * Entity removal works
    * Wrote a resource tracker for 3d models
    * Basic collision testing works (with the crappy n^2 algorithm) and multimethods are called for objects that collide
    * Wrote a simple monster that chases a player if it's within it's eyesight range and disconnects you (and everyone else <sup>_</sup>) when it catches you
  1. if there were goals you were unable to meet, what were the reasons?
    * Didn't have time to try scripting an attack/health experiment
  1. what are your specific goals for the next week?
    * Improvements to input system
      * Dead-zone for xbox controller
      * Signaling of other buttons, triggers on controller
      * Customizable keyboard controls (through code)
    * Generalize my resource tracker so we can reuse it for other types of resources
      * Probably can do this pretty quickly with a macro or two
    * Refactor the message system
      * We need a more coherent processing scheme on the client
      * Need an easier way to define messages
      * Need help from Elliott on this
    * Simplify entity movement (split this with Michael)
      * Probably need at least the following:
        * (move entity x y z)
          * Simple wrapper to move by relative offset (x, y, z)
          * Should be a 5 minute fix
        * (walk entity vector)
          * Update desired movement velocity based on input vector
          * This will have to work with physics step and object's properties to work properly
        * (jump entity)
        * (move-towards entity point velocity)
          * move entity towards point at velocity (naively)
        * (simple-physics-step entity)
          * This should update position based on velocity, gravity, forces, etc.
          * Call it in the update method of an object that should fall, not walk through walls, etc.
      * Maybe some higher-level AI movement commands, if there's time?
        * (navigate-towards ...) -- move towards goal, avoid obstacles
        * (chase ...) -- follow an entity; naive version already written, just need to extract pattern from my monster
        * (wander ...) -- move randomly within some bounding volume
    * Start working on playing sounds, and get a background song to play
  1. what is your individual morale?
    * Tired and somewhat nervous about remaining time.

# Status Report 05/03/2011 #

  1. what were your concrete goals for the week?
    * Make player position signalable
      * Send input across the network
      * Send player positions back
    * Script a game object that performs an action periodically
      * Requires implementing timers
      * allows for testing of networking
    * Integrate graphics, networking, input in a small moving block demo, if possible
  1. what goals were you able to accomplish?
    * All goals met
    * Also, cameras track correct players!
  1. if there were goals you were unable to meet, what were the reasons?
    * N/A
  1. what are your specific goals for the next week?
    * Refactor server.lisp
    * Fix entity removal
    * Integrate some basic physics w/ Michael
    * Script a simple monster and a simple player attack
  1. what is your individual morale?
    * Good

# Status Report 04/26/2011 #

  1. what were your concrete goals for the week?
    * Finish reading Practical Common Lisp
    * Get usocket working, and implement a basic position signaler
    * Clean up input code
    * Schedule some pair programming time
  1. what goals were you able to accomplish?
    * Read or skimmed most of Practical Common Lisp
    * Wrote some basic input documentation
    * Traded roles with Elliott, since he knows the low-level details of dealing with the lisp compilers better than I do
    * Wrote a server program that waits for players to join then enters a main loop, and a client program that connects to the server and sends it "Hello, World!"
  1. if there were goals you were unable to meet, what were the reasons?
    * Can't signal player position yet, though we will likely have this working very soon.
  1. what are your specific goals for the next week?
    * Make player position signalable
      * Send input across the network
      * Send player positions back
    * Script a game object that performs an action periodically
      * Requires implementing timers
      * allows for testing of networking
    * Integrate graphics, networking, input in a small moving block demo, if possible
  1. what is your individual morale?
    * Decent

# Status Report 04/19/2011 #

  1. what were your concrete goals for the week?
    * Write a better input interface and document it
    * Player motion w/ keyboard & XBox controller
    * Basic networking, signaling of player position
  1. what goals were you able to accomplish?
    * Keyboard and XBox can both be used as inputs
    * Read some of the usocket documentation, but I'm not sure how to use it yet
  1. if there were goals you were unable to meet, what were the reasons?
    * I got busy with family commitments over the weekend, so I haven't gotten a chance to work on cleaning up my input code yet
    * Not sure how to get the usocket examples to run in our current environment. (Could use some help from Elliott on this next week)
  1. what are your specific goals for the next week?
    * Finish reading Practical Common Lisp
    * Get usocket working, and implement a basic position signaler
    * Clean up input code
    * Schedule some pair programming time w/ Elliott & Michael to hack together a common framework for handling state between networking, physics, and scripting. (I think this is going to be a major road-block if we don't agree on this within the next week)
  1. what is your individual morale?
    * Somewhat concerned about progress and still feeling uncomfortable with Lisp.


# Status Report 04/12/2011 #

  1. what were your concrete goals for the week?
    * Figure out how to get the XBox 360 controller to interface with Lisp
    * Move around in first person view with 360 controller
  1. what goals were you able to accomplish?
    * Got the XBox 360 controller to work successfully (if somewhat crudely)
  1. if there were goals you were unable to meet, what were the reasons?
    * Robert is still working on basic graphics support for the camera, and I don't know enough of the theory at that level to help him debug it; so, we didn't get the first person view working yet.
  1. what are your specific goals for the next week?
    * Improve XBox interface, add documentation
    * Player motion with both keyboard and XBox controller
    * Basic status messaging of player position and orientation between two games
    * Work with Elliott and Michael to work out a clearer model for the top-level of the game logic
  1. what is your individual morale?
    * I'm doing ok