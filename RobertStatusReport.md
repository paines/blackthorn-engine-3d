# Status Report 05/31/2011 #

  1. what were your concrete goals for the week?
> finish EVERYTHING!!1!!11
    * erm,shadow mapping
    * better lighting system / effects
    * help with gameplay integration
    1. what goals were you able to accomplish?
      * shadow mapping is nearly there,
      * we have laser effects.
      * Explosion and Spark type particle systems
      * Deferred lighting 1/2 way there
      * we now load textures
      * Can mount things (ie guns/swords, particle effects) on players
    1. if there were goals you were unable to meet, what were the reasons?
      * EVERYTHING IS NOT FINISHED!!
        * More LISP, Less Sleep!
      * Shadow mapping
        * OpenGL can be hard to debug.  It was trashing my depth maps, but I think I know why now.
    1. what are your specific goals for the next week?
      * no, really, finish everything this time.
        * and by everything i mean get all the lighting/shading effects i want
        * probably have to do other people's work for them too <_<
    1. what is your individual morale?
> > > More sleep, less lisp....and lots less glsl_

# Status Report 05/24/2011 #

  1. what were your concrete goals for the week
    * Finish skinning, if I can't, get 'frames' of models working
    * Implement a better matrix inverter so we can support scaled rooms
    * fix dumb camera
    * maybe bells and whistles
  1. what goals were you able to accomplish?
    * Done skinning
    * Gonna skip on the inverter, we'll be fine w/ hacks
    * Done camera, put in spring mechanic, added collision to not go through walls
    * Particle Engine: DONE! (mostly, will probably need to extend for some specific effects)
    * Efficient billboards: DONE! (same as above)
    * GUI started
    * steps to get shadow mapping in
    * levels animate less dumbly now
  1. if there were goals you were unable to meet, what were the reasons?
    * unmeet-able goals? what?
  1. what are your specific goals for the next week?

> > finish EVERYTHING!!1!!11
    * erm,shadow mapping
    * better lighting system / effects
    * help with gameplay integration
  1. what is your individual morale?
    * /PWNT!
    * OMGWEHAZNOTIEM!

# Status Report 05/17/2011 #

  1. what were your concrete goals for the week?
    * Get skeleton animation and skinning to work
    * Start work on renderer (pipeline, light & material management)
      * deferred lighting, or
      * shadow maps (or both! who needs sleep? :D)
  1. what goals were you able to accomplish?
    * Skeleton's work, skinning is.... ~~problematic~~.... DONE! /update
    * collision with static geometry is nearly there...some kinks to work out
  1. if there were goals you were unable to meet, what were the reasons?
    * I didn't get to working on the bells and whistles because I ended up spending about all my time trying to get the collision and skinning working.
    * ~~The skinning is mostly there, but there are some problems, it seems. and I'm trying to track them down~~ fixed stupid thing...opengl assumes matrices given to it are row-major, but works with column major. i forgot.
  1. what are your specific goals for the next week?
    * Finish skinning, if I can't, get 'frames' of models working
    * Implement a better matrix inverter so we can support scaled rooms
    * fix dumb camera
    * maybe bells and whistles
  1. what is your individual morale?
    * exhausted from constant debugging of collision code and stupid, hard to find issues.  but it works, mostly...so that's good.

# Status Report 05/10/2011 #

  1. what were your concrete goals for the week?
    * Get multiple meshes loaded from one collada file.
      * Start with scene graph
        * Work with Michael on object data structures for physics
        * ie, collide with walls, etc
      * make headway on getting animation into the game
  1. what goals were you able to accomplish?
    * multiple meshes: check
    * scene-graph: half-check
    * basic animation (no skeles/skinning): check
    * collision detection: half-check
  1. if there were goals you were unable to meet, what were the reasons?
    * Scene Graph: We don't need one yet, so it's not fully implemented.  What I did was create a simple node that is put in a tree to manage the transforms and materials for the different meshes.
    * collision detection: I spent a lot of time investigating approaches and spatial structures/algorithms for this. It's kinda hard to find...Eventually decided to go with octrees and started to code one up. Then i realized I could probably use the lisp r-tree library we have lying around for static geometry and put dynamic stuff in a loose octree. So that's where we're going.
  1. what are your specific goals for the next week?
    * Get skeleton animation and skinning to work
    * Start work on renderer (pipeline, light & material management)
      * deferred lighting, or
      * shadow maps (or both! who needs sleep? :D)
    * Maybe help get (static) scene collision detection online
  1. what is your individual morale?
> > is the quarter over yet?? ... wait.. no, it can't be over!! theresstillsomuchlefttodo!!!! IT'S NEVER FINISHED!.
> > Um, I'm doing ok. I think the group needs to meet more. Both to plan and get things done.  We **are** making progress though, steadily.

# Status Report 05/03/2011 #

  1. what were your concrete goals for the week?
    * Integrate meshes and entities
    * Shaders integrated
    * Load multiple meshes from files
    * maybe get vbos to work
  1. what goals were you able to accomplish?
    * I was able to accomplish all of them except the loading multiple meshes.  Additionally, I got the basics for camera movement implemented, as well as some base code for animations (namely, channels and keyframes)

  1. if there were goals you were unable to meet, what were the reasons?
    * I decided to start preparing for rigged animation, and decided I needed to refactor my model loader to make it (hopefully) more flexible.  So we don't yet have multiple meshes per model (need to modify model and mesh structure first)

  1. what are your specific goals for the next week?
    * Get multiple meshes loaded from one collada file.
    * Start with scene graph
      * Work with Michael on object data structures for physics
      * ie, collide with walls, etc
    * make headway on getting animation into the game

  1. what is your individual morale?
    * We're making headway, we have a controllable object and basic server-client code, but there are still big problems that need to be solved, and I'm a little daunted by some of them.

# Status Report 04/26/2011 #

  1. what were your concrete goals for the week?
    * Get 2d textures working, work on object model
  1. what goals were you able to accomplish?
    * We can now load 2d textures and map them to models.
    * I didn't get much work done on the object interface
  1. if there were goals you were unable to meet, what were the reasons?
    * too busy/problems with the texture commit kept me from getting anything else done while away
  1. what are your specific goals for the next week?
> > Get meshes working with objects so we can see things in the game.  Shaders integrated, loading of multiple meshes from one file, and if I'm lucky, I'll manage to get vertex buffers to work.
  1. what is your individual morale?
> > At the moment I'm tired and frustrated at vertex array objects and shaders not working, but not telling me anything useful.  However it's good to see we might be able to use them.

# Status Report 04/19/2011 #

  1. what were your concrete goals for the week?
    * Get simple third person camera implemented
    * Load 3d meshes from collada .dae files
  1. what goals were you able to accomplish?
> > Both, really.
    * The 3rd person camera is far from done, as far as it's final implementation will look, but we can use the controller to orbit around a player, for now.
    * The dae loader was fairly difficult with lots of little nagging things that didn't work or lisp didn't like (stupid lisp).  Nonetheless I am glad i was writing it in lisp instead of C++ (then again, i could use a loader that was already written...)
  1. what are your specific goals for the next week?
> > I will be out of town, so I don't expect to get a lot done.  I would like to get basic 2d textures working.
  1. what is your individual morale?
> > Good.  I'm a little worried at what we'll be able to get done by week 10, given it's week 4 already.  I am happy that the 3rd person camera worked the first time, and I have a good enough grasp on how it's going to work.  Working with cl-opengl is a pain, as it has bugs and debugging the cffi-related calls is a pain.

Pictures:
A cube mesh loaded from file with correct normals, etc
![http://wiki.blackthorn-engine-3d.googlecode.com/hg/rlgrossWeek3_01.jpg](http://wiki.blackthorn-engine-3d.googlecode.com/hg/rlgrossWeek3_01.jpg)

# Status Report 04/12/2011 #

  1. what were your concrete goals for the week?
    * Get a first-person camera working
    * Draw objects using the vertex array interface cl-opengl provides
  1. what goals were you able to accomplish?
> > Both were accomplished
  1. if there were goals you were unable to meet, what were the reasons?
> > The camera took until Friday night to fix because of a matrix construction error
  1. what are your specific goals for the next week?
> > For next week, I want to get basic mesh data loaded from dae files and have a third person camera working.  The priority is to the camera
  1. what is your individual morale?
> > Good.  Sometimes frustrated at lisp as I'm learning the ins-and-outs, but then happy when it makes things easier

Picture time:
A cube drawn using vertex buffer + index array, with a placed and pointed camera
![http://wiki.blackthorn-engine-3d.googlecode.com/hg/rlgrossWeek1_01.png](http://wiki.blackthorn-engine-3d.googlecode.com/hg/rlgrossWeek1_01.png)