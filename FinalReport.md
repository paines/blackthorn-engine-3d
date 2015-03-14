# Final Report #

1. In the project review document, start by addressing these main questions:

  * Game concept: How and why did your game concept change from initial concept to what you implemented?
    * At the beginning of the quarter we were more worried about figuring out whether Lisp would work or not that the game design was never quite flushed out.
    * Time constraints forced us to redesign the game heavily toward the end of the quarter.
    * Also, we never really had an official manager. Robert (and maybe other people) thought that Elliott (being the Lisp guru) was going to make things happen. Robert kind of ended up doing everything.
    * At any rate, we got rid of a lot of platformer mechanics (like variable gravity) in favor of more shooter-like mechanics (and complete antigravity). Jumping and physics ended up being too hard to get done within the time frame.
    * We also got rid of the 2v2 hunt for/ defend treasure element and most of the character models that went along with that.
    * The overall level design was also simplified heavily.

  * Design: How does your final project design compare to the initial design, and what are the reasons for the differences, if any?
    * We didn't really have a clear concept of our design going in, and basically said "have at it" to everyone.
    * Some parts of the code stayed pretty stable. The low-level networking API, for example, was remarkably stable. The high-level networking API however changed quite a bit, as we kept looking for better ways to abstract away the network details.
    * The main server loop was one part that got rewritten a lot. There were at least one or two times that we threw away the entire file and rewrote it from scratch.

  * Schedule: How does your final schedule compare with your projected schedule, and what are the reasons for the differences, if any? (You should be able to glean this from your status reports.)
    * We didn't really have a schedule. We certainly didn't follow it if we did XD. We had something of a crunch at the end of the quarter but that's not really surprising.
    * We needed to get to game mechanics earlier in the quarter.


2. Then address these more general questions:

  * What software methodology and group mechanics decisions worked out well, and which ones did not? Why?
    * In general we kept the philosophy that the code should build all the time. Elliott took on the responsibility for actually enforcing that, although it stopped being so much of an issue after people got used to Lisp.
    * Using Mercurial was also nice, since it allowed us to have everyone working on orthogonal changes at the same time without worry too much about conflicts. But subrepos with Google Code and Windows kind of suck, and forced us to manually update a lot when we shouldn't have needed to.

  * Which aspects of the implementation were more difficult than you expected, and which were easier? Why?
    * We had some issues with OpenGL versions and trying to figure out exactly what we could get away with on different machines. Our final solution was to just use version 2 for the demo, although we really wanted to do some stuff with version 3. (Also, some of the OpenGL features weren't properly exposed, which made them difficult to use.)

  * Which aspects of the project are you particularly proud of? Why?
    * The network does surprisingly well in high latency situations. We successfully ran the server on a machine in Texas and played with 3 clients in the labs.
    * Robert is proud that everything works. Everything being the collision system (with swept sphere), fully-skinned animation, particle systems.
    * Will is proud of his models and animations, even the ones that weren't in the final presentation. The Rocket Robot did look good though.

  * What was the most difficult software problem you faced, and how did you overcome it (if you did)?
    * Robert spent a week fixing two bugs in skinning and collisions that were solved by taking showers.

  * If you used an implementation language other than C++, describe the environments, libraries, and tools you used to support development in that language. What issues did you run into when developing in that language? Would you recommend groups use the language in the future? If so, how would you recommend groups best proceed to make it as straightforward as possible to use the language? And what should groups avoid? Finally, how many lines of code did you write for your project? (Do not include code you did not write, such as library source.) Use any convenient mechanism for counting, but state how you counted.
    * If you already know Lisp, then you're already in a position to make the decision whether or not to use it on your own. If you don't, then we don't suggest trying to learn a language while trying to write a game at the same time.
      * That said, Lisp is perfectly capable of supporting a video game, both in terms of performance, and supporting libraries.
    * We used two compilers:
      * [SBCL](http://www.sbcl.org/)
      * [CCL](http://ccl.clozure.com/)
    * SBCL tends to be less stable on Windows, so we included CCL to save ourselves in case we ran into any unsolvable crashes. It turns out that CCL doesn't give as nice error messages though, so we ended up using SBCL for most of our development. We never actually ran into any critical bugs in SBCL.
    * We used the following libraries:
      * [lispbuilder](http://code.google.com/p/lispbuilder/wiki/LispbuilderSDL) - SDL buildings
      * [cl-opengl](http://common-lisp.net/project/cl-opengl/) - OpenGL bindings
      * [userial](http://nklein.com/software/unet/userial/) - serialization
      * [usocket](http://common-lisp.net/project/usocket/) - portable socket bindings
      * [command-line-arguments](http://common-lisp.net/project/qitab/) - for parsing command line arguments
      * [cxml](http://common-lisp.net/project/cxml/) - for parsing XML
      * [spatial-trees](http://www.cliki.net/spatial-trees) - fancy multi-dimension spatial trees
    * We used [quicklisp](http://www.quicklisp.org/) (a package manager for Lisp libraries). **Quicklisp is amazing. Use it.**
    * Elliott wrote the build system with makefiles and shell scripts. If you want to use it just take it. But the code is kind of nasty.
    * Chris wrote some bindings to DirectX to allow us to use Xbox controllers.
    * `find src/blackthorn3d -name '*.lisp' | xargs wc -l` returns 15112. We estimate that about 2500 lines were just in file header comments (where we display the license in each file), so that leaves about 13000 lines.
    * The code has 14966 open and 14974 close parentheses.

  * In developing the media content for your project, you relied upon a number of tools from the DirectX libraries to modeling software. And you likely did some troubleshooting to make it all work. So that students next year can benefit from what you learned, please detail your tool chain for modeling, exporting, and loading meshes, textures, and animations. Be specific about the tools and versions, any non-obvious steps you had to take to make it work (e.g., exporting from the tool in a specific manner), and any features or operations you specifically had to avoid -- in other words, imagine that you were tutoring someone on how to use the toolchain you used to make it all work. Also, for the tools you did use, what is your opinion of them? Would you use them again, or look elsewhere?
    * Robert: Well, we didn't use any DirectX libraries. We decided to use the COLLADA format for exporting/importing.  Since there wasn't an existing COLLADA importer for lisp, and I didn't feel like learning cffi to write my own bindings to a c one, I ended up writing my own importer.  We ended up supporting vertex positions, normals, and one set of texture coords along with skeleton rigging and skinning.  I also support animations and blinn-phong materials exported from 3DS Max and Maya.
    * Robert: The tool chain worked as such: Export model(s) from 3DS Max using OpenCOLLADA 1.5.  Load the COLLADA data into Lisp data.  Convert into either OpenGL format (ie, c arrays) or triangle data for the server.
    * Will: The modeling/texturing/animating process went like this:
      1. Create an idea for a model in my head
      1. Draw it out on paper (as best I could)
      1. Experiment with 3ds Max in hopes of magically learning everything
      1. Watch a billion tutorials on YouTube to figure out how to do things
      1. Make the model look the way it was envisioned (or better!)
      1. Learn how to do texture mapping http://www.youtube.com/watch?v=EoUzm_x2wP4&feature=related
      1. Make the texture map
      1. Use a image processing program to create the desired texture
      1. Apply the texture (and make necessary adjustments)
      1. Marvel at how awesome your model looks (If it doesn't look awesome you did something wrong. Go back as many steps as needed to fix that)
      1. Watch tutorials on rigging models
      1. Rig the model and attach the vertices
      1. Move the models appendages around to ensure all vertices were assigned (if not fix it)
      1. Watch animation tutorials
      1. Master moving the model with the key frames to get the right speed and tile-ability of animation.
      1. Export to OpenCOLLADA ensuring that you check sample animation and give it the right time interval.
      1. Put the model in the game (Following Robert's above steps)
      1. Repeat all steps for the next model (Maybe skipping the watching of tutorial videos assuming your memory is good)
    * Will: I'd also recommend testing your model in game at each major step in the process to ensure both the model and your engine work together.
    * Will: Also, if you change vertices after you've made the UVW map you'll have to remake it and thus redo the texture and the rigged vertices will also need to be redone. This happened to me when I realized that my human model had 5 vertices that were shared by both his legs. I didn't realize this was the case until I had rigged him and tried to move his legs apart. Hopefully you'll avoid this complication...
    * Will: I also made all the models, then textured them, then rigged them, but I found that if you put the Unwrap UVW modifier in then put the Animate modifier in and rig it you'll be able to see if it'll animate right before you do all the work of texturing it.

  * Would you have rather started with a game engine like Ogre or would you still prefer to work from scratch?
    * No. Although there are Lisp bindings for it.....

  * Looking back over the past 10 weeks, how would you do things differently, and what would you do again in the same situation?
    * Elect a manager.
    * We probably should have chosen a simpler game concept to begin with. A racer might have been easier to pull off in a language that some members weren't too familiar with.

  * Which courses at UCSD do you think best prepared you for CSE 125?
    * Personal hobbies. Internships. Open source involvement.
    * [and Will](Robert.md) 167 for 3d math, 168 for 3d math and advanced lighting. Engel's 190 on GPU programming was very useful for shaders, but that's probably not going to be offered again.

  * What was the most important thing that you learned in the class?
    * For various group members, Lisp.
    * For those who did artwork, 3DS Max.
    * Working as a group (maybe). (Maybe working? Working maybe?)

  * Please post final screenshots of your game on your group pages for posterity.
    * Just a sec....


3. Finally, if you wish, I would appreciate any feedback on the course (entirely optional):

  * What books did you find helpful that were not on the recommended list but should be? What books were on the recommended list but were not useful and should be removed?
    * For graphics: Realtime Rendering and Game Programming Gems
    * For other areas we didn't really use books.

  * I will be teaching this course next Spring. What advice/tips/suggestions would you give students who will take the course next year?
    * Be careful about choosing <X new version of DirectX or OpenGL>. For one, it's nice to support older platforms. And if you want to support OS X or Linux, you almost have to, because sometimes things misbehave and give you a lower version than the hardware is capable of supporting.

  * How can the course be improved for next year?
    * Make everyone use Lisp. Or maybe Verilog. ;-)