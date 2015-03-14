# Status Report 05/31/2011 #

  1. what were your concrete goals for the week?
    * Will be gone Friday to Tuesday for ICPC.
    * Fix stuff? (It seems like I get stuck doing this no matter what my set goals are.)
    * Improve server timer.
    * Maybe text, but I don't want to waste time on stuff we won't use.
    * Maybe sound effects (as in, actually put them into the game, and maybe get my roommate to record us some epic, Unreal-style KILLING STREAK voice overs, although I don't know if that really fits the game concept).
    * Could do refactoring if I have free time (as if that'll happen).
  1. what goals were you able to accomplish?
    * ICPC.
    * Fixed some stuff (as usual).
    * Moved to upstream userial library.
    * Not much else.
  1. if there were goals you were unable to meet, what were the reasons?
    * ICPC.
  1. what are your specific goals for the next week?
    * Finish the game.
  1. what is your individual morale?
    * Uhhh, well the game's doing reasonably well without me; I don't really have anything to do with it though.

# Status Report 05/24/2011 #

  1. what were your concrete goals for the week?
    * Text.
    * Improve sound (send it over network, add sampling).
    * Fix Clozure (optional, but it would make me nervous if we only had one working compiler for the demo).
  1. what goals were you able to accomplish?
    * Sound can be initiated over the network. Sampling theoretically works.
    * Clozure was fixed. The problem was actually really stupid (I was throwing away a reference and it was getting garbage collected).
    * Helped people with various things and fixed misc stuff.
    * Didn't work on fonts, don't think we'll need it.
  1. if there were goals you were unable to meet, what were the reasons?
    * Starting a sound at a specific position, rewinding a sound, etc are extremely buggy in the underlying library, and it's been essentially impossible to support those inside the game engine. Working with the lispbuilder maintainers to fix those problems, no idea when that'll be finished.
  1. what are your specific goals for the next week?
    * Will be gone Friday to Tuesday for ICPC.
    * Fix stuff? (It seems like I get stuck doing this no matter what my set goals are.)
    * Improve server timer.
    * Maybe text, but I don't want to waste time on stuff we won't use.
    * Maybe sound effects (as in, actually put them into the game, and maybe get my roommate to record us some epic, Unreal-style KILLING STREAK voice overs, although I don't know if that really fits the game concept).
    * Could do refactoring if I have free time (as if that'll happen).
  1. what is your individual morale?
    * Ok, could be better; getting uncomfortably close to end of quarter and it's a little crazy with this trip happening right now.

# Status Report 05/17/2011 #

  1. what were your concrete goals for the week?
    * Fix things that people need me to fix.
    * Optionally fix various OS/implementation combinations.
    * Sound.
    * Text.
  1. what goals were you able to accomplish?
    * SBCL/Windows networking finally works thanks to usocket fix.
    * Initial sound working. Except on Clozure/Windows XD. Spent time trying to track that down, no luck so far.
  1. if there were goals you were unable to meet, what were the reasons?
    * Didn't have time to do text after spending time trying to fix Clozure.
  1. what are your specific goals for the next week?
    * Text.
    * Improve sound (send it over network, add sampling).
    * Fix Clozure (optional, but it would make me nervous if we only had one working compiler for the demo).
  1. what is your individual morale?
    * Doing ok.

# Status Report 05/10/2011 #

  1. what were your concrete goals for the week?
    * Hopefully fix SBCL on Windows. That would be really nice. Alternatively, I could just forget about SBCL on Windows and would probably be happier as a result (although it wouldn't run as fast).
    * Optionally fix CLISP and Allegro. I think they may even be having the same problem.
    * Clean up is very seriously needed in some areas.
    * Rethink entity sync. Right now we do not support extension of the client entity. I'm not sure how we're going to do e.g. particle effects without reworking this a bit.
  1. what goals were you able to accomplish?
    * Not a whole heck of a lot. I think the most I really did was help people with other problems. Most of my personal goals were shelved or delayed.
  1. if there were goals you were unable to meet, what were the reasons?
    * Fixing usocket/sbcl/win32 requires help from their developers.
    * Other OS fixes are not high priority.
    * I spent some time harassing the SBCL developers to let me upload official builds for them. (Maybe this doesn't matter for this specific project, but it's good for the Lisp community in general to have up-to-date builds.)
    * I've been playing Mass Effect 2. :-)
  1. what are your specific goals for the next week?
    * Fix things that people need me to fix.
    * Optionally fix various OS/implementation combinations.
    * Sound.
    * Text.
  1. what is your individual morale?
    * Need to get my game on again.

# Status Report 05/03/2011 #

  1. what were your concrete goals for the week?
    * Fix bugs in network code, and make it more robust.
    * Clean up the network/serialization API.
    * Finally sync state changes over the network.
  1. what goals were you able to accomplish?
    * Fixed a lot of bugs in the net code, and found even more.
    * Many of the bugs I found were actually a result of bugs in the underlying networking library we use. Many bugs were reproducible on only one compiler, or on only one OS, or a specific compiler on a specific OS. Naturally I took it upon myself to fix all possible platforms. Many bugs I was able to work around or ignore, and some I sent reports to the network library authors to be fixed. Everything almost works, although I'm still blocked by one critical bug on SBCL on Windows. So the situation looks something like:

| SBCL | Clozure | CLISP | Allegro |
|:-----|:--------|:------|:--------|
| Works on non-Windows | Works | Client works | Client works |

  * Despite my escapades into bug smashing, I managed to find the time to sync objects ("entities") across the network.
  1. if there were goals you were unable to meet, what were the reasons?
    * I honestly was probably trying to do too much. Not that the original goal was too unreasonable, but I've dealt with at least a dozen or so bugs in getting everything to work, and a signficant portion of those aren't mine.
    * Didn't really have time for cleaning up after all that.
  1. what are your specific goals for the next week?
    * Hopefully fix SBCL on Windows. That would be really nice. Alternatively, I could just forget about SBCL on Windows and would probably be happier as a result (although it wouldn't run as fast).
    * Optionally fix CLISP and Allegro. I think they may even be having the same problem.
    * Clean up is very seriously needed in some areas.
    * Rethink entity sync. Right now we do not support extension of the client entity. I'm not sure how we're going to do e.g. particle effects without reworking this a bit.
  1. what is your individual morale?
    * Holy shit, it works?!

# Status Report 04/26/2011 #

  1. what were your concrete goals for the week?
    * Glue stuff. Specically, I'm hoping to get graphics and the game core talking to each other (and hopefully the network).
    * Basic serialization. And if that's easy, then also stuffing buffers into UDP packets and flinging them across the network.
    * Play Portal 2!!! Oh yeah!
  1. what goals were you able to accomplish?
    * I finished Portal 2 single player and coop.
    * I switched jobs with Chris: I took on primary networking and secondary game logic, and he took primary game logic and secondary networking.
    * I wrote a lot of socket code.
    * I finished up the serialization code that was mostly non-existant last week.
  1. if there were goals you were unable to meet, what were the reasons?
    * Well, I didn't spend any time gluing things together, but that's mostly a result of swapping roles.
  1. what are your specific goals for the next week?
    * Fix bugs in network code, and make it more robust.
    * Clean up the network/serialization API.
    * Finally sync state changes over the network.
  1. what is your individual morale?
    * Doing pretty well.

# Status Report 04/19/2011 #

  1. what were your concrete goals for the week?
    * Work on the core game representation logic and data structures.
    * Think about basic networking code (maybe; I'm not sure if we're really ready for this yet).
    * Continue to be obnoxious about code style.
    * Continue to bug ACS to get things right on the lab machines.
    * Tweak the wiki clone code to do a better job of rewriting paths.
  1. what goals were you able to accomplish?
    * Well, I committed some nominal "game core" code, but it doesn't do much.
    * Started on some serialization and networking.
    * Loosened up a bit on code style (I think people can handle it).
    * Better Emacs support for SLIME.
  1. if there were goals you were unable to meet, what were the reasons?
    * Didn't get around to the wiki rewrite (just didn't have time).
    * My game core code doesn't really DO anything, but I guess that shouldn't be a surprise because it's basically meant to be glue and there isn't really anything to glue yet.
  1. what are your specific goals for the next week?
    * Glue stuff. Specically, I'm hoping to get graphics and the game core talking to each other (and hopefully the network).
    * Basic serialization. And if that's easy, then also stuffing buffers into UDP packets and flinging them across the network.
    * Play Portal 2!!! Oh yeah!
  1. what is your individual morale?
    * Not quite getting traction yet, but not bad by any means.

# Status Report 04/12/2011 #

  1. what were your concrete goals for the week?
    * Set up the repository on Google Code.
    * Added build system (compilers, libraries, makefiles, shell scripts, etc) to the repository.
    * Added an automatic Emacs configuration script.
    * Bugged Geoff and ACS to get software installed on the lab machines.
    * Wrote a script to clone the Google Code wiki to pisa.
    * Annoyed various people by refactoring their code for them.
    * Argued strenuously about the costs and benefits of writing FFI code in pure Lisp.
    * Tracked down an 18-month old bug in SBCL/CFFI/Lispbuilder which is finally resolved in Windows 7 SP 1.
    * Fixed a bug in my old installer defaulting to compatibility mode on Windows 7 (which we will presumably be using eventually).
  1. what goals were you able to accomplish?
    * All of the above except the setup on lab machines isn't entirely done yet.
  1. if there were goals you were unable to meet, what were the reasons?
    * ACS is slow to update their machines.
  1. what are your specific goals for the next week?
    * Work on the core game representation logic and data structures.
    * Think about basic networking code (maybe; I'm not sure if we're really ready for this yet).
    * Continue to be obnoxious about code style.
    * Continue to bug ACS to get things right on the lab machines.
    * Tweak the wiki clone code to do a better job of rewriting paths.
  1. what is your individual morale?
    * Tired but satisfied.