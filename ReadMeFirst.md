# Introduction #

This page is intended to get the Blackthorn 3D environment set up and running. It is intended to be a step by step guide. Please extend it if you change the build system, add libraries or other dependencies.


# Details for running on Windows #

Windows development is done primarily on Windows 7. It is likely to be backwards compatible up to Windows XP though.

## Steps ##
  1. Install MSYS and MinGW (if you don't already have them)
    * Run the MinGW installer from:
      * http://sourceforge.net/projects/mingw/files/Automated%20MinGW%20Installer/mingw-get-inst/mingw-get-inst-20110316/mingw-get-inst-20110316.exe/download
      * When choosing what to install, be sure to **install the MSYS Basic System**.
  1. Install Mercurial (if you don't already have it)
    * The installer is available at:
      * http://mercurial.selenic.com/release/windows/Mercurial-1.8.4.exe
  1. Download the project with the following command:
    * `hg clone https://blackthorn-engine-3d.googlecode.com/hg/ blackthorn-engine-3d`
  1. If you don't have SBCL and/or Quicklisp installed, run:
    * `./build/get-dependencies.sh`
    * Then log out, log back in (to get PATH set properly) and run:
    * `./build/get-dependencies.sh`
    * (The second time it installs Quicklisp.)
  1. You can test that the download works by running:
    * `make shell`
    * To leave the interpreter, type: `(quit)`
  1. Now try to run the game:
    * `make server`
    * `make client` (in a separate console)