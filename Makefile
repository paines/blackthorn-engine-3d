#### Blackthorn -- Lisp Game Engine
####
#### Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
####
#### Permission is hereby granted, free of charge, to any person
#### obtaining a copy of this software and associated documentation
#### files (the "Software"), to deal in the Software without
#### restriction, including without limitation the rights to use, copy,
#### modify, merge, publish, distribute, sublicense, and/or sell copies
#### of the Software, and to permit persons to whom the Software is
#### furnished to do so, subject to the following conditions:
####
#### The above copyright notice and this permission notice shall be
#### included in all copies or substantial portions of the Software.
####
#### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#### NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#### HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#### WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#### DEALINGS IN THE SOFTWARE.
####

# Search PATH for a Lisp compiler.
cl := $(shell build/scripts/find-lisp.sh)
ifeq (${cl},)
	$(error No Lisp compiler found.)
endif

# Which ASDF system to load:
system := lkcas

# Stardard drivers:
quicklisp-setup := build/scripts/quicklisp-setup.lisp
emacs-setup := build/scripts/emacs-setup.lisp
prop :=  build/scripts/property.lisp
nop := build/scripts/nop.lisp
load :=  build/scripts/load.lisp
test :=  build/scripts/test.lisp
dist :=  build/scripts/dist.lisp
prof :=  build/scripts/profile.lisp
atdoc :=  build/scripts/atdoc.lisp

# Select which driver to run (load by default).
driver := ${load}

args :=

# Specify the run command for the installer.
command := \\\"\\x24INSTDIR\\\\main.exe\\\"

# A temporary file for passing values around.
tempfile := .tmp

# A command which can be used to get an ASDF system property.
ifeq (${cl}, allegro)
	get-property = $(shell alisp +B +s ${prop} -e "(defparameter *driver-system* :${system})" -e "(defparameter *output-file* \"${tempfile}\")" -e "(defparameter *output-expression* '$(1))" -L "${quicklisp-setup}")
else
ifeq (${cl}, sbcl)
	get-property = $(shell sbcl --load ${quicklisp-setup} --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})
else
ifeq (${cl}, sbcl-builtin)
	get-property = $(shell SBCL_HOME="$(shell build/scripts/pwd.sh)/build/sbcl/" build/sbcl/sbcl --load ${quicklisp-setup} --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})
else
ifeq (${cl}, clisp)
	get-property = $(shell clisp -x "(load \"${quicklisp-setup}\")" -x "(defparameter *driver-system* \"${system}\")" -x "(defparameter *output-file* \"${tempfile}\")" -x "(defparameter *output-expression* '$(1))" -x "(load \"${prop}\")")
else
ifeq (${cl}, ecl)
	get-property = $(shell ecl -load ${quicklisp-setup} -eval "(defparameter *driver-system* \"${system}\")" -eval "(defparameter *output-file* \"${tempfile}\")" -eval "(defparameter *output-expression* '$(1))" -load ${prop})
else
ifeq (${cl}, clozure)
	get-property = $(shell ccl --load ${quicklisp-setup} --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})
else
ifeq (${cl}, clozure-builtin)
	get-property = $(shell build/ccl/wx86cl.exe --load ${quicklisp-setup} --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})
endif
endif
endif
endif
endif
endif
endif

# Get ASDF system properties for the specified system.
define get-properties
	$(eval temp := $$(call get-property,(asdf:component-name *system*)))
	$(eval name := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(or (asdf:component-property *system* :long-name) (asdf:component-name *system*))))
	$(eval longname := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:component-version *system*)))
	$(eval version := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:system-description *system*)))
	$(eval description := $$(shell cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:component-property *system* :url)))
	$(eval url := $$(shell cat $${tempfile}))
endef

export cl, db, system, driver, name, longname, version, description, url, command

.PHONY: new
new:
	$(MAKE) clean
	$(MAKE) load

.PHONY: load
load:
	$(MAKE) load-${cl}

.PHONY: load-allegro
load-allegro:
	alisp +B +s ${driver} -e "(defparameter *driver-system* :${system})" -L "${quicklisp-setup}" -- ${args}

.PHONY: load-sbcl
load-sbcl:
	sbcl --eval "(defparameter *driver-system* \"${system}\")" --load ${quicklisp-setup} --load ${driver} -- ${args}

.PHONY: load-sbcl-builtin
load-sbcl-builtin:
	SBCL_HOME="$(shell build/scripts/pwd.sh)/build/sbcl/" build/sbcl/sbcl --eval "(defparameter *driver-system* \"${system}\")" --load ${quicklisp-setup} --load ${driver} -- ${args}

.PHONY: load-clisp
load-clisp:
	clisp -x "(defparameter *driver-system* \"${system}\")" -x "(load \"${quicklisp-setup}\")" -x "(load \"${driver}\")" -- ${args}

.PHONY: load-ecl
load-ecl:
	ecl -eval "(defparameter *driver-system* \"${system}\")" -load ${quicklisp-setup} -load ${driver} -- ${args}

.PHONY: load-clozure
load-clozure:
	ccl --eval "(defparameter *driver-system* \"${system}\")" --load ${quicklisp-setup} --load ${driver} -- ${args}

.PHONY: load-clozure-builtin
load-clozure-builtin:
	build/ccl/wx86cl.exe --eval "(defparameter *driver-system* \"${system}\")" --load ${quicklisp-setup} --load ${driver} -- ${args}

.PHONY: shell
shell:
	$(MAKE) driver="${nop}" new

.PHONY: setup-emacs
setup-emacs:
	$(MAKE) driver="${emacs-setup}" new
#$(shell build/scripts/find-emacs.sh) -l build/emacs/emacs.el

.PHONY: slime
slime:
	$(MAKE) clean
	emacs --eval "(progn (slime '${cl}) (while (not (slime-connected-p)) (sleep-for 0 200)) (slime-interactive-eval \"(defparameter *driver-system* \\\"${system}\\\")\") (slime-load-file \"${driver}\"))"

.PHONY: server
server:
	$(MAKE) args="--server=127.0.0.1 --port=12345" new

.PHONY: client
client:
	$(MAKE) args="--connect=127.0.0.1 --port=12345" new

.PHONY: server3
server3:
	$(MAKE) args="--server --port=12345 --players=3" new

.PHONY: server4
server4:
	$(MAKE) args="--server --port=12345 --players=4" new

.PHONY: server5
server5:
	$(MAKE) args="--server --port=12345 --players=5" new

.PHONY: thopter
thopter:
	$(MAKE) system="thopter" new

.PHONY: bunny
bunny:
	$(MAKE) system="bunnyslayer" new

.PHONY: stress
stress:
	$(MAKE) system="blackthorn-stress-test" new

.PHONY: collision
collision:
	$(MAKE) system="blackthorn-collision-test" new

.PHONY: test
test:
	$(MAKE) driver="${test}" system="blackthorn3d-test" new

.PHONY: prof
prof:
	$(MAKE) driver="${prof}" new

.PHONY: distnoclean
distnoclean:
	mkdir -p bin/res
	cp -r $(wildcard lib/*) bin
	cp -r $(wildcard res/*) bin/res
	$(MAKE) driver="${dist}" new

.PHONY: dist
dist:
	$(MAKE) distclean distnoclean

.PHONY: doc
doc:
	$(MAKE) docclean
	$(MAKE) atdoc

.PHONY: atdoc
atdoc:
	$(MAKE) driver="${atdoc}" new

.PHONY: install-w32
install-w32:
	-$(call get-properties)
	$(MAKE) distclean
	mkdir -p bin
	if test -e build/native/windows/${name}.ico; then cp build/native/windows/${name}.ico bin/app.ico; else cp build/native/windows/bt.ico bin/app.ico; fi
	$(MAKE) distnoclean postinstall-w32

.PHONY: postinstall-w32
postinstall-w32:
	-$(call get-properties)
	cp -r build/native/windows/is_user_admin.nsh COPYRIGHT bin
	awk "{gsub(/@NAME@/, \"${name}\");print}" build/native/windows/install.nsi | awk "{gsub(/@LONGNAME@/, \"${longname}\");print}" | awk "{gsub(/@VERSION@/, \"${version}\");print}" | awk "{gsub(/@DESCRIPTION@/, \"${description}\");print}" | awk "{gsub(/@URL@/, \"${url}\");print}" | awk "{gsub(/@COMMAND@/, \"${command}\");print}" > bin/install.nsi
	makensis bin/install.nsi
	mv bin/*-install.exe .

.PHONY: install-unix
install-unix:
	-$(call get-properties)
	$(MAKE) dist
	cp build/native/unix/run.sh bin
	mv bin ${name}
	tar cfz ${name}-${version}-linux.tar.gz ${name}
	mv ${name} bin

.PHONY: install-mac
install-mac:
	-$(call get-properties)
	rm -rf "${longname}.app"
	$(MAKE) dist
	mkdir "${longname}.app" "${longname}.app/Contents" "${longname}.app/Contents/MacOS" "${longname}.app/Contents/Frameworks" "${longname}.app/Contents/Resources"
	awk "{gsub(/@NAME@/, \"${name}\");print}" macosx/Info.plist | awk "{gsub(/@LONGNAME@/, \"${longname}\");print}" | awk "{gsub(/@VERSION@/, \"${version}\");print}" | awk "{gsub(/@DESCRIPTION@/, \"${description}\");print}" | awk "{gsub(/@URL@/, \"${url}\");print}" > "${longname}.app/Contents/Info.plist"
	cp -r $(wildcard lib/*.framework) "${longname}.app/Contents/Frameworks"
	cp -r res "${longname}.app/Contents/Resources"
	cp bin/main "${longname}.app/Contents/MacOS"
	cp build/native/macosx/PkgInfo COPYRIGHT "${longname}.app/Contents"
	if test -e "build/native/macosx/${name}.icns"; then cp "build/native/macosx/${name}.icns" "${longname}.app/Contents/Resources/app.icns"; else cp build/native/macosx/bt.icns "${longname}.app/Contents/Resources/app.icns"; fi
	tar cfz "${name}-${version}-macos.tar.gz" "${longname}.app"
	$(MAKE) -f dmg_utils.make NAME="${longname}" VERSION="${version}" SOURCE_DIR=. SOURCE_FILES="${longname}.app COPYRIGHT"

.PHONY: clean
clean:
	rm -rf ${tempfile}

.PHONY: docclean
docclean:
	rm -rf doc

.PHONY: distclean
distclean:
	$(MAKE) clean docclean
	rm -rf build.in build.out bin
