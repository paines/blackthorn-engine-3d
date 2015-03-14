# Lisp Code #

Use the following guidelines for formatting Lisp code.

## File Format ##

Save all files in Unix format (i.e. \n line endings) rather than DOS format (i.e. \r\n line endings).

## Header Blocks ##

Use the project-wide header block for all source files. See an example in [blackthorn3d.asd](http://code.google.com/p/blackthorn-engine-3d/source/browse/blackthorn3d.asd).

## Copyright Attributions ##

When you create a file, modify the attribution to include your name and the current year.

When you make non-trivial modifications to a file, modify the attribution to include your name (and if the file is more than a year old, split the attribution line by year to include you only for the current year and later).

## Comment Blocks ##

Different types of comments are preceded by different numbers of semicolons:

```
;;;;
;;;; This is a file header block. Include an empty comment line before and after.
;;;;

;;;
;;; This is a section header block. Include an empty comment line before and after.
;;;

;; This comment describes code immediately following.
;; It is appropriate for discussing functions or code blocks.

; This one-line comment describes code on the same line.
```

## Documentation Strings ##

In general, use doc strings rather than comment blocks for documenting functions. Preferably, doc strings should follow [Atdoc format](http://www.lichteblau.com/atdoc/doc/) to make it easy to generate API documentation for the project.

## White Space ##

Use Emacs and lisp-mode to indent your code. In some cases you are allowed discretion to indent code as you wish, but in general you should defer to Emac's judgement.

Always use spaces. Never use tabs.

Do not leave extra white space on the end of your lines.

Leave exactly one newline at the end of the file (to end the last line). Do not leave any blank lines at the end of the file.