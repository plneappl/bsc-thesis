# MAKEFILE to compile the paper on Mac
#
# Usage: `make`
#
# Usage for lazy people on Mac:
# 1. install homebrew
# 2. brew install fswatch
# 3. make demon (monitors files and recompile as necessary)
# or
# 3. make demon OPEN='opening command' (monitors files and recompile as necessary)
#
# If something looks wrong, then `make fresh`.

OPEN = open

# Pass V=1 to see the output.
V = 0

latex = pdflatex

srcbib = master.bib

# used to include DirtyWork.hs
srclhs = master.lhs DirtyWork.hs

srctex = bcprules.sty fig/*.tex macros.tex sigplanconf.cls syntax-tree.tex

# Dependencies are listed in a topologically sorted order (from the end to the
# beginning).

ifeq ($V, 1)
  REDIR=
else
  REDIR=> /dev/null
endif

all:	open

open: master.pdf
	$(OPEN) $<

%.pdf: %.tex %.blg
	$(latex) master $(REDIR) && $(latex) master $(REDIR)
	touch master.blg # Otherwise it will look "out-of-date" and trigger a rebuild without changes.

master.tex: master.lhs DirtyWork.tex
	lhs2TeX $< > $@

%.blg: %.aux %.bib
	bibtex master $(REDIR)

%.aux: %.tex DirtyWork.tex
	$(latex) master $(REDIR)

%.tex: %.hs
	runhaskell $< > $@

.PRECIOUS: %.aux %.tex %.blg %.pdf

clean:
	rm -f \
	master.aux master.bbl master.blg master.log master.tex \
	master.pdf master.ptb master.toc

fresh:
	make clean
	make

fswatch = fswatch -0o

# `cmd1 | $(xargs) cmd2` will run cmd2 each time cmd1 outputs a '\0'-terminated
# string.
xargs = xargs -0 -n 1 -I '{}' -t

demon:
	make
	$(fswatch) $(srclhs) $(srctex) $(srcbib) Makefile | $(xargs) make & \
	wait
