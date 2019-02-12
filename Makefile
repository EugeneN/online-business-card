buildroot := $(shell pwd)
stackroot := $(shell stack path --dist-dir)
suffix    := build/business-demo/business-demo.jsexe/
distdir   := $(buildroot)/$(stackroot)/$(suffix)
expdir    := $(buildroot)/dist/


.PHONY : serve copyres build all open dist servedist

serve: all
	sws --port 4444 -d -P $(distdir)

servedist: dist
	sws --port 4444 -d -P $(expdir)

copyres:
	cp -R ./res/  $(distdir)
	cp -R ./extra/  $(distdir)

build:
	stack build

all: build copyres 

open: all
	open $(distdir)index.html

dist: all
	rm -rf $(expdir)
	mkdir -p $(expdir)
	cp -R ./res/  $(expdir)
	cp $(distdir)out.js $(expdir)
	cp $(distdir)rts.js $(expdir)
	cp $(distdir)lib.js $(expdir)
	cp $(distdir)runmain.js $(expdir)
	# gzip -6 -f $(expdir)all.js
	mv $(expdir)index-dist.html $(expdir)index.html 
