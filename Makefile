buildroot := $(shell pwd)
stackroot := $(shell stack path --dist-dir)
suffix    := build/business-demo/business-demo.jsexe/
distdir := $(buildroot)/$(stackroot)/$(suffix)


.PHONY : serve copyres build all open

serve:
	sws --port 4444 -d -P $(distdir)

copyres:
	cp -R ./res/  $(distdir)

build:
	stack build

all: build copyres 

open: all
	open $(distdir)index.html