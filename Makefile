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
	uglifyjs $(distdir)out.js > $(expdir)out.js
	uglifyjs $(distdir)rts.js > $(expdir)rts.js
	uglifyjs $(distdir)lib.js > $(expdir)lib.js
	uglifyjs $(distdir)runmain.js > $(expdir)runmain.js
	# gzip -6 -f $(expdir)all.js
	mv $(expdir)index-dist.html $(expdir)index.html 

push: dist
	git add . && git commit -m "bs-push" && git push

deploy: dist
	cp -R ./dist/ ../eugenen.github.io/
	cd ../eugenen.github.io/ && git add . && git commit -m "bs-deploy" && git push

