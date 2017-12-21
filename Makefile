.PHONY: all clean config

GPRBUILD=$(if $(shell command -v gprbuild 2> /dev/null),\
	gprbuild,\
	$(if $(shell command -v gnatmake 2> /dev/null),\
		gnatmake $(warning "Using gnatmake instead of gprbuild..."),\
		$(error "Neither gprbuild nor gnatmake installed...")))

GPRCLEAN := $(if $(shell command -v gprclean 2> /dev/null),\
	gprclean,\
	$(if $(shell command -v gnatclean 2> /dev/null),\
		gnatmake $(warning "Using gnatclean instead of gprclean..."),\
		$(error "Neither gnatclean nor gprclean installed...")))
VERSION_SOURCE=src/version.ads
VERSION=$(shell git describe --tags)

SDL_DIR=/usr/lib
LATEX=pdflatex

all: as run instructions

config:
	@echo 'package Version is' > $(VERSION_SOURCE)
	@echo '    Tag : constant String := "'$(VERSION)'";' >> $(VERSION_SOURCE)
	@echo 'end Version;' >> $(VERSION_SOURCE)

as: config
	$(GPRBUILD) -P src/assembler.gpr

run: config
	$(GPRBUILD) -XSDL2_LIB_DIR=$(SDL_DIR) -P src/runner.gpr

instructions: instructions.pdf

%.pdf: %.tex
	@$(LATEX) -interaction=batchmode $<
	@$(LATEX) -interaction=batchmode $<
	-@rm $*.aux $*.log $*.lot $*.out $*.toc


clean: config
	$(GPRCLEAN) -P src/assembler.gpr
	$(GPRCLEAN) -P src/runner.gpr


