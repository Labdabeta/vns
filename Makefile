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

all: as run

config:
	@echo 'package Version is' > $(VERSION_SOURCE)
	@echo '    Tag : constant String := "'$(VERSION)'";' >> $(VERSION_SOURCE)
	@echo 'end Version;' >> $(VERSION_SOURCE)

as: config
	$(GPRBUILD) -P src/assembler.gpr

run: config
	$(GPRBUILD) -P src/runner.gpr

clean: config
	$(GPRCLEAN) -P src/assembler.gpr
	$(GPRCLEAN) -P src/runner.gpr

