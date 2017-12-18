.PHONY: all clean config

GPRBUILD := $(shell command -v gprbuild 2> /dev/null)
GPRCLEAN := $(shell command -v gprclean 2> /dev/null)
VERSION_SOURCE=src/version.ads
VERSION=$(shell git describe --tags)

all: as run

config:
ifndef GPRBUILD
	GPRBUILD := $(shell command -v gnatmake 2> /dev/null)
	USINGGNATMAKE := true
endif
ifndef GPRBUILD
	$(error "Neither gprbuild nor gnatmake installed...")
endif
ifdef USINGGNATMAKE
	$(warning "Using gnatmake instead of gprbuild...")
endif

ifndef GPRCLEAN
	GPRCLEAN := $(shell command -v gnatclean 2> /dev/null)
	USINGGNATCLEAN := true
endif
ifndef GPRBUILD
	$(error "Neither gprclean nor gnatclean installed...")
endif
ifdef USINGGNATCLEAN
	$(warning "Using gnatclean instead of gprclean...")
endif
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

