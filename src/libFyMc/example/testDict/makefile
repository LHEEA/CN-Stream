#!/usr/bin/make

PROJECT_DIR=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))

EXES = main

### Compiler -------------------------------------------------------------------
# FC      = ifort
FC      = gfortran

FLAGMOD1= $(DIR_OBJ)
OPTSC0  = $(FLAGMOD1)
OPTSC = $(OPTSC0)

CFLAGS = -fPIC -O2 -g -c

COMPILE_OBJECT_RULE=@$(FC) $(CFLAGS) $(OPTSC)

## Add Static Library Rule

## Static library Path
LIBPATH = $(PROJECT_DIR)../../lib/

## add static library rule and header file path (.mod)
LDFLAGS = -fintrinsic-modules-path $(LIBPATH) -L$(LIBPATH) -lfymc

### Targets for compilation ----------------------------------------------------
Release: OPTSC = $(OPTFLAGS) $(OPTSC0)
Release: OPTSL = $(OPTFLAGS) $(OPTSL0)
Release: main

### auxiliary variables --------------------------------------------------------
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

main:main.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC) $< -o $@ $(LDFLAGS)
EXES := $(EXES)

### compiling rules ------------------------------------------------------------
$(DOBJ)main.o: main.f90
	@echo $(COTEXT)
	@$(COMPILE_OBJECT_RULE) $< $(LDFLAGS)

### phony auxiliary rules ------------------------------------------------------
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr @rm *.so *.o *.mod

.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(addprefix $(DEXE),$(EXES))

.PHONY : clean
clean: cleanobj

.PHONY : cleanall
cleanall: clean cleanexe
