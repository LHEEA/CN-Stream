#!/usr/bin/make

### Path -----------------------------------------------------------------------
DSRC=src/
DOBJ=obj/

MKDIRS  = $(DOBJ)

### Compiler -------------------------------------------------------------------
FC      = ifort
#FC      = gfortran

OPTSC0  = -c

### Targets for compilation ----------------------------------------------------
Release: OPTSC = $(OPTFLAGS) $(OPTSC0)
Release: OPTSL = $(OPTFLAGS) $(OPTSL0)
Release: $(MKDIRS)
Release: main

### auxiliary variables --------------------------------------------------------
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

main:$(DOBJ)main.o
	@echo $(COTEXT)
	@$(FC) -o $@ $(DOBJ)*.o -ldl
EXES := main

### compiling rules ------------------------------------------------------------
$(DOBJ)main.o: main.f90 \
	$(DOBJ)modComm_CN_Stream.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC) $< -o $@

$(DOBJ)modComm_CN_Stream.o: modComm_CN_Stream.f90 \
	$(DOBJ)modVariablesRF.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC) $< -o $@

$(DOBJ)modVariablesRF.o: modVariablesRF.f90
	@echo $(COTEXT)
	@$(FC) $(OPTSC) $< -o $@


	

	

### phony auxiliary rules ------------------------------------------------------
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ) @rm *.so *.o *.mod
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DMOD)
.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(addprefix $(DEXE),$(EXES))
.PHONY : cleanVTK
cleanvtk:
	@echo deleting VTK
	@rm -rf VTK
.PHONY : clean
clean: cleanobj cleanmod
.PHONY : cleanall
cleanall: clean cleanexe cleanvtk
