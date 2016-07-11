R=R
# -> you can do    R=R-devel  make ....

PACKAGE=mews
# get VERSION from mews/DESCRIPTION  
## ("::" = expand only  once, but doesn't work in make <= 3.81)
VERSION := $(shell sed -n '/^Version: /s///p' mews/DESCRIPTION)

TARBALL := $(PACKAGE)_$(VERSION).tar.gz
ZIPFILE := =$(PACKAGE)_$(VERSION).zip

CPP_SRC := $(PACKAGE)/src/*.cpp

all:
	make doc-update
	make build-package
	make install
	make pdf


build-package: $(TARBALL)
$(TARBALL): $(PACKAGE)/NAMESPACE $(CPP_SRC)
	$(R) CMD build --resave-data=no $(PACKAGE)

install: $(TARBALL)
	$(R) CMD INSTALL --preclean $<
	@touch $@

doc-update: $(PACKAGE)/R/*.R
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"collate\", \"rd\"))" | $(R) --slave
	@touch doc-update

namespace-update :: $(PACKAGE)/NAMESPACE
$(PACKAGE)/NAMESPACE: $(PACKAGE)/R/*.R
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"namespace\"))" | $(R) --slave

## To enable quick compile, run from R:
##    library(TMB); precompile(flags="-O0 -g")
quick-install: enum-update $(PACKAGE)/src/LSN.so
	enum-update $(PACKAGE)/src/OU.so
	$(R) CMD INSTALL $(PACKAGE)

$(PACKAGE)/src/LSN.so: $(PACKAGE)/src/LSN.cpp
	cd $(PACKAGE)/src; echo "library(TMB); compile('LSN.cpp')" | $(R) --slave
$(PACKAGE)/src/OU.so: $(PACKAGE)/src/OU.cpp
	cd $(PACKAGE)/src; echo "library(TMB); compile('OU.cpp')" | $(R) --slave

unexport TEXINPUTS
pdf: $(PACKAGE).pdf
$(PACKAGE).pdf: $(PACKAGE)/man/*.Rd
	rm -f $(PACKAGE).pdf
	$(R) CMD Rd2pdf --no-preview $(PACKAGE)

check:
	$(R) CMD check $(PACKAGE)

## *NOT* using 'R --vanilla' : then cannot find testthat, TMB, etc they are installed into R's "system" library

test:
	echo "devtools::test('mews')" | $(R) --slave

quick-check: quick-install ex-test

ex-test:
	echo "library(mews); example(mews)" | $(R) --slave


unlock:
	\rm -rf `Rscript --vanilla -e 'writeLines(.Library)'`/00LOCK-mews
#               ------------------------------------------ = R's system library
#	rm -rf ${R_LIBS}/00LOCK-mews
##               ^^^^^^^ This only works if R_LIBS contains a single directory and the same that 'R CMD INSTALL' uses..

clean:
	\rm -f install doc-update
