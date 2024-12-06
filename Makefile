soib_reprex_orig.Rout: soib_reprex_orig.R

soib_summary.html: soib_reprex_orig.rda soib_summary.rmd
	Rscript --vanilla -e "rmarkdown::render('soib_summary.rmd')"

### makestuff boilerplate
Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp: | makestuff
	- $(RM) makestuff/*.stamp
	cd makestuff && $(MAKE) pull
	touch $@
makestuff:
	git clone --depth 1 $(msrepo)/makestuff

-include makestuff/os.mk

## -include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk

