soib_summary.html: soib_summary.rmd experiments.Rout funs.Rout
	Rscript --vanilla -e "rmarkdown::render('soib_summary.rmd')"

experiments.Rout: experiments.R funs.R

experiments3.Rout: experiments3.R funs.R

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

autopipeR = TRUE
autoknit = defined

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk

