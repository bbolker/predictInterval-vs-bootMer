soib_reprex_orig.Rout: soib_reprex_orig.R

soib_summary.html: soib_reprex_orig.rda soib_summary.rmd
	Rscript --vanilla -e "rmarkdown::render('soib_summary.rmd')"

include makestuff/wrapR.mk
