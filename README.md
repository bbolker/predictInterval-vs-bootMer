 # predictInterval-vs-bootMer

Debugging major discordance in uncertainty estimates using predictInterval()

## BMB notes

* `merTools::predictInterval()` and `bootMer` are giving very different answers
* possible causes:
    * bad computation of vcov? (RX vs Hessian)
	* failure of Wald approx?
	* conditioning on estimates of theta?
* `soib_summary.{rmd,html}`: summary of the problem
* `funs.R`: utility functions
* `experiments.R`: more sims
* `experiments2.R`: struggling with simulated cloglog/sleepstudy example
* `experiments3.R`: further investigations of the bootstrap distributions of beta, theta, pred for a subsample
* `soib_reprex_orig.R`: original (slightly modified) example

