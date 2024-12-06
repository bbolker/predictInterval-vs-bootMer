 # predictInterval-vs-bootMer

Debugging major discordance in uncertainty estimates using predictInterval()

## BMB notes

* `merTools::predictInterval()` and `bootMer` are giving very different answers
* possible causes:
    * bad computation of vcov? (RX vs Hessian)
	* failure of Wald approx?
	* conditioning on estimates of theta?

### to do

* 
