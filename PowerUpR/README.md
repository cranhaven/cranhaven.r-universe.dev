<h2> Power Analysis Tools for Multilevel Randomized Experiments </h2>

To install and load the library
```{r}
install.packages("PowerUpR")
library(PowerUpR)
```

Statistical power, minimum detectable effect size (MDES), MDES difference (MDESD), or minimum required sample size (MRSS) can be requested by using the relevant function given design parameters. In general, each function begins with an **output** name, follows by a period, and ends with a **design** name in the form `<output>.<design>()`. There are three types of output; `mdes` for main effects (`mdes` or `mdesd` for moderation effects),  `power`, and `mrss`. Each output can be requested for eighteen types of designs to detect main treatment effect; `ira`, `ira_pn`, `bira2`, `bira2_pn`, `bira2f1`, `bira2c1`, `cra2`, `cra2_pn`, `bira3`, `bcra3r2`, `bcra3r2_pn`, `bcra3f2`, `cra3`, `bira4`, `bcra4r2`, `bcra4r3`, `bcra4f3`, `cra4`, and seven types of designs to detect moderator effects; `mod211`, `mod212`, `mod221`, `mod222`, `mod331`, `mod332`, and `mod333`. To detect mediator effects, only `power` can be requested for seven types of designs; `med211`, `med221`, `med331`, `med321`, `med311`, `med_pn21`, `med_pn31`, and `med_pn32`.

For designs to detect main effects, first three letters stand for the type of assignment; for individual random assignment `ira`, for blocked individual random assignment `bira`, for cluster random assignment `cra`, and for blocked cluster random assignment `bcra`. First (or the only number) indicate total number of levels. The single letter inbetween refers to whether the top level is random or fixed. Partially nested designs are denoted with `pn`.  

Naming conventions are slighlty different for designs to detect moderator and mediator effects. Numbers following `mod` keyword indicate total number of levels, the level at which randomization takes place, and the level at which moderator resides correspondingly. As for the mediator effects, numbers following `med` keyword indicate the level at which treatment, mediator and outcome variables reside. 

For example, the function `mdes.cra2()` can be called to calculate MDES for the main treatment effect in a two-level cluster-randomized trial. Similiarly, the function `mdesd.mod222()` can be called to calculate MDESD for moderator effect residing at level 2 in a two-level cluster-randomized trial. Finally, the function `power.med221()` can be called to calculate statistical power for mediator residing at level 2 in a two-level cluster-randomized trial. 

Live app at: <br>
<https://powerupr.shinyapps.io/index/> 

**Acknowledgement**:

This work is supported by National Science Foundation through a collaborative research grant titiled “Power Analyses for Moderator and Mediator Effects in Cluster Randomized Trials” to Benjamin Kelcey (Award Number: 1437679), Jessaca Spybrook (Award Number:1437692), and Nianbo Dong (Award Number: 1437745).



