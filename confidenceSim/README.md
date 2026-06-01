# Highly Customizable, Parallelized Simulations of Frequentist Confidence Clinical Trials

Simulate one or many frequentist confidence clinical trials based on a specified set of parameters.  From a two-arm, single-stage trial to a perpetually run Adaptive Platform Trial, this package offers vast flexibility to customize your trial and observe operational characterisitics over thousands of instances.

The analysis of trial results is carried out using the `confidenceCurves` package (on [CRAN](https://cran.r-project.org/package=confidenceCurves) and [GitHub](https://github.com/FredaWerdiger/confidenceCurves)) and all details about frequentist confidence analysis may be found there.

## Trial Designs
The complexity of the design of the trial being simulated varies from a two-arm one-stage trial, to a Multi-Arm Multi-Stage (**MAMS**) Trial through to an **Adaptive Platform Trial**. The trial design features are specified by a parameter list that is fed into the `runSingleTrial` function via the `inputs` field. There is an example parameter list named ```inputs``` which may be loaded by using:
```
data(inputs)
```
Some examples of designs are as follows:

### Standard Randomised Control Trial (RCT)
With `confidenceSim` you may design a randomised trial comparing a single experimental treatment to control with a single analysis point which is the primary analysis. Primary analysis of treatment efficacy is based on confidence in treatment benefit. To design a single-stage trial, the `looks` fields would be a single number, respresenting the maximum sample size. The length of `looks` represents the number of stages. 

### Group sequential trial
A group sequential trial involves multiple interim analysis with options to stop early for efficacy, harm or futility. With `confidenceSim`, decisions to stop early are made on the basis of confidence analysis. To control for Type I error, thresholds are derived with the `getGSdesign` function using an alpha-spending function. However, there are numerous options for trial monitoring. For example, by setting `multiarm.mode = 'MONITOR FUTILITY'` the trial will only be stopped early for futility, and a test for efficacy only undertaken in the final, primary, analysis. In that case a fixed `alpha=0.05` may be set. Otherwise `multiarm.mode = 'CONFIDENCE-BASED'` uses confidence monitoring for futility, efficacy and inferiority (harm). 
<figure>

  <img width="848" height="226" alt="ICH confidence trial graphic" src="https://github.com/user-attachments/assets/d8276f53-380f-4625-b495-49430a1143dd" />
<caption>
  Figure 1. Example of a 2-arm 6-stage group sequential trial with multiple interim analysis points.
</caption>

</figure>

### MAMS
A multi-arm multi-stage trial involves multiple experimental treatments compared to a single control, and multiple analysis points. For example, consider a design evaluating efficacy of two experimental treatments against a common control over two stages. At the end of the first stage, only the best performing treatment will go on, with all remaining patients assigned to either this arm or control. In this case, all three response rates (control + 2 treatments) must be specified in the `resprate` parameter in `inputs`, with control as the first rate. Then, by setting `multiarm.mode='SELECT THE BEST'`, the interim analysis will carry the best treatment forward to the final analysis. Even if the second best arm exceeds the confidence thresholds, it will not carry forward. In this R package, multiple arms are compared to control with a <strong>pairwise analysis</strong> and we <strong>only control for multiplicity across stages, not arms</strong>. The same confidence threshold is applied to each pairwise analysis. In this case, each pair is treated like a separate trial.

### Adaptive Platform Trial
The `perpetual` setting for `inputs` designates if you want to run the trial perpetually, like an adaptive platform trial, adding new arms once old arms are dropped. If you want to run perpetually, the simulations will take new arms from the `resprate` field once another arm is dropped. Therefore, include *all available treatments* in `resprate` and specify how many will run at any given time with the `alloc.ratio` field. For example, you may have six treatments to use - in which case there should be seven different response rates in `resprate`, but you only run three study arms - in which case, `alloc.ratio` should be of length 3, like `alloc.ratio = c(1,1,1)`. As with MAMS, each arm goes through its own trial, with comparison to **concurrent controls** only. The `runSingleTrial` tracks the trial of each treatment separately, knowing if an analysis point is an interim analysis or a final analysis based on the number of interims and the number of patients on the arm so far. The fields `alloc.ratio` and `looks` determines the journey for each treatment, and the fields `resprate` and `ppm` determine how long the platform trial itself runs for, based on how many people are recruited and how many treatments there are. When treatments run out, the `perpetual` setting is turned off. When people run out, the trial stops. 

## Outcome measures and Data types
In `runSingleTrial`, the primary outcome may be binary, ordinal or continuous. 

### Binary data
If the primary outcome measure is binary, patient response is 0 or 1. In this case, the point estimate can be specified in `estimator.type` as either a risk difference ('risk diff'), odds ratio ('odds ratio'), or risk ratio ('risk ratio'). This instruction is then passed to `makeConfidenceCurves` and calculated there. To specify response rates for binary data into `inputs`, the function expects a list with each item giving the probability of a positive '1' response. For example, a two arm trial with binary outcome may have `resprate = c(0.4, 0.6)` with probability of control response being 0.4 and treatment response being 0.6.

A few notes:

#### Odds Ratio and Risk Ratio
If a ratio estimate is used, it will be converted to the log scale and the `mean` value for the results returned by `runSingleTrial` is on the log scale. Further, it is very important to note that it **assumes that smaller is better***. So that 0 is a better result than one. The `dir.benefit` field for `makeConfidenceCurves` is set to 0, meaning a point estimate of less than zero indicates benefit to treatment over control.

#### Risk Rifference
For risk difference, the linear and not log scale is used. As compared with above, a point estimate of greater than zero indicates benefit to treatment over control.

### Ordinal data
If the primary outcome is ordinal, the point estimate is a generalised odds ratio and is calculated by the `genodds` R package. In this scheme, smaller is better. If the ordinal outcomes take of values 1 through 6, it is assumed that 1 is the best outcome and 6 is the worst.  To specify response rates, `resprate` expects a list of lists, with each list corresponding to a different arm. Each arm should have the probability of response for each ordinal value. For a two-arm trial predicting response on a 6-point ordinal outcome, the structure is:

```
resprate = list(control = c(0.1, 0.1, 0.2, 0.5, 0.6, 0.6), treatment = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
```

### Continuous data
If the primary outcome is continuous, the point estimate is a Difference of Means, where a positive value denotes benefit to treatment over control. In this case, response rates should be fed to `resprate` as a list of lists, which each list combining the mean and standard deviation for each arm. For example:

```
resprate = list(control=c(0,1), treatment=c(0.2,1)
```
