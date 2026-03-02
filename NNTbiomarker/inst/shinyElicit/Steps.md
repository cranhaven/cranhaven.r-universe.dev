#  DEMO report for Statistical Considerations
-----

### Step 1. Introduction: the clinical scenario:

- What is the clinical challenge the biomarker should address?

Prognosis of cutaneous T cell
                    lymphoma (CTCL). In early stages of CTCL, patients (Stages IA-IIA)
                    usually do well and have slowly progressive disease, which does not
                    require aggressive therapy associated with substantial side effects.
                    However, about 15% of these patients have unexpected progressive course
                    and rapid demise.. 



### Step 2.  Patients and options :

- Who are the patients to help? What are the clinical decision options?

The biomarker is intended to help CTCL patients in Stages IA-IIA,
by identifying who should receive 
**Immediate aggressive treatment** and who should receive **Standard care**.


### Step 3. Criterion for a useful biomarker: Defining the NNT discomfort zone:

- What region of NNTs (number needed to treat) make both choices uncomfortable? <br>What NNT's for positive and negative test groups would make thse decision clear-cut?  <br>What predictive values for positive and negative tests will give NNT's making the decision clear-cut?

Currently, the proportion of patients who should receive prevalence is *number needed to treat* to help one (*NNT*) is 
The biomarker test will be useful can create a clinical
consensus supporting using the test for clinical decisions. 
if the NNT among test-positive patients, *NNT<sub>Pos</sub>*, is less than *NNT<sub>Lower</sub>*  = 3, 
and 
if the NNT among test-negative patients, *NNT<sub>Neg</sub>*, is greater than *NNT<sub>Upper</sub>*  = 50.

Therefore we choose targets *NNT<sub>Pos</sub>* = 3  and  *NNT<sub>Neg</sub>* = 84.
This performance should suffice to create a clinical
consensus supporting using the test for clinical decisions.
These values correspond to *positive predictive value = PPV* = 0.3333333, 
and *negative predictive value = NPV* = 0.9880952.

### Step 4.  Specific clinical benefit:

- How, in detail, will patients be helped by a test that achieves the criteria?

If the biomarker test achieves these predictive values,
the benefit to patients will be a biomarker progression risk model
                    that is able to classify patients into high and low risk groups will
                    enable personalized and more aggressive therapy for the patients at
                    highest risk for progression..  

### Step 5. Prospective study requirements:

- Given the target predictive values how large should a prospective study be, and how long the follow-up?

The retrospective study will 
recruit 30 patients.
. 
If the test divides the 30 
patients into roughly 
50% positive and 
50% negative, 
and if the estimates
match the hoped-for values 
*NNT<sub>Pos</sub>* = 3  and  *NNT<sub>Neg</sub>* = 84, 
then the confidence intervals would be
(0.114, 0.577) for *PPV*, and 
(0.827, 1) for *NPV*, or equivalently
(1.733, 8.776)  for *NNT<sub>Pos</sub>*, and 
(5.793, 2.6989145 &times; 10<sup>4</sup>) for *NNT<sub>Neg</sub>*. 



### Step 6. Retrospective study requirements:

- Given a prevalence value, what sensitivity and specificity do we hope for, and what should the sample sizes be to estimate them sufficiently?


The proportion of patients with rapid progression is assumed to be 
8%.
Combining that with the target *PPV* and
*NPV*, the required
sensitivity (*SN*) and specificity (*SP*) are 88% and 85%,
respectively (contra-Bayes Theorem). 
To get a sense of the accuracy of
anticipated estimates in the retrospective (case/control) portion of the
study, we consider anticipated results for samples sizes 22 cases 
and 40
controls. For example, if the estimates 
*SN* = 19/22 
=  88 % and
*SP* = 34/40 
=  85  % 
are observed, then the corresponding confidence intervals
will be 
(0.679, 0.96)
for *SN*, and 
(0.717, 0.935)
for *SP*.
For *NNT<sub>Pos</sub>* and for *NNT<sub>Neg</sub>*,
the Bayes predictive
intervals will be 
(1.875, 4.987)
for *NNT<sub>Pos</sub>* , and 
(30.724, 244.177)
for *NNT<sub>Neg</sub>* . 
(These predictive intervals derive from assuming independent Jeffreys
priors for *SN* and *SP*, sampling from joint independent posteriors
incorporating the anticipated results, and applying Bayes theorem).
