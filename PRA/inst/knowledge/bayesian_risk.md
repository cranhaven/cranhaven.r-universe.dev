# Bayesian Inference for Risk Analysis

## Overview

Bayesian methods in project risk analysis allow you to compute the probability of risk events from root causes, and to update those probabilities as new information (observations) becomes available. This is particularly valuable in early project stages when data is sparse.

## Prior Risk Probability

Given a set of root causes C_1, C_2, ..., C_n, the overall probability of a risk event R is computed using the law of total probability:

P(R) = sum over i of: P(R|C_i) * P(C_i) + P(R|not C_i) * (1 - P(C_i))

Each root cause contributes independently to the risk. You need:
- **cause_probs**: P(C_i) for each root cause (probability the cause is present)
- **risks_given_causes**: P(R|C_i) (probability of risk if cause is present)
- **risks_given_not_causes**: P(R|not C_i) (probability of risk if cause is absent)

## Posterior Updating

When you observe whether a cause has actually occurred, you can update the risk probability using Bayesian inference. Observed causes are no longer uncertain - they become known (TRUE/FALSE), and the posterior probability accounts for this evidence.

Use `risk_post_prob()` with an `observed_causes` vector where:
- TRUE = cause was observed to have occurred
- FALSE = cause was observed to not have occurred
- NA = cause status is still unknown (remains at prior probability)

## Cost Distribution

Risk probabilities can be translated into cost distributions:
- **Prior cost distribution** (`cost_pdf`): Simulates project costs under prior risk probabilities. Each simulation draws whether each risk occurs (based on risk_probs), then samples from the corresponding cost distribution.
- **Posterior cost distribution** (`cost_post_pdf`): Same process but using updated (observed) risk states. Observed risks are known TRUE/FALSE; unobserved risks use their prior probabilities.

Parameters:
- **means_given_risks / sds_given_risks**: Mean and SD of additional cost if the risk occurs (one pair per risk).
- **base_cost**: Baseline project cost before any risk impacts.

## When to Use Bayesian vs. MCS

- **Bayesian**: Best when you have identifiable root causes with estimated probabilities, and you want to update estimates as the project progresses. Good for risk registers.
- **MCS**: Best when you have task-level distribution estimates and want total project cost/schedule distributions. Better for comprehensive uncertainty quantification.
- They can be combined: use Bayesian methods to estimate risk probabilities, then feed those into an MCS.

## References

- NASA Risk-Informed Decision Making Handbook (SP-2010-576).
- NASA Bayesian Schedule and Cost Risk Analysis.
