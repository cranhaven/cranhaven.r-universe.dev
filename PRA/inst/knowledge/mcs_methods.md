# Monte Carlo Simulation for Project Risk Analysis

## Overview

Monte Carlo simulation (MCS) is a quantitative risk analysis technique that uses random sampling from probability distributions to estimate the range of possible project outcomes (cost, duration). It is the most widely used method for schedule and cost risk analysis in project management.

## When to Use

- When task-level uncertainty can be described with probability distributions
- When you need percentile-based estimates (P50, P80, P95) for budgeting or scheduling
- When tasks may be correlated and you need to capture joint uncertainty
- When management requires a confidence level for cost or schedule targets

## Distribution Selection

Choose distributions based on available information:

- **Normal**: Use when historical data suggests symmetric uncertainty around a mean. Parameters: mean, sd.
- **Triangular**: Use when you can estimate optimistic (a), most likely (c), and pessimistic (b) values. Most common in practice. Parameters: a (min), b (max), c (mode).
- **Uniform**: Use when all values in a range are equally likely, or when only a range is known. Parameters: min, max.

## Correlation

Tasks are often correlated (e.g., labor cost overruns affect multiple tasks). Ignoring positive correlation underestimates total project variance. Provide a correlation matrix to capture these dependencies. If no correlation matrix is specified, tasks are assumed independent.

## Interpreting Results

- **P50 (median)**: The value with a 50% chance of being exceeded. Often used as the "most likely" total.
- **P80**: Common threshold for project contingency budgeting. 80% confidence the actual value will not exceed this.
- **P95**: High-confidence estimate, often used for management reserve or worst-case planning.
- **Mean**: Expected value of the total. May differ from P50 for skewed distributions.
- **Standard deviation**: Measures spread of the total distribution.

## Convergence

Use at least 10,000 simulations for stable percentile estimates. Fewer simulations (1,000) may suffice for rough estimates but percentiles in the tails (P5, P95) require more samples.

## Contingency and Sensitivity

After running MCS, follow up with:
- **Contingency analysis**: Calculate the reserve needed as the difference between P80/P95 and P50.
- **Sensitivity analysis**: Identify which tasks contribute most to total variance, guiding risk mitigation priorities.

## References

- GAO Cost Estimating and Assessment Guide (GAO-20-195G), Chapter 12: Quantifying Risk and Uncertainty.
- RAND Corporation, Quantitative Risk Analysis for Project Management (WR-112).
