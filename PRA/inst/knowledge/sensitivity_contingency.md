# Sensitivity Analysis and Contingency Reserves

## Sensitivity Analysis

### Purpose
Sensitivity analysis identifies which tasks or risk factors contribute most to total project uncertainty. This guides risk mitigation by focusing attention on the tasks where reducing uncertainty has the greatest impact on the project outcome.

### Method
PRA computes sensitivity by measuring the variance contribution of each task to the total project variance. Tasks with higher sensitivity values are the primary drivers of overall uncertainty.

### Interpreting Results
- Results are returned as a numeric vector with one value per task
- Higher values indicate tasks that contribute more to total variance
- Use these to prioritize risk mitigation efforts: reducing uncertainty in high-sensitivity tasks yields the greatest reduction in overall project risk
- Typically presented as a tornado chart (horizontal bar chart sorted by sensitivity)

### Input Format
Sensitivity analysis uses the same input format as Monte Carlo simulation: a list of task distributions and an optional correlation matrix. This makes it easy to run both analyses on the same project data.

## Contingency Reserves

### Purpose
Contingency reserves provide a financial buffer to account for project uncertainty. The reserve amount is the difference between a high-confidence estimate and the baseline estimate.

### Calculation
Contingency = P_high - P_base

Where:
- **P_high** (default 0.95): The 95th percentile of the total cost/duration distribution. There is a 95% probability the actual value will not exceed this.
- **P_base** (default 0.50): The median (50th percentile) of the distribution. This represents the "most likely" total.

The contingency function takes the Monte Carlo simulation results as input, so run `mcs()` first, then pass the result to `contingency()`.

### Common Confidence Levels
- **P80 contingency**: Moderate confidence. Common in commercial projects.
- **P95 contingency**: High confidence. Common in government and defense projects.
- **Management reserve**: Additional buffer beyond contingency for unknown unknowns. Typically 5-10% of BAC.

### Workflow
1. Define task distributions
2. Run Monte Carlo simulation: `mcs(num_sims, task_dists, cor_mat)`
3. Compute contingency: `contingency(mcs_result, phigh = 0.95, pbase = 0.50)`
4. Run sensitivity: `sensitivity(task_dists, cor_mat)`
5. Focus risk mitigation on high-sensitivity tasks to reduce the needed contingency

## References

- GAO Cost Estimating and Assessment Guide (GAO-20-195G).
- GAO Schedule Assessment Guide (GAO-16-89G).
