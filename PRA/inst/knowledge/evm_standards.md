# Earned Value Management (EVM)

## Overview

Earned Value Management is a project performance measurement methodology that integrates scope, schedule, and cost data. It provides early warning indicators of project health and enables forecasting of final cost and schedule outcomes.

## Core Metrics

### Baseline Metrics
- **BAC (Budget at Completion)**: Total planned budget for the project.
- **PV (Planned Value)**: The authorized budget for work scheduled to be completed by a given time. PV = BAC * planned_percent_complete.
- **EV (Earned Value)**: The authorized budget for work actually completed. EV = BAC * actual_percent_complete.
- **AC (Actual Cost)**: The realized cost for work performed in a given period.

### Variance Indicators
- **SV (Schedule Variance)**: SV = EV - PV. Positive means ahead of schedule.
- **CV (Cost Variance)**: CV = EV - AC. Positive means under budget.

### Performance Indices
- **SPI (Schedule Performance Index)**: SPI = EV / PV. Values > 1.0 indicate ahead of schedule.
- **CPI (Cost Performance Index)**: CPI = EV / AC. Values > 1.0 indicate under budget.

## Forecasting

### EAC (Estimate at Completion)
Three methods depending on assumptions about future performance:

- **Typical (CPI-based)**: EAC = BAC / CPI. Assumes future cost performance matches current CPI. Use when current trends are expected to continue.
- **Atypical**: EAC = AC + (BAC - EV). Assumes future work proceeds at the original planned rate. Use when current variances are one-time anomalies.
- **Combined (CPI * SPI)**: EAC = AC + (BAC - EV) / (CPI * SPI). Accounts for both cost and schedule performance. Use when schedule pressure affects costs.

### Other Forecasts
- **ETC (Estimate to Complete)**: ETC = EAC - AC, or ETC = (BAC - EV) / CPI. Remaining cost to finish.
- **VAC (Variance at Completion)**: VAC = BAC - EAC. Expected overrun (negative) or underrun (positive).
- **TCPI (To-Complete Performance Index)**: TCPI = (BAC - EV) / (BAC - AC) or (BAC - EV) / (EAC - AC). The CPI needed on remaining work to meet a target.

## Warning Thresholds

- CPI or SPI below 0.9 typically signals a project in trouble.
- Research shows that CPI rarely improves by more than 10% after a project is 20% complete.
- TCPI > 1.0 means the project must improve performance to meet its target.

## References

- DOE Earned Value Management Tutorial Series.
- DOE EVMS Interpretation Handbook (EVMSIH v2.0).
- ANSI/EIA-748 Earned Value Management Systems standard.
