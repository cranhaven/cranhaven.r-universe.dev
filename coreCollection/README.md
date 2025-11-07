# coreCollection

R package for creating a core collection based on the provided distanceMatrix, required size of the core n and optionally a set of preselected accessions to be included into the core.

Based on a provided `distanceMatrix` and required number `n` of accessions
within the core, a random set of accessions is created, implicitly dividing the full
population into initial groups based on the nearest randomly chosen random accession. If a
set of `preselected` accessions is provided, this initial division is adjusted using the
`adjustedGroupMethod`. Then, using the `coreSelectMethod` in the `algorithm`, the
core accessions within these groups are calculated, resulting in the final core collection.

### Usage

```R
CoreCollection(
  distanceMatrix,
  n,
  preselected = c(),
  coreSelectMethod = "A-NE",
  adjustedGroupMethod = "split",
  algorithm = "randomDescent",
  seed = NULL
)
```

### Arguments

| argument | description |
| :--- | :--- |
| distanceMatrix | A distance matrix; can be either a matrix or a dist|
|n | The number of items in the core|
| preselected	| An optional list of preselected accessions to be included in the core collection; the provided accessions should occur in the labels or rownames of the provided distanceMatrix |
| coreSelectMethod | The method for computing core accessions within the groups: A-NE (accession nearest entry), E-NE (entry nearest entry) or E-E (entry entry) |
| adjustedGroupMethod	| The method to handle adjusting groups when multiple preselected accessions occur within a single group: split to just split the initial groups with multiple accessions or recompute to recompute the division of accessions over the groups. |
| algorithm	| Algorithm applied to compute a solution: currently, only randomDescent is available |
| seed | The seed used when generating the core collection. If no seed is provided, a random seed is chosen and each time the recompute() method is called on the object, a new seed will be used. |
