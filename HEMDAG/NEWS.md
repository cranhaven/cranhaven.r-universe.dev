#### HEMDAG 2.7.4

###### Changes
- remove extra input parameter ``f.criterion`` from ``tpr.dag.cv``, ``tpr.dag.holdout``, ``find.best.f`` and ``compute.fmax``: type of F-measure used to select the best F-measure is always the harmonic mean between the average precision and recall (``f.criterion="F"``) and never the F-measure computed as average across examples (``f.criterion="avF"``);
- fix a minor bug in ``tpr.dag.holdout``;
- add warning checks in ``tpr.dag.cv`` and ``tpr.dag.holdout``;
- improve some test cases and manual;

#### HEMDAG 2.7.3

###### New Features
- add ``build.scores.matrix.from.list``;
- add ``build.scores.matrix.from.tupla``;
- add several test cases;

###### Changes
- streamline and lighten HEMDAG's hierarchical functions (namespace clearer and lighter);
- rename the following functions:
    - htd-dag:
        - ``Do.HTD`` --> ``htd.vanilla``;
        - ``Do.HTD.holdout`` --> ``htd.holdout``;
    - obozinski heuristic methods:
        - ``heuristic.max`` --> ``obozinski.max``;
        - ``heuristic.and`` --> ``obozinski.and``;
        - ``heuristic.or`` --> ``obozinski.or``;
        - ``Do.heuristic.methods`` --> ``obozinski.methods``;
        - ``Do.heuristic.methods.holdout`` --> ``obozinski.holdout``;
    - gpav:
        - ``GPAV`` --> ``gpav``;
        - ``GPAV.over.examples`` --> ``gpav.over.examples``;
        - ``GPAV.parallel`` --> ``gpav.parallel``;
        - ``Do.GPAV`` --> ``gpav.vanilla``;
        - ``Do.GPAV.holdout`` --> ``gpav.holdout``;
    - tpr-dag:
        - ``TPR.DAG`` --> ``tpr.dag``;
        - ``Do.TPR.DAG`` --> ``tpr.dag.cv``;
        - ``Do.TPR.DAG.holdout`` --> ``tpr.dag.holdout``;
    - utility functions:
        - ``get.parents`` --> ``build.parents``;
        - ``get.parents.top.down`` --> ``build.parents.top.down``;
        - ``get.parents.bottom.up`` --> ``build.parents.bottom.up``;
        - ``get.parents.topological.sorting`` --> ``build.parents.topological.sorting``;
        - ``get.children.top.down`` --> ``build.children.top.down``;
        - ``get.children.bottom.up`` --> ``build.children.bottom.up``;
        - ``check.DAG.integrity`` --> ``check.dag.integrity``;
        - ``do.subgraph`` --> ``build.subgraph``;
        - ``do.submatrix`` --> ``build.submatrix``;
        - ``do.stratified.cv.data.single.class`` --> ``stratified.cv.data.single.class``;
        - ``do.stratified.cv.data.over.classes`` --> ``stratified.cv.data.over.classes``;
        - ``do.unstratified.cv.data`` --> ``unstratified.cv.data``;
        - ``do.edges.from.HPO.obo`` --> ``build.edges.from.hpo.obo``;
    - performance metrics:
        - ``AUPRC.single.class`` --> ``auprc.single.class``;
        - ``AUPRC.single.over.classes`` --> ``auprc.single.over.classes``;
        - ``AUROC.single.class`` --> ``auroc.single.class``;
        - ``AUROC.single.over.classes`` --> ``auroc.single.over.classes``;
        - ``compute.Fmeasure.multilabel`` --> ``compute.fmax``;
- remove the following functions (no more needed):
    - ``Do.flat.scores.normalization``;
    - ``Do.full.annotation.matrix``;
- improve manual;
- make HEMDAG's documentation clearer and less redundant;

#### HEMDAG 2.6.1

###### Changes
- fix ``stringsAsFactors`` issue -- [link](https://developer.r-project.org/Blog/public/2020/02/16/stringsasfactors/index.html);

#### HEMDAG 2.6.0

###### Changes
- fix NAMESPACE notes in CRAN checks;
- add link to the GitHub repository ``obogaf::parser``;
- adjust link to read the docs;

#### HEMDAG 2.5.9

###### New Features
- add ``build.consistent.graph``;

###### Changes
- add some warning checks in functions that compute performance metrics;
- improve some graph utility functions;
- improve manual;
- improve tutorial on read the docs -- [link](https://hemdag.readthedocs.io);
- make namespace clearer;
- fix minor bugs;
- remove defunct functions;

#### HEMDAG 2.4.8

###### Changes
- fix a minor bug in ``Do.GPAV.holdout``;
- improve package description;

#### HEMDAG 2.4.7

###### New Features
- fix degenerate case in ``precision.at.all.recall.levels.single.class`` (labels are all negatives/positives);
- fix degenerate case in ``precision.at.given.recall.levels.over.classes`` (labels in a fold are all negatives/positives);
- fix degenerate case in ``do.stratified.cv.data.single.class`` (sampling of the labels with just one positive/negative);
- add input variable ``compute.performance`` to the following high level functions:
    - ``Do.TPR.DAG`` and ``Do.TPR.DAG.holdout``;
    - ``Do.HTD`` and ``Do.HTD.holdout``;
    - ``Do.GPAV`` and ``Do.GPAV.holdout``;
    - ``Do.heuristic.methods`` and ``Do.heuristic.methods.holdout``;

###### Changes
- improve manual;

#### HEMDAG 2.3.6

###### New Features
- add ``lexicographical.topological.sort``;

###### Changes
- fix minor bugs;
- improve manual;

#### HEMDAG 2.2.5

###### New Features
- precision-recall performance computed through ``precrec`` package:
    - add ``precision.at.all.recall.levels.single.class``;
    - ``PXR.at.multiple.recall.levels.over.classes`` --> ``precision.at.given.recall.levels.over.classes``;
- improve IO functions: the extension of the input or output file can be or plain text (``.txt``) or compressed (``.gz``);

###### Changes
- fix minor bugs;
- improve manual;

#### HEMDAG 2.2.4

###### Changes
- fix ``CRAN`` Package Check Results: remove unneeded header and define from ``GPAV C++`` source code

#### HEMDAG 2.2.3

###### New Features
- add ``GPAV`` algorithm (Burdakov et al., *Journal of Computational Mathematics*, 2006 -- [link](https://doi.org/10.1007/0-387-30065-1_3));
- Embed ``GPAV`` algorithm in the top-down step of the functions ``TPR.DAG``, ``Do.TPR.DAG`` and ``Do.TPR.DAG.holdout``;
- Some functions have been defunct. To know the defunct functions just typing in the R environment: ``help("HEMDAG-defunct")``;

###### Changes
- improve manual;

###### AUTHOR
- add **Alessandro Petrini** as author for his contribution in writing the ``C++`` code of ``GPAV`` algorithm;

#### HEMDAG 2.1.3

###### Changes
- various fixes from 2.1.2

#### HEMDAG 2.1.2

###### New Features
- improve performance metrics:
    - add ``compute.Fmeasure.multilabel``;
    - add ``PXR.at.multiple.recall.levels.over.classes``;
    - all the performance metrics (``AUPRC``, ``AUROC``, ``FMM``, ``PXR``) can be computed either **one-shot** or averaged **across folds**;

- improve the high-level hierarchical ensemble functions:
    - embed the new performance metric functions;
    - add the parameter ``metric``: maximization by ``FMAX`` or ``PRC`` (see manual for further details);
    - add some checkers (warning/stop messages) to make the library more user-friendly;

###### Changes
- improve manual;

#### HEMDAG 2.0.1

###### Changes
- fix bug in ``do.stratified.cv.data.single.class``;

#### HEMDAG 2.0.0

###### New Features
- add ``TPR-DAG``: function gathering several hierarchical ensemble variants;
- add ``Do.TPR.DAG``: high-level function to run ``TPR-DAG`` **cross-validated** experiments;
- add ``Do.TPR.DAG.holdout``: high-level functions to run ``TPR-DAG`` **holdout** experiments;

- The following ``TPR-DAG`` and ``DESCENS`` high-level functions were remove:
    - ``Do.tpr.threshold.free``;
    - ``Do.tpr.threshold.cv``;
    - ``Do.tpr.weighted.threshold.free.cv``;
    - ``Do.tpr.weighted.threshold.cv``;
    - ``Do.descens.threshold.free``;
    - ``Do.descens.threshold.cv``;
    - ``Do.descens.weighted.threshold.free.cv``;
    - ``Do.descens.tau.cv``;
    - ``Do.descens.weighted.threshold.cv``;
    - ``Do.tpr.threshold.free.holdout``;
    - ``Do.tpr.threshold.holdout``;
    - ``Do.tpr.weighted.threshold.free.holdout``;
    - ``Do.tpr.weighted.threshold.holdout``;
    - ``Do.descens.threshold.free.holdout``;
    - ``Do.descens.threshold.holdout``;
    - ``Do.descens.weighted.threshold.free.holdout``;
    - ``Do.descens.tau.holdout``;
    - ``Do.descens.weighted.threshold.holdout``;

> NOTE: all the removed functions can be run opportunely by setting the input parameters of the new high-level function ``Do.TPR.DAG`` (for **cross-validated** experiments) and ``Do.TPR.DAG.holdout`` (for **hold-out** experiments);

###### Changes
- improve manual;

#### HEMDAG 1.1.1

###### New Features
- add ``DESCENS`` algorithm;
- add Heuristic Methods ``Max``, ``And``, ``Or`` (Obozinski et al., Genome Biology, 2008 -- [link](https://genomebiology.biomedcentral.com/articles/10.1186/gb-2008-9-s1-s6));
- add ``tupla.matrix`` function;

###### Changes
- improve manual;
- add link to the GitHub repository ``HPOparser`` (note: from version ``2.6.0`` ``HPOparser`` was changed in ``obogaf::parser``);
- add ``CITATION`` file;

#### HEMDAG 1.0.0

###### Package Genesis
