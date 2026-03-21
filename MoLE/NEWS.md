Version 1.0.1:

SUBROUTINE: change

CHECKSUCCESS: adjusted for catering use of new VMATCH & collostruction frequency locally determined
CHECKMARKER: removed
DIE: typo corrected which led to improper saving of usageHistory
FIRSTINFIRSTOUT: order of constituents is now determined a la CANDIDATESCORE, using world$candidateScore
FIRSTSPEAKER: number of distinctions now dependent on modelparameters in object "world" 
FREQUPDATE: adjusted to correctly process number according to new generalizations (cf. GENERALIZE) & adjusted for catering use of new VMATCH 
FUSE: log message for graveyard$history was not saved properly previously. Now it is.
GENERALIZE: semantic role of argument of one-place verb no longer Actor per default, but Actor or Undergoer depending on role requirements & adjusted for catering use of new VMATCH & collostruction frequency locally determined
HISTORY: removed (didn't add much)
INTERPRET.INT: adjusted for catering use of new VMATCH
MULTIRUN: removed, as object "world" is locked and model parameters cannot be overwritten from within function. Instead of MULTIRUN, RUN can be used.
NOUNDESEMANTICIZATION: log message for graveyard$history was not saved properly previously. Now it is.
NOUNS: bug in selection of referential items for local person fixed.
PERSONUPDATE: new pronouns overwrite their source lexemes (instead of living side by side, which causes homonymy in the pronoun paradigm eventually, as first and second person target the same candidates). And log message for graveyard$history was not saved properly previously. Now it is.
PROCREATE: typo corrected (because of which frequency numbers were not reset at birth)
REDUCE: typo corrected (because of which markers didn't erode further)
REFCHECK: recruitment selection now by prominence (instead of match with original reference) & adjusted for catering use of new VMATCH & collostruction frequency locally determined
SUMMARY: error resolved that was caused by list of markers (which included markers that eroded away and hence were no longer available). And overview of markers is now made for last dead agent (instead of living one), for better comparison of frequency numbers.
VERBDESEMANTICIZATION: log message for graveyard$history was not saved properly previously. Now it is.
VERBMORPHOLOGY: adjusted for catering use of new VMATCH
VMATCH: noise is added by default, which is necessary for simulations with low-dimensional meaning space
WORDORDER: adjusted for catering use of new VMATCH

