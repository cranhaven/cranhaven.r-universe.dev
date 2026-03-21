test_that('basic_word_list_check',{
  expect_equal(length(keyToEnglish(letters)), 26)

})

test_that('hash_stability_check',{
  expect_equal(keyToEnglish(letters), c("AddressBearInfectiousRecordDeformity", "FragmentParticularlyHomologousDepthSurrender",
                                        "ProminentPrecipitateRelaxAnimationTubular", "VersionNationalBloomReptileMake",
                                        "LiabilityBiteFixedLikenessKnot", "FretStomachRoveCaptainPretty",
                                        "KernelBestYoungMeetSubordinate", "DrumServeNitrogenNeglectHind",
                                        "SpiderAstringentParticularReckonDropping", "ChildPearlProvideBehindOutline",
                                        "ImposingPointedToolToneFreshwater", "DisguiseParadiseYouthEntreatyRude",
                                        "AlphabetSerpentineIndefiniteSpecificArtillery", "InductionOccurSpeedVariationPermit",
                                        "MouseFermentBastardBeatPatch", "ProcureExceptionAssignAntecedentWhole",
                                        "UsageMembraneOrchestraBeginningInitiate", "FormedBendStarchSportInclose",
                                        "SuperiorBirdMushroomFaultAdherence", "EntrailsRushMorningExpenseGroove",
                                        "AbilityCitizenObservingSufferingBirth", "FundMomentIndiesEntryAnnual",
                                        "TabletNitrogenReadilyAtlanticAttached", "BluntMistakeExcellentCommunionWrongly",
                                        "GrantBreathUsageHumorRock", "TroopDarkenDigestiveResortProperly"
  ))


})
