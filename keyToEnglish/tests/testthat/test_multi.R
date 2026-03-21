test_that('random_sentence_check',{
  expect_equal(length(generate_random_sentences(10)), 10)

})

test_that('hashed_sentence_check', {
  expect_equal(hash_to_sentence(letters), c("RosyLeatherKangarooSoftensCorkWelts", "IllicitTiledBrambleThreatensVinylKnives",
                                            "AncientExtradimensionalMacadamiaConcealsChitinBazaars", "ImplodingWhiteChumpSuesTealNeckties",
                                            "FoolishBrassHedgerowThreatensTarnishedDuelists", "FabulousBrassLiverGluesFiberglassNymphs",
                                            "BlissfulIvoryIronsightDropsElectrumSliders", "AutomaticPurpleHamburgerVexesHarlequinHoops",
                                            "SuburbanBambooCompensatorHarassesPeridotDetectives", "SomberLeadPencilpendantRubsAnodizedKings",
                                            "AgileRuggedKidnapperBoilsIcyBeignets", "TroublingSkeletalCodpieceYanksCeruleanWarrens",
                                            "ZealousAqueousSingerBefriendsSharpWelders", "DisinheritedDottedKitchenSwallowsTallAles",
                                            "RichCleanJokerDefacesTransparentSardines", "ConsciousSpeckledSpellBuriesLiquidVampires",
                                            "PrimitiveKitchArmyBaptizesMoltenGingers", "BanefulShaggyHipWhipsOchreMagnates",
                                            "ThriftyShortVagrantVeneratesCyanSabres", "PalatableIcyLawnmowerVexesConstantanShoals",
                                            "MythicalOilyBoobyWhitensBasaltSojourners", "WarmLacySeraphBuysElectrumSamurai",
                                            "UnhappyHardwoodFighterDressesPyriteMasts", "ExplodingWhiteVizierExterminatesLeatherRavens",
                                            "CapriciousEbonyRiggerVandalizesTweedyMissiles", "SillyAzureBaritonePeppersSiliconPaellas"
  ))
})

test_that('random_sentence_structure', {
  expect_true(
    all(grepl('^[A-Z][a-z ]+\\.$', generate_random_sentences(5)))
  )
})

test_that('random_sentence_structure_fast', {
  expect_true(
    all(grepl('^[A-Z][a-z ]+\\.$', generate_random_sentences(5, fast=TRUE)))
  )
})

