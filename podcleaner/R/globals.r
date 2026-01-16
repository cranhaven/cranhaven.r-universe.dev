
# Clean ####

## Addresses ####

### globals_address_names ####

#' Place names in address entries
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   place names in directory address entries. For each place name a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for place name matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_address_names <- tibble::tribble(
  ~pattern,                                                           ~replacement,                        ~ignore_case,
  "\\bAbbotsf?[ou]?r?d?\\b",                                          "Abbotsford",                        TRUE,
  "\\bAbbotsford.+\\s",                                               "Abbotsford ",                       TRUE,
  "\\bAbbotsford\\s(?:p.+)?l(?=[\\b.$])",                             "Abbotsford Place",                  TRUE,
  "\\bAbbotsfo.*?r(?:oa)?d\\b",                                       "Abbotsford",                        TRUE,
  "\\bAber(?:corn)?\\b",                                              "Abercorn",                          TRUE,
  "\\bAbercr?o?(?:m|ni)?(?:by)?\\b",                                  "Abercromby",                        TRUE,
  "\\bAbercrombyst\\b",                                               "Abercromby Street",                 TRUE,
  "\\bAuchincampbell\\b",                                             "Auchincampbell",                    TRUE,
  "\\bAuchingramont\\b",                                              "Auchingramont",                     TRUE,
  "\\bAdelphist\\b",                                                  "Adelphi Street",                    TRUE,
  "\\bAlbany\\sp[a-z\\-]+",                                           "Albany Place",                      TRUE,
  "\\bAlpinest\\b",                                                   "Alpine Street",                     TRUE,
  "\\bAn((?!n\\s))(?(1)d?e?r?s?t?o?n?\\b)",                           "Anderston",                         TRUE,
  "\\bA[nu]ders[lt]o(?:n|rs|u)\\b",                                   "Anderston",                         TRUE,
  "\\bAnderstreet\\b",                                                "Anderston",                         TRUE,
  "\\b[A-Z]\\s\\bAnderston\\b",                                       "Anderston",                         TRUE,
  "\\bA[nu]dersto?n\\b",                                              "Anderston",                         TRUE,
  "\\bAndr?e?w\\b[,.]?",                                              "Andrew",                            TRUE,
  "\\bAndrew(?=\\s(?:Square|Street))",                                "Andrews",                           TRUE,
  "\\bAnn\\s?fie?(?:ld|hl)?\\b",                                      "Annfield",                          TRUE,
  "\\bApsle[jy]s?\\b",                                                "Apsley",                            TRUE,
  "\\bArde[nu]caple\\b",                                              "Ardencaple",                        TRUE,
  "\\bArdgowan\\b",                                                   "Ardgowan",                          TRUE,
  "\\bArg[jvuy]?-?[dl']?[ae]?\\b",                                    "Argyle",                            TRUE,
  "\\bArglye\\b",                                                     "Argyle",                            TRUE,
  "\\bArgyle\\s?s[it]\\b",                                            "Argyle street",                     TRUE,
  "\\bAl?rlin(?:g|<r)ton\\b",                                         "Arlington",                         TRUE,
  "\\bAthole\\b",                                                     "Athole",                            TRUE,
  "\\bAuchenheglish\\b",                                              "Auchenheglish",                     TRUE,
  "\\bAvondale\\b",                                                   "Avondale Street",                   TRUE,
  "\\bAvondalest(?:reet)?\\b",                                        "Avondale Street",                   TRUE,
  "\\bAyton\\s+([a-z]+).+Blackfriar.+",                               "Ayton \\1, off Blackfriars Street", TRUE,
  "\\bBack\\b",                                                       "Back",                              TRUE,
  "(Baird\\sStreet)(.*(glebe|westmuir).*)?.*",                        "\\1\\2",                            TRUE,
  "\\bBa[hl](?:rn|m|n)an[nu]o",                                       "Balmanno",                          TRUE,
  "\\bBat(?:h|li)\\b",                                                "Bath",                              TRUE,
  "\\bB[ao]\\s?[rt]ony\\b",                                           "Barony",                            TRUE,
  "\\bBedfo.*?r(?:oa)?d\\b",                                          "Bedford",                           TRUE,
  "\\bBelgr[ao]ve\\b",                                                "Belgrave",                          TRUE,
  "\\bBell\\b",                                                       "Bell",                              TRUE,
  "\\bBellgr[ao]ve\\b",                                               "Bellgrove",                         TRUE,
  "\\bBellgrove\\s?st\\b",                                            "Bellgrove Street",                  TRUE,
  "\\bBenview\\b",                                                    "Benview",                           TRUE,
  "\\bBerkley\\b",                                                    "Berkley",                           TRUE,
  "\\bBirdston\\b",                                                   "Birdston",                          TRUE,
  "\\bBishop\\b",                                                     "Bishop",                            TRUE,
  "\\bBishopb(?:riggs)?\\b",                                          "Bishopbriggs",                      TRUE,
  "\\bBlackfr?(?:iars)?\\b\\'?",                                      "Blackfriars",                       TRUE,
  "\\bBlantyre\\b",                                                   "Blantyre",                          TRUE,
  "\\bBlythsw?(?:ood)?\\b",                                           "Blythswood",                        TRUE,
  "both\\s?w?(?:ell)?",                                               "Bothwell",                          TRUE,
  "\\bBoturic[hk]\\b",                                                "Boturich",                          TRUE,
  "\\bBowes-?hill\\b",                                                "Boweshill",                         TRUE,
  "\\bBrea?dalban[ae]\\b",                                            "Breadalbane",                       TRUE,
  "\\bBroad\\b",                                                      "Broad",                             TRUE,
  "\\bBroomwa.*?r(?:oa)?d\\b",                                        "Broomward",                         TRUE,
  "\\bBrown\\b",                                                      "Brown",                             TRUE,
  "\\bBridgeg(?:eg)?a?t?e?\\b",                                       "Bridgegate",                        TRUE,
  "\\bBridgegatest(?:reet)?\\b",                                      "Bridgegate Street",                 TRUE,
  "\\bBridge(?:to?)?n?\\b\\??",                                       "Bridgeton",                         TRUE,
  "\\bBridge?t(?:[ao][nu])?\\b",                                      "Bridgeton",                         TRUE,
  "\\bBridgn\\b",                                                     "Bridgeton",                         TRUE,
  "\\b(?<=Street|\\.)\\s?Bridgeton\\b",                               ", Bridgeton",                       TRUE,
  "\\bBroo?[mi]n?([^h]?)(?(1)(?:[eit]+)?\\s?(?:law)?j?\\b)",          "Broomielaw",                        TRUE,
  "\\bBroo[mn]i+el(?:aw)?\\b",                                        "Broomielaw",                        TRUE,
  "\\bBrunsw?(?:ic)?k?\\b",                                           "Brunswick",                         TRUE,
  "\\bB[nu][ce][cgi]l?(?:euch)?\\b",                                  "Buccleuch",                         TRUE,
  "\\bBuccleuchst\\b",                                                "Buccleuch Street",                  TRUE,
  "\\bBuch((?!an street))(?(1)a?n?(?:an)?\\b),?",                     "Buchanan",                          TRUE,
  "\\bBurrelsla\\b",                                                  "Burrels Lane",                      TRUE,
  "\\bBuchananst\\b",                                                 "Buchanan Street",                   TRUE,
  "\\bCadogan\\b",                                                    "Cadogan",                           TRUE,
  "\\bCa?l(?:t?o?n?)?\\b",                                            "Calton",                            TRUE,
  "\\bCalt(?:o[nu])?\\b",                                             "Calton",                            TRUE,
  "\\bCaltonmouth\\b",                                                "Calton-mouth",                      TRUE,
  "\\bCa(?:m|in)b[ir]?(?:id)?g?e?\\b",                                "Cambridge",                         TRUE,
  "\\bCamden\\d",                                                     "Camden",                            TRUE,
  "\\bCampb?e?l?l?\\b",                                               "Campbell",                          TRUE,
  "\\bCampsid?e\\b",                                                  "Campside",                          TRUE,
  "\\bCann?(?:ing)?\\b",                                              "Canning",                           TRUE,
  "\\bCanningst\\b",                                                  "Canning Street",                    TRUE,
  "\\bCandle?r?i?[gs]?[gs]?[gs]?\\b-?",                               "Candleriggs",                       TRUE,
  "\\bCanon\\b",                                                      "Canon",                             TRUE,
  "\\bCarlt?o?[nu]?\\b",                                              "Carlton",                           TRUE,
  "\\bCarnarv(?:on)?\\b[.,]?",                                        "Carnarvon",                         TRUE,
  "\\bCarri[ce]k\\b",                                                 "Carrick",                           TRUE,
  "\\bCarrickst\\b",                                                  "Carrick Street",                    TRUE,
  "\\bCastle\\b",                                                     "Castle",                            TRUE,
  "\\bCathedr?(?:al)?\\b",                                            "Cathedral",                         TRUE,
  "\\bCavend(?:ish)?\\b",                                             "Cavendish",                         TRUE,
  "\\bCharlotte\\b",                                                  "Charlotte",                         TRUE,
  "\\bCharle[3s]\\b",                                                 "Charles",                           TRUE,
  "\\bCha[rd](?:lotte)?\\b",                                          "Charlotte",                         TRUE,
  "\\bCharing\\s?Cross\\b",                                           "Charing Cross",                     TRUE,
  "\\bChatham\\b",                                                    "Chatham",                           TRUE,
  "\\bCheaps(?:ide)?\\b",                                             "Cheapside",                         TRUE,
  "\\bC[il]arem(?:on)?t?\\b",                                         "Claremont",                         TRUE,
  "\\bCleland\\b",                                                    "Cleland",                           TRUE,
  "\\bCl[vy]de\\b",                                                   "Clyde",                             TRUE,
  "\\bClydefo.*?r(?:oa)?d\\b",                                        "Clydeford",                         TRUE,
  "\\bC[oq][bh]urg\\b",                                               "Coburg",                            TRUE,
  "\\bCoatbridge\\b",                                                 "Coatbridge",                        TRUE,
  "\\bCollege\\b",                                                    "College",                           TRUE,
  "\\bComm?(?:er)?c?e?\\b",                                           "Commerce",                          TRUE,
  "\\bCommercest\\b",                                                 "Commerce Street",                   TRUE,
  "\\bCommercial\\b",                                                 "Commercial",                        TRUE,
  "\\bCopeland\\b",                                                   "Copeland",                          TRUE,
  "\\bCow[ces]?[au]?d?[ci]?\\s?[dlns]?(?:e[nu])?s?\\b",               "Cowcaddens",                        TRUE,
  "\\bCraignestock\\b",                                               "Craignestock",                      TRUE,
  "\\bCraigrownie\\b",                                                "Craigrownie",                       TRUE,
  "\\bCranston\\b",                                                   "Cranston",                          TRUE,
  "\\bCrawfo.*?r(?:oa)?d\\b",                                         "Crawford",                          TRUE,
  "\\bCrooksto[nu]\\b",                                               "Crookston",                         TRUE,
  "\\bCross[bh]ill\\b",                                               "Crosshill",                         TRUE,
  "\\bCrown\\b",                                                      "Crown",                             TRUE,
  "\\bCroy\\b",                                                       "Croy",                              TRUE,
  "\\bCrown\\s?p(?:oint)?\\b",                                        "Crown Point",                       TRUE,
  "\\bCumb(?:er)?l?a?n?d?\\b",                                        "Cumberland",                        TRUE,
  "\\bDale\\b",                                                       "Dale",                              TRUE,
  "\\bDalh?(?:ousie)?\\b",                                            "Dalhousie",                         TRUE,
  "\\bDalm?(?:a[ir])?j?[an]?(?:[ao][ce])?k?(?=\\b|rd)",               "Dalmarnock",                        TRUE,
  "\\bDahnarnock\\b",                                                 "Dalmarnock",                        TRUE,
  "\\bDalmarnockrd\\b",                                               "Dalmarnock Road",                   TRUE,
  "\\bDix[eo]?n\\b",                                                  "Dixon",                             TRUE,
  "\\bD[eo]b(?:bie)?'?s?\\.?\\s?[il]?o?a?n?\\b",                      "Dobbies Loan",                      TRUE,
  "\\bDoug(?:las)?\\b",                                               "Douglas",                           TRUE,
  "\\bDove\\s?hill\\b",                                               "Dovehill",                          TRUE,
  "\\bDowanhill\\b",                                                  "Dowanhill",                         TRUE,
  "\\bDunclutha\\b",                                                  "Dunclutha",                         TRUE,
  "\\bDry\\s?gate\\b",                                                "Drygate",                           TRUE,
  "\\bDuke\\b",                                                       "Duke",                              TRUE,
  "\\bDukest\\b",                                                     "Duke Street",                       TRUE,
  "\\bDumb?(?:ar)?t?o?n?\\b",                                         "Dumbarton",                         TRUE,
  "\\bD[nu][nu]?d?(?:as)?\\b",                                        "Dundas",                            TRUE,
  "\\bDundasst\\b",                                                   "Dundas Street",                     TRUE,
  "\\bDu(?:n|ri)l?(?:op)?\\b",                                        "Dunlop",                            TRUE,
  "\\bEgli[nu]t?(?:on)?\\b",                                          "Eglinton",                          TRUE,
  "\\bEglintonst\\b",                                                 "Eglinton Street",                   TRUE,
  "\\bElm\\s?ba[nu]k\\b",                                             "Elmbank",                           TRUE,
  "\\bElderslie\\b",                                                  "Elderslie",                         TRUE,
  "\\bEldo[nu]\\b",                                                   "Eldon",                             TRUE,
  "\\bElgin\\b",                                                      "Elgin",                             TRUE,
  "\\bElmbank\\b",                                                    "Elmbank",                           TRUE,
  "\\bE+nfield\\b",                                                   "Enfield",                           TRUE,
  "\\bEn(?:och)?\\b",                                                 "Enoch",                             TRUE,
  "\\bFa[ir]lane",                                                    "Farlane",                           TRUE,
  "\\bFairlie\\b",                                                    "Fairlie",                           TRUE,
  "\\bFerguson\\b",                                                   "Ferguson",                          TRUE,
  "\\bFind?la[yv]\\b",                                                "Finlay",                            TRUE,
  "\\bFinlayst\\b",                                                   "Finlay Street",                     TRUE,
  "\\bFinnieston\\b",                                                 "Finnieston",                        TRUE,
  "\\bFish\\s?market\\b",                                             "Fish market",                       TRUE,
  "\\b[FP]ossil\\b",                                                  "Fossil",                            TRUE,
  "\\bFortland\\b",                                                   "Fortland",                          TRUE,
  "\\bFred(?:er)?(?:ic)?k?\\b",                                       "Frederick",                         TRUE,
  "\\bGa[il]l?(?:[jo](?:w|vi)-?(?:g|tr))?[as]?[t-]?e?\\b-?",          "Gallowgate",                        TRUE,
  "\\b[CG]allow-?gate\\b",                                            "Gallowgate",                        TRUE,
  "\\bGa?(?:m|rn)?e?\\s?[ft]?\\s?h?(?:i(?:li|ll|n))?\\b",             "Garnethill",                        TRUE,
  "\\bGarnethil\\sJ",                                                 "Garnethill",                        TRUE,
  "(?<=Hill).+\\K\\bGar\\b",                                          "Garnethill",                        TRUE,
  "\\bGar[nu]et[bh]ill\\b",                                           "Garnethill",                        TRUE,
  "\\bGarng(?:ad)?\\b",                                               "Garngad",                           TRUE,
  "\\bGarngadrd\\b",                                                  "Garngad Road",                      TRUE,
  "\\bGarngadhill\\b",                                                "Garngad Hill",                      TRUE,
  "\\bGarsc?(?:ad)?(?:den)?\\b",                                      "Garscadden",                        TRUE,
  "\\bGarsc[un]?[bh]?e?\\b",                                          "Garscube",                          TRUE,
  "\\bGarscube&",                                                     "Garscube",                          TRUE,
  "\\bGarscuberd\\b",                                                 "Garscube Road",                     TRUE,
  "\\b(?:Street,?\\s)?Geo(?:rg)?[aeo]?s?\\b",                         "George",                            TRUE,
  "\\bGeorgest(?:reet)?\\b",                                          "George Street",                     TRUE,
  "\\bGeorgess?\\b",                                                  "Georges",                           TRUE,
  "\\bGeorgessrd\\b",                                                 "Georges Road",                      TRUE,
  "\\bGibson\\b",                                                     "Gibson",                            TRUE,
  "\\bGlassf(?:or)?d?\\b",                                            "Glassford",                         TRUE,
  "\\bGlassfo.*?r(?:oa)?d\\b",                                        "Glassford",                         TRUE,
  "\\bGlouc(?:es)?(?:ter)?\\b",                                       "Gloucester",                        TRUE,
  "\\bGor[bh]?a?[il]?s?\\b",                                          "Gorbals",                           TRUE,
  "\\b(Gorbals).?\\b",                                                "\\1",                               TRUE,
  "\\bGord(?:on)?\\b",                                                "Gordon",                            TRUE,
  "\\bGo\\s?van\\b",                                                  "Govan",                             TRUE,
  "\\bGov((?!an street))(?(1)a?[nu]?(?:hill)?\\b),?\\.?",             "Govanhill",                         TRUE,
  "\\bGrace\\b",                                                      "Grace",                             TRUE,
  "\\bGr[ars][ejmso]?(?:me)?\\b",                                     "Graeme",                            TRUE,
  "\\bG(?:ne|ros)me\\b",                                              "Graeme",                            TRUE,
  "\\bGraham\\b",                                                     "Graham",                            TRUE,
  "\\bGranby\\b",                                                     "Granby",                            TRUE,
  "\\bGreenh?(?:ead)?\\b",                                            "Greenhead",                         TRUE,
  "\\bGre[ep]nock\\b",                                                "Greenock",                          TRUE,
  "\\bGreenside\\b",                                                  "Greenside",                         TRUE,
  "\\bGreen\\s?vale\\b",                                              "Greenvale",                         TRUE,
  "\\bGrossvenor\\b",                                                 "Grossvenor",                        TRUE,
  "\\bGrove\\b",                                                      "Grove",                             TRUE,
  "\\bHam(?:il)?t?(?:on)?\\b",                                        "Hamilton",                          TRUE,
  "\\bHanover\\b",                                                    "Hanover",                           TRUE,
  "\\bHarmony\\b",                                                    "Harmony",                           TRUE,
  "\\bHelensb(?:urgh)?\\b",                                           "Helensburgh",                       TRUE,
  "\\bHigh\\b",                                                       "High",                              TRUE,
  "\\bHighst\\b",                                                     "High Street",                       TRUE,
  "\\bHill\\b",                                                       "Hill",                              TRUE,
  "\\bHillh(?:ead)?\\b",                                              "Hillhead",                          TRUE,
  "\\bHolm\\b",                                                       "Holm",                              TRUE,
  "\\bHolm((?!\\sstreet))(?(1)[bh]?(?:ead)?\\b)",                     "Holmhead",                          TRUE,
  "\\bHolyr(?:ood)?\\b",                                              "Holyrood",                          TRUE,
  "\\bHope\\b",                                                       "Hope",                              TRUE,
  "\\bHopest(?:reet)?\\b",                                            "Hope Street",                       TRUE,
  "\\bHopet(?:ou?)?n\\b",                                             "Hopetoun",                          TRUE,
  "\\bHosp(?:ital)?\\b",                                              "Hospital",                          TRUE,
  "\\bHouston\\b",                                                    "Houston",                           TRUE,
  "\\bHowa?r?d?\\b",                                                  "Howard",                            TRUE,
  "\\bHowa.*?r(?:oa)?d\\b",                                           "Howard",                            TRUE,
  "\\bHundred\\s?acre\\s?hill\\b",                                    "Hundred Acre Hill",                 TRUE,
  "\\bHut[ce]?(?:[bhk]|li)?e?s?o?[nu]?\\b",                           "Hutcheson",                         TRUE,
  "\\bHutchesont?(?:ow)?n\\b",                                        "Hutchesontown",                     TRUE,
  "\\bHutchesonto\\b",                                                "Hutchesontown",                     TRUE,
  "\\bHutcheson.+?t\\s?own\\b",                                       "Hutchesontown",                     TRUE,
  "\\bHyde\\s?park\\b",                                               "Hydepark",                          TRUE,
  "\\bIngr(?:am)?\\b",                                                "Ingram",                            TRUE,
  "\\bJama(?:ica)?\\b",                                               "Jamaica",                           TRUE,
  "\\bJam\\b\\.?(?=\\sst)",                                           "Jamaica",                           TRUE,
  "\\bJamieson\\b",                                                   "Jamieson",                          TRUE,
  "\\bJamaicast\\b",                                                  "Jamaica Street",                    TRUE,
  "\\bJa(?:me)?s\\b[\\.']?",                                          "James",                             TRUE,
  "\\bJohnst(?:reet)?\\b",                                            "John Street",                       TRUE,
  "\\bJohn Street\\b[,\\s]+\\bBridge?t?o?n?.*",                       "John Street, Bridgeton",            TRUE,
  "\\bJohn Street(?:\\b\\s[a-z]\\s\\b)?Bridgeton\\b",                 "John Street, Bridgeton",            TRUE,
  "\\bKelvi[nu]\\b",                                                  "Kelvingrove",                       TRUE,
  "\\bKelvingr(?:ove)?\\b",                                           "Kelvingrove",                       TRUE,
  "\\bKelvinha(?:ugh)?\\b",                                           "Kelvinhaugh",                       TRUE,
  "\\bKenn(?:edy)?\\b",                                               "Kennedy",                           TRUE,
  "\\bKent\\b",                                                       "Kent",                              TRUE,
  "\\bKerr\\b",                                                       "Kerr",                              TRUE,
  "\\bKile\\b",                                                       "Kile",                              TRUE,
  "\\bKillermont\\b",                                                 "Killermont",                        TRUE,
  "\\bKingst?o?n?\\b",                                                "Kingston",                          TRUE,
  "\\bKi[nu]g\\b",                                                    "King",                              TRUE,
  "\\bK[il][dnu]n(?:ing)?\\b",                                        "Kinning",                           TRUE,
  "\\bKirkst(?:reet)?\\b",                                            "Kirk street",                       TRUE,
  "\\bLo?ance(?:[ft]ield)?\\b",                                       "Lancefield",                        TRUE,
  "\\bLandressy\\b",                                                  "Landressy",                         TRUE,
  "\\bLansd(?:owne)?\\b",                                             "Lansdowne",                         TRUE,
  "\\bLaurel\\b",                                                     "Laurel",                            TRUE,
  "\\bLittlest(?:reet)?\\b",                                          "Little Street",                     TRUE,
  "\\bLit(?:tle)?\\.?\\s?Dovehill\\b",                                "Little Dovehill",                   TRUE,
  "\\bL(?:ittle)?\\.?\\s?Hamilton Street\\b",                         "Little Hamilton Street",            TRUE,
  "\\bL[ou][nu]d?(?:on)?\\b",                                         "London",                            TRUE,
  "\\bLond[ou]?n\\b",                                                 "London",                            TRUE,
  "\\bLyne?doch\\b",                                                  "Lynedoch",                          TRUE,
  "\\bM\\s?['c]\\s?(?=\\w)",                                          "Mac ",                              TRUE,
  "\\bMac,?\\s?(?=\\w)",                                              "Mac ",                              TRUE,
  "\\bMain\\sStreet.+?([A-Z]|off)",                                   "Main Street, \\1",                  FALSE,
  "\\bMain\\sStreet([A-Z]|off)",                                      "Main Street, \\1",                  FALSE,
  "\\bMa[il]tl(?:an)?d?\\b",                                          "Maitland",                          TRUE,
  "\\bMarg(?:aret)?\\b",                                              "Margaret",                          TRUE,
  "\\bMarlboro(?:ugh)?\\b",                                           "Marlborough",                       TRUE,
  "\\bMary\\s?hill\\b",                                               "Maryhill",                          TRUE,
  "\\bMax-?we?l?l?\\b",                                               "Maxwell",                           TRUE,
  "\\bMaxweltow?n\\b",                                                "Maxweltown",                        TRUE,
  "\\bMayfieldpl\\b",                                                 "Mayfield Place",                    TRUE,
  "\\bMenstrie\\b",                                                   "Menstrie",                          TRUE,
  "\\bMerch(?:an)?t?\\b",                                             "Merchant",                          TRUE,
  "\\bMiddleton\\b",                                                  "Middleton",                         TRUE,
  "\\bMile(?:[\\-\\s]*)?e[nu]d\\b",                                   "Mile-end",                          TRUE,
  "\\bMill[ae]r\\b",                                                  "Miller",                            TRUE,
  "\\bMill?Road\\b",                                                  "Millroad",                          TRUE,
  "\\bMilt(?:on)?\\b",                                                "Milton",                            TRUE,
  "\\bMilton 3 t",                                                    "Milton Street",                     TRUE,
  "\\bMonteith\\b",                                                   "Monteith",                          TRUE,
  "\\bMontrose\\b",                                                   "Montrose",                          TRUE,
  "\\bMoore?\\b",                                                     "Moore",                             TRUE,
  "\\bMorrison\\b",                                                   "Morrison",                          TRUE,
  "\\bMuslin\\b",                                                     "Muslin",                            TRUE,
  "\\bMuslin\\s+Street[,\\s]+Br\\w+",                                 "Muslin Street, Bridgeton",          TRUE,
  "\\bNelson\\b",                                                     "Nelson",                            TRUE,
  "\\bNels[ou][nu]\\b.+?Street",                                      "Nelson Street",                     TRUE,
  "\\bNew(?:h|il)all\\b",                                             "Newhall",                           TRUE,
  "\\bNewst(?:reet)?\\b",                                             "New Street",                        TRUE,
  "\\bNew\\s?Cit[vy}].+?Road",                                        "New City Road",                     TRUE,
  "\\bNew\\s?ven(?:ne\\s?l)?\\b",                                     "New Vennel",                        TRUE,
  "\\bNewt(?:o[an])?\\b",                                             "Newton",                            TRUE,
  "\\bNew\\s?wyn?d?\\b",                                              "New Wynd",                          TRUE,
  "\\bNich?ols(?:on)?\\b",                                            "Nicholson",                         TRUE,
  "\\bNile\\b",                                                       "Nile",                              TRUE,
  "\\bNiles[ht]\\b",                                                  "Nile Street",                       TRUE,
  "\\bNile\\sstf\\b",                                                 "Nile Street",                       TRUE,
  "\\bNorf(?:ol)?k\\b",                                               "Norfolk",                           TRUE,
  "\\bNormalplace\\b",                                                "Normal Place",                      TRUE,
  "\\bNorth\\b",                                                      "North",                             TRUE,
  "\\bNorth\\sCityrd\\b",                                             "North City Road",                   TRUE,
  "\\bNorth\\s+Courts?.+R.+\\sE.+",                                   "North Court, Royal Exchange",       TRUE,
  "\\bNorth\\s+Dal.+arno.+\\sRoad",                                   "North Dalmarnock Road",             TRUE,
  "\\bNorth\\s+\\bHano?v?(?:er)?\\b\\.?(?:.+Street)?",                "North Hanover Street",              TRUE,
  "\\bNorth\\s+\\bPortl?v?(?:and)?\\b\\.?(?:.+Street)?",              "North Portland Street",             TRUE,
  "\\bOakfiel[dm]\\b",                                                "Oakfield",                          TRUE,
  "\\bOswa?ld",                                                       "Oswald",                            TRUE,
  "\\bOrcha.*?r(?:oa)?d\\b",                                          "Orchard",                           TRUE,
  "\\bO[nr][ers]\\b",                                                 "Orr",                               TRUE,
  "\\bOn\\b-(?=\\sstreet)",                                           "Orr",                               TRUE,
  "\\bOsborne\\b",                                                    "Osborne",                           TRUE,
  "\\bOswald\\b",                                                     "Oswald",                            TRUE,
  "\\bOxf(?:or)?d?\\b",                                               "Oxford",                            TRUE,
  "\\bOxfo.*?r(?:oa)?d\\b",                                           "Oxford",                            TRUE,
  "\\bPais?(?:ley)?\\b'?",                                            "Paisley",                           TRUE,
  "\\bPaisleyrd\\b",                                                  "Paisley Road",                      TRUE,
  "\\bPa[ir]l?i?a?(?:m|rn)?e?n?t?a?r?y?\\b[\\.,]?(?=\\s+R(?:oa)?d)",  "Parliamentary",                     TRUE,
  "\\bParkgro\\b",                                                    "Parkholm",                          TRUE,
  "\\bParkho(?:use)?\\b",                                             "Parkhouse",                         TRUE,
  "\\bPart(?:ic)?k?\\b",                                              "Partick",                           TRUE,
  "\\bParson\\b",                                                     "Parson",                            TRUE,
  "\\bPaters(?:on)?\\b",                                              "Paterson",                          TRUE,
  "\\bPeel\\b",                                                       "Peel",                              TRUE,
  "\\bPiccadilly\\b",                                                 "Piccadilly",                        TRUE,
  "\\bPitt\\b",                                                       "Pitt",                              TRUE,
  "\\bPolloc?k\\b",                                                   "Pollock",                           TRUE,
  "\\bPolloksh?a?w?s?\\b\\.?(?=\\s+?Road|$)",                         "Pollokshaws",                       TRUE,
  "(?<=street,\\s)\\bPolloks(?:haws)?\\b",                            "Pollokshaws",                       TRUE,
  "\\bPollokst\\b",                                                   "Pollok Street",                     TRUE,
  "\\bPoor(?:\\s?house)?\\b",                                         "Poorhouse",                         TRUE,
  "\\bPort\\b.+?Dundas.+Road",                                        "Port-Dundas Road",                  TRUE,
  "\\bPort[,\\s\\-]\\s?(\\w)",                                        "Port-\\1",                          TRUE,
  "\\b[Pp]ort([A-Z])",                                                "Port-\\1",                          TRUE,
  "\\bPort\\b[\\s\\-,]\\bStreet\\b",                                  "Port Street",                       TRUE,
  "\\bPort(?:-)er\\b",                                                "Porter",                            TRUE,
  "\\b[Pp]ort(?!-|\\s|Dundas)[il]?a?[ns]?d?\\b",                      "Portland",                          TRUE,
  "\\bPort(?:-)land\\b",                                              "Portland",                          TRUE,
  "\\bPort-?man\\b",                                                  "Portman",                           TRUE,
  "\\bPort(?:-)ugal\\b",                                              "Portugal",                          TRUE,
  "\\bPrince'?s'?[\\s\u2013]+(\\w+)",                                 "Princes \\1",                       TRUE,
  "\\bProspect\\b",                                                   "Prospect",                          TRUE,
  "\\bQue[eu]n\\b",                                                   "Queen",                             TRUE,
  "\\bQueen'?s'?[\\s\u2013]+(\\w+)",                                  "Queens \\1",                        TRUE,
  "\\bRailw(?:ay)?\\b",                                               "Railway",                           TRUE,
  "\\bReg(?:en)?t?\\b",                                               "Regent",                            TRUE,
  "\\b[BR]egent\\b",                                                  "Regent",                            TRUE,
  "\\bReid(?:st)?\\b.+(?:Street\\.?|Bridg.+)",                        "Reid Street, Bridgeton",            TRUE,
  "\\bRe+n\\s?[fil](?:iel)?(?:cl|d)?\\b",                             "Renfield",                          TRUE,
  "\\bRenfieldst(?:reet)?\\b",                                        "Renfield Street",                   TRUE,
  "\\b[ER]e[nu]fr?(?:ew)?\\b",                                        "Renfrew",                           TRUE,
  "\\bRenfrewst(?:reet)?\\b",                                         "Renfrew Street",                    TRUE,
  "Renfrew[\\s;]+Street",                                             "Renfrew Street",                    TRUE,
  "\\bRichard\\b",                                                    "Richard",                           TRUE,
  "\\bRicha.*?r(?:oa)?d\\b",                                          "Richard",                           TRUE,
  "\\bRol(?:lo)?[sx?]\\b",                                            "Rollox",                            TRUE,
  "\\bRob(?:er)?t?s?(?:on)?\\b",                                      "Robertson",                         TRUE,
  "\\bropeworks\\b",                                                  "Ropeworks",                         TRUE,
  "\\bRosebank\\b.+$",                                                "Rosebank, Garngad road",            TRUE,
  "\\bRose\\b",                                                       "Rose",                              TRUE,
  "\\bRosest(?:reet)?\\b",                                            "Rose Street",                       TRUE,
  "\\bRose\\s+?Street.+?Hutchesontown\\b",                            "Rose Street, Hutchesontown",        TRUE,
  "\\bRosehal(?:\\b]|l\\b)?",                                         "Rosehall",                          TRUE,
  "\\b[KR]otte[nu]r(?:ow)?\\b",                                       "Rottenrow",                         TRUE,
  "\\bRoy(?:al)?",                                                    "Royal",                             TRUE,
  "\\bR.+\\bExc?h?(?:an)?(?:ange)?\\b",                               "Royal Exchange",                    TRUE,
  "\\bR[un]mford\\b",                                                 "Rumford",                           TRUE,
  "\\bRumfo.*?r(?:oa)?d\\b",                                          "Rumford",                           TRUE,
  "\\bRuther([^f])(?(1).+)",                                          "Rutherglen Loan",                   TRUE,
  "\\bRutland\\b",                                                    "Rutland",                           TRUE,
  "^Saint\\s+James\\s+Street.+kin.+",                                 "Saint James Street, Kingston",      TRUE,
  "^Saint Rollox.+",                                                  "Saint Rollox",                      TRUE,
  "\\bSaint\\sVincentpl\\b",                                          "Saint Vincent Place",               TRUE,
  "\\bSalisb(?:ury)?\\b",                                             "Salisbury",                         TRUE,
  "\\bSalttnarket\\b",                                                "Salt Market",                       TRUE,
  "\\bSand[jy]?\\s?f?(?:ord)?\\b",                                    "Sandyford",                         TRUE,
  "\\bSandy\\s?ford\\b",                                              "Sandyford",                         TRUE,
  "\\bSandyfo.*?r(?:oa)?d\\b",                                        "Sandyford",                         TRUE,
  "\\bSauceibank\\b",                                                 "Saucel Bank",                       TRUE,
  "\\bSau'?c(?:hi|M)e(?:ha[il]l)?\\b",                                "Sauchiehall",                       TRUE,
  "\\bSauch.+?Str?e?e?t?",                                            "Sauchiehall Street",                TRUE,
  "\\bShamr?(?:ock)?\\b",                                             "Shamrock",                          TRUE,
  "\\bSharpes\\b",                                                    "Sharps",                            TRUE,
  "\\bShawfieldbank\\b",                                              "Shawfield Bank",                    TRUE,
  "\\bShaw\\s?l(?:an)?ds\\b",                                         "Shawlands",                         TRUE,
  "\\bShawsrd\\b",                                                    "Shaws Road",                        TRUE,
  "\\bShettles(?:ton)?\\b",                                           "Shettleston",                       TRUE,
  "\\bShield\\s?hall\\b",                                             "Shieldhall",                        TRUE,
  "\\bShuttle\\b",                                                    "Shuttle",                           TRUE,
  "\\bSo(?:uth)?ciety\\b",                                            "Society",                           TRUE,
  "\\bSo(?:uth)?ho\\b",                                               "Soho",                              TRUE,
  "\\bSo(?:uth)?merset\\b",                                           "Somerset",                          TRUE,
  "\\bSo(?:uth)?mertoun\\b",                                          "Somertoun",                         TRUE,
  "\\bSomm?erville\\b",                                               "Somerville",                        TRUE,
  "\\bSo(?:uth)?mm?erville\\b",                                       "Somerville",                        TRUE,
  "\\bSouthcroft\\b",                                                 "Southcroft",                        TRUE,
  "\\bSouthutbbank\\b",                                               "Sornbank",                          TRUE,
  "\\bSouth\\sside.+harb.+",                                          "South side, Glasgow Harbour",       TRUE,
  "\\bSp.?outmout[bh]\\b",                                            "Spoutmouth",                        TRUE,
  "\\bSpringf(?:iel)?d?\\b",                                          "Springfield",                       TRUE,
  "\\bSpringfield\\b[,\\s]+(?!lane|court|place).+",                   "Springfield.",                      TRUE,
  "\\bStanhope\\b",                                                   "Stanhope",                          TRUE,
  "\\bStanley?\\b",                                                   "Stanley",                           TRUE,
  "\\bStevenson\\b",                                                  "Stevenson",                         TRUE,
  "\\bStewart\\b",                                                    "Stewart",                           TRUE,
  "\\bStirl(?:ing)?\\b",                                              "Stirling",                          TRUE,
  "\\bStirlingsrd\\b",                                                "Stirling Road",                     TRUE,
  "\\bStobc?r?o?[3s]?[8as]?\\b",                                      "Stobcross",                         TRUE,
  "\\bSt[osu]bc?r?o?s?[as]?\\b\\.?(?:\\s+Street|\\s+\\w+[,.$])?",     "Stobcross Street",                  TRUE,
  "\\bStock[\\-\\s]?w?e?l?l?(?:ell)?\\b",                             "Stockwell",                         TRUE,
  "\\bStockwell\\s\\b[A-Z]\\b",                                       "Stockwell",                         TRUE,
  "\\bSuffolk\\b",                                                    "Suffolk",                           TRUE,
  "\\bSurrey\\b",                                                     "Surrey",                            TRUE,
  "\\bStrathbungo\\b",                                                "Strathbungo",                       TRUE,
  "\\bStrut[bh]erss?t?\\b\\.?(?:\\s+street)?",                        "Struthers Street",                  TRUE,
  "\\bTenn?[ae]nt\\b",                                                "Tennent",                           TRUE,
  "\\bThis(?:tle)?\\b",                                               "Thistle",                           TRUE,
  "\\bThomson\\b",                                                    "Thomson",                           TRUE,
  "\\bT[ao]bago\\b",                                                  "Tobago",                            TRUE,
  "\\bTobagost(?:reet)?\\b",                                          "Tobago Street",                     TRUE,
  "\\bTo\\s?wnh(?:ead)?\\b",                                          "Townhead",                          TRUE,
  "\\bTowns?(?:\\s+)?mill\\b",                                        "Townmill",                          TRUE,
  "\\bTradesto\\s?[Bn]\\b",                                           "Tradeston",                         TRUE,
  "\\bTr(?:ad)?e?s?t?o?[bnu]?\\b",                                    "Tradeston",                         TRUE,
  "\\btrade[3s]\\s?ton\\b",                                           "Tradeston",                         TRUE,
  "\\bTro[dn]g(?:at)?[ce]?\\b",                                       "Trongate",                          TRUE,
  "\\bTr.+?gate\\b",                                                  "Trongate",                          TRUE,
  "\\bUnion\\b",                                                      "Union",                             TRUE,
  "\\bUpper(?:\\s+)?fauld\\b",                                        "Upperfauld",                        TRUE,
  "\\bVict(?:oria)?\\b",                                              "Victoria",                          TRUE,
  "\\bViewpark\\b",                                                   "Viewpark",                          TRUE,
  "\\bVin[ec]?e?n?t?\\b",                                             "Vincent",                           TRUE,
  "(?<=\\.)\\bVincentpl\\b",                                          ", Vincent Place",                   TRUE,
  "\\bViolet\\s+gro(?:ve)?\\b",                                       "Violet grove",                      TRUE,
  "\\bVirg(?:inia)?\\b",                                              "Virginia",                          TRUE,
  "\\bWalm.+cr.+\\b",                                                 "Walmer Crescent",                   TRUE,
  "\\b(?:AV|W)alworth\\b",                                            "Walworth",                          TRUE,
  "\\bWa[nr]r?(?:oc[bh])\\b",                                         "Warroch",                           TRUE,
  "\\bWar(?:wi)?ck\\b",                                               "Warwick",                           TRUE,
  "\\bWashingt(?:on)?\\b",                                            "Washington",                        TRUE,
  "\\bWatt\\b",                                                       "Watt",                              TRUE,
  "\\bWater\\b-?",                                                    "Water",                             TRUE,
  "\\bWaterloo\\b",                                                   "Waterloo",                          TRUE,
  "\\bWellcroft\\b",                                                  "Wellcroft",                         TRUE,
  "\\bWelling(?:on)?\\b",                                             "Wellington",                        TRUE,
  "\\bWemyss?\\b",                                                    "Wemyss",                            TRUE,
  "\\bWest\\s+str.+(?:\\s+)?trad.+\\b",                               "West Street, Tradeston",            TRUE,
  "\\bWestern\\b",                                                    "Western",                           TRUE,
  "\\bWestmuir\\b",                                                   "Westmuir",                          TRUE,
  "\\bWhitehall\\b",                                                  "Whitehall",                         TRUE,
  "\\bWillowb(?:an)?k?\\b",                                           "Willowbank",                        TRUE,
  "\\bW(?:illia)?m\\b",                                               "William",                           TRUE,
  "\\bW(?:d|il)son\\b",                                               "Wilson",                            TRUE,
  "\\bWin(?:dsor)?\\b\\.?(?:\\s+)?(place|Terrace|Street)?",           "Windsor \\1",                       TRUE,
  "\\bWoodla(?:nds)?\\b",                                             "Woodlands",                         TRUE,
  "\\bWoodsi(?:de)?\\b",                                              "Woodside",                          TRUE,
  "\\bYork\\b",                                                       "York",                              TRUE,
  "\\bYoungs\\b",                                                     "Young street",                      TRUE
)


### globals_numbers ####

#' Numbers in address entries
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   numbers in directory address entries. For each number a replacement pattern
#'   is provided for used in substitution operations as well as a boolean operator
#'   indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for number matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_numbers <- tibble::tribble(
  ~pattern,                                             ~replacement,       ~ignore_case,
  "(?<=\\d|^)[\\]iIl](?![A-Za-z])",                     "1",                FALSE,
  "[iIl](?=\\d|\u00a3)",                                "1",                FALSE,
  "(?<=\\s)[iIl](?=,|\\s)",                             "1",                FALSE,
  "(?<=\\d)S-'(?![A-Za-z])",                            "3",                FALSE,
  "S-'(?=\\d)",                                         "3",                FALSE,
  "(?<=\\s)S-'(?=,|\\s)",                               "3",                FALSE,
  "(?<=\\d)(?:C|G|<J|{J)(?![A-Za-z])",                  "6",                FALSE,
  "(?:C|G|<J)(?=\\d)",                                  "6",                FALSE,
  "(?<=\\s)(?:C|G|<J)(?=,|\\s)",                        "6",                FALSE,
  "(?<=\\d)T(?![A-Za-z])",                              "7",                FALSE,
  "T(?=\\d)",                                           "7",                FALSE,
  "(?<=\\s)T(?=,|\\s)",                                 "7",                FALSE,
  "(?<=\\d)S(?![A-Za-z])",                              "8",                FALSE,
  "S(?=\\d)",                                           "8",                FALSE,
  "(?<=\\s)S(?=,|\\s)",                                 "8",                FALSE,
  "(?<=\\d)S(?=[A-Z])",                                 "8",                FALSE,
  "\\blu\\b",                                           "10",               FALSE,
  "\\bH(?=[A-Z])",                                      "11 ",              FALSE,
  "(?<=\\s|^)II(?=\\s)",                                "11 ",              FALSE,
  "\\bIS\\b",                                           "18",               FALSE,
  "\\bCI\\b",                                           "61",               FALSE,
  "\\bfl\\b",                                           "71",               FALSE,
  "\\bIll\\b",                                          "111",              FALSE,
  "\\bllo(?=[A-Z])",                                    "113",              FALSE,
  "\\bHi\\b",                                           "114",              FALSE,
  "(?<=\\d)-?(\\.5|\u00a3|\\^|\\||\\\\|f|h|i|j)-?",     "1/2",              TRUE,
  "(?<=\\s)j(?=\\s)",                                   "1/2",              TRUE
)


### Places ####

#### globals_places_raw ####
#' Place types in address entries
#'
#' A character vector of common place types found in directory address entries
#'
#' @format A character string vector.
globals_places_raw <- c(
  "Academy",
  "Arcade",
  "Arches",
  "Bay",
  "Bank",
  "Bazaar",
  "Brewery",
  "Bridge",
  "Buildings",
  "Brickfields",
  "Cabinet",
  "Castle",
  "City",
  "Circus",
  "Close",
  "Colliery",
  "Cottage",
  "Court",
  "Crescent",
  "Dairy",
  "Drive",
  "Dyeworks",
  "Entrance",
  "Entry",
  "Establishment",
  "Factory",
  "Foundry",
  "Garden",
  "Glebe",
  "Harbour",
  "Hotel",
  "House",
  "Hill",
  "Ironworks",
  "Junction",
  "Lane",
  "Lace",
  "Loan",
  "Lodge",
  "Manufactory",
  "Market",
  "Mill",
  "Place",
  "Point",
  "Port",
  "Quadrant",
  "Quarry",
  "Quay",
  "Road",
  "Row",
  "Show-rooms",
  "Square",
  "Stables",
  "Stand",
  "Store",
  "Street",
  "Tanyard",
  "Terrace",
  "Vennel",
  "Vaults",
  "Villa",
  "Warehouse",
  "Wynd"
)


#### globals_places_regex ####

#' Place types in address entries
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   place types in directory address entries. For each place type a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for place type matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_places_regex <- tibble::tribble(
  ~pattern,                                         ~replacement,           ~ignore_case,
  "\\bAcademy\\b",                                  "Academy",              TRUE,
  "((?:Argyle)[\\s.]?)?Arc(?:ade)?\\b",             "\\1Arcade",            TRUE,
  "\\barc?(?:ade)\\b",                              "Arcade",               TRUE,
  "\\bArches\\b",                                   "Arches",               TRUE,
  "\\barch(?:es)?\\b",                              "Arches",               TRUE,
  "\\bBay\\b",                                      "Bay",                  TRUE,
  "\\bBazaar\\b",                                   "Bazaar",               TRUE,
  "\\bBank\\b",                                     "Bank",                 TRUE,
  "\\bBranch\\b",                                   "Branch",               TRUE,
  "\\bBrewery\\b",                                  "Brewery",              TRUE,
  "\\bBrickfields\\b",                              "Brickfields",          TRUE,
  "\\bBridge\\b",                                   "Bridge",               TRUE,
  "\\bBri?d?g?e?\\b",                               "Bridge",               TRUE,
  "\\bBuildings\\b",                                "Buildings",            TRUE,
  "\\bbds\\b",                                      "Buildings",            TRUE,
  "\\bCabinet\\b",                                  "Cabinet",              TRUE,
  "\\bCastle\\b",                                   "Castle",               TRUE,
  "\\bCity\\b",                                     "City",                 TRUE,
  "\\bCircus\\b",                                   "Circus",               TRUE,
  "\\bcir(?:cus)?\\b",                              "Circus",               TRUE,
  "\\bClose\\b",                                    "Close",                TRUE,
  "\\bclo(?:se)?\\b",                               "Close",                TRUE,
  "\\bColliery\\b",                                 "Colliery",             TRUE,
  "\\bCottage\\b",                                  "Cottage",              TRUE,
  "\\bCourt\\b",                                    "Court",                TRUE,
  "\\bco?(?:ur)?t?\\b,?",                           "Court",                TRUE,
  "\\bcre?s?c?(?:en)?t?\\b",                        "Crescent",             TRUE,
  "\\bcerst\\b",                                    "Crescent",             TRUE,
  "\\bCustoms\\b",                                  "Customs",              TRUE,
  "[HM].+?Customs\\b",                              "Her Majestys Customs", TRUE,
  "\\bDairy\\b",                                    "Dairy",                TRUE,
  "\\bDrive\\b",                                    "Drive",                TRUE,
  "\\bdri?v?e?\\b",                                 "Drive",                TRUE,
  "\\bDyeworks\\b",                                 "Dyeworks",             TRUE,
  "\\bEntrance\\b",                                 "Entrance",             TRUE,
  "\\bEntry\\b",                                    "Entry",                TRUE,
  "\\bEstab(?:lishment)?\\b",                       "Establishment",        TRUE,
  "\\b(Biscuit\\s)?Factory?\\b",                    "\\1 Factory",          TRUE,
  "\\bFoundry\\b",                                  "Foundry",              TRUE,
  "\\bGarden(s)?\\b",                               "Garden\\1",            TRUE,
  "\\bgl(?:ebe)?\\b",                               "Glebe",                TRUE,
  "\\bHarbour\\b",                                  "Harbour",              TRUE,
  "\\bHill\\b",                                     "Hill",                 TRUE,
  "\\bHotel\\b",                                    "Hotel",                TRUE,
  "\\bj?(?:h|l|li)ouse\\b",                         "House",                TRUE,
  "\\bho\\b",                                       "House",                TRUE,
  "\\bIronworks\\b",                                "Ironworks",            TRUE,
  "\\bJunction\\b",                                 "Junction",             TRUE,
  "\\bPost Office\\b",                              "Post Office",          FALSE,
  "P\\.\\s?O\\.\\s?",                               "Post Office",          FALSE,
  "\\bP.+?O(?:ffice)",                              "Post Office",          FALSE,
  "\\bPost Office(?:\\w+)?",                        "Post Office",          TRUE,
  "\\bGeneral Post Office\\b",                      "General Post Office",  FALSE,
  "G\\.\\s?Post Office\\b",                         "General Post Office",  FALSE,
  "\\bLace\\b",                                     "Lace",                 TRUE,
  "\\bLane\\b",                                     "Lane",                 TRUE,
  "\\b[Il][an](?:ne)?\\b",                          "Lane",                 TRUE,
  "\\bLoan\\b",                                     "Loan",                 TRUE,
  "\\bLodge\\b",                                    "Lodge",                TRUE,
  "\\bManufactory\\b",                              "Manufactory",          TRUE,
  "(Cattle|Fish|S[an]lt)?[\\s.]Market\\b",          "\\1Market",            TRUE,
  "(Cattle|Fish|S[an]lt)?Market\\b",                "\\1 Market",           TRUE,
  "\\bMill\\b",                                     "Mill",                 TRUE,
  "\\bPl\\b",                                       "Place",                TRUE,
  "\\bPlace\\b",                                    "Place",                TRUE,
  "\\bp[il][acn]?(?:ce)?\\b",                       "Place",                TRUE,
  "_place\\b",                                      "Place",                TRUE,
  "(?<=\\w)Pl(?:ace)?\\b",                          " Place",               TRUE,
  "\\bPoint\\b",                                    "Point",                TRUE,
  "\\bPort\\b",                                     "Port",                 TRUE,
  "\\bP(?:or)?t?\\b[,.]",                           "Port",                 TRUE,
  "\\bQuadrant\\b",                                 "Quadrant",             TRUE,
  "\\bQuay\\b",                                     "Quay",                 TRUE,
  "\\bq[uy]?[oa]?y?\\b",                            "Quay",                 TRUE,
  "\\bQuarry\\b",                                   "Quarry",               TRUE,
  "\\bRoad\\b",                                     "Road",                 TRUE,
  "\\b[ir](?:[eo]a)?d\\b",                          "Road",                 TRUE,
  "1\\s-\\soad\\b",                                 "Road",                 TRUE,
  "\\brt\\s?L\\b",                                  "Road",                 TRUE,
  "\\bro\\b",                                       "Road",                 TRUE,
  "(?<=\\w)R(?:oa)?d\\b",                          " Road",                 TRUE,
  "\\brow\\b",                                      "Row",                  TRUE,
  "\\bReceiving house\\b",                          "Receiving house",      TRUE,
  "\\breceiving\\s?house\\b",                       "Receiving house",      TRUE,
  "\\bShow-rooms\\b",                               "Show-rooms",           TRUE,
  "\\bSquare\\b",                                   "Square",               TRUE,
  "\\bsq[nu]?(?:are)?\\b",                          "Square",               TRUE,
  "\\bStables\\b",                                  "Stables",              TRUE,
  "\\bStand\\b",                                    "Stand",                TRUE,
  "\\bStore\\b",                                    "Store",                TRUE,
  "\\bStreet",                                      "Street",               TRUE,
  "(?<!^)\\b[iaes]t[ri]?[ie]?(?:et)?\\b\\.?",       "Street",               TRUE,
  "\\bsh\\b\\.",                                    "Street",               TRUE,
  "\\bs[ft][er]eet\\b\\.?",                         "Street",               TRUE,
  "\\bs[ir]\\b\\.?",                                "Street",               TRUE,
  "\\bstr[e']+t\\b\\.?",                            "Street",               TRUE,
  "sfc",                                            "Street",               TRUE,
  "\\bi-treet\\b",                                  "Street",               TRUE,
  "\\bstl\\b",                                      "Street",               TRUE,
  "(street)(?=[a-z])",                              "\\1 ",                 TRUE,
  "(street),(?=[a-z])",                             "\\1 ",                 TRUE,
  "\\b(street)\\b[^\\s,]?(?=\\s)",                  "\\1 ",                 TRUE,
  "(\\w)\\1st",                                     "\\1\\1 Street",        TRUE,
  "3\\street",                                      "Street",               TRUE,
  "\\bTanyard\\b",                                  "Tanyard",              TRUE,
  "\\bTerrace\\b",                                  "Terrace",              TRUE,
  "\\bte[nr](?:[r\\-]ace)?\\b",                     "Terrace",              TRUE,
  "\\bVaults\\b",                                   "Vaults",               TRUE,
  "\\bVennel\\b",                                   "Vennel",               TRUE,
  "\\bVe[nr][mn]el\\b",                             "Vennel",               TRUE,
  "\\bVilla(s)?\\b",                                "Villa\\1",             TRUE,
  "\\bWarehouse\\b",                                "Warehouse",            TRUE,
  "Whole[ps]ale\\b",                                "Wholesale",            TRUE,
  "\\bWynd\\b",                                     "Wynd",                 TRUE,
  "\\bw[jy]?'?n?d?\\b",                             "Wynd",                 TRUE
)


### globals_saints ####

#' Saints in address names
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   name of Saints in directory address names. For each Saint a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for Saint name matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_saints <- tibble::tribble(
  ~pattern,                     ~replacement,   ~ignore_case,
  "\\bAndr?e?w\\b",             "Andrew",       TRUE,
  "\\bAnn\\b",                  "Ann",          TRUE,
  "\\bBernard\\b",              "Bernard",      TRUE,
  "\\bDavid\\b",                "David",        TRUE,
  "\\bEn(?:och)?\\b",           "Enoch",        TRUE,
  "\\bGermains\\b",             "Germains",     TRUE,
  "\\bGe[co](?:rg)?e?s?\\b",    "George",       TRUE,
  "\\bGeorgess?\\b",            "George",       TRUE,
  "\\bJa(?:me)?s\\b",           "James",        TRUE,
  "\\bJohn\\b",                 "John",         TRUE,
  "\\bJoseph\\b",               "Joseph",       TRUE,
  "\\bkilda\\b",                "kilda",        TRUE,
  "\\bMarg(?:aret)?\\b",        "Margaret",     TRUE,
  "\\bMark\\b",                 "Mark",         TRUE,
  "\\bMarnock\\b",              "Marnock",      TRUE,
  "\\bMary\\b",                 "Mary",         TRUE,
  "\\bMungo\\b",                "Mungo",        TRUE,
  "\\bNicholas\\b",             "Nicholas",     TRUE,
  "\\bNinian\\b",               "Ninian",       TRUE,
  "\\bPaul\\b",                 "Paul",         TRUE,
  "\\bPeter\\b",                "Peter",        TRUE,
  "\\bRol(?:lo)?[sx?]\\b",      "Rollox",       TRUE,
  "\\bVin[ec]?e?n?t?\\b-?",     "Vincent",      TRUE,
  "\\bV\\smcen\\st\\b",         "Vincent",      TRUE
)


### globals_suffixes ####

#' Address suffixes
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   suffixes in directory address entries. For each suffix a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for suffix matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_suffixes <- tibble::tribble(
  ~pattern,                               ~replacement, ~ignore_case,
  "\\bNo?(?:rth)?\\b(?!\\shome)\\.?",     "North",      TRUE,
  "(?<!\\')\\bSo?u?(?:th)?\\b\\.?",       "South",      TRUE,
  "\\bSo(?=[A-Z])",                       "South",      TRUE,
  "\\bso(uth)?\\b",                       "South",      TRUE,
  "\\bSsiuth\\b",                         "South",      TRUE,
  "\\bSouth[uth]+\\b",                    "South",      TRUE,
  "\\bWe?(?:st)?\\b\\.?",                 "West",       TRUE,
  "(?<!')\\bEa?(?:st)?\\b\\.?",           "East",       TRUE,
  "\\bLittle\\b",                         "Little",     TRUE,
  "\\bM(?:ai)?ns?\\b\\.?",                "Main",       TRUE,
  "\\b(main)([a-z]\\.?)",                 "\\1 \\2",    TRUE,
  "\\bGr?(?:ea)?t?\\b(?!\\.P)",           "Great",      TRUE,
  "\\bg(?!\\.P)\\b",                      "Great",      TRUE,
  "\\bC\\.(?=\\sclyde)",                  "Great",      TRUE,
  "\\bUp?p?e?r?\\b\\.?",                  "Upper",      TRUE
)


### globals_worksites ####

#' Worksites in address entries
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   worksite names in directory address entries. For each worksite a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for worksite name matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_worksites <- tibble::tribble(
  ~pattern,            ~replacement,       ~ignore_case,
  "Woody(?:ar)?d",     "Woodyard",         TRUE
)

















































## People ####

### globals_forenames ####

#' Forenames in directory records
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   forenames in directory name entries. For each forename a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for forename matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_forenames <- tibble::tribble(
  ~pattern,                                 ~replacement,                ~ignore_case,
  "\\bAbr(?:aham)?\\b",                     "Abraham",                   TRUE,
  "\\bAllan\\b",                            "Allan",                     TRUE,
  "\\bAle[sx](?:and[ae]r)?\\b",             "Alexander",                 TRUE,
  "\\bAnd(?:re)?w?\\b",                     "Andrew",                    TRUE,
  "\\bAng(?:us)?\\b",                       "Angus",                     TRUE,
  "\\bArch?i?b?(?:al)?d?\\b",               "Archibald",                 TRUE,
  "\\bArt[ht](?:ur)?\\b",                   "Arthur",                    TRUE,
  "\\bA\\b\\.?",                            "Alexander",                 TRUE,
  "\\bBenj?(?:amin)?\\b",                   "Benjamin",                  TRUE,
  "\\bBern?(?:ard)?\\b",                    "Bernard",                   TRUE,
  "\\bBro(?:th)?(?:er)?s?\\b",              "Brothers",                  TRUE,
  "\\bB\\b\\.?",                            "Bernard",                   TRUE,
  "\\bCamp?(?:bell)?\\b",                   "Campbell",                  TRUE,
  "\\bCat(?:h|li)(?:erine)?\\b",            "Catherine",                 TRUE,
  "\\bCha?(?:rle)?s?\\b",                   "Charles",                   TRUE,
  "\\bChristo?(?:pher)?\\b",                "Christopher",               TRUE,
  "\\bCor?(?:nelius)?\\b",                  "Cornelius",                 TRUE,
  "\\bC\\b\\.?",                            "Colin",                     TRUE,
  "\\bDan(?:iel)?\\b",                      "Daniel",                    TRUE,
  "\\bDavid\\b",                            "David",                     TRUE,
  "\\bDom(?:inic)?k?\\b",                   "Dominick",                  TRUE,
  "\\bDon(?:ald)?\\b",                      "Donald",                    TRUE,
  "\\bDug(?:ald)?\\b",                      "Dugald",                    TRUE,
  "\\bDunc?(?:an)?\\b",                     "Duncan",                    TRUE,
  "\\bD\\b\\.?",                            "David",                     TRUE,
  "\\bEaton\\b",                            "Eaton",                     TRUE,
  "\\bEben(?:ezer)?\\b",                    "Ebenezer",                  TRUE,
  "\\bEdm(?:und)?\\b",                      "Edmund",                    TRUE,
  "\\bEdw(?:ar)?d?\\b",                     "Edward",                    TRUE,
  "\\bEliza?b?(?:eth)?\\b",                 "Elizabeth",                 TRUE,
  "\\bE\\b\\.?",                            "Edward",                    TRUE,
  "\\bFran(?:cis)?\\b",                     "Francis",                   TRUE,
  "\\bFred(?:eric)?k?\\b",                  "Frederick",                 TRUE,
  "\\bF\\b\\.?",                            "Frederick",                 TRUE,
  "\\bGabriel\\b",                          "Gabriel",                   TRUE,
  "\\bGavin\\b",                            "Gavin",                     TRUE,
  "\\bGeo(?:rge)?\\b",                      "George",                    TRUE,
  "\\bGil(?:bert)?\\b",                     "Gilbert",                   TRUE,
  "\\bGodf(?:rey)?\\b",                     "Godfrey",                   TRUE,
  "\\bG\\b\\.?",                            "George",                    TRUE,
  "\\bHar(?:ry)?\\b",                       "Harry",                     TRUE,
  "\\bHector\\b",                           "Hector",                    TRUE,
  "\\bH(enr)?(?(1)y|y)\\b",                 "Henry",                     TRUE,
  "\\bHen(?:ry)?\\b",                       "Henry",                     TRUE,
  "\\bHug?h?\\b",                           "Hugh",                      TRUE,
  "\\bH\\b\\.?",                            "Hugh",                      TRUE,
  "\\bI\\b\\.?",                            "Isaac",                     TRUE,
  "\\bJacob\\b",                            "Jacob",                     TRUE,
  "\\bJa?(?:me)?s\\b",                      "James",                     TRUE,
  "\\bJane\\b",                             "Jane",                      TRUE,
  "\\bJanet\\b",                            "Janet",                     TRUE,
  "\\bJasp(?:er)?\\b",                      "Jasper",                    TRUE,
  "\\bJ(?:[no][bh])?(?:no|[nou])\\b",       "John",                      TRUE,
  "\\bJo?nh?\\b",                           "John",                      TRUE,
  "\\bJ\\b\\.?\\s?W\\.?",                   "John William",              TRUE,
  "\\bJonathan\\b",                         "Jonathan",                  TRUE,
  "\\bJos(?:eph)?\\b",                      "Joseph",                    TRUE,
  "\\bJosh(?:ua)?\\b",                      "Joshua",                    TRUE,
  "\\bJ\\b\\.?",                            "John",                      TRUE,
  "\\bKen(neth)?\\b(?(1)|\\.)",             "Kenneth",                   TRUE,
  "\\bLaurence\\b",                         "Laurence",                  TRUE,
  "\\bLawr(?:ence)?\\b",                    "Lawrence",                  TRUE,
  "\\bMalc?(?:om)?\\b",                     "Malcolm",                   TRUE,
  "\\bMarg(?:are)?t?\\b",                   "Margaret",                  TRUE,
  "\\bMary\\b",                             "Mary",                      TRUE,
  "\\bMat(hew)?\\b(?(1)|\\.)",              "Mathew",                    TRUE,
  "\\bMath(?:ew)?\\b",                      "Mathew",                    TRUE,
  "\\bMatth?e?w?\\b",                       "Matthew",                   TRUE,
  "\\bMich(?:ael)?\\b",                     "Michael",                   TRUE,
  "\\bMiss?\\b",                            "Miss",                      TRUE,
  "\\bM\\b\\.?",                            "Matthew",                   TRUE,
  "\\bNorm?(?:an)?\\b",                     "Norman",                    TRUE,
  "\\bN\\b\\.?",                            "Neil",                      TRUE,
  "\\bPat(?:ric)?k\\b",                     "Patrick",                   TRUE,
  "\\bPet(?:er)?\\b",                       "Peter",                     TRUE,
  "\\bPhilip\\b",                           "Philip",                    TRUE,
  "\\bP\\b\\.?",                            "Philip",                    TRUE,
  "\\bRich(?:ar)?d?\\b",                    "Richard",                   TRUE,
  "\\bRitchie\\b",                          "Ritchie",                   TRUE,
  "\\bR(?:ob)?(?:er)?[ot]b?\\b",            "Robert",                    TRUE,
  "\\bRon(?:ald)?\\b",                      "Ronald",                    TRUE,
  "\\bRoss\\b",                             "Ross",                      TRUE,
  "\\bR\\b\\.?",                            "Robert",                    TRUE,
  "\\bSam(?:ue)?l?\\b",                     "Samuel",                    TRUE,
  "\\bStembr(?:idge)?\\b",                  "Stembridge",                TRUE,
  "\\bS\\b\\.?",                            "Samuel",                    TRUE,
  "\\bTho(?:ma)?s?\\b",                     "Thomas",                    TRUE,
  "\\bTor(?:rance)?\\b",                    "Torrance",                  TRUE,
  "\\bT\\b\\.?",                            "Thomas",                    TRUE,
  "\\bW(?:alte)?r\\b",                      "Walter",                    TRUE,
  "\\bW(?:illia)?(?:in|m|rn)\\b",           "William",                   TRUE,
  "\\bW\\b\\.?",                            "William",                   TRUE
)


### globals_macs ####

#' "Mac" pre-fixes in name entries
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   "Mac" pre-fixes in directory name entries. For each "Mac" pre-fix a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for "Mac" pre-fix matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_macs <- tibble::tribble(
  ~pattern,                                    ~replacement,                ~ignore_case,
  "\\bM\\s?['ac],?\\s?(?=\\w)",                "Mac",                       FALSE,
  "\\bMac,\\s(?=[a-z])",                       "Mac",                       FALSE
)


### globals_surnames ####

#' Surnames in directory records
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   surnames in directory name entries. For each surname a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for surname matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_surnames <- tibble::tribble(
  ~pattern,                                     ~replacement,               ~ignore_case,
  "\\bAbb?ott?\\b",                             "Abbott",                   TRUE,
  "\\bAbe?rcromb(?:ie|y)\\b",                   "Abercromby",               TRUE,
  "\\bAbe(?:m|rn)ethy\\b",                      "Abernethy",                TRUE,
  "\\bAbrams\\b",                               "Abrams",                   TRUE,
  "\\bAdam\\b",                                 "Adam",                     TRUE,
  "\\bAdams\\b",                                "Adams",                    TRUE,
  "\\bA\\s?damson\\b",                          "Adamson",                  TRUE,
  "\\bAdd?ie\\b",                               "Addie",                    TRUE,
  "\\bAdrain\\b",                               "Adrain",                   TRUE,
  "\\bAdshead\\b",                              "Adshead",                  TRUE,
  "\\bAffleck\\b",                              "Affleck",                  TRUE,
  "\\bA(?:h|ii)n\\b",                           "Ahn",                      TRUE,
  "\\bAgnew\\b",                                "Agnew",                    TRUE,
  "\\bAiken\\b",                                "Aiken",                    TRUE,
  "\\bAikenhead\\b",                            "Aikenhead",                TRUE,
  "\\bAikman\\b",                               "Aikman",                   TRUE,
  "\\bAinslie\\b",                              "Ainslie",                  TRUE,
  "\\bAird\\b",                                 "Aird",                     TRUE,
  "\\bAitch[ei]son\\b",                         "Aitchison",                TRUE,
  "\\bAitk[ei]n\\b",                            "Aitken",                   TRUE,
  "\\bAitkenhead\\b",                           "Aitkenhead",               TRUE,
  "\\bA[il]ton\\b",                             "Aiton",                    TRUE,
  "\\bAlbrecht\\b",                             "Albrecht",                 TRUE,
  "\\bAlexander\\b",                            "Alexander",                TRUE,
  "\\bAlgie\\b",                                "Algie",                    TRUE,
  "\\bAll?iso[nu]\\b",                          "Alison",                   TRUE,
  "\\bAlla[nu]\\b",                             "Allan",                    TRUE,
  "\\bAllardice\\b",                            "Allardice",                TRUE,
  "\\bAlle[nu]\\b",                             "Allen",                    TRUE,
  "\\bAlston\\b",                               "Alston",                   TRUE,
  "\\bAmatore\\b",                              "Amatore",                  TRUE,
  "\\bAmbrose\\b",                              "Ambrose",                  TRUE,
  "\\bAncell\\b",                               "Ancell",                   TRUE,
  "\\bAnderson\\b",                             "Anderson",                 TRUE,
  "\\bAndrew\\b",                               "Andrew",                   TRUE,
  "\\bAndrews\\b",                              "Andrews",                  TRUE,
  "\\bAngus\\b",                                "Angus",                    TRUE,
  "\\bAnnacker\\b",                             "Annacker",                 TRUE,
  "\\bAnnan\\b",                                "Annan",                    TRUE,
  "\\bArbuckle\\b",                             "Arbuckle",                 TRUE,
  "\\bArchibald\\b",                            "Archibald",                TRUE,
  "\\bArmour\\b",                               "Armour",                   TRUE,
  "\\bArmstrong\\b",                            "Armstrong",                TRUE,
  "\\bArneil\\b",                               "Arneil",                   TRUE,
  "\\bArnott?\\b",                              "Arnott",                   TRUE,
  "\\bArrol\\b",                                "Arrol",                    TRUE,
  "\\bArthur\\b",                               "Arthur",                   TRUE,
  "\\bAsher\\b",                                "Asher",                    TRUE,
  "\\bAuchincloss\\b",                          "Auchincloss",              TRUE,
  "\\bAuchinvole\\b",                           "Auchinvole",               TRUE,
  "\\bAuchterlonie\\b",                         "Auchterlonie",             TRUE,
  "\\bAuld\\b",                                 "Auld",                     TRUE,
  "\\bAustin\\b",                               "Austin",                   TRUE,
  "\\bBail?lie\\b",                             "Baillie",                  TRUE,
  "\\bBalderston\\b",                           "Balderston",               TRUE,
  "\\bBannerman\\b",                            "Bannerman",                TRUE,
  "\\bBarnhill\\b",                             "Barnhill",                 TRUE,
  "\\bBarlowe?\\b",                             "Barlow",                   TRUE,
  "\\bBayle?y\\b",                              "Bayly",                    TRUE,
  "\\bBeall?e\\b",                              "Beale",                    TRUE,
  "\\bBen?ett?\\b",                             "Bennett",                  TRUE,
  "\\bBonn?ar\\b",                              "Bonnar",                   TRUE,
  "\\bBeale?\\b",                               "Bealle",                   TRUE,
  "\\bBeatt?(?:ie|y|}')\\b",                    "Beattie",                  TRUE,
  "\\bBell\\b",                                 "Bell",                     TRUE,
  "\\bBerr?(?:y|ie)\\b",                        "Berrie",                   TRUE,
  "\\bBla[ce]kie\\b",                           "Blackie",                  TRUE,
  "\\bBlair?\\b",                               "Blair",                    TRUE,
  "\\bBlythe?\\b",                              "Blyth",                    TRUE,
  "\\bBicker?t(?:on)?\\b",                      "Bickerton",                TRUE,
  "\\bBinn(?:ie|ey)\\b",                        "Binnie",                   TRUE,
  "\\bBirrell?\\b",                             "Birrell",                  TRUE,
  "\\bBissett?\\b",                             "Bissett",                  TRUE,
  "\\bBonn?ar\\b",                              "Bonar",                    TRUE,
  "\\bBookl[ae]ss\\b",                          "Blooklass",                TRUE,
  "\\bBred?ie\\b",                              "Breddie",                  TRUE,
  "\\bBridges?\\b",                             "Bridge",                   TRUE,
  "\\bBroadle?y\\b",                            "Broadley",                 TRUE,
  "\\bBroome?\\b",                              "Broom",                    TRUE,
  "\\bBrowne?\\b",                              "Brown",                    TRUE,
  "\\bBrownl[ei]e\\b",                          "Brownlie",                 TRUE,
  "\\bBryd[eo]n\\b",                            "Bryden",                   TRUE,
  "\\bBun?rrell\\b",                            "Burrell",                  TRUE,
  "\\bBunt[eio]n\\b",                           "Bunten",                   TRUE,
  "\\bBurne?\\b",                               "Burn",                     TRUE,
  "\\bBy[ae]rs\\b",                             "Byars",                    TRUE,
  "\\bCair[nu]s\\b[\\.,]?",                     "Cairns",                   TRUE,
  "\\bCampbell\\b",                             "Campbell",                 TRUE,
  "\\bCann[ao]n\\b",                            "Cannan",                   TRUE,
  "\\bCarroll?\\b",                             "Carroll",                  TRUE,
  "\\bCarswell\\b",                             "Carswell",                 TRUE,
  "\\bCl[ae]rke?\\b",                           "Clark",                    TRUE,
  "\\bClell?and\\b",                            "Cleland",                  TRUE,
  "\\bClubb?\\b",                               "Clubb",                    TRUE,
  "\\bClugstone?\\b",                           "Clugston",                 TRUE,
  "\\bCochrane?\\b",                            "Cochran",                  TRUE,
  "\\bC[ou]lling\\b",                           "Colling",                  TRUE,
  "\\bCoolvill?e?\\b",                          "Coolvill",                 TRUE,
  "\\bC[eo]nnaghan\\b",                         "Connaghan",                TRUE,
  "\\bConnell?\\b",                             "Connell",                  TRUE,
  "\\bCop?land\\b",                             "Copeland",                 TRUE,
  "\\bCorbett?\\b",                             "Corbett",                  TRUE,
  "\\bCo[ou]per\\b",                            "Couper",                   TRUE,
  "\\bCoverley?\\b",                            "Coverley",                 TRUE,
  "\\bCowan\\b",                                "Cowan",                    TRUE,
  "\\b[CG]ree\\b",                              "Cree",                     TRUE,
  "\\bCruic?ks[bh]anks?\\b",                    "Cruickshank",              TRUE,
  "\\b[CG]umming\\b",                           "Cumming",                  TRUE,
  "\\bCunninghame?\\b",                         "Cunninghame",              TRUE,
  "\\bDansk[ei]n\\b",                           "Danskin",                  TRUE,
  "\\bDavie\\b",                                "Davie",                    TRUE,
  "\\bDavie?s\\b",                              "Davis",                    TRUE,
  "\\bD[eo]\\sBois\\b",                         "De Bois",                  TRUE,
  "\\bDenn?ison\\b",                            "Dennison",                 TRUE,
  "\\bDennistou?n\\b",                          "Denniston",                TRUE,
  "\\bDeny\\b",                                 "Derry",                    TRUE,
  "\\bDickson\\b",                              "Dickson",                  TRUE,
  "\\bDixo?n\\b",                               "Dixon",                    TRUE,
  "\\bDobb?ie\\b",                              "Dobbie",                   TRUE,
  "\\bDou?[cg]h[ea]rty\\b",                     "Docherty",                 TRUE,
  "\\bDodd?s?\\b",                              "Dods",                     TRUE,
  "\\bDona[cg]h(?:ie|y)\\b",                    "Donachy",                  TRUE,
  "\\bDonovan\\b",                              "Donovan",                  TRUE,
  "\\bDorm[ao]n\\b",                            "Dormon",                   TRUE,
  "\\bD[ou]ngall\\b",                           "Dougall",                  TRUE,
  "\\bDowns\\b",                                "Downs",                    TRUE,
  "\\bDunnett?\\b",                             "Dunnett",                  TRUE,
  "\\bEast[co]n\\b",                            "Easton",                   TRUE,
  "\\bEdm[aou]nd?s?\\b",                        "Edmond",                   TRUE,
  "\\bEdwards?\\b",                             "Edwards",                  TRUE,
  "\\bElborne?\\b",                             "Elborn",                   TRUE,
  "\\bElliott?\\b",                             "Elliot",                   TRUE,
  "\\bFairl(?:ey|ie)\\b",                       "Fairley",                  TRUE,
  "\\bFe[nu]namore\\b",                         "Fennamore",                TRUE,
  "\\bFennell?\\b",                             "Fennel",                   TRUE,
  "\\bFerguss?on\\b",                           "Fergusson",                TRUE,
  "\\bFind?lay\\b",                             "Finlay",                   TRUE,
  "\\bFind?layson\\b",                          "Finlayson",                TRUE,
  "\\bFinn[ae]rty\\b",                          "Finnerty",                 TRUE,
  "\\bFisk[ei]n\\b",                            "Fiskin",                   TRUE,
  "\\bFlind?t\\b",                              "Flindt",                   TRUE,
  "\\bFra[sz]er\\b",                            "Fraser",                   TRUE,
  "\\b[CG]artland\\b",                          "Gartland",                 TRUE,
  "\\bGai?rdi?ner\\b",                          "Gardner",                  TRUE,
  "\\bGarrow(?:ay)?\\b",                        "Garroway",                 TRUE,
  "\\bGall?ett?[iy]\\b",                        "Galletti",                 TRUE,
  "\\b[CG]atheral\\b",                          "Gatheral",                 TRUE,
  "\\bGrai?nger\\b",                            "Grainger",                 TRUE,
  "\\bGe(?:m|rn)m[ei]ll?\\b",                   "Gemmell",                  TRUE,
  "\\bGentles?\\b",                             "Gentles",                  TRUE,
  "\\bG[il]assford\\b",                         "Glassford",                TRUE,
  "\\bGrai?nger\\b",                            "Grainger",                 TRUE,
  "\\bGre[ae]r\\b",                             "Grear",                    TRUE,
  "\\bGr[ai]tt[ao]n\\b",                        "Gritton",                  TRUE,
  "\\bGunniss?\\b",                             "Gunniss",                  TRUE,
  "\\bHadd?en\\b",                              "Haden",                    TRUE,
  "\\bHagg[ae]rty\\b",                          "Haggarty",                 TRUE,
  "\\bHai?re?\\b",                              "Hair",                     TRUE,
  "\\bHald[ae]ne?\\b",                          "Halden",                   TRUE,
  "\\bHalle?y\\b",                              "Halley",                   TRUE,
  "\\bHanley\\b",                               "Hanley",                   TRUE,
  "\\bHanna[hy]\\b",                            "Hannah",                   TRUE,
  "\\bHarv(?:ie|ey)\\b",                        "Harvey",                   TRUE,
  "\\bHassen\\b",                               "Hassen",                   TRUE,
  "\\bHeale?y\\b",                              "Healy",                    TRUE,
  "\\bHemphill\\b",                             "Hemphill",                 TRUE,
  "\\bHend?r(?:ie|y)\\b",                       "Henry",                    TRUE,
  "\\bHenderson\\b",                            "Henderson",                TRUE,
  "\\bHe[ir]bert?son\\b",                       "Herberson",                TRUE,
  "\\bHewitt\\b",                               "Hewitt",                   TRUE,
  "\\bHetherington\\b",                         "Hetherington",             TRUE,
  "\\bHi(?:g|sr)gins\\b",                       "Higgins",                  TRUE,
  "\\bHill\\b",                                 "Hill",                     TRUE,
  "\\bHillcoat\\b",                             "Hillcoat",                 TRUE,
  "\\bHiller\\b",                               "Hiller",                   TRUE,
  "\\bHillhouse\\b",                            "Hillhouse",                TRUE,
  "\\bHilli?ard\\b",                            "Hilliard",                 TRUE,
  "\\bHillins\\b",                              "Hillins",                  TRUE,
  "\\bHillon\\b",                               "Hillon",                   TRUE,
  "\\bHilpert\\b",                              "Hilpert",                  TRUE,
  "\\bHilton\\b",                               "Hilton",                   TRUE,
  "\\bHinmers\\b",                              "Hinmers",                  TRUE,
  "\\bHinshaw\\b",                              "Hinshaw",                  TRUE,
  "\\bHin[cs]helwood\\b",                       "Hinshelwood",              TRUE,
  "\\bHislop\\b",                               "Hislop",                   TRUE,
  "\\bHobbs\\b",                                "Hobbs",                    TRUE,
  "\\bHodgart\\b",                              "Hodgart",                  TRUE,
  "\\bHodges?\\b",                              "Hodge",                    TRUE,
  "\\bHodg[eh]ton\\b",                          "Hodghton",                 TRUE,
  "\\bHoey\\b",                                 "Hoey",                     TRUE,
  "\\bHogg\\b",                                 "Hogg",                     TRUE,
  "\\bHoggan\\b",                               "Hoggan",                   TRUE,
  "\\bHogganfield\\b",                          "Hogganfield",              TRUE,
  "\\bHollier\\b",                              "Hollier",                  TRUE,
  "\\bHolm\\b",                                 "Holm",                     TRUE,
  "\\bHolme?s\\b",                              "Holmes",                   TRUE,
  "\\bHolt\\b",                                 "Holt",                     TRUE,
  "\\bHome\\b",                                 "Home",                     TRUE,
  "\\bHomewood\\b",                             "Homewood",                 TRUE,
  "\\bHoneyman\\b",                             "Honeyman",                 TRUE,
  "\\bHood\\b",                                 "Hood",                     TRUE,
  "\\bHooper\\b",                               "Hooper",                   TRUE,
  "\\bHopkirk\\b",                              "Hopkirk",                  TRUE,
  "\\bHorn\\b",                                 "Horn",                     TRUE,
  "\\bHo(?:m|rn)e\\b",                          "Horne",                    TRUE,
  "\\bHosie\\b",                                "Hosie",                    TRUE,
  "\\bHossack\\b",                              "Hossack",                  TRUE,
  "\\bHoustou?n\\b",                            "Houston",                  TRUE,
  "\\bHoward\\b",                               "Howard",                   TRUE,
  "\\bHowatson\\b",                             "Howatson",                 TRUE,
  "\\bHowarth\\b",                              "Howarth",                  TRUE,
  "\\bHowatt?\\b",                              "Howat",                    TRUE,
  "\\bHowden\\b",                               "Howden",                   TRUE,
  "\\bHowie\\b",                                "Howie",                    TRUE,
  "\\bHowieson\\b",                             "Howieson",                 TRUE,
  "\\bHowlison\\b",                             "Howlison",                 TRUE,
  "\\bHubbard\\b",                              "Hubbard",                  TRUE,
  "\\bHughes\\b",                               "Hughes",                   TRUE,
  "\\bHume\\b",                                 "Hume",                     TRUE,
  "\\bHunt\\b",                                 "Hunt",                     TRUE,
  "\\bHunter\\b",                               "Hunter",                   TRUE,
  "\\bHussey\\b",                               "Hussey",                   TRUE,
  "\\bH[au]tch[ei]son\\b",                      "Hutcheson",                TRUE,
  "\\bHutton\\b",                               "Hutton",                   TRUE,
  "\\bHyam\\b",                                 "Hyam",                     TRUE,
  "\\bHyde\\b",                                 "Hyde",                     TRUE,
  "\\bHynds?\\b",                               "Hynd",                     TRUE,
  "\\bI(?:m|rn)rie\\b",                         "Imrie",                    TRUE,
  "\\b,?\\s+\\bjun(?:io)?r?\\b",                " Junior",                  TRUE,
  "\\b[EK]incaid\\b",                           "Kincaid",                  TRUE,
  "\\bKin[nu]inmont\\b",                        "Kinninmont",               TRUE,
  "\\bKin[nu]inmont\\b",                        "Kinninmont",               TRUE,
  "\\bLe[ce]k\\b",                              "Leck",                     TRUE,
  "\\bLoudou?n\\b",                             "Loudon",                   TRUE,
  "\\bMac\\sAins(?:h|li)\\b",                   "Mac Ainsh",                TRUE,
  "\\bMac\\sAul[ae]y\\b",                       "Mac Aulay",                TRUE,
  "\\bMac\\sAu?slan\\b",                        "Mac Auslan",               TRUE,
  "\\bMac\\sBr[iy]de\\b",                       "Mac Bride",                TRUE,
  "\\bMac\\sCl[ae]uchan\\b",                    "Mac Cleuchan",             TRUE,
  "\\bMac\\sClem[eo]nt\\b",                     "Mac Clement",              TRUE,
  "\\bMac\\sCluske?y\\b",                       "Mac Clusky",               TRUE,
  "\\bMac\\sConn?[aeo][cg]h(?:ie|y)\\b",        "Mac Connochie",            TRUE,
  "\\bMac\\sCorm?[ai]ck\\b",                    "Mac Cormick",              TRUE,
  "\\bMac\\sCrea?dd?(?:ie|y)\\b",               "Mac Creadie",              TRUE,
  "\\bMac\\sCrind[el][el]l?\\b",                "Mac Crindell",             TRUE,
  "\\bMac\\sDow[ae]ll\\b",                      "Mac Dowall",               TRUE,
  "\\bMac\\sEw[ae]n\\b",                        "Mac Ewan",                 TRUE,
  "\\bMac\\sFad[yz][ae]?[ae]?n\\b",             "Mac Fadyen",               TRUE,
  "\\bMac\\sFedrie?s\\b",                       "Mac Fedries",              TRUE,
  "\\bMac\\sGarre?y\\b",                        "Mac Garrey",               TRUE,
  "\\bMac\\sGh?ie\\b",                          "Mac Gie",                  TRUE,
  "\\bMac\\sGlash[ae]n\\b",                     "Mac Glashan",              TRUE,
  "\\bMac\\sGradd?(?:ie|y)\\b",                 "Mac Graddie",              TRUE,
  "\\bMac\\sInn[ei]s\\b",                       "Mac Innis",                TRUE,
  "\\bMac\\s[il]v[eo]r\b",                      "Mac Iver",                 TRUE,
  "\\bMac\\sKind?l[ae]y\\b",                    "Mac Innis",                TRUE,
  "\\bMac\\sKird(?:ie|y)\\b",                   "Mac Kirdy",                TRUE,
  "\\bMac\\sLau?[cg]hl[ai]n\\b",                "Mac Lachlan",              TRUE,
  "\\bMac\\sLau?r[ei]n\\b",                     "Mac Laren",                TRUE,
  "\\bMac\\sMill[ea]n\\b",                      "Mac Millan",               TRUE,
  "\\bMac\\sNaught[aeo]n\\b",                   "Mac Naughton",             TRUE,
  "\\bMac\\sNeill?\\b",                         "Mac Neill",                TRUE,
  "\\bMac\\sN[ei]e\\b",                         "Mac Nie",                  TRUE,
  "\\bMac\\sParl[ea]ne?\\b",                    "Mac Parlane",              TRUE,
  "\\bMac\\sSymond?\\b",                        "Mac Symon",                TRUE,
  "\\bMains?\\b",                               "Mac Main",                 TRUE,
  "\\bMeigh[ae]n\\b",                           "Meighan",                  TRUE,
  "\\bMeikle?h?am\\b",                          "Meikleham",                TRUE,
  "\\b[HM]eiklejohn\\b",                        "Meiklejohn",               TRUE,
  "\\bMill[ae]r\\b",                            "Millar",                   TRUE,
  "\\bMillig[ae]n\\b",                          "Milligan",                 TRUE,
  "\\bMon(?:ie|y)\\b",                          "Monie",                    TRUE,
  "\\bMontague?\\b",                            "Montague",                 TRUE,
  "\\bMontgomer(?:ie|y)\\b",                    "Montgomery",               TRUE,
  "\\b(?:M|Bl)ood(?:ie|y)\\b",                  "Moodie",                   TRUE,
  "\\bMull[ei]n\\b",                            "Mullen",                   TRUE,
  "\\bMus(?:li|h)et\\b",                        "Mushet",                   TRUE,
  "\\bNeill?\\b",                               "Neil",                     TRUE,
  "\\bNei?lson?\\b",                            "Neilson",                  TRUE,
  "\\bNich?ol\\b",                              "Nichol",                   TRUE,
  "\\bNisbett?\\b",                             "Nichol",                   TRUE,
  "\\bNotma(?:ri|n)\\b",                        "Nichol",                   TRUE,
  "\\bO'Neill?\\b",                             "O'Neill",                  TRUE,
  "\\bOas?tt?\\b",                              "Oatt",                     TRUE,
  "\\bOgilv(?:ie|y)\\b",                        "Ogilvie",                  TRUE,
  "\\bO(?:m|rn)ish\\b",                         "Omish",                    TRUE,
  "\\bOwens?\\b",                               "Owens",                    TRUE,
  "\\bPatt?erson\\b",                           "Patterson",                TRUE,
  "\\bPattie?son\\b",                           "Pattison",                 TRUE,
  "\\bPenne?y\\b",                              "Penney",                   TRUE,
  "\\bPennyc[ou][oi]c?k\\b",                    "Pennycook",                TRUE,
  "\\bPerr(?:ie|y)\\b",                         "Pennycook",                TRUE,
  "\\bPhillips?\\b",                            "Phillips",                 TRUE,
  "\\bPhilps?\\b",                              "Philps",                   TRUE,
  "\\bPigott?\\b",                              "Pigott",                   TRUE,
  "\\bPolloc?k\\b",                             "Pigott",                   TRUE,
  "\\bPriestle?y\\b",                           "Pigott",                   TRUE,
  "\\bRankine?\\b",                             "Rankine",                  TRUE,
  "\\b[ER]ae\\b",                               "Rae",                      TRUE,
  "\\b(?:Ea|R)ankin\\b",                        "Rankin",                   TRUE,
  "\\b[ER]eevey\\b",                            "Reevey",                   TRUE,
  "\\b[BR]eid\\b",                              "Reid",                     TRUE,
  "\\bRichards?\\b",                            "Richard",                  TRUE,
  "\\bRodd[ea]n\\b",                            "Roddan",                   TRUE,
  "\\bRod?gers?\\b",                            "Rodger",                   TRUE,
  "\\b[BR]ough\\b",                             "Rough",                    TRUE,
  "\\bRowand?\\b",                              "Rowand",                   TRUE,
  "\\bSalmond?\\b",                             "Salmond",                  TRUE,
  "\\b,?\\s+\\bsen(?:ior)?\\b",                 " Senior",                  TRUE,
  "\\bScoul?l[ae]r\\b",                         "Scouller",                 TRUE,
  "\\bSeligmann?\\b",                           "Seligmann",                TRUE,
  "\\bSell?[ae]rs?\\b",                         "Sellars",                  TRUE,
  "\\bSheriffs?\\b",                            "Sheriff",                  TRUE,
  "\\bShield?s\\b",                             "Shields",                  TRUE,
  "\\bSill[ea]rs\\b",                           "Sillers",                  TRUE,
  "\\bSimm?\\b",                                "Sim",                      TRUE,
  "\\bSimp?son\\b",                             "Simpson",                  TRUE,
  "\\bSlim[ao]n\\b",                            "Slimon",                   TRUE,
  "\\bSm[ei]llie\\b",                           "Smellie",                  TRUE,
  "\\bSm[iy]th\\b",                             "Smith",                    TRUE,
  "\\bSnedd[eo]n\\b",                           "Sneddon",                  TRUE,
  "\\bSommLerva?ill?e?\\b",                     "Sommerville",              TRUE,
  "\\bSpencer?\\b",                             "Spence",                   TRUE,
  "\\bSteele?\\b",                              "Steel",                    TRUE,
  "\\bStephens?\\b",                            "Stephen",                  TRUE,
  "\\bStratt?on\\b",                            "Stratton",                 TRUE,
  "\\bStruthe[rt]s\\b",                         "Struthers",                TRUE,
  "\\bSwann?\\b",                               "Swan",                     TRUE,
  "\\bTa[iy]lor\\b",                            "Taylor",                   TRUE,
  "\\bTenn[ae]nt\\b",                           "Tennent",                  TRUE,
  "\\bThomp?son\\b",                            "Thompson",                 TRUE,
  "\\bThynn?e\\b",                              "Thyne",                    TRUE,
  "\\bTodd?\\b",                                "Tod",                      TRUE,
  "\\bTon[ae]r\\b",                             "Toner",                    TRUE,
  "\\bTosha[co]h\\b",                           "Toshach",                  TRUE,
  "\\bTow[ae]rt\\b",                            "Towart",                   TRUE,
  "\\bWalshe?\\b",                              "Walsh",                    TRUE,
  "\\bWylde?\\b",                               "Wylde",                    TRUE,
  "\\bYuille?\\b",                              "Yuill",                    TRUE
)















































## globals_occupations ####

#' Occupations in directory records
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   occupations in directory entries. For each occupation a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for occupation matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_occupations <- tibble::tribble(
  ~pattern,                                                                                                                                        ~replacement,                     ~ignore_case,
  "\\b((?:commission|emigration|insurance|packet|ship)?(?:\\sand\\s)?(?:egg|emigration|provision|rag|spirit|steam-packet)?\\s?)agent(s)?\\b",      "\\1agent\\2",                    TRUE,
  "\\bba?ke?r\\b",                                                                                                                                 "baker",                          TRUE,
  "\\bbookman\\b",                                                                                                                                 "bookman",                        TRUE,
  "\\bbookseller\\b",                                                                                                                              "bookseller",                     TRUE,
  "\\bbuilder\\b",                                                                                                                                 "builder",                        TRUE,
  "\\bcabinetmaker\\b",                                                                                                                            "cabinetmaker",                   TRUE,
  "\\b(letter-)?carr(?:ier)?\\b",                                                                                                                  "\\1carrier",                     TRUE,
  "\\bcarter\\b",                                                                                                                                  "carter",                         TRUE,
  "\\bcivil engineer\\b",                                                                                                                          "civil engineer",                 TRUE,
  "\\bcoalmaster\\b",                                                                                                                              "coalmaster",                     TRUE,
  "\\bcollector\\b",                                                                                                                               "collector",                      TRUE,
  "\\bdairyman\\b",                                                                                                                                "dairyman",                       TRUE,
  "\\b((?:wine)?(?:\\sand\\s)?(?:spirit)?\\s?)dealer\\b",                                                                                          "\\1dealer",                      TRUE,
  "\\bdraper\\b",                                                                                                                                  "draper",                         TRUE,
  "\\bengineer\\b",                                                                                                                                "engineer",                       TRUE,
  "\\bfruiterer\\b",                                                                                                                               "fruiterer",                      TRUE,
  "\\bgeneral outfitter\\b",                                                                                                                       "general outfitter",              TRUE,
  "\\b(green-)?grocer\\b",                                                                                                                         "\\1grocer",                      TRUE,
  "\\bhairdresser\\b",                                                                                                                             "hairdresser",                    TRUE,
  "\\bhouse\\s(factor|proprietor)\\b",                                                                                                             "house \\1",                      TRUE,
  "\\b((?:gas meter|railway)?\\s?)inspector\\b",                                                                                                   "\\1inspector",                   TRUE,
  "\\bironmonger\\b",                                                                                                                              "ironmonger",                     TRUE,
  "\\biron turner\\b",                                                                                                                             "iron turner",                    TRUE,
  "\\b((?:letterpress)?\\s?)printer\\b",                                                                                                           "\\1printer",                     TRUE,
  "\\b((?:glue)?(?:\\sand\\s)?(?:size)?\\s?)maker\\b",                                                                                             "\\1maker",                       TRUE,
  "\\b((?:cap|heel|tobacco)?(?:\\sand\\s)?(?:snuff|toeplate)?(?:\\sand\\s)?(?:nail)?\\s?)manufacturer?\\b",                                        "\\1manufacturer",                TRUE,
  "\\bmanager\\b",                                                                                                                                 "manager",                        TRUE,
  "\\bmanaging partner\\b",                                                                                                                        "managing partner",               TRUE,
  "\\bmanufacturing chemist\\b",                                                                                                                   "manufacturing chemist",          TRUE,
  "\\bmason\\b",                                                                                                                                   "mason",                          TRUE,
  "\\bmeasurer\\b",                                                                                                                                "measurer",                       TRUE,
  "\\b((?:butter|canvas|cigar|commission|fi[s3]h|grain|spirit|vegetable|wine)?(?:\\sand\\s)?(?:egg|provision|rag|spirit)?\\s?)merch(?:an)?t\\b",   "\\1merchant",                    TRUE,
  "\\bmil(?:liner)?\\.? and dressm(?:aker)?\\b\\.?",                                                                                               "milliner and dressmaker",        TRUE,
  "\\bminister\\b",                                                                                                                                "minister",                       TRUE,
  "\\b((?:bar-|gas|medical)?\\s?)officer\\b",                                                                                                      "\\1officer",                     TRUE,
  "\\bprecentor\\b",                                                                                                                               "precentor",                      TRUE,
  "\\bphysician\\b",                                                                                                                               "physician",                      TRUE,
  "\\bpublisher\\b",                                                                                                                               "publisher",                      TRUE,
  "\\briver-pilot\\b",                                                                                                                             "river-pilot",                    TRUE,
  "\\bsaddler\\b",                                                                                                                                 "saddler",                        TRUE,
  "\\bslater\\b",                                                                                                                                  "slater",                         TRUE,
  "\\bsurgeon\\b",                                                                                                                                 "surgeon",                        TRUE,
  "\\b((?:medical)?\\s?)superintendent\\b",                                                                                                        "\\1superintendent",              TRUE,
  "\\btanyard\\b",                                                                                                                                 "tanyard",                        TRUE,
  "\\btin packing case maker\\b",                                                                                                                  "tin packing case maker",         TRUE,
  "\\bteacher\\b",                                                                                                                                 "teacher",                        TRUE,
  "\\btobacconist\\b",                                                                                                                             "tobacconist",                    TRUE,
  "\\b((?:wholesale cloth and tailor's)?\\s?)trimming\\b",                                                                                         "//1trimming",                    TRUE,
  "\\bupholsterer\\b",                                                                                                                             "upholsterer",                    TRUE,
  "\\bvictuall?er\\b",                                                                                                                             "victualler",                     TRUE,
  "\\bwarehouseman\\b",                                                                                                                            "warehouseman",                   TRUE,
  "\\bwriter\\b",                                                                                                                                  "writer",                         TRUE
)
















































## globals_titles ####

#' Titles in directory name records
#'
#' A dataset containing regular expression meant to match commonly (OCR) misread
#'   titles in directory name records. For each title a replacement
#'   pattern is provided for used in substitution operations as well as a boolean
#'   operator indicating whether the corresponding regex is case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for title matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_titles <- tibble::tribble(
  ~pattern,                              ~replacement,                      ~ignore_case,
  "\\bAdvisee\\b",                       "Advisee",                         TRUE,
  "\\bCapt(?:ain)?\\b",                  "Captain",                         TRUE,
  "\\bLieutenant\\b",                    "Lieutenant",                      TRUE,
  "\\bMajor\\b",                         "Major",                           TRUE,
  "\\bRev\\b\\.?",                       "Reverend",                        TRUE,
  "\"Miss(es)?\\b\\.?",                  "Miss\\1",                         TRUE,
  "\\bMrs\\b\\.?",                       "Mrs.",                            TRUE
)
















































## Others ####

## globals_ampersand_vector ####

#' Ampersand in directory entries
#'
#' A character vector of regular expressions to match common (OCR) errors in
#'   reading the ampersand character: "&" in directory entries.
#'
#' @format A character string vector.
globals_ampersand_vector <- c(
  "[<('6ciqt]?[.$\u00a7#}|*%?&\u00a334dfijJoqSy][.$~;*\u00a6\u2022/\\-\"'cefjy[]?",
  "<f \\/", "tj \\-"
)


## globals_and_single_quote ####

#' Ampersand in directory entries
#'
#' A character vector of regular expressions to match common (OCR) errors in
#'   reading the ampersand character: "&" in directory entries.
#'
#' @format A character string vector.
#'
#' @section Details:
#'   Some regexes contain the single quote character: "'".
globals_and_single_quote <- c(
  "\\'&", "&", "\\$", "\\$\u00a6", "\\$\u2022", "#", "\u00a7", "\u00a7\u2022",
  "\\(f", "\\(J\\-", "<\\.f", "<\\*", "<%", "<\u00a3\u00a6", "<\u00a3\u2022",
  "<\\$\u00a6", "<\u00a7", "<\u00a7\u00a6",
  # "<\\|\\*", "<\\|\\'",
  "<f", "<f\\*",
  "<\\-f", "<j", "<j\\-", "<j\\'", "<J \\-", "<J\\-", "<J\\'", "<J\\*", "<y",
  "<\\$\u2022",
  "\u00abf", "and", "c\\$\\'", "c\\)\\'\\-",
  "4", "4\\'",
  "cf", "cf\\'", "cj\\'", "cj\\-", "cj\\*", "cy", "d\\-", "d;", "ef", "ej\\-",
  "f\\.", "fy", "g\\u00b0\\-", "if", "ij\\'", "ij\\-", "ij \\-", "oJ", "q", "rj\\-",
  "S;", "t j\\-", "tf", "ti;", "tj\\-", "tj \\-", "ty"
)

## globals_and_double_quote ####

#' Ampersand in directory entries
#'
#' A character vector of regular expressions to match common (OCR) errors in
#'   reading the ampersand character: "&" in directory entries.
#'
#' @format A character string vector.
#'
#' @section Details:
#'   Some regexes contain the double quote character: '"'.
globals_and_double_quote <- c('<J\\"', 'cj\\"')


## globals_ampersand ####

#' Ampersand in directory entries
#'
#' A dataset containing regular expression meant to match common (OCR) errors in
#'   reading the ampersand character: "&" in directory entries. For each error
#'   pattern a replacement pattern is provided for used in substitution operations
#'   as well as a boolean operator indicating whether the corresponding regex is
#'   case sensitive or not.
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{pattern}{regex for ampersand reading error matching}
#'   \item{replacement}{replacement pattern for substitution operations}
#'   \item{ignore_case}{
#'     boolean operator indicating whether the corresponding
#'     regex is case sensitive or not.
#'   }
#' }
globals_ampersand <- tibble::tribble(
  ~pattern,                         ~replacement,                           ~ignore_case,
  "<f \\/",                         "&",                                    TRUE,
  "\\btj \\-",                      "&",                                    TRUE,
  "\\'&",                           "&",                                    TRUE,
  "\\$",                            "&",                                    TRUE,
  "\\$\u00a6",                      "&",                                    TRUE,
  "\\$\u2022",                      "&",                                    TRUE,
  "#",                              "&",                                    TRUE,
  "\u00a7",                         "&",                                    TRUE,
  "\u00a7\u2022",                   "&",                                    TRUE,
  "\\(f",                           "&",                                    TRUE,
  "\\(J\\-",                        "&",                                    TRUE,
  "<\\.f",                          "&",                                    TRUE,
  "<\\*",                           "&",                                    TRUE,
  "<%",                             "&",                                    TRUE,
  "<\u00a3\u00a6",                  "&",                                    TRUE,
  "<\u00a3\u2022",                  "&",                                    TRUE,
  "<\\$\u00a6",                     "&",                                    TRUE,
  "<\u00a7",                        "&",                                    TRUE,
  "<\u00a7\u00a6",                  "&",                                    TRUE,
  "<\\|\\*",                        "&",                                    TRUE,
  "<\\|\\'",                        "&",                                    TRUE,
  "<f\\b",                          "&",                                    TRUE,
  "<f\\*",                          "&",                                    TRUE,
  "<\\-f",                          "&",                                    TRUE,
  "<j\\b",                          "&",                                    TRUE,
  "<j\\-",                          "&",                                    TRUE,
  "<j\\'",                          "&",                                    TRUE,
  "<J \\-",                         "&",                                    TRUE,
  "<J\\-",                          "&",                                    TRUE,
  "<J\\'",                          "&",                                    TRUE,
  "<J\\*",                          "&",                                    TRUE,
  "<y\\b",                          "&",                                    TRUE,
  "<\\$\u2022",                     "&",                                    TRUE,
  "\u00abf\\b",                     "&",                                    TRUE,
  "\\band\\b",                      "&",                                    TRUE,
  "c\\$\\'",                        "&",                                    TRUE,
  "c\\)\\'\\-",                     "&",                                    TRUE,
  "4",                              "&",                                    TRUE,
  "4\\'",                           "&",                                    TRUE,
  "\bcf\\b",                        "&",                                    TRUE,
  "\\bcf\\'",                       "&",                                    TRUE,
  "\\bcj\\b\\'",                    "&",                                    TRUE,
  "\\bcj\\-",                       "&",                                    TRUE,
  "\\bcj\\*",                       "&",                                    TRUE,
  "\\bcy\\b",                       "&",                                    TRUE,
  "\\bd\\-",                        "&",                                    TRUE,
  "\\bd;",                          "&",                                    TRUE,
  "\\bef\\b",                       "&",                                    TRUE,
  "\\bej\\-",                       "&",                                    TRUE,
  "\\bf\\.",                        "&",                                    TRUE,
  "\\bfy\\b",                       "&",                                    TRUE,
  "\\bg\\u00b0\\-",                 "&",                                    TRUE,
  "\\bif\\b",                       "&",                                    TRUE,
  "\\bij\\'",                       "&",                                    TRUE,
  "\\bij\\-",                       "&",                                    TRUE,
  "\\bij \\-",                      "&",                                    TRUE,
  "\\boJ\\b",                       "&",                                    TRUE,
  "\\bq\\b",                        "&",                                    TRUE,
  "\\brj\\-",                       "&",                                    TRUE,
  "\\bS;",                          "&",                                    TRUE,
  "\\bt j\\-",                      "&",                                    TRUE,
  "\\btf\\b",                       "&",                                    TRUE,
  "\\bti;",                         "&",                                    TRUE,
  "\\btj\\-",                       "&",                                    TRUE,
  "\\btj \\-",                      "&",                                    TRUE,
  "\\bty\\b",                       "&",                                    TRUE,
  '<J\\"',                          "&",                                    TRUE,
  '\\bcj\\"',                       "&",                                    TRUE
)


# Load ####

## globals_general_colnames ####

#' General directory column names
#'
#' A character vector of column names for general directories.
#'
#' @format A character string vector.
globals_general_colnames <- c(
  "page", "surname", "forename", "occupation", "addresses"
)


## globals_trades_colnames ####

#' Trades directory column names
#'
#' A character vector of column names for trades directories.
#'
#' @format A character string vector.
globals_trades_colnames <- c(
  "page", "rank", "occupation", "type", "surname", "forename",
  "address.trade.body", "address.trade.number"
)


## globals_union_colnames ####

#' Combined directories column names
#'
#' A character vector of column names for the dataset where general directory
#'   records are matched to trades directory records.
#'
#' @format A character string vector.
globals_union_colnames <- c(
  "directory", "page", "rank", "surname", "forename", "occupation", "type",
  "address.trade.number", "address.trade.body",
  "address.house.number", "address.house.body"
)

# Fix structure ####

## House ####

## globals_regex_house_to_address ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to move the word "house" from the occupation
#'   column to the addresses column in general directory.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_move_house_to_address}}
globals_regex_house_to_address <- ";\\s\\b(?:[bh][op](?:use)?|res(?:idence)?)\\b\\.?.*"


## globals_regex_house_split_trade ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to separate trades from house addresses in general
#'   directory.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_split_trade_house_addresses}}
globals_regex_house_split_trade <- paste0(
  "(?:^|[;,\u201e\\s]*)",
  "(?<![\\-])?",
  "\\b(?:",
  "res(?:id)?(?:ence)?",
  "|",
  "(?:(?:[bdht]|li|jh)[aop])(?:[ui\\/]se)?s?",
  ")\\b",
  "[.,\u201e\\s]+"
)

## globals_regex_occupation_from_address ####

## Occupation ####
#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to repatriate occupation from address column in general
#'   directory.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_repatriate_occupation_from_address}}
globals_regex_occupation_from_address <- paste0(
  "^(?:and\\s?)?(?:(?:",
  paste(globals_occupations$pattern, collapse = ")|(?:"),
  ")).+?(?=,|;|\\d|[A-Z]|$)"
)


## globals_regex_address_prefix ####

## Address ####
#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to remove undesired pre-fixes in general
#'   directory address records.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{utils_remove_address_prefix}}
globals_regex_address_prefix <-
  "\\b(?:depot|(?<!post\\s)office|stores|work(?:shop)?s?)\\b"


## globals_regex_split_trade_addresses ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to split multiple trade addresses when more than one
#'   are provided.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{utils_remove_address_prefix}}
globals_regex_split_trade_addresses <- paste0(
  "(?<=",
  paste(
    "^",
    ";",
    "(?<=\\D\\D)\\s\\ba[an]d\\b\\s(?=[A-Z0-9])",
    "(?<=\\D\\D)\\s\\ba[an]d,\\b\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\s\\&\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\&(?=[A-Z0-9])",
    "(?<!and|\\d)[,;]\\s(?!and|\\D)",
    sep = "|"),
  ")",
  ".+?",
  "(?=(?:",
  paste(
    ";",
    "$",
    "(?<=\\D\\D)\\s\\ba[an]d\\b\\s(?=[A-Z0-9])",
    "(?<=\\D\\D)\\s\\ba[an]d,\\b\\s(?=[A-Z0-9])",
    # "(?<=\\D),\\s\\ba[an]d\\b\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\s\\&\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\&(?=[A-Z0-9])",
    "(?<!and|\\d)[,;]\\s(?!and|\\D)",
    sep = "|"),
  "))"
)


## globals_regex_and_filter ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to the word "and" in a filtering operation part of a
#'   mutate operation in the general directory provided.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_split_trade_addresses}}
globals_regex_and_filter <- "^and\\s\\d"


## globals_regex_and_match ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to match the word "and" in a filtering operation part
#'   of a mutate operation in the general directory provided.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_split_trade_addresses}}
globals_regex_and_match <- "^and\\s\\K.+"


## globals_regex_irrelevants ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to match irrelevant information in the directory
#'   dataset provided.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{utils_clear_irrelevants}}
globals_regex_irrelevants <- "(?:character\\(0\\)?\\.?|\\s?[\u2014\\-(]\\s?See.+|Appendix.+)"


## globals_regex_split_address_numbers ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to separate numbers from body in provided general
#'   directory address entries.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_split_address_numbers_bodies}}
globals_regex_split_address_numbers <- "^[0-9,\\-\\s/]+?(?=\\s[[:alpha:]])"


## globals_regex_split_address_body ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to separate numbers from body in provided general
#'   directory address entries.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_split_address_numbers_bodies}}
globals_regex_split_address_body <- "^(?:[0-9,\\-\\s/]+?(?=[[:alpha:]]))?\\K.+"


## globals_regex_split_address_empty ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to separate numbers from body in provided general
#'   directory address entries.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{general_split_address_numbers_bodies}}
globals_regex_split_address_empty <- "^$"


# Clean entries ####

## globals_regex_titles ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used to match title in provided directory name entries.
#'
#' @format A character string vector.
globals_regex_titles <- paste0(
  '[[:punct:][:blank:]]*(?:', paste(globals_titles$pattern, collapse = "|"),
  ')[[:punct:][:blank:]]*'
)

# Combine ####

## globals_regex_address_house_body_number ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used in the making of the match.string that eventually
#'   enables the matching of general and trades directory records.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{combine_label_failed_matches}}
globals_regex_address_house_body_number <- "house.(?:body|number)"


## globals_regex_get_address_house_type ####

#' Regular expression for mutate operations in directory datasets
#'
#' Regular expression used in the making of the match.string that eventually
#'   enables the matching of general and trades directory records.
#'
#' @format A character string vector.
#'
#' @seealso \code{\link{combine_get_address_house_type}}
globals_regex_get_address_house_type <- "(?<=house\\.).+$"
