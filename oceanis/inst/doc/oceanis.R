## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE)

## -----------------------------------------------------------------------------
donnees_monoloc <- rio::import(file = system.file("data/donnees_monoloc.rda",
                                                  package = "oceanis"))

## ---- fig.width = 5-----------------------------------------------------------
library(oceanis)

# chargement des donnees
donnees_monoloc <- rio::import(file = system.file("data/donnees_monoloc.rda",
                                                  package = "oceanis"))
# visualisation de la distribution de la variable VAR_AN_MOY
distrib_variable(data = donnees_monoloc,
                 varRatio = "VAR_AN_MOY",
                 nbClasses = 4)

## ----fondCarte, fig.height = 3, fig.width = 3---------------------------------
# chemin du fond de carte .shp
path_to_shp <- system.file("extdata",
                           "dep_francemetro_2018.shp",
                           package = "oceanis")
# import de l'objet sf
depm <- sf::st_read(dsn = path_to_shp, quiet = TRUE)

# visualisation de la geometrie
# modification des marges
par(mai = c(0,0,0,0))
# contour des departements de France metropolitaine
plot(sf::st_geometry(depm))

## ----zonageAFacon, fig.height = 5, fig.width = 5------------------------------
library(oceanis)

# chargement des donnees :
# donnees a facon avec variable de regroupement (ZE2010)
donnees_a_facon <- rio::import(file = system.file("data/donnees_a_facon.rda",
                                                  package = "oceanis"))
# fond communal des departements 13, 30, 83 et 84
com_dep_13_30_83_84 <- sf::st_as_sf(rio::import(file = system.file("data/com_dep_13_30_83_84.rda",
                                                      package = "oceanis")))

# chemin du fond de carte .shp
path_to_shp <- system.file("extdata",
                           "dep_francemetro_2018.shp",
                           package = "oceanis")
# import en objet sf
depm <- sf::st_read(dsn = path_to_shp, quiet = TRUE, stringsAsFactors = FALSE)

# creation du zonage des zones d'emploi des Bouches-du-Rhone (partie entiere des ze)
ze13etplus <- zonage_a_facon(fondMaille = com_dep_13_30_83_84,
                             groupe = donnees_a_facon,
                             idMaille = "DEPCOM",
                             idGroupe = "ZE2010",
                             libGroupe = "LIB_ZE2010",
                             fondContour = NULL)
# creation du zonage des zones d'emploi des Bouches-du-Rhone (partie tronquee des ze au contour du departement)
ze13 <- zonage_a_facon(fondMaille = com_dep_13_30_83_84,
                       groupe = donnees_a_facon,
                       idMaille = "DEPCOM",
                       idGroupe = "ZE2010",
                       libGroupe = "LIB_ZE2010",
                       fondContour = depm[depm$CODE=="13",])

# visualisation de la geometrie
# modification des marges
par(mai = c(0,0,0,0))
# fond des ze des Bouches-du-Rhone en 1er pour fixer le niveau de zoom, en bleu
plot(sf::st_geometry(ze13etplus),
     col = "powderblue",
     border = "transparent")
# fond de la partie tronquee des ze des Bouches-du-Rhone, en rouge
plot(sf::st_geometry(ze13),
     col = "lightsalmon",
     border = "transparent",
     add = TRUE)
# contour des communes
plot(sf::st_geometry(com_dep_13_30_83_84),
     col = "transparent",
     border = "lavender",
     add = TRUE)
# contour de la partie tronquee des ze des Bouches-du-Rhone, en rouge
plot(sf::st_geometry(ze13),
     col = "transparent",
     border = "indianred",
     lwd = 3,
     add = TRUE)
# contour des ze des Bouches-du-Rhone, en bleu
plot(sf::st_geometry(ze13etplus),
     col = "transparent",
     border = "steelblue",
     lwd = 3,
     add = TRUE)

## ----shinyRondsAnalyseClasses, eval = FALSE-----------------------------------
#  library(oceanis)
#  library(shiny)
#  
#  # chargement des donnees
#  donnees_monoloc <- rio::import(file = system.file("data/donnees_monoloc.rda",
#                                                    package = "oceanis"))
#  
#  # import du fond des departements
#  depm <- sf::st_read(dsn = system.file("extdata",
#                                        "dep_francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  # import du fond des regions
#  regm <- sf::st_read(dsn = system.file("extdata",
#                                        "reg_francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  # import du fond de France metropolitaine
#  fram <- sf::st_read(dsn = system.file("extdata",
#                                        "francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  
#  shiny_classes_ronds(data = donnees_monoloc,
#                      fondMaille = depm[depm$REG %in% c("93","94"),],
#                      fondMailleElargi = depm,
#                      fondContour = fram,
#                      fondSuppl = regm,
#                      idData = "COD_DEP",
#                      varVolume = "POP_2015",
#                      varRatio = "VAR_AN_MOY")

## ----shinyJoignantes, eval = FALSE--------------------------------------------
#  library(oceanis)
#  library(shiny)
#  
#  # chargement des donnees
#  donnees_biloc <- rio::import(file = system.file("data/donnees_biloc.rda",
#                                                  package = "oceanis"))
#  
#  # import du fond des regions
#  regm <- sf::st_read(dsn = system.file("extdata",
#                                        "reg_francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  # import du fond des departements
#  depm <- sf::st_read(dsn = system.file("extdata",
#                                        "dep_francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  # import du fond de France metropolitaine
#  fram <- sf::st_read(dsn = system.file("extdata",
#                                        "francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  
#  shiny_joignantes(data = donnees_biloc,
#                   fondMaille = regm,
#                   typeMaille = "REG",
#                   fondContour = fram,
#                   fondSuppl = depm,
#                   idDataDepart = "REG_DEPART",
#                   idDataArrivee = "REG_ARRIVEE",
#                   varFlux = "MIGR",
#                   decalageAllerRetour = 10,
#                   decalageCentroid = 20)

## ----leafletAnalyseClassesRonds, fig.height = 6, fig.width = 9----------------
library(oceanis)
library(leaflet)
library(sf)
library(classInt)
library(leaflet.extras)
library(stringr)

# chargement des donnees
donnees_monoloc <- rio::import(file = system.file("data/donnees_monoloc.rda",
                                                  package = "oceanis"))

# import du fond des departements
depm <- sf::st_read(dsn = system.file("extdata",
                                      "dep_francemetro_2018.shp",
                                      package = "oceanis"),
                    quiet = TRUE,
                    stringsAsFactors = FALSE)
# import du fond des regions
regm <- sf::st_read(dsn = system.file("extdata",
                                      "reg_francemetro_2018.shp",
                                      package = "oceanis"),
                    quiet = TRUE,
                    stringsAsFactors = FALSE)

# affichage de la carte
map <- leaflet_ronds_classes(data = donnees_monoloc,
                             fondMaille = depm[depm$REG=="93",],
                             fondMailleElargi = depm,
                             fondSuppl = regm,
                             idData = "COD_DEP",
                             varVolume = "POP_2015",
                             varRatio = "VAR_AN_MOY")

# affichage du rayon du rond le plus grand en metres
rayon_ronds(map)

# affichage de la carte avec des rayons de ronds plus grands
map <- leaflet_ronds_classes(data = donnees_monoloc,
                             fondMaille = depm[depm$REG == "93",],
                             fondMailleElargi = depm,
                             fondSuppl = regm,
                             idData = "COD_DEP",
                             varVolume = "POP_2015",
                             varRatio = "VAR_AN_MOY",
                             rayonRond = 29000,
                             rapportRond = NULL)

# affichage du rapport du rond le plus grand, a recuperer pour permettre la comparaison de plusieurs cartes
rapport_ronds(map)

# ajout de la legende des ronds (position par defaut)
map <- add_legende_ronds(map = map,
                         titre = "Population en 2015",
                         zoom = 6)
# ajout de la legende des classes (position par defaut)
map <- add_legende_classes(map = map,
                           titre = "Variation ann.moy. 2010-2015",
                           zoom = 6)

# modification de la position de la legende des ronds et du niveau de zoom
map <- add_legende_ronds(map = map,
                         titre = "Population en 2015",
                         lng = 8,
                         lat = 44,
                         zoom = 8)
# modification de la position de la legende des classes et du niveau de zoom
map <- add_legende_classes(map = map,
                           titre = "Variation ann.moy. 2010-2015",
                           lng = 7.5,
                           lat = 43.5,
                           zoom = 8)

# ajout d'une source a la carte
map <- add_source(map = map,
                  source = "Source : INSEE - RP2016")
# ajout d'un titre a la carte
map <- add_titre(map = map,
                 titre = "Population des departements de la region Provence-Alpes-Cote d'Azur en 2015 et son evolution depuis 2010")

# affichage de la palette par defaut
# affiche_palette(nomPalette = "defaut")
recup_palette(stylePalette = "defaut", nbPos = 3)

# affichage de la palette Insee_Rouge
recup_palette(stylePalette = "Insee_Rouge", nbPos = 3)

# modification du style de la palette et de la bordure des ronds
map <- set_couleur_classes(map = map,
                           stylePalette = "Insee_Rouge",
                           colBorder = "grey")

# modification de l'opacite de la representation elargie
map <- set_opacite_elargi(map = map,
                          opacite = 0.3)

map

## ----plotSaphirs, fig.height = 6, fig.width = 7-------------------------------
library(oceanis)

# chargement des donnees
donnees_biloc_saphirs <- rio::import(file = system.file("data/donnees_biloc_saphirs.rda",
                                                        package = "oceanis"))

# import du fond des regions
regm <- sf::st_read(dsn = system.file("extdata",
                                      "reg_francemetro_2018.shp",
                                      package = "oceanis"),
                    quiet = TRUE,
                    stringsAsFactors = FALSE)
# import du fond de France metropolitaine
fram <- sf::st_read(dsn = system.file("extdata",
                                      "francemetro_2018.shp",
                                      package = "oceanis"), 
                    quiet = TRUE,
                    stringsAsFactors = FALSE)
# import du fond des pays
paysm <- sf::st_read(dsn = system.file("extdata",
                                       "paysf_2018.shp",
                                       package = "oceanis"),
                     quiet = TRUE,
                     stringsAsFactors = FALSE)
# import du fond de mer
merm <- sf::st_read(dsn = system.file("extdata",
                                      "merf_2018.shp",
                                      package = "oceanis"),
                    quiet = TRUE,
                    stringsAsFactors = FALSE)

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs,
                             fondMaille = regm,
                             typeMaille = "REG",
                             idDataDepart = "REG_DEPART",
                             idDataArrivee = "REG_ARRIVEE",
                             varFlux = "MIGR",
                             direction = "Ent",
                             titreLeg = "Entrees",
                             xLeg = 1100000,
                             yLeg = 6470000,
                             titreCarte = "Migrations residentielles vers l'Ile-de-France",
                             sourceCarte = "Source : INSEE - RP2016",
                             colBorder = "transparent",
                             colBorderMaille = "grey")

# construction de la table des etiquettes
etiquettes <- coordonnees_etiquettes(fondMaille = regm,
                                     listeCode = as.character(regm$CODE))
# modification des valeurs (latitude Y, longitude X, taille, couleur et style de police)
etiquettes[etiquettes$CODE=="24","Y"] <- 6680000
etiquettes[etiquettes$CODE=="27","Y"] <- 6660000
etiquettes[etiquettes$CODE=="28","X"] <- 410000
etiquettes[etiquettes$CODE=="32","Y"] <- 7015000
etiquettes[etiquettes$CODE=="44","X"] <- 955000
etiquettes[etiquettes$CODE=="52","X"] <- 330000
etiquettes[etiquettes$CODE=="52","Y"] <- 6700000
etiquettes[etiquettes$CODE=="53","X"] <- 215000
etiquettes[etiquettes$CODE=="75","Y"] <- 6420000
etiquettes[etiquettes$CODE=="76","Y"] <- 6270000
etiquettes[etiquettes$CODE=="84","Y"] <- 6455000
etiquettes[etiquettes$CODE=="93","Y"] <- 6290000
etiquettes[etiquettes$CODE=="94","Y"] <- 6120000
etiquettes[etiquettes$CODE!="11","TAILLE"] <- 0.6
etiquettes[etiquettes$CODE=="11","COL"] <- "#002D7F"
etiquettes[etiquettes$CODE!="11","FONT"] <- 1

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs,
                             fondMaille = regm,
                             typeMaille = "REG",
                             idDataDepart = "REG_DEPART",
                             idDataArrivee = "REG_ARRIVEE",
                             varFlux = "MIGR",
                             direction = "Ent",
                             titreLeg = "Entrees",
                             xLeg = 1150000,
                             yLeg = 6470000,
                             titreCarte = "Migrations residentielles vers l'Ile-de-France",
                             sourceCarte = "Source : INSEE - RP2016",
                             etiquettes = etiquettes,
                             colBorder = "transparent",
                             colBorderMaille = "grey")

# ajout de colonnes dans les fonds pour modifier leur apparence
# couleur de remplissage : COL
# couleur des contours : BORDER
# epaisseur des contours : EPAISEEUR
merm$COL <- "lightsteelblue"
merm$BORDER <- "lightsteelblue"
paysm$COL <- "gray"
paysm$BORDER <- "white"
fram$BORDER <- "darkgray"
fram$EPAISSEUR <- 2

# creation des listes des fonds d'habillage, en-dessous et au-dessus de l'analyse
fondSousAnalyse <- list(merm,paysm)
fondSurAnalyse <- list(fram)

# affichage de la carte
fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs,
                             fondMaille = regm,
                             fondSousAnalyse = fondSousAnalyse,
                             fondSurAnalyse = fondSurAnalyse,
                             typeMaille = "REG",
                             idDataDepart = "REG_DEPART",
                             idDataArrivee = "REG_ARRIVEE",
                             varFlux = "MIGR",
                             direction = "Ent",
                             titreLeg = "Entrees",
                             xLeg = 1150000,
                             yLeg = 6470000,
                             titreCarte = "Migrations residentielles vers l'Ile-de-France",
                             sourceCarte = "Source : INSEE - RP2016",
                             etiquettes = etiquettes,
                             colBorder = "transparent",
                             colBorderMaille = "grey")


## ----exportImage, eval = FALSE------------------------------------------------
#  
#  library(grDevices)
#  
#  jpeg(filename = "sortie.jpg",
#       quality = 100,
#       width = 16,
#       height = 18,
#       units = "cm",
#       res = 120)
#  
#  # affichage de la carte
#  fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs,
#                               fondMaille = regm,
#                               fondSousAnalyse = fondSousAnalyse,
#                               fondSurAnalyse = fondSurAnalyse,
#                               typeMaille = "REG",
#                               idDataDepart = "REG_DEPART",
#                               idDataArrivee = "REG_ARRIVEE",
#                               varFlux = "MIGR",
#                               direction = "Ent",
#                               titreLeg = "Entrees",
#                               xLeg = 1150000,
#                               yLeg = 6470000,
#                               titreCarte = "Migrations residentielles vers l'Ile-de-France",
#                               sourceCarte = "Source : INSEE - RP2016",
#                               etiquettes = etiquettes,
#                               colBorder = "transparent",
#                               colBorderMaille = "grey")
#  
#  dev.off()
#  
#  pdf(file = "sortie.pdf",
#      width = 10,
#      height = 10)
#  
#  # affichage de la carte
#  fond_saphirs <- plot_saphirs(data = donnees_biloc_saphirs,
#                               fondMaille = regm,
#                               fondSousAnalyse = fondSousAnalyse,
#                               fondSurAnalyse = fondSurAnalyse,
#                               typeMaille = "REG",
#                               idDataDepart = "REG_DEPART",
#                               idDataArrivee = "REG_ARRIVEE",
#                               varFlux = "MIGR",
#                               direction = "Ent",
#                               titreLeg = "Entrees",
#                               xLeg = 1150000,
#                               yLeg = 6470000,
#                               titreCarte = "Migrations residentielles vers l'Ile-de-France",
#                               sourceCarte = "Source : INSEE - RP2016",
#                               etiquettes = etiquettes,
#                               colBorder = "transparent",
#                               colBorderMaille = "grey")
#  
#  dev.off()

## ----exportQgis, eval = FALSE-------------------------------------------------
#  library(oceanis)
#  library(leaflet)
#  
#  # chargement des donnees
#  donnees_monoloc <- rio::import(file = system.file("data/donnees_monoloc.rda",
#                                                    package = "oceanis"))
#  
#  # import du fond des departements
#  depm <- sf::st_read(dsn = system.file("extdata",
#                                        "dep_francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  # import du fond des regions
#  regm <- sf::st_read(dsn = system.file("extdata",
#                                        "reg_francemetro_2018.shp",
#                                        package = "oceanis"),
#                      quiet = TRUE,
#                      stringsAsFactors = FALSE)
#  
#  # affichage de la carte avec des rayons de ronds plus grands
#  map <- leaflet_ronds_classes(data = donnees_monoloc,
#                               fondMaille = depm[depm$REG == "93",],
#                               fondMailleElargi = depm,
#                               fondSuppl = regm,
#                               idData = "COD_DEP",
#                               varVolume = "POP_2015",
#                               varRatio = "VAR_AN_MOY",
#                               rayonRond = 29000,
#                               rapportRond = NULL)
#  
#  # modification de la position de la legende des ronds et du niveau de zoom
#  map <- add_legende_ronds(map = map,
#                           titre = "Population en 2015",
#                           lng = 8.5,
#                           lat = 45,
#                           zoom = 8)
#  # modification de la position de la legende des classes et du niveau de zoom
#  map <- add_legende_classes(map = map,
#                             titre = "Variation ann.moy. 2010-2015",
#                             lng = 8,
#                             lat = 44.5,
#                             zoom = 8)
#  # ajout d'une source a la carte
#  map <- add_source(map = map,
#                    source = "Source : INSEE - RP2016")
#  # ajout d'un titre a la carte
#  map <- add_titre(map = map,
#                   titre = "Population des departements de la region Provence-Alpes-Cote d'Azur en 2015 et son evolution depuis 2010")
#  # modification de la couleur de bordure des ronds
#  map <- set_bordure_ronds(map = map,
#                           colBorderPos = "grey")
#  # modification du style de la palette
#  map <- set_couleur_classes(map = map,
#                             stylePalette = "Insee_Rouge")
#  # modification de l'opacite de la representation elargie
#  map <- set_opacite_elargi(map,
#                            opacite = 0.6)
#  
#  export_qgis_ronds_classes(map,
#                            cheminDossier = getwd(),
#                            nomFichier = "export_carte_rp_ac",
#                            titre1 = "Population des departements de la region Provence-Alpes-Cote d'Azur en 2015 et son evolution depuis 2010",
#                            titre2 = "",
#                            source = "Source : INSEE - RP2016")

