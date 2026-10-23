OzButterflies database
----------------------

This folder contains the OzButterflies database. It consists of this
README.txt file, 2 spreadsheets (each in 3 formats), reflectance spectra 
of the red, green and blue colour standards (standard-<colour>.ProcSpec) 
and specimen data files. 

Specimen data file are stored in folders by family, species and
specimen ID.


Summary spreadsheet
-------------------

The summary spreadsheet, Oz_butterflies_summary.csv, .xslx or .json,
contains a row for each species in the database and the following
columns:

Family          Species family
Species         Binomial species names
Specimens       Number of specimens of the species in the database
Females         Number of female specimens of the species in the database
Males           Number of male specimens of the species in the database
Dimorphic       "y" if clear sexual dimorphism in visible or UV, "n" if
                difficult to ID sexes in visible or UV
Iridescent      "y" if obvious iridescent patches as measured in different
                angles, "n" if iridescence not clear

Specimen spreadsheet
--------------------

ID              Specimen identifier
Family          Specimen family
Genus           Specimen genus
Species         Specimen specific name (i.e. not the binomial species name)
Sex             "Female" or "Male"
Body.damage     "y" or "n" if body has visible damage
Forewing.dorsal.damage   "y" or "n" if dorsal forewing has visible damage
Forewing.ventral.damage  "y" or "n" if ventral forewing has visible damage
Hindwing.dorsal.damage   "y" or "n" if dorsal hindwing has visible damage
Hindwing.ventral.damage  "y" or "n" if ventral hindwing has visible damage
Site            Site code, see "Site Codes" below
Latitude        Latitude of site
Longitude       Longitude of site
Climate         "Temperate", "Subtropical or "Tropical"
Date            Date of collection in day/month/year format
Collector       Initials of collector
Binomial        Full species name
Pinned          "y" if specimen is pinned (i.e. intact), "n" if wings are separated from body
Spectra         "y" if colour patches on the specimen were measured by spectrophotometer, "n" if not
DNA             "y" if specimen has CO1 sequence in the database, "n" if it does not

Specimen data
-------------

Each specimen folder contains photographs of the specimen in Sony raw
fomat (file extension .ARW).  In the following file names, "<ID>"
represents the specimen ID.

Pinned (intact) specimens have photographs named:
<ID>-v-RGB.ARW   Ventral wing surfaces photographed in visible light
<ID>-d-RGB.ARW   Dorsal wing surfaces photographed in visible light
<ID>-v-UV.ARW    Ventral wing surfaces photographed in ultraviolet light
<ID>-d-UV.ARW    Dorsal wing surfaces photographed in ultraviolet light

Photographs of non-pinned specimens (wings separated from body):
<ID>-RGB.ARW   Dorsal and ventral wing surfaces photographed in visible light
<ID>-UV.ARW    Dorsal and ventral wing surfaces photographed in ultraviolet light

Specimens with DNA include the following files:
<ID>-f.gb       CO1 gene gene sequenced forward

Spectroscoped specimens include the following files:

<ID>-<side>.jpeg Reference image for speced patch numbers, i.e. it
                 identifies the locations where spectra were
                 measured. Side can be "d" (dorsal), "v" (ventral) or
                 NULL for both sides in the same picture
<ID>-<side>-<patch number>-<angle>.procspec where:
                 <ID> is the specimen number;
                 <side> is either "d" (dorsal), "v" (ventral), or "b"
                 (both) and identifies the reference image used to
                 locate the measured patch;
                 <patch number> identifies the measured patch which can
                 be located using the appropriate reference image;
                 <angle> is either "s" for standard 45 degrees or "a",
                 60 degrees or "a2" if the 60 degrees inclination was
                 not enough to show iridescence
<ID>.csv         CSV files containing all procspec measures
<ID>.png       Graph of the spectra measured at 45 degrees and 60 degrees or more

Site Codes
----------

Code    Place                                   City
BG      Cairns Botanic Gardens                  Cairns
JCU     James Cook University campus            Cairns
GP      Gamboora Park                           Cairns
MR      Mossman River (Foxton Avenue Reserve)   Mossman
BBG     Mt. Coot-tha Botanic Gardens            Brisbane
CC      Cabbage Creek                           Brisbane
OC      Oxley Creek                             Brisbane
LSP     Lakeside Park                           Brisbane
MQ      Macquarie University campus             Sydney
WPP     West Pymple Park                        Sydney
KRG     Ku-ring-gai Wildflower Garden           Sydney
WLP     Westleigh Park                          Sydney
AO      Allan Small Oval                        Sydney
JB      Jubes Mountain Bike Track               Sydney
WLR     Woo-La-Ra Park                          Sydney
PP      Parramatta park                         Sydney

Non-standardised collecting sites, i.e. haphazard collecting
PD   Port Douglas City                          Port Douglas
LG   Lisgar Gardens                             Sydney
CCF  Cabbage Creek Forest                       Brisbane
