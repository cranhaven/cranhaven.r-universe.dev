# OzButterflies database

This folder contains the OzButterflies database. It consists of this README.txt file, 2 spreadsheets (summary and specimen, each in 3 formats), reflectance spectra of the red, green and blue colour standards (standard-\<colour\>.ProcSpec files) and specimen data files (photographs, spectra, and cytochrome C oxidase subunit 1 (CO1) sequence).

Specimen data file are stored in folders by family, species and specimen ID.

## Summary spreadsheet

The summary spreadsheet, Oz_butterflies_summary.csv, .xslx or .json, contains a row for each species in the database and the following columns:

| Column     | Description                                                                                     |
|--------------------------|----------------------------------------------|
| Family     | Species family                                                                                  |
| Species    | Binomial species names                                                                          |
| Specimens  | Number of specimens of the species in the database                                              |
| Females    | Number of female specimens of the species in the database                                       |
| Males      | Number of male specimens of the species in the database                                         |
| Dimorphic  | "y" if clear sexual dimorphism in visible or UV, "n" if difficult to ID sexes in visible or UV  |
| Iridescent | "y" if obvious iridescent patches as measured in different angles, "n" if iridescence not clear |

## Specimen spreadsheet

| Column                  | Description                                                                          |
|--------------------------|----------------------------------------------|
| ID                      | Specimen identifier                                                                  |
| Family                  | Specimen family                                                                      |
| Genus                   | Specimen genus                                                                       |
| Species                 | Specimen specific name (i.e. not the binomial species name)                          |
| Sex                     | "Female" or "Male"                                                                   |
| Body.damage             | "y" or "n" if body has visible damage                                                |
| Forewing.dorsal.damage  | "y" or "n" if dorsal forewing has visible damage                                     |
| Forewing.ventral.damage | "y" or "n" if ventral forewing has visible damage                                    |
| Hindwing.dorsal.damage  | "y" or "n" if dorsal hindwing has visible damage                                     |
| Hindwing.ventral.damage | "y" or "n" if ventral hindwing has visible damage                                    |
| Site                    | Site code, see "Site Codes" below                                                    |
| Latitude                | Latitude of site                                                                     |
| Longitude               | Longitude of site                                                                    |
| Climate                 | "Temperate", "Subtropical or"Tropical"                                               |
| Date                    | Date of collection in day/month/year format                                          |
| Collector               | Initials of collector                                                                |
| Binomial                | Full species name                                                                    |
| Pinned                  | "y" if specimen is pinned (i.e. intact), "n" if wings are separated from body        |
| Spectra                 | "y" if colour patches on the specimen were measured by spectrophotometer, "n" if not |
| DNA                     | "y" if specimen has CO1 sequence in the database, "n" if it does not                 |
| Repo.zipname            | Name of zip file in repository that contains specimen data                           |

## Specimen data

Each specimen folder contains photographs of the specimen in Sony raw fomat (file extension .ARW). In the following file names, "\<ID\>" represents the specimen identifier.

Pinned (intact) specimens have photographs named:

| File name        | Description                                             |
|---------------------------------|---------------------------------------|
| \<ID\>-v-RGB.ARW | Ventral wing surfaces photographed in visible light     |
| \<ID\>-d-RGB.ARW | Dorsal wing surfaces photographed in visible light      |
| \<ID\>-v-UV.ARW  | Ventral wing surfaces photographed in ultraviolet light |
| \<ID\>-d-UV.ARW  | Dorsal wing surfaces photographed in ultraviolet light  |

Photographs of non-pinned specimens (wings separated from body):

| File name      | Description                                                        |
|---------------------------------|---------------------------------------|
| \<ID\>-RGB.ARW | Dorsal and ventral wing surfaces photographed in visible light     |
| \<ID\>-UV.ARW  | Dorsal and ventral wing surfaces photographed in ultraviolet light |

Specimens with DNA include the following files:

| File name    | Description                         |
|--------------|-------------------------------------|
| \<ID\>-f.ab1 | CO1 gene gene sequenced forward (f) |

Spectroscoped specimens include the following files:

| File Name                                     | Description                                                                                                                                                                                                                                                                                                                                                |
|--------------------------|----------------------------------------------|
| `<ID>-<side>.jpeg`                            | Reference image indicating the locations where spectra were measured. The `<side>` parameter can be `"d"` (dorsal), `"v"` (ventral), or `NULL` (both sides in the same image).                                                                                                                                                                             |
| `<ID>-<side>-<patch number>-<angle>.procspec` | File containing processed spectral measurements. Parameters: `<ID>` (specimen number), `<side>` (`"d"`, `"v"`, or `"b"` for dorsal, ventral, or both, respectively), `<patch number>` (measured patch, referenced in the corresponding image), and `<angle>` (`"s"` for 45°, `"a"` for 60°, `"a2"` if 60° inclination was not enough to show iridescence). |
| `<ID>.csv`                                    | CSV file containing all processed spectral measurements.                                                                                                                                                                                                                                                                                                   |
| `<ID>.png`                                    | Graph of the spectra measured at 45° and 60° or more.                                                                                                                                                                                                                                                                                                      |

## Site Codes

Sites with standardised collecting times

| Code | Place                         | City     |
|------|-------------------------------|----------|
| BG   | Cairns Botanic Gardens        | Cairns   |
| JCU  | James Cook University campus  | Cairns   |
| GP   | Gamboora Park                 | Cairns   |
| MR   | Mossman River                 | Mossman  |
| BBG  | Mt. Coot-tha Botanic Gardens  | Brisbane |
| CC   | Cabbage Creek                 | Brisbane |
| OC   | Oxley Creek                   | Brisbane |
| LSP  | Lakeside Park                 | Brisbane |
| MQ   | Macquarie University campus   | Sydney   |
| WPP  | West Pymple Park              | Sydney   |
| KRG  | Ku-ring-gai Wildflower Garden | Sydney   |
| WLP  | Westleigh Park                | Sydney   |
| AO   | Allan Small Oval              | Sydney   |
| JB   | Jubes Mountain Bike Track     | Sydney   |
| WLR  | Woo-La-Ra Park                | Sydney   |
| PP   | Parramatta park               | Sydney   |

Non-standardised collecting sites, i.e. haphazard collecting

| Code | Place                | City         |
|------|----------------------|--------------|
| PD   | Port Douglas City    | Port Douglas |
| LG   | Lisgar Gardens       | Sydney       |
| CCF  | Cabbage Creek Forest | Brisbane     |
