s1 = Sys.time()
library(MRG)
# Neccessary to silence sf startup messages
suppressMessages(library(sf))
library(giscoR)
#'
# These are SYNTHETIC agricultural FSS data 
data(ifs_dk) # Census data
ifs_weight = ifs_dk %>% dplyr::filter(Sample == 1) # Extract weighted subsample

# Create spatial data
ifg = fssgeo(ifs_dk, locAdj = "LL")
fsg = fssgeo(ifs_weight, locAdj = "LL")
# Read nuts borders, used for extracting smaller data set 
borders = gisco_get_nuts(nuts_level = 2)

dkb = borders[borders$CNTR_CODE == "DK",] %>% st_transform(crs = 3035)
ifg$dkb = st_join(ifg, dkb)$NUTS_ID
ifg = ifg[!is.na(ifg$dkb) & ifg$dkb == "DK01",]
fsg$dkb = st_join(fsg, dkb)$NUTS_ID
fsg = fsg[!is.na(fsg$dkb) & fsg$dkb == "DK01",]

ifg$ft = as.numeric(substr(ifg$FARMTYPE, 3, 4))^2

s2 = Sys.time()
#'
# Set the base resolutions, and create a hierarchical list with gridded data
ress = c(1,5,10,20,40)*1000
# Gridding Utilized agricultural area (UAA), organic UAA and ft together
ifl = gridData(ifg, c("UAA", "UAAXK0000_ORG", "ft"), res = ress)

# Gridding the UAA from the survey - the survey weights are in the column EXT_MODULE
fsl = gridData(fsg,  vars = c("UAA"), weights = "EXT_MODULE",  res = ress)

# Create a multi-resolution grid only with farm number as confidentiality rule, then plot results
himg0 = multiResGrid(ifl, checkReliability = FALSE, suppresslim = 0)

# Create a multi-resolution grid of UAA, also based on the dominance rule (default)
himg1 = multiResGrid(ifl, vars = "UAA", ifg = ifg)

# Create joint multi-resolution grid of organic UAA
himg2 = multiResGrid(ifl, vars = "UAAXK0000_ORG", ifg = ifg, 
                     checkReliability = FALSE, suppresslim = 0)

# Create joint multi-resolution grid of organic UAA and total UAA
himg3 = multiResGrid(ifl, vars = c("UAA", "UAAXK0000_ORG"), ifg = ifg, 
                  checkReliability = FALSE, suppresslim = 0)


# Create joint multi-resolution grid of organic UAA and total UAA, with suppression
himg4 = multiResGrid(ifl, vars = c("UAA", "UAAXK0000_ORG"), ifg = ifg, 
                     checkReliability = FALSE, suppresslim = 0.1)

# Create joint multi-resolution grid of ft
himg5 = multiResGrid(ifl, vars = c("ft"), ifg = ifg, 
                     checkReliability = FALSE, suppresslim = 0.1)

himg6 = multiResGrid(ifl, vars = c("UAA", "UAAXK0000_ORG", "ft"), ifg = ifg, 
                     checkReliability = FALSE, suppresslim = 0.1)

himg7 = MRGmerge(himg1, himg2, himg3 = himg5)

s3 = Sys.time()


# Create multi-resolution grid of UAA and organic UAA, based on survey data,
# also applying reliability check
himg5 <-  multiResGrid(fsl, vars = c("UAA"), weights = "EXT_MODULE", ifg = fsg, 
                      strat = "STRA_ID_CORE", checkReliability = TRUE, reliabilitySplit = 5)
summary(himg0, digits = 5)
summary(himg1, digits = 5)
# To avoid FAQ 7.31 problem in summary - mean(himg3$UAA) = 19182.5 is not consistently rounded to 19182 or 19183 
himg3$UAA = himg3$UAA + 0.001
summary(himg3, digits = 5)
himg4$UAA = himg4$UAA + 0.001
summary(himg4, digits = 5)
summary(himg5, digits = 5)
s4 = Sys.time()

MRGobject = createMRGobject(ifg = ifg, ress = ress, var = "UAA")
himg1 = multiResGrid(MRGobject)
# Parameters can be updated in the object or in the call to multiResGrid
MRGobject$suppresslim = 0.02
himg2 = multiResGrid(MRGobject)
himg3 = multiResGrid(MRGobject, suppresslim = 0.05)
summary(himg1,digits = 5)
summary(himg2, digits = 5)
summary(himg3, digits = 5)

s5 = Sys.time()
## IGNORE_RDIFF_BEGIN
s5-s4
s4-s3
s3-s2
s2-s1

s5-s1
s4-s1
s3-s1
s2-s1
## IGNORE_RDIFF_END
