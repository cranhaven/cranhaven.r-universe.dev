library('glue')

# simulate genotype data in plink

extDir = 'inst/extdata'

npop = 50
nvar = 10
alleleFreq = 0.1
plinkSimPath = file.path(extDir, 'wgas.sim')
plinkOut = file.path(extDir, 'geno_sample')

plinkSim = glue('{nvar} snp {alleleFreq} {alleleFreq} 1 1')
writeLines(plinkSim, plinkSimPath)
plinkCmd = glue(
  '--simulate {plinkSimPath} --make-bed --out {plinkOut} ',
  '--simulate-ncases {npop/2} --simulate-ncontrols {npop/2} --seed 1')
system(glue('~/plink_mac_20220402/plink {plinkCmd}'))
