##$Author: sinnwell $
##$Date: 2011/12/05 20:53:27 $
##$Header: /projects/genetics/cvs/cvsroot/haplo.stats/test/test.seqhap.R,v 1.1 2011/12/05 20:53:27 sinnwell Exp $
##$Locker:  $
##$Log: test.seqhap.R,v $
##Revision 1.1  2011/12/05 20:53:27  sinnwell
##changed from .q to .R, to work with R check
##
##Revision 1.1  2011/04/28 20:08:12  sinnwell
##new makefile, pulls R/man files from mgenet, rlocal

## package: haplo.stats
## test script: seqhap

## settings
verbose=TRUE
require(haplo.stats)
Sys.setlocale("LC_ALL", "C")
Sys.getlocale()


if(verbose) cat("testing with seqhap example dataset \n")

  data(seqhap.dat)
  mydata.y <- seqhap.dat[,1]
  mydata.x <- seqhap.dat[,-1]
  # load positions
  data(seqhap.pos)
  pos=seqhap.pos$pos
  # run seqhap with default settings

  seed <- c(45, 16, 22, 24, 15,  3, 11, 47, 24, 40, 18,  0)
  set.seed(seed)
  
  myobj.default <- seqhap(y=mydata.y, geno=mydata.x, pos=pos)
  
  set.seed(seed)
  myobj.mh25 <- seqhap(y=mydata.y, geno=mydata.x, pos=pos,
                   mh.threshold=2.5, haplo.freq.min=.01)
  
  set.seed(seed)
  myobj.perm1K <- seqhap(y=mydata.y, geno=mydata.x, pos=pos,
                   sim.control=score.sim.control(min.sim=1000, max.sim=1000))
  


  print(myobj.default)
   
  print(myobj.mh25)

  print(myobj.perm1K)

