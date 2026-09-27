#$Author: sinnwell $
#$Date: 2003/12/08 20:22:37 $
#$Header: /projects/genetics/cvs/cvsroot/haplo.stats/R/score.sim.control.q,v 1.3 2003/12/08 20:22:37 sinnwell Exp $
#$Locker:  $
#$Log: score.sim.control.q,v $
#Revision 1.3  2003/12/08 20:22:37  sinnwell
# changed T,F to TRUE,FALSE
#
#Revision 1.2  2003/09/12 16:33:01  sinnwell
#remove pval.choice
#
#Revision 1.1  2003/08/22 21:08:42  sinnwell
#Initial revision
#
# License: 
# 
# Copyright 2003 Mayo Foundation for Medical Education and Research. 
# 
# This program is free software; you can redistribute it and/or modify it under the terms of 
# the GNU General Public License as published by the Free Software Foundation; either 
# version 2 of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or 
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
# more details.
# 
# You should have received a copy of the GNU General Public License along with this 
# program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
# Boston, MA 02111-1307 USA
# 
# For other licensing arrangements, please contact Daniel J. Schaid.
# 
# Daniel J. Schaid, Ph.D.
# Division of Biostatistics
# Harwick Building Room 775
# Mayo Clinic
# 200 First St., SW
# Rochester, MN 55905
# 
# phone: 507-284-0639
# fax:      507-284-9542
# email: schaid@mayo.edu
# 

score.sim.control <- function(p.threshold = 0.25, min.sim = 1000, max.sim = 20000.,
                              verbose = FALSE)
# Sinnwell JP, Schaid DJ 7/2003
# Mayo Clinic Rochester
{
  
  # return a list of controls for simulations in haplo.score
  
	if(p.threshold < 0 | p.threshold > 0.9) {
		warning("The value of p.threshold is out of range, the devault value of 0.25 is used"
			)
		p.threshold <- 0.25
	}
#	if(is.na(pmatch(pval.choice, c("global", "max")))) {
#		warning("The value of p.val.choice should be 'global' or 'max'; the default value 'global' is used"
#			)
#		pval.choice <- "global"
#	}
	if(min.sim < 1) {
		warning("The value of min.sim is out of range, the default value of 1000 is used"
			)
		min.sim <- 1000
	}
	if(max.sim < min.sim | max.sim > 100000000.) {
		warning("The value of max.sim is not valid! \n\n            If less than min.sim, it is set equal to min.sim. \n\n            If over 100,000,000 set to default of 20,000"
			)
		if(max.sim < min.sim)
			max.sim <- min.sim
		else max.sim <- 20000.
	}
	return(list(p.threshold = p.threshold, min.sim = min.sim,
                    max.sim = max.sim, verbose = verbose))
}
