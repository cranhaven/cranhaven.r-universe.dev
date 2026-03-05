c
c  event : A Library of Special Functions for Event Histories
c  Copyright (C) 1998 J.K. Lindsey
c
c  This program is free software; you can redistribute it and/or modify
c  it under the terms of the GNU General Public License as published by
c  the Free Software Foundation; either version 2 of the License, or
c  (at your option) any later version.
c
c  This program is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c  GNU General Public License for more details.
c
c  You should have received a copy of the GNU General Public License
c  along with this program; if not, write to the Free Software
c  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
c
c  DESCRIPTION
c
c    Header file for Survival Kit
c
c
c***********************************************************************
c   Definition of parameters to used in the various programs of the    *
c                            SURVIVAL KIT                              *
c***********************************************************************
      IMPLICIT NONE
c
c
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c  PARAMETERS THAT MAY BE MODIFIED BY THE USER
c  Remember: when you change any of the parameters, you have to recompile
c  your programs.
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c
c
c Parameters related to the size of your problem
c ==============================================
c
      INTEGER MXSTRA,NTIMMAX,NSTIMAX
      INTEGER NRECMAX,NRECMAX2,MXEF,MXEF_USED,NDIMAX
c
c-- NRECMAX = maximum number of elementary records
c--    (Caution: with time-dependent variables this may be much larger 
c--    than the number of records in your original data file!!)
c
      PARAMETER(NRECMAX=20000)
c
c-- NRECMAX2 = maximum number of elementary records in the 'future'
c--    data file (FUTURE statement in PREPARE).
c--    These will normally be much fewer than in the data file
c
      PARAMETER(NRECMAX2=2000)
c
c-- MXEF = max. number of covariates. These are discrete
c--    (CLASS) covariates and continuous covariates.
c--    Time dependent covariates have to be counted twice (because
c--    the states before and after change occupy two positions on
c--    the recoded data file) !!
c
      PARAMETER(MXEF=30)
c
c-- MXEF_USED = max. number of covariates actually used in the
c--    MODEL statement of COX or WEIBULL.
c--    There may be situations where you create recoded files with
c--    very many covariates with PREPARE and never use all of them
c--    together in COX or WEIBULL. In this case it may make sense
c--    to have MXFE_USED smaller that MXEF (it must not be larger!)
c--    to save core memory space.
c
      PARAMETER(MXEF_USED=30)
c

c-- MXSTRA= max. number of strata, i.e. levels of the strata variable
c--    defined in the STRATA statement.
c
      PARAMETER(MXSTRA=25)
c
c-- NDIMAX = max. total number of levels of effects in the model
c--    (size of the vector of solutions)
c
       PARAMETER(NDIMAX=100)
c
c-- NTIMMAX = largest possible value of the (dependent) time variable
c--    this value is necessary as an upper limit for an efficient
c--    comutation of log(time) and exp(time) and when calculating
c--    functional values of the survivor curve (SURVIVAL statement
c--    of COX and WEIBULL)
c
      PARAMETER(NTIMMAX=6000)
c
c-- NSTIMAX = maximum number of distinct times or quantiles that
c--    can be defined in the SURVIVAL statement options SPECIFIC
c--    and QUANTILE.
c
      PARAMETER(NSTIMAX=20)
c
c Parameters used in the lbfgs maximisationn routine
c ==================================================
c
      INTEGER MVEC_BF

c-- MVEC_BF = number of vectors used to store the approximation of the Hessian
c--    (rank of approx. Hessian = MVEC_BF). Values between 3 and 20 might be
c--    used).
c
      PARAMETER(MVEC_BF= 15)
c
c Parameters used in the numerical integration to get moments of the
c marginal posterior of a random effect variance parameter
c (when the MOMENTS option in the RANDOM statement is used)
c===================================================================
c
      INTEGER NPGAUSS,NITER_GAUSS
c
c-- NPGAUSS = number of points used during Gauss-Hermite integration
c      (has to be between 3 and 5)
c
      PARAMETER(NPGAUSS=5)
c
c-- NITER_GAUSS = number of iterations in the Gauss-Hermite integration
c     process
C
      PARAMETER(NITER_GAUSS=10)
