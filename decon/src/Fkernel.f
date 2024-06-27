C ===========================================================================
      subroutine FFTSupport(DT,NDT,DLO,DHI,WINDOW,SIG,FT,SMOOTH,NFT)

      implicit integer (I-N) 
      double precision DT(NDT), FT(NFT), SMOOTH(NFT), 
     *     SIG,DLO,DHI,WINDOW
C     This program was modified based on AS 176 and AS 97
C
C     ALGORITHM AS 176  APPL. STATIST. (1982) VOL.31, NO.1
C     Modified using AS R50 (Appl. Statist. (1984))
C
C     Find density estimate by kernel method using Gaussian kernel.
C     The interval on which the estimate is evaluated has end points
C     DLO and DHI.   If ICAL is not zero then it is assumed that the
C     routine has been called before with the same data and end points
C     and that the array FT has not been altered.
C
C     Auxiliary routines called: FORRT & REVRT from AS 97
C
      DATA ZERO/0.0/, HALF/0.5/, ONE/1.0/, SIX/6.0/, THIR2/32.0/
      DATA BIG/30.0/, KFTLO/5/, KFTHI/21/ 
     *PI,TWOPI/3.141592654,6.283185307/
C
C     The constant BIG is set so that exp(-BIG) can be calculated
C     without causing underflow problems and can be considered = 0.
C
C     Initialize and check for valid parameter values.
C
    2 STEP = (DHI - DLO) / FLOAT(NFT)
      AINC = ONE / (NDT * STEP)
      NFT2 = NFT / 2
      HW = WINDOW / STEP
      FAC1 = 2. * (PI * HW / NFT) ** 2
C
C     Discretize the data
C
      DLO1 = DLO - STEP * HALF
      DO 3 J = 1, NFT
    3 FT(J) = ZERO
      DO 4 I = 1, NDT
	WT = (DT(I) - DLO1) / STEP
	JJ = INT(WT)
	IF (JJ .LT. 1 .OR. JJ .GT. NFT) GO TO 4
	WT = WT - JJ
	WINC = WT * AINC
	KK = JJ + 1
	IF (JJ .EQ. NFT) KK = 1
	FT(JJ) = FT(JJ) + AINC - WINC
	FT(KK) = FT(KK) + WINC
    4 CONTINUE
C
C     Transform to find FT.
C
      CALL FORRT(FT, NFT)
C
C     Find transform of density estimate.
C
   10 JHI = SQRT(BIG / FAC1)
      JMAX = MIN(NFT2 - 1, JHI)
      SMOOTH(1) = FT(1)
      RJ = ZERO
      DO 11 J = 1, JMAX
	RJ = RJ + ONE
	RJFAC = RJ * RJ * FAC1
	BC = ONE - RJFAC / (HW * HW * SIX)
        FAC = ((1.0-2.0*RJFAC)**3 * EXP(RJFAC* (SIG/WINDOW)**2)) / BC
        IF (RJFAC>0.5) FAC = 0.0
	J1 = J + 1
	J2 = J1 + NFT2
	SMOOTH(J1) = FAC * FT(J1)
	SMOOTH(J2) = FAC * FT(J2)
   11 CONTINUE
C
C     Cope with underflow by setting tail of transform to zero.
C
      IF (JHI + 1 - NFT2) 21, 23, 20
   20 SMOOTH(NFT2 + 1) = EXP(-FAC1 * FLOAT(NFT2)**2) * FT(NFT2 + 1)
      GO TO 24
   21 J2LO = JHI + 2
      DO 22 J1 = J2LO, NFT2
	J2 = J1 + NFT2
	SMOOTH(J1) = ZERO
	SMOOTH(J2) = ZERO
   22 CONTINUE
   23 SMOOTH(NFT2 + 1) = ZERO
C
C     Invert Fourier transform of SMOOTH to get estimate and eliminate
C     negative density values.
C

   24 CALL REVRT(SMOOTH, NFT)
      DO 25 J = 1, NFT
   25 IF (SMOOTH(J) .LT. ZERO) SMOOTH(J) = ZERO
      END
C == Small Normal errors with Gaussian Kernel =================
      subroutine FFTGauss(DT,NDT,DLO,DHI,WINDOW,SIG,FT,SMOOTH,NFT)

      implicit integer (I-N) 
      double precision DT(NDT), FT(NFT), SMOOTH(NFT), 
     *     SIG,DLO,DHI,WINDOW
C     This program was modified based on AS 176 and AS 97
C
C     ALGORITHM AS 176  APPL. STATIST. (1982) VOL.31, NO.1
C     Modified using AS R50 (Appl. Statist. (1984))
C
C     Find density estimate by kernel method using Gaussian kernel.
C     The interval on which the estimate is evaluated has end points
C     DLO and DHI.   If ICAL is not zero then it is assumed that the
C     routine has been called before with the same data and end points
C     and that the array FT has not been altered.
C
C     Auxiliary routines called: FORRT & REVRT from AS 97
C
      DATA ZERO/0.0/, HALF/0.5/, ONE/1.0/, SIX/6.0/, THIR2/32.0/
      DATA BIG/30.0/, KFTLO/5/, KFTHI/21/ 
     *PI,TWOPI/3.141592654,6.283185307/
C
C     The constant BIG is set so that exp(-BIG) can be calculated
C     without causing underflow problems and can be considered = 0.
C
C     Initialize and check for valid parameter values.
C
    2 STEP = (DHI - DLO) / FLOAT(NFT)
      AINC = ONE / (NDT * STEP)
      NFT2 = NFT / 2
      HW = WINDOW / STEP
      FAC1 = 2. * (PI * HW / NFT) ** 2
C
C     Discretize the data
C
      DLO1 = DLO - STEP * HALF
      DO 3 J = 1, NFT
    3 FT(J) = ZERO
      DO 4 I = 1, NDT
	WT = (DT(I) - DLO1) / STEP
	JJ = INT(WT)
	IF (JJ .LT. 1 .OR. JJ .GT. NFT) GO TO 4
	WT = WT - JJ
	WINC = WT * AINC
	KK = JJ + 1
	IF (JJ .EQ. NFT) KK = 1
	FT(JJ) = FT(JJ) + AINC - WINC
	FT(KK) = FT(KK) + WINC
    4 CONTINUE
C
C     Transform to find FT.
C
      CALL FORRT(FT, NFT)
C
C     Find transform of density estimate.
C
   10 JHI = SQRT(BIG / FAC1)
      JMAX = MIN(NFT2 - 1, JHI)
      SMOOTH(1) = FT(1)
      RJ = ZERO
      DO 11 J = 1, JMAX
	RJ = RJ + ONE
	RJFAC = RJ * RJ * FAC1
	BC = ONE - RJFAC / (HW * HW * SIX)
	FAC = EXP(-RJFAC*(1.0-(SIG/WINDOW)**2)) / BC
	J1 = J + 1
	J2 = J1 + NFT2
	SMOOTH(J1) = FAC * FT(J1)
	SMOOTH(J2) = FAC * FT(J2)
   11 CONTINUE
C
C     Cope with underflow by setting tail of transform to zero.
C
      IF (JHI + 1 - NFT2) 21, 23, 20
   20 SMOOTH(NFT2 + 1) = EXP(-FAC1 * FLOAT(NFT2)**2) * FT(NFT2 + 1)
      GO TO 24
   21 J2LO = JHI + 2
      DO 22 J1 = J2LO, NFT2
	J2 = J1 + NFT2
	SMOOTH(J1) = ZERO
	SMOOTH(J2) = ZERO
   22 CONTINUE
   23 SMOOTH(NFT2 + 1) = ZERO
C
C     Invert Fourier transform of SMOOTH to get estimate and eliminate
C     negative density values.
C

   24 CALL REVRT(SMOOTH, NFT)
      DO 25 J = 1, NFT
   25 IF (SMOOTH(J) .LT. ZERO) SMOOTH(J) = ZERO
      END

C ===========================================================================
      subroutine FFTLaplace(DT,NDT,DLO,DHI,WINDOW,
     *     SIG,FT,SMOOTH,NFT)

      implicit integer (I-N) 
      double precision DT(NDT), FT(NFT), SMOOTH(NFT), 
     *     SIG,DLO,DHI,WINDOW
C     This program was modified based on AS 176 and AS 97
C
C     ALGORITHM AS 176  APPL. STATIST. (1982) VOL.31, NO.1
C     Modified using AS R50 (Appl. Statist. (1984))
C
C     Find density estimate by kernel method using Gaussian kernel.
C     The interval on which the estimate is evaluated has end points
C     DLO and DHI.   If ICAL is not zero then it is assumed that the
C     routine has been called before with the same data and end points
C     and that the array FT has not been altered.
C
C     Auxiliary routines called: FORRT & REVRT from AS 97
C
      DATA ZERO/0.0/, HALF/0.5/, ONE/1.0/, SIX/6.0/, THIR2/32.0/
      DATA BIG/30.0/, KFTLO/5/, KFTHI/21/ 
     *PI,TWOPI/3.141592654,6.283185307/
C
C     The constant BIG is set so that exp(-BIG) can be calculated
C     without causing underflow problems and can be considered = 0.
C
C     Initialize and check for valid parameter values.
C
    2 STEP = (DHI - DLO) / FLOAT(NFT)
      AINC = ONE / (NDT * STEP)
      NFT2 = NFT / 2
      HW = WINDOW / STEP
      FAC1 = 2. * (PI * HW / NFT) ** 2
C
C     Discretize the data
C
      DLO1 = DLO - STEP * HALF
      DO 3 J = 1, NFT
    3 FT(J) = ZERO
      DO 4 I = 1, NDT
	WT = (DT(I) - DLO1) / STEP
	JJ = INT(WT)
	IF (JJ .LT. 1 .OR. JJ .GT. NFT) GO TO 4
	WT = WT - JJ
	WINC = WT * AINC
	KK = JJ + 1
	IF (JJ .EQ. NFT) KK = 1
	FT(JJ) = FT(JJ) + AINC - WINC
	FT(KK) = FT(KK) + WINC
    4 CONTINUE
C
C     Transform to find FT.
C
      CALL FORRT(FT, NFT)
C
C     Find transform of density estimate.
C
   10 JHI = SQRT(BIG / FAC1)
      JMAX = MIN(NFT2 - 1, JHI)
      SMOOTH(1) = FT(1)
      RJ = ZERO
      DO 11 J = 1, JMAX
	RJ = RJ + ONE
	RJFAC = RJ * RJ * FAC1
	BC = ONE - RJFAC / (HW * HW * SIX)
	FAC = EXP(-RJFAC)*(1.0+(SIG/WINDOW)**2*RJFAC*2.0)/ BC
	J1 = J + 1
	J2 = J1 + NFT2
	SMOOTH(J1) = FAC * FT(J1)
	SMOOTH(J2) = FAC * FT(J2)
   11 CONTINUE
C
C     Cope with underflow by setting tail of transform to zero.
C
      IF (JHI + 1 - NFT2) 21, 23, 20
   20 SMOOTH(NFT2 + 1) = EXP(-FAC1 * FLOAT(NFT2)**2) * FT(NFT2 + 1)
      GO TO 24
   21 J2LO = JHI + 2
      DO 22 J1 = J2LO, NFT2
	J2 = J1 + NFT2
	SMOOTH(J1) = ZERO
	SMOOTH(J2) = ZERO
   22 CONTINUE
   23 SMOOTH(NFT2 + 1) = ZERO
C
C     Invert Fourier transform of SMOOTH to get estimate and eliminate
C     negative density values.
C

   24 CALL REVRT(SMOOTH, NFT)
      DO 25 J = 1, NFT
   25 IF (SMOOTH(J) .LT. ZERO) SMOOTH(J) = ZERO
      END



      SUBROUTINE FORRT(X, M)
C
C     ALGORITHM AS 97  APPL. STATIST. (1976) VOL.25, NO. 2
C
C     Forward discrete Fourier transform in one dimension of real
C     data using complex transform subroutine FASTG.
C
C     X = array of real input data, type real, dimension M.
C     M = length of the transform, must be a power of 2.
C     The minimum length is 8, maximum 2**21.
C
C     The result is placed in X as described in the text of the paper.
C
C     Auxiliary routines required: SCRAG (or SCRAM) & FASTG from AS 83,
C     but with SCRAG modified as described on page 168 of the paper for
C     this algorithm.
C
      implicit integer (I-N) 
      double precision X(M)
      DATA ZERO/0.0/, QUART/0.25/, HALF/0.5/, ONE/1.0/, ONE5/1.5/,
     *	TWO/2.0/, FOUR/4.0/, PI,TWOPI/3.141592654,6.283185307/
C
C     Check for valid transform size.
C
      II = 8
      DO 2 K = 3, 21
	IPOW = K
	IF (II .EQ. M) GO TO 3
	II = II * 2
    2 CONTINUE
C
C     If this point is reached, an illegal size was specified.
C
      RETURN
    3 PIE = PI
C
C     Separate odd and even parts into two halves.
C     First bit reverse the whole array of length M.
C
      CALL SCRAG(X, M, IPOW)
C
C     Next bit reverse the half arrays separately.
C
      N = M / 2
      JPOW = IPOW - 1
      CALL SCRAG(X, N, JPOW)
      CALL SCRAG(X(N+1), N, JPOW)
C
C     Faster alternative to the two lines above to SCRAM.
C     	CALL SCRAM(X, X(N+1), N, JPOW)
C
C     Now do the transform.
C
      CALL FASTG(X, X(N+1), N, 1)
C
C     Unscramble the transform results.
C
      CALL SCRAG(X, N, JPOW)
      CALL SCRAG(X(N+1), N, JPOW)
C
C     Faster alternative to the two lines above to SCRAM.
C     	CALL SCRAM(X, X(N+1), N, JPOW)
C
      NN = N / 2
C
C     Now unravel the result; first the special cases.
C
      Z = HALF * (X(1) + X(N+1))
      X(N+1) = HALF * (X(1) - X(N+1))
      X(1) = Z
      NN1 = NN + 1
      NN2 = NN1 + N
      X(NN1) = HALF * X(NN1)
      X(NN2) = -HALF * X(NN2)
      Z = PIE / N
      BCOS = -TWO * (SIN(Z / TWO) **2)
      BSIN = SIN(Z)
      UN = ONE
      VN = ZERO
      DO 4 K = 2, NN
	Z = UN * BCOS + VN * BSIN + UN
	VN = VN * BCOS - UN * BSIN + VN
	SAVE1 = ONE5 - HALF * (Z * Z + VN * VN)
	UN = Z * SAVE1
	VN = VN * SAVE1
	KI = N + K
	L = N + 2 - K
	LI = N + L
	AN = QUART * (X(K) + X(L))
	BN = QUART * (X(KI) - X(LI))
	CN = QUART * (X(KI) + X(LI))
	DN = QUART * (X(L) - X(K))
	XN = UN * CN - VN * DN
	YN = UN * DN + VN * CN
	X(K) = AN + XN
	X(KI) = BN + YN
	X(L) = AN - XN
	X(LI) = YN - BN
    4 CONTINUE
      RETURN
      END
C

      SUBROUTINE REVRT(X, M)
C
C     ALGORITHM AS 97.1  APPL. STATIST. (1976) VOL.25, NO. 2
C
C     Inverse discrete Fourier transform in one dimension of real
C     data using complex transform subroutine FASTG.
C
C     X = array of Fourier components as output from subroutine FORRT,
C         type real, dimension M.	
C     M = length of the inverse transform, must be a power of 2.
C     The minimum length is 8, maximum 2**21.
C
C     Auxiliary routines required: SCRAG & FASTG from AS 83, but
C     with SCRAG modified as described on page 168 of the paper for
C     this algorithm.
C
      implicit integer (I-N) 
      double precision X(M)
      DATA ZERO/0.0/, HALF/0.5/, ONE/1.0/, ONE5/1.5/,
     *	TWO/2.0/, FOUR/4.0/, PI,TWOPI/3.141592654,6.283185307/
C
C     Check for valid transform size.
C
      II = 8
      DO 2 K = 3, 21
	IPOW = K
	IF (II .EQ. M) GO TO 3
	II = II * 2
    2 CONTINUE
C
C     If this point is reached, an illegal size was specified.
C
      RETURN
    3 PIE = PI
      N = M / 2
      NN = N / 2
C
C     Undo the spectrum into that of two interleaved series.
C     First, the special cases.
C
      Z = X(1) + X(N+1)
      X(N+1) = X(1) - X(N+1)
      X(1) = Z
      NN1 = NN + 1
      NN2 = NN1 + N
      X(NN1) = TWO * X(NN1)
      X(NN2) = -TWO * X(NN2)
      Z = PIE / N
      BCOS = -TWO * (SIN(Z / TWO) **2)
      BSIN = SIN(Z)
      UN = ONE
      VN = ZERO
      DO 4 K = 2, NN
	Z = UN * BCOS + VN * BSIN + UN
	VN = VN * BCOS - UN * BSIN + VN
	SAVE1 = ONE5 - HALF * (Z * Z + VN * VN)
	UN = Z * SAVE1
	VN = VN * SAVE1
	KI = N + K
	L = N + 2 - K
	LI = N + L
	AN = X(K) + X(L)
	BN = X(KI) - X(LI)
	PN = X(K) - X(L)
	QN = X(KI) + X(LI)
	CN = UN * PN + VN * QN
	DN = UN * QN - VN * PN
	X(K) = AN - DN
	X(KI) = BN + CN
	X(L) = AN + DN
	X(LI) = CN - BN
    4 CONTINUE
C
C     Now do the inverse transform
C
      CALL FASTG(X, X(N+1), N, -1)
C
C     Now undo the order - the half arrays are already bit reversed;
C     bit reverse the whole array.
C
      CALL SCRAG(X, M, IPOW)
      RETURN
      END
C
	subroutine fastg(xreal, ximag, n, itype)
c
c       Algorithm AS 83.2 Appl. Statist. (1975) vol.24, no.1
c
c       Radix 4 complex discrete fast Fourier transform without
c       unscrambling, suitable for convolutions or other applications
c       which do not require unscrambling.   Called by subroutine
c       FASTF which also does the unscrambling.
c
      implicit integer (i-n) 
      double precision xreal(n), ximag(n)
      data    zero, half, one, one5, two, four
     +          /0.0,  0.5, 1.0,  1.5, 2.0,  4.0/
     *PI,TWOPI/3.141592654,6.283185307/
	ifaca = n / 4
	if (itype .eq. 0) return
	if (itype .gt. 0) go to 5
c
c       ITYPE < 0 indicates inverse transform required.
c       Calculate conjugate.
c
	do 4 k = 1, n
    4   ximag(k) = -ximag(k)
c
c       Following code is executed for IFACA = N/4, N/16, N/64, ...
c       until IFACA <= 1.
c
    5   ifcab = ifaca * 4
	z = PI / ifcab
	bcos = -two * sin(z)**2
	bsin = sin(two * z)
	cw1 = one
	sw1 = zero
	do 10 litla = 1, ifaca
	  do 8 i0 = litla, n, ifcab
	    i1 = i0 + ifaca
	    i2 = i1 + ifaca
	    i3 = i2 + ifaca
	    xs0 = xreal(i0) + xreal(i2)
	    xs1 = xreal(i0) - xreal(i2)
	    ys0 = ximag(i0) + ximag(i2)
	    ys1 = ximag(i0) - ximag(i2)
	    xs2 = xreal(i1) + xreal(i3)
	    xs3 = xreal(i1) - xreal(i3)
	    ys2 = ximag(i1) + ximag(i3)
	    ys3 = ximag(i1) - ximag(i3)
	    xreal(i0) = xs0 + xs2
	    ximag(i0) = ys0 + ys2
	    x1 = xs1 + ys3
	    y1 = ys1 - xs3
	    x2 = xs0 - xs2
	    y2 = ys0 - ys2
	    x3 = xs1 - ys3
	    y3 = ys1 + xs3
	    if (litla .eq. 1) then
	      xreal(i2) = x1
	      ximag(i2) = y1
	      xreal(i1) = x2
	      ximag(i1) = y2
	      xreal(i3) = x3
	      ximag(i3) = y3
	    else
	      xreal(i2) = x1 * cw1 + y1 * sw1
	      ximag(i2) = y1 * cw1 - x1 * sw1
	      xreal(i1) = x2 * cw2 + y2 * sw2
	      ximag(i1) = y2 * cw2 - x2 * sw2
	      xreal(i3) = x3 * cw3 + y3 * sw3
	      ximag(i3) = y3 * cw3 - x3 * sw3
	    end if
    8     continue
c
c       Calculate a new set of twiddle factors.
c
	  if (litla .lt. ifaca) then
	    z = cw1 * bcos - sw1 * bsin + cw1
	    sw1 = bcos * sw1 + bsin * cw1 + sw1
	    tempr = one5 - half * (z * z + sw1 * sw1)
	    cw1 = z * tempr
	    sw1 = sw1 * tempr
	    cw2 = cw1 * cw1 - sw1 * sw1
	    sw2 = two * cw1 * sw1
	    cw3 = cw1 * cw2 - sw1 * sw2
	    sw3 = cw1 * sw2 + cw2 * sw1
	  end if
   10   continue
	if (ifaca .le. 1) go to 14
c
c       Set up the transform split for the next stage.
c
	ifaca = ifaca / 4
	if (ifaca .gt. 0) go to 5
c
c       Radix 2 calculation, if needed.
c
	if (ifaca .lt. 0) return
	do 13 k = 1, n, 2
	  tempr = xreal(k) + xreal(k+1)
	  xreal(k+1) = xreal(k) - xreal(k+1)
	  xreal(k) = tempr
	  tempr = ximag(k) + ximag(k+1)
	  ximag(k+1) = ximag(k) - ximag(k+1)
	  ximag(k) = tempr
   13   continue
   14   if (itype .lt. 0) then
c
c       Inverse transform; conjugate the result.
c
	  do 16 k = 1, n
   16     ximag(k) = -ximag(k)
	  return
	end if
c
c       Forward transform
c
	z = one / n
	do 18 k = 1, n
	  xreal(k) = xreal(k) * z
	  ximag(k) = ximag(k) * z
   18   continue
c
	return
	end
c
c
c
	subroutine scrag(xreal, n, ipow)
c
c       Algorithm AS 83.3 Appl. Statist. (1975) vol.24, no.1
c ***   MODIFIED FOR USE WITH AS 97 ***
c
c       Subroutine for unscrambling FFT data.
c

      implicit integer (i-n) 
      double precision xreal(n)
      DIMENSION     l(19)
      equivalence (l1,l(1)), (l2,l(2)), (l3,l(3)), (l4,l(4)),
     +          (l5,l(5)), (l6,l(6)), (l7,l(7)), (l8,l(8)), (l9,l(9)),
     +          (l10,l(10)), (l11,l(11)), (l12,l(12)), (l13,l(13)),
     +          (l14,l(14)), (l15,l(15)), (l16,l(16)), (l17,l(17)),
     +          (l18,l(18)), (l19,l(19))
c
	ii = 1
	itop = 2 ** (ipow - 1)
	i = 20 - ipow
	do 5 k = 1, i
    5   l(k) = ii
	l0 = ii
	i = i + 1
	do 6 k = i, 19
	  ii = ii * 2
	  l(k) = ii
    6   continue
c
	ii = 0
	do 9 j1 = 1, l1, l0
	  do 9 j2 = j1, l2, l1
	    do 9 j3 = j2, l3, l2
	      do 9 j4 = j3, l4, l3
		do 9 j5 = j4, l5, l4
		  do 9 j6 = j5, l6, l5
		    do 9 j7 = j6, l7, l6
		      do 9 j8 = j7, l8, l7
			do 9 j9 = j8, l9, l8
			  do 9 j10 = j9, l10, l9
			    do 9 j11 = j10, l11, l10
			      do 9 j12 = j11, l12, l11
				do 9 j13 = j12, l13, l12
				  do 9 j14 = j13, l14, l13
				    do 9 j15 = j14, l15, l14
				      do 9 j16 = j15, l16, l15
					do 9 j17 = j16, l17, l16
					  do 9 j18 = j17, l18, l17
					    do 9 j19 = j18, l19, l18
					      j20 = j19
					      do 9 i = 1, 2
						ii = ii + 1
						if (ii .lt. j20) then
c
c       J20 is the bit-reverse of II pairwise interchange.
c
						  tempr = xreal(ii)
						  xreal(ii) = xreal(j20)
						  xreal(j20) = tempr
						end if
						j20 = j20 + itop
    9   continue
c
	return
	end
 
