* The file has been altered by Benjamin Christoffersen to soly include the
* the bivariate normal distribution and only in the extreme case is left.
*
      DOUBLE PRECISION FUNCTION MVBVU( SH, SK, R )
*
*     A function for computing bivariate normal probabilities;
*       developed using
*         Drezner, Z. and Wesolowsky, G. O. (1989),
*         On the Computation of the Bivariate Normal Integral,
*         J. Stat. Comput. Simul.. 35 pp. 101-107.
*       with extensive modications for double precisions by
*         Alan Genz and Yihong Ge
*         Department of Mathematics
*         Washington State University
*         Pullman, WA 99164-3113
*         Email : alangenz@wsu.edu
*
* BVN - calculate the probability that X is larger than SH and Y is
*       larger than SK.
*
* Parameters
*
*   SH  REAL, integration limit
*   SK  REAL, integration limit
*   R   REAL, correlation coefficient
*   LG  INTEGER, number of Gauss Rule Points and Weights
*
*      DOUBLE PRECISION BVN, SH, SK, R, ZERO, TWOPI
      DOUBLE PRECISION BVN, SH, SK, R, TWOPI
      INTEGER I, LG, NG
      PARAMETER ( TWOPI = 6.283185307179586D0 )
      DOUBLE PRECISION X(10,1), W(10,1), AS, A, B, C, D, RS, XS
*      DOUBLE PRECISION MVPHI, SN, ASR, H, K, BS, HS, HK
      DOUBLE PRECISION MVPHI, H, K, BS, HK
*     Gauss Legendre Points and Weights, N =  6
*      DATA ( W(I,1), X(I,1), I = 1, 3 ) /
*     *  0.1713244923791705D+00,-0.9324695142031522D+00,
*     *  0.3607615730481384D+00,-0.6612093864662647D+00,
*     *  0.4679139345726904D+00,-0.2386191860831970D+00/
*     Gauss Legendre Points and Weights, N = 12
*      DATA ( W(I,2), X(I,2), I = 1, 6 ) /
*     *  0.4717533638651177D-01,-0.9815606342467191D+00,
*     *  0.1069393259953183D+00,-0.9041172563704750D+00,
*     *  0.1600783285433464D+00,-0.7699026741943050D+00,
*     *  0.2031674267230659D+00,-0.5873179542866171D+00,
*     *  0.2334925365383547D+00,-0.3678314989981802D+00,
*     *  0.2491470458134029D+00,-0.1252334085114692D+00/
*     Gauss Legendre Points and Weights, N = 20
*      DATA ( W(I,3), X(I,3), I = 1, 10 ) /
      DATA ( W(I,1), X(I,1), I = 1, 10 ) /
     *  0.1761400713915212D-01,-0.9931285991850949D+00,
     *  0.4060142980038694D-01,-0.9639719272779138D+00,
     *  0.6267204833410906D-01,-0.9122344282513259D+00,
     *  0.8327674157670475D-01,-0.8391169718222188D+00,
     *  0.1019301198172404D+00,-0.7463319064601508D+00,
     *  0.1181945319615184D+00,-0.6360536807265150D+00,
     *  0.1316886384491766D+00,-0.5108670019508271D+00,
     *  0.1420961093183821D+00,-0.3737060887154196D+00,
     *  0.1491729864726037D+00,-0.2277858511416451D+00,
     *  0.1527533871307259D+00,-0.7652652113349733D-01/
*      IF ( ABS(R) .LT. 0.3 ) THEN
*         NG = 1
*         LG = 3
*      ELSE IF ( ABS(R) .LT. 0.75 ) THEN
*         NG = 2
*         LG = 6
*      ELSE
*         NG = 3
*         LG = 10
*      ENDIF
      PARAMETER ( NG = 1)
      PARAMETER ( LG = 10 )
      H = SH
      K = SK
      HK = H*K
      BVN = 0
*      IF ( ABS(R) .LT. 0.925 ) THEN
*         HS = ( H*H + K*K )/2
*         ASR = ASIN(R)
*         DO I = 1, LG
*            SN = SIN(ASR*( X(I,NG)+1 )/2)
*            BVN = BVN + W(I,NG)*EXP( ( SN*HK - HS )/( 1 - SN*SN ) )
*            SN = SIN(ASR*(-X(I,NG)+1 )/2)
*            BVN = BVN + W(I,NG)*EXP( ( SN*HK - HS )/( 1 - SN*SN ) )
*         END DO
*         BVN = BVN*ASR/(2*TWOPI) + MVPHI(-H)*MVPHI(-K)
*      ELSE
      IF ( R .LT. 0 ) THEN
         K = -K
         HK = -HK
      ENDIF
      IF ( ABS(R) .LT. 1 ) THEN
         AS = ( 1 - R )*( 1 + R )
         A = SQRT(AS)
         BS = ( H - K )**2
         C = ( 4 - HK )/8
         D = ( 12 - HK )/16
         BVN = A*EXP( -(BS/AS + HK)/2 )
     +             *( 1 - C*(BS - AS)*(1 - D*BS/5)/3 + C*D*AS*AS/5 )
         IF ( HK .GT. -160 ) THEN
            B = SQRT(BS)
            BVN = BVN - EXP(-HK/2)*SQRT(TWOPI)*MVPHI(-B/A)*B
     +                    *( 1 - C*BS*( 1 - D*BS/5 )/3 )
         ENDIF
         A = A/2
         DO I = 1, LG
            XS = ( A*(X(I,NG)+1) )**2
            RS = SQRT( 1 - XS )
            BVN = BVN + A*W(I,NG)*
     +              ( EXP( -BS/(2*XS) - HK/(1+RS) )/RS
     +              - EXP( -(BS/XS+HK)/2 )*( 1 + C*XS*( 1 + D*XS ) ) )
            XS = AS*(-X(I,NG)+1)**2/4
            RS = SQRT( 1 - XS )
            BVN = BVN + A*W(I,NG)*EXP( -(BS/XS + HK)/2 )
     +                    *( EXP( -HK*XS/(2*(1+RS)**2) )/RS
     +                       - ( 1 + C*XS*( 1 + D*XS ) ) )
         END DO
         BVN = -BVN/TWOPI
      ENDIF
      IF ( R .GT. 0 ) THEN
         BVN =  BVN + MVPHI( -MAX( H, K ) )
      ELSE
         BVN = -BVN
         IF ( K .GT. H ) THEN
            IF ( H .LT. 0 ) THEN
               BVN = BVN + MVPHI(K)  - MVPHI(H)
            ELSE
               BVN = BVN + MVPHI(-H) - MVPHI(-K)
            ENDIF
         ENDIF
      ENDIF
*      ENDIF
      MVBVU = BVN
      END
