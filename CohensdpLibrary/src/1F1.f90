FUNCTION hyg1F1(A, B, Z)
    !******************************************************************
    !*      Purpose: This program computes the confluent              *
    !*               hypergeometric function M(a,b,x) using           *
    !*               subroutine CHGM                                  *
    !*      Input  : a  --- Parameter                                 *
    !*               b  --- Parameter ( b <> 0,-1,-2,... )            *
    !*               x  --- Argument                                  *
    !*      Output:  HG --- M(a,b,x)                                  *
    !* -------------------------------------------------------------- *
    !* REFERENCE: "Fortran Routines for Computation of Special        *
    !*             Functions jin.ece.uiuc.edu/routines/routines.html" *
    !*                                                                *
    !*                              F90 Release By J-P Moreau, Paris. *
    !*                                     (www.jpmoreau.fr)          *
    !******************************************************************
    !**                                                              **
    !** I removed some of the goto's in DO's and added ENDDO         **
    !**     (Denis Cousineau, 6 aout 2022)                           **
    !**                                                              **
    !******************************************************************
    INTEGER, PARAMETER    :: PR=KIND(1.0D0)
    REAL(PR), INTENT(IN)  :: A, B, Z
    REAL(PR)              :: hyg1F1
    REAL(PR)              :: HG

    ! Moreau version, more precise.
    CALL CHGM(A,B,Z,HG)
    hyg1F1 = HG

END FUNCTION hyg1F1




SUBROUTINE CHGM(A,B,X,HG)
    !===================================================
    !  Purpose: Compute confluent hypergeometric function
    !           M(a,b,x)
    !  Input  : a  --- Parameter
    !           b  --- Parameter ( b <> 0,-1,-2,... )
    !           x  --- Argument
    !  Output:  HG --- M(a,b,x)
    !  Routine called: GAMMA for computing GAMMA(x)
    !===================================================
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
    PI=3.141592653589793D0
    Y1=0.0D0
    A0=A
    A1=A
    X0=X
    HG=0.0D0
    IF (B.EQ.0.0D0.OR.B.EQ.-ABS(INT(B))) THEN
       HG=1.0D+300
    ELSE IF (A.EQ.0.0D0.OR.X.EQ.0.0D0) THEN
       HG=1.0D0
    ELSE IF (A.EQ.-1.0D0) THEN
       HG=1.0D0-X/B
    ELSE IF (A.EQ.B) THEN
       HG=DEXP(X)
    ELSE IF (A-B.EQ.1.0D0) THEN
       HG=(1.0D0+X/B)*DEXP(X)
    ELSE IF (A.EQ.1.0D0.AND.B.EQ.2.0D0) THEN
       HG=(DEXP(X)-1.0D0)/X
    ELSE IF (A.EQ.INT(A).AND.A.LT.0.0D0) THEN
       M=INT(-A)
       R=1.0D0
       HG=1.0D0
       DO K=1,M     ! removed label 10
          R=R*(A+K-1.0D0)/K/(B+K-1.0D0)*X
          HG=HG+R   ! removed label 10
        enddo       ! added for newer compilers
    ENDIF
    IF (HG.NE.0.0D0) RETURN
    IF (X.LT.0.0D0) THEN
       A=B-A
       A0=A
       X=DABS(X)
    ENDIF
    IF (A.LT.2.0D0) NL=0
    IF (A.GE.2.0D0) THEN
       NL=1
       LA=INT(A)
       A=A-LA-1.0D0
    ENDIF
    DO 30 N=0,NL
       IF (A0.GE.2.0D0) A=A+1.0D0
       IF (X.LE.30.0D0+DABS(B).OR.A.LT.0.0D0) THEN
          HG=1.0D0
          RG=1.0D0
          DO 15 J=1,500
             RG=RG*(A+J-1.0D0)/(J*(B+J-1.0D0))*X
             HG=HG+RG
             IF (DABS(RG/HG).LT.1.0D-15) GO TO 25
15        CONTINUE
       ELSE
          TA =  GAMMA(A)
          TB =  GAMMA(B)
          XG=B-A
          TBA = GAMMA(XG)
          SUM1=1.0D0
          SUM2=1.0D0
          R1=1.0D0
          R2=1.0D0
          DO I=1,8          ! removed label 20
             R1=-R1*(A+I-1.0D0)*(A-B+I)/(X*I)
             R2=-R2*(B-A+I-1.0D0)*(A-I)/(X*I)
             SUM1=SUM1+R1
             SUM2=SUM2+R2   ! removed label 20
          enddo             ! added for newer compiler
          HG1=TB/TBA*X**(-A)*DCOS(PI*A)*SUM1
          HG2=TB/TA*DEXP(X)*X**(A-B)*SUM2
          HG=HG1+HG2
       ENDIF
25         IF (N.EQ.0) Y0=HG
       IF (N.EQ.1) Y1=HG
30      CONTINUE
    IF (A0.GE.2.0D0) THEN
       DO I=1,LA-1      ! removed label 35
          HG=((2.0D0*A-B+X)*Y1+(B-A)*Y0)/A
          Y0=Y1
          Y1=HG
          A=A+1.0D0     ! remove label 35
       enddo            !  added for newer compier
    ENDIF

    IF (X0.LT.0.0D0) HG=HG*DEXP(X0)
    A=A1
    X=X0
    RETURN
END

