      subroutine hg1f1(a,b,z,n,fz)
      implicit logical (a-z)
      integer n
      double precision a,b,z(n),fz(n)
      integer i
      double precision x,y,d,eps,zi,ezi,ai,gofbai
      double precision gammaf
      external gammaf
      eps=1.d-15
      gofbai=gammaf(b)/gammaf(b-a)
      DO i=1,n
         d = 1.d0
         zi = z(i)
         IF(zi.lt.0) THEN
            ezi=exp(zi/2)
            ai=b-a
            if(zi.lt.-1400) THEN
               fz(i) = exp((-a)*log(-zi))*gofbai+5.6e-3+1.9e-3*b
C   add +5.6e-3+1.9e-3*b to keep the function monotone 
               CYCLE
            END IF
         ELSE
            ezi=1.d0
            ai=a
         ENDIF
         x = ezi
         y = ezi
         DO WHILE (abs(y).gt.abs(x)*eps)
            y = -y*(ai+d-1.d0)/(b+d-1.d0)*zi/d
            x = x+y
            d = d+1.d0
         END DO
         fz(i) = ezi*x
      END DO
      RETURN
      END
