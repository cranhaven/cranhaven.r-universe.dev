      subroutine estatics3(th, des, n, fval, grad)
C
C  function values and gradients (4 parameters)
C
C  fval = th(1) * exp(- th(4) * des(i,4)) * des(i,1)
C       + th(2) * exp(- th(4) * des(i,4)) * des(i,2)
C       + th(3) * exp(- th(4) * des(i,4)) * des(i,3)
C
      implicit logical (a-z)
      integer n
      double precision th(4),des(n,4),fval(n),grad(n,4)
      integer i
      double precision z4,fv
      fv=0.d0
      DO i=1,n
         z4=exp(-th(4)*des(i,4))
         if(des(i,1).gt.0) THEN
            fv=z4*th(1)
            grad(i,1)=z4
            grad(i,2)=0.d0
            grad(i,3)=0.d0
         END IF
         if(des(i,2).gt.0) THEN
            fv=z4*th(2)
            grad(i,1)=0.d0
            grad(i,2)=z4
            grad(i,3)=0.d0
         END IF
         if(des(i,3).gt.0) THEN
            fv=z4*th(3)
            grad(i,1)=0.d0
            grad(i,2)=0.d0
            grad(i,3)=z4
         END IF
         grad(i,4)=-des(i,4)*fv
         fval(i)=fv
      END DO
      RETURN
      END

      subroutine estatics2(th, des, n, fval, grad)
C
C  function values and gradients (3 parameters)
C
C  fval = th(1) * exp(- th(3) * des(i,3)) * des(i,1)
C       + th(2) * exp(- th(3) * des(i,3)) * des(i,2)
C
      implicit logical (a-z)
      integer n
      double precision th(3),des(n,3),fval(n),grad(n,3)
      integer i
      double precision z3,fv
      fv=0.d0
      DO i=1,n
         z3=exp(-th(3)*des(i,3))
         if(des(i,1).gt.0) THEN
            fv=z3*th(1)
            grad(i,1)=z3
            grad(i,2)=0.d0
         END IF
         if(des(i,2).gt.0) THEN
            fv=z3*th(2)
            grad(i,1)=0.d0
            grad(i,2)=z3
         END IF
         grad(i,3)=-des(i,3)*fv
         fval(i)=fv
      END DO
      RETURN
      END

      subroutine estatics1(th, des, n, fval, grad)
C
C  function values and gradients (2 parameter)
C
C  fval = th(1) * exp(- th(2) * des(i,2)) * des(i,1)
C
      implicit logical (a-z)
      integer n
      double precision th(2),des(n,2),fval(n),grad(n,2)
      integer i
      double precision z2,fv
      fv=0.d0
      DO i=1,n
         z2=exp(-th(2)*des(i,2))
         if(des(i,1).gt.0) THEN
            fv=z2*th(1)
            grad(i,1)=z2
         END IF
         grad(i,2)=-des(i,2)*fv
         fval(i)=fv
      END DO
      RETURN
      END

      subroutine estatics3fixedR2(th, r2star, des, n, fval, grad)
C
C  function values and gradients (3 parameters)
C
C  fval = th(1) * exp(- r2star * des(i,4)) * des(i,1)
C       + th(2) * exp(- r2star * des(i,4)) * des(i,2)
C       + th(3) * exp(- r2star * des(i,4)) * des(i,3)
C
      implicit logical (a-z)
      integer n
      double precision th(3),r2star,des(n,4),fval(n),grad(n,3)
      integer i
      double precision z4,fv
      fv=0.d0
      DO i=1,n
         z4=exp(-r2star*des(i,4))
         if(des(i,1).gt.0) THEN
            fv=z4*th(1)
            grad(i,1)=z4
            grad(i,2)=0.d0
            grad(i,3)=0.d0
         END IF
         if(des(i,2).gt.0) THEN
            fv=z4*th(2)
            grad(i,1)=0.d0
            grad(i,2)=z4
            grad(i,3)=0.d0
         END IF
         if(des(i,3).gt.0) THEN
            fv=z4*th(3)
            grad(i,1)=0.d0
            grad(i,2)=0.d0
            grad(i,3)=z4
         END IF
         fval(i)=fv
      END DO
      RETURN
      END

      subroutine estatics2fixedR2(th, r2star, des, n, fval, grad)
C
C  function values and gradients (2 parameters)
C
C  fval = th(1) * exp(- r2star * des(i,3)) * des(i,1)
C       + th(2) * exp(- r2star * des(i,3)) * des(i,2)
C
      implicit logical (a-z)
      integer n
      double precision th(2),r2star,des(n,3),fval(n),grad(n,2)
      integer i
      double precision z3,fv
      fv=0.d0
      DO i=1,n
         z3=exp(-r2star*des(i,3))
         if(des(i,1).gt.0) THEN
            fv=z3*th(1)
            grad(i,1)=z3
            grad(i,2)=0.d0
         END IF
         if(des(i,2).gt.0) THEN
            fv=z3*th(2)
            grad(i,1)=0.d0
            grad(i,2)=z3
         END IF
         fval(i)=fv
      END DO
      RETURN
      END

      subroutine estatics1fixedR2(th, r2star, des, n, fval, grad)
C
C  function values and gradients (1 parameter)
C
C  fval = th(1) * exp(- r2star * des(i,2)) * des(i,1)
C
      implicit logical (a-z)
      integer n
      double precision th(1),r2star,des(n,2),fval(n),grad(n,1)
      integer i
      double precision z2,fv
      fv=0.d0
      DO i=1,n
         z2=exp(-r2star*des(i,2))
         if(des(i,1).gt.0) THEN
            fv=z2*th(1)
            grad(i,1)=z2
         END IF
         fval(i)=fv
      END DO
      RETURN
      END
