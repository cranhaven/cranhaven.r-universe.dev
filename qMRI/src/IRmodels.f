      subroutine IRfluid(th, invtime, n, fval, grad)
C
C  function values and gradients (2 parameters, S_f, R_f)
C
C  fval = th(1) * abs( 1 - 2*exp(-invtime*th(2))
C
      implicit logical (a-z)
      integer n
      double precision th(2),invtime(n),fval(n),grad(n,2)
      integer i
      double precision z1,z2,th1,th2
      th1=th(1)
      th2=th(2)
      DO i=1,n
         z1=exp(-invtime(i)*th2)
         z2=1.d0-2.d0*z1
         fval(i)=th1*abs(z2)
         grad(i,1)=abs(z2)
         grad(i,2)=2.d0*z1*invtime(i)*dsign(1.d0,z2)
      END DO
      RETURN
      END

      subroutine IRmix(th, invtime, s0, r1, n, fval, grad)
C
C  function values and gradients (3 parameters, f, R_s, S_s)
C
C  fval = abs(s0*th(1)*(1-2*exp(-invtime*t1))+(1-th(1))*th(3)*(1-2*exp(-invtime*th2)))
C
      implicit logical (a-z)
      integer n
      double precision th(3),invtime(n),s0,r1,fval(n),grad(n,3)
      integer i
      double precision z1,z2,z3,th1,th2,th3,th13,fv,vz
      th1=th(1)
      th2=th(2)
      th3=th(3)
      th13=th3*(1.d0-th1)
      DO i=1,n
         z1=s0*(1.d0-2.d0*exp(-invtime(i)*r1))
         z2=exp(-invtime(i)*th2)
         z3=1.d0-2.d0*z2
         fv=th1*z1+th13*z3
         vz=dsign(1.d0,fv)
         fval(i)=abs(fv)
         grad(i,1)=(z1-th3*z3)*vz
         grad(i,2)=(th13*2.d0*z2*invtime(i))*vz
         grad(i,3)=(1.d0-th1)*z3*vz
      END DO
      RETURN
      END

      subroutine IRmix5(th, invtime, n, fval, grad)
C
C  function values and gradients (5 parameters, f, R_s, S_s, R_f, S_f)
C
C  fval = abs(th(5)*th(1)*(1-2*exp(-invtime*th(4))+(1-th(1))*th(3)*(1-2*exp(-invtime*th(2))))
C
      implicit logical (a-z)
      integer n
      double precision th(5),invtime(n),fval(n),grad(n,5)
      integer i
      double precision z1,z2,z3,z4,z5,th1,th2,th3,th4,th5,
     1                 th13,th15,fv,vz
      th1=th(1)
      th2=th(2)
      th3=th(3)
      th4=th(4)
      th5=th(5)
      th13=th3*(1.d0-th1)
      th15=th5*th1
      DO i=1,n
         z4=exp(-invtime(i)*th4)
         z1=th(5)*(1.d0-2.d0*z4)
         z2=exp(-invtime(i)*th2)
         z3=1.d0-2.d0*z2
         z5=1.d0-2.d0*z4
         fv=th1*z1+th13*z3
         vz=dsign(1.d0,fv)
         fval(i)=abs(fv)
         grad(i,1)=(z1-th3*z3)*vz
         grad(i,2)=(th13*2.d0*z2*invtime(i))*vz
         grad(i,3)=(1.d0-th1)*z3*vz
         grad(i,4)=(th15*2.d0*z4*invtime(i))*vz
         grad(i,5)=th1*z5*vz
      END DO
      RETURN
      END

      subroutine IRmixfv(th, invtime, s0, r1, n, fval)
C
C  function values  (3 parameters, f, R_s, S_s)
C
C  fval = abs(s0*th(1)*(1-2*exp(-invtime*t1))+(1-th(1))*th(3)*(1-2*exp(-invtime*th2)))
C
      implicit logical (a-z)
      integer n
      double precision th(3),invtime(n),s0,r1,fval(n)
      integer i
      double precision z1,z2,z3,th1,th2,th3,th13,fv
      th1=th(1)
      th2=th(2)
      th3=th(3)
      th13=th3*(1.d0-th1)
      DO i=1,n
         z1=s0*(1.d0-2.d0*exp(-invtime(i)*r1))
         z2=exp(-invtime(i)*th2)
         z3=1.d0-2.d0*z2
         fv=th1*z1+th13*z3
         fval(i)=abs(fv)
      END DO
      RETURN
      END

      subroutine IRmix0(th1, invtime, th2, th3, s0, t1, n, fval, grad)
C
C  function values and gradients (1 parameters, f)
C
C  fval = abs(s0*th(1)*(1-2*exp(-invtime*t1))+(1-th(1))*th(3)*(1-2*exp(-invtime*th(2))))
C
      implicit logical (a-z)
      integer n
      double precision th1,invtime(n),s0,t1,fval(n),grad(n),th2,th3
      integer i
      double precision z1,z3,th13,fv
      th13=th3*(1.d0-th1)
      DO i=1,n
         z1=s0*(1.d0-2.d0*exp(-invtime(i)*t1))
         z3=(1.d0-2.d0*exp(-invtime(i)*th2))
         fv=th1*z1+th13*z3
         fval(i)=abs(fv)
         grad(i)=(z1-th3*z3)*dsign(1.d0,fv)
      END DO
      RETURN
      END

      subroutine IRmix5fv(th, invtime, n, fval)
C
C  function values and gradients (5 parameters, f, R_s, S_s, R_f, S_f)
C
C  fval = abs(th(5)*th(1)*(1-2*exp(-invtime*th(4))+(1-th(1))*th(3)*(1-2*exp(-invtime*th(2))))
C
      implicit logical (a-z)
      integer n
      double precision th(5),invtime(n),fval(n)
      integer i
      double precision z1,z2,z3,z4,z5,th1,th2,th3,th4,th5,
     1                 th13,th15,fv
      th1=th(1)
      th2=th(2)
      th3=th(3)
      th4=th(4)
      th5=th(5)
      th13=th3*(1.d0-th1)
      th15=th5*th1
      DO i=1,n
         z4=exp(-invtime(i)*th4)
         z1=th(5)*(1.d0-2.d0*z4)
         z2=exp(-invtime(i)*th2)
         z3=1.d0-2.d0*z2
         z5=1.d0-2.d0*z4
         fv=th1*z1+th13*z3
         fval(i)=abs(fv)
      END DO
      RETURN
      END

