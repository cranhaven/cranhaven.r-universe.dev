c   GGMselect R package
c   Copyright INRA 2017
c   INRA, UR1404, Research Unit MaIAGE
c   F78352 Jouy-en-Josas, France.
c  
c   URL: http://genome.jouy.inra.fr/logiciels/GGMselect
c --------------------------------------------------------

c    Loops for the method EW
c Called by the R function  EW

      subroutine bouclet(p, k, veutlw,
     c a,b,c,d,h,
     c Xy,XX,alea, L, LEW)
c     a=2*h*beta/n, b=sqrt(2h), c= 2h, d=tau**2
c     INPUT
      integer p, k, veutlw
      double precision a, b, c, d, h
      double precision Xy(p), XX(p,p), alea(p*k)
c     INPUT/OUTPUT
      double precision L(p), LEW(p)
c LEW are calculated  if veutlw=1 only

c Working variables
      double precision XXL(p)
      integer i, j, it

c     boucle principale
      b1: do it=1,k
c     calcul XXL
         b2: do i=1,p
            XXL(i)=0.D0
            b3: do j=1,p
               XXL(i)=XXL(i)+XX(i,j)*L(j)
            end do b3
         end do b2
c     premier update
         b4: do i=1,p
            L(i)=L(i)+a*(Xy(i)-XXL(i))+b*alea((it-1)*p+i)
           
c     second update
            L(i)=L(i)-c*L(i)/(d+L(i)**2)

            if (veutlw .NE. 0) LEW(i)=LEW(i)+L(i)*h
            end do b4
      end do b1
c To verify
c           write(6, 1001) L(1), L(2)
c      if (veutlw .NE. 0)
c     c      write(6, 1001) LEW(1), LEW(2)
c 1001  FORMAT(2(2x,d10.4)) 
      end subroutine  bouclet
