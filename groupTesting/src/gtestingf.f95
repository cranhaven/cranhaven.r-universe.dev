module gtesting
  use, intrinsic :: iso_c_binding

  implicit none
  private
  public :: gbsonedhom_f
  public :: cvondknachom_f
  public :: gbsonedsreg_f

  
contains

  subroutine gbsonedhom_f(p,Ytmat,Zmat,N,SeSp,Ycol,Zrow,Zcol,U,GI,a,ret) bind(C, name="gbsonedhom_f_")
    implicit none
    integer(kind = c_int), intent(in)                       :: N, Ycol, Zrow, Zcol, GI, a
    real(kind = c_double), intent(in)                       :: p
    real(kind = c_double), intent(in), dimension(Zrow,2)    :: SeSp
    integer(kind = c_int), intent(inout), dimension(N,Ycol) :: Ytmat
    integer(kind = c_int), intent(in), dimension(Zrow,Zcol) :: Zmat
    real(kind = c_double), intent(in), dimension(N,GI)      :: U
    integer(kind = c_int), intent(inout), dimension(N)      :: ret

    integer           :: g, i, j, k, s
    integer           :: Z1, sm1, gma1
    real(kind(1.0d0)) :: zeta, zeta0, zeta1
    integer           :: pid, psz
    real(kind(1.0d0)) :: Se1, Sp1, RSe1, RSp1

    do i = 1, N
      ret(i) = 0	
    end do
    do g = 1, GI
      do i = 1, N
        Ytmat(i,1) = 0
        zeta0 = 1.0_c_double
        zeta1 = 1.0_c_double
        do j=1, Ytmat(i,2)
          pid = Ytmat(i,2+j)
          Z1 = Zmat(pid,1)
          psz = Zmat(pid,2)
          Se1 = SeSp(pid,1)
          Sp1 = SeSp(pid,2)
          sm1 = 0
          do s=1, psz
            k = Zmat(pid,2+s)
            sm1 = sm1 + Ytmat(k,1)
          end do
          gma1 = 0
          if(sm1 .gt. 0) then
            gma1 = 1
          end if
          RSe1 = (Se1**Z1)*((1_c_double-Se1)**(1_c_double-Z1))
          RSp1 = (Sp1**(1_c_double-Z1))*((1_c_double-Sp1)**Z1)
          zeta0 = zeta0*(RSe1**gma1)*(RSp1**(1_c_double-gma1))
          zeta1 = zeta1*RSe1
        end do
        zeta0 = zeta0*(1.0_c_double-p)
        zeta1 = zeta1*p
        zeta = zeta0 + zeta1
        zeta0 = zeta0/zeta
        if(U(i,g) .gt. zeta0) then
          Ytmat(i,1)=1
        else
          Ytmat(i,1)=0
        end if
        if(g .gt. a) then
          ret(i) = ret(i) + Ytmat(i,1)
        end if
      end do
    end do
  end subroutine gbsonedhom_f

  subroutine cvondknachom_f(p,Ytmat,Zmat,N,SeSp,Ycol,Zrow,Zcol,U,GI,a,ret) bind(C, name="cvondknachom_f_")
    implicit none
    integer(kind = c_int), intent(in)                       :: N, Ycol, Zrow, Zcol, GI, a
    real(kind = c_double), intent(in)                       :: p
    real(kind = c_double), intent(in), dimension(Zrow,2)    :: SeSp
    integer(kind = c_int), intent(inout), dimension(N,Ycol) :: Ytmat
    integer(kind = c_int), intent(in), dimension(Zrow,Zcol) :: Zmat
    real(kind = c_double), intent(in), dimension(N,GI)      :: U
    real(kind = c_double), intent(out)                      :: ret

    integer           :: g, i, j, k, s
    integer           :: Z1, sm1, gma1
    real(kind(1.0d0)) :: zeta, zeta0, zeta1
    integer           :: pid, psz
    real(kind(1.0d0)) :: Se1, Sp1, RSe1, RSp1
    integer, dimension(N) :: V
    integer               :: ytsm
    real(kind(1.0d0))     :: Elc, dlc, Eyi, tmp1, dlcmat(GI-a)
    real(kind(1.0d0))     :: covr, d2Q

    V = 0
    covr = 0
    d2Q = 0
    Elc = 0
    do g = 1, GI
      ytsm = 0
      do i = 1, N
        Ytmat(i,1) = 0
        zeta0 = 1.0_c_double
        zeta1 = 1.0_c_double
        do j=1, Ytmat(i,2)
          pid = Ytmat(i,2+j)
          Z1 = Zmat(pid,1)
          psz = Zmat(pid,2)
          Se1 = SeSp(pid,1)
          Sp1 = SeSp(pid,2)
          sm1 = 0
          do s=1, psz
            k = Zmat(pid,2+s)
            sm1 = sm1 + Ytmat(k,1)
          end do
          gma1 = 0
          if(sm1 .gt. 0) then
            gma1 = 1
          end if
          RSe1 = (Se1**Z1)*((1_c_double-Se1)**(1_c_double-Z1))
          RSp1 = (Sp1**(1-Z1))*((1_c_double-Sp1)**Z1)
          zeta0 = zeta0*(RSe1**gma1)*(RSp1**(1_c_double-gma1))
          zeta1 = zeta1*RSe1
        end do
        zeta0 = zeta0*(1.0_c_double-p)
        zeta1 = zeta1*p
        zeta = zeta0 + zeta1
        zeta0 = zeta0/zeta
        if(U(i,g) .gt. zeta0) then
          Ytmat(i,1)=1
        else
          Ytmat(i,1)=0
        end if
        if(g .gt. a) then
          V(i) = V(i) + Ytmat(i,1)
          ytsm = ytsm + Ytmat(i,1)
        end if
      end do
      if(g .gt. a) then
        dlc = dble(ytsm-p*N)/(p*(1.0_c_double-p))
        Elc = Elc + dlc
        dlcmat(g-a) = dlc
      end if
    end do

    Elc = Elc/dble(GI-a)
    do g=1, (GI-a)
      covr = covr+( dlcmat(g)-Elc )*( dlcmat(g)-Elc )
    end do
    do i = 1, N
      Eyi = dble(V(i))/dble(GI-a)
      tmp1 = Eyi/(p**2) + (1.0_c_double-Eyi)/( (1.0_c_double-p)**2 )
      d2Q = d2Q + tmp1
    end do
    ret = d2Q - covr/dble(GI-a)
  end subroutine cvondknachom_f

  subroutine gbsonedsreg_f(p,Ytmat,Zmat,N,SeSp,Ycol,Zrow,Zcol,U,GI,a,ret) bind(C, name="gbsonedsreg_f_")
    implicit none
    integer(kind = c_int), intent(in)                       :: N, Ycol, Zrow, Zcol, GI, a
    real(kind = c_double), intent(in), dimension(N)         :: p
    real(kind = c_double), intent(in), dimension(Zrow,2)    :: SeSp
    integer(kind = c_int), intent(inout), dimension(N,Ycol) :: Ytmat
    integer(kind = c_int), intent(in), dimension(Zrow,Zcol) :: Zmat
    real(kind = c_double), intent(in), dimension(N,GI)      :: U
    integer(kind = c_int), intent(inout), dimension(N)      :: ret

    integer           :: g, i, j, k, s
    integer           :: Z1, sm1, gma1
    real(kind(1.0d0)) :: zeta, zeta0, zeta1
    integer           :: pid, psz
    real(kind(1.0d0)) :: Se1, Sp1, RSe1, RSp1

    do i = 1, N
      ret(i) = 0	
    end do
    do g = 1, GI
      do i = 1, N
        Ytmat(i,1) = 0
        zeta0 = 1.0_c_double
        zeta1 = 1.0_c_double
        do j=1, Ytmat(i,2)
          pid = Ytmat(i,2+j)
          Z1 = Zmat(pid,1)
          psz = Zmat(pid,2)
          Se1 = SeSp(pid,1)
          Sp1 = SeSp(pid,2)
          sm1 = 0
          do s=1, psz
            k = Zmat(pid,2+s)
            sm1 = sm1 + Ytmat(k,1)
          end do
          gma1 = 0
          if(sm1 .gt. 0) then
            gma1 = 1
          end if
          RSe1 = (Se1**Z1)*((1_c_double-Se1)**(1_c_double-Z1))
          RSp1 = (Sp1**(1_c_double-Z1))*((1_c_double-Sp1)**Z1)
          zeta0 = zeta0*(RSe1**gma1)*(RSp1**(1_c_double-gma1))
          zeta1 = zeta1*RSe1
        end do
        zeta0 = (1.0_c_double-p(i))*zeta0
        zeta1 = p(i)*zeta1
        zeta = zeta0 + zeta1
        zeta0 = zeta0/zeta
        if(U(i,g) .gt. zeta0) then
          Ytmat(i,1)=1
        else
          Ytmat(i,1)=0
        end if
        if(g .gt. a) then
          ret(i) = ret(i) + Ytmat(i,1)
        end if
      end do
    end do
  end subroutine gbsonedsreg_f

  subroutine cvondknacreg_f(dg,d2g,blen,p,SeSp,Ytmat,Zmat,X,N,Ycol,Zrow,Zcol,U,GI,a,ret) bind(C, name="cvondknacreg_f_")
    implicit none
    integer(kind = c_int), intent(in)                          :: blen, N, Ycol, Zrow, Zcol, GI, a
    real(kind = c_double), intent(in), dimension(N)            :: dg
    real(kind = c_double), intent(in), dimension(N)            :: d2g
    real(kind = c_double), intent(in), dimension(N)            :: p
    real(kind = c_double), intent(in), dimension(Zrow,2)       :: SeSp
    real(kind = c_double), intent(in), dimension(N,blen)       :: X
    integer(kind = c_int), intent(inout), dimension(N,Ycol)    :: Ytmat
    integer(kind = c_int), intent(in), dimension(Zrow,Zcol)    :: Zmat
    real(kind = c_double), intent(in), dimension(N,GI)         :: U
    real(kind = c_double), intent(out), dimension(blen,blen)   :: ret

    real(kind(1.0d0)), dimension(blen,blen) :: d2Q
    real(kind(1.0d0)), dimension(blen,blen) :: covr
    integer, dimension(N)                :: V
    integer                              :: g, i, j, k, s, c, d
    integer                              :: Z1, sm1, gma1
    real(kind(1.0d0))                       :: zeta, zeta0, zeta1
    integer                              :: pid, psz
    real(kind(1.0d0))                       :: Se1, Sp1, RSe1, RSp1
    real(kind(1.0d0)), dimension(blen)      :: Elc
    real(kind(1.0d0)), dimension(blen)      :: dlc
    real(kind(1.0d0))                       :: Eyi, tmp1, tmp2, lctmp1, d2pdb
    real(kind(1.0d0)), dimension(blen)      :: dpdb
    real(kind(1.0d0)), dimension(GI-a,blen) :: dlcmat

    V = 0
    Elc = 0.0
    d2Q = 0.0
    covr = 0.0
    do g = 1, GI
      dlc = 0.0
      do i = 1, N
        Ytmat(i,1) = 0
        zeta0 = 1.0_c_double
        zeta1 = 1.0_c_double
        do j=1, Ytmat(i,2)
          pid = Ytmat(i,2+j)
          Z1 = Zmat(pid,1)
          psz = Zmat(pid,2)
          Se1 = SeSp(pid,1)
          Sp1 = SeSp(pid,2)
          sm1 = 0
          do s=1, psz
            k = Zmat(pid,2+s)
            sm1 = sm1 + Ytmat(k,1)
          end do
          gma1 = 0
          if(sm1 .gt. 0) then
            gma1 = 1
          end if
          RSe1 = (Se1**Z1)*((1_c_double-Se1)**(1_c_double-Z1))
          RSp1 = (Sp1**(1_c_double-Z1))*((1_c_double-Sp1)**Z1)
          zeta0 = zeta0*(RSe1**gma1)*(RSp1**(1_c_double-gma1))
          zeta1 = zeta1*RSe1
        end do
        zeta0 = zeta0*(1.0_c_double-p(i))
        zeta1 = zeta1*p(i)
        zeta = zeta0 + zeta1
        zeta0 = zeta0/zeta
        if(U(i,g) .gt. zeta0) then
          Ytmat(i,1)=1
        else
          Ytmat(i,1)=0
        end if
        if(g .gt. a) then
          V(i) = V(i) + Ytmat(i,1)
          lctmp1 = dble((Ytmat(i,1)-p(i))*dg(i))/(p(i)*(1.0_c_double-p(i)))
          do s=1, blen
            dlc(s) = dlc(s) + lctmp1*X(i,s)
          end do
	    end if
      end do
      if(g .gt. a) then
        do s=1, blen
          Elc(s) = Elc(s) + dlc(s)
          dlcmat((g-a),s) = dlc(s) 
        end do
      end if
    end do

    do s=1, blen
      Elc(s) = Elc(s)/dble(GI-a)
    end do
    do c=1, blen
      do d=1, blen
        do g=1, (GI-a)
          covr(c,d) = covr(c,d)+( dlcmat(g,c)-Elc(c) )*( dlcmat(g,d)-Elc(d) )
        end do
      end do
    end do

    do i = 1, N
      Eyi = dble(V(i))/dble(GI-a)
      tmp1 = Eyi/(p(i)**2) + (1.0_c_double-Eyi)/( (1.0_c_double-p(i))**2 )
      tmp2 = (p(i) - Eyi)/( p(i)*(1.0_c_double-p(i)) )
      do s = 1, blen
        dpdb(s) = dg(i)*X(i,s)
      end do
      do c = 1, blen
        do d = 1, blen
          d2pdb = d2g(i)*X(i,c)*X(i,d)
          d2Q(c,d) = d2Q(c,d) + dpdb(c)*dpdb(d)*tmp1 + tmp2*d2pdb
        end do
      end do
    end do
    ret = d2Q - covr/dble(GI-a)
  end subroutine cvondknacreg_f

end module gtesting
