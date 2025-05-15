ideal_pcc_corr <-
function(mu0, p, m, p1=0.5, eigval.star, rho.cs=0, Sigma, mu)
{
	kt=1/2*log((1-p1)/p1)

     tt= sqrt( m*(mu0^2)/eigval.star)

      pcc.ideal1=pnorm( tt - kt/tt  )*p1 + pnorm( tt - kt/tt )*(1-p1)


      tt= sqrt( t(mu) %*% solve(Sigma) %*% mu   ) 

      pcc.ideal2=pnorm( tt - kt/tt  )*p1 + pnorm( tt - kt/tt )*(1-p1)

	pcc.01weights.cs = pnorm( (m*mu0-kt)/sqrt( (1-rho.cs)*m+rho.cs*m*m)  )*p1 +
                        pnorm( (m*mu0+kt)/sqrt( (1-rho.cs)*m+rho.cs*m*m)  )*(1-p1)

      c(pcc.ideal1 ,  pcc.ideal2, pcc.01weights.cs )
}
