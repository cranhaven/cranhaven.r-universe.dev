sloplenFUN=function(ls,slope,method="WSmith"){
  if(!missing(slope)){warning("slope must be in degrees")}
     slope=slope*pi/180
    slop_pct=tan(slope)*100
  if(method=="WSmith"){
    m=ifelse(slop_pct<1,0.2,ifelse(slop_pct<3.5,0.3,ifelse(slop_pct<4.5,0.4,0.5)))
    SL=(ls/22.13)^m*((65.41*sin(slope)^2)+(4.56*sin(slope)+0.065))# Wischmieir and Smith (1978)
  }

  else if(method=="Remortel"){
    B=sin(slope)/(0.56+3*(sin(slope))^0.8)
    m=B/(1+B)
    S=ifelse(slop_pct<9,(0.03+10.8*sin(slope)),(16.8*sin(slope)-0.05))
    L=(ls/22.13)^m
    SL=L*S
  }
 else if(method=="Zhang"){
   m=ifelse(slop_pct<1.7,0.2,ifelse(slop_pct<5.2,0.3,ifelse(slop_pct<9,0.4,0.5)))
   S=ifelse(slop_pct<9,(0.03+10.8*sin(slope)),ifelse(slop_pct<17.6,(16.8*sin(slope)-0.05),(21.9*sin(slope)-0.96)))
   L=(ls/22.13)^m
   SL=L*S
 }
  else if(method=="Renard"){
    B=sin(slope)/(0.0896*(3*sin(slope)^0.8+0.56))
    m=B/(1+B)
    L=(ls/22.13)^m
    S=ifelse(L<6,(3*sin(slope)^0.8+0.56),ifelse(slop_pct<9,(10.8*sin(slope)+0.03),(16.8*sin(slope)-0.5)))
    SL=L*S
  }
  else if(method=="Nearing"){
    m=ifelse(slop_pct<1,0.2,ifelse(slop_pct<3.5,0.3,ifelse(slop_pct<4.5,0.4,0.5)))
    L=(ls/22.13)^m
    S1=((65.41*sin(slope)^2)+(4.56*sin(slope)+0.065))
    S2=-1.5+17/(1+exp(2.3-6.1*sin(slope)))
    S=ifelse(slope<14,S1,S2)
    SL=S*L
  }
  else if(method=="Smith"){
    m=ifelse(slop_pct<1,0.2,ifelse(slop_pct<3.5,0.3,ifelse(slop_pct<4.5,0.4,0.5)))
    L=(ls/22.13)^m
    S=0.0065*(slop_pct)^2+0.0453*slop_pct+0.065
    SL=L*S
  }
  else if(method=="Foster"){
    m=ifelse(slop_pct<1,0.2,ifelse(slop_pct<3.5,0.3,ifelse(slop_pct<4.5,0.4,0.5)))
    L=(ls/22.13)^m
    S=0.56+3*sin(slope)^0.8
    SL=L*S
  }
  else if(method=="McCool"){
    m=ifelse(slop_pct<1,0.2,ifelse(slop_pct<3.5,0.3,ifelse(slop_pct<4.5,0.4,0.5)))
    L=(ls/22.13)^m
    S=ifelse(slop_pct<9,(0.03+10.6*sin(slope)),(sin(slope)/0.0896)^0.6)
    SL=L*S
  }
  else if(method=="David"){SL=0.021*(slop_pct)^(4/3)}#David (1988)
  else if(method=="Morgan"){SL=(ls/22)^0.5*(0.065+0.045*slop_pct+0.0065*(slop_pct)^2)}
  else if (method=="Moore"){SL=1.5*((ls/22.13)^0.5)*(sin(slope)/0.09)^1.2}
  return(SL)
}
