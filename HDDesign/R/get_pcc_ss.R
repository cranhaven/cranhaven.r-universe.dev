get_pcc_ss <-
function(u, v, p1, ss)
{
	if (v==0)
	{
		Q_value=c(1,0)
		pcc_value=p1
	}
	else 
	{
		if (u==0)
		{
			Q_value=c(0.5, 0.5)
			pcc_value=0.5
		}
		else
		{
			Q_value=Q_func2(u,sqrt(v), p1=p1)
			pcc_value=sum(Q_value*c(p1, 1-p1))
		}
	}


	if (ss==F)
		pcc_value
	else
		c(pcc_value, Q_value)


}
