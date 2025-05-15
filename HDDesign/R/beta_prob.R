beta_prob <-
function(parm)
{
	pbeta(parm[1], parm[2], parm[3], lower.tail=F, log.p=T)
}
 