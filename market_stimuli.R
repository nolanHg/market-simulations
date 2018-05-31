issue_earnings_report <- function(p_vneg = 0.25, p_neg = 0.25, p_pos = 0.25, p_vpos = 0.25)
{
	result_lvls_v <- c("VBAD", "BAD", "GOOD", "VGOOD")
	probs_v <- c(p_vneg, p_neg, p_pos, p_vpos)	
	result <- sample(result_lvls_v, size = 1, prob = probs_v)
	
	return(invisible(result))
}
