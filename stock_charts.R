library("ggplot2")
library("gridExtra")

################################
#          CONSTANTS           #
################################
TICK_SIZE <- 0.001

display_stock_charts <- function(num_stocks = 6, tickers_v, pdata_l, t)
{
	col_det <- lapply(pdata_l, FUN = function(x_v) x_v[length(x_v)] - x_v[1])		
	colors <- ifelse(col_det < 0, "red", "#119334")

	plots <- mapply(pdata_l, as.list(tickers_v), colors,
		        FUN = function(x_v, y, z) qplot(0:t, x_v, geom = "line", xlab = "Time", ylab = "Price", main = y) + 
			guides(colour = "none") + geom_line(color = z) + 
			annotate(geom = "text", x = 125, y = 30, label = "Q1") +
			annotate(geom = "text", x = 250, y = 30, label = "Q2") +
			annotate(geom = "text", x = 375, y = 30, label = "Q3"),
		        SIMPLIFY = FALSE)	

	grid.arrange(grobs = plots, nrow = num_stocks / 2)
}

market_sim <- function(n_steps = 500, n_hft_traders = 500, n_fund_traders = 500,
	               n_mom_traders = 500, n_rand_traders = 500)
{

	Q1 <- floor(n_steps / 4)
	Q2 <- floor(2 * (n_steps / 4))
	Q3 <- floor(3 * (n_steps / 4))

	n_traders <- n_hft_traders + n_fund_traders + n_mom_traders + n_rand_traders
	tickers_v <- c("GS", "BAC", "SWKS", "GOOS", "UPS", "JPM")

	gs_hist <- rep(NA, n_steps)
	bac_hist <- rep(NA, n_steps)
	swks_hist <- rep(NA, n_steps)
	goos_hist <- rep(NA, n_steps)
	ups_hist <- rep(NA, n_steps)
	jpm_hist <- rep(NA, n_steps)

	gs_hist[1] <- 237.71
	bac_hist[1] <- 30.54
	swks_hist[1] <- 98.83
	goos_hist[1] <- 39.24
	ups_hist[1] <- 116.95
	jpm_hist[1] <- 112.05

	pdata_l <- list(gs_hist, bac_hist, swks_hist, goos_hist, ups_hist, jpm_hist)

	time <- 0 

	## keep track of trader IDs, stock ownership, profits/losses in dataframe	
	## wealth should be distributed according to pareto distribution to make
	## simulation more realistic
	tags_m <- cbind(rep("HFT", n_hft_traders), rep("FUND", n_fund_traders),
		        rep("MOM", n_mom_traders), rep("RAND", n_rand_traders))

	capital_v <- rep(1e6, n_traders)

	traders_df <- data.frame(1 : n_traders, capital_v, tags_m)  
	
	gs_shrs_outs <- 2000 
	bac_shrs_outs <- 2000 
	swks_shrs_outs <- 2000 
	goos_shrs_outs <- 2000 
	ups_shrs_outs <- 2000 
	jpm_shrs_outs <- 2000 
	
	shrs_outs_v <- cbind(gs_shrs_outs, bac_shrs_outs, swks_shrs_outs, goos_shrs_outs, ups_shrs_outs, jpm_shrs_outs)

	gs_health <- list(p_vneg = 0.1, p_neg = 0.3, p_pos = 0.4, p_vpos = 0.2)
	bac_health <- list(p_vneg = 0.1, p_neg = 0.2, p_pos = 0.4, p_vpos = 0.3)	
	swks_health <- list(p_vneg = 0.05, p_neg = 0.2, p_pos = 0.5, p_vpos = 0.25)
	goos_health <- list(p_vneg = 0.2, p_neg = 0.3, p_pos = 0.4, p_vpos = 0.1)
	ups_health <- list(p_vneg = 0.05, p_neg = 0.5, p_pos = 0.3, p_vpos = 0.15) 
	jpm_health <- list(p_vneg = 0.05, p_neg = 0.2, p_pos = 0.3, p_vpos = 0.45)
	
	comp_healths_l <- list(gs = gs_health, bac = bac_health, swks = swks_health, goos = goos_health, ups = ups_health, jpm = jpm_health)	

	q_results_l <- vector("list", length(tickers_v))
	
	for (k in 2 : n_steps) {

		## Issue quarterly reports
		if (time == Q1 || time == Q2 || time == Q3) {
			q_results_l <- lapply(comp_healths_l, FUN = function(x) issue_earnings_report(p_vneg = x$p_vneg, p_neg = x$p_neg, 
												      p_pos = x$p_pos, p_vpos = x$p_vpos))
		print(q_results_l)
		}

		## Calculate supply and demand	
		supply_v <- apply(shrs_outs_v, 2, FUN = function(x) runif(n = 1, min = 0, max = x))
		demand_v <- runif(n = 6, min = 0, max = n_traders)

		if (!is.null(q_results_l[[1]])) {
			for (j in 1 : length(q_results_l)) {
				if (q_results_l[[j]] == "VNEG") {
					demand_v[j] <- demand_v[j] - 0.20 * demand_v[j]	
				} else if (q_results_l[[j]] == "NEG") {
					demand_v[j] <- demand_v[j] - 0.10 * demand_v[j]
				} else if (q_results_l[[j]] == "POS") {
					demand_v[j] <- demand_v[j] + 0.10 * demand_v[j]
				} else if (q_results_l[[j]] == "VPOS") {
					demand_v[j] <- demand_v[j] + 0.20 * demand_v[j]
				}
			}
		}

		demsupp_diff_v <- demand_v - supply_v 
		
		for (i in 1 : length(tickers_v)) {
			pdata_l[[i]][k] <- ifelse(pdata_l[[i]][k-1] <= 0, 0, 
						  pdata_l[[i]][k - 1] + TICK_SIZE * demsupp_diff_v[i] + rnorm(1, mean = 0, sd = 0.002))
		}
		
		time <- time + 1
	}

	display_stock_charts(num_stocks = 6, tickers_v, pdata_l, time)

	return(invisible(traders_df))
}
