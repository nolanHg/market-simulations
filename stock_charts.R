library("ggplot2")
library("gridExtra")

display_stock_charts <- function(num_stocks = 6, tickers_v, pdata_l, t)
{
	col_det <- lapply(pdata_l, FUN = function(x_v) x_v[length(x_v)] - x_v[1])		
	colors <- ifelse(col_det < 0, "red", "#119334")
	print(colors)
	plots <- mapply(pdata_l, as.list(tickers_v), colors,
		        FUN = function(x_v, y, z) qplot(0:t, x_v, geom = "line", xlab = "Time", ylab = "Price", main = y, color = factor(z)) + 
			guides(colour = "none") + geom_line(color = z),
		        SIMPLIFY = FALSE)	
	grid.arrange(grobs = plots, nrow = num_stocks / 2)
}

market_sim <- function(n_steps = 500, n_hft_traders = 500, n_fund_traders = 500,
	               n_mom_traders = 500, n_rand_traders = 500)
{
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

	t <- 0 

	stocks_df <- data.frame(t, gs_hist, bac_hist, swks_hist, goos_hist, ups_hist, jpm_hist)
	colnames(stocks_df) <- c("time", "gs", "bac", "swks", "goos", "ups", "jpm")
	
	## keep track of trader IDs, stock ownership, profits/losses in dataframe	
	## wealth should be distributed according to pareto distribution to make
	## simulation more realistic
	tags_m <- cbind(rep("HFT", n_hft_traders), rep("FUND", n_fund_traders),
		        rep("MOM", n_mom_traders), rep("RAND", n_rand_traders))
	traders_df <- data.frame(1 : n_traders, tags_m)  

	Q1 <- 
	
	for (k in 2 : 500) {
		
		## on every time step, HFT traders execute trading strategy
		## other trading strategies require multiple time-steps to execute	
			
		for (i in 1 : length(tickers_v)) {
			pdata_l[[i]][k] <- pdata_l[[i]][k - 1] + runif(n = 1, min = -1, max = 1)			
		}
		
		t <- t + 1
	}

	display_stock_charts(num_stocks = 6, tickers_v, pdata_l, t)

	return(invisible(traders_df))
}
