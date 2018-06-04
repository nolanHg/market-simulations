################################
## 	   LIBRARIES          ##
################################
library("ggplot2")
library("gridExtra")
library("rmutil")
library("rvest")


################################
##         CONSTANTS          ##
################################
TICK_SIZE <- 0.001


################################
##	   FUNCTIONS          ##
################################


##############################################################################
## BRIEF: Gets actual price of a stock from the Internet.                   ##
## PARAM ticker: Stock exchange code for stock of interest                  ##
##	         (e.g., BAC is the ticker for Bank of America Corporation). ##
## RETURNS: Current stock price as a numeric type.                          ##
##############################################################################
get_current_price <- function(ticker)
{
	## Download HTML data from stock's Yahoo finance page
	url <- paste("https://finance.yahoo.com/quote/", ticker, "?p=", ticker, sep = "")
	html_data <- read_html(url)
	
	## Extract price data from Yahoo finance page
	div_span_elems <- html_data %>% 
		          html_nodes('div span') %>%
		          html_text()	
	current_price <- as.numeric(div_span_elems[11])
	
	## Indicate that price data for current stock has been extracted
	print(paste("--> ", ticker, " price download complete: ", "$", div_span_elems[11], " per share", sep = ""))
	
	## Return current price to caller
	return(invisible(current_price))
}


###############################################################################
## BRIEF: Distributes money supply in economy according to Pareto            ##
##	  distribution.							     ##
## PARAM step_size: Size of movement along Pareto CDF.	         	     ##
## PARAM supply: Total amount of money to distribute amongst the traders.    ##
## PARAM n_traders: Number of traders to distribute money to.	             ##
## PARAM loc: Location parameter of Pareto distribution.		     ##
## PARAM disp: Dispersion parameter of Pareto distribution.		     ##
## RETURNS: Vector containing the amount of money each trader has been       ##
##          allotted.							     ##
###############################################################################
distribute_wealth_pareto <- function(step_size = 0.1, supply = 1e12, n_traders = 500, loc = 1, disp = 1.5)
{
	## Allocate memory for vector
	wealth_v <- rep(NA, n_traders)
	
	wealth_v[1] <- ppareto(step_size, loc, disp) * supply
	wealth_v[2 : n_traders] <- supply * (ppareto(seq(from = 2 * step_size, by = step_size, length = n_traders - 1), loc, disp) -
					     ppareto(seq(from = step_size, by = step_size, length = n_traders - 1), loc, disp))	

	return(invisible(wealth_v))
}


##
## FUNCTION: distributes shares of stock according to Pareto distribution
##
distribute_shares_pareto <- function(step_size = 0.1, stock_names_v, n_shares_v, n_traders = 500, loc = 1, disp = 3)
{
	## Allocate memory for data-frame
	ownership_df <- data.frame(matrix(NA, nrow = n_traders, ncol = length(stock_names_v)))
	colnames(ownership_df) <- stock_names_v

	for (k in 1 : length(stock_names_v)) {	
		ownership_v <- rep(NA, n_traders)
		ownership_v[1] <- ppareto(step_size, loc, disp) * n_shares_v[k]
		ownership_v[2 : n_traders] <- n_shares_v[k] * (ppareto(seq(from = 2 * step_size, by = step_size, length = n_traders - 1), loc, disp) -
				                  ppareto(seq(from = step_size, by = step_size, length = n_traders - 1), loc, disp))
		ownership_df[stock_names_v[k]] <- ownership_v
	}

	return(invisible(ownership_df))
}


##
## FUNCTION: Displays historical stock charts after completion of simulation
##
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


##
## FUNCTION: Runs simulation of the stock market
##
market_sim <- function(n_steps = 500, n_hft_traders = 1000, n_fund_traders = 1000,
	               n_mom_traders = 1000, n_rand_traders = 1000)
{
	Q1 <- floor(n_steps / 4)
	Q2 <- floor(2 * (n_steps / 4))
	Q3 <- floor(3 * (n_steps / 4))
		
	time <- 0
	
	############################################
	## 		Set-up stocks             ##
        ##		    and                   ##
	##               companies                ##
	############################################
	tickers_v <- c("GS", "BAC", "SWKS", "GOOS", "UPS", "JPM")

	gs_hist <- rep(NA, n_steps)
	bac_hist <- rep(NA, n_steps)
	swks_hist <- rep(NA, n_steps)
	goos_hist <- rep(NA, n_steps)
	ups_hist <- rep(NA, n_steps)
	jpm_hist <- rep(NA, n_steps)
	
	print("Downloading current stock prices ...")
	gs_hist[1] <- get_current_price("GS")
	bac_hist[1] <- get_current_price("BAC") 
	swks_hist[1] <- get_current_price("SWKS") 
	goos_hist[1] <- get_current_price("GOOS") 
	ups_hist[1] <- get_current_price("UPS") 
	jpm_hist[1] <- get_current_price("JPM") 
	print("All downloads complete. Running simulation ...")

	pdata_l <- list(gs_hist, bac_hist, swks_hist, goos_hist, ups_hist, jpm_hist)

	gs_health <- list(p_vneg = 0.1, p_neg = 0.3, p_pos = 0.4, p_vpos = 0.2)
	bac_health <- list(p_vneg = 0.1, p_neg = 0.2, p_pos = 0.4, p_vpos = 0.3)	
	swks_health <- list(p_vneg = 0.05, p_neg = 0.2, p_pos = 0.5, p_vpos = 0.25)
	goos_health <- list(p_vneg = 0.2, p_neg = 0.3, p_pos = 0.4, p_vpos = 0.1)
	ups_health <- list(p_vneg = 0.05, p_neg = 0.5, p_pos = 0.3, p_vpos = 0.15) 
	jpm_health <- list(p_vneg = 0.05, p_neg = 0.2, p_pos = 0.3, p_vpos = 0.45)

	comp_healths_l <- list(gs = gs_health, bac = bac_health, swks = swks_health, goos = goos_health, ups = ups_health, jpm = jpm_health)	
	
	## Number of shares outstanding for each company
	gs_shrs_outs <- 4000 
	bac_shrs_outs <- 4000 
	swks_shrs_outs <- 4000 
	goos_shrs_outs <- 4000 
	ups_shrs_outs <- 4000 
	jpm_shrs_outs <- 4000 
	
	shrs_outs_v <- cbind(gs_shrs_outs, bac_shrs_outs, swks_shrs_outs, goos_shrs_outs, ups_shrs_outs, jpm_shrs_outs)

	#############################################
	## 		Set-up traders             ##
	#############################################
	n_traders <- n_hft_traders + n_fund_traders + n_mom_traders + n_rand_traders
	tags_m <- sample(c(rep("HFT", n_hft_traders), rep("FUND", n_fund_traders),
		        rep("MOM", n_mom_traders), rep("RAND", n_rand_traders)))
	capital_v <- distribute_wealth_pareto(n_traders = n_traders) 
	ownership_df <- distribute_shares_pareto(n_traders = n_traders, stock_names_v = tickers_v, n_shares_v = shrs_outs_v)
	traders_df <- data.frame(1 : n_traders, capital_v, tags_m, ownership_df)  
	colnames(traders_df) <- c("ID", "CAPITAL", "STRATEGY", tickers_v)
	
	#############################################
	##          Run market simulation	   ##
	#############################################
	q_results_l <- vector("list", length(tickers_v))
	
	for (k in 2 : n_steps) {

		## Issue quarterly reports
		if (time == Q1 || time == Q2 || time == Q3) {
			q_results_l <- lapply(comp_healths_l, FUN = function(x) issue_earnings_report(p_vneg = x$p_vneg, p_neg = x$p_neg, 
												      p_pos = x$p_pos, p_vpos = x$p_vpos))
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
