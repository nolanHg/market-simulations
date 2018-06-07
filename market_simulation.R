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

exec_momentum_strategy <- function(mom_df, prev_obs, curr_obs)
{
	if (curr_obs > prev_obs) {
		print("BUY")
	} else {
		print("SELL")
	}
}


exec_fundamental_strategy <- function(fund_df)
{

}


exec_hft_strategy <- function(hft_df)
{

}


exec_rand_strategy <- function(rand_df)
{

}

calc_stock_prices <- function(tickers_v, supply_v, demand_v, prev_prices_v)
{

}

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


##############################################################################
## BRIEF: Distributes money supply in economy according to Pareto           ##
##	  distribution.							    ##
## PARAM step_size: Size of movements along Pareto CDF.	         	    ##
## PARAM supply: Total amount of money to distribute amongst the traders.   ##
## PARAM n_traders: Number of traders to distribute money to.	            ##
## PARAM loc: Location parameter of Pareto distribution.		    ##
## PARAM disp: Dispersion parameter of Pareto distribution.		    ##
## RETURNS: Vector containing the amount of money each trader has been      ##
##          allotted.							    ##
##############################################################################
distribute_wealth_pareto <- function(step_size = 0.1, supply = 1e12, n_traders = 500, loc = 1, disp = 1.5)
{
	## Allocate memory for vector
	wealth_v <- rep(NA, n_traders)
	
	wealth_v[1] <- ppareto(step_size, loc, disp) * supply
	wealth_v[2 : n_traders] <- supply * (ppareto(seq(from = 2 * step_size, by = step_size, length = n_traders - 1), loc, disp) -
					     ppareto(seq(from = step_size, by = step_size, length = n_traders - 1), loc, disp))	

	return(invisible(wealth_v))
}


##############################################################################
## BRIEF: Distributes shares of stock according to Pareto distribution.     ##
## PARAM step_size: Size of movements along Pareto CDF.      	            ##
## PARAM stock_names_v: Vector of stock tickers. 			    ##
## PARAM n_shares_v: Number of shares outstanding for each stock.	    ##
## PARAM n_traders: Number of traders to distribute the shares to.          ##
## PARAM loc: Location parameter of Pareto distribution. 		    ##
## PARAM disp: Dispersion parameter of Pareto distribution.		    ##
## RETURNS: Data-frame containing share ownership information. 		    ##
##############################################################################
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


##############################################################################
## BRIEF: Displays historical stock charts after completion of simulation   ##
## PARAM num_stocks: Number of stocks whose charts will be displayed.	    ##
## PARAM tickers_v: Vector of stock tickers (used for titles in the graphs).##
## PARAM pdata_l: List containing vectors of historical price data 	    ##
##		  generated by the market simulation.			    ##
## PARAM t: Price data is graphed from time 0 to time t.		    ##
## RETURNS: Void.							    ##
##############################################################################
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


##############################################################################
## BRIEF: Runs simulation of the stock market.		        	    ##
## PARAM n_steps: Number time steps to run the simulation for.		    ##
## PARAM n_hft_traders: Number of traders using a high-frequency	    ##
##	                trading strategy.				    ##
## PARAM n_fund_traders: Number traders who use fundamental analysis	    ##
##		         to make trading decisions.			    ##
## PARAM n_mom_traders: Number of traders who make trading decisions	    ##
##		        based on upward and downward price trends.	    ##
## PARAM n_rand_traders: Number of traders who make trading decisions 	    ##
##			 randomly.	                                    ##
## RETURNS: Data-frame containing stock-ownership and net-worth data        ##
##	    about each trader in the simulation.			    ##
##############################################################################
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
	tickers_v <- cbind("GS", "BAC", "SWKS", "GOOS", "UPS", "JPM")
	
	stocks_df <- data.frame(matrix(NA, nrow = n_steps, ncol = length(tickers_v)))
	colnames(stocks_df) <- tickers_v

	print("Downloading current stock prices ...")
	stocks_df[1, ] <- apply(tickers_v, MARGIN = 1, FUN = function(t) get_current_price(t))
	gs_hist[1] <- get_current_price("GS")
	bac_hist[1] <- get_current_price("BAC") 
	swks_hist[1] <- get_current_price("SWKS") 
	goos_hist[1] <- get_current_price("GOOS") 
	ups_hist[1] <- get_current_price("UPS") 
	jpm_hist[1] <- get_current_price("JPM") 
	print("All downloads complete. Running simulation ...")
		
	moving_avg_mem_gs <- list(oldest = gs_hist[1], prev_sum = gs_hist[1])

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
	
	mom_df <- subset(traders_df, STRATEGY == "MOM")
	fund_df <- subset(traders_df, STRATEGY == "FUND")
	hft_df <- subset(traders_df, STRATEGY == "HFT")
	rand_df <- subset(traders_df, STRATEGY == "RAND") 
	
	#############################################
	##          Run market simulation	   ##
	#############################################
	q_results_l <- vector("list", length(tickers_v))
	
	for (k in 1 : n_steps) {
		
		###############################
		##  Issue quarterly reports  ##
		###############################
		if (time == Q1 || time == Q2 || time == Q3) {
			q_results_l <- lapply(comp_healths_l, FUN = function(x) issue_earnings_report(p_vneg = x$p_vneg, p_neg = x$p_neg, 
												      p_pos = x$p_pos, p_vpos = x$p_vpos))
		}
		
		####################################
		##  Calculate supply and demand   ##
		####################################
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
			pdata_l[[i]][k + 1] <- ifelse(pdata_l[[i]][k] <= 0, 0, 
						      pdata_l[[i]][k] + TICK_SIZE * demsupp_diff_v[i] + rnorm(1, mean = 0, sd = 0.002))
		}

		##################################
		##  Execute trading strategies  ##
		##################################
		
		## Make moving average observation every 50 time-steps

		moving_avg_mem_gs$oldest <- ifelse(time <= 50, 0, pdata_l[[1]][time - 50])
		moving_avg_mem_gs$prev_sum <- ifelse(time <= 50, moving_avg_mem_gs$prev_sum + pdata_l[[1]][k + 1],
						     moving_avg_mem_gs$prev_sum + pdata_l[[1]][k + 1] - moving_avg_mem_gs$oldest)

		moving_avg_gs <- ifelse(k < 50, NA, moving_avg_mem_gs$prev_sum / 50)

		if (k %% 50 == 0 && k >= 100) {
			print(moving_avg_obs_gs)
			print(moving_avg_gs)
			exec_momentum_strategy(mom_df, moving_avg_obs_gs, moving_avg_gs)
		}

		if (k %% 50 == 0) {
			moving_avg_obs_gs <- moving_avg_gs	
		}

		time <- time + 1
	}

	display_stock_charts(num_stocks = 6, tickers_v, pdata_l, time)

	return(invisible(traders_df))
}
