################################
## 	   LIBRARIES          ##
################################
library("ggplot2")
library("gridExtra")
library("rmutil")
library("rvest")
library("lemon")


################################
##         CONSTANTS          ##
################################
TICK_SIZE <- 0.001
TICKERS_V <- cbind("GS", "BAC", "SWKS", "GOOS", "UPS", "JPM")
NUM_STOCKS <- length(TICKERS_V)


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
distribute_wealth_pareto <- function(step_size = 0.1, supply = 1e12, n_traders = 500, loc = 1, disp = 1.5) {
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
## BRIEF: Displays historical stock charts after completion of simulation.  ##
## PARAM stocks_df: Dataframe of historical stock price data.		    ##
## RETURNS: Void.							    ##
##############################################################################
display_stock_charts <- function(stocks_df)
{
	n_stocks <- ncol(stocks_df) - 1
	q_step <- floor(nrow(stocks_df) / 4)
	q_x <- seq(from = q_step, by = q_step, length = 3)
	col_det <- apply(stocks_df[ , 1 : n_stocks], MARGIN = 2, FUN = function(x_v) x_v[length(x_v)] - x_v[1])		
	colors <- ifelse(col_det < 0, "#FF0000", "#119334")
	
	tickers_l <- as.list(colnames(stocks_df[ , 1 : n_stocks]))

	plots <- mapply(tickers_l, colors, FUN = function(u, v)  
			ggplot(stocks_df, aes(x = Time)) + 
			geom_line(aes_string(y = u), colour = v),
			#geom_point(aes(x = 2, y = 10), color = "black", fill = "blue", alpha = 0.4, size = 3, stroke = 1.5, shape = 21),
			SIMPLIFY = FALSE)
	
	grid.arrange(grobs = plots,  nrow = length(colnames(stocks_df)) / 2)
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
market_sim <- function(n_steps = 400, 
		       n_hft_traders = 1000, 
		       n_fund_traders = 1000,
	               n_mom_traders = 1000, mavg_l = 20,
		       n_rand_traders = 1000)
{
	## Times at which quarterly reports will be issued by the companies
	Q1 <- floor(n_steps / 4)
	Q2 <- floor(2 * (n_steps / 4))
	Q3 <- floor(3 * (n_steps / 4))
		
	############################################
	## 		Set-up stocks             ##
        ##		    and                   ##
	##               companies                ##
	############################################
	
	## Create a dataframe to store historical stock price data
	stocks_df <- data.frame(matrix(NA, nrow = n_steps, ncol = length(TICKERS_V)))
	colnames(stocks_df) <- TICKERS_V 
	
	## Download stock prices from Yahoo! finance.
	print("Downloading current stock prices ...")
	stocks_df[1, ] <- apply(TICKERS_V, MARGIN = 2, FUN = function(t) get_current_price(t))
	print("All downloads complete. Running simulation ...")
	
	## Set financial health of each company
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
	ownership_df <- distribute_shares_pareto(n_traders = n_traders, stock_names_v = TICKERS_V, n_shares_v = shrs_outs_v)
	traders_df <- data.frame(1 : n_traders, capital_v, tags_m, ownership_df)  
	colnames(traders_df) <- c("ID", "CAPITAL", "STRATEGY", TICKERS_V)
	
	mom_df <- subset(traders_df, STRATEGY == "MOM")
	fund_df <- subset(traders_df, STRATEGY == "FUND")
	hft_df <- subset(traders_df, STRATEGY == "HFT")
	rand_df <- subset(traders_df, STRATEGY == "RAND") 
	
	#############################################
	##          Run market simulation	   ##
	#############################################
	q_results_l <- vector("list", length(TICKERS_V))
	
	## Sum used to compute price's moving average
	sum_last_obs_v <- rep(0, NUM_STOCKS)
	sum_last_obs_v <- as.numeric(stocks_df[1,])
	
	demand_v <- rep(0, NUM_STOCKS)
	supply_v <- rep(0, NUM_STOCKS)

	for (t in 1 : n_steps) {
		
		current_prices_v <- as.numeric(stocks_df[t, ])
		
		##################################
		##  Execute trading strategies  ##
		##################################
	
		#############################################################
		## Random supply and demand needed to set market in motion ##
		#############################################################

		supply_v <- apply(shrs_outs_v, MARGIN = 2, FUN = function(x) runif(n = 1, min = 0, max = x))
		demand_v <- runif(n = NUM_STOCKS, min = 0, max = n_traders)
	
		##############
		## MOMENTUM ##
		##############
		
		## Calculate n-step moving average for each stock
		sum_last_obs_v <- ifelse(rep(t, NUM_STOCKS) <= mavg_l, sum_last_obs_v + as.numeric(stocks_df[t, ]),
					        sum_last_obs_v + as.numeric(stocks_df[t, ]) - as.numeric(stocks_df[t - mavg_l, ]))
		
		mavg_v <- sum_last_obs_v / mavg_l 
		## If price exceeds 20-step moving average by 2%, buy 100 shares
		## else if price is below 20-step moving average by 2%, sell 100 shares
		## else hold 
		if (t %% 20 == 0) {
			for (j in 1 : NUM_STOCKS) {
				if (current_prices_v[j] >= mavg_v[j] * 1.02) {

					## MORE LIKELY TO BUY STOCKS	
					for (k in 1 : n_mom_traders) {

						if (mom_df["CAPITAL"][k,] >= 100 * current_prices_v[j]) {

							decision <- sample(c("BUY", "SELL", "HOLD"), 1, prob = c(0.6, 0.3, 0.1))

							if (decision == "BUY") {

								demand_v[j] <- demand_v[j] + 10
								supply_v[j] <- supply_v[j] - 10
								mom_df["CAPITAL"][k,] <- mom_df["CAPITAL"][k,] - 100 * current_prices_v[j]
								mom_df[TICKERS_V[j]][k,] <- mom_df[TICKERS_V[j]][k,] + 100

							} else if (decision == "SELL") {

								demand_v[j] <- demand_v[j] - 10
								supply_v[j] <- supply_v[j] + 10
								mom_df["CAPITAL"][k,] <- mom_df["CAPITAL"][k,] + 100 * current_prices_v[j]
								mom_df[TICKERS_V[j]][k,] <- mom_df[TICKERS_V[j]][k,] - 100

							}
						}
					}
				} else if (current_prices_v[j] <= mavg_v[j] * 0.98) {

					## MORE LIKELY TO SELL STOCKS 
					for (k in 1 : n_mom_traders) {

						if (mom_df[TICKERS_V[j]][k,] >= 100) {
							
							decision <- sample(c("BUY", "SELL", "HOLD"), 1, prob = c(0.3, 0.6, 0.1))
							
							if (decision == "SELL") {

								demand_v[j] <- demand_v[j] - 10
								supply_v[j] <- supply_v[j] + 10
								mom_df["CAPITAL"][k,] <- mom_df["CAPITAL"][k,] + 100 * current_prices_v[j]
								mom_df[TICKERS_V[j]][k,] <- mom_df[TICKERS_V[j]][k,] - 100

							} else if (decision == "BUY") {

								demand_v[j] <- demand_v[j] + 10
								supply_v[j] <- supply_v[j] - 10
								mom_df["CAPITAL"][k,] <- mom_df["CAPITAL"][k,] - 100 * current_prices_v[j]
								mom_df[TICKERS_V[j]][k,] <- mom_df[TICKERS_V[j]][k,] + 100

							}
						}
					}
				}
			
			} 
		}

		#########
		## HFT ##
		#########

	
		#################
		## FUNDAMENTAL ##
		#################

		if (t == Q1 || t == Q2 || t == Q3) {

			q_results_l <- lapply(comp_healths_l, FUN = function(x) issue_earnings_report(p_vneg = x$p_vneg, p_neg = x$p_neg, 
												      p_pos = x$p_pos, p_vpos = x$p_vpos))
			## Fundamental traders make trades based on quarterly results	
			for (j in 1 : length(q_results_l)) {
				if (q_results_l[[j]] == "VBAD") {

					prob_buy <- 0.05
					prob_sell <- 0.9
					prob_hold <- 0.05

				} else if (q_results_l[[j]] == "BAD") {
					prob_buy <- 0.1
					prob_sell <- 0.8
					prob_hold <- 0.1
	
				} else if (q_results_l[[j]] == "GOOD") {
				
					prob_buy <- 0.8
					prob_sell <- 0.1
					prob_hold <- 0.1

				} else {
					
					prob_buy <- 0.9
					prob_sell <- 0.05
					prob_hold <- 0.05

				}
				
				
				for (k in 1 : n_fund_traders) {
					decision <- sample(c("BUY", "SELL", "HOLD"), 1, prob = c(prob_buy, prob_sell, prob_hold))	
					
					if (decision == "BUY") {
						if (fund_df["CAPITAL"][k,] >= 100 * current_prices_v[j]) {
							demand_v[j] <- demand_v[j] + 10 
							supply_v[j] <- supply_v[j] - 10	
							fund_df["CAPITAL"][k,] <- fund_df["CAPITAL"][k,] - 100 * current_prices_v[j]
							fund_df[TICKERS_V[j]][k,] <- fund_df[TICKERS_V[j]][k,] + 100 
						}
					} else if (decision == "SELL") {
						if (fund_df[TICKERS_V[j]][k,] >= 100) {
							demand_v[j] <- demand_v[j] - 10 
							supply_v[j] <- supply_v[j] + 10	
							fund_df["CAPITAL"][k,] <- fund_df["CAPITAL"][k,] + 100 * current_prices_v[j]
							fund_df[TICKERS_V[j]][k,] <- fund_df[TICKERS_V[j]][k,] - 100 
						}
					}
				}
			}
		}
		
		###########################################################
		## Calculate new stock prices based on supply and demand ##
		###########################################################
		demsupp_diff_v <- demand_v - supply_v 
		stocks_df[t + 1, ] <- mapply(stocks_df[t, ], demsupp_diff_v, FUN = function(x, y) 
					     ifelse(x <= 0, 0, x + TICK_SIZE * y + rnorm(1, mean = 0, sd = 0.002))) 

	}

	stocks_df["Time"] <- 0 : (n_steps)
	display_stock_charts(stocks_df)
	
	return(invisible(traders_df))
}
