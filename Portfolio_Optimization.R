library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(corpcor)
library(pso)
library(GenSA)

################################################################################
# ASSUMPTIONS
################################################################################

set.seed(1) #Sets randomization to be constant, allowing for reproduction of results

start_date = as.Date('2018-01-01')
end_date = as.Date('2023-05-02')
riskFree = 0.045 / 12 #Treasury yield on a monthly basis - 5/2/2023
target_stdev = NULL #Set to NULL if no target

n_rand_portfolios = 5000
training_periods = 1
rolling_periods = 4
price_freq = 'monthly'

w_Materials = .0235
w_ConsumerCyclical = 0.1016
w_FinancialServices = 0.1216
w_RealEstate = 0.0263
w_ConsumerDefensive = 0.0731
w_Healthcare = 0.1447
w_Utilities = 0.0287
w_CommServices = 0.0827
w_Energy = 0.0486
w_Industrials = 0.0819
w_Technology = 0.2639

################################################################################
# PORTFOLIO RETURNS
################################################################################

# This section reads in stocks either from a CSV, or pre-defined stocks from 
# the user. Prices for these stocks are then pulled from Yahoo, and returns
# over the historical period calculated. 

fundData = read.csv(file.choose(new = F), header = T)
assets = fundData$ticker

#Read in CLOSE stock prices from Yahoo
prices = NULL
for (asset in assets){
  prices = cbind(prices, getSymbols.yahoo(asset, 
                                          from = start_date, 
                                          to = end_date, 
                                          periodicity = price_freq, 
                                          auto.assign = F)[,4])
}

#Remove any stocks that do not have data for the full period
remove_NA_cols = function(df){
  df[, colSums(is.na(df)) <= 1, drop = F]
}
prices = remove_NA_cols(prices)

#Calculate returns for each stock, and remove first row NA's
returns = na.omit(ROC(prices))

#Clean up column titles
colnames(returns) = assets

### Determine the index of stocks belonging to each sector in the S&P 500 
sector_names = c('Materials', 'Consumer Cyclical', 'Financial Services', 'Real Estate',
                 'Consumer Defensive', 'Healthcare', 'Utilities', 'Communication Services',
                 'Energy', 'Industrials', 'Technology')
sector_weights = c(w_Materials, w_ConsumerCyclical, w_FinancialServices, w_RealEstate,
                   w_ConsumerDefensive, w_Healthcare, w_Utilities, w_CommServices,
                   w_Energy, w_Industrials, w_Technology)
sector_df = as.data.frame(cbind(sector_names, sector_weights))
colnames(sector_df) = c('Sector', 'Weight')

################################################################################
# SET PORTFOLIO SPECS
################################################################################

# Using the PortfolioAnalytics package, any portfolio objectives and constraints
# can be defined here.

port_spec = portfolio.spec(colnames(returns))
port_spec = add.constraint(portfolio = port_spec, type = 'weight_sum', min_sum = .99, max_sum = 1.01)
port_spec = add.constraint(portfolio = port_spec, type = 'box', min = .01, max = .25)
port_spec = add.constraint(portfolio = port_spec, type = 'transaction_cost', ptc = 0.01)
port_spec = add.objective(portfolio = port_spec, type = 'return', name = 'mean')
port_spec = add.objective(portfolio = port_spec, type = 'risk', name = 'StdDev', target = target_stdev)
# Impose sector constraints
for (sector in sector_df$Sector){
  port_spec = add.constraint(port_spec,
                             type = 'group',
                             groups = list(which(fundData$sector == sector)),
                             group_min = as.numeric(sector_df$Weight[which(sector_df$Sector == sector)]) * 0.5,
                             group_max = as.numeric(sector_df$Weight[which(sector_df$Sector == sector)]) * 1.5)
}


################################################################################
# SINGLE PERIOD PORTFOLIO OPTIMIZATIONS
################################################################################

# Portfolios are optimized on a single period basis, using different global solvers.

# RANDOM #
rp = random_portfolios(port_spec, n_rand_portfolios, "sample") #Create random portfolios for optimizer

opt_random = optimize.portfolio(returns,
                                portfolio = port_spec,
                                optimize_method = 'random',
                                rp = rp,
                                trace = TRUE)
opt_random #print results

# PSO (Particle Swarm Optimization) # - iterates until max number of optimizations reached
opt_pso = optimize.portfolio(returns,
                             portfolio = port_spec,
                             optimize_method = 'pso',
                             trace = TRUE)
opt_pso #print results

# # GenSA (Generalized Simulated Annealing) #
# opt_gensa = optimize.portfolio(returns,
#                                portfolio = port_spec,
#                                optimize_method = 'GenSA',
#                                trace = TRUE)
# opt_gensa #print results

################################################################################
# PORTFOLIO BACKTESTING
################################################################################

# Portfolios are backtested over the historical period for performance evaluation
# Sit back, relax, and read a book or watch some TV, since this'll take awhile

opt_random_rebal = optimize.portfolio.rebalancing(returns, 
                                                  portfolio = port_spec,
                                                  optimize_method = 'random',
                                                  rp = rp,
                                                  search_size = 20000,
                                                  rebalance_on = 'quarters',
                                                  training_period = training_periods,
                                                  rolling_window = rolling_periods) #rebalance once every 6 months
opt_random_rebal

chart.Weights(opt_random_rebal, main = "Rebalanced Weights Over Time - Random")
w_opt_random_rebal = extractWeights(opt_random_rebal)
r_opt_random_rebal = Return.portfolio(returns, weights = w_opt_random_rebal)
colnames(r_opt_random_rebal) = "Random_Opt"


opt_pso_rebal = optimize.portfolio.rebalancing(returns,
                                               portfolio = port_spec,
                                               optimize_method = 'pso',
                                               search_size = 20000,
                                               rebalance_on = 'quarters',
                                               training_period = training_periods,
                                               rolling_window = rolling_periods)
opt_pso_rebal

chart.Weights(opt_pso_rebal, main = "Rebalanced Weights Over Time - PSO")
w_opt_pso_rebal = extractWeights(opt_pso_rebal)
r_opt_pso_rebal = Return.portfolio(returns, weights = w_opt_pso_rebal)
colnames(r_opt_pso_rebal) = "PSO"

# opt_gensa_rebal = optimize.portfolio.rebalancing(returns,
#                                                  portfolio = port_spec,
#                                                  optimize_method = 'GenSA',
#                                                  search_size = 20000,
#                                                  rebalance_on = 'quarters',
#                                                  training_period = training_periods,
#                                                  rolling_window = rolling_periods)
# opt_gensa_rebal
# 
# chart.Weights(opt_gensa_rebal, main = "Rebalanced Weights Over Time - GenSA")
# w_opt_gensa_rebal = extractWeights(opt_gensa_rebal)
# r_opt_gensa_rebal = Return.portfolio(returns, weights = w_opt_gensa_rebal)
# colnames(r_opt_gensa_rebal) = "GenSA"

################################################################################
# BENCHMARK RETURNS
################################################################################

equal_weight = rep(1 / ncol(returns), ncol(returns))
r_benchmark = Return.portfolio(returns, weights = equal_weight)
colnames(r_benchmark) = 'Equal Weight Portfolio'

sp500prices = getSymbols.yahoo('^GSPC', from = start_date, periodicity = price_freq, auto.assign = FALSE)[,4]
sp500returns = na.omit(ROC(sp500prices))
colnames(sp500returns) = 'S&P500'

VBIAXprices = getSymbols.yahoo('VBIAX', from = start_date, periodicity = price_freq, auto.assign = FALSE)[,4]
VBIAXreturns = na.omit(ROC(VBIAXprices))
colnames(VBIAXreturns) = 'VBIAX'

################################################################################
# CUMULATIVE RETURNS OVER HISTORICAL PERIOD
################################################################################

returns_df = cbind(r_opt_random_rebal, r_opt_pso_rebal, r_benchmark, sp500returns, VBIAXreturns)
#returns_df = cbind(r_opt_random_rebal, r_opt_pso_rebal, r_opt_gensa_rebal, r_benchmark, sp500returns)

charts.PerformanceSummary(returns_df, main = 'Portfolio Backtest')

#table.AnnualizedReturns(R = cbind(r_opt_random_rebal, r_opt_pso_rebal, r_opt_gensa_rebal, r_benchmark, sp500returns), Rf = riskFree)
table.AnnualizedReturns(R = cbind(r_opt_random_rebal, r_opt_pso_rebal, r_benchmark, sp500returns, VBIAXreturns), Rf = riskFree)
