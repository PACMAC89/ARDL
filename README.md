# ARDL
#Autoregressive Distributed Lags Approach long- and short run relationships between the U.S. stock market and selected Macroeconomic Variables

#I applied the ARDL model and cointegration technique to analyse the following hypotheses
#- negative relationship between the S&P 500 and inflation. 
#- negative effect of the expected 10-year real interest rate on the S&P 500 index.
#- positive effect of money aggregate M2 on the S&P 500 index. 
#- positive effect of the industrial production on the S&P 500 index. 
#- negative effect of the WTI crude oil price on the S&P 500 index. 

#The following aspects were analyzed: 
#- Unit root test
#- ARDL Cointegration tests
#- Short- and long-run relationships
#- Error Correction - cointegration trajectories
#- Granger-causality using F-statistics
#- Diagnostic and stability test results
#-Impulse Response Functions


#R Code
install.packages("tsDyn")
library(tsDyn)
library(vars)
library(readr)
library(tsibble)
library(dplyr)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
#für VECM
library(urca)  # ca.jo, ur.df, finland
library(vars)  # vec2var
library(tsDyn) # VECM
library(tseries)
library(readr)
library(readxl)

#Loading Data

##SNP returns
SNP500_Return <- read_excel("SNP500_Return.xlsx", 
                            col_types = c("skip", "skip", "numeric"))

##GDP_GROWTH_RATE
GDP_growth_rate <- read_excel("RGDP_growth_rate.xls", 
                               col_types = c("numeric"))
GDP2 <- td(GDP ~ 1, to= "monthly", method = "denton-cholette")

##INFLATION
PCE_rates_M_csv <- read_excel("PCE_rates_M.csv.xls", 
                              col_types = c("date", "numeric"))
##INTEREST_RATE
RInterestRate_Effectice_federal_fund_Rate_csv <- read_excel("RInterestRate_Effectice_federal_fund-Rate.csv.xls", 
                                                            col_types = c("date", "numeric"))
##COMMODITY
Bloomberg_Commodity_Historical_Data <- read_delim("Bloomberg Commodity Historical Data.csv", 
                                                  delim = ";", escape_double = FALSE, col_types = cols(Price = col_number()), 
                                                  trim_ws = TRUE)

#Transformation into logarithmic data
LSNP <- log(SNP500_Return)

#Create one dataframe
df <- data.frame(SNP500_M,GDP_Mnew,Interest_M,PCE,BCI[,4])

#Estimate VAR with AIC as performance metric and max lag=8
var_aic <- VAR(df[,c(2,3,4,5,6,7)], type = "const", lag.max = 8, ic = "AIC", season = 4)

#ADF Tests (transform into TS objects) - stationary or non-stationary
adf.test(SNP)
adf.test(GDP1) 
adf.test(IR)  
adf.test(PCETS)
adf.test(BCITS)

#Choosing optimal number of lags
lagselect <- VARselect(dset,lag.max = 10, type = "const")
lagselect$criteria

#Cointegration test; bei type entweder "trace" oder "eigen"
ctest <- ca.jo(dset, type = "trace", K=5)
summary(ctest)

# Building the VECM model - 5=number of lags, 2 number of cointegrating relations
Model_1 <- VECM(dset,5,r = 2,estim = "ML")
summary(Model_1)
