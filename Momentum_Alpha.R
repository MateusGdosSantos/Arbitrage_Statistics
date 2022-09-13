############################ BACKTEST MOMENTUM #################################
################################################################################

########################## Instalacao dos pacotes ##############################

#install.packages("quantmod")
#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("fUnitRoots")
#install.packages("devtools")
#install.packages("rollRegres")
#install.packages("BETS")
#install.packages("ggpubr")
#install.packages('timetk')
#install.packages('ellipsis')
#install.packages('PerformanceAnalytics')
#installed.packages("magrittr")

######################### Carregando bibliotecas ###############################

require(quantmod)
library(magrittr)
library(BETS)
library(zoo)
require(fGarch)
library(rb3)
library(forecast)
require(devtools)
library(rollRegres)
library(tidyverse)
library(tseries)
library(tidyquant)
library(PerformanceAnalytics)
library(ggpubr)
require(scales)
library(magrittr)
require(dplyr)
options(error=traceback)

############################# Obtencao de Dados ################################

ibovespa = paste(index_comp_get('IBOV'), ".SA", sep = "")

prices = tq_get(ibovespa, get = 'stock.prices', complete_cases = FALSE)

returns = prices %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
                mutate_fun = periodReturn,
                period = 'daily',
                col_rename = 'returns')

ibov = tq_get('^BVSP', get = 'stock.prices', complete_cases = FALSE) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ibov') 

cdi = BETSget(12, from = '2012-01-01') %>% 
  rename('cdi' = value) %>% 
  mutate(cdi = cdi / 100)

####################### Calculo dos excessos de retorno ########################

market_excess = left_join(ibov,cdi, by = 'date') %>% 
  mutate('market_excess' = ibov - cdi) %>% 
  select(date, market_excess)

equity_excess_return = inner_join(returns, cdi, by = 'date') %>% 
  mutate('equity_excess_return' = returns - cdi)

returns_data = inner_join(market_excess, equity_excess_return, by = 'date') %>% 
  select(date, symbol.y, equity_excess_return, market_excess) %>% 
  spread(symbol.y, value = equity_excess_return)

########################## Regressao Linear movel ##############################

Alphas = matrix(ncol = length(ibovespa), nrow = nrow(returns_data)) %>% 
  `colnames<-`(c(ibovespa))
#for (j in 3:length(ibovespa)) {}

Rollregress = roll_regres(returns_data$market_excess ~ returns_data$PETR4.SA,
                            width = 20,
                            do_compute = c("sigmas", "r.squareds"))

Alphas <- as.data.frame(Rollregress$coefs[,1]) %>% 
  na.omit()  

plot = Alphas %>% 
  ggplot(aes(x = index(Alphas))) + 
              geom_line(aes(y = `Rollregress$coefs[, 1]`))+
  geom_line(aes(y = mean(`Rollregress$coefs[, 1]`) + 2*sd(`Rollregress$coefs[, 1]`)), color = 'Red', linetype="dashed", lwd = 0.9) + 
  geom_line(aes(y = mean(`Rollregress$coefs[, 1]`) - 2*sd(`Rollregress$coefs[, 1]`)), color = 'Red', linetype="dashed", lwd = 0.9) +
  xlab("") + ylab("") + theme_pubclean()
plot
