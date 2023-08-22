# Autor: Matheus Vanzela
# Testes de séries temporais pacote forecastHybrid

library(pacman)
p_load("forecast", "tseries", "timeSeries", "FinTS", "forecastHybrid", "BatchGetSymbols", "xts",
       devtools, ggthemes, pacotes_series, pacotes_manipula,
       "tidyverse", "WriteXLS", "xtable", "tbl2xts",  "dplyr", "janitor")

# Cleaning environment
rm(list=ls())

# Get stock prices
apple <- BatchGetSymbols(tickers = "AAPL",
                         first.date = '2013-01-02', #Data de início
                         last.date = Sys.time(), #Data atual
                         type.return = "log", #Valores como log-retorno
                         freq.data = "daily") #retornos diários)

# Dataset
df.apple <- apple$df.tickers

# Select
df.apple <- df.apple %>% select(ref.date, ret.closing.prices) %>% slice(-1)
  
# Time series
ts.apple <- xts(df.apple[,-1], df.apple$ref.date)

# Quick model
q.model <- hybridModel(ts.apple)

# Forecast
f.model <- forecast(q.model, h=48)

# Plot
autoplot(f.model)

# Plot 
plot(q.model, type = "fit", ggplot = TRUE)

summary(q.model)

f.model$pointForecasts
accuracy(q.model, individual=TRUE)
