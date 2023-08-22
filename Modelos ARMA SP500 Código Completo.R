# Autor: Matheus Vanzela
# Lista 3 - Exercícios 3 e 4
# Inicialmente foi codificado via R Markdown, no entanto tive um erro na renderização.
# Como não consegui resolver de jeito nenhum, estão aqui todos as linhas de código em formato .R

# Pacotes
library(pacman)
p_load(BatchGetSymbols, tidyverse, ggthemes, FinTS, WriteXLS,
       xtable, tbl2xts, forecast, tseries, timeSeries, dplyr, janitor, xts)

# Limpando o ambiente
rm(list = ls())

## HOMEWORK 3 

#Refere-se aos exercícios 3 e 4 da lista 3 (ARMA Prática)

### Exercício 3
# Estimar um modelo ARMA para os tickers SP500 e criar uma coluna de previsão para t+1, indicando qual seria o maior retorno.
# A parte 1 da homework 3 vai do Data Wrangling até a checagem dos requisitos de estacionariedade da série temporal. A modelagem dos ativos selecionados, checagem dos resíduos, imposição da heurística para os ativos que tiveram resultado (0,0,0), visualização dos gráficos e geração dos datasets de resultados está na parte II por conta da memória utilizada na renderização. Foram escolhidos os 10 primeiros ativos para a tarefa.


# PARTE I ----------------------------------------------------------------

# Dataset with tickers
sp500_get <- BatchGetSymbols::GetSP500Stocks()

# Get tickers
tickers <- sp500_get$Tickers

sp500 <- BatchGetSymbols(tickers = tickers,
                         first.date = '2010-01-02', #Data de início
                         last.date = Sys.time(), #Data atual
                         type.return = "log", #Valores como log-retorno
                         freq.data = "daily") #retornos diários)

# Get dataset with values
ds.sp500 <- sp500["df.tickers"]
# Transforming and clean colnames
ds.sp500 <- ds.sp500 %>% as.data.frame() 

# More intuitive colnames
names(ds.sp500) = gsub(pattern = "df.tickers.", replacement = "", x = names(ds.sp500))

# Selecting
daily <- ds.sp500 %>% 
  select(ticker, ref.date, ret.closing.prices) %>% 
  slice(-1)
#object.size(daily)/1000000

# Pivoting Long to Wide
daily_wide <- daily %>% 
  pivot_wider(names_from = ticker, values_from = ret.closing.prices)
glimpse(daily_wide)

# Removing NAs
# Defining functions to clean code
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

# Removing any columns with NA value
daily_wide <- daily_wide %>% 
  select(where(not_all_na))

# Converting data to time series
daily_ts <- xts(daily_wide[,-1], daily_wide$ref.date)
# Line 1 seems to be NAs values... check this after whatever 
daily_ts <- daily_ts[-1,]
# Summary statistics of time series
sum <- summary(daily_ts)

# Calibragem automática do modelo
# apenas para os 10 primeiros por causa da memória
fit_arima <- apply(daily_ts[,1:10], 2, auto.arima, approximation = FALSE) 

# Function to get optimum order of auto.arima
ordem <- function(lista){
  ordens <- list()
  for (i in 1:length(lista)) {
    ordens[[i]] <- forecast::arimaorder(lista[[i]])
  }
  return(ordens)
} 

# Apply ordem function
ordem_fit <- ordem(fit_arima)
# Adjust outputs
ordem_fit <- ordem_fit %>%
  as.data.frame %>%
  t()
# Delete row.names
row.names(ordem_fit) <- NULL

# apenas para os 10 primeiros por causa da memória
ordem_fit <- ordem_fit %>% as.data.frame()
soma <- apply(ordem_fit, 1, sum)

# Function to replace arima=(0,0,0) to (1,0,0) [heuristic rule]
replace_zeros <- function(order_vector, num_stocks){
  soma <- apply(order_vector, 1, sum)
  vector_replaced <- as.data.frame(order_vector)
  for (i in 1:num_stocks) {
    if (soma[i]==0){
      order_vector[i,"p"] = 1
    }
  }
  return(order_vector)
}

# Replacing zero values in ordem_fit
ordem_fit <- replace_zeros(ordem_fit, num_stocks = 10)
ordem_fit <- as.matrix(ordem_fit)


#Declaration of dataset with limited stocks
ts_top10 <- daily_ts[,1:10]

# DF test
lapply(ts_top10, 
       function(x) adf.test(x[!is.na(x)], alternative = 'stationary', k=0)) 

#Os testes até aqui foram para modelar os dados e testar as condições de estacionariedade. Foram definidas funções para as rotinas que demandas loops.

# Test to one stock
# Application of Arima function in time series 
x <- Arima(ts_top10[,1], order = ordem_fit[1,])

# Checking residuals
checkresiduals(x, lag=10, plot = FALSE)

# Defining Arima function to model all selected stocks
fun_arima <- function(temp_serie, order_vec, num_stocks){
  lista <- list()
  for (var in 1:num_stocks) {
    lista[[var]] <- Arima(temp_serie[,var], order = order_vec[var,])
  }
  return(lista)
}

#Aplicando a função programada são modelados ARIMA's para os 10 ativos seguindo os resultados da função auto.arima() combinado com a heurística criada para coerção dos modelos (0,0,0) para (1,0,0). Alguns testes Ljung Box foram feitos para checagem das condições de heterocedasticidade dos resíduos. As funções FAC e FACP são apresentadas no próximo pdf



# ----------------------------------------------------------------------------
# Parte II

# Modelling Arima to selected stocks
modelos <- fun_arima(ts_top10, ordem_fit, 10)

# Checking residuals in a list of models
lapply(modelos, checkresiduals, lag=2, plot=TRUE)

# De acordo com os testes Ljung-Box conjugados à análise gráfica das FAC e FACP
# Modelling forecasts
forecast_modelos <- lapply(modelos, forecast, 5)

forecast_modelos

# Accuracy measures
accuracy(forecast_modelos[[1]])

# Exclusão da linha com a data de início que tem retorno NA em todas as células
daily_wide <- daily_wide[rowSums(is.na(daily_wide))<(length(daily_wide)-1),]

# Date object to coincide with time series objects
data <- daily_wide$ref.date
tail(data)

# Test to one stock
#mmm <- xts(forecast_modelos[[1]][["fitted"]], data)
#test <- ts_top10[,1]
# Plots sobrepostos
#plot(test, col="red")
#lines(mmm, col="blue")

# Selecting mean values from forecast_modelos
fit_mean <- matrix()
for (i in seq_along(forecast_modelos)) {
  line_matrix <- forecast_modelos[[i]]$mean 
  fit_mean <- fit_mean %>% cbind(line_matrix)
}
# Drop first row
fit_mean <- fit_mean[,-1]
# Column names
colnames(fit_mean) <- tickers[1:10]
# Corresponding date
datas <- seq(as.Date("2023-08-16"), as.Date("2023-08-20"), by = "day")
glimpse(datas)
fit_mean_dataset <- as.data.frame(fit_mean)
#View(fit_mean_dataset)
rownames(fit_mean_dataset) <- datas

# Dataset with 1:t+5 periods, which t+1..t+5 will be forecasted values
ts_top10_dataframe <- as.data.frame(ts_top10)
#View(tail(ts_top10_dataframe))
data_forecasted <- rbind(ts_top10_dataframe, fit_mean_dataset)
glimpse(data_forecasted)
# Five predictions and today return
preditos <- tail(data_forecasted)

# Find max(preditos) to t+1 period
# This will confirm tomorrow and the values are available to top10 choosen stocks
# Top10 are choosen beacause of limitations with memory
View(preditos)
# Col with max value to second row (t+1)
max_preditos_tomorrow <- max.col(preditos[2,])
# Declare stock with max return 
stock_max_tomorrow <- (preditos[max_preditos_tomorrow])

# Show Results
stock_max_tomorrow

### Resposta do Exercício 3:
#A ação com maior expectativa de retorno será ADM (dentre as 10 escolhidas) com 2,61 de retorno 
# para a data de 17 de agosto de 2023. Agora é só acompanhar!!!!

# Configuração das necessidades para responder ao exercícios 4 --------------------------------
# Size of trainning and test models based on dataset percent
train_pct <- 0.85
train_size <- round(train_pct * length(ts_top10)/ncol(ts_top10))

# Trainning and testing datasets
trainning <- apply(ts_top10, 2, window, end=train_size)
testing <- apply(ts_top10, 2, window, start=train_size+1, end=nrow(ts_top10))

# # Modelling arima to training data
# ordem_fit[1,]
# treino_ds <- trainning[,1]
# treino_ds <- treino_ds %>% as.xts
# class(treino_ds)
# 
# treino_teste <- arima(trainning[,1], order=ordem_fit[1,])
# treino_teste
# 
# forecast_treino_teste <- treino_teste %>% forecast(nrow(testing), bootstrap=TRUE)
# autoplot(forecast_treino_teste)
# 
# forecast_treino_teste$mean

treino_arima <- fun_arima(trainning, ordem_fit, 10)
# Forecasting since arima modelling
pred_treino <- lapply(treino_arima, forecast, nrow(testing))

pred_treino_datas <- lapply(pred_treino, xts, data)
pred_treino_data_teste <- ts(pred_treino[[1]], 
                              data)

# Function to plot iteratively
fun_plot <-  function(lista){
  p <- autoplot(lista)+autolayer(fitted(lista))
  return(p)
}

# Apply fun_plot in set of stocks forescat
lapply(pred_treino, fun_plot)

# Defining matrix to store results
mz <- matrix(ncol = 7)
# TIckers names from time series of top 10 papers selected
tickers <- colnames(ts_top10)

# Verfying accuracy in series modelled
for (i in 1:10) {
  a_vec <- treino_arima[[i]] %>% forecast(h = nrow(testing)) %>% accuracy(testing[,i])
  a_vec[,c("RMSE","MAE","MAPE","MASE")]
  mz <- mz %>% 
    rbind(a_vec)
}
View(mz)

# Matriz com as métricas de performance
mz <- mz[-1, c("RMSE","MAE","MAPE","MASE")]


# Resposta do ex 4) 
# A partir da matriz mz é possível comparar como o modelo se ajustou aos dados, tanto no conjunto de treino, quanto no conjunto de testes.
# Ao comparar as médias de RMSE do conjunto de treino, em relação ao conjunto de testes é possível perceber uma paridade entre os conjuntos.
# Este resultado pode ser interpretado como o modelo gerado a partir do conjunto de treinamento e suas previsões tem resultados parecidos 
# com as previsões do conjunto de testes (nesse caso é de 15% do dataset). Um ajuste com baixo RMSE no conjunto de treinamento indica overfit;
# Como os resultados foram parecidos, essa métrica aponta que o modelo é capaz de ser utilizado como apoio a tomaa de decisão de compra de ativos.
# Utilizando o MASE como métrica de performance (que representa uma comparação com as previsões naive), podemos conferir melhor desempenho dos modelos 
# ARIMA propostos. Este é outro indicativo de capacidade de utilização da ferramenta.

# Verificação de quantas oportunidades a decisão de comprar um ativo e vender com um retorno maior que o dia anterior 
# conseguiu ser capturado pelos modelos, considerando a média de forecast. 

## Observações:
# Apesar do código estar pronto para auferir em quais situações o forecast 
# captura retornos positivos comparados a retornos positivos observados, 
# a estrutura dos resultados preditos com o modelo arima é rígida em torno da média. 
# Na minha avaliação essa característica permite a aplicação do modelo apenas para períodos subsequentes

# Declaração do dataset com as médias previstas

# Função para coletar as médias com os nomes dos ativos
func_coleta_media <- function(lista, nomes){
  ds  <- NULL
  for (i in 1:10){
    tmp <- pred_treino[[i]][["mean"]]
    ds <- cbind(ds, tmp)
  }
  colnames(ds) <- nomes
  return(as.data.frame(ds))
}

# Declaração do dataset com as médias
media_forecast <- func_coleta_media(pred_treino, tickers)

# Função para coletar os resultados observados correspondentes aos forecasts médios
res_obs <- ts_top10 %>% as.data.frame %>% tail(n=514)

# Agora bastaria codificar a comparação entre os datasets.
# No entanto, de acordo com o observado os resultados seriam irreais pensando nessas previsões com horizonte temporal maior






