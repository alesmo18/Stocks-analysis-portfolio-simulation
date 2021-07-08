# Caricamento delle librerie
library(tseries)
library(zoo)
library(xts)
library(quantmod)
library(dygraphs)
library(kernlab)
library(forecast)
library(htmltools)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
library(forecast)

### BIFS - Progetto
## Alessio Mognato 844953

# Titoli scelti: 
# NVDA, AMD settore tecnologia Nvidia e AMD
# BPSO.MI, ISP.MI banche Banca Popolare di Sondrio e Intesa Sanpaolo
# PFE, BMY settore farmaceutico Pfizer Inc e Bristol-Myers Squibb Company 

# Scaricamento dei dati: Data acquisition

di <- "2018-10-01" # data inizio
df <- "2020-10-01" # data fine
NVDA <- getSymbols("NVDA", src= "yahoo", from = di, to = df, auto.assign=FALSE) 
AMD <- getSymbols("AMD", src= "yahoo", from = di, to = df, auto.assign=FALSE)
BPSO <- getSymbols("BPSO.MI", src= "yahoo", from = di, to = df, auto.assign=FALSE) 
ISP <- getSymbols("ISP.MI", src= "yahoo", from = di, to = df, auto.assign=FALSE) 
PFE <- getSymbols("PFE", src= "yahoo", from = di, to = df, auto.assign=FALSE)
BMY <- getSymbols("BMY", src= "yahoo", from = di, to = df, auto.assign=FALSE)

### Data visualization, exploratory analysis

stocks_names <- c("NVDA","AMD","BPSO","ISP","PFE","BMY")

# Adj close per ogni titolo, cadenza giornaliera

NVDA_daily <- to.daily(NVDA)$NVDA.Adjusted
AMD_daily <- to.daily(AMD)$AMD.Adjusted
BPSO_daily <- to.daily(BPSO)$BPSO.Adjusted
ISP_daily <- to.daily(ISP)$ISP.Adjusted
PFE_daily <- to.daily(PFE)$PFE.Adjusted
BMY_daily <- to.daily(BMY)$BMY.Adjusted

merged_stocks_daily <- merge(NVDA_daily, AMD_daily, BPSO_daily, ISP_daily, PFE_daily, BMY_daily)
colnames(merged_stocks_daily) <- stocks_names
head(merged_stocks_daily)
frequency(merged_stocks_daily)

# Adj close per ogni titolo, cadenza settimanale

frequency(NVDA)

NVDA_weekly <- to.weekly(NVDA)$NVDA.Adjusted
AMD_weekly <- to.weekly(AMD)$AMD.Adjusted
BPSO_weekly <- to.weekly(BPSO)$BPSO.Adjusted
ISP_weekly <- to.weekly(ISP)$ISP.Adjusted
PFE_weekly <- to.weekly(PFE)$PFE.Adjusted
BMY_weekly <- to.weekly(BMY)$BMY.Adjusted

merged_stocks_weekly <- merge(NVDA_weekly, AMD_weekly, BPSO_weekly, ISP_weekly, PFE_weekly, BMY_weekly)
colnames(merged_stocks_weekly) <- stocks_names
head(merged_stocks_weekly)
frequency(merged_stocks_weekly)

# Adj close per ogni titolo, cadenza mensile

frequency(NVDA)

NVDA <- to.monthly(NVDA)$NVDA.Adjusted
AMD <- to.monthly(AMD)$AMD.Adjusted
BPSO <- to.monthly(BPSO)$BPSO.Adjusted
ISP <- to.monthly(ISP)$ISP.Adjusted
PFE <- to.monthly(PFE)$PFE.Adjusted
BMY <- to.monthly(BMY)$BMY.Adjusted

merged_stocks <- merge(NVDA, AMD, BPSO, ISP, PFE, BMY)
colnames(merged_stocks) <- stocks_names
head(merged_stocks)
frequency(merged_stocks)

# Plots delle adjcloses mensili per ogni stock
par(mfrow=c(3,2))
plot(merged_stocks$NVDA, main="AdjClose Nvidia", col="green")
plot(merged_stocks$AMD, main="AdjClose AMD", col="red")
plot(merged_stocks$BPSO, main="AdjClose BancaP.Sondrio", col="blue")
plot(merged_stocks$ISP, main="AdjClose IntesaSanPaolo", col="orange")
plot(merged_stocks$PFE, main="AdjClose Pfizer", col="black")
plot(merged_stocks$BMY, main="AdjClose Bristol-Myers", col="yellow")

# Plot di tutte le adjcloses mensili
#dygraph(merged_stocks, main="Comparison Adjusted Closes")
dygraph(merged_stocks, main="Comparison Adjusted Closes") %>%
  dyAxis("y", label = "Adjusted Close Price") %>%
  dyAxis("x", label = "Time") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector(height = 50) 


### Descriptive analytics
# Calcolo dei simple returns mensili per ogni titolo
# si utilizza la funzione Return.calculate dal package "PerformanceAnalytics"

NVDA_simple_return <- na.omit(Return.calculate(NVDA, method = "simple"))
AMD_simple_return <- na.omit(Return.calculate(AMD, method = "simple"))
BPSO_simple_return <- na.omit(Return.calculate(BPSO, method = "simple"))
ISP_simple_return <- na.omit(Return.calculate(ISP, method = "simple"))
PFE_simple_return <- na.omit(Return.calculate(PFE, method = "simple"))
BMY_simple_return <- na.omit(Return.calculate(BMY, method = "simple"))

stocks_simple_returns <- merge(NVDA_simple_return, AMD_simple_return, BPSO_simple_return,ISP_simple_return,PFE_simple_return,BMY_simple_return)
colnames(stocks_simple_returns) <- c("NVDA","AMD","BPSO","ISP","PFE","BMY")

# Calcolo dei simple returns giornalieri per ogni titolo

NVDA_simple_return_daily <- na.omit(Return.calculate(NVDA_daily, method = "simple"))
AMD_simple_return_daily <- na.omit(Return.calculate(AMD_daily, method = "simple"))
BPSO_simple_return_daily <- na.omit(Return.calculate(BPSO_daily, method = "simple"))
ISP_simple_return_daily <- na.omit(Return.calculate(ISP_daily, method = "simple"))
PFE_simple_return_daily <- na.omit(Return.calculate(PFE_daily, method = "simple"))
BMY_simple_return_daily <- na.omit(Return.calculate(BMY_daily, method = "simple"))

stocks_simple_returns_daily <- merge(NVDA_simple_return_daily, AMD_simple_return_daily, BPSO_simple_return_daily,ISP_simple_return_daily,PFE_simple_return_daily,BMY_simple_return_daily)
colnames(stocks_simple_returns_daily) <- c("NVDA","AMD","BPSO","ISP","PFE","BMY")

# Plot singoli dei simple returns

par(mfrow=c(3,2))
plot(NVDA_simple_return, main="Nvidia simple returns", col="green") 
plot(AMD_simple_return, main="AMD simple returns", col="red")
plot(BPSO_simple_return, main="BPSO simple returns", col="blue")
plot(ISP_simple_return, main="ISP simple returns", col="orange")
plot(PFE_simple_return, main="PFE simple returns", col="black")
plot(BMY_simple_return, main="BMY simple returns", col="yellow")

# Plot di tutti i simple returns mensili
#dygraph(stocks_simple_returns, main="Comparison Simple returns")
dygraph(stocks_simple_returns, main="Comparison Simple returns") %>%
  dyAxis("y", label = "Returns") %>%
  dyAxis("x", label = "Time") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector(height = 50) 


# Calcolo dei CC returns mensili per ogni titolo
# si utlizza sempre la funzione sopracitata con method = "compound"

NVDA_cc_return <- na.omit(Return.calculate(NVDA, method = "compound"))
AMD_cc_return <- na.omit(Return.calculate(AMD, method = "compound"))
BPSO_cc_return <- na.omit(Return.calculate(BPSO, method = "compound"))
ISP_cc_return <- na.omit(Return.calculate(ISP, method = "compound"))
PFE_cc_return <- na.omit(Return.calculate(PFE, method = "compound"))
BMY_cc_return <- na.omit(Return.calculate(BMY, method = "compound"))

stocks_cc_returns <- merge(NVDA_cc_return, AMD_cc_return, BPSO_cc_return,ISP_cc_return,PFE_cc_return,BMY_cc_return)
colnames(stocks_cc_returns) <- c("NVDA","AMD","BPSO","ISP","PFE","BMY")

# Plot singoli dei CC returns

par(mfrow=c(3,2))
plot(NVDA_cc_return, main="Nvidia CC returns", col="green") 
plot(AMD_cc_return, main="AMD CC returns", col="red")
plot(BPSO_cc_return, main="BPSO CC returns", col="blue")
plot(ISP_cc_return, main="ISP CC returns", col="orange")
plot(PFE_simple_return, main="PFE CC returns", col="black")
plot(BMY_cc_return, main="BMY CC returns", col="yellow")

# Plot di tutti i CC returns mensili
#dygraph(stocks_cc_returns, main="Comparison CC returns")
dygraph(stocks_cc_returns, main="Comparison CC returns") %>%
  dyAxis("y", label = "Returns") %>%
  dyAxis("x", label = "Time") %>%
  dyOptions(stackedGraph = FALSE) %>%
  dyRangeSelector(height = 50) 

# Plots per i titoli appartenenti alla stessa categoria di mercato

TECH <- merge(NVDA_cc_return, AMD_cc_return)
BANK <- merge(BPSO_cc_return,ISP_cc_return)
PHARM <- merge(PFE_cc_return,BMY_cc_return)
par(mfrow=c(3,1))
plot(TECH, main ="CC returns NVIDIA & AMD", col=c("green", "red"))
plot(BANK, main ="CC returns Banca P. Sondrio & Intesa S. Paolo", col=c("blue", "orange"))
plot(PHARM, main ="CC returns Pfizer & BMY", col=c("black", "yellow"))

#Istogrammi, lineplot, boxplot e qqplot anche su shinyapp per i CC returns

#4Panel-Diagnostic-Plots

NVDA.mat <- as.matrix(NVDA_cc_return)
AMD.mat <- as.matrix(AMD_cc_return)
BPSO.mat <- as.matrix(BPSO_cc_return)
ISP.mat <- as.matrix(ISP_cc_return)
PFE.mat <- as.matrix(PFE_cc_return)
BMY.mat <- as.matrix(BMY_cc_return)

# ricerca outliers
boxplot.stats(NVDA.mat)$out 
boxplot.stats(AMD.mat)$out # NO OUTLIERS
boxplot.stats(BPSO.mat)$out
boxplot.stats(ISP.mat)$out 
boxplot.stats(PFE.mat)$out
boxplot.stats(BMY.mat)$out # NO OUTLIERS

# Boxplot totale
par(mfrow=c(1,1))
stocks.mat <- as.matrix(stocks_cc_returns)
chart.Boxplot(stocks.mat, outlier.symbol = "O", main="Boxplot CC returns stocks", 
              xlab="return")

par(mfrow=c(2,2))
# 4P NVDA
hist(NVDA_cc_return, breaks = 20, freq=F, main="Istogramma CC returns NVDA", xlab="return", col = "green")
plot(density(NVDA_cc_return), main="Densita' dei CC returns NVDA", xlab="return", col="green")
qqnorm(NVDA_cc_return, main="QQPlot CC returns NVDA", col="green")
qqline(NVDA_cc_return)
chart.Boxplot(NVDA.mat, outlier.symbol = "O", main="Boxplot CC returns NVDA", col = "green", xlab="return")
# 4P AMD
hist(AMD_cc_return, breaks = 20, freq=F, main="Istogramma CC returns AMD", xlab="return", col = "red")
plot(density(AMD_cc_return), main="Densita' dei CC returns AMD", xlab="return", col="red")
qqnorm(AMD_cc_return, main="QQPlot CC returns NVDA", col="red")
qqline(AMD_cc_return)
chart.Boxplot(AMD.mat, outlier.symbol = "O", main="Boxplot CC returns AMD", col = "red", xlab="return")

# 4P BPSO
hist(BPSO_cc_return, breaks = 20, freq=F, main="Istogramma CC returns BPSO", xlab="return", col = "blue")
plot(density(BPSO_cc_return), main="Densita' dei CC returns BPSO", xlab="return", col="blue")
qqnorm(BPSO_cc_return, main="QQPlot CC returns BPSO", col="blue")
qqline(BPSO_cc_return)
chart.Boxplot(BPSO.mat, outlier.symbol = "O", main="Boxplot CC returns BPSO", col = "blue", xlab="return")
# 4P ISP
hist(ISP_cc_return, breaks = 20, freq=F, main="Istogramma CC returns ISP", xlab="return", col = "orange")
plot(density(ISP_cc_return), main="Densita' dei CC returns ISP", xlab="return", col="orange")
qqnorm(ISP_cc_return, main="QQPlot CC returns ISP", col="orange")
qqline(ISP_cc_return)
chart.Boxplot(ISP.mat, outlier.symbol = "O", main="Boxplot CC returns ISP", col = "orange", xlab="return")

# 4P PFE
hist(PFE_cc_return, breaks = 20, freq=F, main="Istogramma CC returns PFE", xlab="return", col = "black")
plot(density(PFE_cc_return), main="Densita' dei CC returns PFE", xlab="return", col="black")
qqnorm(PFE_cc_return, main="QQPlot CC returns PFE", col="black")
qqline(PFE_cc_return)
chart.Boxplot(PFE.mat, outlier.symbol = "O", main="Boxplot CC returns PFE", col = "yellow", xlab="return")
# 4P PFE
hist(BMY_cc_return, breaks = 20, freq=F, main="Istogramma CC returns BMY", xlab="return", col = "yellow")
plot(density(BMY_cc_return), main="Densita' dei CC returns BMY", xlab="return", col="yellow")
qqnorm(BMY_cc_return, main="QQPlot CC returns BMY", col="black")
qqline(BMY_cc_return)
chart.Boxplot(BMY.mat, outlier.symbol = "O", main="Boxplot CC returns BMY", col = "yellow", xlab="return")

## Univariate statistics

# si applica la funzione apply(obj, margin=2 sulle colonne, f)
# media
mean = apply(stocks_cc_returns,2,mean) 
# varianza
var = apply(stocks_cc_returns,2,var)
# dev stand
sd = apply(stocks_cc_returns,2,sd)
# skewness
s = apply(stocks_cc_returns,2,skewness)
# kurtosis
k = apply(stocks_cc_returns,2,kurtosis)
#lapply(stocks_cc_returns,density)

#quantili calcolo
quantili <- data.frame(apply(stocks_cc_returns, 2, quantile))
# plots
par(mfrow=c(1,1))
plot_quantili <- function(xts_cc, nomeStock){
  hist(xts_cc, breaks=30, freq=F, main=paste("Istogramma CC returns con quantili di", colnames(xts_cc)), xlab="return")
  for(i in 1:6){
    abline(v=quantili[i, nomeStock], col="red")
  }
}
plot_quantili(NVDA_cc_return, "NVDA")
plot_quantili(AMD_cc_return, "AMD")
plot_quantili(BPSO_cc_return, "BPSO")
plot_quantili(ISP_cc_return, "ISP")
plot_quantili(PFE_cc_return, "PFE")
plot_quantili(BMY_cc_return, "BMY")

# stampa del massimo valore per ogni stats
max(mean) # AMD
max(var) # NVDA
max(sd) # NVDA
max(s) # AMD
max(k) # ISP
univ_stats <- rbind(mean, var, sd, s, k)
colnames(univ_stats) <- stocks_names
# visualizzazione delle univ stats
print(univ_stats)

# volatily

barplot(var, main="Volatility of stocks", horiz=FALSE,
        names.arg= stocks_names) # volatily maggiore NVDA

# matrici di covarianza e correlazione

cov_mat <- var(rbind(NVDA.mat,AMD.mat,BPSO.mat,ISP.mat,PFE.mat,BMY.mat))
pairs(cbind(NVDA.mat, AMD.mat,BPSO.mat, ISP.mat, PFE.mat, BMY.mat), col = "blue", pch = 1, cex=1, main = "Scatterplots matrix")
# dagli scatterplots risultano correlazioni lineari tra BPSO e ISP
# plot della corr matrix con libreria corrplot
# Le correlazioni positive sono rappresentate in blue, quelle negative in rosso
# Intensità di colore e size del cerchio sono proporzionali ai coeff di correlazione
cor_matrix <- cor(cbind(NVDA.mat,AMD.mat,BPSO.mat,ISP.mat,PFE.mat,BMY.mat))
par(mfrow=c(1,1))
corrplot(cor_matrix, type = "upper", order = "hclust", main="Correlation plot",
         tl.col = "black", tl.srt = 65)
# BPSO e ISP sono correlati

### Portfolio management e forcasting
# asset beta computation, define func
beta <- function(stock, market_index) {
  b <- cov(stock, market_index) / var(market_index)
  return (b)
}

# indice NASDAQ
NASDAQ <- getSymbols("^IXIC", src="yahoo", from=di, to=df, auto.assign = F)
NASDAQ <- to.monthly(NASDAQ)
NASDAQ_cc <- na.omit(Return.calculate(NASDAQ$NASDAQ.Adjusted, method = "compound"))
colnames(NASDAQ_cc) <- c("NASDAQ")

# calcolo beta per ogni stocks

length_period <- dim(NASDAQ_cc)[1]
dim(NVDA_cc_return)[1]
bd <- 6 # beta delta, arco temporale di 6 mesi
start <- bd + 1

# inizializzazione var.
NVDA.betas <- NULL
AMD.betas <- NULL
BPSO.betas <- NULL
ISP.betas  <- NULL
PFE.betas <- NULL
BMY.betas <- NULL

# beta val computation

for (i in start:length_period) {
  NVDA.beta <- beta(NVDA_cc_return[(i-bd):(i-1)],NASDAQ_cc[(i-bd):(i-1)])
  AMD.beta <- beta(AMD_cc_return[(i-bd):(i-1)],NASDAQ_cc[(i-bd):(i-1)])
  BPSO.beta <- beta(BPSO_cc_return[(i-bd):(i-1)],NASDAQ_cc[(i-bd):(i-1)])
  ISP.beta  <- beta(ISP_cc_return[(i-bd):(i-1)],NASDAQ_cc[(i-bd):(i-1)])
  PFE.beta <- beta(PFE_cc_return[(i-bd):(i-1)],NASDAQ_cc[(i-bd):(i-1)])
  BMY.beta <- beta(BMY_cc_return[(i-bd):(i-1)],NASDAQ_cc[(i-bd):(i-1)])
  # creazione di una xts per ogni stock b
  NVDA.xts.beta <- as.xts(NVDA.beta, order.by = index(NVDA_cc_return[(i-1)]))
  AMD.xts.beta <- as.xts(AMD.beta, order.by = index(AMD_cc_return[(i-1)]))
  BPSO.xts.beta <- as.xts(BPSO.beta, order.by = index(BPSO_cc_return[(i-1)]))
  ISP.xts.beta <- as.xts(ISP.beta, order.by = index(ISP_cc_return[(i-1)]))
  PFE.xts.beta <- as.xts(PFE.beta, order.by = index(PFE_cc_return[(i-1)]))
  BMY.xts.beta <- as.xts(BMY.beta, order.by = index(BMY_cc_return[(i-1)]))
  
  if(is.null(NVDA.xts.beta)) {
    NVDA.betas <- NVDA.xts.beta
    AMD.betas <- AMD.xts.beta
    BPSO.betas <- BPSO.xts.beta
    ISP.betas <-  ISP.xts.beta
    PFE.betas <- PFE.xts.beta
    BMY.betas <- BMY.xts.beta
  } else {
    NVDA.betas <- rbind(NVDA.betas, NVDA.xts.beta)
    AMD.betas <- rbind(AMD.betas, AMD.xts.beta)
    BPSO.betas <- rbind(BPSO.betas, BPSO.xts.beta)
    ISP.betas  <- rbind(ISP.betas, ISP.xts.beta)
    PFE.betas <- rbind(PFE.betas, PFE.xts.beta)
    BMY.betas <- rbind(BMY.betas, BMY.xts.beta)
  }
  # stampa dei beta val
  print("BETA ASSET: indice NASDAQ")
  print("è una misura della sua volatilità dei rendimenti rispetto all'intero mercato.")
  print("Viene utilizzato come misura del rischio:")
  print("Il coefficiente beta può essere interpretato come segue:")
  print("β = 1 esattamente volatile come il mercato")
  print("β> 1 più volatile del mercato")
  print("β 0 meno volatile del mercato")
  print("β = 0 non correlato al mercato")
  print("β <0 correlato negativamente al mercato")
  print(paste("Finestra temporale calcolo data inizio:", index(NVDA_cc_return)[i-bd]))
  print(paste("Finestra temporale calcolo data fine:", index(NVDA_cc_return)[i-1]))
  print(paste("NVDA beta:", NVDA.beta))
  print(paste("AMD beta:", AMD.beta))
  print(paste("BPSO beta:", BPSO.beta))
  print(paste("ISP beta:", ISP.beta))
  print(paste("PFE beta:", PFE.beta))
  print(paste("BMY beta:", BMY.beta))
  
}

# simple plot betas
par(mfrow=c(1,1))
betas <- merge(NVDA.betas$NASDAQ, AMD.betas$NASDAQ, BPSO.betas$NASDAQ, ISP.betas$NASDAQ, PFE.betas$NASDAQ,BMY.betas$NASDA)
colnames(betas) <- stocks_names
#dev(width = 550, height = 330, unit = "px")
plot(betas)
par(mfrow=c(1,1))

# scaricamento dei dati per il forecasting
# dato che sono richiesti 80 mesi + 30 + 10 mesi occorrono minimo circa 9 anni di dati
# si utilizzano i dati degli ultimi 10 anni per ogni titolo
# date inizio e fine forecasting
di_fc <- "2010-10-01"
df_fc <- "2020-10-31"

# scaricamento dati per forecasting 
NVDA_fc <- get.hist.quote("NVDA", provider = "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(NVDA_fc) <- as.yearmon(index(NVDA_fc))
NVDA_fc_ccr <- na.omit(Return.calculate(NVDA_fc, method = "compound"))
AMD_fc <- get.hist.quote("AMD", provider = "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(AMD_fc) <- as.yearmon(index(AMD_fc))
AMD_fc_ccr <- na.omit(Return.calculate(AMD_fc, method = "compound"))
BPSO_fc <- get.hist.quote("BPSO.MI", provider = "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(BPSO_fc) <- as.yearmon(index(BPSO_fc))
BPSO_fc_ccr <- na.omit(Return.calculate(BPSO_fc, method = "compound"))
ISP_fc <- get.hist.quote("ISP.MI", provider = "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(ISP_fc) <- as.yearmon(index(ISP_fc))
ISP_fc_ccr <- na.omit(Return.calculate(ISP_fc, method = "compound"))
PFE_fc <- get.hist.quote("PFE", provider = "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(PFE_fc) <- as.yearmon(index(PFE_fc))
PFE_fc_ccr <- na.omit(Return.calculate(PFE_fc, method = "compound"))
BMY_fc <- get.hist.quote("BMY", provider = "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(BMY_fc) <- as.yearmon(index(BMY_fc))
BMY_fc_ccr <- na.omit(Return.calculate(BMY_fc, method = "compound"))

# scaricamento anche dell'indice NASDAQ
NASQ_fc <- get.hist.quote("^IXIC", provider= "yahoo", start = di_fc, end = df_fc, quote = "AdjClose", compression = "monthly")
index(NASQ_fc) <- as.yearmon(index(NASQ_fc))
NASQ_fc_ccr <- na.omit(Return.calculate(NASQ_fc, method = "compound"))

# si utilizza ARIMA model
# n = 80 mesi training set
# m = 30 mesi test set
# l = 10 invest

arima_function <- function(xts_stock, i){
  
  train_set <- xts_stock[1:80] 
  test_set <- xts_stock[81:110] 
  acc <- 5
  
  # test per trovare i migliori coeff AR e MA 
  #  range 0 ≤ p ≤ 5 and 0 ≤ q ≤ 5 come per auto.arima
  for(AR in 1:5){
    for(MA in 1:5){
      try(fit_test <- arima(train_set, order = c(AR, 0, MA)))
      # h length of forecasting periods, level confidence level
      forecast <- forecast(fit_test, h = length(test_set), level = c(80,95))
      # sostituzione acc migliore
      if( accuracy(forecast, test_set)[2,2] < acc){ 
          acc <- accuracy(forecast, test_set)[2,2]
          return_fit <- fit_test
          return_forecast <- forecast
      }
    }
  }
  #plot forecast
  plot(forecast, main=paste("ARMA forecast: ", stocks_names[i], "CC returns"), xlab="time", ylab="return")
  lines(test_set)
  
  return(list(forecast = return_forecast, 
              fit = return_fit,
              test_set = test_set))
}

NVDA_fit <- arima_function(NVDA_fc_ccr, 1)
NVDA_fit_acc <- accuracy(NVDA_fit$forecast, NVDA_fit$test_set)[2,2]
print(NVDA_fit_acc)

AMD_fit <- arima_function(AMD_fc_ccr, 2)
AMD_fit_acc <- accuracy(AMD_fit$forecast, AMD_fit$test_set)[2,2]
print(AMD_fit_acc)

BPSO_fit <- arima_function(BPSO_fc_ccr, 3)
BPSO_fit_acc <- accuracy(BPSO_fit$forecast, BPSO_fit$test_set)[2,2]
print(BPSO_fit_acc)

ISP_fit <- arima_function(ISP_fc_ccr, 4)
ISP_fit_acc <- accuracy(ISP_fit$forecast, ISP_fit$test_set)[2,2]
print(ISP_fit_acc)

PFE_fit <- arima_function(PFE_fc_ccr, 5)
PFE_fit_acc <- accuracy(PFE_fit$forecast, PFE_fit$test_set)[2,2]
print(PFE_fit_acc)

BMY_fit <- arima_function(BMY_fc_ccr, 6)
BMY_fit_acc <- accuracy(BMY_fit$forecast, BMY_fit$test_set)[2,2]
print(BMY_fit_acc)


#fc index
NASQ_fit <- arima_function(NASQ_fc_ccr, 999)
NASQ_fit_acc <- accuracy(NASQ_fit$forecast, NASQ_fit$test_set)[2,2]
print(NASQ_fit_acc)

merged_acc <- c(NVDA_fit_acc*100, AMD_fit_acc*100, BPSO_fit_acc*100, ISP_fit_acc*100, PFE_fit_acc*100, BMY_fit_acc*100)
names(merged_acc) <- stocks_names
print(merged_acc)

# Investimento forecast-based strategy

conf_lev = c(80,95)

# best params fitted
print(NVDA_fit$forecast$method) # NVDA(1,0,5)
print(AMD_fit$forecast$method) # AMD(2,0,2)
print(BPSO_fit$forecast$method) # BPSO(4,0,3)
print(ISP_fit$forecast$method) # ISP(5,0,2)
print(PFE_fit$forecast$method) # PFE(3,0,4)
print(BMY_fit$forecast$method) # BMY(1,0,4)

# Forecast di tre anni
years_fc <- 40
NVDA_forecast_inv <- forecast(NVDA_fit$fit, h=years_fc, level=conf_lev)$mean
AMD_forecast_inv <- forecast(AMD_fit$fit, h=years_fc, level=conf_lev)$mean
BPSO_forecast_inv <- forecast(BPSO_fit$fit, h=years_fc, level=conf_lev)$mean
ISP_forecast_inv <- forecast(ISP_fit$fit, h=years_fc, level=conf_lev)$mean
PFE_forecast_inv <- forecast(PFE_fit$fit, h=years_fc, level=conf_lev)$mean
BMY_forecast_inv <- forecast(BMY_fit$fit, h=years_fc, level=conf_lev)$mean
NASQ_forecast_inv <- forecast(NASQ_fit$fit, h=years_fc, level=conf_lev)$mean

# shiny print
arimaplotGen <- function(ticker){
  
  ticker_arm <- arima_function(ticker, NULL)
  "ARIMA Params all prints:"
  print(ticker_arm$fit)
  #print(accuracy(ticker_arm$forecast, ticker_arm$test_set))
}

# Ci interessano gli ultimi 10 mesi di investimento, pertanto:

NVDA_forecast <- as.xts(window(NVDA_forecast_inv, start=c(2020,01), end=c(2020,10)))
AMD_forecast <- as.xts(window(AMD_forecast_inv, start=c(2020,01), end=c(2020,10)))
BPSO_forecast <- as.xts(window(BPSO_forecast_inv, start=c(2020,01), end=c(2020,10)))
ISP_forecast <- as.xts(window(ISP_forecast_inv, start=c(2020,01), end=c(2020,10)))
PFE_forecast <- as.xts(window(PFE_forecast_inv, start=c(2020,01), end=c(2020,10)))
BMY_forecast <- as.xts(window(BMY_forecast_inv, start=c(2020,01), end=c(2020,10)))
NASQ_forecast <- as.xts(window(NASQ_forecast_inv, start=c(2020,01), end=c(2020,10)))

merged_stocks_forecast <-merge(NVDA_forecast, AMD_forecast, BPSO_forecast, ISP_forecast,
                               PFE_forecast, BMY_forecast)
colnames(merged_stocks_forecast) <- stocks_names

# calcolo dei beta per la finestra di investimento
ydt_nasdaq <- 0.1271
beta_calc <- vector('numeric', length = 6)
for (i in 1:length(beta_calc)){
  beta_calc[i] <- beta(merged_stocks_forecast[,i], NASQ_forecast[,1])
}
# rendimenti attesi, based on beta calc
rendimenti <- vector('numeric', length = 6)
for(i in 1:length(rendimenti)){
  rendimenti[i] <- beta_calc[i] * ydt_nasdaq
}

# Si costruiscono 5000 portfolio con pesi casuali e si sceglie quello con return massimo
mat_covarianza <- cov(merged_stocks_forecast)
num_port <- 5000

mat_weights <- matrix(nrow=num_port, ncol=6)
returns <- vector('numeric', length = num_port)
risks <- vector('numeric', length = num_port)
sharpes <- vector('numeric', length = num_port)

for (i in 1:num_port){
  weights <- runif(n=6)
  weights <- weights/sum(weights)
  mat_weights[i,] <- weights
  
  return_pred <- log(1+sum(weights*rendimenti))
  returns[i] <- return_pred
  
  risk <- sqrt(t(weights)%*%(mat_covarianza%*%weights))
  risks[i] <- risk
  
  sharpe_ratio <- return_pred/risk
  sharpes[i] <- sharpe_ratio
}

portfolio_list <- as.data.frame(cbind(mat_weights, returns, risks, sharpes))
dim(portfolio_list)
colnames(portfolio_list) <- c(stocks_names, "return", "risk", "sharperatio")
portfolio <- portfolio_list[which.max(portfolio_list$return),]
show(portfolio)

# Simulazione investimento
# budget 5000
# transaction_cost 0.01
# 10 month from 2020-01
V <- 5000
transaction_cost <- 0.01
start <- "2020-01-02"
end <- "2020-09-30"

#calcolo numero azioni per stock         
num_azioni <- vector('list', length = 6)
for(i in 1:length(num_azioni)){
  num_azioni[i] <- floor(V*portfolio[,i]/(merged_stocks_daily[,i][start])*(1+transaction_cost))
}
names(num_azioni) <- stocks_names

costo_azioni <- vector('list', length = 6)
costo_totale <- 0
for(i in 1:length(costo_azioni)){
  costo_azioni[i] <- as.numeric(num_azioni[i])*(merged_stocks_daily[,i][start])*(1+transaction_cost)
  costo_totale <- costo_totale+as.numeric(costo_azioni[i])
}
names(costo_azioni) <- stocks_names
costo_azioni
costo_totale
V_rest <- V - costo_totale

portfolio_initial_value <- 0
return_calc <- 0
for (i in 1:6){
  portfolio_initial_value <- portfolio_initial_value+as.numeric(num_azioni[i])*as.numeric(merged_stocks_daily[,i][start])
  return_calc <- return_calc+(portfolio[,i]*as.numeric(merged_stocks_daily[,i][start]/as.numeric(merged_stocks_daily[,i][end])-1))
}
portfolio_final_value <- portfolio_initial_value*(1+return_calc)

message("Valore portfolio iniziale:",portfolio_initial_value)
message("Valore portfolio finale:",portfolio_final_value)
message("Previsione valore:", portfolio_initial_value*(1+portfolio$return))
message("Rendimento atteso portfolio:", round(100*portfolio$return, 2),"%")
message("Rendimento effettivo del portfolio:", round(100*return_calc, 2),"%")

NVDA_optim <- NVDA_simple_return[1:14,]
AMD_optim <- AMD_simple_return[1:14,]
BPSO_optim <- BPSO_simple_return[1:14,]
ISP_optim <- ISP_simple_return[1:14,]
PFE_optim <- PFE_simple_return[1:14,]
BMY_optim <- BMY_simple_return[1:14,]

merged_stocks_optim <- merge(NVDA_optim, AMD_optim, BPSO_optim, ISP_optim, PFE_optim, BMY_optim)

p_optim <- portfolio.optim(merged_stocks_optim, shorts = F)
#print(p_optim)

#pw calc
for(i in 1:length(p_optim$pw)){
  if(p_optim$pw[i] <= 0)
    p_optim$pw[i] = 0
}

#names(p_optim$pw) <- stocks_names
print(p_optim$pw) #show weights
#print(p_optim$px) #show the returns of the overall portfolio
print(p_optim$pm) #the expected portfolio return

mean_portfolio_returns <- seq(0.0,0.02,length.out=150) # 2%
risk_portfolio <- numeric(length(mean_portfolio_returns))+NA
for( i in 1:length(mean_portfolio_returns) ) {
  portfolio <- NULL
  try(portfolio <- portfolio.optim(merged_stocks_optim,mean_portfolio_returns[i], shorts = F))
  if(!is.null(portfolio)) 
    risk_portfolio[i] <- portfolio$ps
}

plot(risk_portfolio, mean_portfolio_returns, pch=20, col="black", xlab="sigma", ylab="return",main="Frontiera efficiente")
points(p_optim$ps, p_optim$pm, pch=10, col="red" )

# calcolo num azioni e costo per ogni stock

NVDA_azioni <- floor(V*p_optim$pw[1]/(as.numeric(NVDA_daily[start])*(1+transaction_cost)))
AMD_azioni <- floor(V*p_optim$pw[2]/(as.numeric(AMD_daily[start])*(1+transaction_cost)))
BPSO_azioni <- floor(V*p_optim$pw[3]/(as.numeric(BPSO_daily[start])*(1+transaction_cost)))
ISP_azioni <- floor(V*p_optim$pw[4]/(as.numeric(ISP_daily[start])*(1+transaction_cost)))
PFE_azioni <- floor(V*p_optim$pw[5]/(as.numeric(PFE_daily[start])*(1+transaction_cost)))
BMY_azioni <- floor(V*p_optim$pw[6]/(as.numeric(BMY_daily[start])*(1+transaction_cost)))

NVDA_costo <- (NVDA_azioni*as.numeric(NVDA_daily[start]))*(1+transaction_cost)
AMD_costo <- (AMD_azioni*as.numeric(AMD_daily[start]))*(1+transaction_cost)
BPSO_costo <- (BPSO_azioni*as.numeric(BPSO_daily[start]))*(1+transaction_cost)
ISP_costo <- (ISP_azioni*as.numeric(ISP_daily[start]))*(1+transaction_cost)
PFE_costo <- (PFE_azioni*as.numeric(PFE_daily[start]))*(1+transaction_cost)
BMY_costo <- (BMY_azioni*as.numeric(BMY_daily[start]))*(1+transaction_cost)

# calcolo costo totale e resto
V_cost <- NVDA_costo + AMD_costo + BPSO_costo + ISP_costo + PFE_costo + BMY_costo
V_rest <- round(V-V_cost, 3)

NVDA_initial <- as.numeric(NVDA_daily[start])
AMD_initial <- as.numeric(AMD_daily[start])
PFE_initial <- as.numeric(PFE_daily[start])
ISP_initial <- as.numeric(ISP_daily[start])
PFE_initial <- as.numeric(PFE_daily[start])
BMY_initial <- as.numeric(BMY_daily[start])

NVDA_final <- as.numeric(NVDA_daily[end])
AMD_final <- as.numeric(AMD_daily[end])
PFE_final <- as.numeric(PFE_daily[end])
ISP_final <- as.numeric(ISP_daily[end])
PFE_final <- as.numeric(PFE_daily[end])
BMY_final <- as.numeric(BMY_daily[end])

# Valore p_optim a inizio investimento
p_optim_initial <- NVDA_initial*NVDA_azioni+
  AMD_initial*AMD_azioni+ 
  PFE_initial*PFE_azioni+
  ISP_initial*ISP_azioni+
  PFE_initial*PFE_azioni+
  BMY_initial*BMY_azioni

# Valore p_optim a fine investimento
p_optim_final<- NVDA_final*NVDA_azioni+
  AMD_final*AMD_azioni+ 
  PFE_final*PFE_azioni+
  ISP_final*ISP_azioni+
  PFE_final*PFE_azioni+
  BMY_final*BMY_azioni

# Valore p_optim_return effettivo
p_optim_rtn <- p_optim$pw[1]*(NVDA_final/NVDA_initial-1)+
  p_optim$pw[2]*(AMD_final/AMD_initial-1)+ 
  p_optim$pw[3]*(PFE_final/PFE_initial-1)+
  p_optim$pw[4]*(ISP_final/ISP_initial-1)+ 
  p_optim$pw[5]*(PFE_final/PFE_initial-1)+
  p_optim$pw[6]*(BMY_final/BMY_initial-1) 

print(V_cost)
print(V_rest)
names(p_optim$pw) <- stocks_names
print(sum(p_optim$pw))
options(scipen = 999)
print(round(p_optim$pw,3))
message("Ritorno atteso del p_optim:",round(100*p_optim$pm,2),"%\n")
message("Ritorno effettivo del p_optim:",round(100*p_optim_rtn,2),"%\n")


### run all code till here, then use Run app
library(shiny)
source("./ui.R")
source("./server.R")
shinyApp(ui = shinyUI, server = shinyServer)


