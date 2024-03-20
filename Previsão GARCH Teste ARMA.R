####################################
### Titulo: Modelo Final
### Dados da Amostra: BOVA11.SA 2018-01-01 a 2024-02-29
### Data: 05/02/2024
#####################################

rm(list = ls())

# Packages ----------------------------------------------------------------

if(!require(yfR)) install.packages('yfR'); require(yfR)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(ggforce)) install.packages('ggforce'); require(ggforce)
if(!require(forecast)) install.packages('forecast'); require(forecast)


# Fim ---------------------------------------------------------------------

# Importar Dados ----------------------------------------------------------

df <- yfR::yf_get('BOVA11.SA', first_date = as.Date('2018-01-01'), last_date = as.Date('2024-02-29'))

df.xts <- xts::xts(df$price_close, order.by = as.Date(df$ref_date)) 

rt.xts <- xts::xts(df$ret_closing_prices, order.by = as.Date(df$ref_date)) 

rt.xts <- rt.xts[-1]

# Fim ---------------------------------------------------------------------

# Dados Train Test --------------------------------------------------------

# Divida os dados em conjuntos de treinamento e teste (80% para treinamento, 20% para teste)
train_size <- (length(rt.xts) - 18) # floor(0.8 * nrow(X))

rt_train <- rt.xts[1:train_size]

rt_test <- rt.xts[(train_size+1):length(rt.xts),]


# Fim ---------------------------------------------------------------------


# Metodo de AIC -----------------------------------------------------------

md <- auto.arima(rt_train, max.p = 10, max.q = 10, stepwise = F, ic = 'aicc')

summary(md)

pred <- forecast(md, h = 18)

ytest <- rt_test
ypred <- pred$mean


mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs((ytest-ypred)/ytest))
sqrt(mean((ytest - ypred)^2)) / sqrt(mean((ytest - 0.0030751578)^2))


# Fim ---------------------------------------------------------------------


# Modelo ARMA(i,j) --------------------------------------------------------

for (i in 0:15) {
  for (j in 0:15) {
    md <- Arima(rt_train,order = c(i,0,j))
    
    summary(md)
    
    ytest <- rt_train
    ypred <- md$fitted
    
    
    mae <- mean(abs(ytest - ypred))
    mse <- mean((ytest - ypred)^2)
    rmse <- sqrt(mean((ytest - ypred)^2))
    mape <- mean(abs((ytest-ypred)/ytest))
    utheil <- sqrt(mean((ytest - ypred)^2)) / sqrt(mean((ytest - 0.0030751578)^2))
    aic <- md$aic
    
    mt <- data.frame(Modelo = paste0('ARMA',' (',i,', ',j,')'), 
                     MAE    = round(mae,4),
                     MSE    = round(mse,4),
                     RMSE   = round(rmse,4),
                     MAPE   = round(mape,4),
                     UTheil = round(utheil,4),
                     AIC = aic)
    
    ifelse(i == 0 & j == 0, mt2 <- mt, mt2 <- rbind(mt2,mt)) 
  }
}


md <- Arima(rt_train,order = c(9,0,10))



# Fim ---------------------------------------------------------------------


# Modelo ARMA(9,10) GARCH(1,1) --------------------------------------------

spec <- rugarch::ugarchspec(mean.model = list(armaOrder = c(9,10)),
                            variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            distribution.model = "std")

fit_final <- rugarch::ugarchfit(spec = spec, data = rt_train ,out.sample = 0)

y_pred <- rugarch::ugarchforecast(fit_final, n.ahead=18)


ypred <- NULL
for(i in 1:(length(y_pred@forecast$seriesFor))){
  ypred[i] <- 123.95 * (1 + sum(y_pred@forecast$seriesFor[1:i]))
}

ytest <- df.xts[1492:1509]

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs((ytest-ypred)/ytest))
sqrt(mean((ytest - ypred)^2)) / sqrt(mean((ytest - 123.95)^2))


# Fim ---------------------------------------------------------------------

# Modelo ARMA(9,10) GARCH(1,2) --------------------------------------------

spec <- rugarch::ugarchspec(mean.model = list(armaOrder = c(9,10)),
                            variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
                            distribution.model = "std")

fit_final <- rugarch::ugarchfit(spec = spec, data = rt_train ,out.sample = 0)

y_pred <- rugarch::ugarchforecast(fit_final, n.ahead=18)


ypred <- NULL
for(i in 1:(length(y_pred@forecast$seriesFor))){
  ypred[i] <- 123.95 * (1 + sum(y_pred@forecast$seriesFor[1:i]))
}

ytest <- df.xts[1492:1509]

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs((ytest-ypred)/ytest))
sqrt(mean((ytest - ypred)^2)) / sqrt(mean((ytest - 123.95)^2))


# Fim ---------------------------------------------------------------------

# Modelo ARMA(9,10) GARCH(2,1) --------------------------------------------

spec <- rugarch::ugarchspec(mean.model = list(armaOrder = c(9,10)),
                            variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
                            distribution.model = "std")

fit_final <- rugarch::ugarchfit(spec = spec, data = rt_train ,out.sample = 0)

y_pred <- rugarch::ugarchforecast(fit_final, n.ahead=18)


ypred <- NULL
for(i in 1:(length(y_pred@forecast$seriesFor))){
  ypred[i] <- 123.95 * (1 + sum(y_pred@forecast$seriesFor[1:i]))
}

ytest <- df.xts[1492:1509]

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs((ytest-ypred)/ytest))
sqrt(mean((ytest - ypred)^2)) / sqrt(mean((ytest - 123.95)^2))


# Fim ---------------------------------------------------------------------

# Modelo ARMA(9,10) GARCH(2,2) --------------------------------------------

spec <- rugarch::ugarchspec(mean.model = list(armaOrder = c(9,10)),
                            variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
                            distribution.model = "std")

fit_final <- rugarch::ugarchfit(spec = spec, data = rt_train ,out.sample = 0)

y_pred <- rugarch::ugarchforecast(fit_final, n.ahead=18)


ypred <- NULL
for(i in 1:(length(y_pred@forecast$seriesFor))){
  ypred[i] <- 123.95 * (1 + sum(y_pred@forecast$seriesFor[1:i]))
}

ytest <- df.xts[1492:1509]

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs((ytest-ypred)/ytest))
sqrt(mean((ytest - ypred)^2)) / sqrt(mean((ytest - 123.95)^2))


# Fim ---------------------------------------------------------------------


y_pred <- rugarch::ugarchforecast(fit_final, n.ahead=18)

ypred <- NULL
for(i in 1:(length(y_pred@forecast$seriesFor))){
  ypred[i] <- 123.95 * (1 + sum(y_pred@forecast$seriesFor[1:i]))
}

ftd <- rugarch::fitted(fit_final)

ft <- df.xts[1] |> as.numeric()
for (i in 1:length(ftd)) {
  ft[i+1] <- as.numeric(df.xts[i])*(1+as.numeric(ftd[i]))
}

data <- data.frame(Data = df$ref_date,
                   Close = df$price_close,
                   Fitted = c(ft,ypred))

# Plote as previsões em relação aos valores reais
plotly::ggplotly(
  ggplot(data = data) +
    geom_line(aes(x = Data, y = Close, color = "Actual")) +
    geom_line(aes(x = Data, y = Fitted, color = "Predicted")) +
    scale_color_manual(values = c("Predicted" = "red", "Actual" = "blue")) +
    labs(x = "Date", y = "Price") + 
    theme_minimal()
)

ggplot(data) +
  geom_line(aes(x = Data, y = Close, color = "Valor Real"), size = 1) +
  geom_line(aes(x = Data, y = Fitted, color = "Valor Predito"), size = 1) +
  scale_color_manual(values = c("Valor Predito" = "red", "Valor Real" = "blue")) +
  labs(x = "Data", y = "Preço de Fechamento") +
  facet_zoom(xlim = c(as.Date("2024-02-01"), as.Date("2024-02-29"))) +
  theme_light() + theme(legend.position = "bottom", legend.title = element_blank())

( 1- mean(abs(data$Close-data$Fitted)/data$Close, na.rm = T) ) * 100

plot(as.numeric(df.xts[1492:1509]), type = 'l')
lines(ypred, col = 2)
