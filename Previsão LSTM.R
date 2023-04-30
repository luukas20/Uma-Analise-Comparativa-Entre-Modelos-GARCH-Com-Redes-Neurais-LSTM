
rm(list = ls())

{if(!require(tidyverse)) install.packages('tidyverse')
  require(tidyverse)}
# {if(!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
#   require(BatchGetSymbols)}
{if(!require(forecast)) install.packages('forecast')
  require(forecast)}
# {if(!require(PerformanceAnalytics)) install.packages('PerformanceAnalytics')
#   require(PerformanceAnalytics)}
{if(!require(keras)) install.packages('keras')
  require(keras)}


bd <- BatchGetSymbols::BatchGetSymbols('GOLL4.SA', first.date = Sys.Date() - 1825, last.date = Sys.Date())
bd.xts <- xts::xts(bd$df.tickers$price.close,order.by = as.Date(bd$df.tickers$ref.date))

window_size <- 15

# Crie a matriz de dados de entrada e saída
normalized_prices <- scale(bd$df.tickers$price.close)
X <- matrix(0, nrow = length(normalized_prices) - window_size, ncol = window_size)
Y <- matrix(0, nrow = length(normalized_prices) - window_size, ncol = 1)

for (i in 1:(length(normalized_prices) - window_size)) {
  j <- i + window_size - 1
  X[i,] <- normalized_prices[i:j]
  Y[i,] <- normalized_prices[j+1]
}

# Divida os dados em conjuntos de treinamento e teste (80% para treinamento, 20% para teste)
train_size <- floor(0.8 * nrow(X))

X_train <- X[1:train_size,]
X_test <- X[(train_size+1):nrow(X),]
 
Y_train <- Y[1:train_size,]
Y_test <- Y[(train_size+1):nrow(Y),]


modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 35,return_sequences = T,input_shape = c(15,1)) |> 
  keras::layer_lstm(units = 35,return_sequences = T) |>
  keras::layer_lstm(units = 35) |>
  keras::layer_dropout(0.2) |>
  keras::layer_dense(units = 1) |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    optimizer = keras::optimizer_adam()
  )


modelo1


ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 50,
  batch_size = 15,
  verbose = 2)

pred <- predict(modelo1, X_test)

ftd <- pred * sd(bd$df.tickers$price.close) + mean(bd$df.tickers$price.close)
close <- bd$df.tickers$price.close[(train_size+16):nrow(bd$df.tickers)]

data <- data.frame(Data = bd$df.tickers$ref.date[(train_size+16):nrow(bd$df.tickers)],
                   Close = close,
                   Fitted = ftd)

# Plote as previsões em relação aos valores reais
plotly::ggplotly(
  ggplot(data = data) +
    geom_line(aes(x = Data, y = Fitted, color = "Predicted")) +
    geom_line(aes(x = Data, y = Close, color = "Actual")) +
    scale_color_manual(values = c("Predicted" = "red", "Actual" = "blue")) +
    labs(x = "Date", y = "Price")+
    theme_minimal()
)
list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_test[245,]

pred10 = c()
i=1
n_future=10

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * sd(bd$df.tickers$price.close) + mean(bd$df.tickers$price.close)))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * sd(bd$df.tickers$price.close) + mean(bd$df.tickers$price.close)))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}

predict <- pred10 * sd(bd$df.tickers$price.close) + mean(bd$df.tickers$price.close)

{if(!require(bizdays)) install.packages('bizdays')
  require(bizdays)}
d <- bizdays::bizseq(Sys.Date(), '2023-04-12', cal = "Brazil/ANBIMA")

acao <- data.frame(date = c(as.Date(tail(bd$df.tickers$ref.date,245)),d),
                   close = c(tail(bd$df.tickers$price.close,245),rep(NA,10)),
                   Fitted = c(tail(data$Fitted,245),rep(NA,10)),
                   predict = c(rep(NA,245),predict))

# Plote as previsões em relação aos valores reais
plotly::ggplotly(
  ggplot2::ggplot(data = acao) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = predict, color = "Predicted")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = close, color = "Actual")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = Fitted, color = 'Fitted'))+
    ggplot2::scale_color_manual(values = c("Predicted" = "red", "Actual" = "blue", 'Fitted' = 'green')) +
    ggplot2::labs(title = 'Rede Neural LSTM',x = "Date", y = "Price")
)


mean(abs((data$Close-data$Fitted)/data$Close), na.rm = T)*100




