####################################
### Titulo: Pesquisa em Grade 
### Dados da Amostra: BOVA11.SA 2018-01-01 a Sys.Date()
### Data: 05/02/2024
#####################################

rm(list = ls())

# Packages ----------------------------------------------------------------

if(!require(yfR)) install.packages('yfR'); require(yfR)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(ggforce)) install.packages('ggforce'); require(ggforce)
if(!require(forecast)) install.packages('forecast'); require(forecast)
if(!require(keras)) install.packages('keras'); require(keras)


# Fim ---------------------------------------------------------------------

# Importar Dados ----------------------------------------------------------

df <- yfR::yf_get('BOVA11.SA', first_date = as.Date('2018-01-01'), last_date = as.Date('2024-02-29'))

df.xts <- xts::xts(df$price_close, order.by = as.Date(df$ref_date)) 

rt.xts <- xts::xts(df$ret_closing_prices, order.by = as.Date(df$ref_date)) 

rt.xts <- rt.xts[-1]

# Fim ---------------------------------------------------------------------

# Dados Train Test --------------------------------------------------------

window_size <- 15

me <- mean(df$price_close)
amp <- max(df$price_close) - min(df$price_close)

# Crie a matriz de dados de entrada e saída
normalized_prices <- (df$price_close - me)/(amp) 
X <- matrix(0, nrow = length(normalized_prices) - window_size, ncol = window_size)
Y <- matrix(0, nrow = length(normalized_prices) - window_size, ncol = 1)

for (i in 1:(length(normalized_prices) - window_size)) {
  j <- i + window_size - 1
  X[i,] <- normalized_prices[i:j]
  Y[i,] <- normalized_prices[j+1]
}

# Divida os dados em conjuntos de treinamento e teste (80% para treinamento, 20% para teste)
train_size <- (nrow(X) - 18) # floor(0.8 * nrow(X))

X_train <- X[1:train_size,]
X_test <- X[(train_size+1):nrow(X),]

Y_train <- Y[1:train_size,]
Y_test <- Y[(train_size+1):nrow(Y),]


# Fim ---------------------------------------------------------------------

# Modelo Pequeno  ---------------------------------------------------------

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 16, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1 <- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)


list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 16, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_lstm(units = 8, activation = 'linear', return_sequences = T) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)


list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me


mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 16, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_lstm(units = 8, activation = 'linear', return_sequences = T) |>
  keras::layer_lstm(units = 4, activation = 'linear', return_sequences = T) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)


list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me


mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

# Fim ---------------------------------------------------------------------

# Modelo Medio  -----------------------------------------------------------

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 64, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)


list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 64, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_lstm(units = 32, activation = 'linear', return_sequences = T) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)

list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 64, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_lstm(units = 32, activation = 'linear', return_sequences = T) |>
  keras::layer_lstm(units = 16, activation = 'linear', return_sequences = T) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)

list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

# Fim ---------------------------------------------------------------------

# Modelo Grande -----------------------------------------------------------


modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 256, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)


list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 256, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_lstm(units = 128, activation = 'linear', return_sequences = T) |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)

list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 256, activation = 'linear', return_sequences = T,input_shape = c(15,1)) |>
  keras::layer_lstm(units = 128, activation = 'linear', return_sequences = T) |>
  keras::layer_lstm(units = 64, activation = 'linear', return_sequences = T) |>
  keras::layer_lstm(units = 32, activation = 'linear') |>
  keras::layer_dense(units = 1, activation = 'linear') |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1<- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)

list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))

# Fim ---------------------------------------------------------------------



modelo1 <- keras::keras_model_sequential() |>
  # arquitetura da rede
  keras::layer_lstm(units = 128, activation = keras::layer_activation_leaky_relu(alpha = 0.4), return_sequences = T,input_shape = c(15,1)) |>
  #keras::layer_batch_normalization() |>
  #keras::layer_dropout(0.06) |>
  keras::layer_lstm(units = 64, activation = keras::layer_activation_leaky_relu(alpha = 0.3), return_sequences = T) |>
  #keras::layer_batch_normalization() |>
  #keras::layer_dropout(0.05) |>
  keras::layer_lstm(units = 32, activation = keras::layer_activation_leaky_relu(alpha = 0.3), return_sequences = F) |>
  #keras::layer_batch_normalization() |>
  #keras::layer_dropout(0.04) |>
  keras::layer_dense(units = 1, activation = keras::layer_activation_leaky_relu(alpha = 0.2)) |>
  # backpropagation
  keras::compile(
    loss = 'mean_squared_error',
    #optimizer = keras::optimizer_adam()
  )

ajuste1 <- modelo1 |> keras::fit(
  x=X_train,
  y=Y_train,
  validation_data = list(X_test, Y_test), 
  epochs = 100,
  batch_size = 16,
  callbacks = list(callback_reduce_lr_on_plateau(factor = 0.05, patience = 20)),
  verbose = 2)

list_output_steps <- matrix(ncol = 15,nrow = 1)
list_output_steps[1,] <- X_train[nrow(X_train),]

pred10 = c()
i=1
n_future=18

while(i <= n_future){
  if(length(list_output_steps) > 15){
    input_steps = list_output_steps[,-1] |> matrix(ncol = 15,nrow = 1)
    pred = predict(modelo1,input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
  else{
    input_steps = list_output_steps
    pred = predict(modelo1, input_steps)
    print(paste('Dia',Sys.Date()+i,'Valores de saida -> ', pred * amp + me))
    list_output_steps <- cbind(input_steps,pred)
    pred10[i] <- pred
    i=i+1
  }
}


ytest <- Y_test * amp + me
ypred <- pred10 * amp + me

mean(abs(ytest - ypred))
mean((ytest - ypred)^2)
sqrt(mean((ytest - ypred)^2))
mean(abs(ytest-ypred)/ytest)
sqrt(mean((ytest - ypred)^2)) / sqrt(mean(diff(ytest, lag = 1)^2))



pred <- predict(modelo1, X_train)

ftd <- pred * amp + me
close <-df$price_close[16:nrow(df)]

data <- data.frame(Data = df$ref_date[16:nrow(df)],
                   Close = close,
                   Fitted = c(ftd,ypred))

# Plote as previsões em relação aos valores reais
plotly::ggplotly(
  ggplot(data = data) +
    geom_line(aes(x = Data, y = Close, color = "Actual")) +
    geom_line(aes(x = Data, y = Fitted, color = "Predicted")) +
    scale_color_manual(values = c("Predicted" = "red", "Actual" = "blue")) +
    labs(x = "Date", y = "Price")
  + theme_minimal()
)
