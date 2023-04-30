
{if(!require(tidyverse)) install.packages('tidyverse')
  require(tidyverse)}
{if(!require(plotly)) install.packages('plotly')
  require(plotly)}
# {if(!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
#   require(BatchGetSymbols)}
# {if(!require(PerformanceAnalytics)) install.packages('PerformanceAnalytics')
#   require(PerformanceAnalytics)}
{if(!require(bizdays)) install.packages('bizdays')
  require(bizdays)}
{if(!require(forecast)) install.packages('forecast')
  require(forecast)}
{if(!require(rugarch)) install.packages('rugarch')
  require(rugarch)}

bd <- BatchGetSymbols::BatchGetSymbols('PETR4.SA', first.date = Sys.Date() - 1825, last.date = Sys.Date(), freq.data = 'daily')

bd.xts <- xts::xts(bd$df.tickers$price.close,order.by = as.Date(bd$df.tickers$ref.date)) 

quantmod::chartSeries(bd.xts)

forecast::autoplot(tail(bd.xts,60))

#auto.arima(rt)

# rt = (Hoje-Ontem)/ontem
rt <- PerformanceAnalytics::CalculateReturns(bd.xts, method = 'discrete')

rt <- rt[-1]

forecast::autoplot(rt)

quantmod::chart_Series(rt)

auto.arima(rt) |> summary()

spec <- rugarch::ugarchspec(mean.model = list(armaOrder = c(2,2)),
                            variance.model = list(model = "sGARCH", garchOrder = c(2, 1)),
                            distribution.model = "std")

fit_test <- rugarch::ugarchfit(spec = spec, data = rt ,out.sample = 20)

fit_test


rugarch::plot(fit_test, which = 'all')

y <- rugarch::ugarchforecast(fit_test, n.ahead=20) 

y

rugarch::fpm(y)


fit_final <- rugarch::ugarchfit(spec = spec, data = rt ,out.sample = 0)

y_pred <- rugarch::ugarchforecast(fit_final, n.ahead=10) 

rugarch::plot(y_pred, which = 'all')

head(rugarch::sigma(y_pred))
head(rugarch::fitted(y_pred)) |> as.numeric() |> round(6)

z <- bd.xts[length(bd.xts)] |> as.numeric()

for(i in 2:(length(y_pred@forecast$seriesFor)+1)){
  z[i] <- z[i-1]*(1+y_pred@forecast$seriesFor[i-1])
}
z

erro <- rugarch::residuals(fit_final)
ftd <- rugarch::fitted(fit_final)

xts::plot.xts(tail(rt,245))
lines(tail(ftd,245), col = 2)

ft <- bd.xts[1]|> as.numeric()
for (i in 1:length(ftd)) {
  ft[i+1] <- as.numeric(bd.xts[i])*(1+as.numeric(ftd[i]))
}

ft.xts <- xts::xts(ft,order.by = as.Date(bd$df.tickers$ref.date)) 

xts::plot.xts(tail(bd.xts,245))
lines(tail(ft.xts,245), col = 2)


d <- bizdays::bizseq(Sys.Date(), '2023-04-11', cal = "Brazil/ANBIMA")

acao <- data.frame(date = c(as.Date(tail(bd$df.tickers$ref.date,245)),d),
                   close = c(tail(bd$df.tickers$price.close,245),rep(NA,10)),
                   predict = c(tail(ft,245),rep(NA,10)),
                   Forecast = c(rep(NA,245),z[-1]))


plotly::ggplotly(
  ggplot2::ggplot(data = acao) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = close, color = "Actual")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = predict, color = "Predicted")) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = Forecast, color = 'Fitted'))+
    ggplot2::scale_color_manual(values = c("Predicted" = "red", "Actual" = "blue", 'Fitted' = 'green')) +
    ggplot2::labs(title = 'ARMA(2,2)-GARCH(2,1)',x = "Date", y = "Price")
)

mean(abs((bd.xts-ft.xts)/bd.xts))*100
