
rm(list = ls())


# Packages ----------------------------------------------------------------

if(!require(BatchGetSymbols)) install.packages('BatchGetSymbols'); require(BatchGetSymbols)
if(!require(PerformanceAnalytics)) install.packages('PerformanceAnalytics'); require(PerformanceAnalytics)
if(!require(tidyverse)) install.packages('tidyverse'); require(tidyverse)
if(!require(forecast)) install.packages('forecast'); require(forecast)



# Fim ---------------------------------------------------------------------


# Importar Dados ----------------------------------------------------------

bd <- BatchGetSymbols::BatchGetSymbols('BOVA11.SA', first.date = as.Date('2018-01-01'), last.date = as.Date('2024-02-29'))

df <- bd$df.tickers

df.xts <- xts::xts(df$price.close, order.by = as.Date(df$ref.date)) 

rt.xts <- xts::xts(df$ret.closing.prices, order.by = as.Date(df$ref.date)) 


# Fim ---------------------------------------------------------------------


# Gráfico de Linha da Série Temporal e Retorno ----------------------------

df %>% ggplot() +
  geom_line(aes(x = ref.date, y = price.close), size = 0.8, color = '#6C8558') +
  labs(x = 'Data', y = 'Preço de Fechamento') +
  theme_minimal() 

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\Linha Cotação BOVA11.pdf", width = 7, height = 5)

df %>% ggplot() +
  geom_line(aes(x = ref.date, y = ret.closing.prices), size = 0.8, color = '#6C8558') +
  labs(x = 'Data', y = 'Retorno de Fechamento') +
  theme_minimal()

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\Linha Retorno BOVA11.pdf", width = 7, height = 5)


# Fim ---------------------------------------------------------------------


# Tabela Estatística Descritiva -------------------------------------------

df %>% 
  summarise(
    Count = n(),
    Min = min(price.close),
    Max = max(price.close),
    Mean = mean(price.close),
    Median = median(price.close),
    Sd = sd(price.close),
    Q1 = quantile(price.close, 0.25),
    Q2 = quantile(price.close, 0.50),
    Q3 = quantile(price.close, 0.75))

df %>% 
  summarise(
    Count = n(),
    Min = min(ret.closing.prices, na.rm = T),
    Max = max(ret.closing.prices, na.rm = T),
    Mean = mean(ret.closing.prices, na.rm = T),
    Median = median(ret.closing.prices, na.rm = T),
    Sd = sd(ret.closing.prices, na.rm = T),
    Q1 = quantile(ret.closing.prices, 0.25, na.rm = T),
    Q2 = quantile(ret.closing.prices, 0.50, na.rm = T),
    Q3 = quantile(ret.closing.prices, 0.9, na.rm = T))

quantile(df$ret.closing.prices, 0.01, na.rm = T)
quantile(df$ret.closing.prices, 0.99, na.rm = T)


# Fim ---------------------------------------------------------------------


# Gráfico de Histograma da Série Temporal e Retorno -----------------------

df %>% ggplot() +
  geom_histogram(aes(x = price.close), bins = 20, color = '#6C8558', fill = "#E1F7D0") +
  labs(x = 'Preço de Fechamento', y = 'Frequência') +
  theme_minimal()
df %>% ggplot() +
  geom_histogram(aes(x = ret.closing.prices), bins = 20, color = '#6C8558', fill = "#E1F7D0") +
  labs(x = 'Retorno de Fechamento', y = 'Frequência') +
  theme_minimal()


# Fim ---------------------------------------------------------------------


# Gráfico de ACF e PACF ---------------------------------------------------


ggAcf(df.xts, size = 0.7, color = '#6C8558') +
  geom_hline(yintercept = 0, size = 0.7, color = '#6C8558') +
  theme_minimal() +
  ggtitle(label = NULL)

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\ACF Cotação BOVA11.pdf", width = 7, height = 5)

ggPacf(df.xts, size = 0.7, color = '#6C8558') +
  geom_hline(yintercept = 0, size = 0.7, color = '#6C8558') +
  theme_minimal() +
  ggtitle(label = NULL)

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\PACF Cotação BOVA11.pdf", width = 7, height = 5)

tseries::adf.test(df.xts)
tseries::kpss.test(df.xts)

ggAcf(rt.xts, size = 0.7, color = '#6C8558') +
  geom_hline(yintercept = 0, size = 0.7, color = '#6C8558') +
  theme_minimal() +
  ggtitle(label = NULL)

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\ACF Retorno BOVA11.pdf", width = 7, height = 5)

ggPacf(rt.xts, size = 0.7, color = '#6C8558') +
  geom_hline(yintercept = 0, size = 0.7, color = '#6C8558') +
  theme_minimal() +
  ggtitle(label = NULL)

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\PACF Retorno BOVA11.pdf", width = 7, height = 5)

tseries::adf.test(rt.xts[-1])
tseries::kpss.test(rt.xts[-1])


# Fim ---------------------------------------------------------------------


# Gráfico de Box-Plot da Série Temporal e Retorno -------------------------

df %>% ggplot() +
  geom_violin(aes(x = 1, y = price.close), fill = "#E1F7D0", color = '#6C8558') +
  geom_boxplot(aes(x = 1, y = price.close), width = 0.3, fill = "#F5EE9C", color = '#000000') +
  labs(x = NULL, y = 'Preço de Fechamento') +
  theme_minimal()

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\Box-Plot Cotação BOVA11.pdf", width = 7, height = 5)

df %>% ggplot() +
  geom_violin(aes(x = 1, y = ret.closing.prices), fill = "#E1F7D0", color = '#6C8558') +
  geom_boxplot(aes(x = 1, y = ret.closing.prices), width = 0.3, fill = "#F5EE9C", color = '#000000')  +
  labs(x = NULL, y = 'Retorno de Fechamento') +
  theme_minimal()

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\Box-Plot Retorno BOVA11.pdf", width = 7, height = 5)

# Fim ---------------------------------------------------------------------

df %>% 
  mutate(ano_mes = format(ref.date, '%Y-%m')) %>% 
  ggplot() +
 # geom_violin(aes(x = ano_mes, y = price.close), fill = "#E1F7D0", color = '#6C8558') +
  geom_boxplot(aes(x = ano_mes, y = price.close), width = 0.3, fill = "#F5EE9C", color = '#000000')  +
  labs(x = NULL, y = 'Retorno de Fechamento') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


