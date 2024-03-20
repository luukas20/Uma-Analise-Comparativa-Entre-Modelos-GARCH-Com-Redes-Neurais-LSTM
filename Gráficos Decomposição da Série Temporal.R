
# Instale o pacote "stats" se ainda não estiver instalado
# install.packages("stats")

# Carregue o pacote
library(stats)
bd <- BatchGetSymbols::BatchGetSymbols('^BVSP', first.date = as.Date('1995-01-01'), last.date = Sys.Date())


df <- bd$df.tickers |>
  dplyr::select(ref.date,price.adjusted) |>
  dplyr::mutate(ref.date = as.Date(ref.date),
                ano_mes = format(ref.date, "%Y-%m")) |>
  dplyr::group_by(ano_mes) |>
  dplyr::mutate(media_mensal = mean(price.adjusted),
                dt = max(ref.date)) |>
  dplyr::ungroup() |>
  dplyr::select(dt,media_mensal) |>
  dplyr::distinct()


# Crie uma série temporal de exemplo (substitua isso pelos seus próprios dados)
x <- ts(df$media_mensal, start = c(1995, 01), end = c(2023, 12), frequency = 12)

# Decomponha a série temporal
m <- decompose(x)

plot(m)

p1 <- ggplot2::autoplot(m$x) +
  labs(y = 'Série') +
  theme_bw() +
  theme(axis.title.x = element_blank(),  # Remove o título do eixo X
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        axis.ticks.x = element_blank(),
        plot.margin =  margin(0.1, 0.5, 0.1, 0.1, "cm"))

p2 <- ggplot2::autoplot(m$trend) +
  labs(y = 'Tendência') +
  theme_bw() +
  theme(axis.title.x = element_blank(),  # Remove o título do eixo X
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"))

p3 <- ggplot2::autoplot(m$seasonal) +
  labs(y = 'Sazonal') +
  theme_bw() +
  theme(axis.title.x = element_blank(),  # Remove o título do eixo X
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        axis.ticks.x = element_blank(),
        plot.margin =  margin(0.1, 0.5, 0.1, 0.1, "cm"))

p4 <- ggplot2::autoplot(m$random) +
  labs(y = 'Resíduos') +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6),
        plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"))

gridExtra::grid.arrange(p1,p2,p3,p4, ncol = 1, nrow = 4)

dev.copy2pdf(file="C:\\Users\\lucas\\OneDrive - 5qgclf\\Documentos\\UFS\\10 Periodo\\TCC\\TCC Latex\\figuras\\Decomposição.pdf", width = 7, height = 5)
