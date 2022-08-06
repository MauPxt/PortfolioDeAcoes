require(quantmod)
require(data.table)
require(ggplot2)
require(scales)

# Variável que armazena o código dos papéis de interesse
ativos <- c("BOVA11.SA", "SANB11.SA", "BBAS3.SA", "BBDC4.SA", "ITUB4.SA")

# Laço de repetição para cada papel de interesse
portfolio <- NULL
for (elemento in ativos) {
    # obtém os dados do papel de interesse
    acao <- getSymbols(
        elemento,
        src = "yahoo",
        from = "2018-01-01",
        to = "2022-01-01",
        auto.assign = F
    )
    # junção dos dados do papel de interesse em um dataframe
    portfolio <- rbind(
        portfolio,
        data.frame(
            data = index(acao),
            ativo = coredata(elemento),
            preco = as.vector(Ad(acao))
        )
    )
}
# removendo valores nulos do dataframe
portfolio <- na.omit(portfolio)


# cálculo da variação do preço de cada papel
dt <- data.table(portfolio)
dt[, data := as.Date(data)]
dt[, idx_preco := preco / preco[1], by = ativo]

# plotagem do gráfico do comportamento do preço dos papéis
ggplot(dt, aes(x = data, y = idx_preco, color = ativo)) +
    geom_line() +
    theme_bw() +
    xlab("Data") +
    ylab("Variação") +
    scale_color_discrete(name = "Ativo")


# cálculo do retorno dos papéis
dt[, ret := preco / shift(preco, 1) - 1, by = ativo]
tab <- dt[!is.na(ret), .(ativo, ret)]
tab <- tab[, .(RE = mean(ret), DV = sd(ret)), by = "ativo"]


# plotagem do gráfico do risco e retorno dos papéis
ggplot(tab, aes(x = DV, y = RE, color = ativo)) +
    geom_point(size = 5) +
    xlab("Volatilidade") +
    ylab("Retorno") +
    scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent) +
    scale_color_discrete(name = "Ativo")
