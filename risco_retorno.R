require(quantmod)
require(data.table)
require(ggplot2)
require(scales)

# Importando dados dos ativos de interesse
bpan <-
    getSymbols(
        "BPAN4.SA",
        src = "yahoo",
        from = "2018-01-01",
        to = "2021-11-01",
        auto.assign = F
    )
bova <-
    getSymbols(
        "BOVA11.SA",
        src = "yahoo",
        from = "2018-01-01",
        to = "2021-11-01",
        auto.assign = F
    )

# Transformando os dados em data frames
banco_pan <-
    data.frame(
        date = index(bpan),
        ticker = coredata("BPAN"),
        price = as.vector(Ad(bpan))
    )
ibovespa <-
    data.frame(
        date = index(bova),
        ticker = coredata("BOVA"),
        price = as.vector(Ad(bova))
    )


data <- rbind(banco_pan, ibovespa)

# Manipulando os dados com a função data table
dt <- data.table(data)
dt[, date := as.Date(date)]
dt[, idx_price := price / price[1], by = ticker]

# Plotando um gráfico com os desempenhos dos dois ativos
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
    geom_line() +
    theme_bw() +
    xlab("Data") +
    ylab("Preço (01-01-2018 = 1)") +
    scale_color_discrete(name = "Companhia")

# Manipulando os dados para obter uma melhor visualização dos retornos
dt[, ret := price / shift(price, 1) - 1, by = ticker]
tab <- dt[!is.na(ret), .(ticker, ret)]
tab <- tab[, .(RE = mean(ret), DV = sd(ret)), by = "ticker"]
tab

# Plotando o gráfico dos retornos dos dois ativos
ggplot(tab, aes(x = DV, y = RE, color = ticker)) +
    geom_point(size = 5) +
    xlab("Volatilidade") +
    ylab("Retorno") +
    scale_y_continuous(label = percent) +
    scale_x_continuous(label = percent)