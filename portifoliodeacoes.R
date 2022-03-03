# Importando os pacotes necessários para dar início às análises:
require(BETS)
require(quantmod)
require(xts)
require(ggplot2)
require(scales)
library(tidyverse)
library(BatchGetSymbols)
library(fPortfolio)
require(PerformanceAnalytics)
require(data.table)
options("getSymbols.warning4.0" = F)

# Solicitando ao usuário os ativos de interesse:
{
  acao1 <- readline(prompt = "Digite o código da sua primeira ação: ")
  acao2 <- readline(prompt = "Digite o código da sua segunda ação: ")
  acao3 <- readline(prompt = "Digite o código da sua terceira ação: ")
  acao4 <- readline(prompt = "Digite o código da sua quarta ação: ")
  acao5 <- readline(prompt = "Digite o código da sua quinta ação: ")
  acao6 <- readline(prompt = "Digite o código da sua sexta ação: ")
  acao7 <- readline(prompt = "Digite o código da sua sétima ação: ")
  acao8 <- readline(prompt = "Digite o código da sua oitava ação: ")
  acao9 <- readline(prompt = "Digite o código da sua nona ação: ")
}

# Solicitando ao usuário o período a ser analisado
{
  inicio <- readline(prompt = "Digite a data incial no formato AAAA-MM-DD: ")
  fim <- readline(prompt = "Digite a data final no formato AAAA-MM-DD: ")
}

# Criando o nome das ações
{
  nacao1 <- getSymbols(acao1, src = "yahoo")
  nacao2 <- getSymbols(acao2, src = "yahoo")
  nacao3 <- getSymbols(acao3, src = "yahoo")
  nacao4 <- getSymbols(acao4, src = "yahoo")
  nacao5 <- getSymbols(acao5, src = "yahoo")
  nacao6 <- getSymbols(acao6, src = "yahoo")
  nacao7 <- getSymbols(acao7, src = "yahoo")
  nacao8 <- getSymbols(acao8, src = "yahoo")
  nacao9 <- getSymbols(acao9, src = "yahoo")
}

# Importando os dados dos ativos de interesse:
{
  acao1 <- getSymbols(acao1, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao2 <- getSymbols(acao2, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao3 <- getSymbols(acao3, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao4 <- getSymbols(acao4, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao5 <- getSymbols(acao5, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao6 <- getSymbols(acao6, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao7 <- getSymbols(acao7, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao8 <- getSymbols(acao8, src = "yahoo", from = inicio, to = fim, auto.assign = F)
  acao9 <- getSymbols(acao9, src = "yahoo", from = inicio, to = fim, auto.assign = F)
}

# Com os dados importados, agora será necessário fazer uma manipulação simples dos dados com o intuito de corrigi-los.

{
  atv1 <- data.frame(date = index(acao1), ticker = coredata(nacao1), price = as.vector(Ad(acao1)))
  atv2 <- data.frame(date = index(acao2), ticker = coredata(nacao2), price = as.vector(Ad(acao2)))
  atv3 <- data.frame(date = index(acao3), ticker = coredata(nacao3), price = as.vector(Ad(acao3)))
  atv4 <- data.frame(date = index(acao4), ticker = coredata(nacao4), price = as.vector(Ad(acao4)))
  atv5 <- data.frame(date = index(acao5), ticker = coredata(nacao5), price = as.vector(Ad(acao5)))
  atv6 <- data.frame(date = index(acao6), ticker = coredata(nacao6), price = as.vector(Ad(acao6)))
  atv7 <- data.frame(date = index(acao7), ticker = coredata(nacao7), price = as.vector(Ad(acao7)))
  atv8 <- data.frame(date = index(acao8), ticker = coredata(nacao8), price = as.vector(Ad(acao8)))
  atv9 <- data.frame(date = index(acao9), ticker = coredata(nacao9), price = as.vector(Ad(acao9)))
}

# Neste momento, já podemos juntar esses dois ativos:
data <- rbind(atv1, atv2, atv3, atv4, atv5, atv6, atv7, atv8, atv9)

# Nesta seção serão feitos apenas alguns ajustes simples:
dt <- data.table(data)
dt[, date := as.Date(date)]
dt[, idx_price := price / price[1], by = ticker]

# Pronto, os ativos já estão prontos para serem plotados em um gráfico, para isso utilizarei os seguintes comandos:
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  theme_bw() +
  xlab("Data") +
  ylab("Preço (01-01-2018 = 1)") +
  scale_color_discrete(name = "Companhia")

# Finalmente, podemos fazer uma análise quanto o retorno desses dois ativos:
dt[, ret := price / shift(price, 1) - 1, by = ticker]
tab <- dt[!is.na(ret), .(ticker, ret)]
tab <- tab[, .(RE = mean(ret), DV = sd(ret)), by = "ticker"]
tab

# Concluindo, para tornar essa análise visual e de fácil entendimento para um possível cliente investidor, temos que executar o seguinte comando:
ggplot(tab, aes(x = DV, y = RE, color = ticker)) +
  geom_point(size = 5) +
  xlab("Volatilidade") +
  ylab("Retorno") +
  scale_y_continuous(label = percent) +
  scale_x_continuous(label = percent)

# Inicialmente irei apenas montar o retorno desses dois ativos com os seguintes comandos:
{
  r1 <- diff(log(acao1[, 6]))
  r1 <- r1[-1, ]
  r1 <- apply.monthly(r1, mean)

  r2 <- diff(log(acao2[, 6]))
  r2 <- r2[-1, ]
  r2 <- apply.monthly(r2, mean)

  r3 <- diff(log(acao3[, 6]))
  r3 <- r3[-1, ]
  r3 <- apply.monthly(r3, mean)

  r4 <- diff(log(acao4[, 6]))
  r4 <- r4[-1, ]
  r4 <- apply.monthly(r4, mean)

  r5 <- diff(log(acao5[, 6]))
  r5 <- r5[-1, ]
  r5 <- apply.monthly(r5, mean)

  r6 <- diff(log(acao6[, 6]))
  r6 <- r6[-1, ]
  r6 <- apply.monthly(r6, mean)

  r7 <- diff(log(acao7[, 6]))
  r7 <- r7[-1, ]
  r7 <- apply.monthly(r7, mean)

  r8 <- diff(log(acao8[, 6]))
  r8 <- r8[-1, ]
  r8 <- apply.monthly(r8, mean)

  r9 <- diff(log(acao9[, 6]))
  r9 <- r9[-1, ]
  r9 <- apply.monthly(r9, mean)
}

retornos <- cbind(r1, r2, r3, r4, r5, r6, r7, r8, r9)
retornos <- na.omit(retornos)
retornos <- as.timeSeries(retornos)

# Feito isso, já é possível montar os dois portfólios hipotéticos, vamos para o primeiro - aquele que foca na eficiência - o ponto de tangência:
portfolio.eficiente <- tangencyPortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio.eficiente

# Para obter os dados da carteira com o menor risco, utilizaremos o seguinte comando:
portfolio.menor.risco <- minvariancePortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio.menor.risco

# Só para fim de curiosidade, para tornar essa alocação de portifólio um pouco mais visual, podemos plotar o gráfico de eficiência de markowitz com os seguintes comandos:
fronteira <- portfolioFrontier(retornos)
frontierPlot(fronteira, col = c("blue", "red"), pch = 20)
monteCarloPoints(fronteira, mcSteps = 5000, pch = 20, cex = 0.25)