require(quantmod)
require(PerformanceAnalytics)
library(fPortfolio)

# Função para calcular os retornos semanais de uma lista de ativos
calcularRetornos <- function(symbols, de='2020-01-01', ate='2020-12-31') {
  retornos <- lapply(symbols, function(symbol) {
    data <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE, from = de, to = ate)
    p <- data[, paste0(symbol, ".Close")]
    r <- Return.calculate(na.omit(p))
    apply.weekly(r, mean)
  })

  retornos <- na.omit(do.call(cbind, retornos))
  as.timeSeries(retornos)
}

# Lista de ativos
acoes <- c("BOVA11.SA", "SANB11.SA", "BBAS3.SA", "BBDC4.SA", "ITUB4.SA")

# Calculando os retornos semanais
retornos <- calcularRetornos(acoes, de='2021-01-01', ate='2022-12-31')

# Construindo o portfólio que tangencia a fronteira de eficiência
portfolio_eficiente <-
  tangencyPortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio_eficiente

# Construindo o portfólio de menor risco
portfolio_menor_risco <-
  minvariancePortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio_menor_risco

# Construindo a fronteira de eficiência
fronteira <- portfolioFrontier(retornos)

# Plot da fronteira de eficiência
frontierPlot(fronteira, col = c("blue", "red"), pch = 20, main = "Fronteira de Eficiência",
             xlab = "Risco", ylab = "Retorno")

# Adicionando alguns pontos na fronteira utilizando o método de Monte Carlo
monteCarloPoints(fronteira,
                 mcSteps = 50,
                 pch = 20,
                 cex = 0.25)
