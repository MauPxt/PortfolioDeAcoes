require(quantmod)
require(PerformanceAnalytics)
library(fPortfolio)

# criando uma função para calcular os retornos. A função consegue receber uma quantidade indefinida de papéis
calcularRetornos <- function(...) {
  # Pegar todos os simbolos passados para a função
  symbols <- c(...)

  # Baixear os dados e calcular o retorno de cada um dos papéis
  listaRetornos <- purrr::map(symbols, function(symbol) {
    p <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
    p <- Cl(p)
    r <- Return.calculate(p)
    rb <- apply.weekly(r, mean)
    return(rb)
  })

  # Convertendo o resultado em uma série temporal
  retornos <- as.timeSeries(listaRetornos)
  
  return(retornos)
}

# Chamando a função com os papéis
retornos <- calculate_returns("BOVA11.SA","BPAN4.SA")

# Montando um portfólio com foco na eficiencia
portfolio_eficiente <-
  tangencyPortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio_eficiente

# Montando um portfólio com foco na minimização dos riscos
portfolio_menor_risco <-
  minvariancePortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio_menor_risco

# Elaborando e plotando a fronteira de markowitz dos ativos
fronteira <- portfolioFrontier(retornos)
frontierPlot(fronteira, col = c("blue", "red"), pch = 20)
monteCarloPoints(fronteira,
                 mcSteps = 5000,
                 pch = 20,
                 cex = 0.25)
