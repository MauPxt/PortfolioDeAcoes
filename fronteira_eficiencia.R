require(quantmod)
require(PerformanceAnalytics)
library(fPortfolio)

# Importando os dados dos ativos de interesse
getSymbols("BPAN4.SA", src = "yahoo")
getSymbols("BOVA11.SA", src = "yahoo")

# Calculando o retorno dos ativos
p <- BPAN4.SA$BPAN4.SA.Close
p <- na.omit(p)
r <- Return.calculate(p)
rb <- apply.weekly(r, mean)

p <- BOVA11.SA$BOVA11.SA.Close
p <- na.omit(p)
r <- Return.calculate(p)
ri <- apply.weekly(r, mean)

retornos <- cbind(rb, ri)
retornos <- na.omit(retornos)
retornos <- as.timeSeries(retornos)

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