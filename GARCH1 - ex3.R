# autor: Matheus Vanzela
# Exercício 3 a-b
# Resolvi usar outro pacote para teste


# Function to calculate persistency
persist <- function(modelo.garch){
  persistence <- modelo.garch@fit[["coef"]][["beta1"]]+modelo.garch@fit[["coef"]][["alpha1"]]+modelo.garch@fit[["coef"]][["omega"]]
  return(persistence)
}

# Function to calculate half life
hl <- function(persistencia){
  L <- log(0.5)/log(persistencia)
  return(L)
}

# Downloading PETR4 data from Yahoo Finance
petr4 <- get.hist.quote(instrument = "PETR4.SA", start = "2019-01-01", end = Sys.Date(), quote = "Close")
ibov <- get.hist.quote(instrument = "^BVSP", start = "2019-01-01", end = Sys.Date(), quote = "Close")

# Eliminating NA's
petr4 = petr4[is.na(petr4)==F] 
ibov = ibov[is.na(ibov)==F]

# Visualize
plot(petr4)
plot(ibov)

# Calculating returns
petr4_returns <- diff(log(petr4))
ibov_returns <- diff(log(ibov))

# Fitting the model
# t-Student
garch.petr.t = garchFit(formula = ~garch(1,1), data = petr4_returns, cond.dist = "std", trace = FALSE)
garch.ibov.t = garchFit(formula = ~garch(1,1), data = ibov_returns, cond.dist = "std", trace = FALSE)
garch.ibov.norm = garchFit(formula = ~garch(1,1), data = ibov_returns, cond.dist = "norm", trace = FALSE)
garch.petr.norm = garchFit(formula = ~garch(1,1), data = petr4_returns, cond.dist = "norm", trace = FALSE)

# Persistency
# IBOV
p.ibov.t <- persist(garch.ibov.t)
p.ibov.t
p.ibov.n <- persist(garch.ibov.norm)
p.ibov.n
hl(p.ibov.t)
hl(p.ibov.n)
# A persistencia e a half life relacionada a distribuição t-Student é maior, 
# indicando que os choques tem maior representação quando modelados pela distribuição t-Student

#PETROBRAS
p.petr.t <- persist(garch.petr.t)
p.petr.t
p.petr.n <- persist(garch.petr.norm)
p.petr.n
hl(p.petr.t)
hl(p.petr.n)
# No caso da PETR4 a diferença mostrou-se ainda maior. É importante destacar que nos dois testes, tanto para o IBOV quanto para PETR
# a persistencia tem um valor próximo de 1, indicando uma alta persistência dos choques.
# Ainda é importante observar que a diferença entre os valores de Half Life no caso de PETR4 é grande o suficiente para afirmar que
# um modelo de volatilidade descrito pela normal voltaria mais rapidamente à volatidade média do que pela t-Student. 
