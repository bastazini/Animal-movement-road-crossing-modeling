###Portugues(English)
#Pacotes (pacakges)
require(SiMRiv)

# Parametro para 1 individuo (Parameter for 1 individual)
PAR <- data.frame(state = "RW", # "RW", "Res.CRW" or "RW.CRW"
                  auto.cor = c(0.9), 
                  state1 = c(0.5), 
                  state2 = c(0.5),
                  step.lentgh = c(25))

# Funcao para simular 1 individuo (generating 1 indvidual)
# Precisa entrar com os parametros (Enter parameters)
# parameters eh um data.frame com apenas 1 linha e os parametros nomeados conforme acima
make.ind <- function(parameters){
  if(parameters$state == "RW"){
    ind <- species(state.RW())  
  }
  if(parameters$state == "Res.CRW"){
    ind <- species(state.Resting() + state.CRW(parameters$auto.cor), 
                   trans = transitionMatrix(parameters$state1, parameters$state2)) + parameters$step.lentgh
  }
  if(parameters$state == "RW.CRW"){
    ind <- species(state.RW() + state.CRW(parameters$auto.cor)
                   , trans = transitionMatrix(parameters$state1, parameters$state2)) + parameters$step.lentgh
  }
  return(ind)
}

# Por exemplo simular um individuo (Exemple for simulating 1 individual)
make.ind(PAR[1,])

# Funcao para gerar n individuos (generating n individuals)
# Nesse caso parameters eh um data.frame com n linhas, cada linha com os parametros para 1 individuo
make.n.ind <- function(parameters){
  inds <- lapply(1:nrow(parameters), function(i) make.ind(parameters[i,]))  
  return(inds)
}

PAR <- data.frame(state = c("RW.CRW", "Res.CRW"), # "RW", "Res.CRW" or "RW.CRW"
                  auto.cor = c(0.9, 0.5), 
                  state1 = c(0.5), 
                  state2 = c(0.5),
                  step.lentgh = c(25))
PAR
# Por exemplo
make.n.ind(PAR)


# Em teoria so precisaria gerar esse data.frame com os parametros
# Poderias usar a funcao rep, sample, runif,... para gerar esse data.frame
#definir o numero de individuos (defining  number of individuals)
n=20
estado=n/2

PAR.test <- data.frame(state = rep(c("RW.CRW", "Res.CRW"), each = estado), # "RW", "Res.CRW" or "RW.CRW"
                       auto.cor = runif(n, min = 0.5, max = 0.9), 
                       state1 = runif(n, min = 0.5, max = 0.5),
                       state2 = runif(n, min = 0.5, max = 0.5),
                       step.lentgh = runif(n, min = 5, max = 29))
PAR.test
make.n.ind(PAR.test)



