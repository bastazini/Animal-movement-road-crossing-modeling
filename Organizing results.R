#Portugues (Ingles)

#Função para organizar a matriz de resultados, para que tenha 4 colunas: individuo, posição X, posição Y e estado (Function to re-organize the result matrix with four columns: individual, position X, position Y, and state)
org <- function(res, n, nsim){
  result <- matrix(NA, nrow = nsim*n, ncol = 4)
  result[,1] <- rep(1:n, each = nsim)
  index1 <- seq(from = 1, to = n*3, by = 3)
  index2 <- seq(from = 1, to = nsim*n, by = nsim)
  for(i in 1:n){
    result[index2[i]:(index2[i]+nsim-1), 2:4] <- res[1:nsim, index1[i]:(index1[i]+2)]
  }
  return(result)  
}
