###Portugues (English)
#Pacotes(Packages)
require(raster)
require(SiMRiv)
require(adehabitatLT)

###abrindo arquivo
#Raster de resist6encia de cada bicho
mapa=raster(file.choose())
NAvalue(mapa)<- 0
crs(mapa)=("+proj=utm +zone=22+south+datum=WGS84")



n=806
estado=n/2

PAR.test <- data.frame(state = rep(c("RW", "Res.CRW"), each = estado), # "RW", "Res.CRW" or "RW.CRW"
                       auto.cor = runif(n, min = 0.5, max = 0.9), 
                       state1 = runif(n, min = 0.1, max = 0.5),
                       state2 = runif(n, min = 0.1, max = 0.5),
                       step.lentgh = 5)
PAR.test
ind=make.n.ind(PAR.test)

nsim=10000
resu.simu=simulate(ind,nsim,resist =mapa,start.resistance=0.2)
resu.thous.287=org(resu.simu, n, nsim)
write.csv(resu.thous.287,"resu.thous.287.csv")

##Se precisar paralelizar
require((doParallel))
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
###Entrar o script aqui
stopCluster(cl)

