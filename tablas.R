library(tidyverse)
load("probabilidades.Rda")

Grupos <- tibble(grupo=c(rep("A",4),rep("B",4),rep("C",4),rep("D",4),
                 rep("E",4),rep("F",4),rep("G",4),rep("H",4)),
                 id=c(1:32),
                 Puntos=rep(0,32),Posición=NA)

equipos <- tibble(Equipo=c("Qatar","Ecuador","Senegal","Holanda",
                           "Inglaterra","Irán","Estados Unidos","Gales",
                           "Argentina","Arabia Saudita","México","Polonia",
                           "Francia","Australia","Dinamarca","Túnez",
                           "España","Costa Rica","Alemania","Japón",
                           "Bélgica","Canadá","Marruecos","Croacia",
                           "Brasil","Serbia","Suiza","Camerún",
                           "Portugal","Uruguay","Ghana","Corea del Sur"),
                  id=c(1:32))

for(i in(1:nrow(probabilidades))) {
  probabilidades$EquipoA[i] <- equipos$id[equipos$Equipo==probabilidades$EquipoA[i]]
  probabilidades$EquipoB[i] <- equipos$id[equipos$Equipo==probabilidades$EquipoB[i]]
}

clasificaciones <- tibble(Posicion=c("Campeón","Subcampeón","Semifinal",
                                     "Cuartos","Octavos","G1","G2","G3","G4"),
                          id_posicion=c(1:9))

fixture_grupos <- tibble(grupo=c(rep("A",6),rep("B",6),rep("C",6),rep("D",6),
                                 rep("E",6),rep("F",6),rep("G",6),rep("H",6)),
                         A=c(c(1,1,1,2,2,3),c(1,1,1,2,2,3)+4,c(1,1,1,2,2,3)+8,
                             c(1,1,1,2,2,3)+12,c(1,1,1,2,2,3)+16,
                             c(1,1,1,2,2,3)+20,c(1,1,1,2,2,3)+24,
                             c(1,1,1,2,2,3)+28),
                         B=c(c(2,3,4,3,4,4),c(2,3,4,3,4,4)+4,c(2,3,4,3,4,4)+8,
                             c(2,3,4,3,4,4)+12,c(2,3,4,3,4,4)+16,
                             c(2,3,4,3,4,4)+20,c(2,3,4,3,4,4)+24,
                             c(2,3,4,3,4,4)+28),
                         Resultado=NA)

#N <- 100000
N <- 25000

# Columnas: simulacion, equipo, posiciones 11
# Files N*32
simulaciones <- as.matrix(data.frame(N=rep(c(1:N),32)[order(rep(c(1:N),32))],
                                     id=rep(0,32*N),
                                     P1=rep(0,32*N),
                                     P2=rep(0,32*N),
                                     P3=rep(0,32*N),
                                     P4=rep(0,32*N),
                                     P5=rep(0,32*N),
                                     P6=rep(0,32*N),
                                     P7=rep(0,32*N),
                                     P8=rep(0,32*N),
                                     P9=rep(0,32*N)))

## Simular grupos

p <- as.matrix(data.frame(P1=rep(NA,48),
               P2=rep(NA,48),
               P3=rep(NA,48)))
sim <- fixture_grupos
for(i in(1:48)) {
  x <- sim[i,]
  if (nrow(probabilidades[probabilidades$EquipoA==as.character(x[2])&
                          probabilidades$EquipoB==as.character(x[3]),])>0) {
    p[i,] <- as.numeric(probabilidades[probabilidades$EquipoA==as.character(x[2])&
                                     probabilidades$EquipoB==as.character(x[3]),c(3:5)])
  } else {
    p[i,] <- as.numeric(probabilidades[probabilidades$EquipoB==as.character(x[2])&
                                     probabilidades$EquipoA==as.character(x[3]),c(3:5)])[c(3,2,1)]
  }
}

options(dplyr.summarise.inform = FALSE)

Ns <- 0
ss <- replicate(N, {
  Ns <<- Ns+1
  
  for(i in (1:48)) {
    sim$Resultado[i] <- sample(c(3,1,0),1,prob=p[i,])
  }
  
  sim$ResultadoB <- ifelse(sim$Resultado==3,0,
                           ifelse(sim$Resultado==1,1,3))
  pos <- sim %>% select(grupo,A,Resultado)
  posB <- sim %>% select(grupo,B,ResultadoB)
  names(posB) <- c("grupo","A","Resultado")
  pos <- rbind(pos,posB)
  pos <- pos %>% group_by(grupo,A) %>% 
    summarise(Puntos=sum(Resultado)) %>% 
    ungroup() %>%
    mutate(Puntos=Puntos+rep(sample(c(.01,.011,.012,.013),4,replace = FALSE),8)) %>% 
    arrange(grupo,desc(Puntos))
  
  primeros <- pos %>% group_by(grupo) %>% filter(row_number()==1) %>% ungroup()
  segundos <- pos %>% group_by(grupo) %>% filter(row_number()==2) %>% ungroup()
  terceros <- pos %>% group_by(grupo) %>% filter(row_number()==3) %>% ungroup()
  cuartos <- pos %>% group_by(grupo) %>% filter(row_number()==4) %>% ungroup()
  simulaciones[(Ns-1)*32+c(1:32),2] <<- c(primeros$A,segundos$A,terceros$A,cuartos$A)
  simulaciones[(Ns-1)*32+c(1:8),8] <<- rep(1,8)
  simulaciones[(Ns-1)*32+c(9:16),9] <<- rep(1,8)
  simulaciones[(Ns-1)*32+c(17:24),10] <<- rep(1,8)
  simulaciones[(Ns-1)*32+c(25:32),11] <<- rep(1,8)
  
  ## Octavos
  octavos <- data.frame(A=c(primeros$A[c(1,3,4,2,5,7,6,8)]),
                        B=c(segundos$A[c(2,4,3,1,6,8,5,7)]),
                        Resultado=NA)
  for(i in(1:8)) {
    x <- octavos[i,]
    if (nrow(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                            probabilidades$EquipoB==as.character(x[2]),])>0) {
      p <- as.numeric(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                                           probabilidades$EquipoB==as.character(x[2]),c(6:7)])
    } else {
      p <- as.numeric(probabilidades[probabilidades$EquipoB==as.character(x[1])&
                                           probabilidades$EquipoA==as.character(x[2]),c(6:7)])[c(2,1)]
    }
    octavos$Resultado[i] <- sample(c(1,0),1,prob=p)
  }
  
  #oct_G <- c(octavos$A[octavos$Resultado==1],octavos$B[octavos$Resultado==0])
  oct_P <- c(octavos$A[octavos$Resultado==0],octavos$B[octavos$Resultado==1])
  #id1 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% oct_G)+(Ns-1)*32
  id2 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% oct_P)+(Ns-1)*32
  #simulaciones[id1,6] <<- rep(1,8)
  simulaciones[id2,7] <<- rep(1,8)
  
  ## Cuartos
  
  cuartos1 <- octavos %>% select(A,B,Resultado)
  cuartos1$A[cuartos1$Resultado==0] <- cuartos1$B[cuartos1$Resultado==0]
  cuartos <- data.frame(A=c(cuartos1$A[c(1,3,5,7)]),
                        B=c(cuartos1$A[c(2,4,6,8)]),
                        Resultado=NA)
  
  for(i in(1:4)) {
    x <- cuartos[i,]
    if (nrow(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                            probabilidades$EquipoB==as.character(x[2]),])>0) {
      p <- as.numeric(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                                       probabilidades$EquipoB==as.character(x[2]),c(6:7)])
    } else {
      p <- as.numeric(probabilidades[probabilidades$EquipoB==as.character(x[1])&
                                       probabilidades$EquipoA==as.character(x[2]),c(6:7)])[c(2,1)]
    }
    cuartos$Resultado[i] <- sample(c(1,0),1,prob=p)
  }
  
  #cua_G <- c(cuartos$A[cuartos$Resultado==1],cuartos$B[cuartos$Resultado==0])
  cua_P <- c(cuartos$A[cuartos$Resultado==0],cuartos$B[cuartos$Resultado==1])
  #id1 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% cua_G)+(Ns-1)*32
  id2 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% cua_P)+(Ns-1)*32
  #simulaciones[id1,5] <<- rep(1,4)
  simulaciones[id2,6] <<- rep(1,4)
  
  ## Semis
  
  semis1 <- cuartos %>% select(A,B,Resultado)
  semis1$A[semis1$Resultado==0] <- semis1$B[semis1$Resultado==0]
  semis <- data.frame(A=c(semis1$A[c(1,3)]),
                        B=c(semis1$A[c(2,4)]),
                        Resultado=NA)
  
  for(i in(1:2)) {
    x <- semis[i,]
    if (nrow(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                            probabilidades$EquipoB==as.character(x[2]),])>0) {
      p <- as.numeric(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                                       probabilidades$EquipoB==as.character(x[2]),c(6:7)])
    } else {
      p <- as.numeric(probabilidades[probabilidades$EquipoB==as.character(x[1])&
                                       probabilidades$EquipoA==as.character(x[2]),c(6:7)])[c(2,1)]
    }
    semis$Resultado[i] <- sample(c(1,0),1,prob=p)
  }
  
  #semi_G <- c(semis$A[semis$Resultado==1],semis$B[semis$Resultado==0])
  semi_P <- c(semis$A[semis$Resultado==0],semis$B[semis$Resultado==1])
  #id1 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% semi_G)+(Ns-1)*32
  id2 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% semi_P)+(Ns-1)*32
  #simulaciones[id1,4] <<- rep(1,2)
  simulaciones[id2,5] <<- rep(1,2)
  
  ## Final
  
  fin1 <- semis %>% select(A,B,Resultado)
  fin1$A[fin1$Resultado==0] <- fin1$B[fin1$Resultado==0]
  fin <- data.frame(A=c(fin1$A[c(1)]),
                      B=c(fin1$A[c(2)]),
                      Resultado=NA)
  
  x <- fin[1,]
  if (nrow(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                          probabilidades$EquipoB==as.character(x[2]),])>0) {
    p <- as.numeric(probabilidades[probabilidades$EquipoA==as.character(x[1])&
                                     probabilidades$EquipoB==as.character(x[2]),c(6:7)])
  } else {
    p <- as.numeric(probabilidades[probabilidades$EquipoB==as.character(x[1])&
                                     probabilidades$EquipoA==as.character(x[2]),c(6:7)])[c(2,1)]
  }
  fin$Resultado[1] <- sample(c(1,0),1,prob=p)
  
  
  fin_G <- c(fin$A[fin$Resultado==1],fin$B[fin$Resultado==0])
  fin_P <- c(fin$A[fin$Resultado==0],fin$B[fin$Resultado==1])
  id1 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% fin_G)+(Ns-1)*32
  id2 <- which(simulaciones[((Ns-1)*32+1):(Ns*32),2] %in% fin_P)+(Ns-1)*32
  simulaciones[id1,3] <<- 1
  simulaciones[id2,4] <<- 1
  
})


prob_grupos <- as.data.frame(simulaciones) %>% 
  select(-N) %>% 
  group_by(id) %>% 
  summarise_all(sum) %>% 
  ungroup()

prob_grupos$P1 <- prob_grupos$P1/N
prob_grupos$P2 <- prob_grupos$P2/N
prob_grupos$P3 <- prob_grupos$P3/N
prob_grupos$P4 <- prob_grupos$P4/N
prob_grupos$P5 <- prob_grupos$P5/N
prob_grupos$P6 <- prob_grupos$P6/N
prob_grupos$P7 <- prob_grupos$P7/N
prob_grupos$P8 <- prob_grupos$P8/N
prob_grupos$P9 <- prob_grupos$P9/N

for(i in (1:32)) {
  prob_grupos$id[i] <- equipos$Equipo[equipos$id==prob_grupos$id[i]]
}

prob_grupos <- prob_grupos %>% arrange(desc(P1))

save(prob_grupos,simulaciones,file="resultados.Rda")
