library(tidyverse)

load("resultados_0.Rda")

df <- prob_grupos |> select(id) |> 
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-19"))

load("resultados_1.Rda")

df2 <- prob_grupos |> select(id) |> 
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-20"))

df <- rbind(df,df2)

load("resultados_4.Rda")

df2 <- prob_grupos |> select(id) |> 
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-21"))

df <- rbind(df,df2)

load("resultados_7.Rda")

df2 <- prob_grupos |> select(id) |> 
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-22"))

df <- rbind(df,df2)

load("resultados.Rda")

df2 <- prob_grupos |> select(id) |> 
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-23"))

df <- rbind(df,df2)

rm(df2,prob_grupos,simulaciones)

save(df,file = "evolucion.Rda")


df |> mutate(Posición=-1*Posición) |> 
  ggplot(aes(x=Fecha,y=Posición,color=id)) +
  geom_line(size=1) +
  geom_point(aes(fill=id),shape=21,size=2,color="black") +
  scale_y_continuous(breaks = unique(-1*df$Posición),
                     labels = df$id[df$Fecha==min(df$Fecha)],
                     sec.axis = sec_axis(~ . * 1,breaks = unique(-1*df$Posición),
                                         labels = df$id[df$Fecha==max(df$Fecha)])) +
  scale_x_continuous(breaks = c(0,1,2)) +
  labs(x="",y="") +
  theme(legend.position = "null",
        panel.background = element_blank(),
        axis.ticks = element_blank())