library(tidyverse)

load("resultados_0.Rda")

df <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-19"))

load("resultados_1.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-20"))

df <- rbind(df,df2)

load("resultados_4.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-21"))

df <- rbind(df,df2)

load("resultados_7.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-22"))

df <- rbind(df,df2)

load("resultados_12.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-23"))

df <- rbind(df,df2)

load("resultados.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-24"))

df <- rbind(df,df2)

rm(df2,prob_grupos,simulaciones)

save(df,file = "evolucion2.Rda")

rm(list=ls())

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

load("resultados_12.Rda")

df2 <- prob_grupos |> select(id) |>
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-23"))

df <- rbind(df,df2)

load("resultados.Rda")

df2 <- prob_grupos |> select(id) |>
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-24"))

df <- rbind(df,df2)

rm(df2,prob_grupos,simulaciones)

save(df,file = "evolucion.Rda")


