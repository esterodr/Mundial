library(tidyverse)

load("resultados_0.Rda")

df <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-19"))

load("resultados_16.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-24"))

df <- rbind(df,df2)

load("resultados_32.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-28"))

df <- rbind(df,df2)

load("resultados.Rda")

df2 <- prob_grupos |> select(id,P1) |>
  mutate(Fecha=as.Date("2022-11-29"))

df <- rbind(df,df2)

rm(df2,prob_grupos,simulaciones)

save(df,file = "evolucion2.Rda")

rm(list=ls())

load("resultados_0.Rda")

df <- prob_grupos |> select(id) |>
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-19"))

load("resultados_16.Rda")

df2 <- prob_grupos |> select(id) |>
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-24"))

df <- rbind(df,df2)

load("resultados_32.Rda")

df2 <- prob_grupos |> select(id) |>
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-28"))

df <- rbind(df,df2)

load("resultados.Rda")

df2 <- prob_grupos |> select(id) |>
  mutate(Posición=seq(1,32,1),
         Fecha=as.Date("2022-11-29"))

df <- rbind(df,df2)

rm(df2,prob_grupos,simulaciones)

save(df,file = "evolucion.Rda")


