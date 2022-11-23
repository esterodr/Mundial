# Código sacado de https://edomt.github.io/Elo-R-WorldCup/

library(tidyverse)

# link https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017
matches <- read_csv("results.csv") 
# matches$home_score[matches$date=="2022-11-20"] <- 0
# matches$away_score[matches$date=="2022-11-20"] <- 2
# matches$home_score[matches$date=="2022-11-21"&matches$home_team=="Senegal"] <- 0
# matches$away_score[matches$date=="2022-11-21"&matches$home_team=="Senegal"] <- 2
# matches$home_score[matches$date=="2022-11-21"&matches$home_team=="England"] <- 6
# matches$away_score[matches$date=="2022-11-21"&matches$home_team=="England"] <- 2
# matches$home_score[matches$date=="2022-11-21"&matches$home_team=="United States"] <- 1
# matches$away_score[matches$date=="2022-11-21"&matches$home_team=="United States"] <- 1
# matches$home_score[matches$date=="2022-11-22"&matches$home_team=="Argentina"] <- 1
# matches$away_score[matches$date=="2022-11-22"&matches$home_team=="Argentina"] <- 2
# matches$home_score[matches$date=="2022-11-22"&matches$home_team=="Mexico"] <- 0
# matches$away_score[matches$date=="2022-11-22"&matches$home_team=="Mexico"] <- 0
# matches$home_score[matches$date=="2022-11-22"&matches$home_team=="Denmark"] <- 0
# matches$away_score[matches$date=="2022-11-22"&matches$home_team=="Denmark"] <- 0
# matches$home_score[matches$date=="2022-11-22"&matches$home_team=="France"] <- 4
# matches$away_score[matches$date=="2022-11-22"&matches$home_team=="France"] <- 1
# matches$home_score[matches$date=="2022-11-23"&matches$home_team=="Germany"] <- 1
# matches$away_score[matches$date=="2022-11-23"&matches$home_team=="Germany"] <- 2
# #matches$home_score[matches$date=="2022-11-23"&matches$home_team=="Spain"] <- 4
# #matches$away_score[matches$date=="2022-11-23"&matches$home_team=="Spain"] <- 1
# matches$home_score[matches$date=="2022-11-23"&matches$home_team=="Morocco"] <- 0
# matches$away_score[matches$date=="2022-11-23"&matches$home_team=="Morocco"] <- 0
# #matches$home_score[matches$date=="2022-11-23"&matches$home_team=="Belgium"] <- 4
# #matches$away_score[matches$date=="2022-11-23"&matches$home_team=="Belgium"] <- 1

matches <- matches |> filter(!is.na(home_score))

teams <- data.frame(team=unique(c(matches$home_team,matches$away_team)))
teams$elo <- 1500
matches <- matches %>% 
  mutate(result = if_else(home_score>away_score,1,
                          if_else(home_score==away_score,0.5,0)))

library(elo)

for(i in seq_len(nrow(matches))) {
  
  match <- matches[i,]
  
  #Pre-match ratings
  teamA_elo <- subset(teams, team==match$home_team)$elo
  teamB_elo <- subset(teams, team==match$away_team)$elo
  
  #Update elo
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 30)
  teamA_new_elo <- new_elo[1,1]
  teamB_new_elo <- new_elo[1,2]
  
  teams <- teams %>% 
    mutate(elo = if_else(team == match$home_team, teamA_new_elo,
                         if_else(team == match$away_team, teamB_new_elo, elo)))
}

teams %>% arrange(desc(elo))

save(teams,file="elo.Rda")

Clasificados <- c("Qatar", "Alemania", "Bélgica", "Francia",
                  "Croacia", "Dinamarca","Inglaterra","Suiza",
                  "Serbia","España","Holanda","Portugal","Polonia",
                  "Gales","Brasil","Argentina","Ecuador","Uruguay",
                  "Canadá", "México","Estados Unidos", "Costa Rica",
                  "Marruecos","Túnez", "Camerún", "Ghana","Senegal",
                  "Irán", "Corea del Sur", "Japón", "Arabia Saudita",
                  "Australia" )

Clasificados_en <- c("Qatar", "Germany", "Belgium", "France",
                  "Croatia", "Denmark","England","Switzerland",
                  "Serbia","Spain","Netherlands","Portugal","Poland",
                  "Wales","Brazil","Argentina","Ecuador","Uruguay",
                  "Canada", "Mexico","United States", "Costa Rica",
                  "Morocco","Tunisia", "Cameroon", "Ghana","Senegal",
                  "Iran", "South Korea", "Japan", "Saudi Arabia",
                  "Australia" )

WC_teams <- teams %>% 
  filter(team %in% Clasificados_en) %>% arrange(-elo)

for(i in (1:nrow(WC_teams))) {
  WC_teams$team[WC_teams$team==Clasificados_en[i]] <- Clasificados[i]
}

p_empate <- function(diferencia) {
  p <- .35 - .2/(max(WC_teams$elo)-min(WC_teams$elo))*diferencia
  p
} 

probs <- function(A,B) {
  pe <- p_empate(abs(A-B))
  probs <- c(elo.prob(A,B)*(1-pe),pe,elo.prob(B,A)*(1-pe))
  probs
}

probabilidades <- expand.grid(WC_teams$team,WC_teams$team)
probabilidades$Var1 <- as.character(probabilidades$Var1)
probabilidades$Var2 <- as.character(probabilidades$Var2)
probabilidades <- probabilidades[probabilidades$Var1<probabilidades$Var2,]
names(probabilidades) <- c("EquipoA","EquipoB")
probabilidades$G <- NA
probabilidades$E <- NA
probabilidades$P <- NA
probabilidades$Gf <- NA
probabilidades$Pf <- NA

for(i in (1:nrow(probabilidades))) {
  eloA <- WC_teams$elo[WC_teams$team==probabilidades$EquipoA[i]]
  eloB <- WC_teams$elo[WC_teams$team==probabilidades$EquipoB[i]]
  p <- probs(eloA,eloB)
  probabilidades$G[i] <- p[1]
  probabilidades$E[i] <- p[2]
  probabilidades$P[i] <- p[3]
  probabilidades$Gf[i] <- elo.prob(eloA,eloB)
  probabilidades$Pf[i] <- 1-elo.prob(eloA,eloB)
}

save(probabilidades,file="probabilidades.Rda")