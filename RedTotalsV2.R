# Goal totals V2
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#Create the home and away team matrix
mls_redtotalsv2 <- tapply(MLS$TR, MLS[c("HomeTeam", "AwayTeam")],mean)
arg_redtotalsv2 <- tapply(ARG$TR, ARG[c("HomeTeam", "AwayTeam")],mean)
# d2_redtotalsv2 <- tapply(D2$TR, D2[c("HomeTeam", "AwayTeam")],mean)
# e0_redtotalsv2 <- tapply(E0$TR, E0[c("HomeTeam", "AwayTeam")],mean)
# e1_redtotalsv2 <- tapply(E1$TR, E1[c("HomeTeam", "AwayTeam")],mean)
# e2_redtotalsv2 <- tapply(E2$TR, E2[c("HomeTeam", "AwayTeam")],mean)
# e3_redtotalsv2 <- tapply(E3$TR, E3[c("HomeTeam", "AwayTeam")],mean)
# ec_redtotalsv2 <- tapply(EC$TR, EC[c("HomeTeam", "AwayTeam")],mean)
# f1_redtotalsv2 <- tapply(F1$TR, F1[c("HomeTeam", "AwayTeam")],mean)
# f2_redtotalsv2 <- tapply(F2$TR, F2[c("HomeTeam", "AwayTeam")],mean)
# g1_redtotalsv2 <- tapply(G1$TR, G1[c("HomeTeam", "AwayTeam")],mean)
# i1_redtotalsv2 <- tapply(I1$TR, I1[c("HomeTeam", "AwayTeam")],mean)
# i2_redtotalsv2 <- tapply(I2$TR, I2[c("HomeTeam", "AwayTeam")],mean)
# n1_redtotalsv2 <- tapply(N1$TR, N1[c("HomeTeam", "AwayTeam")],mean)
# p1_redtotalsv2 <- tapply(P1$TR, P1[c("HomeTeam", "AwayTeam")],mean)
# sc0_redtotalsv2 <- tapply(SC0$TR, SC0[c("HomeTeam", "AwayTeam")],mean)
# sc1_redtotalsv2 <- tapply(SC1$TR, SC1[c("HomeTeam", "AwayTeam")],mean)
# sc2_redtotalsv2 <- tapply(SC2$TR, SC2[c("HomeTeam", "AwayTeam")],mean)
# sc3_redtotalsv2 <- tapply(SC3$TR, SC3[c("HomeTeam", "AwayTeam")],mean)
# sp1_redtotalsv2 <- tapply(SP1$TR, SP1[c("HomeTeam", "AwayTeam")],mean)
# sp2_redtotalsv2 <- tapply(SP2$TR, SP2[c("HomeTeam", "AwayTeam")],mean)
# t1_redtotalsv2 <- tapply(T1$TR, T1[c("HomeTeam", "AwayTeam")],mean)
#Create the row and column sums
#MLS
mls_hrtotals <- rowSums(mls_redtotalsv2, na.rm = T)
mls_artotals <- colSums(mls_redtotalsv2, na.rm = T)
#ARG
arg_hrtotals <- rowSums(arg_redtotalsv2, na.rm = T)
arg_artotals <- colSums(arg_redtotalsv2, na.rm = T)
#D2
# d2_hrtotals <- rowSums(d2_redtotalsv2, na.rm = T)
# d2_artotals <- colSums(d2_redtotalsv2, na.rm = T)
# #E0
# e0_hrtotals <- rowSums(e0_redtotalsv2, na.rm = T)
# e0_artotals <- colSums(e0_redtotalsv2, na.rm = T)
# #E1
# e1_hrtotals <- rowSums(e1_redtotalsv2, na.rm = T)
# e1_artotals <- colSums(e1_redtotalsv2, na.rm = T)
# #E2
# e2_hrtotals <- rowSums(e2_redtotalsv2, na.rm = T)
# e2_artotals <- colSums(e2_redtotalsv2, na.rm = T)
# #E3
# e3_hrtotals <- rowSums(e3_redtotalsv2, na.rm = T)
# e3_artotals <- colSums(e3_redtotalsv2, na.rm = T)
# #EC
# ec_hrtotals <- rowSums(ec_redtotalsv2, na.rm = T)
# ec_artotals <- colSums(ec_redtotalsv2, na.rm = T)
# #F1
# f1_hrtotals <- rowSums(f1_redtotalsv2, na.rm = T)
# f1_artotals <- colSums(f1_redtotalsv2, na.rm = T)
# #F2
# f2_hrtotals <- rowSums(f2_redtotalsv2, na.rm = T)
# f2_artotals <- colSums(f2_redtotalsv2, na.rm = T)
# #G1
# g1_hrtotals <- rowSums(g1_redtotalsv2, na.rm = T)
# g1_artotals <- colSums(g1_redtotalsv2, na.rm = T)
# #I1
# i1_hrtotals <- rowSums(i1_redtotalsv2, na.rm = T)
# i1_artotals <- colSums(i1_redtotalsv2, na.rm = T)
# #I2
# i2_hrtotals <- rowSums(i2_redtotalsv2, na.rm = T)
# i2_artotals <- colSums(i2_redtotalsv2, na.rm = T)
# #N1
# n1_hrtotals <- rowSums(n1_redtotalsv2, na.rm = T)
# n1_artotals <- colSums(n1_redtotalsv2, na.rm = T)
# #P1
# p1_hrtotals <- rowSums(p1_redtotalsv2, na.rm = T)
# p1_artotals <- colSums(p1_redtotalsv2, na.rm = T)
# #SC0
# sc0_hrtotals <- rowSums(sc0_redtotalsv2, na.rm = T)
# sc0_artotals <- colSums(sc0_redtotalsv2, na.rm = T)
# #SC1
# sc1_hrtotals <- rowSums(sc1_redtotalsv2, na.rm = T)
# sc1_artotals <- colSums(sc1_redtotalsv2, na.rm = T)
# #SC2
# sc2_hrtotals <- rowSums(sc2_redtotalsv2, na.rm = T)
# sc2_artotals <- colSums(sc2_redtotalsv2, na.rm = T)
# #SC3
# sc3_hrtotals <- rowSums(sc3_redtotalsv2, na.rm = T)
# sc3_artotals <- colSums(sc3_redtotalsv2, na.rm = T)
# #SP1
# sp1_hrtotals <- rowSums(sp1_redtotalsv2, na.rm = T)
# sp1_artotals <- colSums(sp1_redtotalsv2, na.rm = T)
# #SP2
# sp2_hrtotals <- rowSums(sp2_redtotalsv2, na.rm = T)
# sp2_artotals <- colSums(sp2_redtotalsv2, na.rm = T)
# #T1
# t1_hrtotals <- rowSums(t1_redtotalsv2, na.rm = T)
# t1_artotals <- colSums(t1_redtotalsv2, na.rm = T)

#Bind hrtotal and artotal
mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_hrtotals,mls_artotals)
arg_redtotalsv2 <- cbind(arg_redtotalsv2,arg_hrtotals,arg_artotals)
# d2_redtotalsv2 <- cbind(d2_redtotalsv2,d2_hrtotals,d2_artotals)
# e0_redtotalsv2 <- cbind(e0_redtotalsv2,e0_hrtotals,e0_artotals)
# e1_redtotalsv2 <- cbind(e1_redtotalsv2,e1_hrtotals,e1_artotals)
# e2_redtotalsv2 <- cbind(e2_redtotalsv2,e2_hrtotals,e2_artotals)
# e3_redtotalsv2 <- cbind(e3_redtotalsv2,e3_hrtotals,e3_artotals)
# ec_redtotalsv2 <- cbind(ec_redtotalsv2,ec_hrtotals,ec_artotals)
# f1_redtotalsv2 <- cbind(f1_redtotalsv2,f1_hrtotals,f1_artotals)
# f2_redtotalsv2 <- cbind(f2_redtotalsv2,f2_hrtotals,f2_artotals)
# g1_redtotalsv2 <- cbind(g1_redtotalsv2,g1_hrtotals,g1_artotals)
# i1_redtotalsv2 <- cbind(i1_redtotalsv2,i1_hrtotals,i1_artotals)
# i2_redtotalsv2 <- cbind(i2_redtotalsv2,i2_hrtotals,i2_artotals)
# n1_redtotalsv2 <- cbind(n1_redtotalsv2,n1_hrtotals,n1_artotals)
# p1_redtotalsv2 <- cbind(p1_redtotalsv2,p1_hrtotals,p1_artotals)
# sc0_redtotalsv2 <- cbind(sc0_redtotalsv2,sc0_hrtotals,sc0_artotals)
# sc1_redtotalsv2 <- cbind(sc1_redtotalsv2,sc1_hrtotals,sc1_artotals)
# sc2_redtotalsv2 <- cbind(sc2_redtotalsv2,sc2_hrtotals,sc2_artotals)
# sc3_redtotalsv2 <- cbind(sc3_redtotalsv2,sc3_hrtotals,sc3_artotals)
# sp1_redtotalsv2 <- cbind(sp1_redtotalsv2,sp1_hrtotals,sp1_artotals)
# sp2_redtotalsv2 <- cbind(sp2_redtotalsv2,sp2_hrtotals,sp2_artotals)
# t1_redtotalsv2 <- cbind(t1_redtotalsv2,t1_hrtotals,t1_artotals)

#add total reds
mls_totalreds <- mls_hrtotals + mls_artotals
arg_totalreds <- arg_hrtotals + arg_artotals
# d2_totalreds <- d2_hrtotals + d2_artotals
# e0_totalreds <- e0_hrtotals + e0_artotals
# e1_totalreds <- e1_hrtotals + e1_artotals
# e2_totalreds <- e2_hrtotals + e2_artotals
# e3_totalreds <- e3_hrtotals + e3_artotals
# ec_totalreds <- ec_hrtotals + ec_artotals
# f1_totalreds <- f1_hrtotals + f1_artotals
# f2_totalreds <- f2_hrtotals + f2_artotals
# g1_totalreds <- g1_hrtotals + g1_artotals
# i1_totalreds <- i1_hrtotals + i1_artotals
# i2_totalreds <- i2_hrtotals + i2_artotals
# n1_totalreds <- n1_hrtotals + n1_artotals
# p1_totalreds <- p1_hrtotals + p1_artotals
# sc0_totalreds <- sc0_hrtotals + sc0_artotals
# sc1_totalreds <- sc1_hrtotals + sc1_artotals
# sc2_totalreds <- sc2_hrtotals + sc2_artotals
# sc3_totalreds <- sc3_hrtotals + sc3_artotals
# sp1_totalreds <- sp1_hrtotals + sp1_artotals
# sp2_totalreds <- sp2_hrtotals + sp2_artotals
#t1_totalreds <- t1_hrtotals + t1_artotals
#bind total reds column
mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_totalreds)
arg_redtotalsv2 <- cbind(arg_redtotalsv2,arg_totalreds)
# d2_redtotalsv2 <- cbind(d2_redtotalsv2,d2_totalreds)
# e0_redtotalsv2 <- cbind(e0_redtotalsv2,e0_totalreds)
# e1_redtotalsv2 <- cbind(e1_redtotalsv2,e1_totalreds)
# e2_redtotalsv2 <- cbind(e2_redtotalsv2,e2_totalreds)
# e3_redtotalsv2 <- cbind(e3_redtotalsv2,e3_totalreds)
# ec_redtotalsv2 <- cbind(ec_redtotalsv2,ec_totalreds)
# f1_redtotalsv2 <- cbind(f1_redtotalsv2,f1_totalreds)
# f2_redtotalsv2 <- cbind(f2_redtotalsv2,f2_totalreds)
# g1_redtotalsv2 <- cbind(g1_redtotalsv2,g1_totalreds)
# i1_redtotalsv2 <- cbind(i1_redtotalsv2,i1_totalreds)
# i2_redtotalsv2 <- cbind(i2_redtotalsv2,i2_totalreds)
# n1_redtotalsv2 <- cbind(n1_redtotalsv2,n1_totalreds)
# p1_redtotalsv2 <- cbind(p1_redtotalsv2,p1_totalreds)
# sc0_redtotalsv2 <- cbind(sc0_redtotalsv2,sc0_totalreds)
# sc1_redtotalsv2 <- cbind(sc1_redtotalsv2,sc1_totalreds)
# sc2_redtotalsv2 <- cbind(sc2_redtotalsv2,sc2_totalreds)
# sc3_redtotalsv2 <- cbind(sc3_redtotalsv2,sc3_totalreds)
# sp1_redtotalsv2 <- cbind(sp1_redtotalsv2,sp1_totalreds)
# sp2_redtotalsv2 <- cbind(sp2_redtotalsv2,sp2_totalreds)
# t1_redtotalsv2 <- cbind(t1_redtotalsv2,t1_totalreds)
#Get teams in each division as a vector
mls_teams <- sort(unique(MLS$HomeTeam))
arg_teams <- sort(unique(ARG$HomeTeam))
# d2_teams <- sort(unique(D2$HomeTeam))
# e0_teams <- sort(unique(E0$HomeTeam))
# e1_teams <- sort(unique(E1$HomeTeam))
# e2_teams <- sort(unique(E2$HomeTeam))
# e3_teams <- sort(unique(E3$HomeTeam))
# ec_teams <- sort(unique(EC$HomeTeam))
# f1_teams <- sort(unique(F1$HomeTeam))
# f2_teams <- sort(unique(F2$HomeTeam))
# g1_teams <- sort(unique(G1$HomeTeam))
# i1_teams <- sort(unique(I1$HomeTeam))
# i2_teams <- sort(unique(I2$HomeTeam))
# n1_teams <- sort(unique(N1$HomeTeam))
# p1_teams <- sort(unique(P1$HomeTeam))
# sc0_teams <- sort(unique(SC0$HomeTeam))
# sc1_teams <- sort(unique(SC1$HomeTeam))
# sc2_teams <- sort(unique(SC2$HomeTeam))
# sc3_teams <- sort(unique(SC3$HomeTeam))
# sp1_teams <- sort(unique(SP1$HomeTeam))
# sp2_teams <- sort(unique(SP2$HomeTeam))
# t1_teams <- sort(unique(T1$HomeTeam))
#initialize home and away game vectors
#MLS
mls_home_games <- c()
mls_away_games <-c()
#ARG
arg_home_games <- c()
arg_away_games <-c()
#D2
# d2_home_games <- c()
# d2_away_games <-c()
# #E0
# e0_home_games <- c()
# e0_away_games <-c()
# #E1
# e1_home_games <- c()
# e1_away_games <-c()
# #E2
# e2_home_games <- c()
# e2_away_games <-c()
# #E3
# e3_home_games <- c()
# e3_away_games <-c()
# #EC
# ec_home_games <- c()
# ec_away_games <-c()
# #F1
# f1_home_games <- c()
# f1_away_games <-c()
# #F2
# f2_home_games <- c()
# f2_away_games <-c()
# #G1
# g1_home_games <- c()
# g1_away_games <-c()
# #I1
# i1_home_games <- c()
# i1_away_games <-c()
# #I2
# i2_home_games <- c()
# i2_away_games <-c()
# #N1
# n1_home_games <- c()
# n1_away_games <-c()
# #P1
# p1_home_games <- c()
# p1_away_games <-c()
# #SC0
# sc0_home_games <- c()
# sc0_away_games <-c()
# #SC1
# sc1_home_games <- c()
# sc1_away_games <-c()
# #SC2
# sc2_home_games <- c()
# sc2_away_games <-c()
# #SC3
# sc3_home_games <- c()
# sc3_away_games <-c()
# #SP1
# sp1_home_games <- c()
# sp1_away_games <-c()
# #SP2
# sp2_home_games <- c()
# sp2_away_games <-c()
# #T1
# t1_home_games <- c()
# t1_away_games <-c()
#Get number of home and away games played
#MLS
for (i_mls in 1:length(mls_teams))
{

  mls_home_games[i_mls] <- nrow(MLS[MLS$HomeTeam == mls_teams[i_mls],])
  mls_away_games[i_mls]  <- nrow(MLS[MLS$AwayTeam == mls_teams[i_mls],])

}
#ARG
for (i_arg in 1:length(arg_teams))
{

  arg_home_games[i_arg] <- nrow(ARG[ARG$HomeTeam == arg_teams[i_arg],])
  arg_away_games[i_arg]  <- nrow(ARG[ARG$AwayTeam == arg_teams[i_arg],])

}
# #D2
# for (i_d2 in 1:length(d2_teams))
# {
#
#   d2_home_games[i_d2] <- nrow(D2[D2$HomeTeam == d2_teams[i_d2],])
#   d2_away_games[i_d2]  <- nrow(D2[D2$AwayTeam == d2_teams[i_d2],])
#
# }
# #E0
# for (i_e0 in 1:length(e0_teams))
# {
#
#   e0_home_games[i_e0] <- nrow(E0[E0$HomeTeam == e0_teams[i_e0],])
#   e0_away_games[i_e0]  <- nrow(E0[E0$AwayTeam == e0_teams[i_e0],])
# }
# #E1
# for (i_e1 in 1:length(e1_teams))
# {
#
#   e1_home_games[i_e1] <- nrow(E1[E1$HomeTeam == e1_teams[i_e1],])
#   e1_away_games[i_e1]  <- nrow(E1[E1$AwayTeam == e1_teams[i_e1],])
#
# }
# #E2
# for (i_e2 in 1:length(e2_teams))
# {
#
#   e2_home_games[i_e2] <- nrow(E2[E2$HomeTeam == e2_teams[i_e2],])
#   e2_away_games[i_e2]  <- nrow(E2[E2$AwayTeam == e2_teams[i_e2],])
#
# }
# #E3
# for (i_e3 in 1:length(e3_teams))
# {
#
#   e3_home_games[i_e3] <- nrow(E3[E3$HomeTeam == e3_teams[i_e3],])
#   e3_away_games[i_e3]  <- nrow(E3[E3$AwayTeam == e3_teams[i_e3],])
#
# }
# #EC
# for (i_ec in 1:length(ec_teams))
# {
#
#   ec_home_games[i_ec] <- nrow(EC[EC$HomeTeam == ec_teams[i_ec],])
#   ec_away_games[i_ec]  <- nrow(EC[EC$AwayTeam == ec_teams[i_ec],])
#
# }
# #F1
# for (i_f1 in 1:length(f1_teams))
# {
#
#   f1_home_games[i_f1] <- nrow(F1[F1$HomeTeam == f1_teams[i_f1],])
#   f1_away_games[i_f1]  <- nrow(F1[F1$AwayTeam == f1_teams[i_f1],])
#
# }
# #F2
# for (i_f2 in 1:length(f2_teams))
# {
#
#   f2_home_games[i_f2] <- nrow(F2[F2$HomeTeam == f2_teams[i_f2],])
#   f2_away_games[i_f2]  <- nrow(F2[F2$AwayTeam == f2_teams[i_f2],])
#
# }
# #G1
# for (i_g1 in 1:length(g1_teams))
# {
#
#   g1_home_games[i_g1] <- nrow(G1[G1$HomeTeam == g1_teams[i_g1],])
#   g1_away_games[i_g1]  <- nrow(G1[G1$AwayTeam == g1_teams[i_g1],])
#
# }
# #I1
# for (i_i1 in 1:length(i1_teams))
# {
#
#   i1_home_games[i_i1] <- nrow(I1[I1$HomeTeam == i1_teams[i_i1],])
#   i1_away_games[i_i1]  <- nrow(I1[I1$AwayTeam == i1_teams[i_i1],])
#
# }
# #I2
# for (i_i2 in 1:length(i2_teams))
# {
#
#   i2_home_games[i_i2] <- nrow(I2[I2$HomeTeam == i2_teams[i_i2],])
#   i2_away_games[i_i2]  <- nrow(I2[I2$AwayTeam == i2_teams[i_i2],])
#
# }
# #N1
# for (i_n1 in 1:length(n1_teams))
# {
#
#   n1_home_games[i_n1] <- nrow(N1[N1$HomeTeam == n1_teams[i_n1],])
#   n1_away_games[i_n1]  <- nrow(N1[N1$AwayTeam == n1_teams[i_n1],])
#
# }
# #P1
# for (i_p1 in 1:length(p1_teams))
# {
#
#   p1_home_games[i_p1] <- nrow(P1[P1$HomeTeam == p1_teams[i_p1],])
#   p1_away_games[i_p1]  <- nrow(P1[P1$AwayTeam == p1_teams[i_p1],])
#
# }
# #SC0
# for (i_sc0 in 1:length(sc0_teams))
# {
#
#   sc0_home_games[i_sc0] <- nrow(SC0[SC0$HomeTeam == sc0_teams[i_sc0],])
#   sc0_away_games[i_sc0]  <- nrow(SC0[SC0$AwayTeam == sc0_teams[i_sc0],])
#
# }
# #SC1
# for (i_sc1 in 1:length(sc1_teams))
# {
#
#   sc1_home_games[i_sc1] <- nrow(SC1[SC1$HomeTeam == sc1_teams[i_sc1],])
#   sc1_away_games[i_sc1]  <- nrow(SC1[SC1$AwayTeam == sc1_teams[i_sc1],])
#
# }
# #SC2
# for (i_sc2 in 1:length(sc2_teams))
# {
#
#   sc2_home_games[i_sc2] <- nrow(SC2[SC2$HomeTeam == sc2_teams[i_sc2],])
#   sc2_away_games[i_sc2]  <- nrow(SC2[SC2$AwayTeam == sc2_teams[i_sc2],])
#
# }
# #SC3
# for (i_sc3 in 1:length(sc3_teams))
# {
#
#   sc3_home_games[i_sc3] <- nrow(SC3[SC3$HomeTeam == sc3_teams[i_sc3],])
#   sc3_away_games[i_sc3]  <- nrow(SC3[SC3$AwayTeam == sc3_teams[i_sc3],])
#
# }
# #SP1
# for (i_sp1 in 1:length(sp1_teams))
# {
#
#   sp1_home_games[i_sp1] <- nrow(SP1[SP1$HomeTeam == sp1_teams[i_sp1],])
#   sp1_away_games[i_sp1]  <- nrow(SP1[SP1$AwayTeam == sp1_teams[i_sp1],])
#
# }
# #SP2
# for (i_sp2 in 1:length(sp2_teams))
# {
#
#   sp2_home_games[i_sp2] <- nrow(SP2[SP2$HomeTeam == sp2_teams[i_sp2],])
#   sp2_away_games[i_sp2]  <- nrow(SP2[SP2$AwayTeam == sp2_teams[i_sp2],])
#
# }
# #T1
# for (i_t1 in 1:length(t1_teams))
# {
#
#   t1_home_games[i_t1] <- nrow(T1[T1$HomeTeam == t1_teams[i_t1],])
#   t1_away_games[i_t1]  <- nrow(T1[T1$AwayTeam == t1_teams[i_t1],])
#
# }
#Add home games and away games to get total games played
mls_games_played <- mls_home_games + mls_away_games
arg_games_played <- arg_home_games + arg_away_games
# d2_games_played <- d2_home_games + d2_away_games
# e0_games_played <- e0_home_games + e0_away_games
# e1_games_played <- e1_home_games + e1_away_games
# e2_games_played <- e2_home_games + e2_away_games
# e3_games_played <- e3_home_games + e3_away_games
# ec_games_played <- ec_home_games + ec_away_games
# f1_games_played <- f1_home_games + f1_away_games
# f2_games_played <- f2_home_games + f2_away_games
# g1_games_played <- g1_home_games + g1_away_games
# i1_games_played <- i1_home_games + i1_away_games
# i2_games_played <- i2_home_games + i2_away_games
# n1_games_played <- n1_home_games + n1_away_games
# p1_games_played <- p1_home_games + p1_away_games
# sc0_games_played <- sc0_home_games + sc0_away_games
# sc1_games_played <- sc1_home_games + sc1_away_games
# sc2_games_played <- sc2_home_games + sc2_away_games
# sc3_games_played <- sc3_home_games + sc3_away_games
# sp1_games_played <- sp1_home_games + sp1_away_games
# sp2_games_played <- sp2_home_games + sp2_away_games
# t1_games_played <- t1_home_games + t1_away_games
#Bind total games played

mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_games_played)
arg_redtotalsv2 <- cbind(arg_redtotalsv2,arg_games_played)
# d2_redtotalsv2 <- cbind(d2_redtotalsv2,d2_games_played)
# e0_redtotalsv2 <- cbind(e0_redtotalsv2,e0_games_played)
# e1_redtotalsv2 <- cbind(e1_redtotalsv2,e1_games_played)
# e2_redtotalsv2 <- cbind(e2_redtotalsv2,e2_games_played)
# e3_redtotalsv2 <- cbind(e3_redtotalsv2,e3_games_played)
# ec_redtotalsv2 <- cbind(ec_redtotalsv2,ec_games_played)
# f1_redtotalsv2 <- cbind(f1_redtotalsv2,f1_games_played)
# f2_redtotalsv2 <- cbind(f2_redtotalsv2,f2_games_played)
# g1_redtotalsv2 <- cbind(g1_redtotalsv2,g1_games_played)
# i1_redtotalsv2 <- cbind(i1_redtotalsv2,i1_games_played)
# i2_redtotalsv2 <- cbind(i2_redtotalsv2,i2_games_played)
# n1_redtotalsv2 <- cbind(n1_redtotalsv2,n1_games_played)
# p1_redtotalsv2 <- cbind(p1_redtotalsv2,p1_games_played)
# sc0_redtotalsv2 <- cbind(sc0_redtotalsv2,sc0_games_played)
# sc1_redtotalsv2 <- cbind(sc1_redtotalsv2,sc1_games_played)
# sc2_redtotalsv2 <- cbind(sc2_redtotalsv2,sc2_games_played)
# sc3_redtotalsv2 <- cbind(sc3_redtotalsv2,sc3_games_played)
# sp1_redtotalsv2 <- cbind(sp1_redtotalsv2,sp1_games_played)
# sp2_redtotalsv2 <- cbind(sp2_redtotalsv2,sp2_games_played)
# t1_redtotalsv2 <- cbind(t1_redtotalsv2,t1_games_played)
#Calculate averaye total reds
mls_avg_totalreds <- round((mls_totalreds/ mls_games_played), digits = 4)
arg_avg_totalreds <- round((arg_totalreds/ arg_games_played), digits = 4)
# d2_avg_totalreds <- round((d2_totalreds/ d2_games_played), digits = 4)
# e0_avg_totalreds <- round((e0_totalreds/ e0_games_played), digits = 4)
# e1_avg_totalreds <- round((e1_totalreds/ e1_games_played), digits = 4)
# e2_avg_totalreds <- round((e2_totalreds/ e2_games_played), digits = 4)
# e3_avg_totalreds <- round((e3_totalreds/ e3_games_played), digits = 4)
# ec_avg_totalreds <- round((ec_totalreds/ ec_games_played), digits = 4)
# f1_avg_totalreds <- round((f1_totalreds/ f1_games_played), digits = 4)
# f2_avg_totalreds <- round((f2_totalreds/ f2_games_played), digits = 4)
# g1_avg_totalreds <- round((g1_totalreds/ g1_games_played), digits = 4)
# i1_avg_totalreds <- round((i1_totalreds/ i1_games_played), digits = 4)
# i2_avg_totalreds <- round((i2_totalreds/ i2_games_played), digits = 4)
# n1_avg_totalreds <- round((n1_totalreds/ n1_games_played), digits = 4)
# p1_avg_totalreds <- round((p1_totalreds/ p1_games_played), digits = 4)
# sc0_avg_totalreds <- round((sc0_totalreds/ sc0_games_played), digits = 4)
# sc1_avg_totalreds <- round((sc1_totalreds/ sc1_games_played), digits = 4)
# sc2_avg_totalreds <- round((sc2_totalreds/ sc2_games_played), digits = 4)
# sc3_avg_totalreds <- round((sc3_totalreds/ sc3_games_played), digits = 4)
# sp1_avg_totalreds <- round((sp1_totalreds/ sp1_games_played), digits = 4)
# sp2_avg_totalreds <- round((sp2_totalreds/ sp2_games_played), digits = 4)
# t1_avg_totalreds <- round((t1_totalreds/ t1_games_played), digits = 4)
#Remove NA values
mls_redtotalsv2[is.na(mls_redtotalsv2)] <- ""
arg_redtotalsv2[is.na(arg_redtotalsv2)] <- ""
# d2_redtotalsv2[is.na(d2_redtotalsv2)] <- ""
# e0_redtotalsv2[is.na(e0_redtotalsv2)] <- ""
# e1_redtotalsv2[is.na(e1_redtotalsv2)] <- ""
# e2_redtotalsv2[is.na(e2_redtotalsv2)] <- ""
# e3_redtotalsv2[is.na(e3_redtotalsv2)] <- ""
# ec_redtotalsv2[is.na(ec_redtotalsv2)] <- ""
# f1_redtotalsv2[is.na(f1_redtotalsv2)] <- ""
# f2_redtotalsv2[is.na(f2_redtotalsv2)] <- ""
# g1_redtotalsv2[is.na(g1_redtotalsv2)] <- ""
# i1_redtotalsv2[is.na(i1_redtotalsv2)] <- ""
# i2_redtotalsv2[is.na(i2_redtotalsv2)] <- ""
# n1_redtotalsv2[is.na(n1_redtotalsv2)] <- ""
# p1_redtotalsv2[is.na(p1_redtotalsv2)] <- ""
# sc0_redtotalsv2[is.na(sc0_redtotalsv2)] <- ""
# sc1_redtotalsv2[is.na(sc1_redtotalsv2)] <- ""
# sc2_redtotalsv2[is.na(sc2_redtotalsv2)] <- ""
# sc3_redtotalsv2[is.na(sc3_redtotalsv2)] <- ""
# sp1_redtotalsv2[is.na(sp1_redtotalsv2)] <- ""
# sp2_redtotalsv2[is.na(sp2_redtotalsv2)] <- ""
# t1_redtotalsv2[is.na(t1_redtotalsv2)] <- ""
#Bind averaye total reds

mls_redtotalsv2 <- cbind(mls_redtotalsv2,mls_avg_totalreds)
arg_redtotalsv2 <- cbind(arg_redtotalsv2,arg_avg_totalreds)
# d2_redtotalsv2 <- cbind(d2_redtotalsv2,d2_avg_totalreds)
# e0_redtotalsv2 <- cbind(e0_redtotalsv2,e0_avg_totalreds)
# e1_redtotalsv2 <- cbind(e1_redtotalsv2,e1_avg_totalreds)
# e2_redtotalsv2 <- cbind(e2_redtotalsv2,e2_avg_totalreds)
# e3_redtotalsv2 <- cbind(e3_redtotalsv2,e3_avg_totalreds)
# ec_redtotalsv2 <- cbind(ec_redtotalsv2,ec_avg_totalreds)
# f1_redtotalsv2 <- cbind(f1_redtotalsv2,f1_avg_totalreds)
# f2_redtotalsv2 <- cbind(f2_redtotalsv2,f2_avg_totalreds)
# g1_redtotalsv2 <- cbind(g1_redtotalsv2,g1_avg_totalreds)
# i1_redtotalsv2 <- cbind(i1_redtotalsv2,i1_avg_totalreds)
# i2_redtotalsv2 <- cbind(i2_redtotalsv2,i2_avg_totalreds)
# n1_redtotalsv2 <- cbind(n1_redtotalsv2,n1_avg_totalreds)
# p1_redtotalsv2 <- cbind(p1_redtotalsv2,p1_avg_totalreds)
# sc0_redtotalsv2 <- cbind(sc0_redtotalsv2,sc0_avg_totalreds)
# sc1_redtotalsv2 <- cbind(sc1_redtotalsv2,sc1_avg_totalreds)
# sc2_redtotalsv2 <- cbind(sc2_redtotalsv2,sc2_avg_totalreds)
# sc3_redtotalsv2 <- cbind(sc3_redtotalsv2,sc3_avg_totalreds)
# sp1_redtotalsv2 <- cbind(sp1_redtotalsv2,sp1_avg_totalreds)
# sp2_redtotalsv2 <- cbind(sp2_redtotalsv2,sp2_avg_totalreds)
# t1_redtotalsv2 <- cbind(t1_redtotalsv2,t1_avg_totalreds)

# #Write out to excel files
# write.xlsx(mls_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "MLS")
# write.xlsx(arg_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_redtotalsv2,'RedTotalsV2.xlsx',sheetName = "T1", append = TRUE)



