# Goal totals V2
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#Create the home and away team matrix
mls_yellowtotalsv2 <- tapply(MLS$TY, MLS[c("HomeTeam", "AwayTeam")],mean)
arg_yellowtotalsv2 <- tapply(ARG$TY, ARG[c("HomeTeam", "AwayTeam")],mean)
# d2_yellowtotalsv2 <- tapply(D2$TY, D2[c("HomeTeam", "AwayTeam")],mean)
# e0_yellowtotalsv2 <- tapply(E0$TY, E0[c("HomeTeam", "AwayTeam")],mean)
# e1_yellowtotalsv2 <- tapply(E1$TY, E1[c("HomeTeam", "AwayTeam")],mean)
# e2_yellowtotalsv2 <- tapply(E2$TY, E2[c("HomeTeam", "AwayTeam")],mean)
# e3_yellowtotalsv2 <- tapply(E3$TY, E3[c("HomeTeam", "AwayTeam")],mean)
# ec_yellowtotalsv2 <- tapply(EC$TY, EC[c("HomeTeam", "AwayTeam")],mean)
# f1_yellowtotalsv2 <- tapply(F1$TY, F1[c("HomeTeam", "AwayTeam")],mean)
# f2_yellowtotalsv2 <- tapply(F2$TY, F2[c("HomeTeam", "AwayTeam")],mean)
# g1_yellowtotalsv2 <- tapply(G1$TY, G1[c("HomeTeam", "AwayTeam")],mean)
# i1_yellowtotalsv2 <- tapply(I1$TY, I1[c("HomeTeam", "AwayTeam")],mean)
# i2_yellowtotalsv2 <- tapply(I2$TY, I2[c("HomeTeam", "AwayTeam")],mean)
# n1_yellowtotalsv2 <- tapply(N1$TY, N1[c("HomeTeam", "AwayTeam")],mean)
# p1_yellowtotalsv2 <- tapply(P1$TY, P1[c("HomeTeam", "AwayTeam")],mean)
# sc0_yellowtotalsv2 <- tapply(SC0$TY, SC0[c("HomeTeam", "AwayTeam")],mean)
# sc1_yellowtotalsv2 <- tapply(SC1$TY, SC1[c("HomeTeam", "AwayTeam")],mean)
# sc2_yellowtotalsv2 <- tapply(SC2$TY, SC2[c("HomeTeam", "AwayTeam")],mean)
# sc3_yellowtotalsv2 <- tapply(SC3$TY, SC3[c("HomeTeam", "AwayTeam")],mean)
# sp1_yellowtotalsv2 <- tapply(SP1$TY, SP1[c("HomeTeam", "AwayTeam")],mean)
# sp2_yellowtotalsv2 <- tapply(SP2$TY, SP2[c("HomeTeam", "AwayTeam")],mean)
# t1_yellowtotalsv2 <- tapply(T1$TY, T1[c("HomeTeam", "AwayTeam")],mean)
#Create the row and column sums
#MLS
mls_hytotals <- rowSums(mls_yellowtotalsv2, na.rm = T)
mls_aytotals <- colSums(mls_yellowtotalsv2, na.rm = T)
#ARG
arg_hytotals <- rowSums(arg_yellowtotalsv2, na.rm = T)
arg_aytotals <- colSums(arg_yellowtotalsv2, na.rm = T)
#D2
# d2_hytotals <- rowSums(d2_yellowtotalsv2, na.rm = T)
# d2_aytotals <- colSums(d2_yellowtotalsv2, na.rm = T)
# #E0
# e0_hytotals <- rowSums(e0_yellowtotalsv2, na.rm = T)
# e0_aytotals <- colSums(e0_yellowtotalsv2, na.rm = T)
# #E1
# e1_hytotals <- rowSums(e1_yellowtotalsv2, na.rm = T)
# e1_aytotals <- colSums(e1_yellowtotalsv2, na.rm = T)
# #E2
# e2_hytotals <- rowSums(e2_yellowtotalsv2, na.rm = T)
# e2_aytotals <- colSums(e2_yellowtotalsv2, na.rm = T)
# #E3
# e3_hytotals <- rowSums(e3_yellowtotalsv2, na.rm = T)
# e3_aytotals <- colSums(e3_yellowtotalsv2, na.rm = T)
# #EC
# ec_hytotals <- rowSums(ec_yellowtotalsv2, na.rm = T)
# ec_aytotals <- colSums(ec_yellowtotalsv2, na.rm = T)
# #F1
# f1_hytotals <- rowSums(f1_yellowtotalsv2, na.rm = T)
# f1_aytotals <- colSums(f1_yellowtotalsv2, na.rm = T)
# #F2
# f2_hytotals <- rowSums(f2_yellowtotalsv2, na.rm = T)
# f2_aytotals <- colSums(f2_yellowtotalsv2, na.rm = T)
# #G1
# g1_hytotals <- rowSums(g1_yellowtotalsv2, na.rm = T)
# g1_aytotals <- colSums(g1_yellowtotalsv2, na.rm = T)
# #I1
# i1_hytotals <- rowSums(i1_yellowtotalsv2, na.rm = T)
# i1_aytotals <- colSums(i1_yellowtotalsv2, na.rm = T)
# #I2
# i2_hytotals <- rowSums(i2_yellowtotalsv2, na.rm = T)
# i2_aytotals <- colSums(i2_yellowtotalsv2, na.rm = T)
# #N1
# n1_hytotals <- rowSums(n1_yellowtotalsv2, na.rm = T)
# n1_aytotals <- colSums(n1_yellowtotalsv2, na.rm = T)
# #P1
# p1_hytotals <- rowSums(p1_yellowtotalsv2, na.rm = T)
# p1_aytotals <- colSums(p1_yellowtotalsv2, na.rm = T)
# #SC0
# sc0_hytotals <- rowSums(sc0_yellowtotalsv2, na.rm = T)
# sc0_aytotals <- colSums(sc0_yellowtotalsv2, na.rm = T)
# #SC1
# sc1_hytotals <- rowSums(sc1_yellowtotalsv2, na.rm = T)
# sc1_aytotals <- colSums(sc1_yellowtotalsv2, na.rm = T)
# #SC2
# sc2_hytotals <- rowSums(sc2_yellowtotalsv2, na.rm = T)
# sc2_aytotals <- colSums(sc2_yellowtotalsv2, na.rm = T)
# #SC3
# sc3_hytotals <- rowSums(sc3_yellowtotalsv2, na.rm = T)
# sc3_aytotals <- colSums(sc3_yellowtotalsv2, na.rm = T)
# #SP1
# sp1_hytotals <- rowSums(sp1_yellowtotalsv2, na.rm = T)
# sp1_aytotals <- colSums(sp1_yellowtotalsv2, na.rm = T)
# #SP2
# sp2_hytotals <- rowSums(sp2_yellowtotalsv2, na.rm = T)
# sp2_aytotals <- colSums(sp2_yellowtotalsv2, na.rm = T)
# #T1
# t1_hytotals <- rowSums(t1_yellowtotalsv2, na.rm = T)
# t1_aytotals <- colSums(t1_yellowtotalsv2, na.rm = T)

#Bind hytotal and aytotal
mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_hytotals,mls_aytotals)
arg_yellowtotalsv2 <- cbind(arg_yellowtotalsv2,arg_hytotals,arg_aytotals)
# d2_yellowtotalsv2 <- cbind(d2_yellowtotalsv2,d2_hytotals,d2_aytotals)
# e0_yellowtotalsv2 <- cbind(e0_yellowtotalsv2,e0_hytotals,e0_aytotals)
# e1_yellowtotalsv2 <- cbind(e1_yellowtotalsv2,e1_hytotals,e1_aytotals)
# e2_yellowtotalsv2 <- cbind(e2_yellowtotalsv2,e2_hytotals,e2_aytotals)
# e3_yellowtotalsv2 <- cbind(e3_yellowtotalsv2,e3_hytotals,e3_aytotals)
# ec_yellowtotalsv2 <- cbind(ec_yellowtotalsv2,ec_hytotals,ec_aytotals)
# f1_yellowtotalsv2 <- cbind(f1_yellowtotalsv2,f1_hytotals,f1_aytotals)
# f2_yellowtotalsv2 <- cbind(f2_yellowtotalsv2,f2_hytotals,f2_aytotals)
# g1_yellowtotalsv2 <- cbind(g1_yellowtotalsv2,g1_hytotals,g1_aytotals)
# i1_yellowtotalsv2 <- cbind(i1_yellowtotalsv2,i1_hytotals,i1_aytotals)
# i2_yellowtotalsv2 <- cbind(i2_yellowtotalsv2,i2_hytotals,i2_aytotals)
# n1_yellowtotalsv2 <- cbind(n1_yellowtotalsv2,n1_hytotals,n1_aytotals)
# p1_yellowtotalsv2 <- cbind(p1_yellowtotalsv2,p1_hytotals,p1_aytotals)
# sc0_yellowtotalsv2 <- cbind(sc0_yellowtotalsv2,sc0_hytotals,sc0_aytotals)
# sc1_yellowtotalsv2 <- cbind(sc1_yellowtotalsv2,sc1_hytotals,sc1_aytotals)
# sc2_yellowtotalsv2 <- cbind(sc2_yellowtotalsv2,sc2_hytotals,sc2_aytotals)
# sc3_yellowtotalsv2 <- cbind(sc3_yellowtotalsv2,sc3_hytotals,sc3_aytotals)
# sp1_yellowtotalsv2 <- cbind(sp1_yellowtotalsv2,sp1_hytotals,sp1_aytotals)
# sp2_yellowtotalsv2 <- cbind(sp2_yellowtotalsv2,sp2_hytotals,sp2_aytotals)
# t1_yellowtotalsv2 <- cbind(t1_yellowtotalsv2,t1_hytotals,t1_aytotals)

#add total yellows
mls_totalyellows <- mls_hytotals + mls_aytotals
arg_totalyellows <- arg_hytotals + arg_aytotals
# d2_totalyellows <- d2_hytotals + d2_aytotals
# e0_totalyellows <- e0_hytotals + e0_aytotals
# e1_totalyellows <- e1_hytotals + e1_aytotals
# e2_totalyellows <- e2_hytotals + e2_aytotals
# e3_totalyellows <- e3_hytotals + e3_aytotals
# ec_totalyellows <- ec_hytotals + ec_aytotals
# f1_totalyellows <- f1_hytotals + f1_aytotals
# f2_totalyellows <- f2_hytotals + f2_aytotals
# g1_totalyellows <- g1_hytotals + g1_aytotals
# i1_totalyellows <- i1_hytotals + i1_aytotals
# i2_totalyellows <- i2_hytotals + i2_aytotals
# n1_totalyellows <- n1_hytotals + n1_aytotals
# p1_totalyellows <- p1_hytotals + p1_aytotals
# sc0_totalyellows <- sc0_hytotals + sc0_aytotals
# sc1_totalyellows <- sc1_hytotals + sc1_aytotals
# sc2_totalyellows <- sc2_hytotals + sc2_aytotals
# sc3_totalyellows <- sc3_hytotals + sc3_aytotals
# sp1_totalyellows <- sp1_hytotals + sp1_aytotals
# sp2_totalyellows <- sp2_hytotals + sp2_aytotals
# t1_totalyellows <- t1_hytotals + t1_aytotals
#bind total yellows column
mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_totalyellows)
arg_yellowtotalsv2 <- cbind(arg_yellowtotalsv2,arg_totalyellows)
# d2_yellowtotalsv2 <- cbind(d2_yellowtotalsv2,d2_totalyellows)
# e0_yellowtotalsv2 <- cbind(e0_yellowtotalsv2,e0_totalyellows)
# e1_yellowtotalsv2 <- cbind(e1_yellowtotalsv2,e1_totalyellows)
# e2_yellowtotalsv2 <- cbind(e2_yellowtotalsv2,e2_totalyellows)
# e3_yellowtotalsv2 <- cbind(e3_yellowtotalsv2,e3_totalyellows)
# ec_yellowtotalsv2 <- cbind(ec_yellowtotalsv2,ec_totalyellows)
# f1_yellowtotalsv2 <- cbind(f1_yellowtotalsv2,f1_totalyellows)
# f2_yellowtotalsv2 <- cbind(f2_yellowtotalsv2,f2_totalyellows)
# g1_yellowtotalsv2 <- cbind(g1_yellowtotalsv2,g1_totalyellows)
# i1_yellowtotalsv2 <- cbind(i1_yellowtotalsv2,i1_totalyellows)
# i2_yellowtotalsv2 <- cbind(i2_yellowtotalsv2,i2_totalyellows)
# n1_yellowtotalsv2 <- cbind(n1_yellowtotalsv2,n1_totalyellows)
# p1_yellowtotalsv2 <- cbind(p1_yellowtotalsv2,p1_totalyellows)
# sc0_yellowtotalsv2 <- cbind(sc0_yellowtotalsv2,sc0_totalyellows)
# sc1_yellowtotalsv2 <- cbind(sc1_yellowtotalsv2,sc1_totalyellows)
# sc2_yellowtotalsv2 <- cbind(sc2_yellowtotalsv2,sc2_totalyellows)
# sc3_yellowtotalsv2 <- cbind(sc3_yellowtotalsv2,sc3_totalyellows)
# sp1_yellowtotalsv2 <- cbind(sp1_yellowtotalsv2,sp1_totalyellows)
# sp2_yellowtotalsv2 <- cbind(sp2_yellowtotalsv2,sp2_totalyellows)
# t1_yellowtotalsv2 <- cbind(t1_yellowtotalsv2,t1_totalyellows)
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

mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_games_played)
arg_yellowtotalsv2 <- cbind(arg_yellowtotalsv2,arg_games_played)
# d2_yellowtotalsv2 <- cbind(d2_yellowtotalsv2,d2_games_played)
# e0_yellowtotalsv2 <- cbind(e0_yellowtotalsv2,e0_games_played)
# e1_yellowtotalsv2 <- cbind(e1_yellowtotalsv2,e1_games_played)
# e2_yellowtotalsv2 <- cbind(e2_yellowtotalsv2,e2_games_played)
# e3_yellowtotalsv2 <- cbind(e3_yellowtotalsv2,e3_games_played)
# ec_yellowtotalsv2 <- cbind(ec_yellowtotalsv2,ec_games_played)
# f1_yellowtotalsv2 <- cbind(f1_yellowtotalsv2,f1_games_played)
# f2_yellowtotalsv2 <- cbind(f2_yellowtotalsv2,f2_games_played)
# g1_yellowtotalsv2 <- cbind(g1_yellowtotalsv2,g1_games_played)
# i1_yellowtotalsv2 <- cbind(i1_yellowtotalsv2,i1_games_played)
# i2_yellowtotalsv2 <- cbind(i2_yellowtotalsv2,i2_games_played)
# n1_yellowtotalsv2 <- cbind(n1_yellowtotalsv2,n1_games_played)
# p1_yellowtotalsv2 <- cbind(p1_yellowtotalsv2,p1_games_played)
# sc0_yellowtotalsv2 <- cbind(sc0_yellowtotalsv2,sc0_games_played)
# sc1_yellowtotalsv2 <- cbind(sc1_yellowtotalsv2,sc1_games_played)
# sc2_yellowtotalsv2 <- cbind(sc2_yellowtotalsv2,sc2_games_played)
# sc3_yellowtotalsv2 <- cbind(sc3_yellowtotalsv2,sc3_games_played)
# sp1_yellowtotalsv2 <- cbind(sp1_yellowtotalsv2,sp1_games_played)
# sp2_yellowtotalsv2 <- cbind(sp2_yellowtotalsv2,sp2_games_played)
# t1_yellowtotalsv2 <- cbind(t1_yellowtotalsv2,t1_games_played)
#Calculate averaye total yellows
mls_avg_totalyellows <- round((mls_totalyellows/ mls_games_played), digits = 4)
arg_avg_totalyellows <- round((arg_totalyellows/ arg_games_played), digits = 4)
# d2_avg_totalyellows <- round((d2_totalyellows/ d2_games_played), digits = 4)
# e0_avg_totalyellows <- round((e0_totalyellows/ e0_games_played), digits = 4)
# e1_avg_totalyellows <- round((e1_totalyellows/ e1_games_played), digits = 4)
# e2_avg_totalyellows <- round((e2_totalyellows/ e2_games_played), digits = 4)
# e3_avg_totalyellows <- round((e3_totalyellows/ e3_games_played), digits = 4)
# ec_avg_totalyellows <- round((ec_totalyellows/ ec_games_played), digits = 4)
# f1_avg_totalyellows <- round((f1_totalyellows/ f1_games_played), digits = 4)
# f2_avg_totalyellows <- round((f2_totalyellows/ f2_games_played), digits = 4)
# g1_avg_totalyellows <- round((g1_totalyellows/ g1_games_played), digits = 4)
# i1_avg_totalyellows <- round((i1_totalyellows/ i1_games_played), digits = 4)
# i2_avg_totalyellows <- round((i2_totalyellows/ i2_games_played), digits = 4)
# n1_avg_totalyellows <- round((n1_totalyellows/ n1_games_played), digits = 4)
# p1_avg_totalyellows <- round((p1_totalyellows/ p1_games_played), digits = 4)
# sc0_avg_totalyellows <- round((sc0_totalyellows/ sc0_games_played), digits = 4)
# sc1_avg_totalyellows <- round((sc1_totalyellows/ sc1_games_played), digits = 4)
# sc2_avg_totalyellows <- round((sc2_totalyellows/ sc2_games_played), digits = 4)
# sc3_avg_totalyellows <- round((sc3_totalyellows/ sc3_games_played), digits = 4)
# sp1_avg_totalyellows <- round((sp1_totalyellows/ sp1_games_played), digits = 4)
# sp2_avg_totalyellows <- round((sp2_totalyellows/ sp2_games_played), digits = 4)
# t1_avg_totalyellows <- round((t1_totalyellows/ t1_games_played), digits = 4)
# #Remove NA values
mls_yellowtotalsv2[is.na(mls_yellowtotalsv2)] <- ""
arg_yellowtotalsv2[is.na(arg_yellowtotalsv2)] <- ""
# d2_yellowtotalsv2[is.na(d2_yellowtotalsv2)] <- ""
# e0_yellowtotalsv2[is.na(e0_yellowtotalsv2)] <- ""
# e1_yellowtotalsv2[is.na(e1_yellowtotalsv2)] <- ""
# e2_yellowtotalsv2[is.na(e2_yellowtotalsv2)] <- ""
# e3_yellowtotalsv2[is.na(e3_yellowtotalsv2)] <- ""
# ec_yellowtotalsv2[is.na(ec_yellowtotalsv2)] <- ""
# f1_yellowtotalsv2[is.na(f1_yellowtotalsv2)] <- ""
# f2_yellowtotalsv2[is.na(f2_yellowtotalsv2)] <- ""
# g1_yellowtotalsv2[is.na(g1_yellowtotalsv2)] <- ""
# i1_yellowtotalsv2[is.na(i1_yellowtotalsv2)] <- ""
# i2_yellowtotalsv2[is.na(i2_yellowtotalsv2)] <- ""
# n1_yellowtotalsv2[is.na(n1_yellowtotalsv2)] <- ""
# p1_yellowtotalsv2[is.na(p1_yellowtotalsv2)] <- ""
# sc0_yellowtotalsv2[is.na(sc0_yellowtotalsv2)] <- ""
# sc1_yellowtotalsv2[is.na(sc1_yellowtotalsv2)] <- ""
# sc2_yellowtotalsv2[is.na(sc2_yellowtotalsv2)] <- ""
# sc3_yellowtotalsv2[is.na(sc3_yellowtotalsv2)] <- ""
# sp1_yellowtotalsv2[is.na(sp1_yellowtotalsv2)] <- ""
# sp2_yellowtotalsv2[is.na(sp2_yellowtotalsv2)] <- ""
# t1_yellowtotalsv2[is.na(t1_yellowtotalsv2)] <- ""
#Bind averaye total yellows

mls_yellowtotalsv2 <- cbind(mls_yellowtotalsv2,mls_avg_totalyellows)
arg_yellowtotalsv2 <- cbind(arg_yellowtotalsv2,arg_avg_totalyellows)
# d2_yellowtotalsv2 <- cbind(d2_yellowtotalsv2,d2_avg_totalyellows)
# e0_yellowtotalsv2 <- cbind(e0_yellowtotalsv2,e0_avg_totalyellows)
# e1_yellowtotalsv2 <- cbind(e1_yellowtotalsv2,e1_avg_totalyellows)
# e2_yellowtotalsv2 <- cbind(e2_yellowtotalsv2,e2_avg_totalyellows)
# e3_yellowtotalsv2 <- cbind(e3_yellowtotalsv2,e3_avg_totalyellows)
# ec_yellowtotalsv2 <- cbind(ec_yellowtotalsv2,ec_avg_totalyellows)
# f1_yellowtotalsv2 <- cbind(f1_yellowtotalsv2,f1_avg_totalyellows)
# f2_yellowtotalsv2 <- cbind(f2_yellowtotalsv2,f2_avg_totalyellows)
# g1_yellowtotalsv2 <- cbind(g1_yellowtotalsv2,g1_avg_totalyellows)
# i1_yellowtotalsv2 <- cbind(i1_yellowtotalsv2,i1_avg_totalyellows)
# i2_yellowtotalsv2 <- cbind(i2_yellowtotalsv2,i2_avg_totalyellows)
# n1_yellowtotalsv2 <- cbind(n1_yellowtotalsv2,n1_avg_totalyellows)
# p1_yellowtotalsv2 <- cbind(p1_yellowtotalsv2,p1_avg_totalyellows)
# sc0_yellowtotalsv2 <- cbind(sc0_yellowtotalsv2,sc0_avg_totalyellows)
# sc1_yellowtotalsv2 <- cbind(sc1_yellowtotalsv2,sc1_avg_totalyellows)
# sc2_yellowtotalsv2 <- cbind(sc2_yellowtotalsv2,sc2_avg_totalyellows)
# sc3_yellowtotalsv2 <- cbind(sc3_yellowtotalsv2,sc3_avg_totalyellows)
# sp1_yellowtotalsv2 <- cbind(sp1_yellowtotalsv2,sp1_avg_totalyellows)
# sp2_yellowtotalsv2 <- cbind(sp2_yellowtotalsv2,sp2_avg_totalyellows)
# t1_yellowtotalsv2 <- cbind(t1_yellowtotalsv2,t1_avg_totalyellows)
#
# #Write out to excel files
# write.xlsx(mls_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "MLS")
# write.xlsx(arg_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_yellowtotalsv2,'YellowTotalsV2.xlsx',sheetName = "T1", append = TRUE)



