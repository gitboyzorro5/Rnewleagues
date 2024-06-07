# Goal totals V2
library('dplyr')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
#Create the home and awag team matrix
mls_cornertotalsv2 <- tapply(MLS$TC, MLS[c("HomeTeam", "AwayTeam")],mean)
arg_cornertotalsv2 <- tapply(ARG$TC, ARG[c("HomeTeam", "AwayTeam")],mean)
# d2_cornertotalsv2 <- tapply(D2$TC, D2[c("HomeTeam", "AwayTeam")],mean)
# e0_cornertotalsv2 <- tapply(E0$TC, E0[c("HomeTeam", "AwayTeam")],mean)
# e1_cornertotalsv2 <- tapply(E1$TC, E1[c("HomeTeam", "AwayTeam")],mean)
# e2_cornertotalsv2 <- tapply(E2$TC, E2[c("HomeTeam", "AwayTeam")],mean)
# e3_cornertotalsv2 <- tapply(E3$TC, E3[c("HomeTeam", "AwayTeam")],mean)
# ec_cornertotalsv2 <- tapply(EC$TC, EC[c("HomeTeam", "AwayTeam")],mean)
# f1_cornertotalsv2 <- tapply(F1$TC, F1[c("HomeTeam", "AwayTeam")],mean)
# f2_cornertotalsv2 <- tapply(F2$TC, F2[c("HomeTeam", "AwayTeam")],mean)
# g1_cornertotalsv2 <- tapply(G1$TC, G1[c("HomeTeam", "AwayTeam")],mean)
# i1_cornertotalsv2 <- tapply(I1$TC, I1[c("HomeTeam", "AwayTeam")],mean)
# i2_cornertotalsv2 <- tapply(I2$TC, I2[c("HomeTeam", "AwayTeam")],mean)
# n1_cornertotalsv2 <- tapply(N1$TC, N1[c("HomeTeam", "AwayTeam")],mean)
# p1_cornertotalsv2 <- tapply(P1$TC, P1[c("HomeTeam", "AwayTeam")],mean)
# sc0_cornertotalsv2 <- tapply(SC0$TC, SC0[c("HomeTeam", "AwayTeam")],mean)
# sc1_cornertotalsv2 <- tapply(SC1$TC, SC1[c("HomeTeam", "AwayTeam")],mean)
# sc2_cornertotalsv2 <- tapply(SC2$TC, SC2[c("HomeTeam", "AwayTeam")],mean)
# sc3_cornertotalsv2 <- tapply(SC3$TC, SC3[c("HomeTeam", "AwayTeam")],mean)
# sp1_cornertotalsv2 <- tapply(SP1$TC, SP1[c("HomeTeam", "AwayTeam")],mean)
# sp2_cornertotalsv2 <- tapply(SP2$TC, SP2[c("HomeTeam", "AwayTeam")],mean)
# t1_cornertotalsv2 <- tapply(T1$TC, T1[c("HomeTeam", "AwayTeam")],mean)
#Create the row and column sums
#MLS
mls_hcototals <- rowSums(mls_cornertotalsv2, na.rm = T)
mls_acototals <- colSums(mls_cornertotalsv2, na.rm = T)
#ARG
arg_hcototals <- rowSums(arg_cornertotalsv2, na.rm = T)
arg_acototals <- colSums(arg_cornertotalsv2, na.rm = T)
#D2
# d2_hcototals <- rowSums(d2_cornertotalsv2, na.rm = T)
# d2_acototals <- colSums(d2_cornertotalsv2, na.rm = T)
# #E0
# e0_hcototals <- rowSums(e0_cornertotalsv2, na.rm = T)
# e0_acototals <- colSums(e0_cornertotalsv2, na.rm = T)
# #E1
# e1_hcototals <- rowSums(e1_cornertotalsv2, na.rm = T)
# e1_acototals <- colSums(e1_cornertotalsv2, na.rm = T)
# #E2
# e2_hcototals <- rowSums(e2_cornertotalsv2, na.rm = T)
# e2_acototals <- colSums(e2_cornertotalsv2, na.rm = T)
# #E3
# e3_hcototals <- rowSums(e3_cornertotalsv2, na.rm = T)
# e3_acototals <- colSums(e3_cornertotalsv2, na.rm = T)
# #EC
# ec_hcototals <- rowSums(ec_cornertotalsv2, na.rm = T)
# ec_acototals <- colSums(ec_cornertotalsv2, na.rm = T)
# #F1
# f1_hcototals <- rowSums(f1_cornertotalsv2, na.rm = T)
# f1_acototals <- colSums(f1_cornertotalsv2, na.rm = T)
# #F2
# f2_hcototals <- rowSums(f2_cornertotalsv2, na.rm = T)
# f2_acototals <- colSums(f2_cornertotalsv2, na.rm = T)
# #G1
# g1_hcototals <- rowSums(g1_cornertotalsv2, na.rm = T)
# g1_acototals <- colSums(g1_cornertotalsv2, na.rm = T)
# #I1
# i1_hcototals <- rowSums(i1_cornertotalsv2, na.rm = T)
# i1_acototals <- colSums(i1_cornertotalsv2, na.rm = T)
# #I2
# i2_hcototals <- rowSums(i2_cornertotalsv2, na.rm = T)
# i2_acototals <- colSums(i2_cornertotalsv2, na.rm = T)
# #N1
# n1_hcototals <- rowSums(n1_cornertotalsv2, na.rm = T)
# n1_acototals <- colSums(n1_cornertotalsv2, na.rm = T)
# #P1
# p1_hcototals <- rowSums(p1_cornertotalsv2, na.rm = T)
# p1_acototals <- colSums(p1_cornertotalsv2, na.rm = T)
# #SC0
# sc0_hcototals <- rowSums(sc0_cornertotalsv2, na.rm = T)
# sc0_acototals <- colSums(sc0_cornertotalsv2, na.rm = T)
# #SC1
# sc1_hcototals <- rowSums(sc1_cornertotalsv2, na.rm = T)
# sc1_acototals <- colSums(sc1_cornertotalsv2, na.rm = T)
# #SC2
# sc2_hcototals <- rowSums(sc2_cornertotalsv2, na.rm = T)
# sc2_acototals <- colSums(sc2_cornertotalsv2, na.rm = T)
# #SC3
# sc3_hcototals <- rowSums(sc3_cornertotalsv2, na.rm = T)
# sc3_acototals <- colSums(sc3_cornertotalsv2, na.rm = T)
# #SP1
# sp1_hcototals <- rowSums(sp1_cornertotalsv2, na.rm = T)
# sp1_acototals <- colSums(sp1_cornertotalsv2, na.rm = T)
# #SP2
# sp2_hcototals <- rowSums(sp2_cornertotalsv2, na.rm = T)
# sp2_acototals <- colSums(sp2_cornertotalsv2, na.rm = T)
# #T1
# t1_hcototals <- rowSums(t1_cornertotalsv2, na.rm = T)
# t1_acototals <- colSums(t1_cornertotalsv2, na.rm = T)

#Bind hgtotal and agtotal
mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_hcototals,mls_acototals)
arg_cornertotalsv2 <- cbind(arg_cornertotalsv2,arg_hcototals,arg_acototals)
# d2_cornertotalsv2 <- cbind(d2_cornertotalsv2,d2_hcototals,d2_acototals)
# e0_cornertotalsv2 <- cbind(e0_cornertotalsv2,e0_hcototals,e0_acototals)
# e1_cornertotalsv2 <- cbind(e1_cornertotalsv2,e1_hcototals,e1_acototals)
# e2_cornertotalsv2 <- cbind(e2_cornertotalsv2,e2_hcototals,e2_acototals)
# e3_cornertotalsv2 <- cbind(e3_cornertotalsv2,e3_hcototals,e3_acototals)
# ec_cornertotalsv2 <- cbind(ec_cornertotalsv2,ec_hcototals,ec_acototals)
# f1_cornertotalsv2 <- cbind(f1_cornertotalsv2,f1_hcototals,f1_acototals)
# f2_cornertotalsv2 <- cbind(f2_cornertotalsv2,f2_hcototals,f2_acototals)
# g1_cornertotalsv2 <- cbind(g1_cornertotalsv2,g1_hcototals,g1_acototals)
# i1_cornertotalsv2 <- cbind(i1_cornertotalsv2,i1_hcototals,i1_acototals)
# i2_cornertotalsv2 <- cbind(i2_cornertotalsv2,i2_hcototals,i2_acototals)
# n1_cornertotalsv2 <- cbind(n1_cornertotalsv2,n1_hcototals,n1_acototals)
# p1_cornertotalsv2 <- cbind(p1_cornertotalsv2,p1_hcototals,p1_acototals)
# sc0_cornertotalsv2 <- cbind(sc0_cornertotalsv2,sc0_hcototals,sc0_acototals)
# sc1_cornertotalsv2 <- cbind(sc1_cornertotalsv2,sc1_hcototals,sc1_acototals)
# sc2_cornertotalsv2 <- cbind(sc2_cornertotalsv2,sc2_hcototals,sc2_acototals)
# sc3_cornertotalsv2 <- cbind(sc3_cornertotalsv2,sc3_hcototals,sc3_acototals)
# sp1_cornertotalsv2 <- cbind(sp1_cornertotalsv2,sp1_hcototals,sp1_acototals)
# sp2_cornertotalsv2 <- cbind(sp2_cornertotalsv2,sp2_hcototals,sp2_acototals)
# t1_cornertotalsv2 <- cbind(t1_cornertotalsv2,t1_hcototals,t1_acototals)

#add total corners
mls_totalcorners <- mls_hcototals + mls_acototals
arg_totalcorners <- arg_hcototals + arg_acototals
# d2_totalcorners <- d2_hcototals + d2_acototals
# e0_totalcorners <- e0_hcototals + e0_acototals
# e1_totalcorners <- e1_hcototals + e1_acototals
# e2_totalcorners <- e2_hcototals + e2_acototals
# e3_totalcorners <- e3_hcototals + e3_acototals
# ec_totalcorners <- ec_hcototals + ec_acototals
# f1_totalcorners <- f1_hcototals + f1_acototals
# f2_totalcorners <- f2_hcototals + f2_acototals
# g1_totalcorners <- g1_hcototals + g1_acototals
# i1_totalcorners <- i1_hcototals + i1_acototals
# i2_totalcorners <- i2_hcototals + i2_acototals
# n1_totalcorners <- n1_hcototals + n1_acototals
# p1_totalcorners <- p1_hcototals + p1_acototals
# sc0_totalcorners <- sc0_hcototals + sc0_acototals
# sc1_totalcorners <- sc1_hcototals + sc1_acototals
# sc2_totalcorners <- sc2_hcototals + sc2_acototals
# sc3_totalcorners <- sc3_hcototals + sc3_acototals
# sp1_totalcorners <- sp1_hcototals + sp1_acototals
# sp2_totalcorners <- sp2_hcototals + sp2_acototals
# t1_totalcorners <- t1_hcototals + t1_acototals
#bind total corners column
mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_totalcorners)
arg_cornertotalsv2 <- cbind(arg_cornertotalsv2,arg_totalcorners)
# d2_cornertotalsv2 <- cbind(d2_cornertotalsv2,d2_totalcorners)
# e0_cornertotalsv2 <- cbind(e0_cornertotalsv2,e0_totalcorners)
# e1_cornertotalsv2 <- cbind(e1_cornertotalsv2,e1_totalcorners)
# e2_cornertotalsv2 <- cbind(e2_cornertotalsv2,e2_totalcorners)
# e3_cornertotalsv2 <- cbind(e3_cornertotalsv2,e3_totalcorners)
# ec_cornertotalsv2 <- cbind(ec_cornertotalsv2,ec_totalcorners)
# f1_cornertotalsv2 <- cbind(f1_cornertotalsv2,f1_totalcorners)
# f2_cornertotalsv2 <- cbind(f2_cornertotalsv2,f2_totalcorners)
# g1_cornertotalsv2 <- cbind(g1_cornertotalsv2,g1_totalcorners)
# i1_cornertotalsv2 <- cbind(i1_cornertotalsv2,i1_totalcorners)
# i2_cornertotalsv2 <- cbind(i2_cornertotalsv2,i2_totalcorners)
# n1_cornertotalsv2 <- cbind(n1_cornertotalsv2,n1_totalcorners)
# p1_cornertotalsv2 <- cbind(p1_cornertotalsv2,p1_totalcorners)
# sc0_cornertotalsv2 <- cbind(sc0_cornertotalsv2,sc0_totalcorners)
# sc1_cornertotalsv2 <- cbind(sc1_cornertotalsv2,sc1_totalcorners)
# sc2_cornertotalsv2 <- cbind(sc2_cornertotalsv2,sc2_totalcorners)
# sc3_cornertotalsv2 <- cbind(sc3_cornertotalsv2,sc3_totalcorners)
# sp1_cornertotalsv2 <- cbind(sp1_cornertotalsv2,sp1_totalcorners)
# sp2_cornertotalsv2 <- cbind(sp2_cornertotalsv2,sp2_totalcorners)
# t1_cornertotalsv2 <- cbind(t1_cornertotalsv2,t1_totalcorners)
# #Get teams in each division as a vector
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
#initialize home and awag game vectors
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
#Bind total games plaged

mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_games_played)
arg_cornertotalsv2 <- cbind(arg_cornertotalsv2,arg_games_played)
# d2_cornertotalsv2 <- cbind(d2_cornertotalsv2,d2_games_played)
# e0_cornertotalsv2 <- cbind(e0_cornertotalsv2,e0_games_played)
# e1_cornertotalsv2 <- cbind(e1_cornertotalsv2,e1_games_played)
# e2_cornertotalsv2 <- cbind(e2_cornertotalsv2,e2_games_played)
# e3_cornertotalsv2 <- cbind(e3_cornertotalsv2,e3_games_played)
# ec_cornertotalsv2 <- cbind(ec_cornertotalsv2,ec_games_played)
# f1_cornertotalsv2 <- cbind(f1_cornertotalsv2,f1_games_played)
# f2_cornertotalsv2 <- cbind(f2_cornertotalsv2,f2_games_played)
# g1_cornertotalsv2 <- cbind(g1_cornertotalsv2,g1_games_played)
# i1_cornertotalsv2 <- cbind(i1_cornertotalsv2,i1_games_played)
# i2_cornertotalsv2 <- cbind(i2_cornertotalsv2,i2_games_played)
# n1_cornertotalsv2 <- cbind(n1_cornertotalsv2,n1_games_played)
# p1_cornertotalsv2 <- cbind(p1_cornertotalsv2,p1_games_played)
# sc0_cornertotalsv2 <- cbind(sc0_cornertotalsv2,sc0_games_played)
# sc1_cornertotalsv2 <- cbind(sc1_cornertotalsv2,sc1_games_played)
# sc2_cornertotalsv2 <- cbind(sc2_cornertotalsv2,sc2_games_played)
# sc3_cornertotalsv2 <- cbind(sc3_cornertotalsv2,sc3_games_played)
# sp1_cornertotalsv2 <- cbind(sp1_cornertotalsv2,sp1_games_played)
# sp2_cornertotalsv2 <- cbind(sp2_cornertotalsv2,sp2_games_played)
# t1_cornertotalsv2 <- cbind(t1_cornertotalsv2,t1_games_played)
#Calculate averaye total corners
mls_avg_totalcorners <- round((mls_totalcorners/ mls_games_played), digits = 4)
arg_avg_totalcorners <- round((arg_totalcorners/ arg_games_played), digits = 4)
# d2_avg_totalcorners <- round((d2_totalcorners/ d2_games_played), digits = 4)
# e0_avg_totalcorners <- round((e0_totalcorners/ e0_games_played), digits = 4)
# e1_avg_totalcorners <- round((e1_totalcorners/ e1_games_played), digits = 4)
# e2_avg_totalcorners <- round((e2_totalcorners/ e2_games_played), digits = 4)
# e3_avg_totalcorners <- round((e3_totalcorners/ e3_games_played), digits = 4)
# ec_avg_totalcorners <- round((ec_totalcorners/ ec_games_played), digits = 4)
# f1_avg_totalcorners <- round((f1_totalcorners/ f1_games_played), digits = 4)
# f2_avg_totalcorners <- round((f2_totalcorners/ f2_games_played), digits = 4)
# g1_avg_totalcorners <- round((g1_totalcorners/ g1_games_played), digits = 4)
# i1_avg_totalcorners <- round((i1_totalcorners/ i1_games_played), digits = 4)
# i2_avg_totalcorners <- round((i2_totalcorners/ i2_games_played), digits = 4)
# n1_avg_totalcorners <- round((n1_totalcorners/ n1_games_played), digits = 4)
# p1_avg_totalcorners <- round((p1_totalcorners/ p1_games_played), digits = 4)
# sc0_avg_totalcorners <- round((sc0_totalcorners/ sc0_games_played), digits = 4)
# sc1_avg_totalcorners <- round((sc1_totalcorners/ sc1_games_played), digits = 4)
# sc2_avg_totalcorners <- round((sc2_totalcorners/ sc2_games_played), digits = 4)
# sc3_avg_totalcorners <- round((sc3_totalcorners/ sc3_games_played), digits = 4)
# sp1_avg_totalcorners <- round((sp1_totalcorners/ sp1_games_played), digits = 4)
# sp2_avg_totalcorners <- round((sp2_totalcorners/ sp2_games_played), digits = 4)
# t1_avg_totalcorners <- round((t1_totalcorners/ t1_games_played), digits = 4)
#Remove NA values
mls_cornertotalsv2[is.na(mls_cornertotalsv2)] <- ""
arg_cornertotalsv2[is.na(arg_cornertotalsv2)] <- ""
# d2_cornertotalsv2[is.na(d2_cornertotalsv2)] <- ""
# e0_cornertotalsv2[is.na(e0_cornertotalsv2)] <- ""
# e1_cornertotalsv2[is.na(e1_cornertotalsv2)] <- ""
# e2_cornertotalsv2[is.na(e2_cornertotalsv2)] <- ""
# e3_cornertotalsv2[is.na(e3_cornertotalsv2)] <- ""
# ec_cornertotalsv2[is.na(ec_cornertotalsv2)] <- ""
# f1_cornertotalsv2[is.na(f1_cornertotalsv2)] <- ""
# f2_cornertotalsv2[is.na(f2_cornertotalsv2)] <- ""
# g1_cornertotalsv2[is.na(g1_cornertotalsv2)] <- ""
# i1_cornertotalsv2[is.na(i1_cornertotalsv2)] <- ""
# i2_cornertotalsv2[is.na(i2_cornertotalsv2)] <- ""
# n1_cornertotalsv2[is.na(n1_cornertotalsv2)] <- ""
# p1_cornertotalsv2[is.na(p1_cornertotalsv2)] <- ""
# sc0_cornertotalsv2[is.na(sc0_cornertotalsv2)] <- ""
# sc1_cornertotalsv2[is.na(sc1_cornertotalsv2)] <- ""
# sc2_cornertotalsv2[is.na(sc2_cornertotalsv2)] <- ""
# sc3_cornertotalsv2[is.na(sc3_cornertotalsv2)] <- ""
# sp1_cornertotalsv2[is.na(sp1_cornertotalsv2)] <- ""
# sp2_cornertotalsv2[is.na(sp2_cornertotalsv2)] <- ""
# t1_cornertotalsv2[is.na(t1_cornertotalsv2)] <- ""
#Bind average total corners

mls_cornertotalsv2 <- cbind(mls_cornertotalsv2,mls_avg_totalcorners)
arg_cornertotalsv2 <- cbind(arg_cornertotalsv2,arg_avg_totalcorners)
# d2_cornertotalsv2 <- cbind(d2_cornertotalsv2,d2_avg_totalcorners)
# e0_cornertotalsv2 <- cbind(e0_cornertotalsv2,e0_avg_totalcorners)
# e1_cornertotalsv2 <- cbind(e1_cornertotalsv2,e1_avg_totalcorners)
# e2_cornertotalsv2 <- cbind(e2_cornertotalsv2,e2_avg_totalcorners)
# e3_cornertotalsv2 <- cbind(e3_cornertotalsv2,e3_avg_totalcorners)
# ec_cornertotalsv2 <- cbind(ec_cornertotalsv2,ec_avg_totalcorners)
# f1_cornertotalsv2 <- cbind(f1_cornertotalsv2,f1_avg_totalcorners)
# f2_cornertotalsv2 <- cbind(f2_cornertotalsv2,f2_avg_totalcorners)
# g1_cornertotalsv2 <- cbind(g1_cornertotalsv2,g1_avg_totalcorners)
# i1_cornertotalsv2 <- cbind(i1_cornertotalsv2,i1_avg_totalcorners)
# i2_cornertotalsv2 <- cbind(i2_cornertotalsv2,i2_avg_totalcorners)
# n1_cornertotalsv2 <- cbind(n1_cornertotalsv2,n1_avg_totalcorners)
# p1_cornertotalsv2 <- cbind(p1_cornertotalsv2,p1_avg_totalcorners)
# sc0_cornertotalsv2 <- cbind(sc0_cornertotalsv2,sc0_avg_totalcorners)
# sc1_cornertotalsv2 <- cbind(sc1_cornertotalsv2,sc1_avg_totalcorners)
# sc2_cornertotalsv2 <- cbind(sc2_cornertotalsv2,sc2_avg_totalcorners)
# sc3_cornertotalsv2 <- cbind(sc3_cornertotalsv2,sc3_avg_totalcorners)
# sp1_cornertotalsv2 <- cbind(sp1_cornertotalsv2,sp1_avg_totalcorners)
# sp2_cornertotalsv2 <- cbind(sp2_cornertotalsv2,sp2_avg_totalcorners)
# t1_cornertotalsv2 <- cbind(t1_cornertotalsv2,t1_avg_totalcorners)
#
# unlink('CornerTotalsV2.xlsx')
# # #Write out to excel files
# write.xlsx(mls_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "MLS")
# write.xlsx(arg_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_cornertotalsv2,'CornertotalsV2.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_cornertotalsv2,'CornerTotalsV2.xlsx',sheetName = "T1", append = TRUE)

View(mls_cornertotalsv2)

