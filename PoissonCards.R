library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')
library('sqldf')
mls_GP <- nrow(MLS)
arg_GP <- nrow(ARG)
# d2_GP <- nrow(D2)
# e0_GP <- nrow(E0)
# e1_GP <- nrow(E1)
# e2_GP <- nrow(E2)
# e3_GP <- nrow(E3)
# ec_GP <- nrow(EC)
# f1_GP <- nrow(F1)
# f2_GP <- nrow(F2)
# g1_GP <- nrow(G1)
# i1_GP <- nrow(I1)
# i2_GP <- nrow(I2)
# n1_GP <- nrow(N1)
# p1_GP <- nrow(P1)
# sc0_GP <- nrow(SC0)
# sc1_GP <- nrow(SC1)
# sc2_GP <- nrow(SC2)
# sc3_GP <- nrow(SC3)
# sp1_GP <- nrow(SP1)
# sp2_GP <- nrow(SP2)
# t1_GP <- nrow(T1)

#Calculate total home goals for each division
mls_T_HY <- sum(mls_home_hyc$x)
arg_T_HY <- sum(arg_home_hyc$x)
# d2_T_HY <- sum(d2_home_hyc$x)
# e0_T_HY <- sum(e0_home_hyc$x)
# e1_T_HY <- sum(e1_home_hyc$x)
# e2_T_HY <- sum(e2_home_hyc$x)
# e3_T_HY <- sum(e3_home_hyc$x)
# ec_T_HY <- sum(ec_home_hyc$x)
# f1_T_HY <- sum(f1_home_hyc$x)
# f2_T_HY <- sum(f2_home_hyc$x)
# g1_T_HY <- sum(g1_home_hyc$x)
# i1_T_HY <- sum(i1_home_hyc$x)
# i2_T_HY <- sum(i2_home_hyc$x)
# n1_T_HY <- sum(n1_home_hyc$x)
# p1_T_HY <- sum(p1_home_hyc$x)
# sc0_T_HY <- sum(sc0_home_hyc$x)
# sc1_T_HY <- sum(sc1_home_hyc$x)
# sc2_T_HY <- sum(sc2_home_hyc$x)
# sc3_T_HY <- sum(sc3_home_hyc$x)
# sp1_T_HY <- sum(sp1_home_hyc$x)
# sp2_T_HY <- sum(sp2_home_hyc$x)
# t1_T_HY <- sum(t1_home_hyc$x)
#calculate average home goal

mls_avg_HY <- round(mls_T_HY /mls_GP, digits = 4)
arg_avg_HY <- round(arg_T_HY /arg_GP, digits = 4)
# d2_avg_HY <- round(d2_T_HY /d2_GP, digits = 4)
# e0_avg_HY <- round(e0_T_HY /e0_GP, digits = 4)
# e1_avg_HY <- round(e1_T_HY /e1_GP, digits = 4)
# e2_avg_HY <- round(e2_T_HY /e2_GP, digits = 4)
# e3_avg_HY <- round(e3_T_HY /e3_GP, digits = 4)
# ec_avg_HY <- round(ec_T_HY /ec_GP, digits = 4)
# f1_avg_HY <- round(f1_T_HY /f1_GP, digits = 4)
# f2_avg_HY <- round(f2_T_HY /f2_GP, digits = 4)
# g1_avg_HY <- round(g1_T_HY /g1_GP, digits = 4)
# i1_avg_HY <- round(i1_T_HY /i1_GP, digits = 4)
# i2_avg_HY <- round(i2_T_HY /i2_GP, digits = 4)
# n1_avg_HY <- round(n1_T_HY /n1_GP, digits = 4)
# p1_avg_HY <- round(p1_T_HY /p1_GP, digits = 4)
# sc0_avg_HY <- round(sc0_T_HY /sc0_GP, digits = 4)
# sc1_avg_HY <- round(sc1_T_HY /sc1_GP, digits = 4)
# sc2_avg_HY <- round(sc2_T_HY /sc2_GP, digits = 4)
# sc3_avg_HY <- round(sc3_T_HY /sc3_GP, digits = 4)
# sp1_avg_HY <- round(sp1_T_HY /sp1_GP, digits = 4)
# sp2_avg_HY <- round(sp2_T_HY /sp2_GP, digits = 4)
# t1_avg_HY <- round(t1_T_HY /t1_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AY <- sum(mls_away_ayc$x)
arg_T_AY <- sum(arg_away_ayc$x)
# d2_T_AY <- sum(d2_away_ayc$x)
# e0_T_AY <- sum(e0_away_ayc$x)
# e1_T_AY <- sum(e1_away_ayc$x)
# e2_T_AY <- sum(e2_away_ayc$x)
# e3_T_AY <- sum(e3_away_ayc$x)
# ec_T_AY <- sum(ec_away_ayc$x)
# f1_T_AY <- sum(f1_away_ayc$x)
# f2_T_AY <- sum(f2_away_ayc$x)
# g1_T_AY <- sum(g1_away_ayc$x)
# i1_T_AY <- sum(i1_away_ayc$x)
# i2_T_AY <- sum(i2_away_ayc$x)
# n1_T_AY <- sum(n1_away_ayc$x)
# p1_T_AY <- sum(p1_away_ayc$x)
# sc0_T_AY <- sum(sc0_away_ayc$x)
# sc1_T_AY <- sum(sc1_away_ayc$x)
# sc2_T_AY <- sum(sc2_away_ayc$x)
# sc3_T_AY <- sum(sc3_away_ayc$x)
# sp1_T_AY <- sum(sp1_away_ayc$x)
# sp2_T_AY <- sum(sp2_away_ayc$x)
# t1_T_AY <- sum(t1_away_ayc$x)
#calculate average away goal

mls_avg_AY <- round(mls_T_AY /mls_GP, digits = 4)
arg_avg_AY <- round(arg_T_AY /arg_GP, digits = 4)
# d2_avg_AY <- round(d2_T_AY /d2_GP, digits = 4)
# e0_avg_AY <- round(e0_T_AY /e0_GP, digits = 4)
# e1_avg_AY <- round(e1_T_AY /e1_GP, digits = 4)
# e2_avg_AY <- round(e2_T_AY /e2_GP, digits = 4)
# e3_avg_AY <- round(e3_T_AY /e3_GP, digits = 4)
# ec_avg_AY <- round(ec_T_AY /ec_GP, digits = 4)
# f1_avg_AY <- round(f1_T_AY /f1_GP, digits = 4)
# f2_avg_AY <- round(f2_T_AY /f2_GP, digits = 4)
# g1_avg_AY <- round(g1_T_AY /g1_GP, digits = 4)
# i1_avg_AY <- round(i1_T_AY /i1_GP, digits = 4)
# i2_avg_AY <- round(i2_T_AY /i2_GP, digits = 4)
# n1_avg_AY <- round(n1_T_AY /n1_GP, digits = 4)
# p1_avg_AY <- round(p1_T_AY /p1_GP, digits = 4)
# sc0_avg_AY <- round(sc0_T_AY /sc0_GP, digits = 4)
# sc1_avg_AY <- round(sc1_T_AY /sc1_GP, digits = 4)
# sc2_avg_AY <- round(sc2_T_AY /sc2_GP, digits = 4)
# sc3_avg_AY <- round(sc3_T_AY /sc3_GP, digits = 4)
# sp1_avg_AY <- round(sp1_T_AY /sp1_GP, digits = 4)
# sp2_avg_AY <- round(sp2_T_AY /sp2_GP, digits = 4)
# t1_avg_AY <- round(t1_T_AY /t1_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength

mls_home_yas <- round(((mls_home_hyc$x/mls_home_games))/mls_avg_HY, digits = 4)
arg_home_yas <- round(((arg_home_hyc$x/arg_home_games))/arg_avg_HY, digits = 4)
# d2_home_yas <- round(((d2_home_hyc$x/d2_home_games))/d2_avg_HY, digits = 4)
# e0_home_yas <- round(((e0_home_hyc$x/e0_home_games))/e0_avg_HY, digits = 4)
# e1_home_yas <- round(((e1_home_hyc$x/e1_home_games))/e1_avg_HY, digits = 4)
# e2_home_yas <- round(((e2_home_hyc$x/e2_home_games))/e2_avg_HY, digits = 4)
# e3_home_yas <- round(((e3_home_hyc$x/e3_home_games))/e3_avg_HY, digits = 4)
# ec_home_yas <- round(((ec_home_hyc$x/ec_home_games))/ec_avg_HY, digits = 4)
# f1_home_yas <- round(((f1_home_hyc$x/f1_home_games))/f1_avg_HY, digits = 4)
# f2_home_yas <- round(((f2_home_hyc$x/f2_home_games))/f2_avg_HY, digits = 4)
# g1_home_yas <- round(((g1_home_hyc$x/g1_home_games))/g1_avg_HY, digits = 4)
# i1_home_yas <- round(((i1_home_hyc$x/i1_home_games))/i1_avg_HY, digits = 4)
# i2_home_yas <- round(((i2_home_hyc$x/i2_home_games))/i2_avg_HY, digits = 4)
# n1_home_yas <- round(((n1_home_hyc$x/n1_home_games))/n1_avg_HY, digits = 4)
# p1_home_yas <- round(((p1_home_hyc$x/p1_home_games))/p1_avg_HY, digits = 4)
# sc0_home_yas <- round(((sc0_home_hyc$x/sc0_home_games))/sc0_avg_HY, digits = 4)
# sc1_home_yas <- round(((sc1_home_hyc$x/sc1_home_games))/sc1_avg_HY, digits = 4)
# sc2_home_yas <- round(((sc2_home_hyc$x/sc2_home_games))/sc2_avg_HY, digits = 4)
# sc3_home_yas <- round(((sc3_home_hyc$x/sc3_home_games))/sc3_avg_HY, digits = 4)
# sp1_home_yas <- round(((sp1_home_hyc$x/sp1_home_games))/sp1_avg_HY, digits = 4)
# sp2_home_yas <- round(((sp2_home_hyc$x/sp2_home_games))/sp2_avg_HY, digits = 4)
# t1_home_yas <- round(((t1_home_hyc$x/t1_home_games))/t1_avg_HY, digits = 4)
#calculate away attack strength
mls_away_yas <- round(((mls_away_ayc$x/mls_away_games))/mls_avg_AY, digits = 4)
arg_away_yas <- round(((arg_away_ayc$x/arg_away_games))/arg_avg_AY, digits = 4)
# d2_away_yas <- round(((d2_away_ayc$x/d2_away_games))/d2_avg_AY, digits = 4)
# e0_away_yas <- round(((e0_away_ayc$x/e0_away_games))/e0_avg_AY, digits = 4)
# e1_away_yas <- round(((e1_away_ayc$x/e1_away_games))/e1_avg_AY, digits = 4)
# e2_away_yas <- round(((e2_away_ayc$x/e2_away_games))/e2_avg_AY, digits = 4)
# e3_away_yas <- round(((e3_away_ayc$x/e3_away_games))/e3_avg_AY, digits = 4)
# ec_away_yas <- round(((ec_away_ayc$x/ec_away_games))/ec_avg_AY, digits = 4)
# f1_away_yas <- round(((f1_away_ayc$x/f1_away_games))/f1_avg_AY, digits = 4)
# f2_away_yas <- round(((f2_away_ayc$x/f2_away_games))/f2_avg_AY, digits = 4)
# g1_away_yas <- round(((g1_away_ayc$x/g1_away_games))/g1_avg_AY, digits = 4)
# i1_away_yas <- round(((i1_away_ayc$x/i1_away_games))/i1_avg_AY, digits = 4)
# i2_away_yas <- round(((i2_away_ayc$x/i2_away_games))/i2_avg_AY, digits = 4)
# n1_away_yas <- round(((n1_away_ayc$x/n1_away_games))/n1_avg_AY, digits = 4)
# p1_away_yas <- round(((p1_away_ayc$x/p1_away_games))/p1_avg_AY, digits = 4)
# sc0_away_yas <- round(((sc0_away_ayc$x/sc0_away_games))/sc0_avg_AY, digits = 4)
# sc1_away_yas <- round(((sc1_away_ayc$x/sc1_away_games))/sc1_avg_AY, digits = 4)
# sc2_away_yas <- round(((sc2_away_ayc$x/sc2_away_games))/sc2_avg_AY, digits = 4)
# sc3_away_yas <- round(((sc3_away_ayc$x/sc3_away_games))/sc3_avg_AY, digits = 4)
# sp1_away_yas <- round(((sp1_away_ayc$x/sp1_away_games))/sp1_avg_AY, digits = 4)
# sp2_away_yas <- round(((sp2_away_ayc$x/sp2_away_games))/sp2_avg_AY, digits = 4)
# t1_away_yas <- round(((t1_away_ayc$x/t1_away_games))/t1_avg_AY, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HYC <- round(mls_T_AY /mls_GP, digits = 4)
arg_avg_HYC <- round(arg_T_AY /arg_GP, digits = 4)
# d2_avg_HYC <- round(d2_T_AY /d2_GP, digits = 4)
# e0_avg_HYC <- round(e0_T_AY /e0_GP, digits = 4)
# e1_avg_HYC <- round(e1_T_AY /e1_GP, digits = 4)
# e2_avg_HYC <- round(e2_T_AY /e2_GP, digits = 4)
# e3_avg_HYC <- round(e3_T_AY /e3_GP, digits = 4)
# ec_avg_HYC <- round(ec_T_AY /ec_GP, digits = 4)
# f1_avg_HYC <- round(f1_T_AY /f1_GP, digits = 4)
# f2_avg_HYC <- round(f2_T_AY /f2_GP, digits = 4)
# g1_avg_HYC <- round(g1_T_AY /g1_GP, digits = 4)
# i1_avg_HYC <- round(i1_T_AY /i1_GP, digits = 4)
# i2_avg_HYC <- round(i2_T_AY /i2_GP, digits = 4)
# n1_avg_HYC <- round(n1_T_AY /n1_GP, digits = 4)
# p1_avg_HYC <- round(p1_T_AY /p1_GP, digits = 4)
# sc0_avg_HYC <- round(sc0_T_AY /sc0_GP, digits = 4)
# sc1_avg_HYC <- round(sc1_T_AY /sc1_GP, digits = 4)
# sc2_avg_HYC <- round(sc2_T_AY /sc2_GP, digits = 4)
# sc3_avg_HYC <- round(sc3_T_AY /sc3_GP, digits = 4)
# sp1_avg_HYC <- round(sp1_T_AY /sp1_GP, digits = 4)
# sp2_avg_HYC <- round(sp2_T_AY /sp2_GP, digits = 4)
# t1_avg_HYC <- round(t1_T_AY /t1_GP, digits = 4)
#avg away concede
mls_avg_AYC <- round(mls_T_HY /mls_GP, digits = 4)
arg_avg_AYC <- round(arg_T_HY /arg_GP, digits = 4)
# d2_avg_AYC <- round(d2_T_HY /d2_GP, digits = 4)
# e0_avg_AYC <- round(e0_T_HY /e0_GP, digits = 4)
# e1_avg_AYC <- round(e1_T_HY /e1_GP, digits = 4)
# e2_avg_AYC <- round(e2_T_HY /e2_GP, digits = 4)
# e3_avg_AYC <- round(e3_T_HY /e3_GP, digits = 4)
# ec_avg_AYC <- round(ec_T_HY /ec_GP, digits = 4)
# f1_avg_AYC <- round(f1_T_HY /f1_GP, digits = 4)
# f2_avg_AYC <- round(f2_T_HY /f2_GP, digits = 4)
# g1_avg_AYC <- round(g1_T_HY /g1_GP, digits = 4)
# i1_avg_AYC <- round(i1_T_HY /i1_GP, digits = 4)
# i2_avg_AYC <- round(i2_T_HY /i2_GP, digits = 4)
# n1_avg_AYC <- round(n1_T_HY /n1_GP, digits = 4)
# p1_avg_AYC <- round(p1_T_HY /p1_GP, digits = 4)
# sc0_avg_AYC <- round(sc0_T_HY /sc0_GP, digits = 4)
# sc1_avg_AYC <- round(sc1_T_HY /sc1_GP, digits = 4)
# sc2_avg_AYC <- round(sc2_T_HY /sc2_GP, digits = 4)
# sc3_avg_AYC <- round(sc3_T_HY /sc3_GP, digits = 4)
# sp1_avg_AYC <- round(sp1_T_HY /sp1_GP, digits = 4)
# sp2_avg_AYC <- round(sp2_T_HY /sp2_GP, digits = 4)
# t1_avg_AYC <- round(t1_T_HY /t1_GP, digits = 4)
# #calculate home and away defense strength
#home yellow cards conceded
mls_home_ycc <- aggregate(MLS$AY, by = list(MLS$HomeTeam), FUN = sum)
mls_away_ycc <- aggregate(MLS$HY, by = list(MLS$AwayTeam), FUN = sum)
arg_home_ycc <- aggregate(ARG$AY, by = list(ARG$HomeTeam), FUN = sum)
arg_away_ycc <- aggregate(ARG$HY, by = list(ARG$AwayTeam), FUN = sum)
# d2_home_ycc <- aggregate(D2$AY, by = list(D2$HomeTeam), FUN = sum)
# d2_away_ycc <- aggregate(D2$HY, by = list(D2$AwayTeam), FUN = sum)
# e0_home_ycc <- aggregate(E0$AY, by = list(E0$HomeTeam), FUN = sum)
# e0_away_ycc <- aggregate(E0$HY, by = list(E0$AwayTeam), FUN = sum)
# e1_home_ycc <- aggregate(E1$AY, by = list(E1$HomeTeam), FUN = sum)
# e1_away_ycc <- aggregate(E1$HY, by = list(E1$AwayTeam), FUN = sum)
# e2_home_ycc <- aggregate(E2$AY, by = list(E2$HomeTeam), FUN = sum)
# e2_away_ycc <- aggregate(E2$HY, by = list(E2$AwayTeam), FUN = sum)
# e3_home_ycc <- aggregate(E3$AY, by = list(E3$HomeTeam), FUN = sum)
# e3_away_ycc <- aggregate(E3$HY, by = list(E3$AwayTeam), FUN = sum)
# ec_home_ycc <- aggregate(EC$AY, by = list(EC$HomeTeam), FUN = sum)
# ec_away_ycc <- aggregate(EC$HY, by = list(EC$AwayTeam), FUN = sum)
# f1_home_ycc <- aggregate(F1$AY, by = list(F1$HomeTeam), FUN = sum)
# f1_away_ycc <- aggregate(F1$HY, by = list(F1$AwayTeam), FUN = sum)
# f2_home_ycc <- aggregate(F2$AY, by = list(F2$HomeTeam), FUN = sum)
# f2_away_ycc <- aggregate(F2$HY, by = list(F2$AwayTeam), FUN = sum)
# g1_home_ycc <- aggregate(G1$AY, by = list(G1$HomeTeam), FUN = sum)
# g1_away_ycc <- aggregate(G1$HY, by = list(G1$AwayTeam), FUN = sum)
# i1_home_ycc <- aggregate(I1$AY, by = list(I1$HomeTeam), FUN = sum)
# i1_away_ycc <- aggregate(I1$HY, by = list(I1$AwayTeam), FUN = sum)
# i2_home_ycc <- aggregate(I2$AY, by = list(I2$HomeTeam), FUN = sum)
# i2_away_ycc <- aggregate(I2$HY, by = list(I2$AwayTeam), FUN = sum)
# n1_home_ycc <- aggregate(N1$AY, by = list(N1$HomeTeam), FUN = sum)
# n1_away_ycc <- aggregate(N1$HY, by = list(N1$AwayTeam), FUN = sum)
# p1_home_ycc <- aggregate(P1$AY, by = list(P1$HomeTeam), FUN = sum)
# p1_away_ycc <- aggregate(P1$HY, by = list(P1$AwayTeam), FUN = sum)
# sc0_home_ycc <- aggregate(SC0$AY, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_ycc <- aggregate(SC0$HY, by = list(SC0$AwayTeam), FUN = sum)
# sc1_home_ycc <- aggregate(SC1$AY, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_ycc <- aggregate(SC1$HY, by = list(SC1$AwayTeam), FUN = sum)
# sc2_home_ycc <- aggregate(SC2$AY, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_ycc <- aggregate(SC2$HY, by = list(SC2$AwayTeam), FUN = sum)
# sc3_home_ycc <- aggregate(SC3$AY, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_ycc <- aggregate(SC3$HY, by = list(SC3$AwayTeam), FUN = sum)
# sp1_home_ycc <- aggregate(SP1$AY, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_ycc <- aggregate(SP1$HY, by = list(SP1$AwayTeam), FUN = sum)
# sp2_home_ycc <- aggregate(SP2$AY, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_ycc <- aggregate(SP2$HY, by = list(SP2$AwayTeam), FUN = sum)
# t1_home_ycc <- aggregate(T1$AY, by = list(T1$HomeTeam), FUN = sum)
# t1_away_ycc <- aggregate(T1$HY, by = list(T1$AwayTeam), FUN = sum)
#home defense strength
mls_home_yds <- round(((mls_home_ycc$x/mls_home_games))/mls_avg_HYC, digits = 4)
arg_home_yds <- round(((arg_home_ycc$x/arg_home_games))/arg_avg_HYC, digits = 4)
# d2_home_yds <- round(((d2_home_ycc$x/d2_home_games))/d2_avg_HYC, digits = 4)
# e0_home_yds <- round(((e0_home_ycc$x/e0_home_games))/e0_avg_HYC, digits = 4)
# e1_home_yds <- round(((e1_home_ycc$x/e1_home_games))/e1_avg_HYC, digits = 4)
# e2_home_yds <- round(((e2_home_ycc$x/e2_home_games))/e2_avg_HYC, digits = 4)
# e3_home_yds <- round(((e3_home_ycc$x/e3_home_games))/e3_avg_HYC, digits = 4)
# ec_home_yds <- round(((ec_home_ycc$x/ec_home_games))/ec_avg_HYC, digits = 4)
# f1_home_yds <- round(((f1_home_ycc$x/f1_home_games))/f1_avg_HYC, digits = 4)
# f2_home_yds <- round(((f2_home_ycc$x/f2_home_games))/f2_avg_HYC, digits = 4)
# g1_home_yds <- round(((g1_home_ycc$x/g1_home_games))/g1_avg_HYC, digits = 4)
# i1_home_yds <- round(((i1_home_ycc$x/i1_home_games))/i1_avg_HYC, digits = 4)
# i2_home_yds <- round(((i2_home_ycc$x/i2_home_games))/i2_avg_HYC, digits = 4)
# n1_home_yds <- round(((n1_home_ycc$x/n1_home_games))/n1_avg_HYC, digits = 4)
# p1_home_yds <- round(((p1_home_ycc$x/p1_home_games))/p1_avg_HYC, digits = 4)
# sc0_home_yds <- round(((sc0_home_ycc$x/sc0_home_games))/sc0_avg_HYC, digits = 4)
# sc1_home_yds <- round(((sc1_home_ycc$x/sc1_home_games))/sc1_avg_HYC, digits = 4)
# sc2_home_yds <- round(((sc2_home_ycc$x/sc2_home_games))/sc2_avg_HYC, digits = 4)
# sc3_home_yds <- round(((sc3_home_ycc$x/sc3_home_games))/sc3_avg_HYC, digits = 4)
# sp1_home_yds <- round(((sp1_home_ycc$x/sp1_home_games))/sp1_avg_HYC, digits = 4)
# sp2_home_yds <- round(((sp2_home_ycc$x/sp2_home_games))/sp2_avg_HYC, digits = 4)
# t1_home_yds <- round(((t1_home_ycc$x/t1_home_games))/t1_avg_HYC, digits = 4)
#away defense strength
mls_away_yds <- round(((mls_away_ycc$x/mls_away_games))/mls_avg_AYC, digits = 4)
arg_away_yds <- round(((arg_away_ycc$x/arg_away_games))/arg_avg_AYC, digits = 4)
# d2_away_yds <- round(((d2_away_ycc$x/d2_away_games))/d2_avg_AYC, digits = 4)
# e0_away_yds <- round(((e0_away_ycc$x/e0_away_games))/e0_avg_AYC, digits = 4)
# e1_away_yds <- round(((e1_away_ycc$x/e1_away_games))/e1_avg_AYC, digits = 4)
# e2_away_yds <- round(((e2_away_ycc$x/e2_away_games))/e2_avg_AYC, digits = 4)
# e3_away_yds <- round(((e3_away_ycc$x/e3_away_games))/e3_avg_AYC, digits = 4)
# ec_away_yds <- round(((ec_away_ycc$x/ec_away_games))/ec_avg_AYC, digits = 4)
# f1_away_yds <- round(((f1_away_ycc$x/f1_away_games))/f1_avg_AYC, digits = 4)
# f2_away_yds <- round(((f2_away_ycc$x/f2_away_games))/f2_avg_AYC, digits = 4)
# g1_away_yds <- round(((g1_away_ycc$x/g1_away_games))/g1_avg_AYC, digits = 4)
# i1_away_yds <- round(((i1_away_ycc$x/i1_away_games))/i1_avg_AYC, digits = 4)
# i2_away_yds <- round(((i2_away_ycc$x/i2_away_games))/i2_avg_AYC, digits = 4)
# n1_away_yds <- round(((n1_away_ycc$x/n1_away_games))/n1_avg_AYC, digits = 4)
# p1_away_yds <- round(((p1_away_ycc$x/p1_away_games))/p1_avg_AYC, digits = 4)
# sc0_away_yds <- round(((sc0_away_ycc$x/sc0_away_games))/sc0_avg_AYC, digits = 4)
# sc1_away_yds <- round(((sc1_away_ycc$x/sc1_away_games))/sc1_avg_AYC, digits = 4)
# sc2_away_yds <- round(((sc2_away_ycc$x/sc2_away_games))/sc2_avg_AYC, digits = 4)
# sc3_away_yds <- round(((sc3_away_ycc$x/sc3_away_games))/sc3_avg_AYC, digits = 4)
# sp1_away_yds <- round(((sp1_away_ycc$x/sp1_away_games))/sp1_avg_AYC, digits = 4)
# sp2_away_yds <- round(((sp2_away_ycc$x/sp2_away_games))/sp2_avg_AYC, digits = 4)
# t1_away_yds <- round(((t1_away_ycc$x/t1_away_games))/t1_avg_AYC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_yc <- cbind(mls_division,mls_teams,mls_avg_HY,mls_home_yas,mls_home_yds)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_home_poisson_yc <- cbind(arg_division,arg_teams,arg_avg_HY,arg_home_yas,arg_home_yds)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_home_poisson_yc <- cbind(d2_division,d2_teams,d2_avg_HY,d2_home_yas,d2_home_yds)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_home_poisson_yc <- cbind(e0_division,e0_teams,e0_avg_HY,e0_home_yas,e0_home_yds)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_home_poisson_yc <- cbind(e1_division,e1_teams,e1_avg_HY,e1_home_yas,e1_home_yds)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_home_poisson_yc <- cbind(e2_division,e2_teams,e2_avg_HY,e2_home_yas,e2_home_yds)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_home_poisson_yc <- cbind(e3_division,e3_teams,e3_avg_HY,e3_home_yas,e3_home_yds)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_home_poisson_yc <- cbind(ec_division,ec_teams,ec_avg_HY,ec_home_yas,ec_home_yds)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_home_poisson_yc <- cbind(f1_division,f1_teams,f1_avg_HY,f1_home_yas,f1_home_yds)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_home_poisson_yc <- cbind(f2_division,f2_teams,f2_avg_HY,f2_home_yas,f2_home_yds)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_home_poisson_yc <- cbind(g1_division,g1_teams,g1_avg_HY,g1_home_yas,g1_home_yds)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_home_poisson_yc <- cbind(i1_division,i1_teams,i1_avg_HY,i1_home_yas,i1_home_yds)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_home_poisson_yc <- cbind(i2_division,i2_teams,i2_avg_HY,i2_home_yas,i2_home_yds)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_home_poisson_yc <- cbind(n1_division,n1_teams,n1_avg_HY,n1_home_yas,n1_home_yds)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_home_poisson_yc <- cbind(p1_division,p1_teams,p1_avg_HY,p1_home_yas,p1_home_yds)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_home_poisson_yc <- cbind(sc0_division,sc0_teams,sc0_avg_HY,sc0_home_yas,sc0_home_yds)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_home_poisson_yc <- cbind(sc1_division,sc1_teams,sc1_avg_HY,sc1_home_yas,sc1_home_yds)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_home_poisson_yc <- cbind(sc2_division,sc2_teams,sc2_avg_HY,sc2_home_yas,sc2_home_yds)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_home_poisson_yc <- cbind(sc3_division,sc3_teams,sc3_avg_HY,sc3_home_yas,sc3_home_yds)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_home_poisson_yc <- cbind(sp1_division,sp1_teams,sp1_avg_HY,sp1_home_yas,sp1_home_yds)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_home_poisson_yc <- cbind(sp2_division,sp2_teams,sp2_avg_HY,sp2_home_yas,sp2_home_yds)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_home_poisson_yc <- cbind(t1_division,t1_teams,t1_avg_HY,t1_home_yas,t1_home_yds)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_yc <- cbind(mls_division,mls_teams,mls_avg_AY,mls_away_yas,mls_away_yds)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_away_poisson_yc <- cbind(arg_division,arg_teams,arg_avg_AY,arg_away_yas,arg_away_yds)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_away_poisson_yc <- cbind(d2_division,d2_teams,d2_avg_AY,d2_away_yas,d2_away_yds)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_away_poisson_yc <- cbind(e0_division,e0_teams,e0_avg_AY,e0_away_yas,e0_away_yds)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_away_poisson_yc <- cbind(e1_division,e1_teams,e1_avg_AY,e1_away_yas,e1_away_yds)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_away_poisson_yc <- cbind(e2_division,e2_teams,e2_avg_AY,e2_away_yas,e2_away_yds)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_away_poisson_yc <- cbind(e3_division,e3_teams,e3_avg_AY,e3_away_yas,e3_away_yds)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_away_poisson_yc <- cbind(ec_division,ec_teams,ec_avg_AY,ec_away_yas,ec_away_yds)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_away_poisson_yc <- cbind(f1_division,f1_teams,f1_avg_AY,f1_away_yas,f1_away_yds)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_away_poisson_yc <- cbind(f2_division,f2_teams,f2_avg_AY,f2_away_yas,f2_away_yds)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_away_poisson_yc <- cbind(g1_division,g1_teams,g1_avg_AY,g1_away_yas,g1_away_yds)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_away_poisson_yc <- cbind(i1_division,i1_teams,i1_avg_AY,i1_away_yas,i1_away_yds)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_away_poisson_yc <- cbind(i2_division,i2_teams,i2_avg_AY,i2_away_yas,i2_away_yds)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_away_poisson_yc <- cbind(n1_division,n1_teams,n1_avg_AY,n1_away_yas,n1_away_yds)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_away_poisson_yc <- cbind(p1_division,p1_teams,p1_avg_AY,p1_away_yas,p1_away_yds)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_away_poisson_yc <- cbind(sc0_division,sc0_teams,sc0_avg_AY,sc0_away_yas,sc0_away_yds)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_away_poisson_yc <- cbind(sc1_division,sc1_teams,sc1_avg_AY,sc1_away_yas,sc1_away_yds)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_away_poisson_yc <- cbind(sc2_division,sc2_teams,sc2_avg_AY,sc2_away_yas,sc2_away_yds)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_away_poisson_yc <- cbind(sc3_division,sc3_teams,sc3_avg_AY,sc3_away_yas,sc3_away_yds)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_away_poisson_yc <- cbind(sp1_division,sp1_teams,sp1_avg_AY,sp1_away_yas,sp1_away_yds)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_away_poisson_yc <- cbind(sp2_division,sp2_teams,sp2_avg_AY,sp2_away_yas,sp2_away_yds)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_away_poisson_yc <- cbind(t1_division,t1_teams,t1_avg_AY,t1_away_yas,t1_away_yds)
# #create home and away csv
home_poisson_yc <- rbind(mls_home_poisson_yc,arg_home_poisson_yc)
away_poisson_yc <- rbind(mls_away_poisson_yc,arg_away_poisson_yc)
# unlink("R_home.csv")
# unlink("R_away.csv")
# #write another one
# write.csv(home_poisson,'R_home.csv')
# write.csv(away_poisson,'R_away.csv')
#MLS
HomeTeam_mls_yc <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls_yc <- rep(mls_teams, length(mls_teams))
MLS_fixtures_yc <- cbind(HomeTeam_mls_yc,AwayTeam_mls_yc)
MLS_fixtures_yc <- as.data.frame(MLS_fixtures_yc)
MLS_fixtures_yc <- MLS_fixtures_yc[!MLS_fixtures_yc$HomeTeam_mls_yc == MLS_fixtures_yc$AwayTeam_mls_yc,]
rownames(MLS_fixtures_yc) <- NULL
MLS_fixtures_yc$Div <- "MLS"
MLS_fixtures_yc <- MLS_fixtures_yc[,c(3,1,2)]

MLS_fixtures_yc$avg_HY_mls <- mls_avg_HY

MLS_fixtures_yc$mls_homeyas <- rep(mls_home_yas,each = length(mls_teams)-1)

mls_awayyds_lookup <- cbind(mls_teams,mls_away_yds)

mls_awayyds_lookup <- as.data.frame(mls_awayyds_lookup)

colnames(mls_awayyds_lookup) <- c("AwayTeam_mls_yc","mls_awayyds")


require('RH2')
MLS_fixtures_yc$mls_awayyds <- sqldf("SELECT mls_awayyds_lookup.mls_awayyds FROM mls_awayyds_lookup INNER JOIN MLS_fixtures_yc ON mls_awayyds_lookup.AwayTeam_mls_yc = MLS_fixtures_yc.AwayTeam_mls_yc")

MLS_fixtures_yc$avg_AY_mls <- mls_avg_AY

mls_awayyas_lookup <- cbind(mls_teams,mls_away_yas)

mls_awayyas_lookup <- as.data.frame(mls_awayyas_lookup)

colnames(mls_awayyas_lookup) <- c("AwayTeam_mls_yc","mls_awayyas")

MLS_fixtures_yc$mls_awayyas <- sqldf("SELECT mls_awayyas_lookup.mls_awayyas FROM mls_awayyas_lookup INNER JOIN MLS_fixtures_yc ON mls_awayyas_lookup.AwayTeam_mls_yc = MLS_fixtures_yc.AwayTeam_mls_yc")

MLS_fixtures_yc$mls_homeyds <- rep(mls_home_yds,each = length(mls_teams)-1)

MLS_fixtures_yc$mls_awayyds <- as.numeric(unlist(MLS_fixtures_yc$mls_awayyds))
#xGH
MLS_fixtures_yc$mls_xHYC <- MLS_fixtures_yc$avg_HY_mls * MLS_fixtures_yc$mls_homeyas * MLS_fixtures_yc$mls_awayyds
#xGA

MLS_fixtures_yc$mls_awayyas <- as.numeric(unlist(MLS_fixtures_yc$mls_awayyas))

MLS_fixtures_yc$mls_xAYC <- MLS_fixtures_yc$avg_AY_mls * MLS_fixtures_yc$mls_awayyas * MLS_fixtures_yc$mls_homeyds

MLS_fixtures_yc$mls_0_0 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_0 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_1 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_1 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_0 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_2 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_2 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_1 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_2 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_3 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_0 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_1 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_2 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_3 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_3 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_3 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_4 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_0 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_1 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_2 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_3 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_4 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_4 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_4 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_4 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_5 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_0 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_1 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_2 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_3 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_4 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_5 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_5 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_5 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_5 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_5 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_6 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_0 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(0,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_1 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(1,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_2 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(2,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_3 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(3,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_4 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(4,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_6_5 <- round(stats::dpois(6,MLS_fixtures_yc$mls_xHYC) * stats::dpois(5,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_0_6 <- round(stats::dpois(0,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_1_6 <- round(stats::dpois(1,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_2_6 <- round(stats::dpois(2,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_3_6 <- round(stats::dpois(3,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_4_6 <- round(stats::dpois(4,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
MLS_fixtures_yc$mls_5_6 <- round(stats::dpois(5,MLS_fixtures_yc$mls_xHYC) * stats::dpois(6,MLS_fixtures_yc$mls_xAYC), digits = 4)
#Home win
MLS_fixtures_yc$mls_H <- (
  MLS_fixtures_yc$mls_1_0 + MLS_fixtures_yc$mls_2_0 + MLS_fixtures_yc$mls_2_1 + MLS_fixtures_yc$mls_3_0 + MLS_fixtures_yc$mls_3_1 +
    MLS_fixtures_yc$mls_3_2 + MLS_fixtures_yc$mls_4_0 + MLS_fixtures_yc$mls_4_1 + MLS_fixtures_yc$mls_4_2 + MLS_fixtures_yc$mls_4_3 +
    MLS_fixtures_yc$mls_5_0 + MLS_fixtures_yc$mls_5_1 + MLS_fixtures_yc$mls_5_2 + MLS_fixtures_yc$mls_5_3 + MLS_fixtures_yc$mls_5_4 +
    MLS_fixtures_yc$mls_6_0 + MLS_fixtures_yc$mls_6_1 + MLS_fixtures_yc$mls_6_2 + MLS_fixtures_yc$mls_6_3 + MLS_fixtures_yc$mls_6_4 +
    MLS_fixtures_yc$mls_6_5
)

MLS_fixtures_yc$mls_H <- percent(MLS_fixtures_yc$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures_yc$mls_D <- (

  MLS_fixtures_yc$mls_0_0 + MLS_fixtures_yc$mls_1_1 + MLS_fixtures_yc$mls_2_2 + MLS_fixtures_yc$mls_3_3 + MLS_fixtures_yc$mls_4_4 +
    MLS_fixtures_yc$mls_5_5 + MLS_fixtures_yc$mls_6_6
)

MLS_fixtures_yc$mls_D <- percent(MLS_fixtures_yc$mls_D, accuracy = 0.1)

#Away

MLS_fixtures_yc$mls_A <- (
  MLS_fixtures_yc$mls_0_1 + MLS_fixtures_yc$mls_0_2 + MLS_fixtures_yc$mls_1_2 + MLS_fixtures_yc$mls_0_3 + MLS_fixtures_yc$mls_1_3 +
    MLS_fixtures_yc$mls_2_3 + MLS_fixtures_yc$mls_0_4 + MLS_fixtures_yc$mls_1_4 + MLS_fixtures_yc$mls_2_4 + MLS_fixtures_yc$mls_3_4 +
    MLS_fixtures_yc$mls_0_5 + MLS_fixtures_yc$mls_1_5 + MLS_fixtures_yc$mls_2_5 + MLS_fixtures_yc$mls_3_5 + MLS_fixtures_yc$mls_4_5 +
    MLS_fixtures_yc$mls_0_6 + MLS_fixtures_yc$mls_1_6 + MLS_fixtures_yc$mls_2_6 + MLS_fixtures_yc$mls_3_6 + MLS_fixtures_yc$mls_4_6 +
    MLS_fixtures_yc$mls_5_6
)

MLS_fixtures_yc$mls_A <- percent(MLS_fixtures_yc$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures_yc$mls_ov25 <- (
  MLS_fixtures_yc$mls_2_1 + MLS_fixtures_yc$mls_1_2 + MLS_fixtures_yc$mls_2_2 + MLS_fixtures_yc$mls_3_0 + MLS_fixtures_yc$mls_3_1 +
    MLS_fixtures_yc$mls_3_2 + MLS_fixtures_yc$mls_0_3 + MLS_fixtures_yc$mls_1_3 + MLS_fixtures_yc$mls_2_3 + MLS_fixtures_yc$mls_3_3 +
    MLS_fixtures_yc$mls_4_0 + MLS_fixtures_yc$mls_4_1 + MLS_fixtures_yc$mls_4_2 + MLS_fixtures_yc$mls_4_3 + MLS_fixtures_yc$mls_0_4 +
    MLS_fixtures_yc$mls_1_4 + MLS_fixtures_yc$mls_2_4 + MLS_fixtures_yc$mls_3_4 + MLS_fixtures_yc$mls_4_4 + MLS_fixtures_yc$mls_5_0 +
    MLS_fixtures_yc$mls_5_1 + MLS_fixtures_yc$mls_5_2 + MLS_fixtures_yc$mls_5_3 + MLS_fixtures_yc$mls_5_4 + MLS_fixtures_yc$mls_0_5 +
    MLS_fixtures_yc$mls_1_5 + MLS_fixtures_yc$mls_2_5 + MLS_fixtures_yc$mls_3_5 + MLS_fixtures_yc$mls_4_5 + MLS_fixtures_yc$mls_5_5 +
    MLS_fixtures_yc$mls_6_0 + MLS_fixtures_yc$mls_6_1 + MLS_fixtures_yc$mls_6_2 + MLS_fixtures_yc$mls_6_3 + MLS_fixtures_yc$mls_6_4 +
    MLS_fixtures_yc$mls_6_5 + MLS_fixtures_yc$mls_0_6 + MLS_fixtures_yc$mls_1_6 + MLS_fixtures_yc$mls_2_6 + MLS_fixtures_yc$mls_3_6 +
    MLS_fixtures_yc$mls_4_6 + MLS_fixtures_yc$mls_5_6 + MLS_fixtures_yc$mls_6_6
)
#un25
MLS_fixtures_yc$mls_un25 <- (
  MLS_fixtures_yc$mls_0_0 + MLS_fixtures_yc$mls_1_0 + MLS_fixtures_yc$mls_0_1 + MLS_fixtures_yc$mls_1_1 + MLS_fixtures_yc$mls_2_0 + MLS_fixtures_yc$mls_0_2
)
#odds
MLS_fixtures_yc$mls_ov25_odds <- round((1/MLS_fixtures_yc$mls_ov25),digits = 2)
MLS_fixtures_yc$mls_un25_odds <- round((1/MLS_fixtures_yc$mls_un25),digits = 2)

MLS_fixtures_yc$mls_ov25_odds
MLS_fixtures_yc$mls_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures_yc$mls_ov25 <- percent(MLS_fixtures_yc$mls_ov25, accuracy = 0.1)

MLS_fixtures_yc$mls_un25 <- percent(MLS_fixtures_yc$mls_un25, accuracy = 0.1)
MLS_fixtures_yc$mls_pscore <- paste(round(MLS_fixtures_yc$mls_xHYC,digits = 0),round(MLS_fixtures_yc$mls_xAYC,digits = 0),sep = "-")
#write out
#write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
##################################################################################################################
#ARG
HomeTeam_arg_yc <- rep(arg_teams, each = length(arg_teams))
AwayTeam_arg_yc <- rep(arg_teams, length(arg_teams))
ARG_fixtures_yc <- cbind(HomeTeam_arg_yc,AwayTeam_arg_yc)
ARG_fixtures_yc <- as.data.frame(ARG_fixtures_yc)
ARG_fixtures_yc <- ARG_fixtures_yc[!ARG_fixtures_yc$HomeTeam_arg_yc == ARG_fixtures_yc$AwayTeam_arg_yc,]
rownames(ARG_fixtures_yc) <- NULL
ARG_fixtures_yc$Div <- "ARG"
ARG_fixtures_yc <- ARG_fixtures_yc[,c(3,1,2)]

ARG_fixtures_yc$avg_HY_arg <- arg_avg_HY

ARG_fixtures_yc$arg_homeyas <- rep(arg_home_yas,each = length(arg_teams)-1)

arg_awayyds_lookup <- cbind(arg_teams,arg_away_yds)

arg_awayyds_lookup <- as.data.frame(arg_awayyds_lookup)

colnames(arg_awayyds_lookup) <- c("AwayTeam_arg_yc","arg_awayyds")


require('RH2')
ARG_fixtures_yc$arg_awayyds <- sqldf("SELECT arg_awayyds_lookup.arg_awayyds FROM arg_awayyds_lookup INNER JOIN ARG_fixtures_yc ON arg_awayyds_lookup.AwayTeam_arg_yc = ARG_fixtures_yc.AwayTeam_arg_yc")

ARG_fixtures_yc$avg_AY_arg <- arg_avg_AY

arg_awayyas_lookup <- cbind(arg_teams,arg_away_yas)

arg_awayyas_lookup <- as.data.frame(arg_awayyas_lookup)

colnames(arg_awayyas_lookup) <- c("AwayTeam_arg_yc","arg_awayyas")

ARG_fixtures_yc$arg_awayyas <- sqldf("SELECT arg_awayyas_lookup.arg_awayyas FROM arg_awayyas_lookup INNER JOIN ARG_fixtures_yc ON arg_awayyas_lookup.AwayTeam_arg_yc = ARG_fixtures_yc.AwayTeam_arg_yc")

ARG_fixtures_yc$arg_homeyds <- rep(arg_home_yds,each = length(arg_teams)-1)

ARG_fixtures_yc$arg_awayyds <- as.numeric(unlist(ARG_fixtures_yc$arg_awayyds))
#xGH
ARG_fixtures_yc$arg_xHYC <- ARG_fixtures_yc$avg_HY_arg * ARG_fixtures_yc$arg_homeyas * ARG_fixtures_yc$arg_awayyds
#xGA

ARG_fixtures_yc$arg_awayyas <- as.numeric(unlist(ARG_fixtures_yc$arg_awayyas))

ARG_fixtures_yc$arg_xAYC <- ARG_fixtures_yc$avg_AY_arg * ARG_fixtures_yc$arg_awayyas * ARG_fixtures_yc$arg_homeyds

ARG_fixtures_yc$arg_0_0 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_0 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_0_1 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_1 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_0 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_0_2 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_2 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_1 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_2 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_3 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_0 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_1 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_2 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_0_3 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_3 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_3 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_4 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_0 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_1 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_2 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_3 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_0_4 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_4 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_4 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_4 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_5 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_0 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_1 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_2 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_3 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_4 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_0_5 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_5 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_5 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_5 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_5 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_6 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_0 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(0,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_1 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(1,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_2 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(2,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_3 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(3,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_4 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(4,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_6_5 <- round(stats::dpois(6,ARG_fixtures_yc$arg_xHYC) * stats::dpois(5,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_0_6 <- round(stats::dpois(0,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_1_6 <- round(stats::dpois(1,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_2_6 <- round(stats::dpois(2,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_3_6 <- round(stats::dpois(3,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_4_6 <- round(stats::dpois(4,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
ARG_fixtures_yc$arg_5_6 <- round(stats::dpois(5,ARG_fixtures_yc$arg_xHYC) * stats::dpois(6,ARG_fixtures_yc$arg_xAYC), digits = 4)
#Home win
ARG_fixtures_yc$arg_H <- (
  ARG_fixtures_yc$arg_1_0 + ARG_fixtures_yc$arg_2_0 + ARG_fixtures_yc$arg_2_1 + ARG_fixtures_yc$arg_3_0 + ARG_fixtures_yc$arg_3_1 +
    ARG_fixtures_yc$arg_3_2 + ARG_fixtures_yc$arg_4_0 + ARG_fixtures_yc$arg_4_1 + ARG_fixtures_yc$arg_4_2 + ARG_fixtures_yc$arg_4_3 +
    ARG_fixtures_yc$arg_5_0 + ARG_fixtures_yc$arg_5_1 + ARG_fixtures_yc$arg_5_2 + ARG_fixtures_yc$arg_5_3 + ARG_fixtures_yc$arg_5_4 +
    ARG_fixtures_yc$arg_6_0 + ARG_fixtures_yc$arg_6_1 + ARG_fixtures_yc$arg_6_2 + ARG_fixtures_yc$arg_6_3 + ARG_fixtures_yc$arg_6_4 +
    ARG_fixtures_yc$arg_6_5
)

ARG_fixtures_yc$arg_H <- percent(ARG_fixtures_yc$arg_H, accuracy = 0.1)

#Draw
ARG_fixtures_yc$arg_D <- (

  ARG_fixtures_yc$arg_0_0 + ARG_fixtures_yc$arg_1_1 + ARG_fixtures_yc$arg_2_2 + ARG_fixtures_yc$arg_3_3 + ARG_fixtures_yc$arg_4_4 +
    ARG_fixtures_yc$arg_5_5 + ARG_fixtures_yc$arg_6_6
)

ARG_fixtures_yc$arg_D <- percent(ARG_fixtures_yc$arg_D, accuracy = 0.1)

#Away

ARG_fixtures_yc$arg_A <- (
  ARG_fixtures_yc$arg_0_1 + ARG_fixtures_yc$arg_0_2 + ARG_fixtures_yc$arg_1_2 + ARG_fixtures_yc$arg_0_3 + ARG_fixtures_yc$arg_1_3 +
    ARG_fixtures_yc$arg_2_3 + ARG_fixtures_yc$arg_0_4 + ARG_fixtures_yc$arg_1_4 + ARG_fixtures_yc$arg_2_4 + ARG_fixtures_yc$arg_3_4 +
    ARG_fixtures_yc$arg_0_5 + ARG_fixtures_yc$arg_1_5 + ARG_fixtures_yc$arg_2_5 + ARG_fixtures_yc$arg_3_5 + ARG_fixtures_yc$arg_4_5 +
    ARG_fixtures_yc$arg_0_6 + ARG_fixtures_yc$arg_1_6 + ARG_fixtures_yc$arg_2_6 + ARG_fixtures_yc$arg_3_6 + ARG_fixtures_yc$arg_4_6 +
    ARG_fixtures_yc$arg_5_6
)

ARG_fixtures_yc$arg_A <- percent(ARG_fixtures_yc$arg_A, accuracy = 0.1)

#ov25
ARG_fixtures_yc$arg_ov25 <- (
  ARG_fixtures_yc$arg_2_1 + ARG_fixtures_yc$arg_1_2 + ARG_fixtures_yc$arg_2_2 + ARG_fixtures_yc$arg_3_0 + ARG_fixtures_yc$arg_3_1 +
    ARG_fixtures_yc$arg_3_2 + ARG_fixtures_yc$arg_0_3 + ARG_fixtures_yc$arg_1_3 + ARG_fixtures_yc$arg_2_3 + ARG_fixtures_yc$arg_3_3 +
    ARG_fixtures_yc$arg_4_0 + ARG_fixtures_yc$arg_4_1 + ARG_fixtures_yc$arg_4_2 + ARG_fixtures_yc$arg_4_3 + ARG_fixtures_yc$arg_0_4 +
    ARG_fixtures_yc$arg_1_4 + ARG_fixtures_yc$arg_2_4 + ARG_fixtures_yc$arg_3_4 + ARG_fixtures_yc$arg_4_4 + ARG_fixtures_yc$arg_5_0 +
    ARG_fixtures_yc$arg_5_1 + ARG_fixtures_yc$arg_5_2 + ARG_fixtures_yc$arg_5_3 + ARG_fixtures_yc$arg_5_4 + ARG_fixtures_yc$arg_0_5 +
    ARG_fixtures_yc$arg_1_5 + ARG_fixtures_yc$arg_2_5 + ARG_fixtures_yc$arg_3_5 + ARG_fixtures_yc$arg_4_5 + ARG_fixtures_yc$arg_5_5 +
    ARG_fixtures_yc$arg_6_0 + ARG_fixtures_yc$arg_6_1 + ARG_fixtures_yc$arg_6_2 + ARG_fixtures_yc$arg_6_3 + ARG_fixtures_yc$arg_6_4 +
    ARG_fixtures_yc$arg_6_5 + ARG_fixtures_yc$arg_0_6 + ARG_fixtures_yc$arg_1_6 + ARG_fixtures_yc$arg_2_6 + ARG_fixtures_yc$arg_3_6 +
    ARG_fixtures_yc$arg_4_6 + ARG_fixtures_yc$arg_5_6 + ARG_fixtures_yc$arg_6_6
)
#un25
ARG_fixtures_yc$arg_un25 <- (
  ARG_fixtures_yc$arg_0_0 + ARG_fixtures_yc$arg_1_0 + ARG_fixtures_yc$arg_0_1 + ARG_fixtures_yc$arg_1_1 + ARG_fixtures_yc$arg_2_0 + ARG_fixtures_yc$arg_0_2
)
#odds
ARG_fixtures_yc$arg_ov25_odds <- round((1/ARG_fixtures_yc$arg_ov25),digits = 2)
ARG_fixtures_yc$arg_un25_odds <- round((1/ARG_fixtures_yc$arg_un25),digits = 2)

ARG_fixtures_yc$arg_ov25_odds
ARG_fixtures_yc$arg_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
ARG_fixtures_yc$arg_ov25 <- percent(ARG_fixtures_yc$arg_ov25, accuracy = 0.1)

ARG_fixtures_yc$arg_un25 <- percent(ARG_fixtures_yc$arg_un25, accuracy = 0.1)
ARG_fixtures_yc$arg_pscore <- paste(round(ARG_fixtures_yc$arg_xHYC,digits = 0),round(ARG_fixtures_yc$arg_xAYC,digits = 0),sep = "-")
#write out
#write.xlsx(ARG_fixtures,'Divisions/ARG.xlsx',sheetName = "ARG", append = TRUE)
#################################################################################################################
#D2
# HomeTeam_d2_yc <- rep(d2_teams, each = length(d2_teams))
# AwayTeam_d2_yc <- rep(d2_teams, length(d2_teams))
# D2_fixtures_yc <- cbind(HomeTeam_d2_yc,AwayTeam_d2_yc)
# D2_fixtures_yc <- as.data.frame(D2_fixtures_yc)
# D2_fixtures_yc <- D2_fixtures_yc[!D2_fixtures_yc$HomeTeam_d2_yc == D2_fixtures_yc$AwayTeam_d2_yc,]
# rownames(D2_fixtures_yc) <- NULL
# D2_fixtures_yc$Div <- "D2"
# D2_fixtures_yc <- D2_fixtures_yc[,c(3,1,2)]
#
# D2_fixtures_yc$avg_HY_d2 <- d2_avg_HY
#
# D2_fixtures_yc$d2_homeyas <- rep(d2_home_yas,each = length(d2_teams)-1)
#
# d2_awayyds_lookup <- cbind(d2_teams,d2_away_yds)
#
# d2_awayyds_lookup <- as.data.frame(d2_awayyds_lookup)
#
# colnames(d2_awayyds_lookup) <- c("AwayTeam_d2_yc","d2_awayyds")
#
#
# require('RH2')
# D2_fixtures_yc$d2_awayyds <- sqldf("SELECT d2_awayyds_lookup.d2_awayyds FROM d2_awayyds_lookup INNER JOIN D2_fixtures_yc ON d2_awayyds_lookup.AwayTeam_d2_yc = D2_fixtures_yc.AwayTeam_d2_yc")
#
# D2_fixtures_yc$avg_AY_d2 <- d2_avg_AY
#
# d2_awayyas_lookup <- cbind(d2_teams,d2_away_yas)
#
# d2_awayyas_lookup <- as.data.frame(d2_awayyas_lookup)
#
# colnames(d2_awayyas_lookup) <- c("AwayTeam_d2_yc","d2_awayyas")
#
# D2_fixtures_yc$d2_awayyas <- sqldf("SELECT d2_awayyas_lookup.d2_awayyas FROM d2_awayyas_lookup INNER JOIN D2_fixtures_yc ON d2_awayyas_lookup.AwayTeam_d2_yc = D2_fixtures_yc.AwayTeam_d2_yc")
#
# D2_fixtures_yc$d2_homeyds <- rep(d2_home_yds,each = length(d2_teams)-1)
#
# D2_fixtures_yc$d2_awayyds <- as.numeric(unlist(D2_fixtures_yc$d2_awayyds))
# #xGH
# D2_fixtures_yc$d2_xHYC <- D2_fixtures_yc$avg_HY_d2 * D2_fixtures_yc$d2_homeyas * D2_fixtures_yc$d2_awayyds
# #xGA
#
# D2_fixtures_yc$d2_awayyas <- as.numeric(unlist(D2_fixtures_yc$d2_awayyas))
#
# D2_fixtures_yc$d2_xAYC <- D2_fixtures_yc$avg_AY_d2 * D2_fixtures_yc$d2_awayyas * D2_fixtures_yc$d2_homeyds
#
# D2_fixtures_yc$d2_0_0 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_0 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_0_1 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_1 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_0 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_0_2 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_2 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_1 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_2 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_3 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_0 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_1 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_2 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_0_3 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_3 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_3 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_4 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_0 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_1 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_2 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_3 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_0_4 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_4 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_4 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_4 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_5 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_0 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_1 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_2 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_3 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_4 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_0_5 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_5 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_5 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_5 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_5 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_6 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_0 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(0,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_1 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(1,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_2 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(2,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_3 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(3,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_4 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(4,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_6_5 <- round(stats::dpois(6,D2_fixtures_yc$d2_xHYC) * stats::dpois(5,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_0_6 <- round(stats::dpois(0,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_1_6 <- round(stats::dpois(1,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_2_6 <- round(stats::dpois(2,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_3_6 <- round(stats::dpois(3,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_4_6 <- round(stats::dpois(4,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# D2_fixtures_yc$d2_5_6 <- round(stats::dpois(5,D2_fixtures_yc$d2_xHYC) * stats::dpois(6,D2_fixtures_yc$d2_xAYC), digits = 4)
# #Home win
# D2_fixtures_yc$d2_H <- (
#   D2_fixtures_yc$d2_1_0 + D2_fixtures_yc$d2_2_0 + D2_fixtures_yc$d2_2_1 + D2_fixtures_yc$d2_3_0 + D2_fixtures_yc$d2_3_1 +
#     D2_fixtures_yc$d2_3_2 + D2_fixtures_yc$d2_4_0 + D2_fixtures_yc$d2_4_1 + D2_fixtures_yc$d2_4_2 + D2_fixtures_yc$d2_4_3 +
#     D2_fixtures_yc$d2_5_0 + D2_fixtures_yc$d2_5_1 + D2_fixtures_yc$d2_5_2 + D2_fixtures_yc$d2_5_3 + D2_fixtures_yc$d2_5_4 +
#     D2_fixtures_yc$d2_6_0 + D2_fixtures_yc$d2_6_1 + D2_fixtures_yc$d2_6_2 + D2_fixtures_yc$d2_6_3 + D2_fixtures_yc$d2_6_4 +
#     D2_fixtures_yc$d2_6_5
# )
#
# D2_fixtures_yc$d2_H <- percent(D2_fixtures_yc$d2_H, accuracy = 0.1)
#
# #Draw
# D2_fixtures_yc$d2_D <- (
#
#   D2_fixtures_yc$d2_0_0 + D2_fixtures_yc$d2_1_1 + D2_fixtures_yc$d2_2_2 + D2_fixtures_yc$d2_3_3 + D2_fixtures_yc$d2_4_4 +
#     D2_fixtures_yc$d2_5_5 + D2_fixtures_yc$d2_6_6
# )
#
# D2_fixtures_yc$d2_D <- percent(D2_fixtures_yc$d2_D, accuracy = 0.1)
#
# #Away
#
# D2_fixtures_yc$d2_A <- (
#   D2_fixtures_yc$d2_0_1 + D2_fixtures_yc$d2_0_2 + D2_fixtures_yc$d2_1_2 + D2_fixtures_yc$d2_0_3 + D2_fixtures_yc$d2_1_3 +
#     D2_fixtures_yc$d2_2_3 + D2_fixtures_yc$d2_0_4 + D2_fixtures_yc$d2_1_4 + D2_fixtures_yc$d2_2_4 + D2_fixtures_yc$d2_3_4 +
#     D2_fixtures_yc$d2_0_5 + D2_fixtures_yc$d2_1_5 + D2_fixtures_yc$d2_2_5 + D2_fixtures_yc$d2_3_5 + D2_fixtures_yc$d2_4_5 +
#     D2_fixtures_yc$d2_0_6 + D2_fixtures_yc$d2_1_6 + D2_fixtures_yc$d2_2_6 + D2_fixtures_yc$d2_3_6 + D2_fixtures_yc$d2_4_6 +
#     D2_fixtures_yc$d2_5_6
# )
#
# D2_fixtures_yc$d2_A <- percent(D2_fixtures_yc$d2_A, accuracy = 0.1)
#
# #ov25
# D2_fixtures_yc$d2_ov25 <- (
#   D2_fixtures_yc$d2_2_1 + D2_fixtures_yc$d2_1_2 + D2_fixtures_yc$d2_2_2 + D2_fixtures_yc$d2_3_0 + D2_fixtures_yc$d2_3_1 +
#     D2_fixtures_yc$d2_3_2 + D2_fixtures_yc$d2_0_3 + D2_fixtures_yc$d2_1_3 + D2_fixtures_yc$d2_2_3 + D2_fixtures_yc$d2_3_3 +
#     D2_fixtures_yc$d2_4_0 + D2_fixtures_yc$d2_4_1 + D2_fixtures_yc$d2_4_2 + D2_fixtures_yc$d2_4_3 + D2_fixtures_yc$d2_0_4 +
#     D2_fixtures_yc$d2_1_4 + D2_fixtures_yc$d2_2_4 + D2_fixtures_yc$d2_3_4 + D2_fixtures_yc$d2_4_4 + D2_fixtures_yc$d2_5_0 +
#     D2_fixtures_yc$d2_5_1 + D2_fixtures_yc$d2_5_2 + D2_fixtures_yc$d2_5_3 + D2_fixtures_yc$d2_5_4 + D2_fixtures_yc$d2_0_5 +
#     D2_fixtures_yc$d2_1_5 + D2_fixtures_yc$d2_2_5 + D2_fixtures_yc$d2_3_5 + D2_fixtures_yc$d2_4_5 + D2_fixtures_yc$d2_5_5 +
#     D2_fixtures_yc$d2_6_0 + D2_fixtures_yc$d2_6_1 + D2_fixtures_yc$d2_6_2 + D2_fixtures_yc$d2_6_3 + D2_fixtures_yc$d2_6_4 +
#     D2_fixtures_yc$d2_6_5 + D2_fixtures_yc$d2_0_6 + D2_fixtures_yc$d2_1_6 + D2_fixtures_yc$d2_2_6 + D2_fixtures_yc$d2_3_6 +
#     D2_fixtures_yc$d2_4_6 + D2_fixtures_yc$d2_5_6 + D2_fixtures_yc$d2_6_6
# )
# #un25
# D2_fixtures_yc$d2_un25 <- (
#   D2_fixtures_yc$d2_0_0 + D2_fixtures_yc$d2_1_0 + D2_fixtures_yc$d2_0_1 + D2_fixtures_yc$d2_1_1 + D2_fixtures_yc$d2_2_0 + D2_fixtures_yc$d2_0_2
# )
# #odds
# D2_fixtures_yc$d2_ov25_odds <- round((1/D2_fixtures_yc$d2_ov25),digits = 2)
# D2_fixtures_yc$d2_un25_odds <- round((1/D2_fixtures_yc$d2_un25),digits = 2)
#
# D2_fixtures_yc$d2_ov25_odds
# D2_fixtures_yc$d2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# D2_fixtures_yc$d2_ov25 <- percent(D2_fixtures_yc$d2_ov25, accuracy = 0.1)
#
# D2_fixtures_yc$d2_un25 <- percent(D2_fixtures_yc$d2_un25, accuracy = 0.1)
# D2_fixtures_yc$d2_pscore <- paste(round(D2_fixtures_yc$d2_xHYC,digits = 0),round(D2_fixtures_yc$d2_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(D2_fixtures,'Divisions/D2.xlsx',sheetName = "D2", append = TRUE)
# #################################################################################################################
# #E0
# HomeTeam_e0_yc <- rep(e0_teams, each = length(e0_teams))
# AwayTeam_e0_yc <- rep(e0_teams, length(e0_teams))
# E0_fixtures_yc <- cbind(HomeTeam_e0_yc,AwayTeam_e0_yc)
# E0_fixtures_yc <- as.data.frame(E0_fixtures_yc)
# E0_fixtures_yc <- E0_fixtures_yc[!E0_fixtures_yc$HomeTeam_e0_yc == E0_fixtures_yc$AwayTeam_e0_yc,]
# rownames(E0_fixtures_yc) <- NULL
# E0_fixtures_yc$Div <- "E0"
# E0_fixtures_yc <- E0_fixtures_yc[,c(3,1,2)]
#
# E0_fixtures_yc$avg_HY_e0 <- e0_avg_HY
#
# E0_fixtures_yc$e0_homeyas <- rep(e0_home_yas,each = length(e0_teams)-1)
#
# e0_awayyds_lookup <- cbind(e0_teams,e0_away_yds)
#
# e0_awayyds_lookup <- as.data.frame(e0_awayyds_lookup)
#
# colnames(e0_awayyds_lookup) <- c("AwayTeam_e0_yc","e0_awayyds")
#
#
# require('RH2')
# E0_fixtures_yc$e0_awayyds <- sqldf("SELECT e0_awayyds_lookup.e0_awayyds FROM e0_awayyds_lookup INNER JOIN E0_fixtures_yc ON e0_awayyds_lookup.AwayTeam_e0_yc = E0_fixtures_yc.AwayTeam_e0_yc")
#
# E0_fixtures_yc$avg_AY_e0 <- e0_avg_AY
#
# e0_awayyas_lookup <- cbind(e0_teams,e0_away_yas)
#
# e0_awayyas_lookup <- as.data.frame(e0_awayyas_lookup)
#
# colnames(e0_awayyas_lookup) <- c("AwayTeam_e0_yc","e0_awayyas")
#
# E0_fixtures_yc$e0_awayyas <- sqldf("SELECT e0_awayyas_lookup.e0_awayyas FROM e0_awayyas_lookup INNER JOIN E0_fixtures_yc ON e0_awayyas_lookup.AwayTeam_e0_yc = E0_fixtures_yc.AwayTeam_e0_yc")
#
# E0_fixtures_yc$e0_homeyds <- rep(e0_home_yds,each = length(e0_teams)-1)
#
# E0_fixtures_yc$e0_awayyds <- as.numeric(unlist(E0_fixtures_yc$e0_awayyds))
# #xGH
# E0_fixtures_yc$e0_xHYC <- E0_fixtures_yc$avg_HY_e0 * E0_fixtures_yc$e0_homeyas * E0_fixtures_yc$e0_awayyds
# #xGA
#
# E0_fixtures_yc$e0_awayyas <- as.numeric(unlist(E0_fixtures_yc$e0_awayyas))
#
# E0_fixtures_yc$e0_xAYC <- E0_fixtures_yc$avg_AY_e0 * E0_fixtures_yc$e0_awayyas * E0_fixtures_yc$e0_homeyds
#
# E0_fixtures_yc$e0_0_0 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_0 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_0_1 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_1 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_0 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_0_2 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_2 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_1 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_2 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_3 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_0 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_1 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_2 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_0_3 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_3 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_3 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_4 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_0 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_1 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_2 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_3 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_0_4 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_4 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_4 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_4 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_5 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_0 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_1 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_2 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_3 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_4 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_0_5 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_5 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_5 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_5 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_5 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_6 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_0 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(0,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_1 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(1,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_2 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(2,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_3 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(3,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_4 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(4,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_6_5 <- round(stats::dpois(6,E0_fixtures_yc$e0_xHYC) * stats::dpois(5,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_0_6 <- round(stats::dpois(0,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_1_6 <- round(stats::dpois(1,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_2_6 <- round(stats::dpois(2,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_3_6 <- round(stats::dpois(3,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_4_6 <- round(stats::dpois(4,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# E0_fixtures_yc$e0_5_6 <- round(stats::dpois(5,E0_fixtures_yc$e0_xHYC) * stats::dpois(6,E0_fixtures_yc$e0_xAYC), digits = 4)
# #Home win
# E0_fixtures_yc$e0_H <- (
#   E0_fixtures_yc$e0_1_0 + E0_fixtures_yc$e0_2_0 + E0_fixtures_yc$e0_2_1 + E0_fixtures_yc$e0_3_0 + E0_fixtures_yc$e0_3_1 +
#     E0_fixtures_yc$e0_3_2 + E0_fixtures_yc$e0_4_0 + E0_fixtures_yc$e0_4_1 + E0_fixtures_yc$e0_4_2 + E0_fixtures_yc$e0_4_3 +
#     E0_fixtures_yc$e0_5_0 + E0_fixtures_yc$e0_5_1 + E0_fixtures_yc$e0_5_2 + E0_fixtures_yc$e0_5_3 + E0_fixtures_yc$e0_5_4 +
#     E0_fixtures_yc$e0_6_0 + E0_fixtures_yc$e0_6_1 + E0_fixtures_yc$e0_6_2 + E0_fixtures_yc$e0_6_3 + E0_fixtures_yc$e0_6_4 +
#     E0_fixtures_yc$e0_6_5
# )
#
# E0_fixtures_yc$e0_H <- percent(E0_fixtures_yc$e0_H, accuracy = 0.1)
#
# #Draw
# E0_fixtures_yc$e0_D <- (
#
#   E0_fixtures_yc$e0_0_0 + E0_fixtures_yc$e0_1_1 + E0_fixtures_yc$e0_2_2 + E0_fixtures_yc$e0_3_3 + E0_fixtures_yc$e0_4_4 +
#     E0_fixtures_yc$e0_5_5 + E0_fixtures_yc$e0_6_6
# )
#
# E0_fixtures_yc$e0_D <- percent(E0_fixtures_yc$e0_D, accuracy = 0.1)
#
# #Away
#
# E0_fixtures_yc$e0_A <- (
#   E0_fixtures_yc$e0_0_1 + E0_fixtures_yc$e0_0_2 + E0_fixtures_yc$e0_1_2 + E0_fixtures_yc$e0_0_3 + E0_fixtures_yc$e0_1_3 +
#     E0_fixtures_yc$e0_2_3 + E0_fixtures_yc$e0_0_4 + E0_fixtures_yc$e0_1_4 + E0_fixtures_yc$e0_2_4 + E0_fixtures_yc$e0_3_4 +
#     E0_fixtures_yc$e0_0_5 + E0_fixtures_yc$e0_1_5 + E0_fixtures_yc$e0_2_5 + E0_fixtures_yc$e0_3_5 + E0_fixtures_yc$e0_4_5 +
#     E0_fixtures_yc$e0_0_6 + E0_fixtures_yc$e0_1_6 + E0_fixtures_yc$e0_2_6 + E0_fixtures_yc$e0_3_6 + E0_fixtures_yc$e0_4_6 +
#     E0_fixtures_yc$e0_5_6
# )
#
# E0_fixtures_yc$e0_A <- percent(E0_fixtures_yc$e0_A, accuracy = 0.1)
#
# #ov25
# E0_fixtures_yc$e0_ov25 <- (
#   E0_fixtures_yc$e0_2_1 + E0_fixtures_yc$e0_1_2 + E0_fixtures_yc$e0_2_2 + E0_fixtures_yc$e0_3_0 + E0_fixtures_yc$e0_3_1 +
#     E0_fixtures_yc$e0_3_2 + E0_fixtures_yc$e0_0_3 + E0_fixtures_yc$e0_1_3 + E0_fixtures_yc$e0_2_3 + E0_fixtures_yc$e0_3_3 +
#     E0_fixtures_yc$e0_4_0 + E0_fixtures_yc$e0_4_1 + E0_fixtures_yc$e0_4_2 + E0_fixtures_yc$e0_4_3 + E0_fixtures_yc$e0_0_4 +
#     E0_fixtures_yc$e0_1_4 + E0_fixtures_yc$e0_2_4 + E0_fixtures_yc$e0_3_4 + E0_fixtures_yc$e0_4_4 + E0_fixtures_yc$e0_5_0 +
#     E0_fixtures_yc$e0_5_1 + E0_fixtures_yc$e0_5_2 + E0_fixtures_yc$e0_5_3 + E0_fixtures_yc$e0_5_4 + E0_fixtures_yc$e0_0_5 +
#     E0_fixtures_yc$e0_1_5 + E0_fixtures_yc$e0_2_5 + E0_fixtures_yc$e0_3_5 + E0_fixtures_yc$e0_4_5 + E0_fixtures_yc$e0_5_5 +
#     E0_fixtures_yc$e0_6_0 + E0_fixtures_yc$e0_6_1 + E0_fixtures_yc$e0_6_2 + E0_fixtures_yc$e0_6_3 + E0_fixtures_yc$e0_6_4 +
#     E0_fixtures_yc$e0_6_5 + E0_fixtures_yc$e0_0_6 + E0_fixtures_yc$e0_1_6 + E0_fixtures_yc$e0_2_6 + E0_fixtures_yc$e0_3_6 +
#     E0_fixtures_yc$e0_4_6 + E0_fixtures_yc$e0_5_6 + E0_fixtures_yc$e0_6_6
# )
# #un25
# E0_fixtures_yc$e0_un25 <- (
#   E0_fixtures_yc$e0_0_0 + E0_fixtures_yc$e0_1_0 + E0_fixtures_yc$e0_0_1 + E0_fixtures_yc$e0_1_1 + E0_fixtures_yc$e0_2_0 + E0_fixtures_yc$e0_0_2
# )
# #odds
# E0_fixtures_yc$e0_ov25_odds <- round((1/E0_fixtures_yc$e0_ov25),digits = 2)
# E0_fixtures_yc$e0_un25_odds <- round((1/E0_fixtures_yc$e0_un25),digits = 2)
#
# E0_fixtures_yc$e0_ov25_odds
# E0_fixtures_yc$e0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E0_fixtures_yc$e0_ov25 <- percent(E0_fixtures_yc$e0_ov25, accuracy = 0.1)
#
# E0_fixtures_yc$e0_un25 <- percent(E0_fixtures_yc$e0_un25, accuracy = 0.1)
# E0_fixtures_yc$e0_pscore <- paste(round(E0_fixtures_yc$e0_xHYC,digits = 0),round(E0_fixtures_yc$e0_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #E1
# HomeTeam_e1_yc <- rep(e1_teams, each = length(e1_teams))
# AwayTeam_e1_yc <- rep(e1_teams, length(e1_teams))
# E1_fixtures_yc <- cbind(HomeTeam_e1_yc,AwayTeam_e1_yc)
# E1_fixtures_yc <- as.data.frame(E1_fixtures_yc)
# E1_fixtures_yc <- E1_fixtures_yc[!E1_fixtures_yc$HomeTeam_e1_yc == E1_fixtures_yc$AwayTeam_e1_yc,]
# rownames(E1_fixtures_yc) <- NULL
# E1_fixtures_yc$Div <- "E1"
# E1_fixtures_yc <- E1_fixtures_yc[,c(3,1,2)]
#
# E1_fixtures_yc$avg_HY_e1 <- e1_avg_HY
#
# E1_fixtures_yc$e1_homeyas <- rep(e1_home_yas,each = length(e1_teams)-1)
#
# e1_awayyds_lookup <- cbind(e1_teams,e1_away_yds)
#
# e1_awayyds_lookup <- as.data.frame(e1_awayyds_lookup)
#
# colnames(e1_awayyds_lookup) <- c("AwayTeam_e1_yc","e1_awayyds")
#
#
# require('RH2')
# E1_fixtures_yc$e1_awayyds <- sqldf("SELECT e1_awayyds_lookup.e1_awayyds FROM e1_awayyds_lookup INNER JOIN E1_fixtures_yc ON e1_awayyds_lookup.AwayTeam_e1_yc = E1_fixtures_yc.AwayTeam_e1_yc")
#
# E1_fixtures_yc$avg_AY_e1 <- e1_avg_AY
#
# e1_awayyas_lookup <- cbind(e1_teams,e1_away_yas)
#
# e1_awayyas_lookup <- as.data.frame(e1_awayyas_lookup)
#
# colnames(e1_awayyas_lookup) <- c("AwayTeam_e1_yc","e1_awayyas")
#
# E1_fixtures_yc$e1_awayyas <- sqldf("SELECT e1_awayyas_lookup.e1_awayyas FROM e1_awayyas_lookup INNER JOIN E1_fixtures_yc ON e1_awayyas_lookup.AwayTeam_e1_yc = E1_fixtures_yc.AwayTeam_e1_yc")
#
# E1_fixtures_yc$e1_homeyds <- rep(e1_home_yds,each = length(e1_teams)-1)
#
# E1_fixtures_yc$e1_awayyds <- as.numeric(unlist(E1_fixtures_yc$e1_awayyds))
# #xGH
# E1_fixtures_yc$e1_xHYC <- E1_fixtures_yc$avg_HY_e1 * E1_fixtures_yc$e1_homeyas * E1_fixtures_yc$e1_awayyds
# #xGA
#
# E1_fixtures_yc$e1_awayyas <- as.numeric(unlist(E1_fixtures_yc$e1_awayyas))
#
# E1_fixtures_yc$e1_xAYC <- E1_fixtures_yc$avg_AY_e1 * E1_fixtures_yc$e1_awayyas * E1_fixtures_yc$e1_homeyds
#
# E1_fixtures_yc$e1_0_0 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_0 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_0_1 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_1 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_0 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_0_2 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_2 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_1 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_2 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_3 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_0 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_1 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_2 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_0_3 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_3 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_3 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_4 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_0 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_1 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_2 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_3 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_0_4 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_4 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_4 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_4 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_5 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_0 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_1 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_2 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_3 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_4 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_0_5 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_5 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_5 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_5 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_5 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_6 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_0 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(0,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_1 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(1,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_2 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(2,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_3 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(3,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_4 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(4,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_6_5 <- round(stats::dpois(6,E1_fixtures_yc$e1_xHYC) * stats::dpois(5,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_0_6 <- round(stats::dpois(0,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_1_6 <- round(stats::dpois(1,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_2_6 <- round(stats::dpois(2,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_3_6 <- round(stats::dpois(3,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_4_6 <- round(stats::dpois(4,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# E1_fixtures_yc$e1_5_6 <- round(stats::dpois(5,E1_fixtures_yc$e1_xHYC) * stats::dpois(6,E1_fixtures_yc$e1_xAYC), digits = 4)
# #Home win
# E1_fixtures_yc$e1_H <- (
#   E1_fixtures_yc$e1_1_0 + E1_fixtures_yc$e1_2_0 + E1_fixtures_yc$e1_2_1 + E1_fixtures_yc$e1_3_0 + E1_fixtures_yc$e1_3_1 +
#     E1_fixtures_yc$e1_3_2 + E1_fixtures_yc$e1_4_0 + E1_fixtures_yc$e1_4_1 + E1_fixtures_yc$e1_4_2 + E1_fixtures_yc$e1_4_3 +
#     E1_fixtures_yc$e1_5_0 + E1_fixtures_yc$e1_5_1 + E1_fixtures_yc$e1_5_2 + E1_fixtures_yc$e1_5_3 + E1_fixtures_yc$e1_5_4 +
#     E1_fixtures_yc$e1_6_0 + E1_fixtures_yc$e1_6_1 + E1_fixtures_yc$e1_6_2 + E1_fixtures_yc$e1_6_3 + E1_fixtures_yc$e1_6_4 +
#     E1_fixtures_yc$e1_6_5
# )
#
# E1_fixtures_yc$e1_H <- percent(E1_fixtures_yc$e1_H, accuracy = 0.1)
#
# #Draw
# E1_fixtures_yc$e1_D <- (
#
#   E1_fixtures_yc$e1_0_0 + E1_fixtures_yc$e1_1_1 + E1_fixtures_yc$e1_2_2 + E1_fixtures_yc$e1_3_3 + E1_fixtures_yc$e1_4_4 +
#     E1_fixtures_yc$e1_5_5 + E1_fixtures_yc$e1_6_6
# )
#
# E1_fixtures_yc$e1_D <- percent(E1_fixtures_yc$e1_D, accuracy = 0.1)
#
# #Away
#
# E1_fixtures_yc$e1_A <- (
#   E1_fixtures_yc$e1_0_1 + E1_fixtures_yc$e1_0_2 + E1_fixtures_yc$e1_1_2 + E1_fixtures_yc$e1_0_3 + E1_fixtures_yc$e1_1_3 +
#     E1_fixtures_yc$e1_2_3 + E1_fixtures_yc$e1_0_4 + E1_fixtures_yc$e1_1_4 + E1_fixtures_yc$e1_2_4 + E1_fixtures_yc$e1_3_4 +
#     E1_fixtures_yc$e1_0_5 + E1_fixtures_yc$e1_1_5 + E1_fixtures_yc$e1_2_5 + E1_fixtures_yc$e1_3_5 + E1_fixtures_yc$e1_4_5 +
#     E1_fixtures_yc$e1_0_6 + E1_fixtures_yc$e1_1_6 + E1_fixtures_yc$e1_2_6 + E1_fixtures_yc$e1_3_6 + E1_fixtures_yc$e1_4_6 +
#     E1_fixtures_yc$e1_5_6
# )
#
# E1_fixtures_yc$e1_A <- percent(E1_fixtures_yc$e1_A, accuracy = 0.1)
#
# #ov25
# E1_fixtures_yc$e1_ov25 <- (
#   E1_fixtures_yc$e1_2_1 + E1_fixtures_yc$e1_1_2 + E1_fixtures_yc$e1_2_2 + E1_fixtures_yc$e1_3_0 + E1_fixtures_yc$e1_3_1 +
#     E1_fixtures_yc$e1_3_2 + E1_fixtures_yc$e1_0_3 + E1_fixtures_yc$e1_1_3 + E1_fixtures_yc$e1_2_3 + E1_fixtures_yc$e1_3_3 +
#     E1_fixtures_yc$e1_4_0 + E1_fixtures_yc$e1_4_1 + E1_fixtures_yc$e1_4_2 + E1_fixtures_yc$e1_4_3 + E1_fixtures_yc$e1_0_4 +
#     E1_fixtures_yc$e1_1_4 + E1_fixtures_yc$e1_2_4 + E1_fixtures_yc$e1_3_4 + E1_fixtures_yc$e1_4_4 + E1_fixtures_yc$e1_5_0 +
#     E1_fixtures_yc$e1_5_1 + E1_fixtures_yc$e1_5_2 + E1_fixtures_yc$e1_5_3 + E1_fixtures_yc$e1_5_4 + E1_fixtures_yc$e1_0_5 +
#     E1_fixtures_yc$e1_1_5 + E1_fixtures_yc$e1_2_5 + E1_fixtures_yc$e1_3_5 + E1_fixtures_yc$e1_4_5 + E1_fixtures_yc$e1_5_5 +
#     E1_fixtures_yc$e1_6_0 + E1_fixtures_yc$e1_6_1 + E1_fixtures_yc$e1_6_2 + E1_fixtures_yc$e1_6_3 + E1_fixtures_yc$e1_6_4 +
#     E1_fixtures_yc$e1_6_5 + E1_fixtures_yc$e1_0_6 + E1_fixtures_yc$e1_1_6 + E1_fixtures_yc$e1_2_6 + E1_fixtures_yc$e1_3_6 +
#     E1_fixtures_yc$e1_4_6 + E1_fixtures_yc$e1_5_6 + E1_fixtures_yc$e1_6_6
# )
# #un25
# E1_fixtures_yc$e1_un25 <- (
#   E1_fixtures_yc$e1_0_0 + E1_fixtures_yc$e1_1_0 + E1_fixtures_yc$e1_0_1 + E1_fixtures_yc$e1_1_1 + E1_fixtures_yc$e1_2_0 + E1_fixtures_yc$e1_0_2
# )
# #odds
# E1_fixtures_yc$e1_ov25_odds <- round((1/E1_fixtures_yc$e1_ov25),digits = 2)
# E1_fixtures_yc$e1_un25_odds <- round((1/E1_fixtures_yc$e1_un25),digits = 2)
#
# E1_fixtures_yc$e1_ov25_odds
# E1_fixtures_yc$e1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E1_fixtures_yc$e1_ov25 <- percent(E1_fixtures_yc$e1_ov25, accuracy = 0.1)
#
# E1_fixtures_yc$e1_un25 <- percent(E1_fixtures_yc$e1_un25, accuracy = 0.1)
# E1_fixtures_yc$e1_pscore <- paste(round(E1_fixtures_yc$e1_xHYC,digits = 0),round(E1_fixtures_yc$e1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(E1_fixtures,'Divisions/E1.xlsx',sheetName = "E1", append = TRUE)
# #################################################################################################################
# #E2
# HomeTeam_e2_yc <- rep(e2_teams, each = length(e2_teams))
# AwayTeam_e2_yc <- rep(e2_teams, length(e2_teams))
# E2_fixtures_yc <- cbind(HomeTeam_e2_yc,AwayTeam_e2_yc)
# E2_fixtures_yc <- as.data.frame(E2_fixtures_yc)
# E2_fixtures_yc <- E2_fixtures_yc[!E2_fixtures_yc$HomeTeam_e2_yc == E2_fixtures_yc$AwayTeam_e2_yc,]
# rownames(E2_fixtures_yc) <- NULL
# E2_fixtures_yc$Div <- "E2"
# E2_fixtures_yc <- E2_fixtures_yc[,c(3,1,2)]
#
# E2_fixtures_yc$avg_HY_e2 <- e2_avg_HY
#
# E2_fixtures_yc$e2_homeyas <- rep(e2_home_yas,each = length(e2_teams)-1)
#
# e2_awayyds_lookup <- cbind(e2_teams,e2_away_yds)
#
# e2_awayyds_lookup <- as.data.frame(e2_awayyds_lookup)
#
# colnames(e2_awayyds_lookup) <- c("AwayTeam_e2_yc","e2_awayyds")
#
#
# require('RH2')
# E2_fixtures_yc$e2_awayyds <- sqldf("SELECT e2_awayyds_lookup.e2_awayyds FROM e2_awayyds_lookup INNER JOIN E2_fixtures_yc ON e2_awayyds_lookup.AwayTeam_e2_yc = E2_fixtures_yc.AwayTeam_e2_yc")
#
# E2_fixtures_yc$avg_AY_e2 <- e2_avg_AY
#
# e2_awayyas_lookup <- cbind(e2_teams,e2_away_yas)
#
# e2_awayyas_lookup <- as.data.frame(e2_awayyas_lookup)
#
# colnames(e2_awayyas_lookup) <- c("AwayTeam_e2_yc","e2_awayyas")
#
# E2_fixtures_yc$e2_awayyas <- sqldf("SELECT e2_awayyas_lookup.e2_awayyas FROM e2_awayyas_lookup INNER JOIN E2_fixtures_yc ON e2_awayyas_lookup.AwayTeam_e2_yc = E2_fixtures_yc.AwayTeam_e2_yc")
#
# E2_fixtures_yc$e2_homeyds <- rep(e2_home_yds,each = length(e2_teams)-1)
#
# E2_fixtures_yc$e2_awayyds <- as.numeric(unlist(E2_fixtures_yc$e2_awayyds))
# #xGH
# E2_fixtures_yc$e2_xHYC <- E2_fixtures_yc$avg_HY_e2 * E2_fixtures_yc$e2_homeyas * E2_fixtures_yc$e2_awayyds
# #xGA
#
# E2_fixtures_yc$e2_awayyas <- as.numeric(unlist(E2_fixtures_yc$e2_awayyas))
#
# E2_fixtures_yc$e2_xAYC <- E2_fixtures_yc$avg_AY_e2 * E2_fixtures_yc$e2_awayyas * E2_fixtures_yc$e2_homeyds
#
# E2_fixtures_yc$e2_0_0 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_0 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_0_1 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_1 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_0 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_0_2 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_2 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_1 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_2 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_3 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_0 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_1 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_2 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_0_3 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_3 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_3 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_4 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_0 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_1 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_2 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_3 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_0_4 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_4 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_4 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_4 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_5 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_0 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_1 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_2 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_3 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_4 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_0_5 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_5 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_5 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_5 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_5 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_6 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_0 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(0,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_1 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(1,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_2 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(2,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_3 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(3,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_4 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(4,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_6_5 <- round(stats::dpois(6,E2_fixtures_yc$e2_xHYC) * stats::dpois(5,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_0_6 <- round(stats::dpois(0,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_1_6 <- round(stats::dpois(1,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_2_6 <- round(stats::dpois(2,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_3_6 <- round(stats::dpois(3,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_4_6 <- round(stats::dpois(4,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# E2_fixtures_yc$e2_5_6 <- round(stats::dpois(5,E2_fixtures_yc$e2_xHYC) * stats::dpois(6,E2_fixtures_yc$e2_xAYC), digits = 4)
# #Home win
# E2_fixtures_yc$e2_H <- (
#   E2_fixtures_yc$e2_1_0 + E2_fixtures_yc$e2_2_0 + E2_fixtures_yc$e2_2_1 + E2_fixtures_yc$e2_3_0 + E2_fixtures_yc$e2_3_1 +
#     E2_fixtures_yc$e2_3_2 + E2_fixtures_yc$e2_4_0 + E2_fixtures_yc$e2_4_1 + E2_fixtures_yc$e2_4_2 + E2_fixtures_yc$e2_4_3 +
#     E2_fixtures_yc$e2_5_0 + E2_fixtures_yc$e2_5_1 + E2_fixtures_yc$e2_5_2 + E2_fixtures_yc$e2_5_3 + E2_fixtures_yc$e2_5_4 +
#     E2_fixtures_yc$e2_6_0 + E2_fixtures_yc$e2_6_1 + E2_fixtures_yc$e2_6_2 + E2_fixtures_yc$e2_6_3 + E2_fixtures_yc$e2_6_4 +
#     E2_fixtures_yc$e2_6_5
# )
#
# E2_fixtures_yc$e2_H <- percent(E2_fixtures_yc$e2_H, accuracy = 0.1)
#
# #Draw
# E2_fixtures_yc$e2_D <- (
#
#   E2_fixtures_yc$e2_0_0 + E2_fixtures_yc$e2_1_1 + E2_fixtures_yc$e2_2_2 + E2_fixtures_yc$e2_3_3 + E2_fixtures_yc$e2_4_4 +
#     E2_fixtures_yc$e2_5_5 + E2_fixtures_yc$e2_6_6
# )
#
# E2_fixtures_yc$e2_D <- percent(E2_fixtures_yc$e2_D, accuracy = 0.1)
#
# #Away
#
# E2_fixtures_yc$e2_A <- (
#   E2_fixtures_yc$e2_0_1 + E2_fixtures_yc$e2_0_2 + E2_fixtures_yc$e2_1_2 + E2_fixtures_yc$e2_0_3 + E2_fixtures_yc$e2_1_3 +
#     E2_fixtures_yc$e2_2_3 + E2_fixtures_yc$e2_0_4 + E2_fixtures_yc$e2_1_4 + E2_fixtures_yc$e2_2_4 + E2_fixtures_yc$e2_3_4 +
#     E2_fixtures_yc$e2_0_5 + E2_fixtures_yc$e2_1_5 + E2_fixtures_yc$e2_2_5 + E2_fixtures_yc$e2_3_5 + E2_fixtures_yc$e2_4_5 +
#     E2_fixtures_yc$e2_0_6 + E2_fixtures_yc$e2_1_6 + E2_fixtures_yc$e2_2_6 + E2_fixtures_yc$e2_3_6 + E2_fixtures_yc$e2_4_6 +
#     E2_fixtures_yc$e2_5_6
# )
#
# E2_fixtures_yc$e2_A <- percent(E2_fixtures_yc$e2_A, accuracy = 0.1)
#
# #ov25
# E2_fixtures_yc$e2_ov25 <- (
#   E2_fixtures_yc$e2_2_1 + E2_fixtures_yc$e2_1_2 + E2_fixtures_yc$e2_2_2 + E2_fixtures_yc$e2_3_0 + E2_fixtures_yc$e2_3_1 +
#     E2_fixtures_yc$e2_3_2 + E2_fixtures_yc$e2_0_3 + E2_fixtures_yc$e2_1_3 + E2_fixtures_yc$e2_2_3 + E2_fixtures_yc$e2_3_3 +
#     E2_fixtures_yc$e2_4_0 + E2_fixtures_yc$e2_4_1 + E2_fixtures_yc$e2_4_2 + E2_fixtures_yc$e2_4_3 + E2_fixtures_yc$e2_0_4 +
#     E2_fixtures_yc$e2_1_4 + E2_fixtures_yc$e2_2_4 + E2_fixtures_yc$e2_3_4 + E2_fixtures_yc$e2_4_4 + E2_fixtures_yc$e2_5_0 +
#     E2_fixtures_yc$e2_5_1 + E2_fixtures_yc$e2_5_2 + E2_fixtures_yc$e2_5_3 + E2_fixtures_yc$e2_5_4 + E2_fixtures_yc$e2_0_5 +
#     E2_fixtures_yc$e2_1_5 + E2_fixtures_yc$e2_2_5 + E2_fixtures_yc$e2_3_5 + E2_fixtures_yc$e2_4_5 + E2_fixtures_yc$e2_5_5 +
#     E2_fixtures_yc$e2_6_0 + E2_fixtures_yc$e2_6_1 + E2_fixtures_yc$e2_6_2 + E2_fixtures_yc$e2_6_3 + E2_fixtures_yc$e2_6_4 +
#     E2_fixtures_yc$e2_6_5 + E2_fixtures_yc$e2_0_6 + E2_fixtures_yc$e2_1_6 + E2_fixtures_yc$e2_2_6 + E2_fixtures_yc$e2_3_6 +
#     E2_fixtures_yc$e2_4_6 + E2_fixtures_yc$e2_5_6 + E2_fixtures_yc$e2_6_6
# )
# #un25
# E2_fixtures_yc$e2_un25 <- (
#   E2_fixtures_yc$e2_0_0 + E2_fixtures_yc$e2_1_0 + E2_fixtures_yc$e2_0_1 + E2_fixtures_yc$e2_1_1 + E2_fixtures_yc$e2_2_0 + E2_fixtures_yc$e2_0_2
# )
# #odds
# E2_fixtures_yc$e2_ov25_odds <- round((1/E2_fixtures_yc$e2_ov25),digits = 2)
# E2_fixtures_yc$e2_un25_odds <- round((1/E2_fixtures_yc$e2_un25),digits = 2)
#
# E2_fixtures_yc$e2_ov25_odds
# E2_fixtures_yc$e2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E2_fixtures_yc$e2_ov25 <- percent(E2_fixtures_yc$e2_ov25, accuracy = 0.1)
#
# E2_fixtures_yc$e2_un25 <- percent(E2_fixtures_yc$e2_un25, accuracy = 0.1)
# E2_fixtures_yc$e2_pscore <- paste(round(E2_fixtures_yc$e2_xHYC,digits = 0),round(E2_fixtures_yc$e2_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(E2_fixtures,'Divisions/E2.xlsx',sheetName = "E2", append = TRUE)
# #################################################################################################################
# #E3
# HomeTeam_e3_yc <- rep(e3_teams, each = length(e3_teams))
# AwayTeam_e3_yc <- rep(e3_teams, length(e3_teams))
# E3_fixtures_yc <- cbind(HomeTeam_e3_yc,AwayTeam_e3_yc)
# E3_fixtures_yc <- as.data.frame(E3_fixtures_yc)
# E3_fixtures_yc <- E3_fixtures_yc[!E3_fixtures_yc$HomeTeam_e3_yc == E3_fixtures_yc$AwayTeam_e3_yc,]
# rownames(E3_fixtures_yc) <- NULL
# E3_fixtures_yc$Div <- "E3"
# E3_fixtures_yc <- E3_fixtures_yc[,c(3,1,2)]
#
# E3_fixtures_yc$avg_HY_e3 <- e3_avg_HY
#
# E3_fixtures_yc$e3_homeyas <- rep(e3_home_yas,each = length(e3_teams)-1)
#
# e3_awayyds_lookup <- cbind(e3_teams,e3_away_yds)
#
# e3_awayyds_lookup <- as.data.frame(e3_awayyds_lookup)
#
# colnames(e3_awayyds_lookup) <- c("AwayTeam_e3_yc","e3_awayyds")
#
#
# require('RH2')
# E3_fixtures_yc$e3_awayyds <- sqldf("SELECT e3_awayyds_lookup.e3_awayyds FROM e3_awayyds_lookup INNER JOIN E3_fixtures_yc ON e3_awayyds_lookup.AwayTeam_e3_yc = E3_fixtures_yc.AwayTeam_e3_yc")
#
# E3_fixtures_yc$avg_AY_e3 <- e3_avg_AY
#
# e3_awayyas_lookup <- cbind(e3_teams,e3_away_yas)
#
# e3_awayyas_lookup <- as.data.frame(e3_awayyas_lookup)
#
# colnames(e3_awayyas_lookup) <- c("AwayTeam_e3_yc","e3_awayyas")
#
# E3_fixtures_yc$e3_awayyas <- sqldf("SELECT e3_awayyas_lookup.e3_awayyas FROM e3_awayyas_lookup INNER JOIN E3_fixtures_yc ON e3_awayyas_lookup.AwayTeam_e3_yc = E3_fixtures_yc.AwayTeam_e3_yc")
#
# E3_fixtures_yc$e3_homeyds <- rep(e3_home_yds,each = length(e3_teams)-1)
#
# E3_fixtures_yc$e3_awayyds <- as.numeric(unlist(E3_fixtures_yc$e3_awayyds))
# #xGH
# E3_fixtures_yc$e3_xHYC <- E3_fixtures_yc$avg_HY_e3 * E3_fixtures_yc$e3_homeyas * E3_fixtures_yc$e3_awayyds
# #xGA
#
# E3_fixtures_yc$e3_awayyas <- as.numeric(unlist(E3_fixtures_yc$e3_awayyas))
#
# E3_fixtures_yc$e3_xAYC <- E3_fixtures_yc$avg_AY_e3 * E3_fixtures_yc$e3_awayyas * E3_fixtures_yc$e3_homeyds
#
# E3_fixtures_yc$e3_0_0 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_0 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_0_1 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_1 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_0 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_0_2 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_2 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_1 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_2 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_3 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_0 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_1 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_2 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_0_3 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_3 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_3 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_4 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_0 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_1 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_2 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_3 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_0_4 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_4 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_4 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_4 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_5 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_0 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_1 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_2 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_3 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_4 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_0_5 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_5 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_5 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_5 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_5 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_6 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_0 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(0,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_1 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(1,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_2 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(2,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_3 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(3,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_4 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(4,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_6_5 <- round(stats::dpois(6,E3_fixtures_yc$e3_xHYC) * stats::dpois(5,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_0_6 <- round(stats::dpois(0,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_1_6 <- round(stats::dpois(1,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_2_6 <- round(stats::dpois(2,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_3_6 <- round(stats::dpois(3,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_4_6 <- round(stats::dpois(4,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# E3_fixtures_yc$e3_5_6 <- round(stats::dpois(5,E3_fixtures_yc$e3_xHYC) * stats::dpois(6,E3_fixtures_yc$e3_xAYC), digits = 4)
# #Home win
# E3_fixtures_yc$e3_H <- (
#   E3_fixtures_yc$e3_1_0 + E3_fixtures_yc$e3_2_0 + E3_fixtures_yc$e3_2_1 + E3_fixtures_yc$e3_3_0 + E3_fixtures_yc$e3_3_1 +
#     E3_fixtures_yc$e3_3_2 + E3_fixtures_yc$e3_4_0 + E3_fixtures_yc$e3_4_1 + E3_fixtures_yc$e3_4_2 + E3_fixtures_yc$e3_4_3 +
#     E3_fixtures_yc$e3_5_0 + E3_fixtures_yc$e3_5_1 + E3_fixtures_yc$e3_5_2 + E3_fixtures_yc$e3_5_3 + E3_fixtures_yc$e3_5_4 +
#     E3_fixtures_yc$e3_6_0 + E3_fixtures_yc$e3_6_1 + E3_fixtures_yc$e3_6_2 + E3_fixtures_yc$e3_6_3 + E3_fixtures_yc$e3_6_4 +
#     E3_fixtures_yc$e3_6_5
# )
#
# E3_fixtures_yc$e3_H <- percent(E3_fixtures_yc$e3_H, accuracy = 0.1)
#
# #Draw
# E3_fixtures_yc$e3_D <- (
#
#   E3_fixtures_yc$e3_0_0 + E3_fixtures_yc$e3_1_1 + E3_fixtures_yc$e3_2_2 + E3_fixtures_yc$e3_3_3 + E3_fixtures_yc$e3_4_4 +
#     E3_fixtures_yc$e3_5_5 + E3_fixtures_yc$e3_6_6
# )
#
# E3_fixtures_yc$e3_D <- percent(E3_fixtures_yc$e3_D, accuracy = 0.1)
#
# #Away
#
# E3_fixtures_yc$e3_A <- (
#   E3_fixtures_yc$e3_0_1 + E3_fixtures_yc$e3_0_2 + E3_fixtures_yc$e3_1_2 + E3_fixtures_yc$e3_0_3 + E3_fixtures_yc$e3_1_3 +
#     E3_fixtures_yc$e3_2_3 + E3_fixtures_yc$e3_0_4 + E3_fixtures_yc$e3_1_4 + E3_fixtures_yc$e3_2_4 + E3_fixtures_yc$e3_3_4 +
#     E3_fixtures_yc$e3_0_5 + E3_fixtures_yc$e3_1_5 + E3_fixtures_yc$e3_2_5 + E3_fixtures_yc$e3_3_5 + E3_fixtures_yc$e3_4_5 +
#     E3_fixtures_yc$e3_0_6 + E3_fixtures_yc$e3_1_6 + E3_fixtures_yc$e3_2_6 + E3_fixtures_yc$e3_3_6 + E3_fixtures_yc$e3_4_6 +
#     E3_fixtures_yc$e3_5_6
# )
#
# E3_fixtures_yc$e3_A <- percent(E3_fixtures_yc$e3_A, accuracy = 0.1)
#
# #ov25
# E3_fixtures_yc$e3_ov25 <- (
#   E3_fixtures_yc$e3_2_1 + E3_fixtures_yc$e3_1_2 + E3_fixtures_yc$e3_2_2 + E3_fixtures_yc$e3_3_0 + E3_fixtures_yc$e3_3_1 +
#     E3_fixtures_yc$e3_3_2 + E3_fixtures_yc$e3_0_3 + E3_fixtures_yc$e3_1_3 + E3_fixtures_yc$e3_2_3 + E3_fixtures_yc$e3_3_3 +
#     E3_fixtures_yc$e3_4_0 + E3_fixtures_yc$e3_4_1 + E3_fixtures_yc$e3_4_2 + E3_fixtures_yc$e3_4_3 + E3_fixtures_yc$e3_0_4 +
#     E3_fixtures_yc$e3_1_4 + E3_fixtures_yc$e3_2_4 + E3_fixtures_yc$e3_3_4 + E3_fixtures_yc$e3_4_4 + E3_fixtures_yc$e3_5_0 +
#     E3_fixtures_yc$e3_5_1 + E3_fixtures_yc$e3_5_2 + E3_fixtures_yc$e3_5_3 + E3_fixtures_yc$e3_5_4 + E3_fixtures_yc$e3_0_5 +
#     E3_fixtures_yc$e3_1_5 + E3_fixtures_yc$e3_2_5 + E3_fixtures_yc$e3_3_5 + E3_fixtures_yc$e3_4_5 + E3_fixtures_yc$e3_5_5 +
#     E3_fixtures_yc$e3_6_0 + E3_fixtures_yc$e3_6_1 + E3_fixtures_yc$e3_6_2 + E3_fixtures_yc$e3_6_3 + E3_fixtures_yc$e3_6_4 +
#     E3_fixtures_yc$e3_6_5 + E3_fixtures_yc$e3_0_6 + E3_fixtures_yc$e3_1_6 + E3_fixtures_yc$e3_2_6 + E3_fixtures_yc$e3_3_6 +
#     E3_fixtures_yc$e3_4_6 + E3_fixtures_yc$e3_5_6 + E3_fixtures_yc$e3_6_6
# )
# #un25
# E3_fixtures_yc$e3_un25 <- (
#   E3_fixtures_yc$e3_0_0 + E3_fixtures_yc$e3_1_0 + E3_fixtures_yc$e3_0_1 + E3_fixtures_yc$e3_1_1 + E3_fixtures_yc$e3_2_0 + E3_fixtures_yc$e3_0_2
# )
# #odds
# E3_fixtures_yc$e3_ov25_odds <- round((1/E3_fixtures_yc$e3_ov25),digits = 2)
# E3_fixtures_yc$e3_un25_odds <- round((1/E3_fixtures_yc$e3_un25),digits = 2)
#
# E3_fixtures_yc$e3_ov25_odds
# E3_fixtures_yc$e3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E3_fixtures_yc$e3_ov25 <- percent(E3_fixtures_yc$e3_ov25, accuracy = 0.1)
#
# E3_fixtures_yc$e3_un25 <- percent(E3_fixtures_yc$e3_un25, accuracy = 0.1)
# E3_fixtures_yc$e3_pscore <- paste(round(E3_fixtures_yc$e3_xHYC,digits = 0),round(E3_fixtures_yc$e3_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(E3_fixtures,'Divisions/E3.xlsx',sheetName = "E3", append = TRUE)
# #################################################################################################################
# #EC
# HomeTeam_ec_yc <- rep(ec_teams, each = length(ec_teams))
# AwayTeam_ec_yc <- rep(ec_teams, length(ec_teams))
# EC_fixtures_yc <- cbind(HomeTeam_ec_yc,AwayTeam_ec_yc)
# EC_fixtures_yc <- as.data.frame(EC_fixtures_yc)
# EC_fixtures_yc <- EC_fixtures_yc[!EC_fixtures_yc$HomeTeam_ec_yc == EC_fixtures_yc$AwayTeam_ec_yc,]
# rownames(EC_fixtures_yc) <- NULL
# EC_fixtures_yc$Div <- "EC"
# EC_fixtures_yc <- EC_fixtures_yc[,c(3,1,2)]
#
# EC_fixtures_yc$avg_HY_ec <- ec_avg_HY
#
# EC_fixtures_yc$ec_homeyas <- rep(ec_home_yas,each = length(ec_teams)-1)
#
# ec_awayyds_lookup <- cbind(ec_teams,ec_away_yds)
#
# ec_awayyds_lookup <- as.data.frame(ec_awayyds_lookup)
#
# colnames(ec_awayyds_lookup) <- c("AwayTeam_ec_yc","ec_awayyds")
#
#
# require('RH2')
# EC_fixtures_yc$ec_awayyds <- sqldf("SELECT ec_awayyds_lookup.ec_awayyds FROM ec_awayyds_lookup INNER JOIN EC_fixtures_yc ON ec_awayyds_lookup.AwayTeam_ec_yc = EC_fixtures_yc.AwayTeam_ec_yc")
#
# EC_fixtures_yc$avg_AY_ec <- ec_avg_AY
#
# ec_awayyas_lookup <- cbind(ec_teams,ec_away_yas)
#
# ec_awayyas_lookup <- as.data.frame(ec_awayyas_lookup)
#
# colnames(ec_awayyas_lookup) <- c("AwayTeam_ec_yc","ec_awayyas")
#
# EC_fixtures_yc$ec_awayyas <- sqldf("SELECT ec_awayyas_lookup.ec_awayyas FROM ec_awayyas_lookup INNER JOIN EC_fixtures_yc ON ec_awayyas_lookup.AwayTeam_ec_yc = EC_fixtures_yc.AwayTeam_ec_yc")
#
# EC_fixtures_yc$ec_homeyds <- rep(ec_home_yds,each = length(ec_teams)-1)
#
# EC_fixtures_yc$ec_awayyds <- as.numeric(unlist(EC_fixtures_yc$ec_awayyds))
# #xGH
# EC_fixtures_yc$ec_xHYC <- EC_fixtures_yc$avg_HY_ec * EC_fixtures_yc$ec_homeyas * EC_fixtures_yc$ec_awayyds
# #xGA
#
# EC_fixtures_yc$ec_awayyas <- as.numeric(unlist(EC_fixtures_yc$ec_awayyas))
#
# EC_fixtures_yc$ec_xAYC <- EC_fixtures_yc$avg_AY_ec * EC_fixtures_yc$ec_awayyas * EC_fixtures_yc$ec_homeyds
#
# EC_fixtures_yc$ec_0_0 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_0 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_0_1 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_1 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_0 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_0_2 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_2 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_1 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_2 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_3 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_0 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_1 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_2 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_0_3 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_3 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_3 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_4 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_0 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_1 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_2 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_3 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_0_4 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_4 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_4 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_4 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_5 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_0 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_1 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_2 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_3 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_4 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_0_5 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_5 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_5 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_5 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_5 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_6 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_0 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(0,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_1 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(1,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_2 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(2,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_3 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(3,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_4 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(4,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_6_5 <- round(stats::dpois(6,EC_fixtures_yc$ec_xHYC) * stats::dpois(5,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_0_6 <- round(stats::dpois(0,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_1_6 <- round(stats::dpois(1,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_2_6 <- round(stats::dpois(2,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_3_6 <- round(stats::dpois(3,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_4_6 <- round(stats::dpois(4,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# EC_fixtures_yc$ec_5_6 <- round(stats::dpois(5,EC_fixtures_yc$ec_xHYC) * stats::dpois(6,EC_fixtures_yc$ec_xAYC), digits = 4)
# #Home win
# EC_fixtures_yc$ec_H <- (
#   EC_fixtures_yc$ec_1_0 + EC_fixtures_yc$ec_2_0 + EC_fixtures_yc$ec_2_1 + EC_fixtures_yc$ec_3_0 + EC_fixtures_yc$ec_3_1 +
#     EC_fixtures_yc$ec_3_2 + EC_fixtures_yc$ec_4_0 + EC_fixtures_yc$ec_4_1 + EC_fixtures_yc$ec_4_2 + EC_fixtures_yc$ec_4_3 +
#     EC_fixtures_yc$ec_5_0 + EC_fixtures_yc$ec_5_1 + EC_fixtures_yc$ec_5_2 + EC_fixtures_yc$ec_5_3 + EC_fixtures_yc$ec_5_4 +
#     EC_fixtures_yc$ec_6_0 + EC_fixtures_yc$ec_6_1 + EC_fixtures_yc$ec_6_2 + EC_fixtures_yc$ec_6_3 + EC_fixtures_yc$ec_6_4 +
#     EC_fixtures_yc$ec_6_5
# )
#
# EC_fixtures_yc$ec_H <- percent(EC_fixtures_yc$ec_H, accuracy = 0.1)
#
# #Draw
# EC_fixtures_yc$ec_D <- (
#
#   EC_fixtures_yc$ec_0_0 + EC_fixtures_yc$ec_1_1 + EC_fixtures_yc$ec_2_2 + EC_fixtures_yc$ec_3_3 + EC_fixtures_yc$ec_4_4 +
#     EC_fixtures_yc$ec_5_5 + EC_fixtures_yc$ec_6_6
# )
#
# EC_fixtures_yc$ec_D <- percent(EC_fixtures_yc$ec_D, accuracy = 0.1)
#
# #Away
#
# EC_fixtures_yc$ec_A <- (
#   EC_fixtures_yc$ec_0_1 + EC_fixtures_yc$ec_0_2 + EC_fixtures_yc$ec_1_2 + EC_fixtures_yc$ec_0_3 + EC_fixtures_yc$ec_1_3 +
#     EC_fixtures_yc$ec_2_3 + EC_fixtures_yc$ec_0_4 + EC_fixtures_yc$ec_1_4 + EC_fixtures_yc$ec_2_4 + EC_fixtures_yc$ec_3_4 +
#     EC_fixtures_yc$ec_0_5 + EC_fixtures_yc$ec_1_5 + EC_fixtures_yc$ec_2_5 + EC_fixtures_yc$ec_3_5 + EC_fixtures_yc$ec_4_5 +
#     EC_fixtures_yc$ec_0_6 + EC_fixtures_yc$ec_1_6 + EC_fixtures_yc$ec_2_6 + EC_fixtures_yc$ec_3_6 + EC_fixtures_yc$ec_4_6 +
#     EC_fixtures_yc$ec_5_6
# )
#
# EC_fixtures_yc$ec_A <- percent(EC_fixtures_yc$ec_A, accuracy = 0.1)
#
# #ov25
# EC_fixtures_yc$ec_ov25 <- (
#   EC_fixtures_yc$ec_2_1 + EC_fixtures_yc$ec_1_2 + EC_fixtures_yc$ec_2_2 + EC_fixtures_yc$ec_3_0 + EC_fixtures_yc$ec_3_1 +
#     EC_fixtures_yc$ec_3_2 + EC_fixtures_yc$ec_0_3 + EC_fixtures_yc$ec_1_3 + EC_fixtures_yc$ec_2_3 + EC_fixtures_yc$ec_3_3 +
#     EC_fixtures_yc$ec_4_0 + EC_fixtures_yc$ec_4_1 + EC_fixtures_yc$ec_4_2 + EC_fixtures_yc$ec_4_3 + EC_fixtures_yc$ec_0_4 +
#     EC_fixtures_yc$ec_1_4 + EC_fixtures_yc$ec_2_4 + EC_fixtures_yc$ec_3_4 + EC_fixtures_yc$ec_4_4 + EC_fixtures_yc$ec_5_0 +
#     EC_fixtures_yc$ec_5_1 + EC_fixtures_yc$ec_5_2 + EC_fixtures_yc$ec_5_3 + EC_fixtures_yc$ec_5_4 + EC_fixtures_yc$ec_0_5 +
#     EC_fixtures_yc$ec_1_5 + EC_fixtures_yc$ec_2_5 + EC_fixtures_yc$ec_3_5 + EC_fixtures_yc$ec_4_5 + EC_fixtures_yc$ec_5_5 +
#     EC_fixtures_yc$ec_6_0 + EC_fixtures_yc$ec_6_1 + EC_fixtures_yc$ec_6_2 + EC_fixtures_yc$ec_6_3 + EC_fixtures_yc$ec_6_4 +
#     EC_fixtures_yc$ec_6_5 + EC_fixtures_yc$ec_0_6 + EC_fixtures_yc$ec_1_6 + EC_fixtures_yc$ec_2_6 + EC_fixtures_yc$ec_3_6 +
#     EC_fixtures_yc$ec_4_6 + EC_fixtures_yc$ec_5_6 + EC_fixtures_yc$ec_6_6
# )
# #un25
# EC_fixtures_yc$ec_un25 <- (
#   EC_fixtures_yc$ec_0_0 + EC_fixtures_yc$ec_1_0 + EC_fixtures_yc$ec_0_1 + EC_fixtures_yc$ec_1_1 + EC_fixtures_yc$ec_2_0 + EC_fixtures_yc$ec_0_2
# )
# #odds
# EC_fixtures_yc$ec_ov25_odds <- round((1/EC_fixtures_yc$ec_ov25),digits = 2)
# EC_fixtures_yc$ec_un25_odds <- round((1/EC_fixtures_yc$ec_un25),digits = 2)
#
# EC_fixtures_yc$ec_ov25_odds
# EC_fixtures_yc$ec_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# EC_fixtures_yc$ec_ov25 <- percent(EC_fixtures_yc$ec_ov25, accuracy = 0.1)
#
# EC_fixtures_yc$ec_un25 <- percent(EC_fixtures_yc$ec_un25, accuracy = 0.1)
# EC_fixtures_yc$ec_pscore <- paste(round(EC_fixtures_yc$ec_xHYC,digits = 0),round(EC_fixtures_yc$ec_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(EC_fixtures,'Divisions/EC.xlsx',sheetName = "EC", append = TRUE)
# #################################################################################################################
# #F1
# HomeTeam_f1_yc <- rep(f1_teams, each = length(f1_teams))
# AwayTeam_f1_yc <- rep(f1_teams, length(f1_teams))
# F1_fixtures_yc <- cbind(HomeTeam_f1_yc,AwayTeam_f1_yc)
# F1_fixtures_yc <- as.data.frame(F1_fixtures_yc)
# F1_fixtures_yc <- F1_fixtures_yc[!F1_fixtures_yc$HomeTeam_f1_yc == F1_fixtures_yc$AwayTeam_f1_yc,]
# rownames(F1_fixtures_yc) <- NULL
# F1_fixtures_yc$Div <- "F1"
# F1_fixtures_yc <- F1_fixtures_yc[,c(3,1,2)]
#
# F1_fixtures_yc$avg_HY_f1 <- f1_avg_HY
#
# F1_fixtures_yc$f1_homeyas <- rep(f1_home_yas,each = length(f1_teams)-1)
#
# f1_awayyds_lookup <- cbind(f1_teams,f1_away_yds)
#
# f1_awayyds_lookup <- as.data.frame(f1_awayyds_lookup)
#
# colnames(f1_awayyds_lookup) <- c("AwayTeam_f1_yc","f1_awayyds")
#
#
# require('RH2')
# F1_fixtures_yc$f1_awayyds <- sqldf("SELECT f1_awayyds_lookup.f1_awayyds FROM f1_awayyds_lookup INNER JOIN F1_fixtures_yc ON f1_awayyds_lookup.AwayTeam_f1_yc = F1_fixtures_yc.AwayTeam_f1_yc")
#
# F1_fixtures_yc$avg_AY_f1 <- f1_avg_AY
#
# f1_awayyas_lookup <- cbind(f1_teams,f1_away_yas)
#
# f1_awayyas_lookup <- as.data.frame(f1_awayyas_lookup)
#
# colnames(f1_awayyas_lookup) <- c("AwayTeam_f1_yc","f1_awayyas")
#
# F1_fixtures_yc$f1_awayyas <- sqldf("SELECT f1_awayyas_lookup.f1_awayyas FROM f1_awayyas_lookup INNER JOIN F1_fixtures_yc ON f1_awayyas_lookup.AwayTeam_f1_yc = F1_fixtures_yc.AwayTeam_f1_yc")
#
# F1_fixtures_yc$f1_homeyds <- rep(f1_home_yds,each = length(f1_teams)-1)
#
# F1_fixtures_yc$f1_awayyds <- as.numeric(unlist(F1_fixtures_yc$f1_awayyds))
# #xGH
# F1_fixtures_yc$f1_xHYC <- F1_fixtures_yc$avg_HY_f1 * F1_fixtures_yc$f1_homeyas * F1_fixtures_yc$f1_awayyds
# #xGA
#
# F1_fixtures_yc$f1_awayyas <- as.numeric(unlist(F1_fixtures_yc$f1_awayyas))
#
# F1_fixtures_yc$f1_xAYC <- F1_fixtures_yc$avg_AY_f1 * F1_fixtures_yc$f1_awayyas * F1_fixtures_yc$f1_homeyds
#
# F1_fixtures_yc$f1_0_0 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_0 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_0_1 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_1 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_0 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_0_2 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_2 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_1 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_2 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_3 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_0 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_1 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_2 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_0_3 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_3 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_3 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_4 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_0 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_1 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_2 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_3 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_0_4 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_4 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_4 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_4 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_5 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_0 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_1 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_2 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_3 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_4 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_0_5 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_5 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_5 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_5 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_5 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_6 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_0 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(0,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_1 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(1,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_2 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(2,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_3 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(3,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_4 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(4,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_6_5 <- round(stats::dpois(6,F1_fixtures_yc$f1_xHYC) * stats::dpois(5,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_0_6 <- round(stats::dpois(0,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_1_6 <- round(stats::dpois(1,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_2_6 <- round(stats::dpois(2,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_3_6 <- round(stats::dpois(3,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_4_6 <- round(stats::dpois(4,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# F1_fixtures_yc$f1_5_6 <- round(stats::dpois(5,F1_fixtures_yc$f1_xHYC) * stats::dpois(6,F1_fixtures_yc$f1_xAYC), digits = 4)
# #Home win
# F1_fixtures_yc$f1_H <- (
#   F1_fixtures_yc$f1_1_0 + F1_fixtures_yc$f1_2_0 + F1_fixtures_yc$f1_2_1 + F1_fixtures_yc$f1_3_0 + F1_fixtures_yc$f1_3_1 +
#     F1_fixtures_yc$f1_3_2 + F1_fixtures_yc$f1_4_0 + F1_fixtures_yc$f1_4_1 + F1_fixtures_yc$f1_4_2 + F1_fixtures_yc$f1_4_3 +
#     F1_fixtures_yc$f1_5_0 + F1_fixtures_yc$f1_5_1 + F1_fixtures_yc$f1_5_2 + F1_fixtures_yc$f1_5_3 + F1_fixtures_yc$f1_5_4 +
#     F1_fixtures_yc$f1_6_0 + F1_fixtures_yc$f1_6_1 + F1_fixtures_yc$f1_6_2 + F1_fixtures_yc$f1_6_3 + F1_fixtures_yc$f1_6_4 +
#     F1_fixtures_yc$f1_6_5
# )
#
# F1_fixtures_yc$f1_H <- percent(F1_fixtures_yc$f1_H, accuracy = 0.1)
#
# #Draw
# F1_fixtures_yc$f1_D <- (
#
#   F1_fixtures_yc$f1_0_0 + F1_fixtures_yc$f1_1_1 + F1_fixtures_yc$f1_2_2 + F1_fixtures_yc$f1_3_3 + F1_fixtures_yc$f1_4_4 +
#     F1_fixtures_yc$f1_5_5 + F1_fixtures_yc$f1_6_6
# )
#
# F1_fixtures_yc$f1_D <- percent(F1_fixtures_yc$f1_D, accuracy = 0.1)
#
# #Away
#
# F1_fixtures_yc$f1_A <- (
#   F1_fixtures_yc$f1_0_1 + F1_fixtures_yc$f1_0_2 + F1_fixtures_yc$f1_1_2 + F1_fixtures_yc$f1_0_3 + F1_fixtures_yc$f1_1_3 +
#     F1_fixtures_yc$f1_2_3 + F1_fixtures_yc$f1_0_4 + F1_fixtures_yc$f1_1_4 + F1_fixtures_yc$f1_2_4 + F1_fixtures_yc$f1_3_4 +
#     F1_fixtures_yc$f1_0_5 + F1_fixtures_yc$f1_1_5 + F1_fixtures_yc$f1_2_5 + F1_fixtures_yc$f1_3_5 + F1_fixtures_yc$f1_4_5 +
#     F1_fixtures_yc$f1_0_6 + F1_fixtures_yc$f1_1_6 + F1_fixtures_yc$f1_2_6 + F1_fixtures_yc$f1_3_6 + F1_fixtures_yc$f1_4_6 +
#     F1_fixtures_yc$f1_5_6
# )
#
# F1_fixtures_yc$f1_A <- percent(F1_fixtures_yc$f1_A, accuracy = 0.1)
#
# #ov25
# F1_fixtures_yc$f1_ov25 <- (
#   F1_fixtures_yc$f1_2_1 + F1_fixtures_yc$f1_1_2 + F1_fixtures_yc$f1_2_2 + F1_fixtures_yc$f1_3_0 + F1_fixtures_yc$f1_3_1 +
#     F1_fixtures_yc$f1_3_2 + F1_fixtures_yc$f1_0_3 + F1_fixtures_yc$f1_1_3 + F1_fixtures_yc$f1_2_3 + F1_fixtures_yc$f1_3_3 +
#     F1_fixtures_yc$f1_4_0 + F1_fixtures_yc$f1_4_1 + F1_fixtures_yc$f1_4_2 + F1_fixtures_yc$f1_4_3 + F1_fixtures_yc$f1_0_4 +
#     F1_fixtures_yc$f1_1_4 + F1_fixtures_yc$f1_2_4 + F1_fixtures_yc$f1_3_4 + F1_fixtures_yc$f1_4_4 + F1_fixtures_yc$f1_5_0 +
#     F1_fixtures_yc$f1_5_1 + F1_fixtures_yc$f1_5_2 + F1_fixtures_yc$f1_5_3 + F1_fixtures_yc$f1_5_4 + F1_fixtures_yc$f1_0_5 +
#     F1_fixtures_yc$f1_1_5 + F1_fixtures_yc$f1_2_5 + F1_fixtures_yc$f1_3_5 + F1_fixtures_yc$f1_4_5 + F1_fixtures_yc$f1_5_5 +
#     F1_fixtures_yc$f1_6_0 + F1_fixtures_yc$f1_6_1 + F1_fixtures_yc$f1_6_2 + F1_fixtures_yc$f1_6_3 + F1_fixtures_yc$f1_6_4 +
#     F1_fixtures_yc$f1_6_5 + F1_fixtures_yc$f1_0_6 + F1_fixtures_yc$f1_1_6 + F1_fixtures_yc$f1_2_6 + F1_fixtures_yc$f1_3_6 +
#     F1_fixtures_yc$f1_4_6 + F1_fixtures_yc$f1_5_6 + F1_fixtures_yc$f1_6_6
# )
# #un25
# F1_fixtures_yc$f1_un25 <- (
#   F1_fixtures_yc$f1_0_0 + F1_fixtures_yc$f1_1_0 + F1_fixtures_yc$f1_0_1 + F1_fixtures_yc$f1_1_1 + F1_fixtures_yc$f1_2_0 + F1_fixtures_yc$f1_0_2
# )
# #odds
# F1_fixtures_yc$f1_ov25_odds <- round((1/F1_fixtures_yc$f1_ov25),digits = 2)
# F1_fixtures_yc$f1_un25_odds <- round((1/F1_fixtures_yc$f1_un25),digits = 2)
#
# F1_fixtures_yc$f1_ov25_odds
# F1_fixtures_yc$f1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F1_fixtures_yc$f1_ov25 <- percent(F1_fixtures_yc$f1_ov25, accuracy = 0.1)
#
# F1_fixtures_yc$f1_un25 <- percent(F1_fixtures_yc$f1_un25, accuracy = 0.1)
# F1_fixtures_yc$f1_pscore <- paste(round(F1_fixtures_yc$f1_xHYC,digits = 0),round(F1_fixtures_yc$f1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #F2
# HomeTeam_f2_yc <- rep(f2_teams, each = length(f2_teams))
# AwayTeam_f2_yc <- rep(f2_teams, length(f2_teams))
# F2_fixtures_yc <- cbind(HomeTeam_f2_yc,AwayTeam_f2_yc)
# F2_fixtures_yc <- as.data.frame(F2_fixtures_yc)
# F2_fixtures_yc <- F2_fixtures_yc[!F2_fixtures_yc$HomeTeam_f2_yc == F2_fixtures_yc$AwayTeam_f2_yc,]
# rownames(F2_fixtures_yc) <- NULL
# F2_fixtures_yc$Div <- "F2"
# F2_fixtures_yc <- F2_fixtures_yc[,c(3,1,2)]
#
# F2_fixtures_yc$avg_HY_f2 <- f2_avg_HY
#
# F2_fixtures_yc$f2_homeyas <- rep(f2_home_yas,each = length(f2_teams)-1)
#
# f2_awayyds_lookup <- cbind(f2_teams,f2_away_yds)
#
# f2_awayyds_lookup <- as.data.frame(f2_awayyds_lookup)
#
# colnames(f2_awayyds_lookup) <- c("AwayTeam_f2_yc","f2_awayyds")
#
#
# require('RH2')
# F2_fixtures_yc$f2_awayyds <- sqldf("SELECT f2_awayyds_lookup.f2_awayyds FROM f2_awayyds_lookup INNER JOIN F2_fixtures_yc ON f2_awayyds_lookup.AwayTeam_f2_yc = F2_fixtures_yc.AwayTeam_f2_yc")
#
# F2_fixtures_yc$avg_AY_f2 <- f2_avg_AY
#
# f2_awayyas_lookup <- cbind(f2_teams,f2_away_yas)
#
# f2_awayyas_lookup <- as.data.frame(f2_awayyas_lookup)
#
# colnames(f2_awayyas_lookup) <- c("AwayTeam_f2_yc","f2_awayyas")
#
# F2_fixtures_yc$f2_awayyas <- sqldf("SELECT f2_awayyas_lookup.f2_awayyas FROM f2_awayyas_lookup INNER JOIN F2_fixtures_yc ON f2_awayyas_lookup.AwayTeam_f2_yc = F2_fixtures_yc.AwayTeam_f2_yc")
#
# F2_fixtures_yc$f2_homeyds <- rep(f2_home_yds,each = length(f2_teams)-1)
#
# F2_fixtures_yc$f2_awayyds <- as.numeric(unlist(F2_fixtures_yc$f2_awayyds))
# #xGH
# F2_fixtures_yc$f2_xHYC <- F2_fixtures_yc$avg_HY_f2 * F2_fixtures_yc$f2_homeyas * F2_fixtures_yc$f2_awayyds
# #xGA
#
# F2_fixtures_yc$f2_awayyas <- as.numeric(unlist(F2_fixtures_yc$f2_awayyas))
#
# F2_fixtures_yc$f2_xAYC <- F2_fixtures_yc$avg_AY_f2 * F2_fixtures_yc$f2_awayyas * F2_fixtures_yc$f2_homeyds
#
# F2_fixtures_yc$f2_0_0 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_0 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_0_1 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_1 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_0 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_0_2 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_2 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_1 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_2 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_3 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_0 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_1 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_2 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_0_3 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_3 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_3 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_4 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_0 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_1 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_2 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_3 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_0_4 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_4 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_4 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_4 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_5 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_0 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_1 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_2 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_3 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_4 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_0_5 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_5 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_5 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_5 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_5 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_6 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_0 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(0,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_1 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(1,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_2 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(2,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_3 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(3,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_4 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(4,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_6_5 <- round(stats::dpois(6,F2_fixtures_yc$f2_xHYC) * stats::dpois(5,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_0_6 <- round(stats::dpois(0,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_1_6 <- round(stats::dpois(1,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_2_6 <- round(stats::dpois(2,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_3_6 <- round(stats::dpois(3,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_4_6 <- round(stats::dpois(4,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# F2_fixtures_yc$f2_5_6 <- round(stats::dpois(5,F2_fixtures_yc$f2_xHYC) * stats::dpois(6,F2_fixtures_yc$f2_xAYC), digits = 4)
# #Home win
# F2_fixtures_yc$f2_H <- (
#   F2_fixtures_yc$f2_1_0 + F2_fixtures_yc$f2_2_0 + F2_fixtures_yc$f2_2_1 + F2_fixtures_yc$f2_3_0 + F2_fixtures_yc$f2_3_1 +
#     F2_fixtures_yc$f2_3_2 + F2_fixtures_yc$f2_4_0 + F2_fixtures_yc$f2_4_1 + F2_fixtures_yc$f2_4_2 + F2_fixtures_yc$f2_4_3 +
#     F2_fixtures_yc$f2_5_0 + F2_fixtures_yc$f2_5_1 + F2_fixtures_yc$f2_5_2 + F2_fixtures_yc$f2_5_3 + F2_fixtures_yc$f2_5_4 +
#     F2_fixtures_yc$f2_6_0 + F2_fixtures_yc$f2_6_1 + F2_fixtures_yc$f2_6_2 + F2_fixtures_yc$f2_6_3 + F2_fixtures_yc$f2_6_4 +
#     F2_fixtures_yc$f2_6_5
# )
#
# F2_fixtures_yc$f2_H <- percent(F2_fixtures_yc$f2_H, accuracy = 0.1)
#
# #Draw
# F2_fixtures_yc$f2_D <- (
#
#   F2_fixtures_yc$f2_0_0 + F2_fixtures_yc$f2_1_1 + F2_fixtures_yc$f2_2_2 + F2_fixtures_yc$f2_3_3 + F2_fixtures_yc$f2_4_4 +
#     F2_fixtures_yc$f2_5_5 + F2_fixtures_yc$f2_6_6
# )
#
# F2_fixtures_yc$f2_D <- percent(F2_fixtures_yc$f2_D, accuracy = 0.1)
#
# #Away
#
# F2_fixtures_yc$f2_A <- (
#   F2_fixtures_yc$f2_0_1 + F2_fixtures_yc$f2_0_2 + F2_fixtures_yc$f2_1_2 + F2_fixtures_yc$f2_0_3 + F2_fixtures_yc$f2_1_3 +
#     F2_fixtures_yc$f2_2_3 + F2_fixtures_yc$f2_0_4 + F2_fixtures_yc$f2_1_4 + F2_fixtures_yc$f2_2_4 + F2_fixtures_yc$f2_3_4 +
#     F2_fixtures_yc$f2_0_5 + F2_fixtures_yc$f2_1_5 + F2_fixtures_yc$f2_2_5 + F2_fixtures_yc$f2_3_5 + F2_fixtures_yc$f2_4_5 +
#     F2_fixtures_yc$f2_0_6 + F2_fixtures_yc$f2_1_6 + F2_fixtures_yc$f2_2_6 + F2_fixtures_yc$f2_3_6 + F2_fixtures_yc$f2_4_6 +
#     F2_fixtures_yc$f2_5_6
# )
#
# F2_fixtures_yc$f2_A <- percent(F2_fixtures_yc$f2_A, accuracy = 0.1)
#
# #ov25
# F2_fixtures_yc$f2_ov25 <- (
#   F2_fixtures_yc$f2_2_1 + F2_fixtures_yc$f2_1_2 + F2_fixtures_yc$f2_2_2 + F2_fixtures_yc$f2_3_0 + F2_fixtures_yc$f2_3_1 +
#     F2_fixtures_yc$f2_3_2 + F2_fixtures_yc$f2_0_3 + F2_fixtures_yc$f2_1_3 + F2_fixtures_yc$f2_2_3 + F2_fixtures_yc$f2_3_3 +
#     F2_fixtures_yc$f2_4_0 + F2_fixtures_yc$f2_4_1 + F2_fixtures_yc$f2_4_2 + F2_fixtures_yc$f2_4_3 + F2_fixtures_yc$f2_0_4 +
#     F2_fixtures_yc$f2_1_4 + F2_fixtures_yc$f2_2_4 + F2_fixtures_yc$f2_3_4 + F2_fixtures_yc$f2_4_4 + F2_fixtures_yc$f2_5_0 +
#     F2_fixtures_yc$f2_5_1 + F2_fixtures_yc$f2_5_2 + F2_fixtures_yc$f2_5_3 + F2_fixtures_yc$f2_5_4 + F2_fixtures_yc$f2_0_5 +
#     F2_fixtures_yc$f2_1_5 + F2_fixtures_yc$f2_2_5 + F2_fixtures_yc$f2_3_5 + F2_fixtures_yc$f2_4_5 + F2_fixtures_yc$f2_5_5 +
#     F2_fixtures_yc$f2_6_0 + F2_fixtures_yc$f2_6_1 + F2_fixtures_yc$f2_6_2 + F2_fixtures_yc$f2_6_3 + F2_fixtures_yc$f2_6_4 +
#     F2_fixtures_yc$f2_6_5 + F2_fixtures_yc$f2_0_6 + F2_fixtures_yc$f2_1_6 + F2_fixtures_yc$f2_2_6 + F2_fixtures_yc$f2_3_6 +
#     F2_fixtures_yc$f2_4_6 + F2_fixtures_yc$f2_5_6 + F2_fixtures_yc$f2_6_6
# )
# #un25
# F2_fixtures_yc$f2_un25 <- (
#   F2_fixtures_yc$f2_0_0 + F2_fixtures_yc$f2_1_0 + F2_fixtures_yc$f2_0_1 + F2_fixtures_yc$f2_1_1 + F2_fixtures_yc$f2_2_0 + F2_fixtures_yc$f2_0_2
# )
# #odds
# F2_fixtures_yc$f2_ov25_odds <- round((1/F2_fixtures_yc$f2_ov25),digits = 2)
# F2_fixtures_yc$f2_un25_odds <- round((1/F2_fixtures_yc$f2_un25),digits = 2)
#
# F2_fixtures_yc$f2_ov25_odds
# F2_fixtures_yc$f2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F2_fixtures_yc$f2_ov25 <- percent(F2_fixtures_yc$f2_ov25, accuracy = 0.1)
#
# F2_fixtures_yc$f2_un25 <- percent(F2_fixtures_yc$f2_un25, accuracy = 0.1)
# F2_fixtures_yc$f2_pscore <- paste(round(F2_fixtures_yc$f2_xHYC,digits = 0),round(F2_fixtures_yc$f2_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #G1
# HomeTeam_g1_yc <- rep(g1_teams, each = length(g1_teams))
# AwayTeam_g1_yc <- rep(g1_teams, length(g1_teams))
# G1_fixtures_yc <- cbind(HomeTeam_g1_yc,AwayTeam_g1_yc)
# G1_fixtures_yc <- as.data.frame(G1_fixtures_yc)
# G1_fixtures_yc <- G1_fixtures_yc[!G1_fixtures_yc$HomeTeam_g1_yc == G1_fixtures_yc$AwayTeam_g1_yc,]
# rownames(G1_fixtures_yc) <- NULL
# G1_fixtures_yc$Div <- "G1"
# G1_fixtures_yc <- G1_fixtures_yc[,c(3,1,2)]
#
# G1_fixtures_yc$avg_HY_g1 <- g1_avg_HY
#
# G1_fixtures_yc$g1_homeyas <- rep(g1_home_yas,each = length(g1_teams)-1)
#
# g1_awayyds_lookup <- cbind(g1_teams,g1_away_yds)
#
# g1_awayyds_lookup <- as.data.frame(g1_awayyds_lookup)
#
# colnames(g1_awayyds_lookup) <- c("AwayTeam_g1_yc","g1_awayyds")
#
#
# require('RH2')
# G1_fixtures_yc$g1_awayyds <- sqldf("SELECT g1_awayyds_lookup.g1_awayyds FROM g1_awayyds_lookup INNER JOIN G1_fixtures_yc ON g1_awayyds_lookup.AwayTeam_g1_yc = G1_fixtures_yc.AwayTeam_g1_yc")
#
# G1_fixtures_yc$avg_AY_g1 <- g1_avg_AY
#
# g1_awayyas_lookup <- cbind(g1_teams,g1_away_yas)
#
# g1_awayyas_lookup <- as.data.frame(g1_awayyas_lookup)
#
# colnames(g1_awayyas_lookup) <- c("AwayTeam_g1_yc","g1_awayyas")
#
# G1_fixtures_yc$g1_awayyas <- sqldf("SELECT g1_awayyas_lookup.g1_awayyas FROM g1_awayyas_lookup INNER JOIN G1_fixtures_yc ON g1_awayyas_lookup.AwayTeam_g1_yc = G1_fixtures_yc.AwayTeam_g1_yc")
#
# G1_fixtures_yc$g1_homeyds <- rep(g1_home_yds,each = length(g1_teams)-1)
#
# G1_fixtures_yc$g1_awayyds <- as.numeric(unlist(G1_fixtures_yc$g1_awayyds))
# #xGH
# G1_fixtures_yc$g1_xHYC <- G1_fixtures_yc$avg_HY_g1 * G1_fixtures_yc$g1_homeyas * G1_fixtures_yc$g1_awayyds
# #xGA
#
# G1_fixtures_yc$g1_awayyas <- as.numeric(unlist(G1_fixtures_yc$g1_awayyas))
#
# G1_fixtures_yc$g1_xAYC <- G1_fixtures_yc$avg_AY_g1 * G1_fixtures_yc$g1_awayyas * G1_fixtures_yc$g1_homeyds
#
# G1_fixtures_yc$g1_0_0 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_0 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_0_1 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_1 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_0 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_0_2 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_2 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_1 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_2 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_3 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_0 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_1 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_2 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_0_3 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_3 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_3 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_4 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_0 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_1 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_2 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_3 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_0_4 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_4 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_4 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_4 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_5 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_0 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_1 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_2 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_3 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_4 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_0_5 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_5 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_5 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_5 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_5 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_6 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_0 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(0,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_1 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(1,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_2 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(2,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_3 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(3,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_4 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(4,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_6_5 <- round(stats::dpois(6,G1_fixtures_yc$g1_xHYC) * stats::dpois(5,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_0_6 <- round(stats::dpois(0,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_1_6 <- round(stats::dpois(1,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_2_6 <- round(stats::dpois(2,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_3_6 <- round(stats::dpois(3,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_4_6 <- round(stats::dpois(4,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# G1_fixtures_yc$g1_5_6 <- round(stats::dpois(5,G1_fixtures_yc$g1_xHYC) * stats::dpois(6,G1_fixtures_yc$g1_xAYC), digits = 4)
# #Home win
# G1_fixtures_yc$g1_H <- (
#   G1_fixtures_yc$g1_1_0 + G1_fixtures_yc$g1_2_0 + G1_fixtures_yc$g1_2_1 + G1_fixtures_yc$g1_3_0 + G1_fixtures_yc$g1_3_1 +
#     G1_fixtures_yc$g1_3_2 + G1_fixtures_yc$g1_4_0 + G1_fixtures_yc$g1_4_1 + G1_fixtures_yc$g1_4_2 + G1_fixtures_yc$g1_4_3 +
#     G1_fixtures_yc$g1_5_0 + G1_fixtures_yc$g1_5_1 + G1_fixtures_yc$g1_5_2 + G1_fixtures_yc$g1_5_3 + G1_fixtures_yc$g1_5_4 +
#     G1_fixtures_yc$g1_6_0 + G1_fixtures_yc$g1_6_1 + G1_fixtures_yc$g1_6_2 + G1_fixtures_yc$g1_6_3 + G1_fixtures_yc$g1_6_4 +
#     G1_fixtures_yc$g1_6_5
# )
#
# G1_fixtures_yc$g1_H <- percent(G1_fixtures_yc$g1_H, accuracy = 0.1)
#
# #Draw
# G1_fixtures_yc$g1_D <- (
#
#   G1_fixtures_yc$g1_0_0 + G1_fixtures_yc$g1_1_1 + G1_fixtures_yc$g1_2_2 + G1_fixtures_yc$g1_3_3 + G1_fixtures_yc$g1_4_4 +
#     G1_fixtures_yc$g1_5_5 + G1_fixtures_yc$g1_6_6
# )
#
# G1_fixtures_yc$g1_D <- percent(G1_fixtures_yc$g1_D, accuracy = 0.1)
#
# #Away
#
# G1_fixtures_yc$g1_A <- (
#   G1_fixtures_yc$g1_0_1 + G1_fixtures_yc$g1_0_2 + G1_fixtures_yc$g1_1_2 + G1_fixtures_yc$g1_0_3 + G1_fixtures_yc$g1_1_3 +
#     G1_fixtures_yc$g1_2_3 + G1_fixtures_yc$g1_0_4 + G1_fixtures_yc$g1_1_4 + G1_fixtures_yc$g1_2_4 + G1_fixtures_yc$g1_3_4 +
#     G1_fixtures_yc$g1_0_5 + G1_fixtures_yc$g1_1_5 + G1_fixtures_yc$g1_2_5 + G1_fixtures_yc$g1_3_5 + G1_fixtures_yc$g1_4_5 +
#     G1_fixtures_yc$g1_0_6 + G1_fixtures_yc$g1_1_6 + G1_fixtures_yc$g1_2_6 + G1_fixtures_yc$g1_3_6 + G1_fixtures_yc$g1_4_6 +
#     G1_fixtures_yc$g1_5_6
# )
#
# G1_fixtures_yc$g1_A <- percent(G1_fixtures_yc$g1_A, accuracy = 0.1)
#
# #ov25
# G1_fixtures_yc$g1_ov25 <- (
#   G1_fixtures_yc$g1_2_1 + G1_fixtures_yc$g1_1_2 + G1_fixtures_yc$g1_2_2 + G1_fixtures_yc$g1_3_0 + G1_fixtures_yc$g1_3_1 +
#     G1_fixtures_yc$g1_3_2 + G1_fixtures_yc$g1_0_3 + G1_fixtures_yc$g1_1_3 + G1_fixtures_yc$g1_2_3 + G1_fixtures_yc$g1_3_3 +
#     G1_fixtures_yc$g1_4_0 + G1_fixtures_yc$g1_4_1 + G1_fixtures_yc$g1_4_2 + G1_fixtures_yc$g1_4_3 + G1_fixtures_yc$g1_0_4 +
#     G1_fixtures_yc$g1_1_4 + G1_fixtures_yc$g1_2_4 + G1_fixtures_yc$g1_3_4 + G1_fixtures_yc$g1_4_4 + G1_fixtures_yc$g1_5_0 +
#     G1_fixtures_yc$g1_5_1 + G1_fixtures_yc$g1_5_2 + G1_fixtures_yc$g1_5_3 + G1_fixtures_yc$g1_5_4 + G1_fixtures_yc$g1_0_5 +
#     G1_fixtures_yc$g1_1_5 + G1_fixtures_yc$g1_2_5 + G1_fixtures_yc$g1_3_5 + G1_fixtures_yc$g1_4_5 + G1_fixtures_yc$g1_5_5 +
#     G1_fixtures_yc$g1_6_0 + G1_fixtures_yc$g1_6_1 + G1_fixtures_yc$g1_6_2 + G1_fixtures_yc$g1_6_3 + G1_fixtures_yc$g1_6_4 +
#     G1_fixtures_yc$g1_6_5 + G1_fixtures_yc$g1_0_6 + G1_fixtures_yc$g1_1_6 + G1_fixtures_yc$g1_2_6 + G1_fixtures_yc$g1_3_6 +
#     G1_fixtures_yc$g1_4_6 + G1_fixtures_yc$g1_5_6 + G1_fixtures_yc$g1_6_6
# )
# #un25
# G1_fixtures_yc$g1_un25 <- (
#   G1_fixtures_yc$g1_0_0 + G1_fixtures_yc$g1_1_0 + G1_fixtures_yc$g1_0_1 + G1_fixtures_yc$g1_1_1 + G1_fixtures_yc$g1_2_0 + G1_fixtures_yc$g1_0_2
# )
# #odds
# G1_fixtures_yc$g1_ov25_odds <- round((1/G1_fixtures_yc$g1_ov25),digits = 2)
# G1_fixtures_yc$g1_un25_odds <- round((1/G1_fixtures_yc$g1_un25),digits = 2)
#
# G1_fixtures_yc$g1_ov25_odds
# G1_fixtures_yc$g1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# G1_fixtures_yc$g1_ov25 <- percent(G1_fixtures_yc$g1_ov25, accuracy = 0.1)
#
# G1_fixtures_yc$g1_un25 <- percent(G1_fixtures_yc$g1_un25, accuracy = 0.1)
# G1_fixtures_yc$g1_pscore <- paste(round(G1_fixtures_yc$g1_xHYC,digits = 0),round(G1_fixtures_yc$g1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #I1
# HomeTeam_i1_yc <- rep(i1_teams, each = length(i1_teams))
# AwayTeam_i1_yc <- rep(i1_teams, length(i1_teams))
# I1_fixtures_yc <- cbind(HomeTeam_i1_yc,AwayTeam_i1_yc)
# I1_fixtures_yc <- as.data.frame(I1_fixtures_yc)
# I1_fixtures_yc <- I1_fixtures_yc[!I1_fixtures_yc$HomeTeam_i1_yc == I1_fixtures_yc$AwayTeam_i1_yc,]
# rownames(I1_fixtures_yc) <- NULL
# I1_fixtures_yc$Div <- "I1"
# I1_fixtures_yc <- I1_fixtures_yc[,c(3,1,2)]
#
# I1_fixtures_yc$avg_HY_i1 <- i1_avg_HY
#
# I1_fixtures_yc$i1_homeyas <- rep(i1_home_yas,each = length(i1_teams)-1)
#
# i1_awayyds_lookup <- cbind(i1_teams,i1_away_yds)
#
# i1_awayyds_lookup <- as.data.frame(i1_awayyds_lookup)
#
# colnames(i1_awayyds_lookup) <- c("AwayTeam_i1_yc","i1_awayyds")
#
#
# require('RH2')
# I1_fixtures_yc$i1_awayyds <- sqldf("SELECT i1_awayyds_lookup.i1_awayyds FROM i1_awayyds_lookup INNER JOIN I1_fixtures_yc ON i1_awayyds_lookup.AwayTeam_i1_yc = I1_fixtures_yc.AwayTeam_i1_yc")
#
# I1_fixtures_yc$avg_AY_i1 <- i1_avg_AY
#
# i1_awayyas_lookup <- cbind(i1_teams,i1_away_yas)
#
# i1_awayyas_lookup <- as.data.frame(i1_awayyas_lookup)
#
# colnames(i1_awayyas_lookup) <- c("AwayTeam_i1_yc","i1_awayyas")
#
# I1_fixtures_yc$i1_awayyas <- sqldf("SELECT i1_awayyas_lookup.i1_awayyas FROM i1_awayyas_lookup INNER JOIN I1_fixtures_yc ON i1_awayyas_lookup.AwayTeam_i1_yc = I1_fixtures_yc.AwayTeam_i1_yc")
#
# I1_fixtures_yc$i1_homeyds <- rep(i1_home_yds,each = length(i1_teams)-1)
#
# I1_fixtures_yc$i1_awayyds <- as.numeric(unlist(I1_fixtures_yc$i1_awayyds))
# #xGH
# I1_fixtures_yc$i1_xHYC <- I1_fixtures_yc$avg_HY_i1 * I1_fixtures_yc$i1_homeyas * I1_fixtures_yc$i1_awayyds
# #xGA
#
# I1_fixtures_yc$i1_awayyas <- as.numeric(unlist(I1_fixtures_yc$i1_awayyas))
#
# I1_fixtures_yc$i1_xAYC <- I1_fixtures_yc$avg_AY_i1 * I1_fixtures_yc$i1_awayyas * I1_fixtures_yc$i1_homeyds
#
# I1_fixtures_yc$i1_0_0 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_0 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_0_1 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_1 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_0 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_0_2 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_2 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_1 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_2 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_3 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_0 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_1 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_2 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_0_3 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_3 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_3 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_4 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_0 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_1 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_2 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_3 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_0_4 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_4 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_4 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_4 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_5 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_0 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_1 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_2 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_3 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_4 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_0_5 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_5 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_5 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_5 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_5 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_6 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_0 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(0,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_1 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(1,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_2 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(2,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_3 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(3,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_4 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(4,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_6_5 <- round(stats::dpois(6,I1_fixtures_yc$i1_xHYC) * stats::dpois(5,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_0_6 <- round(stats::dpois(0,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_1_6 <- round(stats::dpois(1,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_2_6 <- round(stats::dpois(2,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_3_6 <- round(stats::dpois(3,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_4_6 <- round(stats::dpois(4,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# I1_fixtures_yc$i1_5_6 <- round(stats::dpois(5,I1_fixtures_yc$i1_xHYC) * stats::dpois(6,I1_fixtures_yc$i1_xAYC), digits = 4)
# #Home win
# I1_fixtures_yc$i1_H <- (
#   I1_fixtures_yc$i1_1_0 + I1_fixtures_yc$i1_2_0 + I1_fixtures_yc$i1_2_1 + I1_fixtures_yc$i1_3_0 + I1_fixtures_yc$i1_3_1 +
#     I1_fixtures_yc$i1_3_2 + I1_fixtures_yc$i1_4_0 + I1_fixtures_yc$i1_4_1 + I1_fixtures_yc$i1_4_2 + I1_fixtures_yc$i1_4_3 +
#     I1_fixtures_yc$i1_5_0 + I1_fixtures_yc$i1_5_1 + I1_fixtures_yc$i1_5_2 + I1_fixtures_yc$i1_5_3 + I1_fixtures_yc$i1_5_4 +
#     I1_fixtures_yc$i1_6_0 + I1_fixtures_yc$i1_6_1 + I1_fixtures_yc$i1_6_2 + I1_fixtures_yc$i1_6_3 + I1_fixtures_yc$i1_6_4 +
#     I1_fixtures_yc$i1_6_5
# )
#
# I1_fixtures_yc$i1_H <- percent(I1_fixtures_yc$i1_H, accuracy = 0.1)
#
# #Draw
# I1_fixtures_yc$i1_D <- (
#
#   I1_fixtures_yc$i1_0_0 + I1_fixtures_yc$i1_1_1 + I1_fixtures_yc$i1_2_2 + I1_fixtures_yc$i1_3_3 + I1_fixtures_yc$i1_4_4 +
#     I1_fixtures_yc$i1_5_5 + I1_fixtures_yc$i1_6_6
# )
#
# I1_fixtures_yc$i1_D <- percent(I1_fixtures_yc$i1_D, accuracy = 0.1)
#
# #Away
#
# I1_fixtures_yc$i1_A <- (
#   I1_fixtures_yc$i1_0_1 + I1_fixtures_yc$i1_0_2 + I1_fixtures_yc$i1_1_2 + I1_fixtures_yc$i1_0_3 + I1_fixtures_yc$i1_1_3 +
#     I1_fixtures_yc$i1_2_3 + I1_fixtures_yc$i1_0_4 + I1_fixtures_yc$i1_1_4 + I1_fixtures_yc$i1_2_4 + I1_fixtures_yc$i1_3_4 +
#     I1_fixtures_yc$i1_0_5 + I1_fixtures_yc$i1_1_5 + I1_fixtures_yc$i1_2_5 + I1_fixtures_yc$i1_3_5 + I1_fixtures_yc$i1_4_5 +
#     I1_fixtures_yc$i1_0_6 + I1_fixtures_yc$i1_1_6 + I1_fixtures_yc$i1_2_6 + I1_fixtures_yc$i1_3_6 + I1_fixtures_yc$i1_4_6 +
#     I1_fixtures_yc$i1_5_6
# )
#
# I1_fixtures_yc$i1_A <- percent(I1_fixtures_yc$i1_A, accuracy = 0.1)
#
# #ov25
# I1_fixtures_yc$i1_ov25 <- (
#   I1_fixtures_yc$i1_2_1 + I1_fixtures_yc$i1_1_2 + I1_fixtures_yc$i1_2_2 + I1_fixtures_yc$i1_3_0 + I1_fixtures_yc$i1_3_1 +
#     I1_fixtures_yc$i1_3_2 + I1_fixtures_yc$i1_0_3 + I1_fixtures_yc$i1_1_3 + I1_fixtures_yc$i1_2_3 + I1_fixtures_yc$i1_3_3 +
#     I1_fixtures_yc$i1_4_0 + I1_fixtures_yc$i1_4_1 + I1_fixtures_yc$i1_4_2 + I1_fixtures_yc$i1_4_3 + I1_fixtures_yc$i1_0_4 +
#     I1_fixtures_yc$i1_1_4 + I1_fixtures_yc$i1_2_4 + I1_fixtures_yc$i1_3_4 + I1_fixtures_yc$i1_4_4 + I1_fixtures_yc$i1_5_0 +
#     I1_fixtures_yc$i1_5_1 + I1_fixtures_yc$i1_5_2 + I1_fixtures_yc$i1_5_3 + I1_fixtures_yc$i1_5_4 + I1_fixtures_yc$i1_0_5 +
#     I1_fixtures_yc$i1_1_5 + I1_fixtures_yc$i1_2_5 + I1_fixtures_yc$i1_3_5 + I1_fixtures_yc$i1_4_5 + I1_fixtures_yc$i1_5_5 +
#     I1_fixtures_yc$i1_6_0 + I1_fixtures_yc$i1_6_1 + I1_fixtures_yc$i1_6_2 + I1_fixtures_yc$i1_6_3 + I1_fixtures_yc$i1_6_4 +
#     I1_fixtures_yc$i1_6_5 + I1_fixtures_yc$i1_0_6 + I1_fixtures_yc$i1_1_6 + I1_fixtures_yc$i1_2_6 + I1_fixtures_yc$i1_3_6 +
#     I1_fixtures_yc$i1_4_6 + I1_fixtures_yc$i1_5_6 + I1_fixtures_yc$i1_6_6
# )
# #un25
# I1_fixtures_yc$i1_un25 <- (
#   I1_fixtures_yc$i1_0_0 + I1_fixtures_yc$i1_1_0 + I1_fixtures_yc$i1_0_1 + I1_fixtures_yc$i1_1_1 + I1_fixtures_yc$i1_2_0 + I1_fixtures_yc$i1_0_2
# )
# #odds
# I1_fixtures_yc$i1_ov25_odds <- round((1/I1_fixtures_yc$i1_ov25),digits = 2)
# I1_fixtures_yc$i1_un25_odds <- round((1/I1_fixtures_yc$i1_un25),digits = 2)
#
# I1_fixtures_yc$i1_ov25_odds
# I1_fixtures_yc$i1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I1_fixtures_yc$i1_ov25 <- percent(I1_fixtures_yc$i1_ov25, accuracy = 0.1)
#
# I1_fixtures_yc$i1_un25 <- percent(I1_fixtures_yc$i1_un25, accuracy = 0.1)
# I1_fixtures_yc$i1_pscore <- paste(round(I1_fixtures_yc$i1_xHYC,digits = 0),round(I1_fixtures_yc$i1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(I1_fixtures,'Divisions/I1.xlsx',sheetName = "I1", append = TRUE)
# ##################MLS
# HomeTeam_i2_yc <- rep(i2_teams, each = length(i2_teams))
# AwayTeam_i2_yc <- rep(i2_teams, length(i2_teams))
# I2_fixtures_yc <- cbind(HomeTeam_i2_yc,AwayTeam_i2_yc)
# I2_fixtures_yc <- as.data.frame(I2_fixtures_yc)
# I2_fixtures_yc <- I2_fixtures_yc[!I2_fixtures_yc$HomeTeam_i2_yc == I2_fixtures_yc$AwayTeam_i2_yc,]
# rownames(I2_fixtures_yc) <- NULL
# I2_fixtures_yc$Div <- "I2"
# I2_fixtures_yc <- I2_fixtures_yc[,c(3,1,2)]
#
# I2_fixtures_yc$avg_HY_i2 <- i2_avg_HY
#
# I2_fixtures_yc$i2_homeyas <- rep(i2_home_yas,each = length(i2_teams)-1)
#
# i2_awayyds_lookup <- cbind(i2_teams,i2_away_yds)
#
# i2_awayyds_lookup <- as.data.frame(i2_awayyds_lookup)
#
# colnames(i2_awayyds_lookup) <- c("AwayTeam_i2_yc","i2_awayyds")
#
#
# require('RH2')
# I2_fixtures_yc$i2_awayyds <- sqldf("SELECT i2_awayyds_lookup.i2_awayyds FROM i2_awayyds_lookup INNER JOIN I2_fixtures_yc ON i2_awayyds_lookup.AwayTeam_i2_yc = I2_fixtures_yc.AwayTeam_i2_yc")
#
# I2_fixtures_yc$avg_AY_i2 <- i2_avg_AY
#
# i2_awayyas_lookup <- cbind(i2_teams,i2_away_yas)
#
# i2_awayyas_lookup <- as.data.frame(i2_awayyas_lookup)
#
# colnames(i2_awayyas_lookup) <- c("AwayTeam_i2_yc","i2_awayyas")
#
# I2_fixtures_yc$i2_awayyas <- sqldf("SELECT i2_awayyas_lookup.i2_awayyas FROM i2_awayyas_lookup INNER JOIN I2_fixtures_yc ON i2_awayyas_lookup.AwayTeam_i2_yc = I2_fixtures_yc.AwayTeam_i2_yc")
#
# I2_fixtures_yc$i2_homeyds <- rep(i2_home_yds,each = length(i2_teams)-1)
#
# I2_fixtures_yc$i2_awayyds <- as.numeric(unlist(I2_fixtures_yc$i2_awayyds))
# #xGH
# I2_fixtures_yc$i2_xHYC <- I2_fixtures_yc$avg_HY_i2 * I2_fixtures_yc$i2_homeyas * I2_fixtures_yc$i2_awayyds
# #xGA
#
# I2_fixtures_yc$i2_awayyas <- as.numeric(unlist(I2_fixtures_yc$i2_awayyas))
#
# I2_fixtures_yc$i2_xAYC <- I2_fixtures_yc$avg_AY_i2 * I2_fixtures_yc$i2_awayyas * I2_fixtures_yc$i2_homeyds
#
# I2_fixtures_yc$i2_0_0 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_0 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_0_1 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_1 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_0 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_0_2 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_2 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_1 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_2 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_3 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_0 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_1 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_2 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_0_3 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_3 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_3 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_4 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_0 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_1 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_2 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_3 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_0_4 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_4 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_4 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_4 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_5 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_0 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_1 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_2 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_3 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_4 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_0_5 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_5 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_5 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_5 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_5 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_6 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_0 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(0,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_1 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(1,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_2 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(2,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_3 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(3,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_4 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(4,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_6_5 <- round(stats::dpois(6,I2_fixtures_yc$i2_xHYC) * stats::dpois(5,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_0_6 <- round(stats::dpois(0,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_1_6 <- round(stats::dpois(1,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_2_6 <- round(stats::dpois(2,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_3_6 <- round(stats::dpois(3,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_4_6 <- round(stats::dpois(4,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# I2_fixtures_yc$i2_5_6 <- round(stats::dpois(5,I2_fixtures_yc$i2_xHYC) * stats::dpois(6,I2_fixtures_yc$i2_xAYC), digits = 4)
# #Home win
# I2_fixtures_yc$i2_H <- (
#   I2_fixtures_yc$i2_1_0 + I2_fixtures_yc$i2_2_0 + I2_fixtures_yc$i2_2_1 + I2_fixtures_yc$i2_3_0 + I2_fixtures_yc$i2_3_1 +
#     I2_fixtures_yc$i2_3_2 + I2_fixtures_yc$i2_4_0 + I2_fixtures_yc$i2_4_1 + I2_fixtures_yc$i2_4_2 + I2_fixtures_yc$i2_4_3 +
#     I2_fixtures_yc$i2_5_0 + I2_fixtures_yc$i2_5_1 + I2_fixtures_yc$i2_5_2 + I2_fixtures_yc$i2_5_3 + I2_fixtures_yc$i2_5_4 +
#     I2_fixtures_yc$i2_6_0 + I2_fixtures_yc$i2_6_1 + I2_fixtures_yc$i2_6_2 + I2_fixtures_yc$i2_6_3 + I2_fixtures_yc$i2_6_4 +
#     I2_fixtures_yc$i2_6_5
# )
#
# I2_fixtures_yc$i2_H <- percent(I2_fixtures_yc$i2_H, accuracy = 0.1)
#
# #Draw
# I2_fixtures_yc$i2_D <- (
#
#   I2_fixtures_yc$i2_0_0 + I2_fixtures_yc$i2_1_1 + I2_fixtures_yc$i2_2_2 + I2_fixtures_yc$i2_3_3 + I2_fixtures_yc$i2_4_4 +
#     I2_fixtures_yc$i2_5_5 + I2_fixtures_yc$i2_6_6
# )
#
# I2_fixtures_yc$i2_D <- percent(I2_fixtures_yc$i2_D, accuracy = 0.1)
#
# #Away
#
# I2_fixtures_yc$i2_A <- (
#   I2_fixtures_yc$i2_0_1 + I2_fixtures_yc$i2_0_2 + I2_fixtures_yc$i2_1_2 + I2_fixtures_yc$i2_0_3 + I2_fixtures_yc$i2_1_3 +
#     I2_fixtures_yc$i2_2_3 + I2_fixtures_yc$i2_0_4 + I2_fixtures_yc$i2_1_4 + I2_fixtures_yc$i2_2_4 + I2_fixtures_yc$i2_3_4 +
#     I2_fixtures_yc$i2_0_5 + I2_fixtures_yc$i2_1_5 + I2_fixtures_yc$i2_2_5 + I2_fixtures_yc$i2_3_5 + I2_fixtures_yc$i2_4_5 +
#     I2_fixtures_yc$i2_0_6 + I2_fixtures_yc$i2_1_6 + I2_fixtures_yc$i2_2_6 + I2_fixtures_yc$i2_3_6 + I2_fixtures_yc$i2_4_6 +
#     I2_fixtures_yc$i2_5_6
# )
#
# I2_fixtures_yc$i2_A <- percent(I2_fixtures_yc$i2_A, accuracy = 0.1)
#
# #ov25
# I2_fixtures_yc$i2_ov25 <- (
#   I2_fixtures_yc$i2_2_1 + I2_fixtures_yc$i2_1_2 + I2_fixtures_yc$i2_2_2 + I2_fixtures_yc$i2_3_0 + I2_fixtures_yc$i2_3_1 +
#     I2_fixtures_yc$i2_3_2 + I2_fixtures_yc$i2_0_3 + I2_fixtures_yc$i2_1_3 + I2_fixtures_yc$i2_2_3 + I2_fixtures_yc$i2_3_3 +
#     I2_fixtures_yc$i2_4_0 + I2_fixtures_yc$i2_4_1 + I2_fixtures_yc$i2_4_2 + I2_fixtures_yc$i2_4_3 + I2_fixtures_yc$i2_0_4 +
#     I2_fixtures_yc$i2_1_4 + I2_fixtures_yc$i2_2_4 + I2_fixtures_yc$i2_3_4 + I2_fixtures_yc$i2_4_4 + I2_fixtures_yc$i2_5_0 +
#     I2_fixtures_yc$i2_5_1 + I2_fixtures_yc$i2_5_2 + I2_fixtures_yc$i2_5_3 + I2_fixtures_yc$i2_5_4 + I2_fixtures_yc$i2_0_5 +
#     I2_fixtures_yc$i2_1_5 + I2_fixtures_yc$i2_2_5 + I2_fixtures_yc$i2_3_5 + I2_fixtures_yc$i2_4_5 + I2_fixtures_yc$i2_5_5 +
#     I2_fixtures_yc$i2_6_0 + I2_fixtures_yc$i2_6_1 + I2_fixtures_yc$i2_6_2 + I2_fixtures_yc$i2_6_3 + I2_fixtures_yc$i2_6_4 +
#     I2_fixtures_yc$i2_6_5 + I2_fixtures_yc$i2_0_6 + I2_fixtures_yc$i2_1_6 + I2_fixtures_yc$i2_2_6 + I2_fixtures_yc$i2_3_6 +
#     I2_fixtures_yc$i2_4_6 + I2_fixtures_yc$i2_5_6 + I2_fixtures_yc$i2_6_6
# )
# #un25
# I2_fixtures_yc$i2_un25 <- (
#   I2_fixtures_yc$i2_0_0 + I2_fixtures_yc$i2_1_0 + I2_fixtures_yc$i2_0_1 + I2_fixtures_yc$i2_1_1 + I2_fixtures_yc$i2_2_0 + I2_fixtures_yc$i2_0_2
# )
# #odds
# I2_fixtures_yc$i2_ov25_odds <- round((1/I2_fixtures_yc$i2_ov25),digits = 2)
# I2_fixtures_yc$i2_un25_odds <- round((1/I2_fixtures_yc$i2_un25),digits = 2)
#
# I2_fixtures_yc$i2_ov25_odds
# I2_fixtures_yc$i2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I2_fixtures_yc$i2_ov25 <- percent(I2_fixtures_yc$i2_ov25, accuracy = 0.1)
#
# I2_fixtures_yc$i2_un25 <- percent(I2_fixtures_yc$i2_un25, accuracy = 0.1)
# I2_fixtures_yc$i2_pscore <- paste(round(I2_fixtures_yc$i2_xHYC,digits = 0),round(I2_fixtures_yc$i2_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################################################################################################################
# #N1
# HomeTeam_n1_yc <- rep(n1_teams, each = length(n1_teams))
# AwayTeam_n1_yc <- rep(n1_teams, length(n1_teams))
# N1_fixtures_yc <- cbind(HomeTeam_n1_yc,AwayTeam_n1_yc)
# N1_fixtures_yc <- as.data.frame(N1_fixtures_yc)
# N1_fixtures_yc <- N1_fixtures_yc[!N1_fixtures_yc$HomeTeam_n1_yc == N1_fixtures_yc$AwayTeam_n1_yc,]
# rownames(N1_fixtures_yc) <- NULL
# N1_fixtures_yc$Div <- "N1"
# N1_fixtures_yc <- N1_fixtures_yc[,c(3,1,2)]
#
# N1_fixtures_yc$avg_HY_n1 <- n1_avg_HY
#
# N1_fixtures_yc$n1_homeyas <- rep(n1_home_yas,each = length(n1_teams)-1)
#
# n1_awayyds_lookup <- cbind(n1_teams,n1_away_yds)
#
# n1_awayyds_lookup <- as.data.frame(n1_awayyds_lookup)
#
# colnames(n1_awayyds_lookup) <- c("AwayTeam_n1_yc","n1_awayyds")
#
#
# require('RH2')
# N1_fixtures_yc$n1_awayyds <- sqldf("SELECT n1_awayyds_lookup.n1_awayyds FROM n1_awayyds_lookup INNER JOIN N1_fixtures_yc ON n1_awayyds_lookup.AwayTeam_n1_yc = N1_fixtures_yc.AwayTeam_n1_yc")
#
# N1_fixtures_yc$avg_AY_n1 <- n1_avg_AY
#
# n1_awayyas_lookup <- cbind(n1_teams,n1_away_yas)
#
# n1_awayyas_lookup <- as.data.frame(n1_awayyas_lookup)
#
# colnames(n1_awayyas_lookup) <- c("AwayTeam_n1_yc","n1_awayyas")
#
# N1_fixtures_yc$n1_awayyas <- sqldf("SELECT n1_awayyas_lookup.n1_awayyas FROM n1_awayyas_lookup INNER JOIN N1_fixtures_yc ON n1_awayyas_lookup.AwayTeam_n1_yc = N1_fixtures_yc.AwayTeam_n1_yc")
#
# N1_fixtures_yc$n1_homeyds <- rep(n1_home_yds,each = length(n1_teams)-1)
#
# N1_fixtures_yc$n1_awayyds <- as.numeric(unlist(N1_fixtures_yc$n1_awayyds))
# #xGH
# N1_fixtures_yc$n1_xHYC <- N1_fixtures_yc$avg_HY_n1 * N1_fixtures_yc$n1_homeyas * N1_fixtures_yc$n1_awayyds
# #xGA
#
# N1_fixtures_yc$n1_awayyas <- as.numeric(unlist(N1_fixtures_yc$n1_awayyas))
#
# N1_fixtures_yc$n1_xAYC <- N1_fixtures_yc$avg_AY_n1 * N1_fixtures_yc$n1_awayyas * N1_fixtures_yc$n1_homeyds
#
# N1_fixtures_yc$n1_0_0 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_0 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_0_1 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_1 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_0 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_0_2 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_2 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_1 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_2 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_3 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_0 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_1 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_2 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_0_3 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_3 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_3 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_4 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_0 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_1 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_2 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_3 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_0_4 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_4 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_4 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_4 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_5 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_0 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_1 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_2 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_3 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_4 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_0_5 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_5 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_5 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_5 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_5 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_6 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_0 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(0,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_1 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(1,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_2 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(2,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_3 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(3,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_4 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(4,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_6_5 <- round(stats::dpois(6,N1_fixtures_yc$n1_xHYC) * stats::dpois(5,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_0_6 <- round(stats::dpois(0,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_1_6 <- round(stats::dpois(1,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_2_6 <- round(stats::dpois(2,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_3_6 <- round(stats::dpois(3,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_4_6 <- round(stats::dpois(4,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# N1_fixtures_yc$n1_5_6 <- round(stats::dpois(5,N1_fixtures_yc$n1_xHYC) * stats::dpois(6,N1_fixtures_yc$n1_xAYC), digits = 4)
# #Home win
# N1_fixtures_yc$n1_H <- (
#   N1_fixtures_yc$n1_1_0 + N1_fixtures_yc$n1_2_0 + N1_fixtures_yc$n1_2_1 + N1_fixtures_yc$n1_3_0 + N1_fixtures_yc$n1_3_1 +
#     N1_fixtures_yc$n1_3_2 + N1_fixtures_yc$n1_4_0 + N1_fixtures_yc$n1_4_1 + N1_fixtures_yc$n1_4_2 + N1_fixtures_yc$n1_4_3 +
#     N1_fixtures_yc$n1_5_0 + N1_fixtures_yc$n1_5_1 + N1_fixtures_yc$n1_5_2 + N1_fixtures_yc$n1_5_3 + N1_fixtures_yc$n1_5_4 +
#     N1_fixtures_yc$n1_6_0 + N1_fixtures_yc$n1_6_1 + N1_fixtures_yc$n1_6_2 + N1_fixtures_yc$n1_6_3 + N1_fixtures_yc$n1_6_4 +
#     N1_fixtures_yc$n1_6_5
# )
#
# N1_fixtures_yc$n1_H <- percent(N1_fixtures_yc$n1_H, accuracy = 0.1)
#
# #Draw
# N1_fixtures_yc$n1_D <- (
#
#   N1_fixtures_yc$n1_0_0 + N1_fixtures_yc$n1_1_1 + N1_fixtures_yc$n1_2_2 + N1_fixtures_yc$n1_3_3 + N1_fixtures_yc$n1_4_4 +
#     N1_fixtures_yc$n1_5_5 + N1_fixtures_yc$n1_6_6
# )
#
# N1_fixtures_yc$n1_D <- percent(N1_fixtures_yc$n1_D, accuracy = 0.1)
#
# #Away
#
# N1_fixtures_yc$n1_A <- (
#   N1_fixtures_yc$n1_0_1 + N1_fixtures_yc$n1_0_2 + N1_fixtures_yc$n1_1_2 + N1_fixtures_yc$n1_0_3 + N1_fixtures_yc$n1_1_3 +
#     N1_fixtures_yc$n1_2_3 + N1_fixtures_yc$n1_0_4 + N1_fixtures_yc$n1_1_4 + N1_fixtures_yc$n1_2_4 + N1_fixtures_yc$n1_3_4 +
#     N1_fixtures_yc$n1_0_5 + N1_fixtures_yc$n1_1_5 + N1_fixtures_yc$n1_2_5 + N1_fixtures_yc$n1_3_5 + N1_fixtures_yc$n1_4_5 +
#     N1_fixtures_yc$n1_0_6 + N1_fixtures_yc$n1_1_6 + N1_fixtures_yc$n1_2_6 + N1_fixtures_yc$n1_3_6 + N1_fixtures_yc$n1_4_6 +
#     N1_fixtures_yc$n1_5_6
# )
#
# N1_fixtures_yc$n1_A <- percent(N1_fixtures_yc$n1_A, accuracy = 0.1)
#
# #ov25
# N1_fixtures_yc$n1_ov25 <- (
#   N1_fixtures_yc$n1_2_1 + N1_fixtures_yc$n1_1_2 + N1_fixtures_yc$n1_2_2 + N1_fixtures_yc$n1_3_0 + N1_fixtures_yc$n1_3_1 +
#     N1_fixtures_yc$n1_3_2 + N1_fixtures_yc$n1_0_3 + N1_fixtures_yc$n1_1_3 + N1_fixtures_yc$n1_2_3 + N1_fixtures_yc$n1_3_3 +
#     N1_fixtures_yc$n1_4_0 + N1_fixtures_yc$n1_4_1 + N1_fixtures_yc$n1_4_2 + N1_fixtures_yc$n1_4_3 + N1_fixtures_yc$n1_0_4 +
#     N1_fixtures_yc$n1_1_4 + N1_fixtures_yc$n1_2_4 + N1_fixtures_yc$n1_3_4 + N1_fixtures_yc$n1_4_4 + N1_fixtures_yc$n1_5_0 +
#     N1_fixtures_yc$n1_5_1 + N1_fixtures_yc$n1_5_2 + N1_fixtures_yc$n1_5_3 + N1_fixtures_yc$n1_5_4 + N1_fixtures_yc$n1_0_5 +
#     N1_fixtures_yc$n1_1_5 + N1_fixtures_yc$n1_2_5 + N1_fixtures_yc$n1_3_5 + N1_fixtures_yc$n1_4_5 + N1_fixtures_yc$n1_5_5 +
#     N1_fixtures_yc$n1_6_0 + N1_fixtures_yc$n1_6_1 + N1_fixtures_yc$n1_6_2 + N1_fixtures_yc$n1_6_3 + N1_fixtures_yc$n1_6_4 +
#     N1_fixtures_yc$n1_6_5 + N1_fixtures_yc$n1_0_6 + N1_fixtures_yc$n1_1_6 + N1_fixtures_yc$n1_2_6 + N1_fixtures_yc$n1_3_6 +
#     N1_fixtures_yc$n1_4_6 + N1_fixtures_yc$n1_5_6 + N1_fixtures_yc$n1_6_6
# )
# #un25
# N1_fixtures_yc$n1_un25 <- (
#   N1_fixtures_yc$n1_0_0 + N1_fixtures_yc$n1_1_0 + N1_fixtures_yc$n1_0_1 + N1_fixtures_yc$n1_1_1 + N1_fixtures_yc$n1_2_0 + N1_fixtures_yc$n1_0_2
# )
# #odds
# N1_fixtures_yc$n1_ov25_odds <- round((1/N1_fixtures_yc$n1_ov25),digits = 2)
# N1_fixtures_yc$n1_un25_odds <- round((1/N1_fixtures_yc$n1_un25),digits = 2)
#
# N1_fixtures_yc$n1_ov25_odds
# N1_fixtures_yc$n1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# N1_fixtures_yc$n1_ov25 <- percent(N1_fixtures_yc$n1_ov25, accuracy = 0.1)
#
# N1_fixtures_yc$n1_un25 <- percent(N1_fixtures_yc$n1_un25, accuracy = 0.1)
# N1_fixtures_yc$n1_pscore <- paste(round(N1_fixtures_yc$n1_xHYC,digits = 0),round(N1_fixtures_yc$n1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(N1_fixtures,'Divisions/N1.xlsx',sheetName = "N1", append = TRUE)
# #################################################################################################################
# #P1
# HomeTeam_p1_yc <- rep(p1_teams, each = length(p1_teams))
# AwayTeam_p1_yc <- rep(p1_teams, length(p1_teams))
# P1_fixtures_yc <- cbind(HomeTeam_p1_yc,AwayTeam_p1_yc)
# P1_fixtures_yc <- as.data.frame(P1_fixtures_yc)
# P1_fixtures_yc <- P1_fixtures_yc[!P1_fixtures_yc$HomeTeam_p1_yc == P1_fixtures_yc$AwayTeam_p1_yc,]
# rownames(P1_fixtures_yc) <- NULL
# P1_fixtures_yc$Div <- "P1"
# P1_fixtures_yc <- P1_fixtures_yc[,c(3,1,2)]
#
# P1_fixtures_yc$avg_HY_p1 <- p1_avg_HY
#
# P1_fixtures_yc$p1_homeyas <- rep(p1_home_yas,each = length(p1_teams)-1)
#
# p1_awayyds_lookup <- cbind(p1_teams,p1_away_yds)
#
# p1_awayyds_lookup <- as.data.frame(p1_awayyds_lookup)
#
# colnames(p1_awayyds_lookup) <- c("AwayTeam_p1_yc","p1_awayyds")
#
#
# require('RH2')
# P1_fixtures_yc$p1_awayyds <- sqldf("SELECT p1_awayyds_lookup.p1_awayyds FROM p1_awayyds_lookup INNER JOIN P1_fixtures_yc ON p1_awayyds_lookup.AwayTeam_p1_yc = P1_fixtures_yc.AwayTeam_p1_yc")
#
# P1_fixtures_yc$avg_AY_p1 <- p1_avg_AY
#
# p1_awayyas_lookup <- cbind(p1_teams,p1_away_yas)
#
# p1_awayyas_lookup <- as.data.frame(p1_awayyas_lookup)
#
# colnames(p1_awayyas_lookup) <- c("AwayTeam_p1_yc","p1_awayyas")
#
# P1_fixtures_yc$p1_awayyas <- sqldf("SELECT p1_awayyas_lookup.p1_awayyas FROM p1_awayyas_lookup INNER JOIN P1_fixtures_yc ON p1_awayyas_lookup.AwayTeam_p1_yc = P1_fixtures_yc.AwayTeam_p1_yc")
#
# P1_fixtures_yc$p1_homeyds <- rep(p1_home_yds,each = length(p1_teams)-1)
#
# P1_fixtures_yc$p1_awayyds <- as.numeric(unlist(P1_fixtures_yc$p1_awayyds))
# #xGH
# P1_fixtures_yc$p1_xHYC <- P1_fixtures_yc$avg_HY_p1 * P1_fixtures_yc$p1_homeyas * P1_fixtures_yc$p1_awayyds
# #xGA
#
# P1_fixtures_yc$p1_awayyas <- as.numeric(unlist(P1_fixtures_yc$p1_awayyas))
#
# P1_fixtures_yc$p1_xAYC <- P1_fixtures_yc$avg_AY_p1 * P1_fixtures_yc$p1_awayyas * P1_fixtures_yc$p1_homeyds
#
# P1_fixtures_yc$p1_0_0 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_0 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_0_1 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_1 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_0 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_0_2 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_2 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_1 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_2 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_3 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_0 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_1 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_2 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_0_3 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_3 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_3 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_4 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_0 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_1 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_2 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_3 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_0_4 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_4 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_4 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_4 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_5 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_0 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_1 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_2 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_3 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_4 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_0_5 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_5 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_5 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_5 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_5 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_6 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_0 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(0,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_1 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(1,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_2 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(2,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_3 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(3,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_4 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(4,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_6_5 <- round(stats::dpois(6,P1_fixtures_yc$p1_xHYC) * stats::dpois(5,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_0_6 <- round(stats::dpois(0,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_1_6 <- round(stats::dpois(1,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_2_6 <- round(stats::dpois(2,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_3_6 <- round(stats::dpois(3,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_4_6 <- round(stats::dpois(4,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# P1_fixtures_yc$p1_5_6 <- round(stats::dpois(5,P1_fixtures_yc$p1_xHYC) * stats::dpois(6,P1_fixtures_yc$p1_xAYC), digits = 4)
# #Home win
# P1_fixtures_yc$p1_H <- (
#   P1_fixtures_yc$p1_1_0 + P1_fixtures_yc$p1_2_0 + P1_fixtures_yc$p1_2_1 + P1_fixtures_yc$p1_3_0 + P1_fixtures_yc$p1_3_1 +
#     P1_fixtures_yc$p1_3_2 + P1_fixtures_yc$p1_4_0 + P1_fixtures_yc$p1_4_1 + P1_fixtures_yc$p1_4_2 + P1_fixtures_yc$p1_4_3 +
#     P1_fixtures_yc$p1_5_0 + P1_fixtures_yc$p1_5_1 + P1_fixtures_yc$p1_5_2 + P1_fixtures_yc$p1_5_3 + P1_fixtures_yc$p1_5_4 +
#     P1_fixtures_yc$p1_6_0 + P1_fixtures_yc$p1_6_1 + P1_fixtures_yc$p1_6_2 + P1_fixtures_yc$p1_6_3 + P1_fixtures_yc$p1_6_4 +
#     P1_fixtures_yc$p1_6_5
# )
#
# P1_fixtures_yc$p1_H <- percent(P1_fixtures_yc$p1_H, accuracy = 0.1)
#
# #Draw
# P1_fixtures_yc$p1_D <- (
#
#   P1_fixtures_yc$p1_0_0 + P1_fixtures_yc$p1_1_1 + P1_fixtures_yc$p1_2_2 + P1_fixtures_yc$p1_3_3 + P1_fixtures_yc$p1_4_4 +
#     P1_fixtures_yc$p1_5_5 + P1_fixtures_yc$p1_6_6
# )
#
# P1_fixtures_yc$p1_D <- percent(P1_fixtures_yc$p1_D, accuracy = 0.1)
#
# #Away
#
# P1_fixtures_yc$p1_A <- (
#   P1_fixtures_yc$p1_0_1 + P1_fixtures_yc$p1_0_2 + P1_fixtures_yc$p1_1_2 + P1_fixtures_yc$p1_0_3 + P1_fixtures_yc$p1_1_3 +
#     P1_fixtures_yc$p1_2_3 + P1_fixtures_yc$p1_0_4 + P1_fixtures_yc$p1_1_4 + P1_fixtures_yc$p1_2_4 + P1_fixtures_yc$p1_3_4 +
#     P1_fixtures_yc$p1_0_5 + P1_fixtures_yc$p1_1_5 + P1_fixtures_yc$p1_2_5 + P1_fixtures_yc$p1_3_5 + P1_fixtures_yc$p1_4_5 +
#     P1_fixtures_yc$p1_0_6 + P1_fixtures_yc$p1_1_6 + P1_fixtures_yc$p1_2_6 + P1_fixtures_yc$p1_3_6 + P1_fixtures_yc$p1_4_6 +
#     P1_fixtures_yc$p1_5_6
# )
#
# P1_fixtures_yc$p1_A <- percent(P1_fixtures_yc$p1_A, accuracy = 0.1)
#
# #ov25
# P1_fixtures_yc$p1_ov25 <- (
#   P1_fixtures_yc$p1_2_1 + P1_fixtures_yc$p1_1_2 + P1_fixtures_yc$p1_2_2 + P1_fixtures_yc$p1_3_0 + P1_fixtures_yc$p1_3_1 +
#     P1_fixtures_yc$p1_3_2 + P1_fixtures_yc$p1_0_3 + P1_fixtures_yc$p1_1_3 + P1_fixtures_yc$p1_2_3 + P1_fixtures_yc$p1_3_3 +
#     P1_fixtures_yc$p1_4_0 + P1_fixtures_yc$p1_4_1 + P1_fixtures_yc$p1_4_2 + P1_fixtures_yc$p1_4_3 + P1_fixtures_yc$p1_0_4 +
#     P1_fixtures_yc$p1_1_4 + P1_fixtures_yc$p1_2_4 + P1_fixtures_yc$p1_3_4 + P1_fixtures_yc$p1_4_4 + P1_fixtures_yc$p1_5_0 +
#     P1_fixtures_yc$p1_5_1 + P1_fixtures_yc$p1_5_2 + P1_fixtures_yc$p1_5_3 + P1_fixtures_yc$p1_5_4 + P1_fixtures_yc$p1_0_5 +
#     P1_fixtures_yc$p1_1_5 + P1_fixtures_yc$p1_2_5 + P1_fixtures_yc$p1_3_5 + P1_fixtures_yc$p1_4_5 + P1_fixtures_yc$p1_5_5 +
#     P1_fixtures_yc$p1_6_0 + P1_fixtures_yc$p1_6_1 + P1_fixtures_yc$p1_6_2 + P1_fixtures_yc$p1_6_3 + P1_fixtures_yc$p1_6_4 +
#     P1_fixtures_yc$p1_6_5 + P1_fixtures_yc$p1_0_6 + P1_fixtures_yc$p1_1_6 + P1_fixtures_yc$p1_2_6 + P1_fixtures_yc$p1_3_6 +
#     P1_fixtures_yc$p1_4_6 + P1_fixtures_yc$p1_5_6 + P1_fixtures_yc$p1_6_6
# )
# #un25
# P1_fixtures_yc$p1_un25 <- (
#   P1_fixtures_yc$p1_0_0 + P1_fixtures_yc$p1_1_0 + P1_fixtures_yc$p1_0_1 + P1_fixtures_yc$p1_1_1 + P1_fixtures_yc$p1_2_0 + P1_fixtures_yc$p1_0_2
# )
# #odds
# P1_fixtures_yc$p1_ov25_odds <- round((1/P1_fixtures_yc$p1_ov25),digits = 2)
# P1_fixtures_yc$p1_un25_odds <- round((1/P1_fixtures_yc$p1_un25),digits = 2)
#
# P1_fixtures_yc$p1_ov25_odds
# P1_fixtures_yc$p1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# P1_fixtures_yc$p1_ov25 <- percent(P1_fixtures_yc$p1_ov25, accuracy = 0.1)
#
# P1_fixtures_yc$p1_un25 <- percent(P1_fixtures_yc$p1_un25, accuracy = 0.1)
# P1_fixtures_yc$p1_pscore <- paste(round(P1_fixtures_yc$p1_xHYC,digits = 0),round(P1_fixtures_yc$p1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(P1_fixtures,'Divisions/P1.xlsx',sheetName = "P1", append = TRUE)
# #################################################################################################################
# #SC0
# HomeTeam_sc0_yc <- rep(sc0_teams, each = length(sc0_teams))
# AwayTeam_sc0_yc <- rep(sc0_teams, length(sc0_teams))
# SC0_fixtures_yc <- cbind(HomeTeam_sc0_yc,AwayTeam_sc0_yc)
# SC0_fixtures_yc <- as.data.frame(SC0_fixtures_yc)
# SC0_fixtures_yc <- SC0_fixtures_yc[!SC0_fixtures_yc$HomeTeam_sc0_yc == SC0_fixtures_yc$AwayTeam_sc0_yc,]
# rownames(SC0_fixtures_yc) <- NULL
# SC0_fixtures_yc$Div <- "SC0"
# SC0_fixtures_yc <- SC0_fixtures_yc[,c(3,1,2)]
#
# SC0_fixtures_yc$avg_HY_sc0 <- sc0_avg_HY
#
# SC0_fixtures_yc$sc0_homeyas <- rep(sc0_home_yas,each = length(sc0_teams)-1)
#
# sc0_awayyds_lookup <- cbind(sc0_teams,sc0_away_yds)
#
# sc0_awayyds_lookup <- as.data.frame(sc0_awayyds_lookup)
#
# colnames(sc0_awayyds_lookup) <- c("AwayTeam_sc0_yc","sc0_awayyds")
#
#
# require('RH2')
# SC0_fixtures_yc$sc0_awayyds <- sqldf("SELECT sc0_awayyds_lookup.sc0_awayyds FROM sc0_awayyds_lookup INNER JOIN SC0_fixtures_yc ON sc0_awayyds_lookup.AwayTeam_sc0_yc = SC0_fixtures_yc.AwayTeam_sc0_yc")
#
# SC0_fixtures_yc$avg_AY_sc0 <- sc0_avg_AY
#
# sc0_awayyas_lookup <- cbind(sc0_teams,sc0_away_yas)
#
# sc0_awayyas_lookup <- as.data.frame(sc0_awayyas_lookup)
#
# colnames(sc0_awayyas_lookup) <- c("AwayTeam_sc0_yc","sc0_awayyas")
#
# SC0_fixtures_yc$sc0_awayyas <- sqldf("SELECT sc0_awayyas_lookup.sc0_awayyas FROM sc0_awayyas_lookup INNER JOIN SC0_fixtures_yc ON sc0_awayyas_lookup.AwayTeam_sc0_yc = SC0_fixtures_yc.AwayTeam_sc0_yc")
#
# SC0_fixtures_yc$sc0_homeyds <- rep(sc0_home_yds,each = length(sc0_teams)-1)
#
# SC0_fixtures_yc$sc0_awayyds <- as.numeric(unlist(SC0_fixtures_yc$sc0_awayyds))
# #xGH
# SC0_fixtures_yc$sc0_xHYC <- SC0_fixtures_yc$avg_HY_sc0 * SC0_fixtures_yc$sc0_homeyas * SC0_fixtures_yc$sc0_awayyds
# #xGA
#
# SC0_fixtures_yc$sc0_awayyas <- as.numeric(unlist(SC0_fixtures_yc$sc0_awayyas))
#
# SC0_fixtures_yc$sc0_xAYC <- SC0_fixtures_yc$avg_AY_sc0 * SC0_fixtures_yc$sc0_awayyas * SC0_fixtures_yc$sc0_homeyds
#
# SC0_fixtures_yc$sc0_0_0 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_0 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_0_1 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_1 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_0 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_0_2 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_2 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_1 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_2 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_3 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_0 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_1 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_2 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_0_3 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_3 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_3 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_4 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_0 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_1 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_2 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_3 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_0_4 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_4 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_4 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_4 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_5 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_0 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_1 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_2 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_3 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_4 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_0_5 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_5 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_5 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_5 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_5 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_6 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_0 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(0,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_1 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(1,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_2 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(2,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_3 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(3,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_4 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(4,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_6_5 <- round(stats::dpois(6,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(5,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_0_6 <- round(stats::dpois(0,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_1_6 <- round(stats::dpois(1,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_2_6 <- round(stats::dpois(2,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_3_6 <- round(stats::dpois(3,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_4_6 <- round(stats::dpois(4,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# SC0_fixtures_yc$sc0_5_6 <- round(stats::dpois(5,SC0_fixtures_yc$sc0_xHYC) * stats::dpois(6,SC0_fixtures_yc$sc0_xAYC), digits = 4)
# #Home win
# SC0_fixtures_yc$sc0_H <- (
#   SC0_fixtures_yc$sc0_1_0 + SC0_fixtures_yc$sc0_2_0 + SC0_fixtures_yc$sc0_2_1 + SC0_fixtures_yc$sc0_3_0 + SC0_fixtures_yc$sc0_3_1 +
#     SC0_fixtures_yc$sc0_3_2 + SC0_fixtures_yc$sc0_4_0 + SC0_fixtures_yc$sc0_4_1 + SC0_fixtures_yc$sc0_4_2 + SC0_fixtures_yc$sc0_4_3 +
#     SC0_fixtures_yc$sc0_5_0 + SC0_fixtures_yc$sc0_5_1 + SC0_fixtures_yc$sc0_5_2 + SC0_fixtures_yc$sc0_5_3 + SC0_fixtures_yc$sc0_5_4 +
#     SC0_fixtures_yc$sc0_6_0 + SC0_fixtures_yc$sc0_6_1 + SC0_fixtures_yc$sc0_6_2 + SC0_fixtures_yc$sc0_6_3 + SC0_fixtures_yc$sc0_6_4 +
#     SC0_fixtures_yc$sc0_6_5
# )
#
# SC0_fixtures_yc$sc0_H <- percent(SC0_fixtures_yc$sc0_H, accuracy = 0.1)
#
# #Draw
# SC0_fixtures_yc$sc0_D <- (
#
#   SC0_fixtures_yc$sc0_0_0 + SC0_fixtures_yc$sc0_1_1 + SC0_fixtures_yc$sc0_2_2 + SC0_fixtures_yc$sc0_3_3 + SC0_fixtures_yc$sc0_4_4 +
#     SC0_fixtures_yc$sc0_5_5 + SC0_fixtures_yc$sc0_6_6
# )
#
# SC0_fixtures_yc$sc0_D <- percent(SC0_fixtures_yc$sc0_D, accuracy = 0.1)
#
# #Away
#
# SC0_fixtures_yc$sc0_A <- (
#   SC0_fixtures_yc$sc0_0_1 + SC0_fixtures_yc$sc0_0_2 + SC0_fixtures_yc$sc0_1_2 + SC0_fixtures_yc$sc0_0_3 + SC0_fixtures_yc$sc0_1_3 +
#     SC0_fixtures_yc$sc0_2_3 + SC0_fixtures_yc$sc0_0_4 + SC0_fixtures_yc$sc0_1_4 + SC0_fixtures_yc$sc0_2_4 + SC0_fixtures_yc$sc0_3_4 +
#     SC0_fixtures_yc$sc0_0_5 + SC0_fixtures_yc$sc0_1_5 + SC0_fixtures_yc$sc0_2_5 + SC0_fixtures_yc$sc0_3_5 + SC0_fixtures_yc$sc0_4_5 +
#     SC0_fixtures_yc$sc0_0_6 + SC0_fixtures_yc$sc0_1_6 + SC0_fixtures_yc$sc0_2_6 + SC0_fixtures_yc$sc0_3_6 + SC0_fixtures_yc$sc0_4_6 +
#     SC0_fixtures_yc$sc0_5_6
# )
#
# SC0_fixtures_yc$sc0_A <- percent(SC0_fixtures_yc$sc0_A, accuracy = 0.1)
#
# #ov25
# SC0_fixtures_yc$sc0_ov25 <- (
#   SC0_fixtures_yc$sc0_2_1 + SC0_fixtures_yc$sc0_1_2 + SC0_fixtures_yc$sc0_2_2 + SC0_fixtures_yc$sc0_3_0 + SC0_fixtures_yc$sc0_3_1 +
#     SC0_fixtures_yc$sc0_3_2 + SC0_fixtures_yc$sc0_0_3 + SC0_fixtures_yc$sc0_1_3 + SC0_fixtures_yc$sc0_2_3 + SC0_fixtures_yc$sc0_3_3 +
#     SC0_fixtures_yc$sc0_4_0 + SC0_fixtures_yc$sc0_4_1 + SC0_fixtures_yc$sc0_4_2 + SC0_fixtures_yc$sc0_4_3 + SC0_fixtures_yc$sc0_0_4 +
#     SC0_fixtures_yc$sc0_1_4 + SC0_fixtures_yc$sc0_2_4 + SC0_fixtures_yc$sc0_3_4 + SC0_fixtures_yc$sc0_4_4 + SC0_fixtures_yc$sc0_5_0 +
#     SC0_fixtures_yc$sc0_5_1 + SC0_fixtures_yc$sc0_5_2 + SC0_fixtures_yc$sc0_5_3 + SC0_fixtures_yc$sc0_5_4 + SC0_fixtures_yc$sc0_0_5 +
#     SC0_fixtures_yc$sc0_1_5 + SC0_fixtures_yc$sc0_2_5 + SC0_fixtures_yc$sc0_3_5 + SC0_fixtures_yc$sc0_4_5 + SC0_fixtures_yc$sc0_5_5 +
#     SC0_fixtures_yc$sc0_6_0 + SC0_fixtures_yc$sc0_6_1 + SC0_fixtures_yc$sc0_6_2 + SC0_fixtures_yc$sc0_6_3 + SC0_fixtures_yc$sc0_6_4 +
#     SC0_fixtures_yc$sc0_6_5 + SC0_fixtures_yc$sc0_0_6 + SC0_fixtures_yc$sc0_1_6 + SC0_fixtures_yc$sc0_2_6 + SC0_fixtures_yc$sc0_3_6 +
#     SC0_fixtures_yc$sc0_4_6 + SC0_fixtures_yc$sc0_5_6 + SC0_fixtures_yc$sc0_6_6
# )
# #un25
# SC0_fixtures_yc$sc0_un25 <- (
#   SC0_fixtures_yc$sc0_0_0 + SC0_fixtures_yc$sc0_1_0 + SC0_fixtures_yc$sc0_0_1 + SC0_fixtures_yc$sc0_1_1 + SC0_fixtures_yc$sc0_2_0 + SC0_fixtures_yc$sc0_0_2
# )
# #odds
# SC0_fixtures_yc$sc0_ov25_odds <- round((1/SC0_fixtures_yc$sc0_ov25),digits = 2)
# SC0_fixtures_yc$sc0_un25_odds <- round((1/SC0_fixtures_yc$sc0_un25),digits = 2)
#
# SC0_fixtures_yc$sc0_ov25_odds
# SC0_fixtures_yc$sc0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC0_fixtures_yc$sc0_ov25 <- percent(SC0_fixtures_yc$sc0_ov25, accuracy = 0.1)
#
# SC0_fixtures_yc$sc0_un25 <- percent(SC0_fixtures_yc$sc0_un25, accuracy = 0.1)
# SC0_fixtures_yc$sc0_pscore <- paste(round(SC0_fixtures_yc$sc0_xHYC,digits = 0),round(SC0_fixtures_yc$sc0_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SC0_fixtures,'Divisions/SC0.xlsx',sheetName = "SC0", append = TRUE)
# #################################################################################################################
# #SC1
# HomeTeam_sc1_yc <- rep(sc1_teams, each = length(sc1_teams))
# AwayTeam_sc1_yc <- rep(sc1_teams, length(sc1_teams))
# SC1_fixtures_yc <- cbind(HomeTeam_sc1_yc,AwayTeam_sc1_yc)
# SC1_fixtures_yc <- as.data.frame(SC1_fixtures_yc)
# SC1_fixtures_yc <- SC1_fixtures_yc[!SC1_fixtures_yc$HomeTeam_sc1_yc == SC1_fixtures_yc$AwayTeam_sc1_yc,]
# rownames(SC1_fixtures_yc) <- NULL
# SC1_fixtures_yc$Div <- "SC1"
# SC1_fixtures_yc <- SC1_fixtures_yc[,c(3,1,2)]
#
# SC1_fixtures_yc$avg_HY_sc1 <- sc1_avg_HY
#
# SC1_fixtures_yc$sc1_homeyas <- rep(sc1_home_yas,each = length(sc1_teams)-1)
#
# sc1_awayyds_lookup <- cbind(sc1_teams,sc1_away_yds)
#
# sc1_awayyds_lookup <- as.data.frame(sc1_awayyds_lookup)
#
# colnames(sc1_awayyds_lookup) <- c("AwayTeam_sc1_yc","sc1_awayyds")
#
#
# require('RH2')
# SC1_fixtures_yc$sc1_awayyds <- sqldf("SELECT sc1_awayyds_lookup.sc1_awayyds FROM sc1_awayyds_lookup INNER JOIN SC1_fixtures_yc ON sc1_awayyds_lookup.AwayTeam_sc1_yc = SC1_fixtures_yc.AwayTeam_sc1_yc")
#
# SC1_fixtures_yc$avg_AY_sc1 <- sc1_avg_AY
#
# sc1_awayyas_lookup <- cbind(sc1_teams,sc1_away_yas)
#
# sc1_awayyas_lookup <- as.data.frame(sc1_awayyas_lookup)
#
# colnames(sc1_awayyas_lookup) <- c("AwayTeam_sc1_yc","sc1_awayyas")
#
# SC1_fixtures_yc$sc1_awayyas <- sqldf("SELECT sc1_awayyas_lookup.sc1_awayyas FROM sc1_awayyas_lookup INNER JOIN SC1_fixtures_yc ON sc1_awayyas_lookup.AwayTeam_sc1_yc = SC1_fixtures_yc.AwayTeam_sc1_yc")
#
# SC1_fixtures_yc$sc1_homeyds <- rep(sc1_home_yds,each = length(sc1_teams)-1)
#
# SC1_fixtures_yc$sc1_awayyds <- as.numeric(unlist(SC1_fixtures_yc$sc1_awayyds))
# #xGH
# SC1_fixtures_yc$sc1_xHYC <- SC1_fixtures_yc$avg_HY_sc1 * SC1_fixtures_yc$sc1_homeyas * SC1_fixtures_yc$sc1_awayyds
# #xGA
#
# SC1_fixtures_yc$sc1_awayyas <- as.numeric(unlist(SC1_fixtures_yc$sc1_awayyas))
#
# SC1_fixtures_yc$sc1_xAYC <- SC1_fixtures_yc$avg_AY_sc1 * SC1_fixtures_yc$sc1_awayyas * SC1_fixtures_yc$sc1_homeyds
#
# SC1_fixtures_yc$sc1_0_0 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_0 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_0_1 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_1 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_0 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_0_2 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_2 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_1 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_2 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_3 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_0 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_1 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_2 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_0_3 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_3 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_3 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_4 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_0 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_1 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_2 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_3 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_0_4 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_4 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_4 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_4 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_5 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_0 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_1 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_2 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_3 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_4 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_0_5 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_5 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_5 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_5 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_5 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_6 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_0 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(0,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_1 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(1,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_2 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(2,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_3 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(3,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_4 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(4,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_6_5 <- round(stats::dpois(6,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(5,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_0_6 <- round(stats::dpois(0,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_1_6 <- round(stats::dpois(1,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_2_6 <- round(stats::dpois(2,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_3_6 <- round(stats::dpois(3,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_4_6 <- round(stats::dpois(4,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# SC1_fixtures_yc$sc1_5_6 <- round(stats::dpois(5,SC1_fixtures_yc$sc1_xHYC) * stats::dpois(6,SC1_fixtures_yc$sc1_xAYC), digits = 4)
# #Home win
# SC1_fixtures_yc$sc1_H <- (
#   SC1_fixtures_yc$sc1_1_0 + SC1_fixtures_yc$sc1_2_0 + SC1_fixtures_yc$sc1_2_1 + SC1_fixtures_yc$sc1_3_0 + SC1_fixtures_yc$sc1_3_1 +
#     SC1_fixtures_yc$sc1_3_2 + SC1_fixtures_yc$sc1_4_0 + SC1_fixtures_yc$sc1_4_1 + SC1_fixtures_yc$sc1_4_2 + SC1_fixtures_yc$sc1_4_3 +
#     SC1_fixtures_yc$sc1_5_0 + SC1_fixtures_yc$sc1_5_1 + SC1_fixtures_yc$sc1_5_2 + SC1_fixtures_yc$sc1_5_3 + SC1_fixtures_yc$sc1_5_4 +
#     SC1_fixtures_yc$sc1_6_0 + SC1_fixtures_yc$sc1_6_1 + SC1_fixtures_yc$sc1_6_2 + SC1_fixtures_yc$sc1_6_3 + SC1_fixtures_yc$sc1_6_4 +
#     SC1_fixtures_yc$sc1_6_5
# )
#
# SC1_fixtures_yc$sc1_H <- percent(SC1_fixtures_yc$sc1_H, accuracy = 0.1)
#
# #Draw
# SC1_fixtures_yc$sc1_D <- (
#
#   SC1_fixtures_yc$sc1_0_0 + SC1_fixtures_yc$sc1_1_1 + SC1_fixtures_yc$sc1_2_2 + SC1_fixtures_yc$sc1_3_3 + SC1_fixtures_yc$sc1_4_4 +
#     SC1_fixtures_yc$sc1_5_5 + SC1_fixtures_yc$sc1_6_6
# )
#
# SC1_fixtures_yc$sc1_D <- percent(SC1_fixtures_yc$sc1_D, accuracy = 0.1)
#
# #Away
#
# SC1_fixtures_yc$sc1_A <- (
#   SC1_fixtures_yc$sc1_0_1 + SC1_fixtures_yc$sc1_0_2 + SC1_fixtures_yc$sc1_1_2 + SC1_fixtures_yc$sc1_0_3 + SC1_fixtures_yc$sc1_1_3 +
#     SC1_fixtures_yc$sc1_2_3 + SC1_fixtures_yc$sc1_0_4 + SC1_fixtures_yc$sc1_1_4 + SC1_fixtures_yc$sc1_2_4 + SC1_fixtures_yc$sc1_3_4 +
#     SC1_fixtures_yc$sc1_0_5 + SC1_fixtures_yc$sc1_1_5 + SC1_fixtures_yc$sc1_2_5 + SC1_fixtures_yc$sc1_3_5 + SC1_fixtures_yc$sc1_4_5 +
#     SC1_fixtures_yc$sc1_0_6 + SC1_fixtures_yc$sc1_1_6 + SC1_fixtures_yc$sc1_2_6 + SC1_fixtures_yc$sc1_3_6 + SC1_fixtures_yc$sc1_4_6 +
#     SC1_fixtures_yc$sc1_5_6
# )
#
# SC1_fixtures_yc$sc1_A <- percent(SC1_fixtures_yc$sc1_A, accuracy = 0.1)
#
# #ov25
# SC1_fixtures_yc$sc1_ov25 <- (
#   SC1_fixtures_yc$sc1_2_1 + SC1_fixtures_yc$sc1_1_2 + SC1_fixtures_yc$sc1_2_2 + SC1_fixtures_yc$sc1_3_0 + SC1_fixtures_yc$sc1_3_1 +
#     SC1_fixtures_yc$sc1_3_2 + SC1_fixtures_yc$sc1_0_3 + SC1_fixtures_yc$sc1_1_3 + SC1_fixtures_yc$sc1_2_3 + SC1_fixtures_yc$sc1_3_3 +
#     SC1_fixtures_yc$sc1_4_0 + SC1_fixtures_yc$sc1_4_1 + SC1_fixtures_yc$sc1_4_2 + SC1_fixtures_yc$sc1_4_3 + SC1_fixtures_yc$sc1_0_4 +
#     SC1_fixtures_yc$sc1_1_4 + SC1_fixtures_yc$sc1_2_4 + SC1_fixtures_yc$sc1_3_4 + SC1_fixtures_yc$sc1_4_4 + SC1_fixtures_yc$sc1_5_0 +
#     SC1_fixtures_yc$sc1_5_1 + SC1_fixtures_yc$sc1_5_2 + SC1_fixtures_yc$sc1_5_3 + SC1_fixtures_yc$sc1_5_4 + SC1_fixtures_yc$sc1_0_5 +
#     SC1_fixtures_yc$sc1_1_5 + SC1_fixtures_yc$sc1_2_5 + SC1_fixtures_yc$sc1_3_5 + SC1_fixtures_yc$sc1_4_5 + SC1_fixtures_yc$sc1_5_5 +
#     SC1_fixtures_yc$sc1_6_0 + SC1_fixtures_yc$sc1_6_1 + SC1_fixtures_yc$sc1_6_2 + SC1_fixtures_yc$sc1_6_3 + SC1_fixtures_yc$sc1_6_4 +
#     SC1_fixtures_yc$sc1_6_5 + SC1_fixtures_yc$sc1_0_6 + SC1_fixtures_yc$sc1_1_6 + SC1_fixtures_yc$sc1_2_6 + SC1_fixtures_yc$sc1_3_6 +
#     SC1_fixtures_yc$sc1_4_6 + SC1_fixtures_yc$sc1_5_6 + SC1_fixtures_yc$sc1_6_6
# )
# #un25
# SC1_fixtures_yc$sc1_un25 <- (
#   SC1_fixtures_yc$sc1_0_0 + SC1_fixtures_yc$sc1_1_0 + SC1_fixtures_yc$sc1_0_1 + SC1_fixtures_yc$sc1_1_1 + SC1_fixtures_yc$sc1_2_0 + SC1_fixtures_yc$sc1_0_2
# )
# #odds
# SC1_fixtures_yc$sc1_ov25_odds <- round((1/SC1_fixtures_yc$sc1_ov25),digits = 2)
# SC1_fixtures_yc$sc1_un25_odds <- round((1/SC1_fixtures_yc$sc1_un25),digits = 2)
#
# SC1_fixtures_yc$sc1_ov25_odds
# SC1_fixtures_yc$sc1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC1_fixtures_yc$sc1_ov25 <- percent(SC1_fixtures_yc$sc1_ov25, accuracy = 0.1)
#
# SC1_fixtures_yc$sc1_un25 <- percent(SC1_fixtures_yc$sc1_un25, accuracy = 0.1)
# SC1_fixtures_yc$sc1_pscore <- paste(round(SC1_fixtures_yc$sc1_xHYC,digits = 0),round(SC1_fixtures_yc$sc1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# ###SC2
# HomeTeam_sc2_yc <- rep(sc2_teams, each = length(sc2_teams))
# AwayTeam_sc2_yc <- rep(sc2_teams, length(sc2_teams))
# SC2_fixtures_yc <- cbind(HomeTeam_sc2_yc,AwayTeam_sc2_yc)
# SC2_fixtures_yc <- as.data.frame(SC2_fixtures_yc)
# SC2_fixtures_yc <- SC2_fixtures_yc[!SC2_fixtures_yc$HomeTeam_sc2_yc == SC2_fixtures_yc$AwayTeam_sc2_yc,]
# rownames(SC2_fixtures_yc) <- NULL
# SC2_fixtures_yc$Div <- "SC2"
# SC2_fixtures_yc <- SC2_fixtures_yc[,c(3,1,2)]
#
# SC2_fixtures_yc$avg_HY_sc2 <- sc2_avg_HY
#
# SC2_fixtures_yc$sc2_homeyas <- rep(sc2_home_yas,each = length(sc2_teams)-1)
#
# sc2_awayyds_lookup <- cbind(sc2_teams,sc2_away_yds)
#
# sc2_awayyds_lookup <- as.data.frame(sc2_awayyds_lookup)
#
# colnames(sc2_awayyds_lookup) <- c("AwayTeam_sc2_yc","sc2_awayyds")
#
#
# require('RH2')
# SC2_fixtures_yc$sc2_awayyds <- sqldf("SELECT sc2_awayyds_lookup.sc2_awayyds FROM sc2_awayyds_lookup INNER JOIN SC2_fixtures_yc ON sc2_awayyds_lookup.AwayTeam_sc2_yc = SC2_fixtures_yc.AwayTeam_sc2_yc")
#
# SC2_fixtures_yc$avg_AY_sc2 <- sc2_avg_AY
#
# sc2_awayyas_lookup <- cbind(sc2_teams,sc2_away_yas)
#
# sc2_awayyas_lookup <- as.data.frame(sc2_awayyas_lookup)
#
# colnames(sc2_awayyas_lookup) <- c("AwayTeam_sc2_yc","sc2_awayyas")
#
# SC2_fixtures_yc$sc2_awayyas <- sqldf("SELECT sc2_awayyas_lookup.sc2_awayyas FROM sc2_awayyas_lookup INNER JOIN SC2_fixtures_yc ON sc2_awayyas_lookup.AwayTeam_sc2_yc = SC2_fixtures_yc.AwayTeam_sc2_yc")
#
# SC2_fixtures_yc$sc2_homeyds <- rep(sc2_home_yds,each = length(sc2_teams)-1)
#
# SC2_fixtures_yc$sc2_awayyds <- as.numeric(unlist(SC2_fixtures_yc$sc2_awayyds))
# #xGH
# SC2_fixtures_yc$sc2_xHYC <- SC2_fixtures_yc$avg_HY_sc2 * SC2_fixtures_yc$sc2_homeyas * SC2_fixtures_yc$sc2_awayyds
# #xGA
#
# SC2_fixtures_yc$sc2_awayyas <- as.numeric(unlist(SC2_fixtures_yc$sc2_awayyas))
#
# SC2_fixtures_yc$sc2_xAYC <- SC2_fixtures_yc$avg_AY_sc2 * SC2_fixtures_yc$sc2_awayyas * SC2_fixtures_yc$sc2_homeyds
#
# SC2_fixtures_yc$sc2_0_0 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_0 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_0_1 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_1 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_0 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_0_2 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_2 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_1 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_2 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_3 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_0 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_1 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_2 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_0_3 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_3 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_3 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_4 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_0 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_1 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_2 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_3 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_0_4 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_4 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_4 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_4 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_5 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_0 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_1 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_2 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_3 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_4 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_0_5 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_5 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_5 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_5 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_5 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_6 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_0 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(0,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_1 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(1,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_2 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(2,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_3 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(3,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_4 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(4,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_6_5 <- round(stats::dpois(6,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(5,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_0_6 <- round(stats::dpois(0,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_1_6 <- round(stats::dpois(1,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_2_6 <- round(stats::dpois(2,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_3_6 <- round(stats::dpois(3,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_4_6 <- round(stats::dpois(4,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# SC2_fixtures_yc$sc2_5_6 <- round(stats::dpois(5,SC2_fixtures_yc$sc2_xHYC) * stats::dpois(6,SC2_fixtures_yc$sc2_xAYC), digits = 4)
# #Home win
# SC2_fixtures_yc$sc2_H <- (
#   SC2_fixtures_yc$sc2_1_0 + SC2_fixtures_yc$sc2_2_0 + SC2_fixtures_yc$sc2_2_1 + SC2_fixtures_yc$sc2_3_0 + SC2_fixtures_yc$sc2_3_1 +
#     SC2_fixtures_yc$sc2_3_2 + SC2_fixtures_yc$sc2_4_0 + SC2_fixtures_yc$sc2_4_1 + SC2_fixtures_yc$sc2_4_2 + SC2_fixtures_yc$sc2_4_3 +
#     SC2_fixtures_yc$sc2_5_0 + SC2_fixtures_yc$sc2_5_1 + SC2_fixtures_yc$sc2_5_2 + SC2_fixtures_yc$sc2_5_3 + SC2_fixtures_yc$sc2_5_4 +
#     SC2_fixtures_yc$sc2_6_0 + SC2_fixtures_yc$sc2_6_1 + SC2_fixtures_yc$sc2_6_2 + SC2_fixtures_yc$sc2_6_3 + SC2_fixtures_yc$sc2_6_4 +
#     SC2_fixtures_yc$sc2_6_5
# )
#
# SC2_fixtures_yc$sc2_H <- percent(SC2_fixtures_yc$sc2_H, accuracy = 0.1)
#
# #Draw
# SC2_fixtures_yc$sc2_D <- (
#
#   SC2_fixtures_yc$sc2_0_0 + SC2_fixtures_yc$sc2_1_1 + SC2_fixtures_yc$sc2_2_2 + SC2_fixtures_yc$sc2_3_3 + SC2_fixtures_yc$sc2_4_4 +
#     SC2_fixtures_yc$sc2_5_5 + SC2_fixtures_yc$sc2_6_6
# )
#
# SC2_fixtures_yc$sc2_D <- percent(SC2_fixtures_yc$sc2_D, accuracy = 0.1)
#
# #Away
#
# SC2_fixtures_yc$sc2_A <- (
#   SC2_fixtures_yc$sc2_0_1 + SC2_fixtures_yc$sc2_0_2 + SC2_fixtures_yc$sc2_1_2 + SC2_fixtures_yc$sc2_0_3 + SC2_fixtures_yc$sc2_1_3 +
#     SC2_fixtures_yc$sc2_2_3 + SC2_fixtures_yc$sc2_0_4 + SC2_fixtures_yc$sc2_1_4 + SC2_fixtures_yc$sc2_2_4 + SC2_fixtures_yc$sc2_3_4 +
#     SC2_fixtures_yc$sc2_0_5 + SC2_fixtures_yc$sc2_1_5 + SC2_fixtures_yc$sc2_2_5 + SC2_fixtures_yc$sc2_3_5 + SC2_fixtures_yc$sc2_4_5 +
#     SC2_fixtures_yc$sc2_0_6 + SC2_fixtures_yc$sc2_1_6 + SC2_fixtures_yc$sc2_2_6 + SC2_fixtures_yc$sc2_3_6 + SC2_fixtures_yc$sc2_4_6 +
#     SC2_fixtures_yc$sc2_5_6
# )
#
# SC2_fixtures_yc$sc2_A <- percent(SC2_fixtures_yc$sc2_A, accuracy = 0.1)
#
# #ov25
# SC2_fixtures_yc$sc2_ov25 <- (
#   SC2_fixtures_yc$sc2_2_1 + SC2_fixtures_yc$sc2_1_2 + SC2_fixtures_yc$sc2_2_2 + SC2_fixtures_yc$sc2_3_0 + SC2_fixtures_yc$sc2_3_1 +
#     SC2_fixtures_yc$sc2_3_2 + SC2_fixtures_yc$sc2_0_3 + SC2_fixtures_yc$sc2_1_3 + SC2_fixtures_yc$sc2_2_3 + SC2_fixtures_yc$sc2_3_3 +
#     SC2_fixtures_yc$sc2_4_0 + SC2_fixtures_yc$sc2_4_1 + SC2_fixtures_yc$sc2_4_2 + SC2_fixtures_yc$sc2_4_3 + SC2_fixtures_yc$sc2_0_4 +
#     SC2_fixtures_yc$sc2_1_4 + SC2_fixtures_yc$sc2_2_4 + SC2_fixtures_yc$sc2_3_4 + SC2_fixtures_yc$sc2_4_4 + SC2_fixtures_yc$sc2_5_0 +
#     SC2_fixtures_yc$sc2_5_1 + SC2_fixtures_yc$sc2_5_2 + SC2_fixtures_yc$sc2_5_3 + SC2_fixtures_yc$sc2_5_4 + SC2_fixtures_yc$sc2_0_5 +
#     SC2_fixtures_yc$sc2_1_5 + SC2_fixtures_yc$sc2_2_5 + SC2_fixtures_yc$sc2_3_5 + SC2_fixtures_yc$sc2_4_5 + SC2_fixtures_yc$sc2_5_5 +
#     SC2_fixtures_yc$sc2_6_0 + SC2_fixtures_yc$sc2_6_1 + SC2_fixtures_yc$sc2_6_2 + SC2_fixtures_yc$sc2_6_3 + SC2_fixtures_yc$sc2_6_4 +
#     SC2_fixtures_yc$sc2_6_5 + SC2_fixtures_yc$sc2_0_6 + SC2_fixtures_yc$sc2_1_6 + SC2_fixtures_yc$sc2_2_6 + SC2_fixtures_yc$sc2_3_6 +
#     SC2_fixtures_yc$sc2_4_6 + SC2_fixtures_yc$sc2_5_6 + SC2_fixtures_yc$sc2_6_6
# )
# #un25
# SC2_fixtures_yc$sc2_un25 <- (
#   SC2_fixtures_yc$sc2_0_0 + SC2_fixtures_yc$sc2_1_0 + SC2_fixtures_yc$sc2_0_1 + SC2_fixtures_yc$sc2_1_1 + SC2_fixtures_yc$sc2_2_0 + SC2_fixtures_yc$sc2_0_2
# )
# #odds
# SC2_fixtures_yc$sc2_ov25_odds <- round((1/SC2_fixtures_yc$sc2_ov25),digits = 2)
# SC2_fixtures_yc$sc2_un25_odds <- round((1/SC2_fixtures_yc$sc2_un25),digits = 2)
#
# SC2_fixtures_yc$sc2_ov25_odds
# SC2_fixtures_yc$sc2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC2_fixtures_yc$sc2_ov25 <- percent(SC2_fixtures_yc$sc2_ov25, accuracy = 0.1)
#
# SC2_fixtures_yc$sc2_un25 <- percent(SC2_fixtures_yc$sc2_un25, accuracy = 0.1)
# SC2_fixtures_yc$sc2_pscore <- paste(round(SC2_fixtures_yc$sc2_xHYC,digits = 0),round(SC2_fixtures_yc$sc2_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SC2_fixtures,'Divisions/SC2.xlsx',sheetName = "SC2", append = TRUE)
# ################################################################################################################################################################################################################################
# #SC3
# HomeTeam_sc3_yc <- rep(sc3_teams, each = length(sc3_teams))
# AwayTeam_sc3_yc <- rep(sc3_teams, length(sc3_teams))
# SC3_fixtures_yc <- cbind(HomeTeam_sc3_yc,AwayTeam_sc3_yc)
# SC3_fixtures_yc <- as.data.frame(SC3_fixtures_yc)
# SC3_fixtures_yc <- SC3_fixtures_yc[!SC3_fixtures_yc$HomeTeam_sc3_yc == SC3_fixtures_yc$AwayTeam_sc3_yc,]
# rownames(SC3_fixtures_yc) <- NULL
# SC3_fixtures_yc$Div <- "SC3"
# SC3_fixtures_yc <- SC3_fixtures_yc[,c(3,1,2)]
#
# SC3_fixtures_yc$avg_HY_sc3 <- sc3_avg_HY
#
# SC3_fixtures_yc$sc3_homeyas <- rep(sc3_home_yas,each = length(sc3_teams)-1)
#
# sc3_awayyds_lookup <- cbind(sc3_teams,sc3_away_yds)
#
# sc3_awayyds_lookup <- as.data.frame(sc3_awayyds_lookup)
#
# colnames(sc3_awayyds_lookup) <- c("AwayTeam_sc3_yc","sc3_awayyds")
#
#
# require('RH2')
# SC3_fixtures_yc$sc3_awayyds <- sqldf("SELECT sc3_awayyds_lookup.sc3_awayyds FROM sc3_awayyds_lookup INNER JOIN SC3_fixtures_yc ON sc3_awayyds_lookup.AwayTeam_sc3_yc = SC3_fixtures_yc.AwayTeam_sc3_yc")
#
# SC3_fixtures_yc$avg_AY_sc3 <- sc3_avg_AY
#
# sc3_awayyas_lookup <- cbind(sc3_teams,sc3_away_yas)
#
# sc3_awayyas_lookup <- as.data.frame(sc3_awayyas_lookup)
#
# colnames(sc3_awayyas_lookup) <- c("AwayTeam_sc3_yc","sc3_awayyas")
#
# SC3_fixtures_yc$sc3_awayyas <- sqldf("SELECT sc3_awayyas_lookup.sc3_awayyas FROM sc3_awayyas_lookup INNER JOIN SC3_fixtures_yc ON sc3_awayyas_lookup.AwayTeam_sc3_yc = SC3_fixtures_yc.AwayTeam_sc3_yc")
#
# SC3_fixtures_yc$sc3_homeyds <- rep(sc3_home_yds,each = length(sc3_teams)-1)
#
# SC3_fixtures_yc$sc3_awayyds <- as.numeric(unlist(SC3_fixtures_yc$sc3_awayyds))
# #xGH
# SC3_fixtures_yc$sc3_xHYC <- SC3_fixtures_yc$avg_HY_sc3 * SC3_fixtures_yc$sc3_homeyas * SC3_fixtures_yc$sc3_awayyds
# #xGA
#
# SC3_fixtures_yc$sc3_awayyas <- as.numeric(unlist(SC3_fixtures_yc$sc3_awayyas))
#
# SC3_fixtures_yc$sc3_xAYC <- SC3_fixtures_yc$avg_AY_sc3 * SC3_fixtures_yc$sc3_awayyas * SC3_fixtures_yc$sc3_homeyds
#
# SC3_fixtures_yc$sc3_0_0 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_0 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_0_1 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_1 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_0 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_0_2 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_2 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_1 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_2 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_3 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_0 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_1 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_2 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_0_3 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_3 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_3 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_4 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_0 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_1 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_2 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_3 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_0_4 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_4 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_4 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_4 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_5 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_0 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_1 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_2 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_3 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_4 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_0_5 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_5 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_5 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_5 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_5 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_6 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_0 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(0,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_1 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(1,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_2 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(2,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_3 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(3,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_4 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(4,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_6_5 <- round(stats::dpois(6,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(5,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_0_6 <- round(stats::dpois(0,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_1_6 <- round(stats::dpois(1,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_2_6 <- round(stats::dpois(2,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_3_6 <- round(stats::dpois(3,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_4_6 <- round(stats::dpois(4,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# SC3_fixtures_yc$sc3_5_6 <- round(stats::dpois(5,SC3_fixtures_yc$sc3_xHYC) * stats::dpois(6,SC3_fixtures_yc$sc3_xAYC), digits = 4)
# #Home win
# SC3_fixtures_yc$sc3_H <- (
#   SC3_fixtures_yc$sc3_1_0 + SC3_fixtures_yc$sc3_2_0 + SC3_fixtures_yc$sc3_2_1 + SC3_fixtures_yc$sc3_3_0 + SC3_fixtures_yc$sc3_3_1 +
#     SC3_fixtures_yc$sc3_3_2 + SC3_fixtures_yc$sc3_4_0 + SC3_fixtures_yc$sc3_4_1 + SC3_fixtures_yc$sc3_4_2 + SC3_fixtures_yc$sc3_4_3 +
#     SC3_fixtures_yc$sc3_5_0 + SC3_fixtures_yc$sc3_5_1 + SC3_fixtures_yc$sc3_5_2 + SC3_fixtures_yc$sc3_5_3 + SC3_fixtures_yc$sc3_5_4 +
#     SC3_fixtures_yc$sc3_6_0 + SC3_fixtures_yc$sc3_6_1 + SC3_fixtures_yc$sc3_6_2 + SC3_fixtures_yc$sc3_6_3 + SC3_fixtures_yc$sc3_6_4 +
#     SC3_fixtures_yc$sc3_6_5
# )
#
# SC3_fixtures_yc$sc3_H <- percent(SC3_fixtures_yc$sc3_H, accuracy = 0.1)
#
# #Draw
# SC3_fixtures_yc$sc3_D <- (
#
#   SC3_fixtures_yc$sc3_0_0 + SC3_fixtures_yc$sc3_1_1 + SC3_fixtures_yc$sc3_2_2 + SC3_fixtures_yc$sc3_3_3 + SC3_fixtures_yc$sc3_4_4 +
#     SC3_fixtures_yc$sc3_5_5 + SC3_fixtures_yc$sc3_6_6
# )
#
# SC3_fixtures_yc$sc3_D <- percent(SC3_fixtures_yc$sc3_D, accuracy = 0.1)
#
# #Away
#
# SC3_fixtures_yc$sc3_A <- (
#   SC3_fixtures_yc$sc3_0_1 + SC3_fixtures_yc$sc3_0_2 + SC3_fixtures_yc$sc3_1_2 + SC3_fixtures_yc$sc3_0_3 + SC3_fixtures_yc$sc3_1_3 +
#     SC3_fixtures_yc$sc3_2_3 + SC3_fixtures_yc$sc3_0_4 + SC3_fixtures_yc$sc3_1_4 + SC3_fixtures_yc$sc3_2_4 + SC3_fixtures_yc$sc3_3_4 +
#     SC3_fixtures_yc$sc3_0_5 + SC3_fixtures_yc$sc3_1_5 + SC3_fixtures_yc$sc3_2_5 + SC3_fixtures_yc$sc3_3_5 + SC3_fixtures_yc$sc3_4_5 +
#     SC3_fixtures_yc$sc3_0_6 + SC3_fixtures_yc$sc3_1_6 + SC3_fixtures_yc$sc3_2_6 + SC3_fixtures_yc$sc3_3_6 + SC3_fixtures_yc$sc3_4_6 +
#     SC3_fixtures_yc$sc3_5_6
# )
#
# SC3_fixtures_yc$sc3_A <- percent(SC3_fixtures_yc$sc3_A, accuracy = 0.1)
#
# #ov25
# SC3_fixtures_yc$sc3_ov25 <- (
#   SC3_fixtures_yc$sc3_2_1 + SC3_fixtures_yc$sc3_1_2 + SC3_fixtures_yc$sc3_2_2 + SC3_fixtures_yc$sc3_3_0 + SC3_fixtures_yc$sc3_3_1 +
#     SC3_fixtures_yc$sc3_3_2 + SC3_fixtures_yc$sc3_0_3 + SC3_fixtures_yc$sc3_1_3 + SC3_fixtures_yc$sc3_2_3 + SC3_fixtures_yc$sc3_3_3 +
#     SC3_fixtures_yc$sc3_4_0 + SC3_fixtures_yc$sc3_4_1 + SC3_fixtures_yc$sc3_4_2 + SC3_fixtures_yc$sc3_4_3 + SC3_fixtures_yc$sc3_0_4 +
#     SC3_fixtures_yc$sc3_1_4 + SC3_fixtures_yc$sc3_2_4 + SC3_fixtures_yc$sc3_3_4 + SC3_fixtures_yc$sc3_4_4 + SC3_fixtures_yc$sc3_5_0 +
#     SC3_fixtures_yc$sc3_5_1 + SC3_fixtures_yc$sc3_5_2 + SC3_fixtures_yc$sc3_5_3 + SC3_fixtures_yc$sc3_5_4 + SC3_fixtures_yc$sc3_0_5 +
#     SC3_fixtures_yc$sc3_1_5 + SC3_fixtures_yc$sc3_2_5 + SC3_fixtures_yc$sc3_3_5 + SC3_fixtures_yc$sc3_4_5 + SC3_fixtures_yc$sc3_5_5 +
#     SC3_fixtures_yc$sc3_6_0 + SC3_fixtures_yc$sc3_6_1 + SC3_fixtures_yc$sc3_6_2 + SC3_fixtures_yc$sc3_6_3 + SC3_fixtures_yc$sc3_6_4 +
#     SC3_fixtures_yc$sc3_6_5 + SC3_fixtures_yc$sc3_0_6 + SC3_fixtures_yc$sc3_1_6 + SC3_fixtures_yc$sc3_2_6 + SC3_fixtures_yc$sc3_3_6 +
#     SC3_fixtures_yc$sc3_4_6 + SC3_fixtures_yc$sc3_5_6 + SC3_fixtures_yc$sc3_6_6
# )
# #un25
# SC3_fixtures_yc$sc3_un25 <- (
#   SC3_fixtures_yc$sc3_0_0 + SC3_fixtures_yc$sc3_1_0 + SC3_fixtures_yc$sc3_0_1 + SC3_fixtures_yc$sc3_1_1 + SC3_fixtures_yc$sc3_2_0 + SC3_fixtures_yc$sc3_0_2
# )
# #odds
# SC3_fixtures_yc$sc3_ov25_odds <- round((1/SC3_fixtures_yc$sc3_ov25),digits = 2)
# SC3_fixtures_yc$sc3_un25_odds <- round((1/SC3_fixtures_yc$sc3_un25),digits = 2)
#
# SC3_fixtures_yc$sc3_ov25_odds
# SC3_fixtures_yc$sc3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC3_fixtures_yc$sc3_ov25 <- percent(SC3_fixtures_yc$sc3_ov25, accuracy = 0.1)
#
# SC3_fixtures_yc$sc3_un25 <- percent(SC3_fixtures_yc$sc3_un25, accuracy = 0.1)
# SC3_fixtures_yc$sc3_pscore <- paste(round(SC3_fixtures_yc$sc3_xHYC,digits = 0),round(SC3_fixtures_yc$sc3_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SC3_fixtures,'Divisions/SC3.xlsx',sheetName = "SC3", append = TRUE)
# #################################################################################################################
# ################################################################################################################
# #################################################################################################################
# #SP1
# HomeTeam_sp1_yc <- rep(sp1_teams, each = length(sp1_teams))
# AwayTeam_sp1_yc <- rep(sp1_teams, length(sp1_teams))
# SP1_fixtures_yc <- cbind(HomeTeam_sp1_yc,AwayTeam_sp1_yc)
# SP1_fixtures_yc <- as.data.frame(SP1_fixtures_yc)
# SP1_fixtures_yc <- SP1_fixtures_yc[!SP1_fixtures_yc$HomeTeam_sp1_yc == SP1_fixtures_yc$AwayTeam_sp1_yc,]
# rownames(SP1_fixtures_yc) <- NULL
# SP1_fixtures_yc$Div <- "SP1"
# SP1_fixtures_yc <- SP1_fixtures_yc[,c(3,1,2)]
#
# SP1_fixtures_yc$avg_HY_sp1 <- sp1_avg_HY
#
# SP1_fixtures_yc$sp1_homeyas <- rep(sp1_home_yas,each = length(sp1_teams)-1)
#
# sp1_awayyds_lookup <- cbind(sp1_teams,sp1_away_yds)
#
# sp1_awayyds_lookup <- as.data.frame(sp1_awayyds_lookup)
#
# colnames(sp1_awayyds_lookup) <- c("AwayTeam_sp1_yc","sp1_awayyds")
#
#
# require('RH2')
# SP1_fixtures_yc$sp1_awayyds <- sqldf("SELECT sp1_awayyds_lookup.sp1_awayyds FROM sp1_awayyds_lookup INNER JOIN SP1_fixtures_yc ON sp1_awayyds_lookup.AwayTeam_sp1_yc = SP1_fixtures_yc.AwayTeam_sp1_yc")
#
# SP1_fixtures_yc$avg_AY_sp1 <- sp1_avg_AY
#
# sp1_awayyas_lookup <- cbind(sp1_teams,sp1_away_yas)
#
# sp1_awayyas_lookup <- as.data.frame(sp1_awayyas_lookup)
#
# colnames(sp1_awayyas_lookup) <- c("AwayTeam_sp1_yc","sp1_awayyas")
#
# SP1_fixtures_yc$sp1_awayyas <- sqldf("SELECT sp1_awayyas_lookup.sp1_awayyas FROM sp1_awayyas_lookup INNER JOIN SP1_fixtures_yc ON sp1_awayyas_lookup.AwayTeam_sp1_yc = SP1_fixtures_yc.AwayTeam_sp1_yc")
#
# SP1_fixtures_yc$sp1_homeyds <- rep(sp1_home_yds,each = length(sp1_teams)-1)
#
# SP1_fixtures_yc$sp1_awayyds <- as.numeric(unlist(SP1_fixtures_yc$sp1_awayyds))
# #xGH
# SP1_fixtures_yc$sp1_xHYC <- SP1_fixtures_yc$avg_HY_sp1 * SP1_fixtures_yc$sp1_homeyas * SP1_fixtures_yc$sp1_awayyds
# #xGA
#
# SP1_fixtures_yc$sp1_awayyas <- as.numeric(unlist(SP1_fixtures_yc$sp1_awayyas))
#
# SP1_fixtures_yc$sp1_xAYC <- SP1_fixtures_yc$avg_AY_sp1 * SP1_fixtures_yc$sp1_awayyas * SP1_fixtures_yc$sp1_homeyds
#
# SP1_fixtures_yc$sp1_0_0 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_0 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_0_1 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_1 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_0 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_0_2 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_2 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_1 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_2 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_3 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_0 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_1 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_2 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_0_3 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_3 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_3 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_4 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_0 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_1 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_2 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_3 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_0_4 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_4 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_4 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_4 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_5 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_0 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_1 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_2 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_3 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_4 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_0_5 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_5 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_5 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_5 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_5 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_6 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_0 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(0,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_1 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(1,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_2 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(2,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_3 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(3,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_4 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(4,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_6_5 <- round(stats::dpois(6,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(5,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_0_6 <- round(stats::dpois(0,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_1_6 <- round(stats::dpois(1,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_2_6 <- round(stats::dpois(2,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_3_6 <- round(stats::dpois(3,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_4_6 <- round(stats::dpois(4,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# SP1_fixtures_yc$sp1_5_6 <- round(stats::dpois(5,SP1_fixtures_yc$sp1_xHYC) * stats::dpois(6,SP1_fixtures_yc$sp1_xAYC), digits = 4)
# #Home win
# SP1_fixtures_yc$sp1_H <- (
#   SP1_fixtures_yc$sp1_1_0 + SP1_fixtures_yc$sp1_2_0 + SP1_fixtures_yc$sp1_2_1 + SP1_fixtures_yc$sp1_3_0 + SP1_fixtures_yc$sp1_3_1 +
#     SP1_fixtures_yc$sp1_3_2 + SP1_fixtures_yc$sp1_4_0 + SP1_fixtures_yc$sp1_4_1 + SP1_fixtures_yc$sp1_4_2 + SP1_fixtures_yc$sp1_4_3 +
#     SP1_fixtures_yc$sp1_5_0 + SP1_fixtures_yc$sp1_5_1 + SP1_fixtures_yc$sp1_5_2 + SP1_fixtures_yc$sp1_5_3 + SP1_fixtures_yc$sp1_5_4 +
#     SP1_fixtures_yc$sp1_6_0 + SP1_fixtures_yc$sp1_6_1 + SP1_fixtures_yc$sp1_6_2 + SP1_fixtures_yc$sp1_6_3 + SP1_fixtures_yc$sp1_6_4 +
#     SP1_fixtures_yc$sp1_6_5
# )
#
# SP1_fixtures_yc$sp1_H <- percent(SP1_fixtures_yc$sp1_H, accuracy = 0.1)
#
# #Draw
# SP1_fixtures_yc$sp1_D <- (
#
#   SP1_fixtures_yc$sp1_0_0 + SP1_fixtures_yc$sp1_1_1 + SP1_fixtures_yc$sp1_2_2 + SP1_fixtures_yc$sp1_3_3 + SP1_fixtures_yc$sp1_4_4 +
#     SP1_fixtures_yc$sp1_5_5 + SP1_fixtures_yc$sp1_6_6
# )
#
# SP1_fixtures_yc$sp1_D <- percent(SP1_fixtures_yc$sp1_D, accuracy = 0.1)
#
# #Away
#
# SP1_fixtures_yc$sp1_A <- (
#   SP1_fixtures_yc$sp1_0_1 + SP1_fixtures_yc$sp1_0_2 + SP1_fixtures_yc$sp1_1_2 + SP1_fixtures_yc$sp1_0_3 + SP1_fixtures_yc$sp1_1_3 +
#     SP1_fixtures_yc$sp1_2_3 + SP1_fixtures_yc$sp1_0_4 + SP1_fixtures_yc$sp1_1_4 + SP1_fixtures_yc$sp1_2_4 + SP1_fixtures_yc$sp1_3_4 +
#     SP1_fixtures_yc$sp1_0_5 + SP1_fixtures_yc$sp1_1_5 + SP1_fixtures_yc$sp1_2_5 + SP1_fixtures_yc$sp1_3_5 + SP1_fixtures_yc$sp1_4_5 +
#     SP1_fixtures_yc$sp1_0_6 + SP1_fixtures_yc$sp1_1_6 + SP1_fixtures_yc$sp1_2_6 + SP1_fixtures_yc$sp1_3_6 + SP1_fixtures_yc$sp1_4_6 +
#     SP1_fixtures_yc$sp1_5_6
# )
#
# SP1_fixtures_yc$sp1_A <- percent(SP1_fixtures_yc$sp1_A, accuracy = 0.1)
#
# #ov25
# SP1_fixtures_yc$sp1_ov25 <- (
#   SP1_fixtures_yc$sp1_2_1 + SP1_fixtures_yc$sp1_1_2 + SP1_fixtures_yc$sp1_2_2 + SP1_fixtures_yc$sp1_3_0 + SP1_fixtures_yc$sp1_3_1 +
#     SP1_fixtures_yc$sp1_3_2 + SP1_fixtures_yc$sp1_0_3 + SP1_fixtures_yc$sp1_1_3 + SP1_fixtures_yc$sp1_2_3 + SP1_fixtures_yc$sp1_3_3 +
#     SP1_fixtures_yc$sp1_4_0 + SP1_fixtures_yc$sp1_4_1 + SP1_fixtures_yc$sp1_4_2 + SP1_fixtures_yc$sp1_4_3 + SP1_fixtures_yc$sp1_0_4 +
#     SP1_fixtures_yc$sp1_1_4 + SP1_fixtures_yc$sp1_2_4 + SP1_fixtures_yc$sp1_3_4 + SP1_fixtures_yc$sp1_4_4 + SP1_fixtures_yc$sp1_5_0 +
#     SP1_fixtures_yc$sp1_5_1 + SP1_fixtures_yc$sp1_5_2 + SP1_fixtures_yc$sp1_5_3 + SP1_fixtures_yc$sp1_5_4 + SP1_fixtures_yc$sp1_0_5 +
#     SP1_fixtures_yc$sp1_1_5 + SP1_fixtures_yc$sp1_2_5 + SP1_fixtures_yc$sp1_3_5 + SP1_fixtures_yc$sp1_4_5 + SP1_fixtures_yc$sp1_5_5 +
#     SP1_fixtures_yc$sp1_6_0 + SP1_fixtures_yc$sp1_6_1 + SP1_fixtures_yc$sp1_6_2 + SP1_fixtures_yc$sp1_6_3 + SP1_fixtures_yc$sp1_6_4 +
#     SP1_fixtures_yc$sp1_6_5 + SP1_fixtures_yc$sp1_0_6 + SP1_fixtures_yc$sp1_1_6 + SP1_fixtures_yc$sp1_2_6 + SP1_fixtures_yc$sp1_3_6 +
#     SP1_fixtures_yc$sp1_4_6 + SP1_fixtures_yc$sp1_5_6 + SP1_fixtures_yc$sp1_6_6
# )
# #un25
# SP1_fixtures_yc$sp1_un25 <- (
#   SP1_fixtures_yc$sp1_0_0 + SP1_fixtures_yc$sp1_1_0 + SP1_fixtures_yc$sp1_0_1 + SP1_fixtures_yc$sp1_1_1 + SP1_fixtures_yc$sp1_2_0 + SP1_fixtures_yc$sp1_0_2
# )
# #odds
# SP1_fixtures_yc$sp1_ov25_odds <- round((1/SP1_fixtures_yc$sp1_ov25),digits = 2)
# SP1_fixtures_yc$sp1_un25_odds <- round((1/SP1_fixtures_yc$sp1_un25),digits = 2)
#
# SP1_fixtures_yc$sp1_ov25_odds
# SP1_fixtures_yc$sp1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP1_fixtures_yc$sp1_ov25 <- percent(SP1_fixtures_yc$sp1_ov25, accuracy = 0.1)
#
# SP1_fixtures_yc$sp1_un25 <- percent(SP1_fixtures_yc$sp1_un25, accuracy = 0.1)
# SP1_fixtures_yc$sp1_pscore <- paste(round(SP1_fixtures_yc$sp1_xHYC,digits = 0),round(SP1_fixtures_yc$sp1_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SP1_fixtures,'Divisions/SP1.xlsx',sheetName = "SP1", append = TRUE)
# #################################################################################################################
# #SP2
# HomeTeam_sp2_yc <- rep(sp2_teams, each = length(sp2_teams))
# AwayTeam_sp2_yc <- rep(sp2_teams, length(sp2_teams))
# SP2_fixtures_yc <- cbind(HomeTeam_sp2_yc,AwayTeam_sp2_yc)
# SP2_fixtures_yc <- as.data.frame(SP2_fixtures_yc)
# SP2_fixtures_yc <- SP2_fixtures_yc[!SP2_fixtures_yc$HomeTeam_sp2_yc == SP2_fixtures_yc$AwayTeam_sp2_yc,]
# rownames(SP2_fixtures_yc) <- NULL
# SP2_fixtures_yc$Div <- "SP2"
# SP2_fixtures_yc <- SP2_fixtures_yc[,c(3,1,2)]
#
# SP2_fixtures_yc$avg_HY_sp2 <- sp2_avg_HY
#
# SP2_fixtures_yc$sp2_homeyas <- rep(sp2_home_yas,each = length(sp2_teams)-1)
#
# sp2_awayyds_lookup <- cbind(sp2_teams,sp2_away_yds)
#
# sp2_awayyds_lookup <- as.data.frame(sp2_awayyds_lookup)
#
# colnames(sp2_awayyds_lookup) <- c("AwayTeam_sp2_yc","sp2_awayyds")
#
#
# require('RH2')
# SP2_fixtures_yc$sp2_awayyds <- sqldf("SELECT sp2_awayyds_lookup.sp2_awayyds FROM sp2_awayyds_lookup INNER JOIN SP2_fixtures_yc ON sp2_awayyds_lookup.AwayTeam_sp2_yc = SP2_fixtures_yc.AwayTeam_sp2_yc")
#
# SP2_fixtures_yc$avg_AY_sp2 <- sp2_avg_AY
#
# sp2_awayyas_lookup <- cbind(sp2_teams,sp2_away_yas)
#
# sp2_awayyas_lookup <- as.data.frame(sp2_awayyas_lookup)
#
# colnames(sp2_awayyas_lookup) <- c("AwayTeam_sp2_yc","sp2_awayyas")
#
# SP2_fixtures_yc$sp2_awayyas <- sqldf("SELECT sp2_awayyas_lookup.sp2_awayyas FROM sp2_awayyas_lookup INNER JOIN SP2_fixtures_yc ON sp2_awayyas_lookup.AwayTeam_sp2_yc = SP2_fixtures_yc.AwayTeam_sp2_yc")
#
# SP2_fixtures_yc$sp2_homeyds <- rep(sp2_home_yds,each = length(sp2_teams)-1)
#
# SP2_fixtures_yc$sp2_awayyds <- as.numeric(unlist(SP2_fixtures_yc$sp2_awayyds))
# #xGH
# SP2_fixtures_yc$sp2_xHYC <- SP2_fixtures_yc$avg_HY_sp2 * SP2_fixtures_yc$sp2_homeyas * SP2_fixtures_yc$sp2_awayyds
# #xGA
#
# SP2_fixtures_yc$sp2_awayyas <- as.numeric(unlist(SP2_fixtures_yc$sp2_awayyas))
#
# SP2_fixtures_yc$sp2_xAYC <- SP2_fixtures_yc$avg_AY_sp2 * SP2_fixtures_yc$sp2_awayyas * SP2_fixtures_yc$sp2_homeyds
#
# SP2_fixtures_yc$sp2_0_0 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_0 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_0_1 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_1 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_0 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_0_2 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_2 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_1 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_2 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_3 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_0 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_1 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_2 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_0_3 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_3 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_3 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_4 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_0 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_1 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_2 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_3 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_0_4 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_4 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_4 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_4 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_5 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_0 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_1 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_2 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_3 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_4 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_0_5 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_5 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_5 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_5 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_5 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_6 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_0 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(0,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_1 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(1,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_2 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(2,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_3 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(3,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_4 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(4,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_6_5 <- round(stats::dpois(6,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(5,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_0_6 <- round(stats::dpois(0,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_1_6 <- round(stats::dpois(1,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_2_6 <- round(stats::dpois(2,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_3_6 <- round(stats::dpois(3,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_4_6 <- round(stats::dpois(4,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# SP2_fixtures_yc$sp2_5_6 <- round(stats::dpois(5,SP2_fixtures_yc$sp2_xHYC) * stats::dpois(6,SP2_fixtures_yc$sp2_xAYC), digits = 4)
# #Home win
# SP2_fixtures_yc$sp2_H <- (
#   SP2_fixtures_yc$sp2_1_0 + SP2_fixtures_yc$sp2_2_0 + SP2_fixtures_yc$sp2_2_1 + SP2_fixtures_yc$sp2_3_0 + SP2_fixtures_yc$sp2_3_1 +
#     SP2_fixtures_yc$sp2_3_2 + SP2_fixtures_yc$sp2_4_0 + SP2_fixtures_yc$sp2_4_1 + SP2_fixtures_yc$sp2_4_2 + SP2_fixtures_yc$sp2_4_3 +
#     SP2_fixtures_yc$sp2_5_0 + SP2_fixtures_yc$sp2_5_1 + SP2_fixtures_yc$sp2_5_2 + SP2_fixtures_yc$sp2_5_3 + SP2_fixtures_yc$sp2_5_4 +
#     SP2_fixtures_yc$sp2_6_0 + SP2_fixtures_yc$sp2_6_1 + SP2_fixtures_yc$sp2_6_2 + SP2_fixtures_yc$sp2_6_3 + SP2_fixtures_yc$sp2_6_4 +
#     SP2_fixtures_yc$sp2_6_5
# )
#
# SP2_fixtures_yc$sp2_H <- percent(SP2_fixtures_yc$sp2_H, accuracy = 0.1)
#
# #Draw
# SP2_fixtures_yc$sp2_D <- (
#
#   SP2_fixtures_yc$sp2_0_0 + SP2_fixtures_yc$sp2_1_1 + SP2_fixtures_yc$sp2_2_2 + SP2_fixtures_yc$sp2_3_3 + SP2_fixtures_yc$sp2_4_4 +
#     SP2_fixtures_yc$sp2_5_5 + SP2_fixtures_yc$sp2_6_6
# )
#
# SP2_fixtures_yc$sp2_D <- percent(SP2_fixtures_yc$sp2_D, accuracy = 0.1)
#
# #Away
#
# SP2_fixtures_yc$sp2_A <- (
#   SP2_fixtures_yc$sp2_0_1 + SP2_fixtures_yc$sp2_0_2 + SP2_fixtures_yc$sp2_1_2 + SP2_fixtures_yc$sp2_0_3 + SP2_fixtures_yc$sp2_1_3 +
#     SP2_fixtures_yc$sp2_2_3 + SP2_fixtures_yc$sp2_0_4 + SP2_fixtures_yc$sp2_1_4 + SP2_fixtures_yc$sp2_2_4 + SP2_fixtures_yc$sp2_3_4 +
#     SP2_fixtures_yc$sp2_0_5 + SP2_fixtures_yc$sp2_1_5 + SP2_fixtures_yc$sp2_2_5 + SP2_fixtures_yc$sp2_3_5 + SP2_fixtures_yc$sp2_4_5 +
#     SP2_fixtures_yc$sp2_0_6 + SP2_fixtures_yc$sp2_1_6 + SP2_fixtures_yc$sp2_2_6 + SP2_fixtures_yc$sp2_3_6 + SP2_fixtures_yc$sp2_4_6 +
#     SP2_fixtures_yc$sp2_5_6
# )
#
# SP2_fixtures_yc$sp2_A <- percent(SP2_fixtures_yc$sp2_A, accuracy = 0.1)
#
# #ov25
# SP2_fixtures_yc$sp2_ov25 <- (
#   SP2_fixtures_yc$sp2_2_1 + SP2_fixtures_yc$sp2_1_2 + SP2_fixtures_yc$sp2_2_2 + SP2_fixtures_yc$sp2_3_0 + SP2_fixtures_yc$sp2_3_1 +
#     SP2_fixtures_yc$sp2_3_2 + SP2_fixtures_yc$sp2_0_3 + SP2_fixtures_yc$sp2_1_3 + SP2_fixtures_yc$sp2_2_3 + SP2_fixtures_yc$sp2_3_3 +
#     SP2_fixtures_yc$sp2_4_0 + SP2_fixtures_yc$sp2_4_1 + SP2_fixtures_yc$sp2_4_2 + SP2_fixtures_yc$sp2_4_3 + SP2_fixtures_yc$sp2_0_4 +
#     SP2_fixtures_yc$sp2_1_4 + SP2_fixtures_yc$sp2_2_4 + SP2_fixtures_yc$sp2_3_4 + SP2_fixtures_yc$sp2_4_4 + SP2_fixtures_yc$sp2_5_0 +
#     SP2_fixtures_yc$sp2_5_1 + SP2_fixtures_yc$sp2_5_2 + SP2_fixtures_yc$sp2_5_3 + SP2_fixtures_yc$sp2_5_4 + SP2_fixtures_yc$sp2_0_5 +
#     SP2_fixtures_yc$sp2_1_5 + SP2_fixtures_yc$sp2_2_5 + SP2_fixtures_yc$sp2_3_5 + SP2_fixtures_yc$sp2_4_5 + SP2_fixtures_yc$sp2_5_5 +
#     SP2_fixtures_yc$sp2_6_0 + SP2_fixtures_yc$sp2_6_1 + SP2_fixtures_yc$sp2_6_2 + SP2_fixtures_yc$sp2_6_3 + SP2_fixtures_yc$sp2_6_4 +
#     SP2_fixtures_yc$sp2_6_5 + SP2_fixtures_yc$sp2_0_6 + SP2_fixtures_yc$sp2_1_6 + SP2_fixtures_yc$sp2_2_6 + SP2_fixtures_yc$sp2_3_6 +
#     SP2_fixtures_yc$sp2_4_6 + SP2_fixtures_yc$sp2_5_6 + SP2_fixtures_yc$sp2_6_6
# )
# #un25
# SP2_fixtures_yc$sp2_un25 <- (
#   SP2_fixtures_yc$sp2_0_0 + SP2_fixtures_yc$sp2_1_0 + SP2_fixtures_yc$sp2_0_1 + SP2_fixtures_yc$sp2_1_1 + SP2_fixtures_yc$sp2_2_0 + SP2_fixtures_yc$sp2_0_2
# )
# #odds
# SP2_fixtures_yc$sp2_ov25_odds <- round((1/SP2_fixtures_yc$sp2_ov25),digits = 2)
# SP2_fixtures_yc$sp2_un25_odds <- round((1/SP2_fixtures_yc$sp2_un25),digits = 2)
#
# SP2_fixtures_yc$sp2_ov25_odds
# SP2_fixtures_yc$sp2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP2_fixtures_yc$sp2_ov25 <- percent(SP2_fixtures_yc$sp2_ov25, accuracy = 0.1)
#
# SP2_fixtures_yc$sp2_un25 <- percent(SP2_fixtures_yc$sp2_un25, accuracy = 0.1)
# SP2_fixtures_yc$sp2_pscore <- paste(round(SP2_fixtures_yc$sp2_xHYC,digits = 0),round(SP2_fixtures_yc$sp2_xAYC,digits = 0),sep = "-")
# #write out
# #write.xlsx(SP2_fixtures,'Divisions/SP2.xlsx',sheetName = "SP2", append = TRUE)
# #################################################################################################################
# #T1
# HomeTeam_t1_yc <- rep(t1_teams, each = length(t1_teams))
# AwayTeam_t1_yc <- rep(t1_teams, length(t1_teams))
# T1_fixtures_yc <- cbind(HomeTeam_t1_yc,AwayTeam_t1_yc)
# T1_fixtures_yc <- as.data.frame(T1_fixtures_yc)
# T1_fixtures_yc <- T1_fixtures_yc[!T1_fixtures_yc$HomeTeam_t1_yc == T1_fixtures_yc$AwayTeam_t1_yc,]
# rownames(T1_fixtures_yc) <- NULL
# T1_fixtures_yc$Div <- "T1"
# T1_fixtures_yc <- T1_fixtures_yc[,c(3,1,2)]
#
# T1_fixtures_yc$avg_HY_t1 <- t1_avg_HY
#
# T1_fixtures_yc$t1_homeyas <- rep(t1_home_yas,each = length(t1_teams)-1)
#
# t1_awayyds_lookup <- cbind(t1_teams,t1_away_yds)
#
# t1_awayyds_lookup <- as.data.frame(t1_awayyds_lookup)
#
# colnames(t1_awayyds_lookup) <- c("AwayTeam_t1_yc","t1_awayyds")
#
#
# require('RH2')
# T1_fixtures_yc$t1_awayyds <- sqldf("SELECT t1_awayyds_lookup.t1_awayyds FROM t1_awayyds_lookup INNER JOIN T1_fixtures_yc ON t1_awayyds_lookup.AwayTeam_t1_yc = T1_fixtures_yc.AwayTeam_t1_yc")
#
# T1_fixtures_yc$avg_AY_t1 <- t1_avg_AY
#
# t1_awayyas_lookup <- cbind(t1_teams,t1_away_yas)
#
# t1_awayyas_lookup <- as.data.frame(t1_awayyas_lookup)
#
# colnames(t1_awayyas_lookup) <- c("AwayTeam_t1_yc","t1_awayyas")
#
# T1_fixtures_yc$t1_awayyas <- sqldf("SELECT t1_awayyas_lookup.t1_awayyas FROM t1_awayyas_lookup INNER JOIN T1_fixtures_yc ON t1_awayyas_lookup.AwayTeam_t1_yc = T1_fixtures_yc.AwayTeam_t1_yc")
#
# T1_fixtures_yc$t1_homeyds <- rep(t1_home_yds,each = length(t1_teams)-1)
#
# T1_fixtures_yc$t1_awayyds <- as.numeric(unlist(T1_fixtures_yc$t1_awayyds))
# #xGH
# T1_fixtures_yc$t1_xHYC <- T1_fixtures_yc$avg_HY_t1 * T1_fixtures_yc$t1_homeyas * T1_fixtures_yc$t1_awayyds
# #xGA
#
# T1_fixtures_yc$t1_awayyas <- as.numeric(unlist(T1_fixtures_yc$t1_awayyas))
#
# T1_fixtures_yc$t1_xAYC <- T1_fixtures_yc$avg_AY_t1 * T1_fixtures_yc$t1_awayyas * T1_fixtures_yc$t1_homeyds
#
# T1_fixtures_yc$t1_0_0 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_0 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_0_1 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_1 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_0 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_0_2 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_2 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_1 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_2 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_3 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_0 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_1 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_2 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_0_3 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_3 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_3 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_4 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_0 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_1 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_2 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_3 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_0_4 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_4 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_4 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_4 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_5 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_0 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_1 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_2 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_3 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_4 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_0_5 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_5 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_5 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_5 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_5 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_6 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_0 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(0,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_1 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(1,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_2 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(2,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_3 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(3,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_4 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(4,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_6_5 <- round(stats::dpois(6,T1_fixtures_yc$t1_xHYC) * stats::dpois(5,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_0_6 <- round(stats::dpois(0,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_1_6 <- round(stats::dpois(1,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_2_6 <- round(stats::dpois(2,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_3_6 <- round(stats::dpois(3,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_4_6 <- round(stats::dpois(4,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# T1_fixtures_yc$t1_5_6 <- round(stats::dpois(5,T1_fixtures_yc$t1_xHYC) * stats::dpois(6,T1_fixtures_yc$t1_xAYC), digits = 4)
# #Home win
# T1_fixtures_yc$t1_H <- (
#   T1_fixtures_yc$t1_1_0 + T1_fixtures_yc$t1_2_0 + T1_fixtures_yc$t1_2_1 + T1_fixtures_yc$t1_3_0 + T1_fixtures_yc$t1_3_1 +
#     T1_fixtures_yc$t1_3_2 + T1_fixtures_yc$t1_4_0 + T1_fixtures_yc$t1_4_1 + T1_fixtures_yc$t1_4_2 + T1_fixtures_yc$t1_4_3 +
#     T1_fixtures_yc$t1_5_0 + T1_fixtures_yc$t1_5_1 + T1_fixtures_yc$t1_5_2 + T1_fixtures_yc$t1_5_3 + T1_fixtures_yc$t1_5_4 +
#     T1_fixtures_yc$t1_6_0 + T1_fixtures_yc$t1_6_1 + T1_fixtures_yc$t1_6_2 + T1_fixtures_yc$t1_6_3 + T1_fixtures_yc$t1_6_4 +
#     T1_fixtures_yc$t1_6_5
# )
#
# T1_fixtures_yc$t1_H <- percent(T1_fixtures_yc$t1_H, accuracy = 0.1)
#
# #Draw
# T1_fixtures_yc$t1_D <- (
#
#   T1_fixtures_yc$t1_0_0 + T1_fixtures_yc$t1_1_1 + T1_fixtures_yc$t1_2_2 + T1_fixtures_yc$t1_3_3 + T1_fixtures_yc$t1_4_4 +
#     T1_fixtures_yc$t1_5_5 + T1_fixtures_yc$t1_6_6
# )
#
# T1_fixtures_yc$t1_D <- percent(T1_fixtures_yc$t1_D, accuracy = 0.1)
#
# #Away
#
# T1_fixtures_yc$t1_A <- (
#   T1_fixtures_yc$t1_0_1 + T1_fixtures_yc$t1_0_2 + T1_fixtures_yc$t1_1_2 + T1_fixtures_yc$t1_0_3 + T1_fixtures_yc$t1_1_3 +
#     T1_fixtures_yc$t1_2_3 + T1_fixtures_yc$t1_0_4 + T1_fixtures_yc$t1_1_4 + T1_fixtures_yc$t1_2_4 + T1_fixtures_yc$t1_3_4 +
#     T1_fixtures_yc$t1_0_5 + T1_fixtures_yc$t1_1_5 + T1_fixtures_yc$t1_2_5 + T1_fixtures_yc$t1_3_5 + T1_fixtures_yc$t1_4_5 +
#     T1_fixtures_yc$t1_0_6 + T1_fixtures_yc$t1_1_6 + T1_fixtures_yc$t1_2_6 + T1_fixtures_yc$t1_3_6 + T1_fixtures_yc$t1_4_6 +
#     T1_fixtures_yc$t1_5_6
# )
#
# T1_fixtures_yc$t1_A <- percent(T1_fixtures_yc$t1_A, accuracy = 0.1)
#
# #ov25
# T1_fixtures_yc$t1_ov25 <- (
#   T1_fixtures_yc$t1_2_1 + T1_fixtures_yc$t1_1_2 + T1_fixtures_yc$t1_2_2 + T1_fixtures_yc$t1_3_0 + T1_fixtures_yc$t1_3_1 +
#     T1_fixtures_yc$t1_3_2 + T1_fixtures_yc$t1_0_3 + T1_fixtures_yc$t1_1_3 + T1_fixtures_yc$t1_2_3 + T1_fixtures_yc$t1_3_3 +
#     T1_fixtures_yc$t1_4_0 + T1_fixtures_yc$t1_4_1 + T1_fixtures_yc$t1_4_2 + T1_fixtures_yc$t1_4_3 + T1_fixtures_yc$t1_0_4 +
#     T1_fixtures_yc$t1_1_4 + T1_fixtures_yc$t1_2_4 + T1_fixtures_yc$t1_3_4 + T1_fixtures_yc$t1_4_4 + T1_fixtures_yc$t1_5_0 +
#     T1_fixtures_yc$t1_5_1 + T1_fixtures_yc$t1_5_2 + T1_fixtures_yc$t1_5_3 + T1_fixtures_yc$t1_5_4 + T1_fixtures_yc$t1_0_5 +
#     T1_fixtures_yc$t1_1_5 + T1_fixtures_yc$t1_2_5 + T1_fixtures_yc$t1_3_5 + T1_fixtures_yc$t1_4_5 + T1_fixtures_yc$t1_5_5 +
#     T1_fixtures_yc$t1_6_0 + T1_fixtures_yc$t1_6_1 + T1_fixtures_yc$t1_6_2 + T1_fixtures_yc$t1_6_3 + T1_fixtures_yc$t1_6_4 +
#     T1_fixtures_yc$t1_6_5 + T1_fixtures_yc$t1_0_6 + T1_fixtures_yc$t1_1_6 + T1_fixtures_yc$t1_2_6 + T1_fixtures_yc$t1_3_6 +
#     T1_fixtures_yc$t1_4_6 + T1_fixtures_yc$t1_5_6 + T1_fixtures_yc$t1_6_6
# )
# #un25
# T1_fixtures_yc$t1_un25 <- (
#   T1_fixtures_yc$t1_0_0 + T1_fixtures_yc$t1_1_0 + T1_fixtures_yc$t1_0_1 + T1_fixtures_yc$t1_1_1 + T1_fixtures_yc$t1_2_0 + T1_fixtures_yc$t1_0_2
# )
# #odds
# T1_fixtures_yc$t1_ov25_odds <- round((1/T1_fixtures_yc$t1_ov25),digits = 2)
# T1_fixtures_yc$t1_un25_odds <- round((1/T1_fixtures_yc$t1_un25),digits = 2)
#
# T1_fixtures_yc$t1_ov25_odds
# T1_fixtures_yc$t1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# T1_fixtures_yc$t1_ov25 <- percent(T1_fixtures_yc$t1_ov25, accuracy = 0.1)
#
# T1_fixtures_yc$t1_un25 <- percent(T1_fixtures_yc$t1_un25, accuracy = 0.1)
# T1_fixtures_yc$t1_pscore <- paste(round(T1_fixtures_yc$t1_xHYC,digits = 0),round(T1_fixtures_yc$t1_xAYC,digits = 0),sep = "-")
# #write out
#write.xlsx(T1_fixtures,'Divisions/T1.xlsx',sheetName = "T1", append = TRUE)
#################################################################################################################

















