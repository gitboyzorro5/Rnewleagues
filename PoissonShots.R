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
mls_T_HST <- sum(mls_home_hst$x)
arg_T_HST <- sum(arg_home_hst$x)
# d2_T_HST <- sum(d2_home_hst$x)
# e0_T_HST <- sum(e0_home_hst$x)
# e1_T_HST <- sum(e1_home_hst$x)
# e2_T_HST <- sum(e2_home_hst$x)
# e3_T_HST <- sum(e3_home_hst$x)
# ec_T_HST <- sum(ec_home_hst$x)
# f1_T_HST <- sum(f1_home_hst$x)
# f2_T_HST <- sum(f2_home_hst$x)
# g1_T_HST <- sum(g1_home_hst$x)
# i1_T_HST <- sum(i1_home_hst$x)
# i2_T_HST <- sum(i2_home_hst$x)
# n1_T_HST <- sum(n1_home_hst$x)
# p1_T_HST <- sum(p1_home_hst$x)
# sc0_T_HST <- sum(sc0_home_hst$x)
# sc1_T_HST <- sum(sc1_home_hst$x)
# sc2_T_HST <- sum(sc2_home_hst$x)
# sc3_T_HST <- sum(sc3_home_hst$x)
# sp1_T_HST <- sum(sp1_home_hst$x)
# sp2_T_HST <- sum(sp2_home_hst$x)
# t1_T_HST <- sum(t1_home_hst$x)
#calculate average home goal

mls_avg_HST <- round(mls_T_HST /mls_GP, digits = 4)
arg_avg_HST <- round(arg_T_HST /arg_GP, digits = 4)
# d2_avg_HST <- round(d2_T_HST /d2_GP, digits = 4)
# e0_avg_HST <- round(e0_T_HST /e0_GP, digits = 4)
# e1_avg_HST <- round(e1_T_HST /e1_GP, digits = 4)
# e2_avg_HST <- round(e2_T_HST /e2_GP, digits = 4)
# e3_avg_HST <- round(e3_T_HST /e3_GP, digits = 4)
# ec_avg_HST <- round(ec_T_HST /ec_GP, digits = 4)
# f1_avg_HST <- round(f1_T_HST /f1_GP, digits = 4)
# f2_avg_HST <- round(f2_T_HST /f2_GP, digits = 4)
# g1_avg_HST <- round(g1_T_HST /g1_GP, digits = 4)
# i1_avg_HST <- round(i1_T_HST /i1_GP, digits = 4)
# i2_avg_HST <- round(i2_T_HST /i2_GP, digits = 4)
# n1_avg_HST <- round(n1_T_HST /n1_GP, digits = 4)
# p1_avg_HST <- round(p1_T_HST /p1_GP, digits = 4)
# sc0_avg_HST <- round(sc0_T_HST /sc0_GP, digits = 4)
# sc1_avg_HST <- round(sc1_T_HST /sc1_GP, digits = 4)
# sc2_avg_HST <- round(sc2_T_HST /sc2_GP, digits = 4)
# sc3_avg_HST <- round(sc3_T_HST /sc3_GP, digits = 4)
# sp1_avg_HST <- round(sp1_T_HST /sp1_GP, digits = 4)
# sp2_avg_HST <- round(sp2_T_HST /sp2_GP, digits = 4)
# t1_avg_HST <- round(t1_T_HST /t1_GP, digits = 4)
############################################################
#Calculate total away goals for each division
mls_T_AST <- sum(mls_away_ast$x)
arg_T_AST <- sum(arg_away_ast$x)
# d2_T_AST <- sum(d2_away_ast$x)
# e0_T_AST <- sum(e0_away_ast$x)
# e1_T_AST <- sum(e1_away_ast$x)
# e2_T_AST <- sum(e2_away_ast$x)
# e3_T_AST <- sum(e3_away_ast$x)
# ec_T_AST <- sum(ec_away_ast$x)
# f1_T_AST <- sum(f1_away_ast$x)
# f2_T_AST <- sum(f2_away_ast$x)
# g1_T_AST <- sum(g1_away_ast$x)
# i1_T_AST <- sum(i1_away_ast$x)
# i2_T_AST <- sum(i2_away_ast$x)
# n1_T_AST <- sum(n1_away_ast$x)
# p1_T_AST <- sum(p1_away_ast$x)
# sc0_T_AST <- sum(sc0_away_ast$x)
# sc1_T_AST <- sum(sc1_away_ast$x)
# sc2_T_AST <- sum(sc2_away_ast$x)
# sc3_T_AST <- sum(sc3_away_ast$x)
# sp1_T_AST <- sum(sp1_away_ast$x)
# sp2_T_AST <- sum(sp2_away_ast$x)
# t1_T_AST <- sum(t1_away_ast$x)
#calculate average away goal

mls_avg_AST <- round(mls_T_AST /mls_GP, digits = 4)
arg_avg_AST <- round(arg_T_AST /arg_GP, digits = 4)
# d2_avg_AST <- round(d2_T_AST /d2_GP, digits = 4)
# e0_avg_AST <- round(e0_T_AST /e0_GP, digits = 4)
# e1_avg_AST <- round(e1_T_AST /e1_GP, digits = 4)
# e2_avg_AST <- round(e2_T_AST /e2_GP, digits = 4)
# e3_avg_AST <- round(e3_T_AST /e3_GP, digits = 4)
# ec_avg_AST <- round(ec_T_AST /ec_GP, digits = 4)
# f1_avg_AST <- round(f1_T_AST /f1_GP, digits = 4)
# f2_avg_AST <- round(f2_T_AST /f2_GP, digits = 4)
# g1_avg_AST <- round(g1_T_AST /g1_GP, digits = 4)
# i1_avg_AST <- round(i1_T_AST /i1_GP, digits = 4)
# i2_avg_AST <- round(i2_T_AST /i2_GP, digits = 4)
# n1_avg_AST <- round(n1_T_AST /n1_GP, digits = 4)
# p1_avg_AST <- round(p1_T_AST /p1_GP, digits = 4)
# sc0_avg_AST <- round(sc0_T_AST /sc0_GP, digits = 4)
# sc1_avg_AST <- round(sc1_T_AST /sc1_GP, digits = 4)
# sc2_avg_AST <- round(sc2_T_AST /sc2_GP, digits = 4)
# sc3_avg_AST <- round(sc3_T_AST /sc3_GP, digits = 4)
# sp1_avg_AST <- round(sp1_T_AST /sp1_GP, digits = 4)
# sp2_avg_AST <- round(sp2_T_AST /sp2_GP, digits = 4)
# t1_avg_AST <- round(t1_T_AST /t1_GP, digits = 4)
#get total home goals and total home games played for each division
#calculate home attack strength

mls_home_sotas <- round(((mls_home_hst$x/mls_home_games))/mls_avg_HST, digits = 4)
arg_home_sotas <- round(((arg_home_hst$x/arg_home_games))/arg_avg_HST, digits = 4)
# d2_home_sotas <- round(((d2_home_hst$x/d2_home_games))/d2_avg_HST, digits = 4)
# e0_home_sotas <- round(((e0_home_hst$x/e0_home_games))/e0_avg_HST, digits = 4)
# e1_home_sotas <- round(((e1_home_hst$x/e1_home_games))/e1_avg_HST, digits = 4)
# e2_home_sotas <- round(((e2_home_hst$x/e2_home_games))/e2_avg_HST, digits = 4)
# e3_home_sotas <- round(((e3_home_hst$x/e3_home_games))/e3_avg_HST, digits = 4)
# ec_home_sotas <- round(((ec_home_hst$x/ec_home_games))/ec_avg_HST, digits = 4)
# f1_home_sotas <- round(((f1_home_hst$x/f1_home_games))/f1_avg_HST, digits = 4)
# f2_home_sotas <- round(((f2_home_hst$x/f2_home_games))/f2_avg_HST, digits = 4)
# g1_home_sotas <- round(((g1_home_hst$x/g1_home_games))/g1_avg_HST, digits = 4)
# i1_home_sotas <- round(((i1_home_hst$x/i1_home_games))/i1_avg_HST, digits = 4)
# i2_home_sotas <- round(((i2_home_hst$x/i2_home_games))/i2_avg_HST, digits = 4)
# n1_home_sotas <- round(((n1_home_hst$x/n1_home_games))/n1_avg_HST, digits = 4)
# p1_home_sotas <- round(((p1_home_hst$x/p1_home_games))/p1_avg_HST, digits = 4)
# sc0_home_sotas <- round(((sc0_home_hst$x/sc0_home_games))/sc0_avg_HST, digits = 4)
# sc1_home_sotas <- round(((sc1_home_hst$x/sc1_home_games))/sc1_avg_HST, digits = 4)
# sc2_home_sotas <- round(((sc2_home_hst$x/sc2_home_games))/sc2_avg_HST, digits = 4)
# sc3_home_sotas <- round(((sc3_home_hst$x/sc3_home_games))/sc3_avg_HST, digits = 4)
# sp1_home_sotas <- round(((sp1_home_hst$x/sp1_home_games))/sp1_avg_HST, digits = 4)
# sp2_home_sotas <- round(((sp2_home_hst$x/sp2_home_games))/sp2_avg_HST, digits = 4)
# t1_home_sotas <- round(((t1_home_hst$x/t1_home_games))/t1_avg_HST, digits = 4)
#calculate away attack strength
mls_away_sotas <- round(((mls_away_ast$x/mls_away_games))/mls_avg_AST, digits = 4)
arg_away_sotas <- round(((arg_away_ast$x/arg_away_games))/arg_avg_AST, digits = 4)
# d2_away_sotas <- round(((d2_away_ast$x/d2_away_games))/d2_avg_AST, digits = 4)
# e0_away_sotas <- round(((e0_away_ast$x/e0_away_games))/e0_avg_AST, digits = 4)
# e1_away_sotas <- round(((e1_away_ast$x/e1_away_games))/e1_avg_AST, digits = 4)
# e2_away_sotas <- round(((e2_away_ast$x/e2_away_games))/e2_avg_AST, digits = 4)
# e3_away_sotas <- round(((e3_away_ast$x/e3_away_games))/e3_avg_AST, digits = 4)
# ec_away_sotas <- round(((ec_away_ast$x/ec_away_games))/ec_avg_AST, digits = 4)
# f1_away_sotas <- round(((f1_away_ast$x/f1_away_games))/f1_avg_AST, digits = 4)
# f2_away_sotas <- round(((f2_away_ast$x/f2_away_games))/f2_avg_AST, digits = 4)
# g1_away_sotas <- round(((g1_away_ast$x/g1_away_games))/g1_avg_AST, digits = 4)
# i1_away_sotas <- round(((i1_away_ast$x/i1_away_games))/i1_avg_AST, digits = 4)
# i2_away_sotas <- round(((i2_away_ast$x/i2_away_games))/i2_avg_AST, digits = 4)
# n1_away_sotas <- round(((n1_away_ast$x/n1_away_games))/n1_avg_AST, digits = 4)
# p1_away_sotas <- round(((p1_away_ast$x/p1_away_games))/p1_avg_AST, digits = 4)
# sc0_away_sotas <- round(((sc0_away_ast$x/sc0_away_games))/sc0_avg_AST, digits = 4)
# sc1_away_sotas <- round(((sc1_away_ast$x/sc1_away_games))/sc1_avg_AST, digits = 4)
# sc2_away_sotas <- round(((sc2_away_ast$x/sc2_away_games))/sc2_avg_AST, digits = 4)
# sc3_away_sotas <- round(((sc3_away_ast$x/sc3_away_games))/sc3_avg_AST, digits = 4)
# sp1_away_sotas <- round(((sp1_away_ast$x/sp1_away_games))/sp1_avg_AST, digits = 4)
# sp2_away_sotas <- round(((sp2_away_ast$x/sp2_away_games))/sp2_avg_AST, digits = 4)
# t1_away_sotas <- round(((t1_away_ast$x/t1_away_games))/t1_avg_AST, digits = 4)
################################################################################
#get average home concede and away concede
mls_avg_HSC <- round(mls_T_AST /mls_GP, digits = 4)
arg_avg_HSC <- round(arg_T_AST /arg_GP, digits = 4)
# d2_avg_HSC <- round(d2_T_AST /d2_GP, digits = 4)
# e0_avg_HSC <- round(e0_T_AST /e0_GP, digits = 4)
# e1_avg_HSC <- round(e1_T_AST /e1_GP, digits = 4)
# e2_avg_HSC <- round(e2_T_AST /e2_GP, digits = 4)
# e3_avg_HSC <- round(e3_T_AST /e3_GP, digits = 4)
# ec_avg_HSC <- round(ec_T_AST /ec_GP, digits = 4)
# f1_avg_HSC <- round(f1_T_AST /f1_GP, digits = 4)
# f2_avg_HSC <- round(f2_T_AST /f2_GP, digits = 4)
# g1_avg_HSC <- round(g1_T_AST /g1_GP, digits = 4)
# i1_avg_HSC <- round(i1_T_AST /i1_GP, digits = 4)
# i2_avg_HSC <- round(i2_T_AST /i2_GP, digits = 4)
# n1_avg_HSC <- round(n1_T_AST /n1_GP, digits = 4)
# p1_avg_HSC <- round(p1_T_AST /p1_GP, digits = 4)
# sc0_avg_HSC <- round(sc0_T_AST /sc0_GP, digits = 4)
# sc1_avg_HSC <- round(sc1_T_AST /sc1_GP, digits = 4)
# sc2_avg_HSC <- round(sc2_T_AST /sc2_GP, digits = 4)
# sc3_avg_HSC <- round(sc3_T_AST /sc3_GP, digits = 4)
# sp1_avg_HSC <- round(sp1_T_AST /sp1_GP, digits = 4)
# sp2_avg_HSC <- round(sp2_T_AST /sp2_GP, digits = 4)
# t1_avg_HSC <- round(t1_T_AST /t1_GP, digits = 4)
#avg away concede
mls_avg_ASC <- round(mls_T_HST /mls_GP, digits = 4)
arg_avg_ASC <- round(arg_T_HST /arg_GP, digits = 4)
# d2_avg_ASC <- round(d2_T_HST /d2_GP, digits = 4)
# e0_avg_ASC <- round(e0_T_HST /e0_GP, digits = 4)
# e1_avg_ASC <- round(e1_T_HST /e1_GP, digits = 4)
# e2_avg_ASC <- round(e2_T_HST /e2_GP, digits = 4)
# e3_avg_ASC <- round(e3_T_HST /e3_GP, digits = 4)
# ec_avg_ASC <- round(ec_T_HST /ec_GP, digits = 4)
# f1_avg_ASC <- round(f1_T_HST /f1_GP, digits = 4)
# f2_avg_ASC <- round(f2_T_HST /f2_GP, digits = 4)
# g1_avg_ASC <- round(g1_T_HST /g1_GP, digits = 4)
# i1_avg_ASC <- round(i1_T_HST /i1_GP, digits = 4)
# i2_avg_ASC <- round(i2_T_HST /i2_GP, digits = 4)
# n1_avg_ASC <- round(n1_T_HST /n1_GP, digits = 4)
# p1_avg_ASC <- round(p1_T_HST /p1_GP, digits = 4)
# sc0_avg_ASC <- round(sc0_T_HST /sc0_GP, digits = 4)
# sc1_avg_ASC <- round(sc1_T_HST /sc1_GP, digits = 4)
# sc2_avg_ASC <- round(sc2_T_HST /sc2_GP, digits = 4)
# sc3_avg_ASC <- round(sc3_T_HST /sc3_GP, digits = 4)
# sp1_avg_ASC <- round(sp1_T_HST /sp1_GP, digits = 4)
# sp2_avg_ASC <- round(sp2_T_HST /sp2_GP, digits = 4)
# t1_avg_ASC <- round(t1_T_HST /t1_GP, digits = 4)
#calculate home and away defense strength
# #home yellow cards conceded
# mls_home_fcc <- aggregate(MLS$AF, by = list(MLS$HomeTeam), FUN = sum)
# mls_away_fcc <- aggregate(MLS$HF, by = list(MLS$AwayTeam), FUN = sum)
# arg_home_fcc <- aggregate(ARG$AF, by = list(ARG$HomeTeam), FUN = sum)
# arg_away_fcc <- aggregate(ARG$HF, by = list(ARG$AwayTeam), FUN = sum)
# d2_home_fcc <- aggregate(D2$AF, by = list(D2$HomeTeam), FUN = sum)
# d2_away_fcc <- aggregate(D2$HF, by = list(D2$AwayTeam), FUN = sum)
# e0_home_fcc <- aggregate(E0$AF, by = list(E0$HomeTeam), FUN = sum)
# e0_away_fcc <- aggregate(E0$HF, by = list(E0$AwayTeam), FUN = sum)
# e1_home_fcc <- aggregate(E1$AF, by = list(E1$HomeTeam), FUN = sum)
# e1_away_fcc <- aggregate(E1$HF, by = list(E1$AwayTeam), FUN = sum)
# e2_home_fcc <- aggregate(E2$AF, by = list(E2$HomeTeam), FUN = sum)
# e2_away_fcc <- aggregate(E2$HF, by = list(E2$AwayTeam), FUN = sum)
# e3_home_fcc <- aggregate(E3$AF, by = list(E3$HomeTeam), FUN = sum)
# e3_away_fcc <- aggregate(E3$HF, by = list(E3$AwayTeam), FUN = sum)
# ec_home_fcc <- aggregate(EC$AF, by = list(EC$HomeTeam), FUN = sum)
# ec_away_fcc <- aggregate(EC$HF, by = list(EC$AwayTeam), FUN = sum)
# f1_home_fcc <- aggregate(F1$AF, by = list(F1$HomeTeam), FUN = sum)
# f1_away_fcc <- aggregate(F1$HF, by = list(F1$AwayTeam), FUN = sum)
# f2_home_fcc <- aggregate(F2$AF, by = list(F2$HomeTeam), FUN = sum)
# f2_away_fcc <- aggregate(F2$HF, by = list(F2$AwayTeam), FUN = sum)
# g1_home_fcc <- aggregate(G1$AF, by = list(G1$HomeTeam), FUN = sum)
# g1_away_fcc <- aggregate(G1$HF, by = list(G1$AwayTeam), FUN = sum)
# i1_home_fcc <- aggregate(I1$AF, by = list(I1$HomeTeam), FUN = sum)
# i1_away_fcc <- aggregate(I1$HF, by = list(I1$AwayTeam), FUN = sum)
# i2_home_fcc <- aggregate(I2$AF, by = list(I2$HomeTeam), FUN = sum)
# i2_away_fcc <- aggregate(I2$HF, by = list(I2$AwayTeam), FUN = sum)
# n1_home_fcc <- aggregate(N1$AF, by = list(N1$HomeTeam), FUN = sum)
# n1_away_fcc <- aggregate(N1$HF, by = list(N1$AwayTeam), FUN = sum)
# p1_home_fcc <- aggregate(P1$AF, by = list(P1$HomeTeam), FUN = sum)
# p1_away_fcc <- aggregate(P1$HF, by = list(P1$AwayTeam), FUN = sum)
# sc0_home_fcc <- aggregate(SC0$AF, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_fcc <- aggregate(SC0$HF, by = list(SC0$AwayTeam), FUN = sum)
# sc1_home_fcc <- aggregate(SC1$AF, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_fcc <- aggregate(SC1$HF, by = list(SC1$AwayTeam), FUN = sum)
# sc2_home_fcc <- aggregate(SC2$AF, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_fcc <- aggregate(SC2$HF, by = list(SC2$AwayTeam), FUN = sum)
# sc3_home_fcc <- aggregate(SC3$AF, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_fcc <- aggregate(SC3$HF, by = list(SC3$AwayTeam), FUN = sum)
# sp1_home_fcc <- aggregate(SP1$AF, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_fcc <- aggregate(SP1$HF, by = list(SP1$AwayTeam), FUN = sum)
# sp2_home_fcc <- aggregate(SP2$AF, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_fcc <- aggregate(SP2$HF, by = list(SP2$AwayTeam), FUN = sum)
# t1_home_fcc <- aggregate(T1$AF, by = list(T1$HomeTeam), FUN = sum)
# t1_away_fcc <- aggregate(T1$HF, by = list(T1$AwayTeam), FUN = sum)
#home defense strength
mls_home_sods <- round(((mls_home_hsc$x/mls_home_games))/mls_avg_HSC, digits = 4)
arg_home_sods <- round(((arg_home_hsc$x/arg_home_games))/arg_avg_HSC, digits = 4)
# d2_home_sods <- round(((d2_home_hsc$x/d2_home_games))/d2_avg_HSC, digits = 4)
# e0_home_sods <- round(((e0_home_hsc$x/e0_home_games))/e0_avg_HSC, digits = 4)
# e1_home_sods <- round(((e1_home_hsc$x/e1_home_games))/e1_avg_HSC, digits = 4)
# e2_home_sods <- round(((e2_home_hsc$x/e2_home_games))/e2_avg_HSC, digits = 4)
# e3_home_sods <- round(((e3_home_hsc$x/e3_home_games))/e3_avg_HSC, digits = 4)
# ec_home_sods <- round(((ec_home_hsc$x/ec_home_games))/ec_avg_HSC, digits = 4)
# f1_home_sods <- round(((f1_home_hsc$x/f1_home_games))/f1_avg_HSC, digits = 4)
# f2_home_sods <- round(((f2_home_hsc$x/f2_home_games))/f2_avg_HSC, digits = 4)
# g1_home_sods <- round(((g1_home_hsc$x/g1_home_games))/g1_avg_HSC, digits = 4)
# i1_home_sods <- round(((i1_home_hsc$x/i1_home_games))/i1_avg_HSC, digits = 4)
# i2_home_sods <- round(((i2_home_hsc$x/i2_home_games))/i2_avg_HSC, digits = 4)
# n1_home_sods <- round(((n1_home_hsc$x/n1_home_games))/n1_avg_HSC, digits = 4)
# p1_home_sods <- round(((p1_home_hsc$x/p1_home_games))/p1_avg_HSC, digits = 4)
# sc0_home_sods <- round(((sc0_home_hsc$x/sc0_home_games))/sc0_avg_HSC, digits = 4)
# sc1_home_sods <- round(((sc1_home_hsc$x/sc1_home_games))/sc1_avg_HSC, digits = 4)
# sc2_home_sods <- round(((sc2_home_hsc$x/sc2_home_games))/sc2_avg_HSC, digits = 4)
# sc3_home_sods <- round(((sc3_home_hsc$x/sc3_home_games))/sc3_avg_HSC, digits = 4)
# sp1_home_sods <- round(((sp1_home_hsc$x/sp1_home_games))/sp1_avg_HSC, digits = 4)
# sp2_home_sods <- round(((sp2_home_hsc$x/sp2_home_games))/sp2_avg_HSC, digits = 4)
# t1_home_sods <- round(((t1_home_hsc$x/t1_home_games))/t1_avg_HSC, digits = 4)
#away defense strength
mls_away_sods <- round(((mls_away_ast$x/mls_away_games))/mls_avg_ASC, digits = 4)
arg_away_sods <- round(((arg_away_ast$x/arg_away_games))/arg_avg_ASC, digits = 4)
# d2_away_sods <- round(((d2_away_ast$x/d2_away_games))/d2_avg_ASC, digits = 4)
# e0_away_sods <- round(((e0_away_ast$x/e0_away_games))/e0_avg_ASC, digits = 4)
# e1_away_sods <- round(((e1_away_ast$x/e1_away_games))/e1_avg_ASC, digits = 4)
# e2_away_sods <- round(((e2_away_ast$x/e2_away_games))/e2_avg_ASC, digits = 4)
# e3_away_sods <- round(((e3_away_ast$x/e3_away_games))/e3_avg_ASC, digits = 4)
# ec_away_sods <- round(((ec_away_ast$x/ec_away_games))/ec_avg_ASC, digits = 4)
# f1_away_sods <- round(((f1_away_ast$x/f1_away_games))/f1_avg_ASC, digits = 4)
# f2_away_sods <- round(((f2_away_ast$x/f2_away_games))/f2_avg_ASC, digits = 4)
# g1_away_sods <- round(((g1_away_ast$x/g1_away_games))/g1_avg_ASC, digits = 4)
# i1_away_sods <- round(((i1_away_ast$x/i1_away_games))/i1_avg_ASC, digits = 4)
# i2_away_sods <- round(((i2_away_ast$x/i2_away_games))/i2_avg_ASC, digits = 4)
# n1_away_sods <- round(((n1_away_ast$x/n1_away_games))/n1_avg_ASC, digits = 4)
# p1_away_sods <- round(((p1_away_ast$x/p1_away_games))/p1_avg_ASC, digits = 4)
# sc0_away_sods <- round(((sc0_away_ast$x/sc0_away_games))/sc0_avg_ASC, digits = 4)
# sc1_away_sods <- round(((sc1_away_ast$x/sc1_away_games))/sc1_avg_ASC, digits = 4)
# sc2_away_sods <- round(((sc2_away_ast$x/sc2_away_games))/sc2_avg_ASC, digits = 4)
# sc3_away_sods <- round(((sc3_away_ast$x/sc3_away_games))/sc3_avg_ASC, digits = 4)
# sp1_away_sods <- round(((sp1_away_ast$x/sp1_away_games))/sp1_avg_ASC, digits = 4)
# sp2_away_sods <- round(((sp2_away_ast$x/sp2_away_games))/sp2_avg_ASC, digits = 4)
# t1_away_sods <- round(((t1_away_ast$x/t1_away_games))/t1_avg_ASC, digits = 4)
#############################################################################
#home poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_home_poisson_sot <- cbind(mls_division,mls_teams,mls_avg_HST,mls_home_sotas,mls_home_sods)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_home_poisson_sot <- cbind(arg_division,arg_teams,arg_avg_HST,arg_home_sotas,arg_home_sods)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_home_poisson_sot <- cbind(d2_division,d2_teams,d2_avg_HST,d2_home_sotas,d2_home_sods)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_home_poisson_sot <- cbind(e0_division,e0_teams,e0_avg_HST,e0_home_sotas,e0_home_sods)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_home_poisson_sot <- cbind(e1_division,e1_teams,e1_avg_HST,e1_home_sotas,e1_home_sods)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_home_poisson_sot <- cbind(e2_division,e2_teams,e2_avg_HST,e2_home_sotas,e2_home_sods)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_home_poisson_sot <- cbind(e3_division,e3_teams,e3_avg_HST,e3_home_sotas,e3_home_sods)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_home_poisson_sot <- cbind(ec_division,ec_teams,ec_avg_HST,ec_home_sotas,ec_home_sods)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_home_poisson_sot <- cbind(f1_division,f1_teams,f1_avg_HST,f1_home_sotas,f1_home_sods)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_home_poisson_sot <- cbind(f2_division,f2_teams,f2_avg_HST,f2_home_sotas,f2_home_sods)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_home_poisson_sot <- cbind(g1_division,g1_teams,g1_avg_HST,g1_home_sotas,g1_home_sods)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_home_poisson_sot <- cbind(i1_division,i1_teams,i1_avg_HST,i1_home_sotas,i1_home_sods)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_home_poisson_sot <- cbind(i2_division,i2_teams,i2_avg_HST,i2_home_sotas,i2_home_sods)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_home_poisson_sot <- cbind(n1_division,n1_teams,n1_avg_HST,n1_home_sotas,n1_home_sods)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_home_poisson_sot <- cbind(p1_division,p1_teams,p1_avg_HST,p1_home_sotas,p1_home_sods)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_home_poisson_sot <- cbind(sc0_division,sc0_teams,sc0_avg_HST,sc0_home_sotas,sc0_home_sods)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_home_poisson_sot <- cbind(sc1_division,sc1_teams,sc1_avg_HST,sc1_home_sotas,sc1_home_sods)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_home_poisson_sot <- cbind(sc2_division,sc2_teams,sc2_avg_HST,sc2_home_sotas,sc2_home_sods)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_home_poisson_sot <- cbind(sc3_division,sc3_teams,sc3_avg_HST,sc3_home_sotas,sc3_home_sods)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_home_poisson_sot <- cbind(sp1_division,sp1_teams,sp1_avg_HST,sp1_home_sotas,sp1_home_sods)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_home_poisson_sot <- cbind(sp2_division,sp2_teams,sp2_avg_HST,sp2_home_sotas,sp2_home_sods)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_home_poisson_sot <- cbind(t1_division,t1_teams,t1_avg_HST,t1_home_sotas,t1_home_sods)
#################################################################################
#away poisson data
#mls
mls_division <- c()
mls_division[1:length(mls_teams)] <- "MLS"
mls_away_poisson_sot <- cbind(mls_division,mls_teams,mls_avg_AST,mls_away_sotas,mls_away_sods)
#arg
arg_division <- c()
arg_division[1:length(arg_teams)] <- "ARG"
arg_away_poisson_sot <- cbind(arg_division,arg_teams,arg_avg_AST,arg_away_sotas,arg_away_sods)
#d2
# d2_division <- c()
# d2_division[1:length(d2_teams)] <- "D2"
# d2_away_poisson_sot <- cbind(d2_division,d2_teams,d2_avg_AST,d2_away_sotas,d2_away_sods)
# #e0
# e0_division <- c()
# e0_division[1:length(e0_teams)] <- "E0"
# e0_away_poisson_sot <- cbind(e0_division,e0_teams,e0_avg_AST,e0_away_sotas,e0_away_sods)
# #e1
# e1_division <- c()
# e1_division[1:length(e1_teams)] <- "E1"
# e1_away_poisson_sot <- cbind(e1_division,e1_teams,e1_avg_AST,e1_away_sotas,e1_away_sods)
# #e2
# e2_division <- c()
# e2_division[1:length(e2_teams)] <- "E2"
# e2_away_poisson_sot <- cbind(e2_division,e2_teams,e2_avg_AST,e2_away_sotas,e2_away_sods)
# #e3
# e3_division <- c()
# e3_division[1:length(e3_teams)] <- "E3"
# e3_away_poisson_sot <- cbind(e3_division,e3_teams,e3_avg_AST,e3_away_sotas,e3_away_sods)
# #ec
# ec_division <- c()
# ec_division[1:length(ec_teams)] <- "EC"
# ec_away_poisson_sot <- cbind(ec_division,ec_teams,ec_avg_AST,ec_away_sotas,ec_away_sods)
# #f1
# f1_division <- c()
# f1_division[1:length(f1_teams)] <- "F1"
# f1_away_poisson_sot <- cbind(f1_division,f1_teams,f1_avg_AST,f1_away_sotas,f1_away_sods)
# #f2
# f2_division <- c()
# f2_division[1:length(f2_teams)] <- "F2"
# f2_away_poisson_sot <- cbind(f2_division,f2_teams,f2_avg_AST,f2_away_sotas,f2_away_sods)
# #g1
# g1_division <- c()
# g1_division[1:length(g1_teams)] <- "G1"
# g1_away_poisson_sot <- cbind(g1_division,g1_teams,g1_avg_AST,g1_away_sotas,g1_away_sods)
# #i1
# i1_division <- c()
# i1_division[1:length(i1_teams)] <- "I1"
# i1_away_poisson_sot <- cbind(i1_division,i1_teams,i1_avg_AST,i1_away_sotas,i1_away_sods)
# #i2
# i2_division <- c()
# i2_division[1:length(i2_teams)] <- "I2"
# i2_away_poisson_sot <- cbind(i2_division,i2_teams,i2_avg_AST,i2_away_sotas,i2_away_sods)
# #n1
# n1_division <- c()
# n1_division[1:length(n1_teams)] <- "N1"
# n1_away_poisson_sot <- cbind(n1_division,n1_teams,n1_avg_AST,n1_away_sotas,n1_away_sods)
# #p1
# p1_division <- c()
# p1_division[1:length(p1_teams)] <- "P1"
# p1_away_poisson_sot <- cbind(p1_division,p1_teams,p1_avg_AST,p1_away_sotas,p1_away_sods)
# #sc0
# sc0_division <- c()
# sc0_division[1:length(sc0_teams)] <- "SC0"
# sc0_away_poisson_sot <- cbind(sc0_division,sc0_teams,sc0_avg_AST,sc0_away_sotas,sc0_away_sods)
# #sc1
# sc1_division <- c()
# sc1_division[1:length(sc1_teams)] <- "SC1"
# sc1_away_poisson_sot <- cbind(sc1_division,sc1_teams,sc1_avg_AST,sc1_away_sotas,sc1_away_sods)
# #sc2
# sc2_division <- c()
# sc2_division[1:length(sc2_teams)] <- "SC2"
# sc2_away_poisson_sot <- cbind(sc2_division,sc2_teams,sc2_avg_AST,sc2_away_sotas,sc2_away_sods)
# #sc3
# sc3_division <- c()
# sc3_division[1:length(sc3_teams)] <- "SC3"
# sc3_away_poisson_sot <- cbind(sc3_division,sc3_teams,sc3_avg_AST,sc3_away_sotas,sc3_away_sods)
# #sp1
# sp1_division <- c()
# sp1_division[1:length(sp1_teams)] <- "SP1"
# sp1_away_poisson_sot <- cbind(sp1_division,sp1_teams,sp1_avg_AST,sp1_away_sotas,sp1_away_sods)
# #sp2
# sp2_division <- c()
# sp2_division[1:length(sp2_teams)] <- "SP2"
# sp2_away_poisson_sot <- cbind(sp2_division,sp2_teams,sp2_avg_AST,sp2_away_sotas,sp2_away_sods)
# #t1
# t1_division <- c()
# t1_division[1:length(t1_teams)] <- "T1"
# t1_away_poisson_sot <- cbind(t1_division,t1_teams,t1_avg_AST,t1_away_sotas,t1_away_sods)
#create home and away csv
home_poisson_sot <- rbind(mls_home_poisson_sot,arg_home_poisson_sot)
away_poisson_sot <- rbind(mls_away_poisson_sot,arg_away_poisson_sot)
# #delete current
# unlink("R_home.csv")
# unlink("R_away.csv")
# #write another one
# write.csv(home_poisson,'R_home.csv')
# write.csv(away_poisson,'R_away.csv')
#MLS
HomeTeam_mls_sot <- rep(mls_teams, each = length(mls_teams))
AwayTeam_mls_sot <- rep(mls_teams, length(mls_teams))
MLS_fixtures_sot <- cbind(HomeTeam_mls_sot,AwayTeam_mls_sot)
MLS_fixtures_sot <- as.data.frame(MLS_fixtures_sot)
MLS_fixtures_sot <- MLS_fixtures_sot[!MLS_fixtures_sot$HomeTeam_mls_sot == MLS_fixtures_sot$AwayTeam_mls_sot,]
rownames(MLS_fixtures_sot) <- NULL
MLS_fixtures_sot$Div <- "MLS"
MLS_fixtures_sot <- MLS_fixtures_sot[,c(3,1,2)]

MLS_fixtures_sot$avg_HST_mls <- mls_avg_HST

MLS_fixtures_sot$mls_homesotas <- rep(mls_home_sotas,each = length(mls_teams)-1)

mls_awaysods_lookup <- cbind(mls_teams,mls_away_sods)

mls_awaysods_lookup <- as.data.frame(mls_awaysods_lookup)

colnames(mls_awaysods_lookup) <- c("AwayTeam_mls_sot","mls_awaysods")


require('RH2')
MLS_fixtures_sot$mls_awaysods <- sqldf("SELECT mls_awaysods_lookup.mls_awaysods FROM mls_awaysods_lookup INNER JOIN MLS_fixtures_sot ON mls_awaysods_lookup.AwayTeam_mls_sot = MLS_fixtures_sot.AwayTeam_mls_sot")

MLS_fixtures_sot$avg_AST_mls <- mls_avg_AST

mls_awaysotas_lookup <- cbind(mls_teams,mls_away_sotas)

mls_awaysotas_lookup <- as.data.frame(mls_awaysotas_lookup)

colnames(mls_awaysotas_lookup) <- c("AwayTeam_mls_sot","mls_awaysotas")

MLS_fixtures_sot$mls_awaysotas <- sqldf("SELECT mls_awaysotas_lookup.mls_awaysotas FROM mls_awaysotas_lookup INNER JOIN MLS_fixtures_sot ON mls_awaysotas_lookup.AwayTeam_mls_sot = MLS_fixtures_sot.AwayTeam_mls_sot")

MLS_fixtures_sot$mls_homesods <- rep(mls_home_sods,each = length(mls_teams)-1)

MLS_fixtures_sot$mls_awaysods <- as.numeric(unlist(MLS_fixtures_sot$mls_awaysods))
#xGH
MLS_fixtures_sot$mls_xHST <- MLS_fixtures_sot$avg_HST_mls * MLS_fixtures_sot$mls_homesotas * MLS_fixtures_sot$mls_awaysods
#xGA

MLS_fixtures_sot$mls_awaysotas <- as.numeric(unlist(MLS_fixtures_sot$mls_awaysotas))

MLS_fixtures_sot$mls_xAST <- MLS_fixtures_sot$avg_AST_mls * MLS_fixtures_sot$mls_awaysotas * MLS_fixtures_sot$mls_homesods

MLS_fixtures_sot$mls_0_0 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_0 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_1 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_1 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_0 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_2 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_2 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_1 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_2 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_3 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_0 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_1 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_2 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_3 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_3 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_3 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_4 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_0 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_1 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_2 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_3 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_4 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_4 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_4 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_4 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_5 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_0 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_1 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_2 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_3 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_4 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_5 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_5 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_5 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_5 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_5 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_6 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_0 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(0,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_1 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(1,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_2 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(2,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_3 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(3,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_4 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(4,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_6_5 <- round(stats::dpois(6,MLS_fixtures_sot$mls_xHST) * stats::dpois(5,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_0_6 <- round(stats::dpois(0,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_1_6 <- round(stats::dpois(1,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_2_6 <- round(stats::dpois(2,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_3_6 <- round(stats::dpois(3,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_4_6 <- round(stats::dpois(4,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
MLS_fixtures_sot$mls_5_6 <- round(stats::dpois(5,MLS_fixtures_sot$mls_xHST) * stats::dpois(6,MLS_fixtures_sot$mls_xAST), digits = 4)
#Home win
MLS_fixtures_sot$mls_H <- (
  MLS_fixtures_sot$mls_1_0 + MLS_fixtures_sot$mls_2_0 + MLS_fixtures_sot$mls_2_1 + MLS_fixtures_sot$mls_3_0 + MLS_fixtures_sot$mls_3_1 +
    MLS_fixtures_sot$mls_3_2 + MLS_fixtures_sot$mls_4_0 + MLS_fixtures_sot$mls_4_1 + MLS_fixtures_sot$mls_4_2 + MLS_fixtures_sot$mls_4_3 +
    MLS_fixtures_sot$mls_5_0 + MLS_fixtures_sot$mls_5_1 + MLS_fixtures_sot$mls_5_2 + MLS_fixtures_sot$mls_5_3 + MLS_fixtures_sot$mls_5_4 +
    MLS_fixtures_sot$mls_6_0 + MLS_fixtures_sot$mls_6_1 + MLS_fixtures_sot$mls_6_2 + MLS_fixtures_sot$mls_6_3 + MLS_fixtures_sot$mls_6_4 +
    MLS_fixtures_sot$mls_6_5
)

MLS_fixtures_sot$mls_H <- percent(MLS_fixtures_sot$mls_H, accuracy = 0.1)

#Draw
MLS_fixtures_sot$mls_D <- (

  MLS_fixtures_sot$mls_0_0 + MLS_fixtures_sot$mls_1_1 + MLS_fixtures_sot$mls_2_2 + MLS_fixtures_sot$mls_3_3 + MLS_fixtures_sot$mls_4_4 +
    MLS_fixtures_sot$mls_5_5 + MLS_fixtures_sot$mls_6_6
)

MLS_fixtures_sot$mls_D <- percent(MLS_fixtures_sot$mls_D, accuracy = 0.1)

#Away

MLS_fixtures_sot$mls_A <- (
  MLS_fixtures_sot$mls_0_1 + MLS_fixtures_sot$mls_0_2 + MLS_fixtures_sot$mls_1_2 + MLS_fixtures_sot$mls_0_3 + MLS_fixtures_sot$mls_1_3 +
    MLS_fixtures_sot$mls_2_3 + MLS_fixtures_sot$mls_0_4 + MLS_fixtures_sot$mls_1_4 + MLS_fixtures_sot$mls_2_4 + MLS_fixtures_sot$mls_3_4 +
    MLS_fixtures_sot$mls_0_5 + MLS_fixtures_sot$mls_1_5 + MLS_fixtures_sot$mls_2_5 + MLS_fixtures_sot$mls_3_5 + MLS_fixtures_sot$mls_4_5 +
    MLS_fixtures_sot$mls_0_6 + MLS_fixtures_sot$mls_1_6 + MLS_fixtures_sot$mls_2_6 + MLS_fixtures_sot$mls_3_6 + MLS_fixtures_sot$mls_4_6 +
    MLS_fixtures_sot$mls_5_6
)

MLS_fixtures_sot$mls_A <- percent(MLS_fixtures_sot$mls_A, accuracy = 0.1)

#ov25
MLS_fixtures_sot$mls_ov25 <- (
  MLS_fixtures_sot$mls_2_1 + MLS_fixtures_sot$mls_1_2 + MLS_fixtures_sot$mls_2_2 + MLS_fixtures_sot$mls_3_0 + MLS_fixtures_sot$mls_3_1 +
    MLS_fixtures_sot$mls_3_2 + MLS_fixtures_sot$mls_0_3 + MLS_fixtures_sot$mls_1_3 + MLS_fixtures_sot$mls_2_3 + MLS_fixtures_sot$mls_3_3 +
    MLS_fixtures_sot$mls_4_0 + MLS_fixtures_sot$mls_4_1 + MLS_fixtures_sot$mls_4_2 + MLS_fixtures_sot$mls_4_3 + MLS_fixtures_sot$mls_0_4 +
    MLS_fixtures_sot$mls_1_4 + MLS_fixtures_sot$mls_2_4 + MLS_fixtures_sot$mls_3_4 + MLS_fixtures_sot$mls_4_4 + MLS_fixtures_sot$mls_5_0 +
    MLS_fixtures_sot$mls_5_1 + MLS_fixtures_sot$mls_5_2 + MLS_fixtures_sot$mls_5_3 + MLS_fixtures_sot$mls_5_4 + MLS_fixtures_sot$mls_0_5 +
    MLS_fixtures_sot$mls_1_5 + MLS_fixtures_sot$mls_2_5 + MLS_fixtures_sot$mls_3_5 + MLS_fixtures_sot$mls_4_5 + MLS_fixtures_sot$mls_5_5 +
    MLS_fixtures_sot$mls_6_0 + MLS_fixtures_sot$mls_6_1 + MLS_fixtures_sot$mls_6_2 + MLS_fixtures_sot$mls_6_3 + MLS_fixtures_sot$mls_6_4 +
    MLS_fixtures_sot$mls_6_5 + MLS_fixtures_sot$mls_0_6 + MLS_fixtures_sot$mls_1_6 + MLS_fixtures_sot$mls_2_6 + MLS_fixtures_sot$mls_3_6 +
    MLS_fixtures_sot$mls_4_6 + MLS_fixtures_sot$mls_5_6 + MLS_fixtures_sot$mls_6_6
)
#un25
MLS_fixtures_sot$mls_un25 <- (
  MLS_fixtures_sot$mls_0_0 + MLS_fixtures_sot$mls_1_0 + MLS_fixtures_sot$mls_0_1 + MLS_fixtures_sot$mls_1_1 + MLS_fixtures_sot$mls_2_0 + MLS_fixtures_sot$mls_0_2
)
#odds
MLS_fixtures_sot$mls_ov25_odds <- round((1/MLS_fixtures_sot$mls_ov25),digits = 2)
MLS_fixtures_sot$mls_un25_odds <- round((1/MLS_fixtures_sot$mls_un25),digits = 2)

MLS_fixtures_sot$mls_ov25_odds
MLS_fixtures_sot$mls_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
MLS_fixtures_sot$mls_ov25 <- percent(MLS_fixtures_sot$mls_ov25, accuracy = 0.1)

MLS_fixtures_sot$mls_un25 <- percent(MLS_fixtures_sot$mls_un25, accuracy = 0.1)
MLS_fixtures_sot$mls_pssotre <- paste(round(MLS_fixtures_sot$mls_xHST,digits = 0),round(MLS_fixtures_sot$mls_xAST,digits = 0),sep = "-")
#write out

#write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
##################################################################################################################
#ARG
HomeTeam_arg_sot <- rep(arg_teams, each = length(arg_teams))
AwayTeam_arg_sot <- rep(arg_teams, length(arg_teams))
ARG_fixtures_sot <- cbind(HomeTeam_arg_sot,AwayTeam_arg_sot)
ARG_fixtures_sot <- as.data.frame(ARG_fixtures_sot)
ARG_fixtures_sot <- ARG_fixtures_sot[!ARG_fixtures_sot$HomeTeam_arg_sot == ARG_fixtures_sot$AwayTeam_arg_sot,]
rownames(ARG_fixtures_sot) <- NULL
ARG_fixtures_sot$Div <- "ARG"
ARG_fixtures_sot <- ARG_fixtures_sot[,c(3,1,2)]

ARG_fixtures_sot$avg_HST_arg <- arg_avg_HST

ARG_fixtures_sot$arg_homesotas <- rep(arg_home_sotas,each = length(arg_teams)-1)

arg_awaysods_lookup <- cbind(arg_teams,arg_away_sods)

arg_awaysods_lookup <- as.data.frame(arg_awaysods_lookup)

colnames(arg_awaysods_lookup) <- c("AwayTeam_arg_sot","arg_awaysods")


require('RH2')
ARG_fixtures_sot$arg_awaysods <- sqldf("SELECT arg_awaysods_lookup.arg_awaysods FROM arg_awaysods_lookup INNER JOIN ARG_fixtures_sot ON arg_awaysods_lookup.AwayTeam_arg_sot = ARG_fixtures_sot.AwayTeam_arg_sot")

ARG_fixtures_sot$avg_AST_arg <- arg_avg_AST

arg_awaysotas_lookup <- cbind(arg_teams,arg_away_sotas)

arg_awaysotas_lookup <- as.data.frame(arg_awaysotas_lookup)

colnames(arg_awaysotas_lookup) <- c("AwayTeam_arg_sot","arg_awaysotas")

ARG_fixtures_sot$arg_awaysotas <- sqldf("SELECT arg_awaysotas_lookup.arg_awaysotas FROM arg_awaysotas_lookup INNER JOIN ARG_fixtures_sot ON arg_awaysotas_lookup.AwayTeam_arg_sot = ARG_fixtures_sot.AwayTeam_arg_sot")

ARG_fixtures_sot$arg_homesods <- rep(arg_home_sods,each = length(arg_teams)-1)

ARG_fixtures_sot$arg_awaysods <- as.numeric(unlist(ARG_fixtures_sot$arg_awaysods))
#xGH
ARG_fixtures_sot$arg_xHST <- ARG_fixtures_sot$avg_HST_arg * ARG_fixtures_sot$arg_homesotas * ARG_fixtures_sot$arg_awaysods
#xGA

ARG_fixtures_sot$arg_awaysotas <- as.numeric(unlist(ARG_fixtures_sot$arg_awaysotas))

ARG_fixtures_sot$arg_xAST <- ARG_fixtures_sot$avg_AST_arg * ARG_fixtures_sot$arg_awaysotas * ARG_fixtures_sot$arg_homesods

ARG_fixtures_sot$arg_0_0 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_0 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_0_1 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_1 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_0 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_0_2 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_2 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_1 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_2 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_3 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_0 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_1 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_2 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_0_3 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_3 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_3 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_4 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_0 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_1 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_2 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_3 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_0_4 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_4 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_4 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_4 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_5 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_0 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_1 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_2 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_3 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_4 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_0_5 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_5 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_5 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_5 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_5 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_6 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_0 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(0,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_1 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(1,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_2 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(2,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_3 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(3,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_4 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(4,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_6_5 <- round(stats::dpois(6,ARG_fixtures_sot$arg_xHST) * stats::dpois(5,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_0_6 <- round(stats::dpois(0,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_1_6 <- round(stats::dpois(1,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_2_6 <- round(stats::dpois(2,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_3_6 <- round(stats::dpois(3,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_4_6 <- round(stats::dpois(4,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
ARG_fixtures_sot$arg_5_6 <- round(stats::dpois(5,ARG_fixtures_sot$arg_xHST) * stats::dpois(6,ARG_fixtures_sot$arg_xAST), digits = 4)
#Home win
ARG_fixtures_sot$arg_H <- (
  ARG_fixtures_sot$arg_1_0 + ARG_fixtures_sot$arg_2_0 + ARG_fixtures_sot$arg_2_1 + ARG_fixtures_sot$arg_3_0 + ARG_fixtures_sot$arg_3_1 +
    ARG_fixtures_sot$arg_3_2 + ARG_fixtures_sot$arg_4_0 + ARG_fixtures_sot$arg_4_1 + ARG_fixtures_sot$arg_4_2 + ARG_fixtures_sot$arg_4_3 +
    ARG_fixtures_sot$arg_5_0 + ARG_fixtures_sot$arg_5_1 + ARG_fixtures_sot$arg_5_2 + ARG_fixtures_sot$arg_5_3 + ARG_fixtures_sot$arg_5_4 +
    ARG_fixtures_sot$arg_6_0 + ARG_fixtures_sot$arg_6_1 + ARG_fixtures_sot$arg_6_2 + ARG_fixtures_sot$arg_6_3 + ARG_fixtures_sot$arg_6_4 +
    ARG_fixtures_sot$arg_6_5
)

ARG_fixtures_sot$arg_H <- percent(ARG_fixtures_sot$arg_H, accuracy = 0.1)

#Draw
ARG_fixtures_sot$arg_D <- (

  ARG_fixtures_sot$arg_0_0 + ARG_fixtures_sot$arg_1_1 + ARG_fixtures_sot$arg_2_2 + ARG_fixtures_sot$arg_3_3 + ARG_fixtures_sot$arg_4_4 +
    ARG_fixtures_sot$arg_5_5 + ARG_fixtures_sot$arg_6_6
)

ARG_fixtures_sot$arg_D <- percent(ARG_fixtures_sot$arg_D, accuracy = 0.1)

#Away

ARG_fixtures_sot$arg_A <- (
  ARG_fixtures_sot$arg_0_1 + ARG_fixtures_sot$arg_0_2 + ARG_fixtures_sot$arg_1_2 + ARG_fixtures_sot$arg_0_3 + ARG_fixtures_sot$arg_1_3 +
    ARG_fixtures_sot$arg_2_3 + ARG_fixtures_sot$arg_0_4 + ARG_fixtures_sot$arg_1_4 + ARG_fixtures_sot$arg_2_4 + ARG_fixtures_sot$arg_3_4 +
    ARG_fixtures_sot$arg_0_5 + ARG_fixtures_sot$arg_1_5 + ARG_fixtures_sot$arg_2_5 + ARG_fixtures_sot$arg_3_5 + ARG_fixtures_sot$arg_4_5 +
    ARG_fixtures_sot$arg_0_6 + ARG_fixtures_sot$arg_1_6 + ARG_fixtures_sot$arg_2_6 + ARG_fixtures_sot$arg_3_6 + ARG_fixtures_sot$arg_4_6 +
    ARG_fixtures_sot$arg_5_6
)

ARG_fixtures_sot$arg_A <- percent(ARG_fixtures_sot$arg_A, accuracy = 0.1)

#ov25
ARG_fixtures_sot$arg_ov25 <- (
  ARG_fixtures_sot$arg_2_1 + ARG_fixtures_sot$arg_1_2 + ARG_fixtures_sot$arg_2_2 + ARG_fixtures_sot$arg_3_0 + ARG_fixtures_sot$arg_3_1 +
    ARG_fixtures_sot$arg_3_2 + ARG_fixtures_sot$arg_0_3 + ARG_fixtures_sot$arg_1_3 + ARG_fixtures_sot$arg_2_3 + ARG_fixtures_sot$arg_3_3 +
    ARG_fixtures_sot$arg_4_0 + ARG_fixtures_sot$arg_4_1 + ARG_fixtures_sot$arg_4_2 + ARG_fixtures_sot$arg_4_3 + ARG_fixtures_sot$arg_0_4 +
    ARG_fixtures_sot$arg_1_4 + ARG_fixtures_sot$arg_2_4 + ARG_fixtures_sot$arg_3_4 + ARG_fixtures_sot$arg_4_4 + ARG_fixtures_sot$arg_5_0 +
    ARG_fixtures_sot$arg_5_1 + ARG_fixtures_sot$arg_5_2 + ARG_fixtures_sot$arg_5_3 + ARG_fixtures_sot$arg_5_4 + ARG_fixtures_sot$arg_0_5 +
    ARG_fixtures_sot$arg_1_5 + ARG_fixtures_sot$arg_2_5 + ARG_fixtures_sot$arg_3_5 + ARG_fixtures_sot$arg_4_5 + ARG_fixtures_sot$arg_5_5 +
    ARG_fixtures_sot$arg_6_0 + ARG_fixtures_sot$arg_6_1 + ARG_fixtures_sot$arg_6_2 + ARG_fixtures_sot$arg_6_3 + ARG_fixtures_sot$arg_6_4 +
    ARG_fixtures_sot$arg_6_5 + ARG_fixtures_sot$arg_0_6 + ARG_fixtures_sot$arg_1_6 + ARG_fixtures_sot$arg_2_6 + ARG_fixtures_sot$arg_3_6 +
    ARG_fixtures_sot$arg_4_6 + ARG_fixtures_sot$arg_5_6 + ARG_fixtures_sot$arg_6_6
)
#un25
ARG_fixtures_sot$arg_un25 <- (
  ARG_fixtures_sot$arg_0_0 + ARG_fixtures_sot$arg_1_0 + ARG_fixtures_sot$arg_0_1 + ARG_fixtures_sot$arg_1_1 + ARG_fixtures_sot$arg_2_0 + ARG_fixtures_sot$arg_0_2
)
#odds
ARG_fixtures_sot$arg_ov25_odds <- round((1/ARG_fixtures_sot$arg_ov25),digits = 2)
ARG_fixtures_sot$arg_un25_odds <- round((1/ARG_fixtures_sot$arg_un25),digits = 2)

ARG_fixtures_sot$arg_ov25_odds
ARG_fixtures_sot$arg_un25_odds
###############################################################################

########Asian Handicaps######################################################################################################
#percentages
ARG_fixtures_sot$arg_ov25 <- percent(ARG_fixtures_sot$arg_ov25, accuracy = 0.1)

ARG_fixtures_sot$arg_un25 <- percent(ARG_fixtures_sot$arg_un25, accuracy = 0.1)
ARG_fixtures_sot$arg_pssotre <- paste(round(ARG_fixtures_sot$arg_xHST,digits = 0),round(ARG_fixtures_sot$arg_xAST,digits = 0),sep = "-")
#write out
#write.xlsx(ARG_fixtures,'Divisions/ARG.xlsx',sheetName = "ARG", append = TRUE)
#################################################################################################################
#D2
# HomeTeam_d2_sot <- rep(d2_teams, each = length(d2_teams))
# AwayTeam_d2_sot <- rep(d2_teams, length(d2_teams))
# D2_fixtures_sot <- cbind(HomeTeam_d2_sot,AwayTeam_d2_sot)
# D2_fixtures_sot <- as.data.frame(D2_fixtures_sot)
# D2_fixtures_sot <- D2_fixtures_sot[!D2_fixtures_sot$HomeTeam_d2_sot == D2_fixtures_sot$AwayTeam_d2_sot,]
# rownames(D2_fixtures_sot) <- NULL
# D2_fixtures_sot$Div <- "D2"
# D2_fixtures_sot <- D2_fixtures_sot[,c(3,1,2)]
#
# D2_fixtures_sot$avg_HST_d2 <- d2_avg_HST
#
# D2_fixtures_sot$d2_homesotas <- rep(d2_home_sotas,each = length(d2_teams)-1)
#
# d2_awaysods_lookup <- cbind(d2_teams,d2_away_sods)
#
# d2_awaysods_lookup <- as.data.frame(d2_awaysods_lookup)
#
# colnames(d2_awaysods_lookup) <- c("AwayTeam_d2_sot","d2_awaysods")
#
#
# require('RH2')
# D2_fixtures_sot$d2_awaysods <- sqldf("SELECT d2_awaysods_lookup.d2_awaysods FROM d2_awaysods_lookup INNER JOIN D2_fixtures_sot ON d2_awaysods_lookup.AwayTeam_d2_sot = D2_fixtures_sot.AwayTeam_d2_sot")
#
# D2_fixtures_sot$avg_AST_d2 <- d2_avg_AST
#
# d2_awaysotas_lookup <- cbind(d2_teams,d2_away_sotas)
#
# d2_awaysotas_lookup <- as.data.frame(d2_awaysotas_lookup)
#
# colnames(d2_awaysotas_lookup) <- c("AwayTeam_d2_sot","d2_awaysotas")
#
# D2_fixtures_sot$d2_awaysotas <- sqldf("SELECT d2_awaysotas_lookup.d2_awaysotas FROM d2_awaysotas_lookup INNER JOIN D2_fixtures_sot ON d2_awaysotas_lookup.AwayTeam_d2_sot = D2_fixtures_sot.AwayTeam_d2_sot")
#
# D2_fixtures_sot$d2_homesods <- rep(d2_home_sods,each = length(d2_teams)-1)
#
# D2_fixtures_sot$d2_awaysods <- as.numeric(unlist(D2_fixtures_sot$d2_awaysods))
# #xGH
# D2_fixtures_sot$d2_xHST <- D2_fixtures_sot$avg_HST_d2 * D2_fixtures_sot$d2_homesotas * D2_fixtures_sot$d2_awaysods
# #xGA
#
# D2_fixtures_sot$d2_awaysotas <- as.numeric(unlist(D2_fixtures_sot$d2_awaysotas))
#
# D2_fixtures_sot$d2_xAST <- D2_fixtures_sot$avg_AST_d2 * D2_fixtures_sot$d2_awaysotas * D2_fixtures_sot$d2_homesods
#
# D2_fixtures_sot$d2_0_0 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_0 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_0_1 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_1 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_0 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_0_2 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_2 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_1 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_2 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_3 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_0 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_1 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_2 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_0_3 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_3 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_3 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_4 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_0 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_1 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_2 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_3 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_0_4 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_4 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_4 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_4 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_5 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_0 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_1 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_2 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_3 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_4 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_0_5 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_5 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_5 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_5 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_5 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_6 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_0 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(0,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_1 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(1,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_2 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(2,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_3 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(3,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_4 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(4,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_6_5 <- round(stats::dpois(6,D2_fixtures_sot$d2_xHST) * stats::dpois(5,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_0_6 <- round(stats::dpois(0,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_1_6 <- round(stats::dpois(1,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_2_6 <- round(stats::dpois(2,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_3_6 <- round(stats::dpois(3,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_4_6 <- round(stats::dpois(4,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# D2_fixtures_sot$d2_5_6 <- round(stats::dpois(5,D2_fixtures_sot$d2_xHST) * stats::dpois(6,D2_fixtures_sot$d2_xAST), digits = 4)
# #Home win
# D2_fixtures_sot$d2_H <- (
#   D2_fixtures_sot$d2_1_0 + D2_fixtures_sot$d2_2_0 + D2_fixtures_sot$d2_2_1 + D2_fixtures_sot$d2_3_0 + D2_fixtures_sot$d2_3_1 +
#     D2_fixtures_sot$d2_3_2 + D2_fixtures_sot$d2_4_0 + D2_fixtures_sot$d2_4_1 + D2_fixtures_sot$d2_4_2 + D2_fixtures_sot$d2_4_3 +
#     D2_fixtures_sot$d2_5_0 + D2_fixtures_sot$d2_5_1 + D2_fixtures_sot$d2_5_2 + D2_fixtures_sot$d2_5_3 + D2_fixtures_sot$d2_5_4 +
#     D2_fixtures_sot$d2_6_0 + D2_fixtures_sot$d2_6_1 + D2_fixtures_sot$d2_6_2 + D2_fixtures_sot$d2_6_3 + D2_fixtures_sot$d2_6_4 +
#     D2_fixtures_sot$d2_6_5
# )
#
# D2_fixtures_sot$d2_H <- percent(D2_fixtures_sot$d2_H, accuracy = 0.1)
#
# #Draw
# D2_fixtures_sot$d2_D <- (
#
#   D2_fixtures_sot$d2_0_0 + D2_fixtures_sot$d2_1_1 + D2_fixtures_sot$d2_2_2 + D2_fixtures_sot$d2_3_3 + D2_fixtures_sot$d2_4_4 +
#     D2_fixtures_sot$d2_5_5 + D2_fixtures_sot$d2_6_6
# )
#
# D2_fixtures_sot$d2_D <- percent(D2_fixtures_sot$d2_D, accuracy = 0.1)
#
# #Away
#
# D2_fixtures_sot$d2_A <- (
#   D2_fixtures_sot$d2_0_1 + D2_fixtures_sot$d2_0_2 + D2_fixtures_sot$d2_1_2 + D2_fixtures_sot$d2_0_3 + D2_fixtures_sot$d2_1_3 +
#     D2_fixtures_sot$d2_2_3 + D2_fixtures_sot$d2_0_4 + D2_fixtures_sot$d2_1_4 + D2_fixtures_sot$d2_2_4 + D2_fixtures_sot$d2_3_4 +
#     D2_fixtures_sot$d2_0_5 + D2_fixtures_sot$d2_1_5 + D2_fixtures_sot$d2_2_5 + D2_fixtures_sot$d2_3_5 + D2_fixtures_sot$d2_4_5 +
#     D2_fixtures_sot$d2_0_6 + D2_fixtures_sot$d2_1_6 + D2_fixtures_sot$d2_2_6 + D2_fixtures_sot$d2_3_6 + D2_fixtures_sot$d2_4_6 +
#     D2_fixtures_sot$d2_5_6
# )
#
# D2_fixtures_sot$d2_A <- percent(D2_fixtures_sot$d2_A, accuracy = 0.1)
#
# #ov25
# D2_fixtures_sot$d2_ov25 <- (
#   D2_fixtures_sot$d2_2_1 + D2_fixtures_sot$d2_1_2 + D2_fixtures_sot$d2_2_2 + D2_fixtures_sot$d2_3_0 + D2_fixtures_sot$d2_3_1 +
#     D2_fixtures_sot$d2_3_2 + D2_fixtures_sot$d2_0_3 + D2_fixtures_sot$d2_1_3 + D2_fixtures_sot$d2_2_3 + D2_fixtures_sot$d2_3_3 +
#     D2_fixtures_sot$d2_4_0 + D2_fixtures_sot$d2_4_1 + D2_fixtures_sot$d2_4_2 + D2_fixtures_sot$d2_4_3 + D2_fixtures_sot$d2_0_4 +
#     D2_fixtures_sot$d2_1_4 + D2_fixtures_sot$d2_2_4 + D2_fixtures_sot$d2_3_4 + D2_fixtures_sot$d2_4_4 + D2_fixtures_sot$d2_5_0 +
#     D2_fixtures_sot$d2_5_1 + D2_fixtures_sot$d2_5_2 + D2_fixtures_sot$d2_5_3 + D2_fixtures_sot$d2_5_4 + D2_fixtures_sot$d2_0_5 +
#     D2_fixtures_sot$d2_1_5 + D2_fixtures_sot$d2_2_5 + D2_fixtures_sot$d2_3_5 + D2_fixtures_sot$d2_4_5 + D2_fixtures_sot$d2_5_5 +
#     D2_fixtures_sot$d2_6_0 + D2_fixtures_sot$d2_6_1 + D2_fixtures_sot$d2_6_2 + D2_fixtures_sot$d2_6_3 + D2_fixtures_sot$d2_6_4 +
#     D2_fixtures_sot$d2_6_5 + D2_fixtures_sot$d2_0_6 + D2_fixtures_sot$d2_1_6 + D2_fixtures_sot$d2_2_6 + D2_fixtures_sot$d2_3_6 +
#     D2_fixtures_sot$d2_4_6 + D2_fixtures_sot$d2_5_6 + D2_fixtures_sot$d2_6_6
# )
# #un25
# D2_fixtures_sot$d2_un25 <- (
#   D2_fixtures_sot$d2_0_0 + D2_fixtures_sot$d2_1_0 + D2_fixtures_sot$d2_0_1 + D2_fixtures_sot$d2_1_1 + D2_fixtures_sot$d2_2_0 + D2_fixtures_sot$d2_0_2
# )
# #odds
# D2_fixtures_sot$d2_ov25_odds <- round((1/D2_fixtures_sot$d2_ov25),digits = 2)
# D2_fixtures_sot$d2_un25_odds <- round((1/D2_fixtures_sot$d2_un25),digits = 2)
#
# D2_fixtures_sot$d2_ov25_odds
# D2_fixtures_sot$d2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# D2_fixtures_sot$d2_ov25 <- percent(D2_fixtures_sot$d2_ov25, accuracy = 0.1)
#
# D2_fixtures_sot$d2_un25 <- percent(D2_fixtures_sot$d2_un25, accuracy = 0.1)
# D2_fixtures_sot$d2_pssotre <- paste(round(D2_fixtures_sot$d2_xHST,digits = 0),round(D2_fixtures_sot$d2_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(D2_fixtures,'Divisions/D2.xlsx',sheetName = "D2", append = TRUE)
# #################################################################################################################
# #E0
# HomeTeam_e0_sot <- rep(e0_teams, each = length(e0_teams))
# AwayTeam_e0_sot <- rep(e0_teams, length(e0_teams))
# E0_fixtures_sot <- cbind(HomeTeam_e0_sot,AwayTeam_e0_sot)
# E0_fixtures_sot <- as.data.frame(E0_fixtures_sot)
# E0_fixtures_sot <- E0_fixtures_sot[!E0_fixtures_sot$HomeTeam_e0_sot == E0_fixtures_sot$AwayTeam_e0_sot,]
# rownames(E0_fixtures_sot) <- NULL
# E0_fixtures_sot$Div <- "E0"
# E0_fixtures_sot <- E0_fixtures_sot[,c(3,1,2)]
#
# E0_fixtures_sot$avg_HST_e0 <- e0_avg_HST
#
# E0_fixtures_sot$e0_homesotas <- rep(e0_home_sotas,each = length(e0_teams)-1)
#
# e0_awaysods_lookup <- cbind(e0_teams,e0_away_sods)
#
# e0_awaysods_lookup <- as.data.frame(e0_awaysods_lookup)
#
# colnames(e0_awaysods_lookup) <- c("AwayTeam_e0_sot","e0_awaysods")
#
#
# require('RH2')
# E0_fixtures_sot$e0_awaysods <- sqldf("SELECT e0_awaysods_lookup.e0_awaysods FROM e0_awaysods_lookup INNER JOIN E0_fixtures_sot ON e0_awaysods_lookup.AwayTeam_e0_sot = E0_fixtures_sot.AwayTeam_e0_sot")
#
# E0_fixtures_sot$avg_AST_e0 <- e0_avg_AST
#
# e0_awaysotas_lookup <- cbind(e0_teams,e0_away_sotas)
#
# e0_awaysotas_lookup <- as.data.frame(e0_awaysotas_lookup)
#
# colnames(e0_awaysotas_lookup) <- c("AwayTeam_e0_sot","e0_awaysotas")
#
# E0_fixtures_sot$e0_awaysotas <- sqldf("SELECT e0_awaysotas_lookup.e0_awaysotas FROM e0_awaysotas_lookup INNER JOIN E0_fixtures_sot ON e0_awaysotas_lookup.AwayTeam_e0_sot = E0_fixtures_sot.AwayTeam_e0_sot")
#
# E0_fixtures_sot$e0_homesods <- rep(e0_home_sods,each = length(e0_teams)-1)
#
# E0_fixtures_sot$e0_awaysods <- as.numeric(unlist(E0_fixtures_sot$e0_awaysods))
# #xGH
# E0_fixtures_sot$e0_xHST <- E0_fixtures_sot$avg_HST_e0 * E0_fixtures_sot$e0_homesotas * E0_fixtures_sot$e0_awaysods
# #xGA
#
# E0_fixtures_sot$e0_awaysotas <- as.numeric(unlist(E0_fixtures_sot$e0_awaysotas))
#
# E0_fixtures_sot$e0_xAST <- E0_fixtures_sot$avg_AST_e0 * E0_fixtures_sot$e0_awaysotas * E0_fixtures_sot$e0_homesods
#
# E0_fixtures_sot$e0_0_0 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_0 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_0_1 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_1 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_0 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_0_2 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_2 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_1 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_2 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_3 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_0 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_1 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_2 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_0_3 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_3 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_3 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_4 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_0 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_1 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_2 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_3 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_0_4 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_4 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_4 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_4 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_5 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_0 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_1 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_2 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_3 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_4 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_0_5 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_5 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_5 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_5 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_5 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_6 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_0 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(0,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_1 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(1,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_2 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(2,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_3 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(3,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_4 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(4,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_6_5 <- round(stats::dpois(6,E0_fixtures_sot$e0_xHST) * stats::dpois(5,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_0_6 <- round(stats::dpois(0,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_1_6 <- round(stats::dpois(1,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_2_6 <- round(stats::dpois(2,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_3_6 <- round(stats::dpois(3,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_4_6 <- round(stats::dpois(4,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# E0_fixtures_sot$e0_5_6 <- round(stats::dpois(5,E0_fixtures_sot$e0_xHST) * stats::dpois(6,E0_fixtures_sot$e0_xAST), digits = 4)
# #Home win
# E0_fixtures_sot$e0_H <- (
#   E0_fixtures_sot$e0_1_0 + E0_fixtures_sot$e0_2_0 + E0_fixtures_sot$e0_2_1 + E0_fixtures_sot$e0_3_0 + E0_fixtures_sot$e0_3_1 +
#     E0_fixtures_sot$e0_3_2 + E0_fixtures_sot$e0_4_0 + E0_fixtures_sot$e0_4_1 + E0_fixtures_sot$e0_4_2 + E0_fixtures_sot$e0_4_3 +
#     E0_fixtures_sot$e0_5_0 + E0_fixtures_sot$e0_5_1 + E0_fixtures_sot$e0_5_2 + E0_fixtures_sot$e0_5_3 + E0_fixtures_sot$e0_5_4 +
#     E0_fixtures_sot$e0_6_0 + E0_fixtures_sot$e0_6_1 + E0_fixtures_sot$e0_6_2 + E0_fixtures_sot$e0_6_3 + E0_fixtures_sot$e0_6_4 +
#     E0_fixtures_sot$e0_6_5
# )
#
# E0_fixtures_sot$e0_H <- percent(E0_fixtures_sot$e0_H, accuracy = 0.1)
#
# #Draw
# E0_fixtures_sot$e0_D <- (
#
#   E0_fixtures_sot$e0_0_0 + E0_fixtures_sot$e0_1_1 + E0_fixtures_sot$e0_2_2 + E0_fixtures_sot$e0_3_3 + E0_fixtures_sot$e0_4_4 +
#     E0_fixtures_sot$e0_5_5 + E0_fixtures_sot$e0_6_6
# )
#
# E0_fixtures_sot$e0_D <- percent(E0_fixtures_sot$e0_D, accuracy = 0.1)
#
# #Away
#
# E0_fixtures_sot$e0_A <- (
#   E0_fixtures_sot$e0_0_1 + E0_fixtures_sot$e0_0_2 + E0_fixtures_sot$e0_1_2 + E0_fixtures_sot$e0_0_3 + E0_fixtures_sot$e0_1_3 +
#     E0_fixtures_sot$e0_2_3 + E0_fixtures_sot$e0_0_4 + E0_fixtures_sot$e0_1_4 + E0_fixtures_sot$e0_2_4 + E0_fixtures_sot$e0_3_4 +
#     E0_fixtures_sot$e0_0_5 + E0_fixtures_sot$e0_1_5 + E0_fixtures_sot$e0_2_5 + E0_fixtures_sot$e0_3_5 + E0_fixtures_sot$e0_4_5 +
#     E0_fixtures_sot$e0_0_6 + E0_fixtures_sot$e0_1_6 + E0_fixtures_sot$e0_2_6 + E0_fixtures_sot$e0_3_6 + E0_fixtures_sot$e0_4_6 +
#     E0_fixtures_sot$e0_5_6
# )
#
# E0_fixtures_sot$e0_A <- percent(E0_fixtures_sot$e0_A, accuracy = 0.1)
#
# #ov25
# E0_fixtures_sot$e0_ov25 <- (
#   E0_fixtures_sot$e0_2_1 + E0_fixtures_sot$e0_1_2 + E0_fixtures_sot$e0_2_2 + E0_fixtures_sot$e0_3_0 + E0_fixtures_sot$e0_3_1 +
#     E0_fixtures_sot$e0_3_2 + E0_fixtures_sot$e0_0_3 + E0_fixtures_sot$e0_1_3 + E0_fixtures_sot$e0_2_3 + E0_fixtures_sot$e0_3_3 +
#     E0_fixtures_sot$e0_4_0 + E0_fixtures_sot$e0_4_1 + E0_fixtures_sot$e0_4_2 + E0_fixtures_sot$e0_4_3 + E0_fixtures_sot$e0_0_4 +
#     E0_fixtures_sot$e0_1_4 + E0_fixtures_sot$e0_2_4 + E0_fixtures_sot$e0_3_4 + E0_fixtures_sot$e0_4_4 + E0_fixtures_sot$e0_5_0 +
#     E0_fixtures_sot$e0_5_1 + E0_fixtures_sot$e0_5_2 + E0_fixtures_sot$e0_5_3 + E0_fixtures_sot$e0_5_4 + E0_fixtures_sot$e0_0_5 +
#     E0_fixtures_sot$e0_1_5 + E0_fixtures_sot$e0_2_5 + E0_fixtures_sot$e0_3_5 + E0_fixtures_sot$e0_4_5 + E0_fixtures_sot$e0_5_5 +
#     E0_fixtures_sot$e0_6_0 + E0_fixtures_sot$e0_6_1 + E0_fixtures_sot$e0_6_2 + E0_fixtures_sot$e0_6_3 + E0_fixtures_sot$e0_6_4 +
#     E0_fixtures_sot$e0_6_5 + E0_fixtures_sot$e0_0_6 + E0_fixtures_sot$e0_1_6 + E0_fixtures_sot$e0_2_6 + E0_fixtures_sot$e0_3_6 +
#     E0_fixtures_sot$e0_4_6 + E0_fixtures_sot$e0_5_6 + E0_fixtures_sot$e0_6_6
# )
# #un25
# E0_fixtures_sot$e0_un25 <- (
#   E0_fixtures_sot$e0_0_0 + E0_fixtures_sot$e0_1_0 + E0_fixtures_sot$e0_0_1 + E0_fixtures_sot$e0_1_1 + E0_fixtures_sot$e0_2_0 + E0_fixtures_sot$e0_0_2
# )
# #odds
# E0_fixtures_sot$e0_ov25_odds <- round((1/E0_fixtures_sot$e0_ov25),digits = 2)
# E0_fixtures_sot$e0_un25_odds <- round((1/E0_fixtures_sot$e0_un25),digits = 2)
#
# E0_fixtures_sot$e0_ov25_odds
# E0_fixtures_sot$e0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E0_fixtures_sot$e0_ov25 <- percent(E0_fixtures_sot$e0_ov25, accuracy = 0.1)
#
# E0_fixtures_sot$e0_un25 <- percent(E0_fixtures_sot$e0_un25, accuracy = 0.1)
# E0_fixtures_sot$e0_pssotre <- paste(round(E0_fixtures_sot$e0_xHST,digits = 0),round(E0_fixtures_sot$e0_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #E1
# HomeTeam_e1_sot <- rep(e1_teams, each = length(e1_teams))
# AwayTeam_e1_sot <- rep(e1_teams, length(e1_teams))
# E1_fixtures_sot <- cbind(HomeTeam_e1_sot,AwayTeam_e1_sot)
# E1_fixtures_sot <- as.data.frame(E1_fixtures_sot)
# E1_fixtures_sot <- E1_fixtures_sot[!E1_fixtures_sot$HomeTeam_e1_sot == E1_fixtures_sot$AwayTeam_e1_sot,]
# rownames(E1_fixtures_sot) <- NULL
# E1_fixtures_sot$Div <- "E1"
# E1_fixtures_sot <- E1_fixtures_sot[,c(3,1,2)]
#
# E1_fixtures_sot$avg_HST_e1 <- e1_avg_HST
#
# E1_fixtures_sot$e1_homesotas <- rep(e1_home_sotas,each = length(e1_teams)-1)
#
# e1_awaysods_lookup <- cbind(e1_teams,e1_away_sods)
#
# e1_awaysods_lookup <- as.data.frame(e1_awaysods_lookup)
#
# colnames(e1_awaysods_lookup) <- c("AwayTeam_e1_sot","e1_awaysods")
#
#
# require('RH2')
# E1_fixtures_sot$e1_awaysods <- sqldf("SELECT e1_awaysods_lookup.e1_awaysods FROM e1_awaysods_lookup INNER JOIN E1_fixtures_sot ON e1_awaysods_lookup.AwayTeam_e1_sot = E1_fixtures_sot.AwayTeam_e1_sot")
#
# E1_fixtures_sot$avg_AST_e1 <- e1_avg_AST
#
# e1_awaysotas_lookup <- cbind(e1_teams,e1_away_sotas)
#
# e1_awaysotas_lookup <- as.data.frame(e1_awaysotas_lookup)
#
# colnames(e1_awaysotas_lookup) <- c("AwayTeam_e1_sot","e1_awaysotas")
#
# E1_fixtures_sot$e1_awaysotas <- sqldf("SELECT e1_awaysotas_lookup.e1_awaysotas FROM e1_awaysotas_lookup INNER JOIN E1_fixtures_sot ON e1_awaysotas_lookup.AwayTeam_e1_sot = E1_fixtures_sot.AwayTeam_e1_sot")
#
# E1_fixtures_sot$e1_homesods <- rep(e1_home_sods,each = length(e1_teams)-1)
#
# E1_fixtures_sot$e1_awaysods <- as.numeric(unlist(E1_fixtures_sot$e1_awaysods))
# #xGH
# E1_fixtures_sot$e1_xHST <- E1_fixtures_sot$avg_HST_e1 * E1_fixtures_sot$e1_homesotas * E1_fixtures_sot$e1_awaysods
# #xGA
#
# E1_fixtures_sot$e1_awaysotas <- as.numeric(unlist(E1_fixtures_sot$e1_awaysotas))
#
# E1_fixtures_sot$e1_xAST <- E1_fixtures_sot$avg_AST_e1 * E1_fixtures_sot$e1_awaysotas * E1_fixtures_sot$e1_homesods
#
# E1_fixtures_sot$e1_0_0 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_0 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_0_1 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_1 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_0 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_0_2 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_2 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_1 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_2 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_3 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_0 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_1 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_2 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_0_3 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_3 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_3 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_4 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_0 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_1 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_2 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_3 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_0_4 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_4 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_4 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_4 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_5 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_0 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_1 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_2 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_3 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_4 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_0_5 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_5 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_5 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_5 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_5 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_6 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_0 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(0,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_1 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(1,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_2 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(2,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_3 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(3,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_4 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(4,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_6_5 <- round(stats::dpois(6,E1_fixtures_sot$e1_xHST) * stats::dpois(5,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_0_6 <- round(stats::dpois(0,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_1_6 <- round(stats::dpois(1,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_2_6 <- round(stats::dpois(2,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_3_6 <- round(stats::dpois(3,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_4_6 <- round(stats::dpois(4,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# E1_fixtures_sot$e1_5_6 <- round(stats::dpois(5,E1_fixtures_sot$e1_xHST) * stats::dpois(6,E1_fixtures_sot$e1_xAST), digits = 4)
# #Home win
# E1_fixtures_sot$e1_H <- (
#   E1_fixtures_sot$e1_1_0 + E1_fixtures_sot$e1_2_0 + E1_fixtures_sot$e1_2_1 + E1_fixtures_sot$e1_3_0 + E1_fixtures_sot$e1_3_1 +
#     E1_fixtures_sot$e1_3_2 + E1_fixtures_sot$e1_4_0 + E1_fixtures_sot$e1_4_1 + E1_fixtures_sot$e1_4_2 + E1_fixtures_sot$e1_4_3 +
#     E1_fixtures_sot$e1_5_0 + E1_fixtures_sot$e1_5_1 + E1_fixtures_sot$e1_5_2 + E1_fixtures_sot$e1_5_3 + E1_fixtures_sot$e1_5_4 +
#     E1_fixtures_sot$e1_6_0 + E1_fixtures_sot$e1_6_1 + E1_fixtures_sot$e1_6_2 + E1_fixtures_sot$e1_6_3 + E1_fixtures_sot$e1_6_4 +
#     E1_fixtures_sot$e1_6_5
# )
#
# E1_fixtures_sot$e1_H <- percent(E1_fixtures_sot$e1_H, accuracy = 0.1)
#
# #Draw
# E1_fixtures_sot$e1_D <- (
#
#   E1_fixtures_sot$e1_0_0 + E1_fixtures_sot$e1_1_1 + E1_fixtures_sot$e1_2_2 + E1_fixtures_sot$e1_3_3 + E1_fixtures_sot$e1_4_4 +
#     E1_fixtures_sot$e1_5_5 + E1_fixtures_sot$e1_6_6
# )
#
# E1_fixtures_sot$e1_D <- percent(E1_fixtures_sot$e1_D, accuracy = 0.1)
#
# #Away
#
# E1_fixtures_sot$e1_A <- (
#   E1_fixtures_sot$e1_0_1 + E1_fixtures_sot$e1_0_2 + E1_fixtures_sot$e1_1_2 + E1_fixtures_sot$e1_0_3 + E1_fixtures_sot$e1_1_3 +
#     E1_fixtures_sot$e1_2_3 + E1_fixtures_sot$e1_0_4 + E1_fixtures_sot$e1_1_4 + E1_fixtures_sot$e1_2_4 + E1_fixtures_sot$e1_3_4 +
#     E1_fixtures_sot$e1_0_5 + E1_fixtures_sot$e1_1_5 + E1_fixtures_sot$e1_2_5 + E1_fixtures_sot$e1_3_5 + E1_fixtures_sot$e1_4_5 +
#     E1_fixtures_sot$e1_0_6 + E1_fixtures_sot$e1_1_6 + E1_fixtures_sot$e1_2_6 + E1_fixtures_sot$e1_3_6 + E1_fixtures_sot$e1_4_6 +
#     E1_fixtures_sot$e1_5_6
# )
#
# E1_fixtures_sot$e1_A <- percent(E1_fixtures_sot$e1_A, accuracy = 0.1)
#
# #ov25
# E1_fixtures_sot$e1_ov25 <- (
#   E1_fixtures_sot$e1_2_1 + E1_fixtures_sot$e1_1_2 + E1_fixtures_sot$e1_2_2 + E1_fixtures_sot$e1_3_0 + E1_fixtures_sot$e1_3_1 +
#     E1_fixtures_sot$e1_3_2 + E1_fixtures_sot$e1_0_3 + E1_fixtures_sot$e1_1_3 + E1_fixtures_sot$e1_2_3 + E1_fixtures_sot$e1_3_3 +
#     E1_fixtures_sot$e1_4_0 + E1_fixtures_sot$e1_4_1 + E1_fixtures_sot$e1_4_2 + E1_fixtures_sot$e1_4_3 + E1_fixtures_sot$e1_0_4 +
#     E1_fixtures_sot$e1_1_4 + E1_fixtures_sot$e1_2_4 + E1_fixtures_sot$e1_3_4 + E1_fixtures_sot$e1_4_4 + E1_fixtures_sot$e1_5_0 +
#     E1_fixtures_sot$e1_5_1 + E1_fixtures_sot$e1_5_2 + E1_fixtures_sot$e1_5_3 + E1_fixtures_sot$e1_5_4 + E1_fixtures_sot$e1_0_5 +
#     E1_fixtures_sot$e1_1_5 + E1_fixtures_sot$e1_2_5 + E1_fixtures_sot$e1_3_5 + E1_fixtures_sot$e1_4_5 + E1_fixtures_sot$e1_5_5 +
#     E1_fixtures_sot$e1_6_0 + E1_fixtures_sot$e1_6_1 + E1_fixtures_sot$e1_6_2 + E1_fixtures_sot$e1_6_3 + E1_fixtures_sot$e1_6_4 +
#     E1_fixtures_sot$e1_6_5 + E1_fixtures_sot$e1_0_6 + E1_fixtures_sot$e1_1_6 + E1_fixtures_sot$e1_2_6 + E1_fixtures_sot$e1_3_6 +
#     E1_fixtures_sot$e1_4_6 + E1_fixtures_sot$e1_5_6 + E1_fixtures_sot$e1_6_6
# )
# #un25
# E1_fixtures_sot$e1_un25 <- (
#   E1_fixtures_sot$e1_0_0 + E1_fixtures_sot$e1_1_0 + E1_fixtures_sot$e1_0_1 + E1_fixtures_sot$e1_1_1 + E1_fixtures_sot$e1_2_0 + E1_fixtures_sot$e1_0_2
# )
# #odds
# E1_fixtures_sot$e1_ov25_odds <- round((1/E1_fixtures_sot$e1_ov25),digits = 2)
# E1_fixtures_sot$e1_un25_odds <- round((1/E1_fixtures_sot$e1_un25),digits = 2)
#
# E1_fixtures_sot$e1_ov25_odds
# E1_fixtures_sot$e1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E1_fixtures_sot$e1_ov25 <- percent(E1_fixtures_sot$e1_ov25, accuracy = 0.1)
#
# E1_fixtures_sot$e1_un25 <- percent(E1_fixtures_sot$e1_un25, accuracy = 0.1)
# E1_fixtures_sot$e1_pssotre <- paste(round(E1_fixtures_sot$e1_xHST,digits = 0),round(E1_fixtures_sot$e1_xAST,digits = 0),sep = "-")
#
# #write out
# #write.xlsx(E1_fixtures,'Divisions/E1.xlsx',sheetName = "E1", append = TRUE)
# #################################################################################################################
# #E2
# HomeTeam_e2_sot <- rep(e2_teams, each = length(e2_teams))
# AwayTeam_e2_sot <- rep(e2_teams, length(e2_teams))
# E2_fixtures_sot <- cbind(HomeTeam_e2_sot,AwayTeam_e2_sot)
# E2_fixtures_sot <- as.data.frame(E2_fixtures_sot)
# E2_fixtures_sot <- E2_fixtures_sot[!E2_fixtures_sot$HomeTeam_e2_sot == E2_fixtures_sot$AwayTeam_e2_sot,]
# rownames(E2_fixtures_sot) <- NULL
# E2_fixtures_sot$Div <- "E2"
# E2_fixtures_sot <- E2_fixtures_sot[,c(3,1,2)]
#
# E2_fixtures_sot$avg_HST_e2 <- e2_avg_HST
#
# E2_fixtures_sot$e2_homesotas <- rep(e2_home_sotas,each = length(e2_teams)-1)
#
# e2_awaysods_lookup <- cbind(e2_teams,e2_away_sods)
#
# e2_awaysods_lookup <- as.data.frame(e2_awaysods_lookup)
#
# colnames(e2_awaysods_lookup) <- c("AwayTeam_e2_sot","e2_awaysods")
#
#
# require('RH2')
# E2_fixtures_sot$e2_awaysods <- sqldf("SELECT e2_awaysods_lookup.e2_awaysods FROM e2_awaysods_lookup INNER JOIN E2_fixtures_sot ON e2_awaysods_lookup.AwayTeam_e2_sot = E2_fixtures_sot.AwayTeam_e2_sot")
#
# E2_fixtures_sot$avg_AST_e2 <- e2_avg_AST
#
# e2_awaysotas_lookup <- cbind(e2_teams,e2_away_sotas)
#
# e2_awaysotas_lookup <- as.data.frame(e2_awaysotas_lookup)
#
# colnames(e2_awaysotas_lookup) <- c("AwayTeam_e2_sot","e2_awaysotas")
#
# E2_fixtures_sot$e2_awaysotas <- sqldf("SELECT e2_awaysotas_lookup.e2_awaysotas FROM e2_awaysotas_lookup INNER JOIN E2_fixtures_sot ON e2_awaysotas_lookup.AwayTeam_e2_sot = E2_fixtures_sot.AwayTeam_e2_sot")
#
# E2_fixtures_sot$e2_homesods <- rep(e2_home_sods,each = length(e2_teams)-1)
#
# E2_fixtures_sot$e2_awaysods <- as.numeric(unlist(E2_fixtures_sot$e2_awaysods))
# #xGH
# E2_fixtures_sot$e2_xHST <- E2_fixtures_sot$avg_HST_e2 * E2_fixtures_sot$e2_homesotas * E2_fixtures_sot$e2_awaysods
# #xGA
#
# E2_fixtures_sot$e2_awaysotas <- as.numeric(unlist(E2_fixtures_sot$e2_awaysotas))
#
# E2_fixtures_sot$e2_xAST <- E2_fixtures_sot$avg_AST_e2 * E2_fixtures_sot$e2_awaysotas * E2_fixtures_sot$e2_homesods
#
# E2_fixtures_sot$e2_0_0 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_0 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_0_1 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_1 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_0 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_0_2 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_2 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_1 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_2 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_3 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_0 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_1 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_2 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_0_3 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_3 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_3 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_4 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_0 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_1 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_2 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_3 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_0_4 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_4 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_4 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_4 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_5 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_0 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_1 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_2 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_3 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_4 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_0_5 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_5 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_5 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_5 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_5 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_6 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_0 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(0,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_1 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(1,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_2 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(2,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_3 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(3,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_4 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(4,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_6_5 <- round(stats::dpois(6,E2_fixtures_sot$e2_xHST) * stats::dpois(5,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_0_6 <- round(stats::dpois(0,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_1_6 <- round(stats::dpois(1,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_2_6 <- round(stats::dpois(2,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_3_6 <- round(stats::dpois(3,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_4_6 <- round(stats::dpois(4,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# E2_fixtures_sot$e2_5_6 <- round(stats::dpois(5,E2_fixtures_sot$e2_xHST) * stats::dpois(6,E2_fixtures_sot$e2_xAST), digits = 4)
# #Home win
# E2_fixtures_sot$e2_H <- (
#   E2_fixtures_sot$e2_1_0 + E2_fixtures_sot$e2_2_0 + E2_fixtures_sot$e2_2_1 + E2_fixtures_sot$e2_3_0 + E2_fixtures_sot$e2_3_1 +
#     E2_fixtures_sot$e2_3_2 + E2_fixtures_sot$e2_4_0 + E2_fixtures_sot$e2_4_1 + E2_fixtures_sot$e2_4_2 + E2_fixtures_sot$e2_4_3 +
#     E2_fixtures_sot$e2_5_0 + E2_fixtures_sot$e2_5_1 + E2_fixtures_sot$e2_5_2 + E2_fixtures_sot$e2_5_3 + E2_fixtures_sot$e2_5_4 +
#     E2_fixtures_sot$e2_6_0 + E2_fixtures_sot$e2_6_1 + E2_fixtures_sot$e2_6_2 + E2_fixtures_sot$e2_6_3 + E2_fixtures_sot$e2_6_4 +
#     E2_fixtures_sot$e2_6_5
# )
#
# E2_fixtures_sot$e2_H <- percent(E2_fixtures_sot$e2_H, accuracy = 0.1)
#
# #Draw
# E2_fixtures_sot$e2_D <- (
#
#   E2_fixtures_sot$e2_0_0 + E2_fixtures_sot$e2_1_1 + E2_fixtures_sot$e2_2_2 + E2_fixtures_sot$e2_3_3 + E2_fixtures_sot$e2_4_4 +
#     E2_fixtures_sot$e2_5_5 + E2_fixtures_sot$e2_6_6
# )
#
# E2_fixtures_sot$e2_D <- percent(E2_fixtures_sot$e2_D, accuracy = 0.1)
#
# #Away
#
# E2_fixtures_sot$e2_A <- (
#   E2_fixtures_sot$e2_0_1 + E2_fixtures_sot$e2_0_2 + E2_fixtures_sot$e2_1_2 + E2_fixtures_sot$e2_0_3 + E2_fixtures_sot$e2_1_3 +
#     E2_fixtures_sot$e2_2_3 + E2_fixtures_sot$e2_0_4 + E2_fixtures_sot$e2_1_4 + E2_fixtures_sot$e2_2_4 + E2_fixtures_sot$e2_3_4 +
#     E2_fixtures_sot$e2_0_5 + E2_fixtures_sot$e2_1_5 + E2_fixtures_sot$e2_2_5 + E2_fixtures_sot$e2_3_5 + E2_fixtures_sot$e2_4_5 +
#     E2_fixtures_sot$e2_0_6 + E2_fixtures_sot$e2_1_6 + E2_fixtures_sot$e2_2_6 + E2_fixtures_sot$e2_3_6 + E2_fixtures_sot$e2_4_6 +
#     E2_fixtures_sot$e2_5_6
# )
#
# E2_fixtures_sot$e2_A <- percent(E2_fixtures_sot$e2_A, accuracy = 0.1)
#
# #ov25
# E2_fixtures_sot$e2_ov25 <- (
#   E2_fixtures_sot$e2_2_1 + E2_fixtures_sot$e2_1_2 + E2_fixtures_sot$e2_2_2 + E2_fixtures_sot$e2_3_0 + E2_fixtures_sot$e2_3_1 +
#     E2_fixtures_sot$e2_3_2 + E2_fixtures_sot$e2_0_3 + E2_fixtures_sot$e2_1_3 + E2_fixtures_sot$e2_2_3 + E2_fixtures_sot$e2_3_3 +
#     E2_fixtures_sot$e2_4_0 + E2_fixtures_sot$e2_4_1 + E2_fixtures_sot$e2_4_2 + E2_fixtures_sot$e2_4_3 + E2_fixtures_sot$e2_0_4 +
#     E2_fixtures_sot$e2_1_4 + E2_fixtures_sot$e2_2_4 + E2_fixtures_sot$e2_3_4 + E2_fixtures_sot$e2_4_4 + E2_fixtures_sot$e2_5_0 +
#     E2_fixtures_sot$e2_5_1 + E2_fixtures_sot$e2_5_2 + E2_fixtures_sot$e2_5_3 + E2_fixtures_sot$e2_5_4 + E2_fixtures_sot$e2_0_5 +
#     E2_fixtures_sot$e2_1_5 + E2_fixtures_sot$e2_2_5 + E2_fixtures_sot$e2_3_5 + E2_fixtures_sot$e2_4_5 + E2_fixtures_sot$e2_5_5 +
#     E2_fixtures_sot$e2_6_0 + E2_fixtures_sot$e2_6_1 + E2_fixtures_sot$e2_6_2 + E2_fixtures_sot$e2_6_3 + E2_fixtures_sot$e2_6_4 +
#     E2_fixtures_sot$e2_6_5 + E2_fixtures_sot$e2_0_6 + E2_fixtures_sot$e2_1_6 + E2_fixtures_sot$e2_2_6 + E2_fixtures_sot$e2_3_6 +
#     E2_fixtures_sot$e2_4_6 + E2_fixtures_sot$e2_5_6 + E2_fixtures_sot$e2_6_6
# )
# #un25
# E2_fixtures_sot$e2_un25 <- (
#   E2_fixtures_sot$e2_0_0 + E2_fixtures_sot$e2_1_0 + E2_fixtures_sot$e2_0_1 + E2_fixtures_sot$e2_1_1 + E2_fixtures_sot$e2_2_0 + E2_fixtures_sot$e2_0_2
# )
# #odds
# E2_fixtures_sot$e2_ov25_odds <- round((1/E2_fixtures_sot$e2_ov25),digits = 2)
# E2_fixtures_sot$e2_un25_odds <- round((1/E2_fixtures_sot$e2_un25),digits = 2)
#
# E2_fixtures_sot$e2_ov25_odds
# E2_fixtures_sot$e2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E2_fixtures_sot$e2_ov25 <- percent(E2_fixtures_sot$e2_ov25, accuracy = 0.1)
#
# E2_fixtures_sot$e2_un25 <- percent(E2_fixtures_sot$e2_un25, accuracy = 0.1)
# E2_fixtures_sot$e2_pssotre <- paste(round(E2_fixtures_sot$e2_xHST,digits = 0),round(E2_fixtures_sot$e2_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(E2_fixtures,'Divisions/E2.xlsx',sheetName = "E2", append = TRUE)
# #################################################################################################################
# #E3
# HomeTeam_e3_sot <- rep(e3_teams, each = length(e3_teams))
# AwayTeam_e3_sot <- rep(e3_teams, length(e3_teams))
# E3_fixtures_sot <- cbind(HomeTeam_e3_sot,AwayTeam_e3_sot)
# E3_fixtures_sot <- as.data.frame(E3_fixtures_sot)
# E3_fixtures_sot <- E3_fixtures_sot[!E3_fixtures_sot$HomeTeam_e3_sot == E3_fixtures_sot$AwayTeam_e3_sot,]
# rownames(E3_fixtures_sot) <- NULL
# E3_fixtures_sot$Div <- "E3"
# E3_fixtures_sot <- E3_fixtures_sot[,c(3,1,2)]
#
# E3_fixtures_sot$avg_HST_e3 <- e3_avg_HST
#
# E3_fixtures_sot$e3_homesotas <- rep(e3_home_sotas,each = length(e3_teams)-1)
#
# e3_awaysods_lookup <- cbind(e3_teams,e3_away_sods)
#
# e3_awaysods_lookup <- as.data.frame(e3_awaysods_lookup)
#
# colnames(e3_awaysods_lookup) <- c("AwayTeam_e3_sot","e3_awaysods")
#
#
# require('RH2')
# E3_fixtures_sot$e3_awaysods <- sqldf("SELECT e3_awaysods_lookup.e3_awaysods FROM e3_awaysods_lookup INNER JOIN E3_fixtures_sot ON e3_awaysods_lookup.AwayTeam_e3_sot = E3_fixtures_sot.AwayTeam_e3_sot")
#
# E3_fixtures_sot$avg_AST_e3 <- e3_avg_AST
#
# e3_awaysotas_lookup <- cbind(e3_teams,e3_away_sotas)
#
# e3_awaysotas_lookup <- as.data.frame(e3_awaysotas_lookup)
#
# colnames(e3_awaysotas_lookup) <- c("AwayTeam_e3_sot","e3_awaysotas")
#
# E3_fixtures_sot$e3_awaysotas <- sqldf("SELECT e3_awaysotas_lookup.e3_awaysotas FROM e3_awaysotas_lookup INNER JOIN E3_fixtures_sot ON e3_awaysotas_lookup.AwayTeam_e3_sot = E3_fixtures_sot.AwayTeam_e3_sot")
#
# E3_fixtures_sot$e3_homesods <- rep(e3_home_sods,each = length(e3_teams)-1)
#
# E3_fixtures_sot$e3_awaysods <- as.numeric(unlist(E3_fixtures_sot$e3_awaysods))
# #xGH
# E3_fixtures_sot$e3_xHST <- E3_fixtures_sot$avg_HST_e3 * E3_fixtures_sot$e3_homesotas * E3_fixtures_sot$e3_awaysods
# #xGA
#
# E3_fixtures_sot$e3_awaysotas <- as.numeric(unlist(E3_fixtures_sot$e3_awaysotas))
#
# E3_fixtures_sot$e3_xAST <- E3_fixtures_sot$avg_AST_e3 * E3_fixtures_sot$e3_awaysotas * E3_fixtures_sot$e3_homesods
#
# E3_fixtures_sot$e3_0_0 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_0 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_0_1 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_1 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_0 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_0_2 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_2 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_1 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_2 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_3 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_0 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_1 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_2 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_0_3 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_3 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_3 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_4 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_0 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_1 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_2 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_3 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_0_4 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_4 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_4 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_4 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_5 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_0 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_1 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_2 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_3 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_4 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_0_5 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_5 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_5 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_5 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_5 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_6 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_0 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(0,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_1 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(1,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_2 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(2,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_3 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(3,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_4 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(4,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_6_5 <- round(stats::dpois(6,E3_fixtures_sot$e3_xHST) * stats::dpois(5,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_0_6 <- round(stats::dpois(0,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_1_6 <- round(stats::dpois(1,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_2_6 <- round(stats::dpois(2,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_3_6 <- round(stats::dpois(3,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_4_6 <- round(stats::dpois(4,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# E3_fixtures_sot$e3_5_6 <- round(stats::dpois(5,E3_fixtures_sot$e3_xHST) * stats::dpois(6,E3_fixtures_sot$e3_xAST), digits = 4)
# #Home win
# E3_fixtures_sot$e3_H <- (
#   E3_fixtures_sot$e3_1_0 + E3_fixtures_sot$e3_2_0 + E3_fixtures_sot$e3_2_1 + E3_fixtures_sot$e3_3_0 + E3_fixtures_sot$e3_3_1 +
#     E3_fixtures_sot$e3_3_2 + E3_fixtures_sot$e3_4_0 + E3_fixtures_sot$e3_4_1 + E3_fixtures_sot$e3_4_2 + E3_fixtures_sot$e3_4_3 +
#     E3_fixtures_sot$e3_5_0 + E3_fixtures_sot$e3_5_1 + E3_fixtures_sot$e3_5_2 + E3_fixtures_sot$e3_5_3 + E3_fixtures_sot$e3_5_4 +
#     E3_fixtures_sot$e3_6_0 + E3_fixtures_sot$e3_6_1 + E3_fixtures_sot$e3_6_2 + E3_fixtures_sot$e3_6_3 + E3_fixtures_sot$e3_6_4 +
#     E3_fixtures_sot$e3_6_5
# )
#
# E3_fixtures_sot$e3_H <- percent(E3_fixtures_sot$e3_H, accuracy = 0.1)
#
# #Draw
# E3_fixtures_sot$e3_D <- (
#
#   E3_fixtures_sot$e3_0_0 + E3_fixtures_sot$e3_1_1 + E3_fixtures_sot$e3_2_2 + E3_fixtures_sot$e3_3_3 + E3_fixtures_sot$e3_4_4 +
#     E3_fixtures_sot$e3_5_5 + E3_fixtures_sot$e3_6_6
# )
#
# E3_fixtures_sot$e3_D <- percent(E3_fixtures_sot$e3_D, accuracy = 0.1)
#
# #Away
#
# E3_fixtures_sot$e3_A <- (
#   E3_fixtures_sot$e3_0_1 + E3_fixtures_sot$e3_0_2 + E3_fixtures_sot$e3_1_2 + E3_fixtures_sot$e3_0_3 + E3_fixtures_sot$e3_1_3 +
#     E3_fixtures_sot$e3_2_3 + E3_fixtures_sot$e3_0_4 + E3_fixtures_sot$e3_1_4 + E3_fixtures_sot$e3_2_4 + E3_fixtures_sot$e3_3_4 +
#     E3_fixtures_sot$e3_0_5 + E3_fixtures_sot$e3_1_5 + E3_fixtures_sot$e3_2_5 + E3_fixtures_sot$e3_3_5 + E3_fixtures_sot$e3_4_5 +
#     E3_fixtures_sot$e3_0_6 + E3_fixtures_sot$e3_1_6 + E3_fixtures_sot$e3_2_6 + E3_fixtures_sot$e3_3_6 + E3_fixtures_sot$e3_4_6 +
#     E3_fixtures_sot$e3_5_6
# )
#
# E3_fixtures_sot$e3_A <- percent(E3_fixtures_sot$e3_A, accuracy = 0.1)
#
# #ov25
# E3_fixtures_sot$e3_ov25 <- (
#   E3_fixtures_sot$e3_2_1 + E3_fixtures_sot$e3_1_2 + E3_fixtures_sot$e3_2_2 + E3_fixtures_sot$e3_3_0 + E3_fixtures_sot$e3_3_1 +
#     E3_fixtures_sot$e3_3_2 + E3_fixtures_sot$e3_0_3 + E3_fixtures_sot$e3_1_3 + E3_fixtures_sot$e3_2_3 + E3_fixtures_sot$e3_3_3 +
#     E3_fixtures_sot$e3_4_0 + E3_fixtures_sot$e3_4_1 + E3_fixtures_sot$e3_4_2 + E3_fixtures_sot$e3_4_3 + E3_fixtures_sot$e3_0_4 +
#     E3_fixtures_sot$e3_1_4 + E3_fixtures_sot$e3_2_4 + E3_fixtures_sot$e3_3_4 + E3_fixtures_sot$e3_4_4 + E3_fixtures_sot$e3_5_0 +
#     E3_fixtures_sot$e3_5_1 + E3_fixtures_sot$e3_5_2 + E3_fixtures_sot$e3_5_3 + E3_fixtures_sot$e3_5_4 + E3_fixtures_sot$e3_0_5 +
#     E3_fixtures_sot$e3_1_5 + E3_fixtures_sot$e3_2_5 + E3_fixtures_sot$e3_3_5 + E3_fixtures_sot$e3_4_5 + E3_fixtures_sot$e3_5_5 +
#     E3_fixtures_sot$e3_6_0 + E3_fixtures_sot$e3_6_1 + E3_fixtures_sot$e3_6_2 + E3_fixtures_sot$e3_6_3 + E3_fixtures_sot$e3_6_4 +
#     E3_fixtures_sot$e3_6_5 + E3_fixtures_sot$e3_0_6 + E3_fixtures_sot$e3_1_6 + E3_fixtures_sot$e3_2_6 + E3_fixtures_sot$e3_3_6 +
#     E3_fixtures_sot$e3_4_6 + E3_fixtures_sot$e3_5_6 + E3_fixtures_sot$e3_6_6
# )
# #un25
# E3_fixtures_sot$e3_un25 <- (
#   E3_fixtures_sot$e3_0_0 + E3_fixtures_sot$e3_1_0 + E3_fixtures_sot$e3_0_1 + E3_fixtures_sot$e3_1_1 + E3_fixtures_sot$e3_2_0 + E3_fixtures_sot$e3_0_2
# )
# #odds
# E3_fixtures_sot$e3_ov25_odds <- round((1/E3_fixtures_sot$e3_ov25),digits = 2)
# E3_fixtures_sot$e3_un25_odds <- round((1/E3_fixtures_sot$e3_un25),digits = 2)
#
# E3_fixtures_sot$e3_ov25_odds
# E3_fixtures_sot$e3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# E3_fixtures_sot$e3_ov25 <- percent(E3_fixtures_sot$e3_ov25, accuracy = 0.1)
#
# E3_fixtures_sot$e3_un25 <- percent(E3_fixtures_sot$e3_un25, accuracy = 0.1)
# E3_fixtures_sot$e3_pssotre <- paste(round(E3_fixtures_sot$e3_xHST,digits = 0),round(E3_fixtures_sot$e3_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(E3_fixtures,'Divisions/E3.xlsx',sheetName = "E3", append = TRUE)
# #################################################################################################################
# #EC
# HomeTeam_ec_sot <- rep(ec_teams, each = length(ec_teams))
# AwayTeam_ec_sot <- rep(ec_teams, length(ec_teams))
# EC_fixtures_sot <- cbind(HomeTeam_ec_sot,AwayTeam_ec_sot)
# EC_fixtures_sot <- as.data.frame(EC_fixtures_sot)
# EC_fixtures_sot <- EC_fixtures_sot[!EC_fixtures_sot$HomeTeam_ec_sot == EC_fixtures_sot$AwayTeam_ec_sot,]
# rownames(EC_fixtures_sot) <- NULL
# EC_fixtures_sot$Div <- "EC"
# EC_fixtures_sot <- EC_fixtures_sot[,c(3,1,2)]
#
# EC_fixtures_sot$avg_HST_ec <- ec_avg_HST
#
# EC_fixtures_sot$ec_homesotas <- rep(ec_home_sotas,each = length(ec_teams)-1)
#
# ec_awaysods_lookup <- cbind(ec_teams,ec_away_sods)
#
# ec_awaysods_lookup <- as.data.frame(ec_awaysods_lookup)
#
# colnames(ec_awaysods_lookup) <- c("AwayTeam_ec_sot","ec_awaysods")
#
#
# require('RH2')
# EC_fixtures_sot$ec_awaysods <- sqldf("SELECT ec_awaysods_lookup.ec_awaysods FROM ec_awaysods_lookup INNER JOIN EC_fixtures_sot ON ec_awaysods_lookup.AwayTeam_ec_sot = EC_fixtures_sot.AwayTeam_ec_sot")
#
# EC_fixtures_sot$avg_AST_ec <- ec_avg_AST
#
# ec_awaysotas_lookup <- cbind(ec_teams,ec_away_sotas)
#
# ec_awaysotas_lookup <- as.data.frame(ec_awaysotas_lookup)
#
# colnames(ec_awaysotas_lookup) <- c("AwayTeam_ec_sot","ec_awaysotas")
#
# EC_fixtures_sot$ec_awaysotas <- sqldf("SELECT ec_awaysotas_lookup.ec_awaysotas FROM ec_awaysotas_lookup INNER JOIN EC_fixtures_sot ON ec_awaysotas_lookup.AwayTeam_ec_sot = EC_fixtures_sot.AwayTeam_ec_sot")
#
# EC_fixtures_sot$ec_homesods <- rep(ec_home_sods,each = length(ec_teams)-1)
#
# EC_fixtures_sot$ec_awaysods <- as.numeric(unlist(EC_fixtures_sot$ec_awaysods))
# #xGH
# EC_fixtures_sot$ec_xHST <- EC_fixtures_sot$avg_HST_ec * EC_fixtures_sot$ec_homesotas * EC_fixtures_sot$ec_awaysods
# #xGA
#
# EC_fixtures_sot$ec_awaysotas <- as.numeric(unlist(EC_fixtures_sot$ec_awaysotas))
#
# EC_fixtures_sot$ec_xAST <- EC_fixtures_sot$avg_AST_ec * EC_fixtures_sot$ec_awaysotas * EC_fixtures_sot$ec_homesods
#
# EC_fixtures_sot$ec_0_0 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_0 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_0_1 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_1 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_0 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_0_2 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_2 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_1 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_2 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_3 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_0 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_1 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_2 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_0_3 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_3 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_3 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_4 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_0 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_1 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_2 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_3 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_0_4 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_4 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_4 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_4 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_5 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_0 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_1 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_2 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_3 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_4 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_0_5 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_5 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_5 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_5 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_5 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_6 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_0 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(0,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_1 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(1,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_2 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(2,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_3 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(3,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_4 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(4,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_6_5 <- round(stats::dpois(6,EC_fixtures_sot$ec_xHST) * stats::dpois(5,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_0_6 <- round(stats::dpois(0,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_1_6 <- round(stats::dpois(1,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_2_6 <- round(stats::dpois(2,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_3_6 <- round(stats::dpois(3,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_4_6 <- round(stats::dpois(4,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# EC_fixtures_sot$ec_5_6 <- round(stats::dpois(5,EC_fixtures_sot$ec_xHST) * stats::dpois(6,EC_fixtures_sot$ec_xAST), digits = 4)
# #Home win
# EC_fixtures_sot$ec_H <- (
#   EC_fixtures_sot$ec_1_0 + EC_fixtures_sot$ec_2_0 + EC_fixtures_sot$ec_2_1 + EC_fixtures_sot$ec_3_0 + EC_fixtures_sot$ec_3_1 +
#     EC_fixtures_sot$ec_3_2 + EC_fixtures_sot$ec_4_0 + EC_fixtures_sot$ec_4_1 + EC_fixtures_sot$ec_4_2 + EC_fixtures_sot$ec_4_3 +
#     EC_fixtures_sot$ec_5_0 + EC_fixtures_sot$ec_5_1 + EC_fixtures_sot$ec_5_2 + EC_fixtures_sot$ec_5_3 + EC_fixtures_sot$ec_5_4 +
#     EC_fixtures_sot$ec_6_0 + EC_fixtures_sot$ec_6_1 + EC_fixtures_sot$ec_6_2 + EC_fixtures_sot$ec_6_3 + EC_fixtures_sot$ec_6_4 +
#     EC_fixtures_sot$ec_6_5
# )
#
# EC_fixtures_sot$ec_H <- percent(EC_fixtures_sot$ec_H, accuracy = 0.1)
#
# #Draw
# EC_fixtures_sot$ec_D <- (
#
#   EC_fixtures_sot$ec_0_0 + EC_fixtures_sot$ec_1_1 + EC_fixtures_sot$ec_2_2 + EC_fixtures_sot$ec_3_3 + EC_fixtures_sot$ec_4_4 +
#     EC_fixtures_sot$ec_5_5 + EC_fixtures_sot$ec_6_6
# )
#
# EC_fixtures_sot$ec_D <- percent(EC_fixtures_sot$ec_D, accuracy = 0.1)
#
# #Away
#
# EC_fixtures_sot$ec_A <- (
#   EC_fixtures_sot$ec_0_1 + EC_fixtures_sot$ec_0_2 + EC_fixtures_sot$ec_1_2 + EC_fixtures_sot$ec_0_3 + EC_fixtures_sot$ec_1_3 +
#     EC_fixtures_sot$ec_2_3 + EC_fixtures_sot$ec_0_4 + EC_fixtures_sot$ec_1_4 + EC_fixtures_sot$ec_2_4 + EC_fixtures_sot$ec_3_4 +
#     EC_fixtures_sot$ec_0_5 + EC_fixtures_sot$ec_1_5 + EC_fixtures_sot$ec_2_5 + EC_fixtures_sot$ec_3_5 + EC_fixtures_sot$ec_4_5 +
#     EC_fixtures_sot$ec_0_6 + EC_fixtures_sot$ec_1_6 + EC_fixtures_sot$ec_2_6 + EC_fixtures_sot$ec_3_6 + EC_fixtures_sot$ec_4_6 +
#     EC_fixtures_sot$ec_5_6
# )
#
# EC_fixtures_sot$ec_A <- percent(EC_fixtures_sot$ec_A, accuracy = 0.1)
#
# #ov25
# EC_fixtures_sot$ec_ov25 <- (
#   EC_fixtures_sot$ec_2_1 + EC_fixtures_sot$ec_1_2 + EC_fixtures_sot$ec_2_2 + EC_fixtures_sot$ec_3_0 + EC_fixtures_sot$ec_3_1 +
#     EC_fixtures_sot$ec_3_2 + EC_fixtures_sot$ec_0_3 + EC_fixtures_sot$ec_1_3 + EC_fixtures_sot$ec_2_3 + EC_fixtures_sot$ec_3_3 +
#     EC_fixtures_sot$ec_4_0 + EC_fixtures_sot$ec_4_1 + EC_fixtures_sot$ec_4_2 + EC_fixtures_sot$ec_4_3 + EC_fixtures_sot$ec_0_4 +
#     EC_fixtures_sot$ec_1_4 + EC_fixtures_sot$ec_2_4 + EC_fixtures_sot$ec_3_4 + EC_fixtures_sot$ec_4_4 + EC_fixtures_sot$ec_5_0 +
#     EC_fixtures_sot$ec_5_1 + EC_fixtures_sot$ec_5_2 + EC_fixtures_sot$ec_5_3 + EC_fixtures_sot$ec_5_4 + EC_fixtures_sot$ec_0_5 +
#     EC_fixtures_sot$ec_1_5 + EC_fixtures_sot$ec_2_5 + EC_fixtures_sot$ec_3_5 + EC_fixtures_sot$ec_4_5 + EC_fixtures_sot$ec_5_5 +
#     EC_fixtures_sot$ec_6_0 + EC_fixtures_sot$ec_6_1 + EC_fixtures_sot$ec_6_2 + EC_fixtures_sot$ec_6_3 + EC_fixtures_sot$ec_6_4 +
#     EC_fixtures_sot$ec_6_5 + EC_fixtures_sot$ec_0_6 + EC_fixtures_sot$ec_1_6 + EC_fixtures_sot$ec_2_6 + EC_fixtures_sot$ec_3_6 +
#     EC_fixtures_sot$ec_4_6 + EC_fixtures_sot$ec_5_6 + EC_fixtures_sot$ec_6_6
# )
# #un25
# EC_fixtures_sot$ec_un25 <- (
#   EC_fixtures_sot$ec_0_0 + EC_fixtures_sot$ec_1_0 + EC_fixtures_sot$ec_0_1 + EC_fixtures_sot$ec_1_1 + EC_fixtures_sot$ec_2_0 + EC_fixtures_sot$ec_0_2
# )
# #odds
# EC_fixtures_sot$ec_ov25_odds <- round((1/EC_fixtures_sot$ec_ov25),digits = 2)
# EC_fixtures_sot$ec_un25_odds <- round((1/EC_fixtures_sot$ec_un25),digits = 2)
#
# EC_fixtures_sot$ec_ov25_odds
# EC_fixtures_sot$ec_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# EC_fixtures_sot$ec_ov25 <- percent(EC_fixtures_sot$ec_ov25, accuracy = 0.1)
#
# EC_fixtures_sot$ec_un25 <- percent(EC_fixtures_sot$ec_un25, accuracy = 0.1)
# EC_fixtures_sot$ec_pssotre <- paste(round(EC_fixtures_sot$ec_xHST,digits = 0),round(EC_fixtures_sot$ec_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(EC_fixtures,'Divisions/EC.xlsx',sheetName = "EC", append = TRUE)
# #################################################################################################################
# #F1
# HomeTeam_f1_sot <- rep(f1_teams, each = length(f1_teams))
# AwayTeam_f1_sot <- rep(f1_teams, length(f1_teams))
# F1_fixtures_sot <- cbind(HomeTeam_f1_sot,AwayTeam_f1_sot)
# F1_fixtures_sot <- as.data.frame(F1_fixtures_sot)
# F1_fixtures_sot <- F1_fixtures_sot[!F1_fixtures_sot$HomeTeam_f1_sot == F1_fixtures_sot$AwayTeam_f1_sot,]
# rownames(F1_fixtures_sot) <- NULL
# F1_fixtures_sot$Div <- "F1"
# F1_fixtures_sot <- F1_fixtures_sot[,c(3,1,2)]
#
# F1_fixtures_sot$avg_HST_f1 <- f1_avg_HST
#
# F1_fixtures_sot$f1_homesotas <- rep(f1_home_sotas,each = length(f1_teams)-1)
#
# f1_awaysods_lookup <- cbind(f1_teams,f1_away_sods)
#
# f1_awaysods_lookup <- as.data.frame(f1_awaysods_lookup)
#
# colnames(f1_awaysods_lookup) <- c("AwayTeam_f1_sot","f1_awaysods")
#
#
# require('RH2')
# F1_fixtures_sot$f1_awaysods <- sqldf("SELECT f1_awaysods_lookup.f1_awaysods FROM f1_awaysods_lookup INNER JOIN F1_fixtures_sot ON f1_awaysods_lookup.AwayTeam_f1_sot = F1_fixtures_sot.AwayTeam_f1_sot")
#
# F1_fixtures_sot$avg_AST_f1 <- f1_avg_AST
#
# f1_awaysotas_lookup <- cbind(f1_teams,f1_away_sotas)
#
# f1_awaysotas_lookup <- as.data.frame(f1_awaysotas_lookup)
#
# colnames(f1_awaysotas_lookup) <- c("AwayTeam_f1_sot","f1_awaysotas")
#
# F1_fixtures_sot$f1_awaysotas <- sqldf("SELECT f1_awaysotas_lookup.f1_awaysotas FROM f1_awaysotas_lookup INNER JOIN F1_fixtures_sot ON f1_awaysotas_lookup.AwayTeam_f1_sot = F1_fixtures_sot.AwayTeam_f1_sot")
#
# F1_fixtures_sot$f1_homesods <- rep(f1_home_sods,each = length(f1_teams)-1)
#
# F1_fixtures_sot$f1_awaysods <- as.numeric(unlist(F1_fixtures_sot$f1_awaysods))
# #xGH
# F1_fixtures_sot$f1_xHST <- F1_fixtures_sot$avg_HST_f1 * F1_fixtures_sot$f1_homesotas * F1_fixtures_sot$f1_awaysods
# #xGA
#
# F1_fixtures_sot$f1_awaysotas <- as.numeric(unlist(F1_fixtures_sot$f1_awaysotas))
#
# F1_fixtures_sot$f1_xAST <- F1_fixtures_sot$avg_AST_f1 * F1_fixtures_sot$f1_awaysotas * F1_fixtures_sot$f1_homesods
#
# F1_fixtures_sot$f1_0_0 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_0 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_0_1 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_1 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_0 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_0_2 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_2 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_1 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_2 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_3 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_0 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_1 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_2 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_0_3 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_3 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_3 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_4 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_0 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_1 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_2 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_3 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_0_4 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_4 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_4 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_4 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_5 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_0 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_1 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_2 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_3 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_4 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_0_5 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_5 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_5 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_5 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_5 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_6 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_0 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(0,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_1 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(1,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_2 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(2,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_3 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(3,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_4 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(4,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_6_5 <- round(stats::dpois(6,F1_fixtures_sot$f1_xHST) * stats::dpois(5,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_0_6 <- round(stats::dpois(0,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_1_6 <- round(stats::dpois(1,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_2_6 <- round(stats::dpois(2,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_3_6 <- round(stats::dpois(3,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_4_6 <- round(stats::dpois(4,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# F1_fixtures_sot$f1_5_6 <- round(stats::dpois(5,F1_fixtures_sot$f1_xHST) * stats::dpois(6,F1_fixtures_sot$f1_xAST), digits = 4)
# #Home win
# F1_fixtures_sot$f1_H <- (
#   F1_fixtures_sot$f1_1_0 + F1_fixtures_sot$f1_2_0 + F1_fixtures_sot$f1_2_1 + F1_fixtures_sot$f1_3_0 + F1_fixtures_sot$f1_3_1 +
#     F1_fixtures_sot$f1_3_2 + F1_fixtures_sot$f1_4_0 + F1_fixtures_sot$f1_4_1 + F1_fixtures_sot$f1_4_2 + F1_fixtures_sot$f1_4_3 +
#     F1_fixtures_sot$f1_5_0 + F1_fixtures_sot$f1_5_1 + F1_fixtures_sot$f1_5_2 + F1_fixtures_sot$f1_5_3 + F1_fixtures_sot$f1_5_4 +
#     F1_fixtures_sot$f1_6_0 + F1_fixtures_sot$f1_6_1 + F1_fixtures_sot$f1_6_2 + F1_fixtures_sot$f1_6_3 + F1_fixtures_sot$f1_6_4 +
#     F1_fixtures_sot$f1_6_5
# )
#
# F1_fixtures_sot$f1_H <- percent(F1_fixtures_sot$f1_H, accuracy = 0.1)
#
# #Draw
# F1_fixtures_sot$f1_D <- (
#
#   F1_fixtures_sot$f1_0_0 + F1_fixtures_sot$f1_1_1 + F1_fixtures_sot$f1_2_2 + F1_fixtures_sot$f1_3_3 + F1_fixtures_sot$f1_4_4 +
#     F1_fixtures_sot$f1_5_5 + F1_fixtures_sot$f1_6_6
# )
#
# F1_fixtures_sot$f1_D <- percent(F1_fixtures_sot$f1_D, accuracy = 0.1)
#
# #Away
#
# F1_fixtures_sot$f1_A <- (
#   F1_fixtures_sot$f1_0_1 + F1_fixtures_sot$f1_0_2 + F1_fixtures_sot$f1_1_2 + F1_fixtures_sot$f1_0_3 + F1_fixtures_sot$f1_1_3 +
#     F1_fixtures_sot$f1_2_3 + F1_fixtures_sot$f1_0_4 + F1_fixtures_sot$f1_1_4 + F1_fixtures_sot$f1_2_4 + F1_fixtures_sot$f1_3_4 +
#     F1_fixtures_sot$f1_0_5 + F1_fixtures_sot$f1_1_5 + F1_fixtures_sot$f1_2_5 + F1_fixtures_sot$f1_3_5 + F1_fixtures_sot$f1_4_5 +
#     F1_fixtures_sot$f1_0_6 + F1_fixtures_sot$f1_1_6 + F1_fixtures_sot$f1_2_6 + F1_fixtures_sot$f1_3_6 + F1_fixtures_sot$f1_4_6 +
#     F1_fixtures_sot$f1_5_6
# )
#
# F1_fixtures_sot$f1_A <- percent(F1_fixtures_sot$f1_A, accuracy = 0.1)
#
# #ov25
# F1_fixtures_sot$f1_ov25 <- (
#   F1_fixtures_sot$f1_2_1 + F1_fixtures_sot$f1_1_2 + F1_fixtures_sot$f1_2_2 + F1_fixtures_sot$f1_3_0 + F1_fixtures_sot$f1_3_1 +
#     F1_fixtures_sot$f1_3_2 + F1_fixtures_sot$f1_0_3 + F1_fixtures_sot$f1_1_3 + F1_fixtures_sot$f1_2_3 + F1_fixtures_sot$f1_3_3 +
#     F1_fixtures_sot$f1_4_0 + F1_fixtures_sot$f1_4_1 + F1_fixtures_sot$f1_4_2 + F1_fixtures_sot$f1_4_3 + F1_fixtures_sot$f1_0_4 +
#     F1_fixtures_sot$f1_1_4 + F1_fixtures_sot$f1_2_4 + F1_fixtures_sot$f1_3_4 + F1_fixtures_sot$f1_4_4 + F1_fixtures_sot$f1_5_0 +
#     F1_fixtures_sot$f1_5_1 + F1_fixtures_sot$f1_5_2 + F1_fixtures_sot$f1_5_3 + F1_fixtures_sot$f1_5_4 + F1_fixtures_sot$f1_0_5 +
#     F1_fixtures_sot$f1_1_5 + F1_fixtures_sot$f1_2_5 + F1_fixtures_sot$f1_3_5 + F1_fixtures_sot$f1_4_5 + F1_fixtures_sot$f1_5_5 +
#     F1_fixtures_sot$f1_6_0 + F1_fixtures_sot$f1_6_1 + F1_fixtures_sot$f1_6_2 + F1_fixtures_sot$f1_6_3 + F1_fixtures_sot$f1_6_4 +
#     F1_fixtures_sot$f1_6_5 + F1_fixtures_sot$f1_0_6 + F1_fixtures_sot$f1_1_6 + F1_fixtures_sot$f1_2_6 + F1_fixtures_sot$f1_3_6 +
#     F1_fixtures_sot$f1_4_6 + F1_fixtures_sot$f1_5_6 + F1_fixtures_sot$f1_6_6
# )
# #un25
# F1_fixtures_sot$f1_un25 <- (
#   F1_fixtures_sot$f1_0_0 + F1_fixtures_sot$f1_1_0 + F1_fixtures_sot$f1_0_1 + F1_fixtures_sot$f1_1_1 + F1_fixtures_sot$f1_2_0 + F1_fixtures_sot$f1_0_2
# )
# #odds
# F1_fixtures_sot$f1_ov25_odds <- round((1/F1_fixtures_sot$f1_ov25),digits = 2)
# F1_fixtures_sot$f1_un25_odds <- round((1/F1_fixtures_sot$f1_un25),digits = 2)
#
# F1_fixtures_sot$f1_ov25_odds
# F1_fixtures_sot$f1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F1_fixtures_sot$f1_ov25 <- percent(F1_fixtures_sot$f1_ov25, accuracy = 0.1)
#
# F1_fixtures_sot$f1_un25 <- percent(F1_fixtures_sot$f1_un25, accuracy = 0.1)
# F1_fixtures_sot$f1_pssotre <- paste(round(F1_fixtures_sot$f1_xHST,digits = 0),round(F1_fixtures_sot$f1_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #F2
# HomeTeam_f2_sot <- rep(f2_teams, each = length(f2_teams))
# AwayTeam_f2_sot <- rep(f2_teams, length(f2_teams))
# F2_fixtures_sot <- cbind(HomeTeam_f2_sot,AwayTeam_f2_sot)
# F2_fixtures_sot <- as.data.frame(F2_fixtures_sot)
# F2_fixtures_sot <- F2_fixtures_sot[!F2_fixtures_sot$HomeTeam_f2_sot == F2_fixtures_sot$AwayTeam_f2_sot,]
# rownames(F2_fixtures_sot) <- NULL
# F2_fixtures_sot$Div <- "F2"
# F2_fixtures_sot <- F2_fixtures_sot[,c(3,1,2)]
#
# F2_fixtures_sot$avg_HST_f2 <- f2_avg_HST
#
# F2_fixtures_sot$f2_homesotas <- rep(f2_home_sotas,each = length(f2_teams)-1)
#
# f2_awaysods_lookup <- cbind(f2_teams,f2_away_sods)
#
# f2_awaysods_lookup <- as.data.frame(f2_awaysods_lookup)
#
# colnames(f2_awaysods_lookup) <- c("AwayTeam_f2_sot","f2_awaysods")
#
#
# require('RH2')
# F2_fixtures_sot$f2_awaysods <- sqldf("SELECT f2_awaysods_lookup.f2_awaysods FROM f2_awaysods_lookup INNER JOIN F2_fixtures_sot ON f2_awaysods_lookup.AwayTeam_f2_sot = F2_fixtures_sot.AwayTeam_f2_sot")
#
# F2_fixtures_sot$avg_AST_f2 <- f2_avg_AST
#
# f2_awaysotas_lookup <- cbind(f2_teams,f2_away_sotas)
#
# f2_awaysotas_lookup <- as.data.frame(f2_awaysotas_lookup)
#
# colnames(f2_awaysotas_lookup) <- c("AwayTeam_f2_sot","f2_awaysotas")
#
# F2_fixtures_sot$f2_awaysotas <- sqldf("SELECT f2_awaysotas_lookup.f2_awaysotas FROM f2_awaysotas_lookup INNER JOIN F2_fixtures_sot ON f2_awaysotas_lookup.AwayTeam_f2_sot = F2_fixtures_sot.AwayTeam_f2_sot")
#
# F2_fixtures_sot$f2_homesods <- rep(f2_home_sods,each = length(f2_teams)-1)
#
# F2_fixtures_sot$f2_awaysods <- as.numeric(unlist(F2_fixtures_sot$f2_awaysods))
# #xGH
# F2_fixtures_sot$f2_xHST <- F2_fixtures_sot$avg_HST_f2 * F2_fixtures_sot$f2_homesotas * F2_fixtures_sot$f2_awaysods
# #xGA
#
# F2_fixtures_sot$f2_awaysotas <- as.numeric(unlist(F2_fixtures_sot$f2_awaysotas))
#
# F2_fixtures_sot$f2_xAST <- F2_fixtures_sot$avg_AST_f2 * F2_fixtures_sot$f2_awaysotas * F2_fixtures_sot$f2_homesods
#
# F2_fixtures_sot$f2_0_0 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_0 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_0_1 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_1 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_0 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_0_2 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_2 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_1 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_2 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_3 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_0 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_1 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_2 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_0_3 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_3 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_3 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_4 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_0 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_1 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_2 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_3 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_0_4 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_4 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_4 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_4 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_5 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_0 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_1 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_2 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_3 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_4 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_0_5 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_5 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_5 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_5 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_5 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_6 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_0 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(0,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_1 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(1,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_2 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(2,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_3 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(3,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_4 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(4,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_6_5 <- round(stats::dpois(6,F2_fixtures_sot$f2_xHST) * stats::dpois(5,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_0_6 <- round(stats::dpois(0,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_1_6 <- round(stats::dpois(1,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_2_6 <- round(stats::dpois(2,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_3_6 <- round(stats::dpois(3,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_4_6 <- round(stats::dpois(4,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# F2_fixtures_sot$f2_5_6 <- round(stats::dpois(5,F2_fixtures_sot$f2_xHST) * stats::dpois(6,F2_fixtures_sot$f2_xAST), digits = 4)
# #Home win
# F2_fixtures_sot$f2_H <- (
#   F2_fixtures_sot$f2_1_0 + F2_fixtures_sot$f2_2_0 + F2_fixtures_sot$f2_2_1 + F2_fixtures_sot$f2_3_0 + F2_fixtures_sot$f2_3_1 +
#     F2_fixtures_sot$f2_3_2 + F2_fixtures_sot$f2_4_0 + F2_fixtures_sot$f2_4_1 + F2_fixtures_sot$f2_4_2 + F2_fixtures_sot$f2_4_3 +
#     F2_fixtures_sot$f2_5_0 + F2_fixtures_sot$f2_5_1 + F2_fixtures_sot$f2_5_2 + F2_fixtures_sot$f2_5_3 + F2_fixtures_sot$f2_5_4 +
#     F2_fixtures_sot$f2_6_0 + F2_fixtures_sot$f2_6_1 + F2_fixtures_sot$f2_6_2 + F2_fixtures_sot$f2_6_3 + F2_fixtures_sot$f2_6_4 +
#     F2_fixtures_sot$f2_6_5
# )
#
# F2_fixtures_sot$f2_H <- percent(F2_fixtures_sot$f2_H, accuracy = 0.1)
#
# #Draw
# F2_fixtures_sot$f2_D <- (
#
#   F2_fixtures_sot$f2_0_0 + F2_fixtures_sot$f2_1_1 + F2_fixtures_sot$f2_2_2 + F2_fixtures_sot$f2_3_3 + F2_fixtures_sot$f2_4_4 +
#     F2_fixtures_sot$f2_5_5 + F2_fixtures_sot$f2_6_6
# )
#
# F2_fixtures_sot$f2_D <- percent(F2_fixtures_sot$f2_D, accuracy = 0.1)
#
# #Away
#
# F2_fixtures_sot$f2_A <- (
#   F2_fixtures_sot$f2_0_1 + F2_fixtures_sot$f2_0_2 + F2_fixtures_sot$f2_1_2 + F2_fixtures_sot$f2_0_3 + F2_fixtures_sot$f2_1_3 +
#     F2_fixtures_sot$f2_2_3 + F2_fixtures_sot$f2_0_4 + F2_fixtures_sot$f2_1_4 + F2_fixtures_sot$f2_2_4 + F2_fixtures_sot$f2_3_4 +
#     F2_fixtures_sot$f2_0_5 + F2_fixtures_sot$f2_1_5 + F2_fixtures_sot$f2_2_5 + F2_fixtures_sot$f2_3_5 + F2_fixtures_sot$f2_4_5 +
#     F2_fixtures_sot$f2_0_6 + F2_fixtures_sot$f2_1_6 + F2_fixtures_sot$f2_2_6 + F2_fixtures_sot$f2_3_6 + F2_fixtures_sot$f2_4_6 +
#     F2_fixtures_sot$f2_5_6
# )
#
# F2_fixtures_sot$f2_A <- percent(F2_fixtures_sot$f2_A, accuracy = 0.1)
#
# #ov25
# F2_fixtures_sot$f2_ov25 <- (
#   F2_fixtures_sot$f2_2_1 + F2_fixtures_sot$f2_1_2 + F2_fixtures_sot$f2_2_2 + F2_fixtures_sot$f2_3_0 + F2_fixtures_sot$f2_3_1 +
#     F2_fixtures_sot$f2_3_2 + F2_fixtures_sot$f2_0_3 + F2_fixtures_sot$f2_1_3 + F2_fixtures_sot$f2_2_3 + F2_fixtures_sot$f2_3_3 +
#     F2_fixtures_sot$f2_4_0 + F2_fixtures_sot$f2_4_1 + F2_fixtures_sot$f2_4_2 + F2_fixtures_sot$f2_4_3 + F2_fixtures_sot$f2_0_4 +
#     F2_fixtures_sot$f2_1_4 + F2_fixtures_sot$f2_2_4 + F2_fixtures_sot$f2_3_4 + F2_fixtures_sot$f2_4_4 + F2_fixtures_sot$f2_5_0 +
#     F2_fixtures_sot$f2_5_1 + F2_fixtures_sot$f2_5_2 + F2_fixtures_sot$f2_5_3 + F2_fixtures_sot$f2_5_4 + F2_fixtures_sot$f2_0_5 +
#     F2_fixtures_sot$f2_1_5 + F2_fixtures_sot$f2_2_5 + F2_fixtures_sot$f2_3_5 + F2_fixtures_sot$f2_4_5 + F2_fixtures_sot$f2_5_5 +
#     F2_fixtures_sot$f2_6_0 + F2_fixtures_sot$f2_6_1 + F2_fixtures_sot$f2_6_2 + F2_fixtures_sot$f2_6_3 + F2_fixtures_sot$f2_6_4 +
#     F2_fixtures_sot$f2_6_5 + F2_fixtures_sot$f2_0_6 + F2_fixtures_sot$f2_1_6 + F2_fixtures_sot$f2_2_6 + F2_fixtures_sot$f2_3_6 +
#     F2_fixtures_sot$f2_4_6 + F2_fixtures_sot$f2_5_6 + F2_fixtures_sot$f2_6_6
# )
# #un25
# F2_fixtures_sot$f2_un25 <- (
#   F2_fixtures_sot$f2_0_0 + F2_fixtures_sot$f2_1_0 + F2_fixtures_sot$f2_0_1 + F2_fixtures_sot$f2_1_1 + F2_fixtures_sot$f2_2_0 + F2_fixtures_sot$f2_0_2
# )
# #odds
# F2_fixtures_sot$f2_ov25_odds <- round((1/F2_fixtures_sot$f2_ov25),digits = 2)
# F2_fixtures_sot$f2_un25_odds <- round((1/F2_fixtures_sot$f2_un25),digits = 2)
#
# F2_fixtures_sot$f2_ov25_odds
# F2_fixtures_sot$f2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# F2_fixtures_sot$f2_ov25 <- percent(F2_fixtures_sot$f2_ov25, accuracy = 0.1)
#
# F2_fixtures_sot$f2_un25 <- percent(F2_fixtures_sot$f2_un25, accuracy = 0.1)
# F2_fixtures_sot$f2_pssotre <- paste(round(F2_fixtures_sot$f2_xHST,digits = 0),round(F2_fixtures_sot$f2_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #G1
# HomeTeam_g1_sot <- rep(g1_teams, each = length(g1_teams))
# AwayTeam_g1_sot <- rep(g1_teams, length(g1_teams))
# G1_fixtures_sot <- cbind(HomeTeam_g1_sot,AwayTeam_g1_sot)
# G1_fixtures_sot <- as.data.frame(G1_fixtures_sot)
# G1_fixtures_sot <- G1_fixtures_sot[!G1_fixtures_sot$HomeTeam_g1_sot == G1_fixtures_sot$AwayTeam_g1_sot,]
# rownames(G1_fixtures_sot) <- NULL
# G1_fixtures_sot$Div <- "G1"
# G1_fixtures_sot <- G1_fixtures_sot[,c(3,1,2)]
#
# G1_fixtures_sot$avg_HST_g1 <- g1_avg_HST
#
# G1_fixtures_sot$g1_homesotas <- rep(g1_home_sotas,each = length(g1_teams)-1)
#
# g1_awaysods_lookup <- cbind(g1_teams,g1_away_sods)
#
# g1_awaysods_lookup <- as.data.frame(g1_awaysods_lookup)
#
# colnames(g1_awaysods_lookup) <- c("AwayTeam_g1_sot","g1_awaysods")
#
#
# require('RH2')
# G1_fixtures_sot$g1_awaysods <- sqldf("SELECT g1_awaysods_lookup.g1_awaysods FROM g1_awaysods_lookup INNER JOIN G1_fixtures_sot ON g1_awaysods_lookup.AwayTeam_g1_sot = G1_fixtures_sot.AwayTeam_g1_sot")
#
# G1_fixtures_sot$avg_AST_g1 <- g1_avg_AST
#
# g1_awaysotas_lookup <- cbind(g1_teams,g1_away_sotas)
#
# g1_awaysotas_lookup <- as.data.frame(g1_awaysotas_lookup)
#
# colnames(g1_awaysotas_lookup) <- c("AwayTeam_g1_sot","g1_awaysotas")
#
# G1_fixtures_sot$g1_awaysotas <- sqldf("SELECT g1_awaysotas_lookup.g1_awaysotas FROM g1_awaysotas_lookup INNER JOIN G1_fixtures_sot ON g1_awaysotas_lookup.AwayTeam_g1_sot = G1_fixtures_sot.AwayTeam_g1_sot")
#
# G1_fixtures_sot$g1_homesods <- rep(g1_home_sods,each = length(g1_teams)-1)
#
# G1_fixtures_sot$g1_awaysods <- as.numeric(unlist(G1_fixtures_sot$g1_awaysods))
# #xGH
# G1_fixtures_sot$g1_xHST <- G1_fixtures_sot$avg_HST_g1 * G1_fixtures_sot$g1_homesotas * G1_fixtures_sot$g1_awaysods
# #xGA
#
# G1_fixtures_sot$g1_awaysotas <- as.numeric(unlist(G1_fixtures_sot$g1_awaysotas))
#
# G1_fixtures_sot$g1_xAST <- G1_fixtures_sot$avg_AST_g1 * G1_fixtures_sot$g1_awaysotas * G1_fixtures_sot$g1_homesods
#
# G1_fixtures_sot$g1_0_0 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_0 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_0_1 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_1 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_0 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_0_2 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_2 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_1 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_2 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_3 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_0 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_1 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_2 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_0_3 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_3 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_3 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_4 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_0 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_1 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_2 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_3 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_0_4 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_4 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_4 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_4 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_5 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_0 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_1 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_2 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_3 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_4 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_0_5 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_5 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_5 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_5 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_5 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_6 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_0 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(0,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_1 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(1,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_2 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(2,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_3 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(3,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_4 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(4,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_6_5 <- round(stats::dpois(6,G1_fixtures_sot$g1_xHST) * stats::dpois(5,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_0_6 <- round(stats::dpois(0,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_1_6 <- round(stats::dpois(1,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_2_6 <- round(stats::dpois(2,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_3_6 <- round(stats::dpois(3,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_4_6 <- round(stats::dpois(4,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# G1_fixtures_sot$g1_5_6 <- round(stats::dpois(5,G1_fixtures_sot$g1_xHST) * stats::dpois(6,G1_fixtures_sot$g1_xAST), digits = 4)
# #Home win
# G1_fixtures_sot$g1_H <- (
#   G1_fixtures_sot$g1_1_0 + G1_fixtures_sot$g1_2_0 + G1_fixtures_sot$g1_2_1 + G1_fixtures_sot$g1_3_0 + G1_fixtures_sot$g1_3_1 +
#     G1_fixtures_sot$g1_3_2 + G1_fixtures_sot$g1_4_0 + G1_fixtures_sot$g1_4_1 + G1_fixtures_sot$g1_4_2 + G1_fixtures_sot$g1_4_3 +
#     G1_fixtures_sot$g1_5_0 + G1_fixtures_sot$g1_5_1 + G1_fixtures_sot$g1_5_2 + G1_fixtures_sot$g1_5_3 + G1_fixtures_sot$g1_5_4 +
#     G1_fixtures_sot$g1_6_0 + G1_fixtures_sot$g1_6_1 + G1_fixtures_sot$g1_6_2 + G1_fixtures_sot$g1_6_3 + G1_fixtures_sot$g1_6_4 +
#     G1_fixtures_sot$g1_6_5
# )
#
# G1_fixtures_sot$g1_H <- percent(G1_fixtures_sot$g1_H, accuracy = 0.1)
#
# #Draw
# G1_fixtures_sot$g1_D <- (
#
#   G1_fixtures_sot$g1_0_0 + G1_fixtures_sot$g1_1_1 + G1_fixtures_sot$g1_2_2 + G1_fixtures_sot$g1_3_3 + G1_fixtures_sot$g1_4_4 +
#     G1_fixtures_sot$g1_5_5 + G1_fixtures_sot$g1_6_6
# )
#
# G1_fixtures_sot$g1_D <- percent(G1_fixtures_sot$g1_D, accuracy = 0.1)
#
# #Away
#
# G1_fixtures_sot$g1_A <- (
#   G1_fixtures_sot$g1_0_1 + G1_fixtures_sot$g1_0_2 + G1_fixtures_sot$g1_1_2 + G1_fixtures_sot$g1_0_3 + G1_fixtures_sot$g1_1_3 +
#     G1_fixtures_sot$g1_2_3 + G1_fixtures_sot$g1_0_4 + G1_fixtures_sot$g1_1_4 + G1_fixtures_sot$g1_2_4 + G1_fixtures_sot$g1_3_4 +
#     G1_fixtures_sot$g1_0_5 + G1_fixtures_sot$g1_1_5 + G1_fixtures_sot$g1_2_5 + G1_fixtures_sot$g1_3_5 + G1_fixtures_sot$g1_4_5 +
#     G1_fixtures_sot$g1_0_6 + G1_fixtures_sot$g1_1_6 + G1_fixtures_sot$g1_2_6 + G1_fixtures_sot$g1_3_6 + G1_fixtures_sot$g1_4_6 +
#     G1_fixtures_sot$g1_5_6
# )
#
# G1_fixtures_sot$g1_A <- percent(G1_fixtures_sot$g1_A, accuracy = 0.1)
#
# #ov25
# G1_fixtures_sot$g1_ov25 <- (
#   G1_fixtures_sot$g1_2_1 + G1_fixtures_sot$g1_1_2 + G1_fixtures_sot$g1_2_2 + G1_fixtures_sot$g1_3_0 + G1_fixtures_sot$g1_3_1 +
#     G1_fixtures_sot$g1_3_2 + G1_fixtures_sot$g1_0_3 + G1_fixtures_sot$g1_1_3 + G1_fixtures_sot$g1_2_3 + G1_fixtures_sot$g1_3_3 +
#     G1_fixtures_sot$g1_4_0 + G1_fixtures_sot$g1_4_1 + G1_fixtures_sot$g1_4_2 + G1_fixtures_sot$g1_4_3 + G1_fixtures_sot$g1_0_4 +
#     G1_fixtures_sot$g1_1_4 + G1_fixtures_sot$g1_2_4 + G1_fixtures_sot$g1_3_4 + G1_fixtures_sot$g1_4_4 + G1_fixtures_sot$g1_5_0 +
#     G1_fixtures_sot$g1_5_1 + G1_fixtures_sot$g1_5_2 + G1_fixtures_sot$g1_5_3 + G1_fixtures_sot$g1_5_4 + G1_fixtures_sot$g1_0_5 +
#     G1_fixtures_sot$g1_1_5 + G1_fixtures_sot$g1_2_5 + G1_fixtures_sot$g1_3_5 + G1_fixtures_sot$g1_4_5 + G1_fixtures_sot$g1_5_5 +
#     G1_fixtures_sot$g1_6_0 + G1_fixtures_sot$g1_6_1 + G1_fixtures_sot$g1_6_2 + G1_fixtures_sot$g1_6_3 + G1_fixtures_sot$g1_6_4 +
#     G1_fixtures_sot$g1_6_5 + G1_fixtures_sot$g1_0_6 + G1_fixtures_sot$g1_1_6 + G1_fixtures_sot$g1_2_6 + G1_fixtures_sot$g1_3_6 +
#     G1_fixtures_sot$g1_4_6 + G1_fixtures_sot$g1_5_6 + G1_fixtures_sot$g1_6_6
# )
# #un25
# G1_fixtures_sot$g1_un25 <- (
#   G1_fixtures_sot$g1_0_0 + G1_fixtures_sot$g1_1_0 + G1_fixtures_sot$g1_0_1 + G1_fixtures_sot$g1_1_1 + G1_fixtures_sot$g1_2_0 + G1_fixtures_sot$g1_0_2
# )
# #odds
# G1_fixtures_sot$g1_ov25_odds <- round((1/G1_fixtures_sot$g1_ov25),digits = 2)
# G1_fixtures_sot$g1_un25_odds <- round((1/G1_fixtures_sot$g1_un25),digits = 2)
#
# G1_fixtures_sot$g1_ov25_odds
# G1_fixtures_sot$g1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# G1_fixtures_sot$g1_ov25 <- percent(G1_fixtures_sot$g1_ov25, accuracy = 0.1)
#
# G1_fixtures_sot$g1_un25 <- percent(G1_fixtures_sot$g1_un25, accuracy = 0.1)
# G1_fixtures_sot$g1_pssotre <- paste(round(G1_fixtures_sot$g1_xHST,digits = 0),round(G1_fixtures_sot$g1_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################
# #I1
# HomeTeam_i1_sot <- rep(i1_teams, each = length(i1_teams))
# AwayTeam_i1_sot <- rep(i1_teams, length(i1_teams))
# I1_fixtures_sot <- cbind(HomeTeam_i1_sot,AwayTeam_i1_sot)
# I1_fixtures_sot <- as.data.frame(I1_fixtures_sot)
# I1_fixtures_sot <- I1_fixtures_sot[!I1_fixtures_sot$HomeTeam_i1_sot == I1_fixtures_sot$AwayTeam_i1_sot,]
# rownames(I1_fixtures_sot) <- NULL
# I1_fixtures_sot$Div <- "I1"
# I1_fixtures_sot <- I1_fixtures_sot[,c(3,1,2)]
#
# I1_fixtures_sot$avg_HST_i1 <- i1_avg_HST
#
# I1_fixtures_sot$i1_homesotas <- rep(i1_home_sotas,each = length(i1_teams)-1)
#
# i1_awaysods_lookup <- cbind(i1_teams,i1_away_sods)
#
# i1_awaysods_lookup <- as.data.frame(i1_awaysods_lookup)
#
# colnames(i1_awaysods_lookup) <- c("AwayTeam_i1_sot","i1_awaysods")
#
#
# require('RH2')
# I1_fixtures_sot$i1_awaysods <- sqldf("SELECT i1_awaysods_lookup.i1_awaysods FROM i1_awaysods_lookup INNER JOIN I1_fixtures_sot ON i1_awaysods_lookup.AwayTeam_i1_sot = I1_fixtures_sot.AwayTeam_i1_sot")
#
# I1_fixtures_sot$avg_AST_i1 <- i1_avg_AST
#
# i1_awaysotas_lookup <- cbind(i1_teams,i1_away_sotas)
#
# i1_awaysotas_lookup <- as.data.frame(i1_awaysotas_lookup)
#
# colnames(i1_awaysotas_lookup) <- c("AwayTeam_i1_sot","i1_awaysotas")
#
# I1_fixtures_sot$i1_awaysotas <- sqldf("SELECT i1_awaysotas_lookup.i1_awaysotas FROM i1_awaysotas_lookup INNER JOIN I1_fixtures_sot ON i1_awaysotas_lookup.AwayTeam_i1_sot = I1_fixtures_sot.AwayTeam_i1_sot")
#
# I1_fixtures_sot$i1_homesods <- rep(i1_home_sods,each = length(i1_teams)-1)
#
# I1_fixtures_sot$i1_awaysods <- as.numeric(unlist(I1_fixtures_sot$i1_awaysods))
# #xGH
# I1_fixtures_sot$i1_xHST <- I1_fixtures_sot$avg_HST_i1 * I1_fixtures_sot$i1_homesotas * I1_fixtures_sot$i1_awaysods
# #xGA
#
# I1_fixtures_sot$i1_awaysotas <- as.numeric(unlist(I1_fixtures_sot$i1_awaysotas))
#
# I1_fixtures_sot$i1_xAST <- I1_fixtures_sot$avg_AST_i1 * I1_fixtures_sot$i1_awaysotas * I1_fixtures_sot$i1_homesods
#
# I1_fixtures_sot$i1_0_0 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_0 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_0_1 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_1 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_0 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_0_2 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_2 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_1 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_2 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_3 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_0 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_1 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_2 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_0_3 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_3 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_3 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_4 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_0 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_1 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_2 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_3 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_0_4 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_4 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_4 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_4 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_5 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_0 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_1 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_2 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_3 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_4 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_0_5 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_5 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_5 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_5 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_5 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_6 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_0 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(0,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_1 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(1,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_2 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(2,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_3 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(3,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_4 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(4,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_6_5 <- round(stats::dpois(6,I1_fixtures_sot$i1_xHST) * stats::dpois(5,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_0_6 <- round(stats::dpois(0,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_1_6 <- round(stats::dpois(1,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_2_6 <- round(stats::dpois(2,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_3_6 <- round(stats::dpois(3,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_4_6 <- round(stats::dpois(4,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# I1_fixtures_sot$i1_5_6 <- round(stats::dpois(5,I1_fixtures_sot$i1_xHST) * stats::dpois(6,I1_fixtures_sot$i1_xAST), digits = 4)
# #Home win
# I1_fixtures_sot$i1_H <- (
#   I1_fixtures_sot$i1_1_0 + I1_fixtures_sot$i1_2_0 + I1_fixtures_sot$i1_2_1 + I1_fixtures_sot$i1_3_0 + I1_fixtures_sot$i1_3_1 +
#     I1_fixtures_sot$i1_3_2 + I1_fixtures_sot$i1_4_0 + I1_fixtures_sot$i1_4_1 + I1_fixtures_sot$i1_4_2 + I1_fixtures_sot$i1_4_3 +
#     I1_fixtures_sot$i1_5_0 + I1_fixtures_sot$i1_5_1 + I1_fixtures_sot$i1_5_2 + I1_fixtures_sot$i1_5_3 + I1_fixtures_sot$i1_5_4 +
#     I1_fixtures_sot$i1_6_0 + I1_fixtures_sot$i1_6_1 + I1_fixtures_sot$i1_6_2 + I1_fixtures_sot$i1_6_3 + I1_fixtures_sot$i1_6_4 +
#     I1_fixtures_sot$i1_6_5
# )
#
# I1_fixtures_sot$i1_H <- percent(I1_fixtures_sot$i1_H, accuracy = 0.1)
#
# #Draw
# I1_fixtures_sot$i1_D <- (
#
#   I1_fixtures_sot$i1_0_0 + I1_fixtures_sot$i1_1_1 + I1_fixtures_sot$i1_2_2 + I1_fixtures_sot$i1_3_3 + I1_fixtures_sot$i1_4_4 +
#     I1_fixtures_sot$i1_5_5 + I1_fixtures_sot$i1_6_6
# )
#
# I1_fixtures_sot$i1_D <- percent(I1_fixtures_sot$i1_D, accuracy = 0.1)
#
# #Away
#
# I1_fixtures_sot$i1_A <- (
#   I1_fixtures_sot$i1_0_1 + I1_fixtures_sot$i1_0_2 + I1_fixtures_sot$i1_1_2 + I1_fixtures_sot$i1_0_3 + I1_fixtures_sot$i1_1_3 +
#     I1_fixtures_sot$i1_2_3 + I1_fixtures_sot$i1_0_4 + I1_fixtures_sot$i1_1_4 + I1_fixtures_sot$i1_2_4 + I1_fixtures_sot$i1_3_4 +
#     I1_fixtures_sot$i1_0_5 + I1_fixtures_sot$i1_1_5 + I1_fixtures_sot$i1_2_5 + I1_fixtures_sot$i1_3_5 + I1_fixtures_sot$i1_4_5 +
#     I1_fixtures_sot$i1_0_6 + I1_fixtures_sot$i1_1_6 + I1_fixtures_sot$i1_2_6 + I1_fixtures_sot$i1_3_6 + I1_fixtures_sot$i1_4_6 +
#     I1_fixtures_sot$i1_5_6
# )
#
# I1_fixtures_sot$i1_A <- percent(I1_fixtures_sot$i1_A, accuracy = 0.1)
#
# #ov25
# I1_fixtures_sot$i1_ov25 <- (
#   I1_fixtures_sot$i1_2_1 + I1_fixtures_sot$i1_1_2 + I1_fixtures_sot$i1_2_2 + I1_fixtures_sot$i1_3_0 + I1_fixtures_sot$i1_3_1 +
#     I1_fixtures_sot$i1_3_2 + I1_fixtures_sot$i1_0_3 + I1_fixtures_sot$i1_1_3 + I1_fixtures_sot$i1_2_3 + I1_fixtures_sot$i1_3_3 +
#     I1_fixtures_sot$i1_4_0 + I1_fixtures_sot$i1_4_1 + I1_fixtures_sot$i1_4_2 + I1_fixtures_sot$i1_4_3 + I1_fixtures_sot$i1_0_4 +
#     I1_fixtures_sot$i1_1_4 + I1_fixtures_sot$i1_2_4 + I1_fixtures_sot$i1_3_4 + I1_fixtures_sot$i1_4_4 + I1_fixtures_sot$i1_5_0 +
#     I1_fixtures_sot$i1_5_1 + I1_fixtures_sot$i1_5_2 + I1_fixtures_sot$i1_5_3 + I1_fixtures_sot$i1_5_4 + I1_fixtures_sot$i1_0_5 +
#     I1_fixtures_sot$i1_1_5 + I1_fixtures_sot$i1_2_5 + I1_fixtures_sot$i1_3_5 + I1_fixtures_sot$i1_4_5 + I1_fixtures_sot$i1_5_5 +
#     I1_fixtures_sot$i1_6_0 + I1_fixtures_sot$i1_6_1 + I1_fixtures_sot$i1_6_2 + I1_fixtures_sot$i1_6_3 + I1_fixtures_sot$i1_6_4 +
#     I1_fixtures_sot$i1_6_5 + I1_fixtures_sot$i1_0_6 + I1_fixtures_sot$i1_1_6 + I1_fixtures_sot$i1_2_6 + I1_fixtures_sot$i1_3_6 +
#     I1_fixtures_sot$i1_4_6 + I1_fixtures_sot$i1_5_6 + I1_fixtures_sot$i1_6_6
# )
# #un25
# I1_fixtures_sot$i1_un25 <- (
#   I1_fixtures_sot$i1_0_0 + I1_fixtures_sot$i1_1_0 + I1_fixtures_sot$i1_0_1 + I1_fixtures_sot$i1_1_1 + I1_fixtures_sot$i1_2_0 + I1_fixtures_sot$i1_0_2
# )
# #odds
# I1_fixtures_sot$i1_ov25_odds <- round((1/I1_fixtures_sot$i1_ov25),digits = 2)
# I1_fixtures_sot$i1_un25_odds <- round((1/I1_fixtures_sot$i1_un25),digits = 2)
#
# I1_fixtures_sot$i1_ov25_odds
# I1_fixtures_sot$i1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I1_fixtures_sot$i1_ov25 <- percent(I1_fixtures_sot$i1_ov25, accuracy = 0.1)
#
# I1_fixtures_sot$i1_un25 <- percent(I1_fixtures_sot$i1_un25, accuracy = 0.1)
# I1_fixtures_sot$i1_pssotre <- paste(round(I1_fixtures_sot$i1_xHST,digits = 0),round(I1_fixtures_sot$i1_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(I1_fixtures,'Divisions/I1.xlsx',sheetName = "I1", append = TRUE)
# ##################
# #I2
# HomeTeam_i2_sot <- rep(i2_teams, each = length(i2_teams))
# AwayTeam_i2_sot <- rep(i2_teams, length(i2_teams))
# I2_fixtures_sot <- cbind(HomeTeam_i2_sot,AwayTeam_i2_sot)
# I2_fixtures_sot <- as.data.frame(I2_fixtures_sot)
# I2_fixtures_sot <- I2_fixtures_sot[!I2_fixtures_sot$HomeTeam_i2_sot == I2_fixtures_sot$AwayTeam_i2_sot,]
# rownames(I2_fixtures_sot) <- NULL
# I2_fixtures_sot$Div <- "I2"
# I2_fixtures_sot <- I2_fixtures_sot[,c(3,1,2)]
#
# I2_fixtures_sot$avg_HST_i2 <- i2_avg_HST
#
# I2_fixtures_sot$i2_homesotas <- rep(i2_home_sotas,each = length(i2_teams)-1)
#
# i2_awaysods_lookup <- cbind(i2_teams,i2_away_sods)
#
# i2_awaysods_lookup <- as.data.frame(i2_awaysods_lookup)
#
# colnames(i2_awaysods_lookup) <- c("AwayTeam_i2_sot","i2_awaysods")
#
#
# require('RH2')
# I2_fixtures_sot$i2_awaysods <- sqldf("SELECT i2_awaysods_lookup.i2_awaysods FROM i2_awaysods_lookup INNER JOIN I2_fixtures_sot ON i2_awaysods_lookup.AwayTeam_i2_sot = I2_fixtures_sot.AwayTeam_i2_sot")
#
# I2_fixtures_sot$avg_AST_i2 <- i2_avg_AST
#
# i2_awaysotas_lookup <- cbind(i2_teams,i2_away_sotas)
#
# i2_awaysotas_lookup <- as.data.frame(i2_awaysotas_lookup)
#
# colnames(i2_awaysotas_lookup) <- c("AwayTeam_i2_sot","i2_awaysotas")
#
# I2_fixtures_sot$i2_awaysotas <- sqldf("SELECT i2_awaysotas_lookup.i2_awaysotas FROM i2_awaysotas_lookup INNER JOIN I2_fixtures_sot ON i2_awaysotas_lookup.AwayTeam_i2_sot = I2_fixtures_sot.AwayTeam_i2_sot")
#
# I2_fixtures_sot$i2_homesods <- rep(i2_home_sods,each = length(i2_teams)-1)
#
# I2_fixtures_sot$i2_awaysods <- as.numeric(unlist(I2_fixtures_sot$i2_awaysods))
# #xGH
# I2_fixtures_sot$i2_xHST <- I2_fixtures_sot$avg_HST_i2 * I2_fixtures_sot$i2_homesotas * I2_fixtures_sot$i2_awaysods
# #xGA
#
# I2_fixtures_sot$i2_awaysotas <- as.numeric(unlist(I2_fixtures_sot$i2_awaysotas))
#
# I2_fixtures_sot$i2_xAST <- I2_fixtures_sot$avg_AST_i2 * I2_fixtures_sot$i2_awaysotas * I2_fixtures_sot$i2_homesods
#
# I2_fixtures_sot$i2_0_0 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_0 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_0_1 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_1 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_0 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_0_2 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_2 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_1 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_2 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_3 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_0 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_1 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_2 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_0_3 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_3 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_3 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_4 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_0 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_1 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_2 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_3 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_0_4 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_4 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_4 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_4 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_5 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_0 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_1 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_2 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_3 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_4 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_0_5 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_5 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_5 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_5 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_5 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_6 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_0 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(0,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_1 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(1,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_2 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(2,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_3 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(3,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_4 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(4,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_6_5 <- round(stats::dpois(6,I2_fixtures_sot$i2_xHST) * stats::dpois(5,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_0_6 <- round(stats::dpois(0,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_1_6 <- round(stats::dpois(1,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_2_6 <- round(stats::dpois(2,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_3_6 <- round(stats::dpois(3,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_4_6 <- round(stats::dpois(4,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# I2_fixtures_sot$i2_5_6 <- round(stats::dpois(5,I2_fixtures_sot$i2_xHST) * stats::dpois(6,I2_fixtures_sot$i2_xAST), digits = 4)
# #Home win
# I2_fixtures_sot$i2_H <- (
#   I2_fixtures_sot$i2_1_0 + I2_fixtures_sot$i2_2_0 + I2_fixtures_sot$i2_2_1 + I2_fixtures_sot$i2_3_0 + I2_fixtures_sot$i2_3_1 +
#     I2_fixtures_sot$i2_3_2 + I2_fixtures_sot$i2_4_0 + I2_fixtures_sot$i2_4_1 + I2_fixtures_sot$i2_4_2 + I2_fixtures_sot$i2_4_3 +
#     I2_fixtures_sot$i2_5_0 + I2_fixtures_sot$i2_5_1 + I2_fixtures_sot$i2_5_2 + I2_fixtures_sot$i2_5_3 + I2_fixtures_sot$i2_5_4 +
#     I2_fixtures_sot$i2_6_0 + I2_fixtures_sot$i2_6_1 + I2_fixtures_sot$i2_6_2 + I2_fixtures_sot$i2_6_3 + I2_fixtures_sot$i2_6_4 +
#     I2_fixtures_sot$i2_6_5
# )
#
# I2_fixtures_sot$i2_H <- percent(I2_fixtures_sot$i2_H, accuracy = 0.1)
#
# #Draw
# I2_fixtures_sot$i2_D <- (
#
#   I2_fixtures_sot$i2_0_0 + I2_fixtures_sot$i2_1_1 + I2_fixtures_sot$i2_2_2 + I2_fixtures_sot$i2_3_3 + I2_fixtures_sot$i2_4_4 +
#     I2_fixtures_sot$i2_5_5 + I2_fixtures_sot$i2_6_6
# )
#
# I2_fixtures_sot$i2_D <- percent(I2_fixtures_sot$i2_D, accuracy = 0.1)
#
# #Away
#
# I2_fixtures_sot$i2_A <- (
#   I2_fixtures_sot$i2_0_1 + I2_fixtures_sot$i2_0_2 + I2_fixtures_sot$i2_1_2 + I2_fixtures_sot$i2_0_3 + I2_fixtures_sot$i2_1_3 +
#     I2_fixtures_sot$i2_2_3 + I2_fixtures_sot$i2_0_4 + I2_fixtures_sot$i2_1_4 + I2_fixtures_sot$i2_2_4 + I2_fixtures_sot$i2_3_4 +
#     I2_fixtures_sot$i2_0_5 + I2_fixtures_sot$i2_1_5 + I2_fixtures_sot$i2_2_5 + I2_fixtures_sot$i2_3_5 + I2_fixtures_sot$i2_4_5 +
#     I2_fixtures_sot$i2_0_6 + I2_fixtures_sot$i2_1_6 + I2_fixtures_sot$i2_2_6 + I2_fixtures_sot$i2_3_6 + I2_fixtures_sot$i2_4_6 +
#     I2_fixtures_sot$i2_5_6
# )
#
# I2_fixtures_sot$i2_A <- percent(I2_fixtures_sot$i2_A, accuracy = 0.1)
#
# #ov25
# I2_fixtures_sot$i2_ov25 <- (
#   I2_fixtures_sot$i2_2_1 + I2_fixtures_sot$i2_1_2 + I2_fixtures_sot$i2_2_2 + I2_fixtures_sot$i2_3_0 + I2_fixtures_sot$i2_3_1 +
#     I2_fixtures_sot$i2_3_2 + I2_fixtures_sot$i2_0_3 + I2_fixtures_sot$i2_1_3 + I2_fixtures_sot$i2_2_3 + I2_fixtures_sot$i2_3_3 +
#     I2_fixtures_sot$i2_4_0 + I2_fixtures_sot$i2_4_1 + I2_fixtures_sot$i2_4_2 + I2_fixtures_sot$i2_4_3 + I2_fixtures_sot$i2_0_4 +
#     I2_fixtures_sot$i2_1_4 + I2_fixtures_sot$i2_2_4 + I2_fixtures_sot$i2_3_4 + I2_fixtures_sot$i2_4_4 + I2_fixtures_sot$i2_5_0 +
#     I2_fixtures_sot$i2_5_1 + I2_fixtures_sot$i2_5_2 + I2_fixtures_sot$i2_5_3 + I2_fixtures_sot$i2_5_4 + I2_fixtures_sot$i2_0_5 +
#     I2_fixtures_sot$i2_1_5 + I2_fixtures_sot$i2_2_5 + I2_fixtures_sot$i2_3_5 + I2_fixtures_sot$i2_4_5 + I2_fixtures_sot$i2_5_5 +
#     I2_fixtures_sot$i2_6_0 + I2_fixtures_sot$i2_6_1 + I2_fixtures_sot$i2_6_2 + I2_fixtures_sot$i2_6_3 + I2_fixtures_sot$i2_6_4 +
#     I2_fixtures_sot$i2_6_5 + I2_fixtures_sot$i2_0_6 + I2_fixtures_sot$i2_1_6 + I2_fixtures_sot$i2_2_6 + I2_fixtures_sot$i2_3_6 +
#     I2_fixtures_sot$i2_4_6 + I2_fixtures_sot$i2_5_6 + I2_fixtures_sot$i2_6_6
# )
# #un25
# I2_fixtures_sot$i2_un25 <- (
#   I2_fixtures_sot$i2_0_0 + I2_fixtures_sot$i2_1_0 + I2_fixtures_sot$i2_0_1 + I2_fixtures_sot$i2_1_1 + I2_fixtures_sot$i2_2_0 + I2_fixtures_sot$i2_0_2
# )
# #odds
# I2_fixtures_sot$i2_ov25_odds <- round((1/I2_fixtures_sot$i2_ov25),digits = 2)
# I2_fixtures_sot$i2_un25_odds <- round((1/I2_fixtures_sot$i2_un25),digits = 2)
#
# I2_fixtures_sot$i2_ov25_odds
# I2_fixtures_sot$i2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# I2_fixtures_sot$i2_ov25 <- percent(I2_fixtures_sot$i2_ov25, accuracy = 0.1)
#
# I2_fixtures_sot$i2_un25 <- percent(I2_fixtures_sot$i2_un25, accuracy = 0.1)
# I2_fixtures_sot$i2_pssotre <- paste(round(I2_fixtures_sot$i2_xHST,digits = 0),round(I2_fixtures_sot$i2_xAST,digits = 0),sep = "-")
#
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# #################################################################################################################################################################################################################
# #N1
# HomeTeam_n1_sot <- rep(n1_teams, each = length(n1_teams))
# AwayTeam_n1_sot <- rep(n1_teams, length(n1_teams))
# N1_fixtures_sot <- cbind(HomeTeam_n1_sot,AwayTeam_n1_sot)
# N1_fixtures_sot <- as.data.frame(N1_fixtures_sot)
# N1_fixtures_sot <- N1_fixtures_sot[!N1_fixtures_sot$HomeTeam_n1_sot == N1_fixtures_sot$AwayTeam_n1_sot,]
# rownames(N1_fixtures_sot) <- NULL
# N1_fixtures_sot$Div <- "N1"
# N1_fixtures_sot <- N1_fixtures_sot[,c(3,1,2)]
#
# N1_fixtures_sot$avg_HST_n1 <- n1_avg_HST
#
# N1_fixtures_sot$n1_homesotas <- rep(n1_home_sotas,each = length(n1_teams)-1)
#
# n1_awaysods_lookup <- cbind(n1_teams,n1_away_sods)
#
# n1_awaysods_lookup <- as.data.frame(n1_awaysods_lookup)
#
# colnames(n1_awaysods_lookup) <- c("AwayTeam_n1_sot","n1_awaysods")
#
#
# require('RH2')
# N1_fixtures_sot$n1_awaysods <- sqldf("SELECT n1_awaysods_lookup.n1_awaysods FROM n1_awaysods_lookup INNER JOIN N1_fixtures_sot ON n1_awaysods_lookup.AwayTeam_n1_sot = N1_fixtures_sot.AwayTeam_n1_sot")
#
# N1_fixtures_sot$avg_AST_n1 <- n1_avg_AST
#
# n1_awaysotas_lookup <- cbind(n1_teams,n1_away_sotas)
#
# n1_awaysotas_lookup <- as.data.frame(n1_awaysotas_lookup)
#
# colnames(n1_awaysotas_lookup) <- c("AwayTeam_n1_sot","n1_awaysotas")
#
# N1_fixtures_sot$n1_awaysotas <- sqldf("SELECT n1_awaysotas_lookup.n1_awaysotas FROM n1_awaysotas_lookup INNER JOIN N1_fixtures_sot ON n1_awaysotas_lookup.AwayTeam_n1_sot = N1_fixtures_sot.AwayTeam_n1_sot")
#
# N1_fixtures_sot$n1_homesods <- rep(n1_home_sods,each = length(n1_teams)-1)
#
# N1_fixtures_sot$n1_awaysods <- as.numeric(unlist(N1_fixtures_sot$n1_awaysods))
# #xGH
# N1_fixtures_sot$n1_xHST <- N1_fixtures_sot$avg_HST_n1 * N1_fixtures_sot$n1_homesotas * N1_fixtures_sot$n1_awaysods
# #xGA
#
# N1_fixtures_sot$n1_awaysotas <- as.numeric(unlist(N1_fixtures_sot$n1_awaysotas))
#
# N1_fixtures_sot$n1_xAST <- N1_fixtures_sot$avg_AST_n1 * N1_fixtures_sot$n1_awaysotas * N1_fixtures_sot$n1_homesods
#
# N1_fixtures_sot$n1_0_0 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_0 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_0_1 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_1 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_0 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_0_2 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_2 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_1 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_2 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_3 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_0 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_1 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_2 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_0_3 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_3 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_3 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_4 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_0 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_1 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_2 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_3 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_0_4 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_4 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_4 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_4 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_5 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_0 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_1 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_2 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_3 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_4 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_0_5 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_5 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_5 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_5 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_5 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_6 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_0 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(0,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_1 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(1,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_2 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(2,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_3 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(3,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_4 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(4,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_6_5 <- round(stats::dpois(6,N1_fixtures_sot$n1_xHST) * stats::dpois(5,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_0_6 <- round(stats::dpois(0,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_1_6 <- round(stats::dpois(1,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_2_6 <- round(stats::dpois(2,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_3_6 <- round(stats::dpois(3,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_4_6 <- round(stats::dpois(4,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# N1_fixtures_sot$n1_5_6 <- round(stats::dpois(5,N1_fixtures_sot$n1_xHST) * stats::dpois(6,N1_fixtures_sot$n1_xAST), digits = 4)
# #Home win
# N1_fixtures_sot$n1_H <- (
#   N1_fixtures_sot$n1_1_0 + N1_fixtures_sot$n1_2_0 + N1_fixtures_sot$n1_2_1 + N1_fixtures_sot$n1_3_0 + N1_fixtures_sot$n1_3_1 +
#     N1_fixtures_sot$n1_3_2 + N1_fixtures_sot$n1_4_0 + N1_fixtures_sot$n1_4_1 + N1_fixtures_sot$n1_4_2 + N1_fixtures_sot$n1_4_3 +
#     N1_fixtures_sot$n1_5_0 + N1_fixtures_sot$n1_5_1 + N1_fixtures_sot$n1_5_2 + N1_fixtures_sot$n1_5_3 + N1_fixtures_sot$n1_5_4 +
#     N1_fixtures_sot$n1_6_0 + N1_fixtures_sot$n1_6_1 + N1_fixtures_sot$n1_6_2 + N1_fixtures_sot$n1_6_3 + N1_fixtures_sot$n1_6_4 +
#     N1_fixtures_sot$n1_6_5
# )
#
# N1_fixtures_sot$n1_H <- percent(N1_fixtures_sot$n1_H, accuracy = 0.1)
#
# #Draw
# N1_fixtures_sot$n1_D <- (
#
#   N1_fixtures_sot$n1_0_0 + N1_fixtures_sot$n1_1_1 + N1_fixtures_sot$n1_2_2 + N1_fixtures_sot$n1_3_3 + N1_fixtures_sot$n1_4_4 +
#     N1_fixtures_sot$n1_5_5 + N1_fixtures_sot$n1_6_6
# )
#
# N1_fixtures_sot$n1_D <- percent(N1_fixtures_sot$n1_D, accuracy = 0.1)
#
# #Away
#
# N1_fixtures_sot$n1_A <- (
#   N1_fixtures_sot$n1_0_1 + N1_fixtures_sot$n1_0_2 + N1_fixtures_sot$n1_1_2 + N1_fixtures_sot$n1_0_3 + N1_fixtures_sot$n1_1_3 +
#     N1_fixtures_sot$n1_2_3 + N1_fixtures_sot$n1_0_4 + N1_fixtures_sot$n1_1_4 + N1_fixtures_sot$n1_2_4 + N1_fixtures_sot$n1_3_4 +
#     N1_fixtures_sot$n1_0_5 + N1_fixtures_sot$n1_1_5 + N1_fixtures_sot$n1_2_5 + N1_fixtures_sot$n1_3_5 + N1_fixtures_sot$n1_4_5 +
#     N1_fixtures_sot$n1_0_6 + N1_fixtures_sot$n1_1_6 + N1_fixtures_sot$n1_2_6 + N1_fixtures_sot$n1_3_6 + N1_fixtures_sot$n1_4_6 +
#     N1_fixtures_sot$n1_5_6
# )
#
# N1_fixtures_sot$n1_A <- percent(N1_fixtures_sot$n1_A, accuracy = 0.1)
#
# #ov25
# N1_fixtures_sot$n1_ov25 <- (
#   N1_fixtures_sot$n1_2_1 + N1_fixtures_sot$n1_1_2 + N1_fixtures_sot$n1_2_2 + N1_fixtures_sot$n1_3_0 + N1_fixtures_sot$n1_3_1 +
#     N1_fixtures_sot$n1_3_2 + N1_fixtures_sot$n1_0_3 + N1_fixtures_sot$n1_1_3 + N1_fixtures_sot$n1_2_3 + N1_fixtures_sot$n1_3_3 +
#     N1_fixtures_sot$n1_4_0 + N1_fixtures_sot$n1_4_1 + N1_fixtures_sot$n1_4_2 + N1_fixtures_sot$n1_4_3 + N1_fixtures_sot$n1_0_4 +
#     N1_fixtures_sot$n1_1_4 + N1_fixtures_sot$n1_2_4 + N1_fixtures_sot$n1_3_4 + N1_fixtures_sot$n1_4_4 + N1_fixtures_sot$n1_5_0 +
#     N1_fixtures_sot$n1_5_1 + N1_fixtures_sot$n1_5_2 + N1_fixtures_sot$n1_5_3 + N1_fixtures_sot$n1_5_4 + N1_fixtures_sot$n1_0_5 +
#     N1_fixtures_sot$n1_1_5 + N1_fixtures_sot$n1_2_5 + N1_fixtures_sot$n1_3_5 + N1_fixtures_sot$n1_4_5 + N1_fixtures_sot$n1_5_5 +
#     N1_fixtures_sot$n1_6_0 + N1_fixtures_sot$n1_6_1 + N1_fixtures_sot$n1_6_2 + N1_fixtures_sot$n1_6_3 + N1_fixtures_sot$n1_6_4 +
#     N1_fixtures_sot$n1_6_5 + N1_fixtures_sot$n1_0_6 + N1_fixtures_sot$n1_1_6 + N1_fixtures_sot$n1_2_6 + N1_fixtures_sot$n1_3_6 +
#     N1_fixtures_sot$n1_4_6 + N1_fixtures_sot$n1_5_6 + N1_fixtures_sot$n1_6_6
# )
# #un25
# N1_fixtures_sot$n1_un25 <- (
#   N1_fixtures_sot$n1_0_0 + N1_fixtures_sot$n1_1_0 + N1_fixtures_sot$n1_0_1 + N1_fixtures_sot$n1_1_1 + N1_fixtures_sot$n1_2_0 + N1_fixtures_sot$n1_0_2
# )
# #odds
# N1_fixtures_sot$n1_ov25_odds <- round((1/N1_fixtures_sot$n1_ov25),digits = 2)
# N1_fixtures_sot$n1_un25_odds <- round((1/N1_fixtures_sot$n1_un25),digits = 2)
#
# N1_fixtures_sot$n1_ov25_odds
# N1_fixtures_sot$n1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# N1_fixtures_sot$n1_ov25 <- percent(N1_fixtures_sot$n1_ov25, accuracy = 0.1)
#
# N1_fixtures_sot$n1_un25 <- percent(N1_fixtures_sot$n1_un25, accuracy = 0.1)
# N1_fixtures_sot$n1_pssotre <- paste(round(N1_fixtures_sot$n1_xHST,digits = 0),round(N1_fixtures_sot$n1_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(N1_fixtures,'Divisions/N1.xlsx',sheetName = "N1", append = TRUE)
# #################################################################################################################
# #P1
# HomeTeam_p1_sot <- rep(p1_teams, each = length(p1_teams))
# AwayTeam_p1_sot <- rep(p1_teams, length(p1_teams))
# P1_fixtures_sot <- cbind(HomeTeam_p1_sot,AwayTeam_p1_sot)
# P1_fixtures_sot <- as.data.frame(P1_fixtures_sot)
# P1_fixtures_sot <- P1_fixtures_sot[!P1_fixtures_sot$HomeTeam_p1_sot == P1_fixtures_sot$AwayTeam_p1_sot,]
# rownames(P1_fixtures_sot) <- NULL
# P1_fixtures_sot$Div <- "P1"
# P1_fixtures_sot <- P1_fixtures_sot[,c(3,1,2)]
#
# P1_fixtures_sot$avg_HST_p1 <- p1_avg_HST
#
# P1_fixtures_sot$p1_homesotas <- rep(p1_home_sotas,each = length(p1_teams)-1)
#
# p1_awaysods_lookup <- cbind(p1_teams,p1_away_sods)
#
# p1_awaysods_lookup <- as.data.frame(p1_awaysods_lookup)
#
# colnames(p1_awaysods_lookup) <- c("AwayTeam_p1_sot","p1_awaysods")
#
#
# require('RH2')
# P1_fixtures_sot$p1_awaysods <- sqldf("SELECT p1_awaysods_lookup.p1_awaysods FROM p1_awaysods_lookup INNER JOIN P1_fixtures_sot ON p1_awaysods_lookup.AwayTeam_p1_sot = P1_fixtures_sot.AwayTeam_p1_sot")
#
# P1_fixtures_sot$avg_AST_p1 <- p1_avg_AST
#
# p1_awaysotas_lookup <- cbind(p1_teams,p1_away_sotas)
#
# p1_awaysotas_lookup <- as.data.frame(p1_awaysotas_lookup)
#
# colnames(p1_awaysotas_lookup) <- c("AwayTeam_p1_sot","p1_awaysotas")
#
# P1_fixtures_sot$p1_awaysotas <- sqldf("SELECT p1_awaysotas_lookup.p1_awaysotas FROM p1_awaysotas_lookup INNER JOIN P1_fixtures_sot ON p1_awaysotas_lookup.AwayTeam_p1_sot = P1_fixtures_sot.AwayTeam_p1_sot")
#
# P1_fixtures_sot$p1_homesods <- rep(p1_home_sods,each = length(p1_teams)-1)
#
# P1_fixtures_sot$p1_awaysods <- as.numeric(unlist(P1_fixtures_sot$p1_awaysods))
# #xGH
# P1_fixtures_sot$p1_xHST <- P1_fixtures_sot$avg_HST_p1 * P1_fixtures_sot$p1_homesotas * P1_fixtures_sot$p1_awaysods
# #xGA
#
# P1_fixtures_sot$p1_awaysotas <- as.numeric(unlist(P1_fixtures_sot$p1_awaysotas))
#
# P1_fixtures_sot$p1_xAST <- P1_fixtures_sot$avg_AST_p1 * P1_fixtures_sot$p1_awaysotas * P1_fixtures_sot$p1_homesods
#
# P1_fixtures_sot$p1_0_0 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_0 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_0_1 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_1 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_0 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_0_2 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_2 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_1 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_2 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_3 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_0 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_1 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_2 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_0_3 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_3 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_3 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_4 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_0 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_1 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_2 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_3 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_0_4 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_4 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_4 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_4 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_5 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_0 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_1 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_2 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_3 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_4 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_0_5 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_5 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_5 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_5 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_5 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_6 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_0 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(0,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_1 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(1,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_2 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(2,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_3 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(3,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_4 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(4,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_6_5 <- round(stats::dpois(6,P1_fixtures_sot$p1_xHST) * stats::dpois(5,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_0_6 <- round(stats::dpois(0,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_1_6 <- round(stats::dpois(1,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_2_6 <- round(stats::dpois(2,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_3_6 <- round(stats::dpois(3,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_4_6 <- round(stats::dpois(4,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# P1_fixtures_sot$p1_5_6 <- round(stats::dpois(5,P1_fixtures_sot$p1_xHST) * stats::dpois(6,P1_fixtures_sot$p1_xAST), digits = 4)
# #Home win
# P1_fixtures_sot$p1_H <- (
#   P1_fixtures_sot$p1_1_0 + P1_fixtures_sot$p1_2_0 + P1_fixtures_sot$p1_2_1 + P1_fixtures_sot$p1_3_0 + P1_fixtures_sot$p1_3_1 +
#     P1_fixtures_sot$p1_3_2 + P1_fixtures_sot$p1_4_0 + P1_fixtures_sot$p1_4_1 + P1_fixtures_sot$p1_4_2 + P1_fixtures_sot$p1_4_3 +
#     P1_fixtures_sot$p1_5_0 + P1_fixtures_sot$p1_5_1 + P1_fixtures_sot$p1_5_2 + P1_fixtures_sot$p1_5_3 + P1_fixtures_sot$p1_5_4 +
#     P1_fixtures_sot$p1_6_0 + P1_fixtures_sot$p1_6_1 + P1_fixtures_sot$p1_6_2 + P1_fixtures_sot$p1_6_3 + P1_fixtures_sot$p1_6_4 +
#     P1_fixtures_sot$p1_6_5
# )
#
# P1_fixtures_sot$p1_H <- percent(P1_fixtures_sot$p1_H, accuracy = 0.1)
#
# #Draw
# P1_fixtures_sot$p1_D <- (
#
#   P1_fixtures_sot$p1_0_0 + P1_fixtures_sot$p1_1_1 + P1_fixtures_sot$p1_2_2 + P1_fixtures_sot$p1_3_3 + P1_fixtures_sot$p1_4_4 +
#     P1_fixtures_sot$p1_5_5 + P1_fixtures_sot$p1_6_6
# )
#
# P1_fixtures_sot$p1_D <- percent(P1_fixtures_sot$p1_D, accuracy = 0.1)
#
# #Away
#
# P1_fixtures_sot$p1_A <- (
#   P1_fixtures_sot$p1_0_1 + P1_fixtures_sot$p1_0_2 + P1_fixtures_sot$p1_1_2 + P1_fixtures_sot$p1_0_3 + P1_fixtures_sot$p1_1_3 +
#     P1_fixtures_sot$p1_2_3 + P1_fixtures_sot$p1_0_4 + P1_fixtures_sot$p1_1_4 + P1_fixtures_sot$p1_2_4 + P1_fixtures_sot$p1_3_4 +
#     P1_fixtures_sot$p1_0_5 + P1_fixtures_sot$p1_1_5 + P1_fixtures_sot$p1_2_5 + P1_fixtures_sot$p1_3_5 + P1_fixtures_sot$p1_4_5 +
#     P1_fixtures_sot$p1_0_6 + P1_fixtures_sot$p1_1_6 + P1_fixtures_sot$p1_2_6 + P1_fixtures_sot$p1_3_6 + P1_fixtures_sot$p1_4_6 +
#     P1_fixtures_sot$p1_5_6
# )
#
# P1_fixtures_sot$p1_A <- percent(P1_fixtures_sot$p1_A, accuracy = 0.1)
#
# #ov25
# P1_fixtures_sot$p1_ov25 <- (
#   P1_fixtures_sot$p1_2_1 + P1_fixtures_sot$p1_1_2 + P1_fixtures_sot$p1_2_2 + P1_fixtures_sot$p1_3_0 + P1_fixtures_sot$p1_3_1 +
#     P1_fixtures_sot$p1_3_2 + P1_fixtures_sot$p1_0_3 + P1_fixtures_sot$p1_1_3 + P1_fixtures_sot$p1_2_3 + P1_fixtures_sot$p1_3_3 +
#     P1_fixtures_sot$p1_4_0 + P1_fixtures_sot$p1_4_1 + P1_fixtures_sot$p1_4_2 + P1_fixtures_sot$p1_4_3 + P1_fixtures_sot$p1_0_4 +
#     P1_fixtures_sot$p1_1_4 + P1_fixtures_sot$p1_2_4 + P1_fixtures_sot$p1_3_4 + P1_fixtures_sot$p1_4_4 + P1_fixtures_sot$p1_5_0 +
#     P1_fixtures_sot$p1_5_1 + P1_fixtures_sot$p1_5_2 + P1_fixtures_sot$p1_5_3 + P1_fixtures_sot$p1_5_4 + P1_fixtures_sot$p1_0_5 +
#     P1_fixtures_sot$p1_1_5 + P1_fixtures_sot$p1_2_5 + P1_fixtures_sot$p1_3_5 + P1_fixtures_sot$p1_4_5 + P1_fixtures_sot$p1_5_5 +
#     P1_fixtures_sot$p1_6_0 + P1_fixtures_sot$p1_6_1 + P1_fixtures_sot$p1_6_2 + P1_fixtures_sot$p1_6_3 + P1_fixtures_sot$p1_6_4 +
#     P1_fixtures_sot$p1_6_5 + P1_fixtures_sot$p1_0_6 + P1_fixtures_sot$p1_1_6 + P1_fixtures_sot$p1_2_6 + P1_fixtures_sot$p1_3_6 +
#     P1_fixtures_sot$p1_4_6 + P1_fixtures_sot$p1_5_6 + P1_fixtures_sot$p1_6_6
# )
# #un25
# P1_fixtures_sot$p1_un25 <- (
#   P1_fixtures_sot$p1_0_0 + P1_fixtures_sot$p1_1_0 + P1_fixtures_sot$p1_0_1 + P1_fixtures_sot$p1_1_1 + P1_fixtures_sot$p1_2_0 + P1_fixtures_sot$p1_0_2
# )
# #odds
# P1_fixtures_sot$p1_ov25_odds <- round((1/P1_fixtures_sot$p1_ov25),digits = 2)
# P1_fixtures_sot$p1_un25_odds <- round((1/P1_fixtures_sot$p1_un25),digits = 2)
#
# P1_fixtures_sot$p1_ov25_odds
# P1_fixtures_sot$p1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# P1_fixtures_sot$p1_ov25 <- percent(P1_fixtures_sot$p1_ov25, accuracy = 0.1)
#
# P1_fixtures_sot$p1_un25 <- percent(P1_fixtures_sot$p1_un25, accuracy = 0.1)
# P1_fixtures_sot$p1_pssotre <- paste(round(P1_fixtures_sot$p1_xHST,digits = 0),round(P1_fixtures_sot$p1_xAST,digits = 0),sep = "-")
# #write.xlsx(P1_fixtures,'Divisions/P1.xlsx',sheetName = "P1", append = TRUE)
# #################################################################################################################
# #SC0
# HomeTeam_sc0_sot <- rep(sc0_teams, each = length(sc0_teams))
# AwayTeam_sc0_sot <- rep(sc0_teams, length(sc0_teams))
# SC0_fixtures_sot <- cbind(HomeTeam_sc0_sot,AwayTeam_sc0_sot)
# SC0_fixtures_sot <- as.data.frame(SC0_fixtures_sot)
# SC0_fixtures_sot <- SC0_fixtures_sot[!SC0_fixtures_sot$HomeTeam_sc0_sot == SC0_fixtures_sot$AwayTeam_sc0_sot,]
# rownames(SC0_fixtures_sot) <- NULL
# SC0_fixtures_sot$Div <- "SC0"
# SC0_fixtures_sot <- SC0_fixtures_sot[,c(3,1,2)]
#
# SC0_fixtures_sot$avg_HST_sc0 <- sc0_avg_HST
#
# SC0_fixtures_sot$sc0_homesotas <- rep(sc0_home_sotas,each = length(sc0_teams)-1)
#
# sc0_awaysods_lookup <- cbind(sc0_teams,sc0_away_sods)
#
# sc0_awaysods_lookup <- as.data.frame(sc0_awaysods_lookup)
#
# colnames(sc0_awaysods_lookup) <- c("AwayTeam_sc0_sot","sc0_awaysods")
#
#
# require('RH2')
# SC0_fixtures_sot$sc0_awaysods <- sqldf("SELECT sc0_awaysods_lookup.sc0_awaysods FROM sc0_awaysods_lookup INNER JOIN SC0_fixtures_sot ON sc0_awaysods_lookup.AwayTeam_sc0_sot = SC0_fixtures_sot.AwayTeam_sc0_sot")
#
# SC0_fixtures_sot$avg_AST_sc0 <- sc0_avg_AST
#
# sc0_awaysotas_lookup <- cbind(sc0_teams,sc0_away_sotas)
#
# sc0_awaysotas_lookup <- as.data.frame(sc0_awaysotas_lookup)
#
# colnames(sc0_awaysotas_lookup) <- c("AwayTeam_sc0_sot","sc0_awaysotas")
#
# SC0_fixtures_sot$sc0_awaysotas <- sqldf("SELECT sc0_awaysotas_lookup.sc0_awaysotas FROM sc0_awaysotas_lookup INNER JOIN SC0_fixtures_sot ON sc0_awaysotas_lookup.AwayTeam_sc0_sot = SC0_fixtures_sot.AwayTeam_sc0_sot")
#
# SC0_fixtures_sot$sc0_homesods <- rep(sc0_home_sods,each = length(sc0_teams)-1)
#
# SC0_fixtures_sot$sc0_awaysods <- as.numeric(unlist(SC0_fixtures_sot$sc0_awaysods))
# #xGH
# SC0_fixtures_sot$sc0_xHST <- SC0_fixtures_sot$avg_HST_sc0 * SC0_fixtures_sot$sc0_homesotas * SC0_fixtures_sot$sc0_awaysods
# #xGA
#
# SC0_fixtures_sot$sc0_awaysotas <- as.numeric(unlist(SC0_fixtures_sot$sc0_awaysotas))
#
# SC0_fixtures_sot$sc0_xAST <- SC0_fixtures_sot$avg_AST_sc0 * SC0_fixtures_sot$sc0_awaysotas * SC0_fixtures_sot$sc0_homesods
#
# SC0_fixtures_sot$sc0_0_0 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_0 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_0_1 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_1 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_0 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_0_2 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_2 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_1 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_2 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_3 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_0 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_1 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_2 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_0_3 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_3 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_3 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_4 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_0 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_1 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_2 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_3 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_0_4 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_4 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_4 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_4 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_5 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_0 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_1 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_2 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_3 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_4 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_0_5 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_5 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_5 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_5 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_5 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_6 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_0 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(0,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_1 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(1,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_2 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(2,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_3 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(3,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_4 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(4,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_6_5 <- round(stats::dpois(6,SC0_fixtures_sot$sc0_xHST) * stats::dpois(5,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_0_6 <- round(stats::dpois(0,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_1_6 <- round(stats::dpois(1,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_2_6 <- round(stats::dpois(2,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_3_6 <- round(stats::dpois(3,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_4_6 <- round(stats::dpois(4,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# SC0_fixtures_sot$sc0_5_6 <- round(stats::dpois(5,SC0_fixtures_sot$sc0_xHST) * stats::dpois(6,SC0_fixtures_sot$sc0_xAST), digits = 4)
# #Home win
# SC0_fixtures_sot$sc0_H <- (
#   SC0_fixtures_sot$sc0_1_0 + SC0_fixtures_sot$sc0_2_0 + SC0_fixtures_sot$sc0_2_1 + SC0_fixtures_sot$sc0_3_0 + SC0_fixtures_sot$sc0_3_1 +
#     SC0_fixtures_sot$sc0_3_2 + SC0_fixtures_sot$sc0_4_0 + SC0_fixtures_sot$sc0_4_1 + SC0_fixtures_sot$sc0_4_2 + SC0_fixtures_sot$sc0_4_3 +
#     SC0_fixtures_sot$sc0_5_0 + SC0_fixtures_sot$sc0_5_1 + SC0_fixtures_sot$sc0_5_2 + SC0_fixtures_sot$sc0_5_3 + SC0_fixtures_sot$sc0_5_4 +
#     SC0_fixtures_sot$sc0_6_0 + SC0_fixtures_sot$sc0_6_1 + SC0_fixtures_sot$sc0_6_2 + SC0_fixtures_sot$sc0_6_3 + SC0_fixtures_sot$sc0_6_4 +
#     SC0_fixtures_sot$sc0_6_5
# )
#
# SC0_fixtures_sot$sc0_H <- percent(SC0_fixtures_sot$sc0_H, accuracy = 0.1)
#
# #Draw
# SC0_fixtures_sot$sc0_D <- (
#
#   SC0_fixtures_sot$sc0_0_0 + SC0_fixtures_sot$sc0_1_1 + SC0_fixtures_sot$sc0_2_2 + SC0_fixtures_sot$sc0_3_3 + SC0_fixtures_sot$sc0_4_4 +
#     SC0_fixtures_sot$sc0_5_5 + SC0_fixtures_sot$sc0_6_6
# )
#
# SC0_fixtures_sot$sc0_D <- percent(SC0_fixtures_sot$sc0_D, accuracy = 0.1)
#
# #Away
#
# SC0_fixtures_sot$sc0_A <- (
#   SC0_fixtures_sot$sc0_0_1 + SC0_fixtures_sot$sc0_0_2 + SC0_fixtures_sot$sc0_1_2 + SC0_fixtures_sot$sc0_0_3 + SC0_fixtures_sot$sc0_1_3 +
#     SC0_fixtures_sot$sc0_2_3 + SC0_fixtures_sot$sc0_0_4 + SC0_fixtures_sot$sc0_1_4 + SC0_fixtures_sot$sc0_2_4 + SC0_fixtures_sot$sc0_3_4 +
#     SC0_fixtures_sot$sc0_0_5 + SC0_fixtures_sot$sc0_1_5 + SC0_fixtures_sot$sc0_2_5 + SC0_fixtures_sot$sc0_3_5 + SC0_fixtures_sot$sc0_4_5 +
#     SC0_fixtures_sot$sc0_0_6 + SC0_fixtures_sot$sc0_1_6 + SC0_fixtures_sot$sc0_2_6 + SC0_fixtures_sot$sc0_3_6 + SC0_fixtures_sot$sc0_4_6 +
#     SC0_fixtures_sot$sc0_5_6
# )
#
# SC0_fixtures_sot$sc0_A <- percent(SC0_fixtures_sot$sc0_A, accuracy = 0.1)
#
# #ov25
# SC0_fixtures_sot$sc0_ov25 <- (
#   SC0_fixtures_sot$sc0_2_1 + SC0_fixtures_sot$sc0_1_2 + SC0_fixtures_sot$sc0_2_2 + SC0_fixtures_sot$sc0_3_0 + SC0_fixtures_sot$sc0_3_1 +
#     SC0_fixtures_sot$sc0_3_2 + SC0_fixtures_sot$sc0_0_3 + SC0_fixtures_sot$sc0_1_3 + SC0_fixtures_sot$sc0_2_3 + SC0_fixtures_sot$sc0_3_3 +
#     SC0_fixtures_sot$sc0_4_0 + SC0_fixtures_sot$sc0_4_1 + SC0_fixtures_sot$sc0_4_2 + SC0_fixtures_sot$sc0_4_3 + SC0_fixtures_sot$sc0_0_4 +
#     SC0_fixtures_sot$sc0_1_4 + SC0_fixtures_sot$sc0_2_4 + SC0_fixtures_sot$sc0_3_4 + SC0_fixtures_sot$sc0_4_4 + SC0_fixtures_sot$sc0_5_0 +
#     SC0_fixtures_sot$sc0_5_1 + SC0_fixtures_sot$sc0_5_2 + SC0_fixtures_sot$sc0_5_3 + SC0_fixtures_sot$sc0_5_4 + SC0_fixtures_sot$sc0_0_5 +
#     SC0_fixtures_sot$sc0_1_5 + SC0_fixtures_sot$sc0_2_5 + SC0_fixtures_sot$sc0_3_5 + SC0_fixtures_sot$sc0_4_5 + SC0_fixtures_sot$sc0_5_5 +
#     SC0_fixtures_sot$sc0_6_0 + SC0_fixtures_sot$sc0_6_1 + SC0_fixtures_sot$sc0_6_2 + SC0_fixtures_sot$sc0_6_3 + SC0_fixtures_sot$sc0_6_4 +
#     SC0_fixtures_sot$sc0_6_5 + SC0_fixtures_sot$sc0_0_6 + SC0_fixtures_sot$sc0_1_6 + SC0_fixtures_sot$sc0_2_6 + SC0_fixtures_sot$sc0_3_6 +
#     SC0_fixtures_sot$sc0_4_6 + SC0_fixtures_sot$sc0_5_6 + SC0_fixtures_sot$sc0_6_6
# )
# #un25
# SC0_fixtures_sot$sc0_un25 <- (
#   SC0_fixtures_sot$sc0_0_0 + SC0_fixtures_sot$sc0_1_0 + SC0_fixtures_sot$sc0_0_1 + SC0_fixtures_sot$sc0_1_1 + SC0_fixtures_sot$sc0_2_0 + SC0_fixtures_sot$sc0_0_2
# )
# #odds
# SC0_fixtures_sot$sc0_ov25_odds <- round((1/SC0_fixtures_sot$sc0_ov25),digits = 2)
# SC0_fixtures_sot$sc0_un25_odds <- round((1/SC0_fixtures_sot$sc0_un25),digits = 2)
#
# SC0_fixtures_sot$sc0_ov25_odds
# SC0_fixtures_sot$sc0_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC0_fixtures_sot$sc0_ov25 <- percent(SC0_fixtures_sot$sc0_ov25, accuracy = 0.1)
#
# SC0_fixtures_sot$sc0_un25 <- percent(SC0_fixtures_sot$sc0_un25, accuracy = 0.1)
# SC0_fixtures_sot$sc0_pssotre <- paste(round(SC0_fixtures_sot$sc0_xHST,digits = 0),round(SC0_fixtures_sot$sc0_xAST,digits = 0),sep = "-")
# #write.xlsx(SC0_fixtures,'Divisions/SC0.xlsx',sheetName = "SC0", append = TRUE)
# #################################################################################################################
# #SC1
# HomeTeam_sc1_sot <- rep(sc1_teams, each = length(sc1_teams))
# AwayTeam_sc1_sot <- rep(sc1_teams, length(sc1_teams))
# SC1_fixtures_sot <- cbind(HomeTeam_sc1_sot,AwayTeam_sc1_sot)
# SC1_fixtures_sot <- as.data.frame(SC1_fixtures_sot)
# SC1_fixtures_sot <- SC1_fixtures_sot[!SC1_fixtures_sot$HomeTeam_sc1_sot == SC1_fixtures_sot$AwayTeam_sc1_sot,]
# rownames(SC1_fixtures_sot) <- NULL
# SC1_fixtures_sot$Div <- "SC1"
# SC1_fixtures_sot <- SC1_fixtures_sot[,c(3,1,2)]
#
# SC1_fixtures_sot$avg_HST_sc1 <- sc1_avg_HST
#
# SC1_fixtures_sot$sc1_homesotas <- rep(sc1_home_sotas,each = length(sc1_teams)-1)
#
# sc1_awaysods_lookup <- cbind(sc1_teams,sc1_away_sods)
#
# sc1_awaysods_lookup <- as.data.frame(sc1_awaysods_lookup)
#
# colnames(sc1_awaysods_lookup) <- c("AwayTeam_sc1_sot","sc1_awaysods")
#
#
# require('RH2')
# SC1_fixtures_sot$sc1_awaysods <- sqldf("SELECT sc1_awaysods_lookup.sc1_awaysods FROM sc1_awaysods_lookup INNER JOIN SC1_fixtures_sot ON sc1_awaysods_lookup.AwayTeam_sc1_sot = SC1_fixtures_sot.AwayTeam_sc1_sot")
#
# SC1_fixtures_sot$avg_AST_sc1 <- sc1_avg_AST
#
# sc1_awaysotas_lookup <- cbind(sc1_teams,sc1_away_sotas)
#
# sc1_awaysotas_lookup <- as.data.frame(sc1_awaysotas_lookup)
#
# colnames(sc1_awaysotas_lookup) <- c("AwayTeam_sc1_sot","sc1_awaysotas")
#
# SC1_fixtures_sot$sc1_awaysotas <- sqldf("SELECT sc1_awaysotas_lookup.sc1_awaysotas FROM sc1_awaysotas_lookup INNER JOIN SC1_fixtures_sot ON sc1_awaysotas_lookup.AwayTeam_sc1_sot = SC1_fixtures_sot.AwayTeam_sc1_sot")
#
# SC1_fixtures_sot$sc1_homesods <- rep(sc1_home_sods,each = length(sc1_teams)-1)
#
# SC1_fixtures_sot$sc1_awaysods <- as.numeric(unlist(SC1_fixtures_sot$sc1_awaysods))
# #xGH
# SC1_fixtures_sot$sc1_xHST <- SC1_fixtures_sot$avg_HST_sc1 * SC1_fixtures_sot$sc1_homesotas * SC1_fixtures_sot$sc1_awaysods
# #xGA
#
# SC1_fixtures_sot$sc1_awaysotas <- as.numeric(unlist(SC1_fixtures_sot$sc1_awaysotas))
#
# SC1_fixtures_sot$sc1_xAST <- SC1_fixtures_sot$avg_AST_sc1 * SC1_fixtures_sot$sc1_awaysotas * SC1_fixtures_sot$sc1_homesods
#
# SC1_fixtures_sot$sc1_0_0 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_0 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_0_1 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_1 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_0 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_0_2 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_2 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_1 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_2 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_3 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_0 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_1 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_2 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_0_3 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_3 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_3 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_4 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_0 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_1 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_2 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_3 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_0_4 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_4 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_4 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_4 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_5 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_0 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_1 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_2 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_3 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_4 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_0_5 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_5 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_5 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_5 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_5 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_6 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_0 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(0,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_1 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(1,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_2 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(2,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_3 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(3,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_4 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(4,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_6_5 <- round(stats::dpois(6,SC1_fixtures_sot$sc1_xHST) * stats::dpois(5,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_0_6 <- round(stats::dpois(0,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_1_6 <- round(stats::dpois(1,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_2_6 <- round(stats::dpois(2,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_3_6 <- round(stats::dpois(3,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_4_6 <- round(stats::dpois(4,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# SC1_fixtures_sot$sc1_5_6 <- round(stats::dpois(5,SC1_fixtures_sot$sc1_xHST) * stats::dpois(6,SC1_fixtures_sot$sc1_xAST), digits = 4)
# #Home win
# SC1_fixtures_sot$sc1_H <- (
#   SC1_fixtures_sot$sc1_1_0 + SC1_fixtures_sot$sc1_2_0 + SC1_fixtures_sot$sc1_2_1 + SC1_fixtures_sot$sc1_3_0 + SC1_fixtures_sot$sc1_3_1 +
#     SC1_fixtures_sot$sc1_3_2 + SC1_fixtures_sot$sc1_4_0 + SC1_fixtures_sot$sc1_4_1 + SC1_fixtures_sot$sc1_4_2 + SC1_fixtures_sot$sc1_4_3 +
#     SC1_fixtures_sot$sc1_5_0 + SC1_fixtures_sot$sc1_5_1 + SC1_fixtures_sot$sc1_5_2 + SC1_fixtures_sot$sc1_5_3 + SC1_fixtures_sot$sc1_5_4 +
#     SC1_fixtures_sot$sc1_6_0 + SC1_fixtures_sot$sc1_6_1 + SC1_fixtures_sot$sc1_6_2 + SC1_fixtures_sot$sc1_6_3 + SC1_fixtures_sot$sc1_6_4 +
#     SC1_fixtures_sot$sc1_6_5
# )
#
# SC1_fixtures_sot$sc1_H <- percent(SC1_fixtures_sot$sc1_H, accuracy = 0.1)
#
# #Draw
# SC1_fixtures_sot$sc1_D <- (
#
#   SC1_fixtures_sot$sc1_0_0 + SC1_fixtures_sot$sc1_1_1 + SC1_fixtures_sot$sc1_2_2 + SC1_fixtures_sot$sc1_3_3 + SC1_fixtures_sot$sc1_4_4 +
#     SC1_fixtures_sot$sc1_5_5 + SC1_fixtures_sot$sc1_6_6
# )
#
# SC1_fixtures_sot$sc1_D <- percent(SC1_fixtures_sot$sc1_D, accuracy = 0.1)
#
# #Away
#
# SC1_fixtures_sot$sc1_A <- (
#   SC1_fixtures_sot$sc1_0_1 + SC1_fixtures_sot$sc1_0_2 + SC1_fixtures_sot$sc1_1_2 + SC1_fixtures_sot$sc1_0_3 + SC1_fixtures_sot$sc1_1_3 +
#     SC1_fixtures_sot$sc1_2_3 + SC1_fixtures_sot$sc1_0_4 + SC1_fixtures_sot$sc1_1_4 + SC1_fixtures_sot$sc1_2_4 + SC1_fixtures_sot$sc1_3_4 +
#     SC1_fixtures_sot$sc1_0_5 + SC1_fixtures_sot$sc1_1_5 + SC1_fixtures_sot$sc1_2_5 + SC1_fixtures_sot$sc1_3_5 + SC1_fixtures_sot$sc1_4_5 +
#     SC1_fixtures_sot$sc1_0_6 + SC1_fixtures_sot$sc1_1_6 + SC1_fixtures_sot$sc1_2_6 + SC1_fixtures_sot$sc1_3_6 + SC1_fixtures_sot$sc1_4_6 +
#     SC1_fixtures_sot$sc1_5_6
# )
#
# SC1_fixtures_sot$sc1_A <- percent(SC1_fixtures_sot$sc1_A, accuracy = 0.1)
#
# #ov25
# SC1_fixtures_sot$sc1_ov25 <- (
#   SC1_fixtures_sot$sc1_2_1 + SC1_fixtures_sot$sc1_1_2 + SC1_fixtures_sot$sc1_2_2 + SC1_fixtures_sot$sc1_3_0 + SC1_fixtures_sot$sc1_3_1 +
#     SC1_fixtures_sot$sc1_3_2 + SC1_fixtures_sot$sc1_0_3 + SC1_fixtures_sot$sc1_1_3 + SC1_fixtures_sot$sc1_2_3 + SC1_fixtures_sot$sc1_3_3 +
#     SC1_fixtures_sot$sc1_4_0 + SC1_fixtures_sot$sc1_4_1 + SC1_fixtures_sot$sc1_4_2 + SC1_fixtures_sot$sc1_4_3 + SC1_fixtures_sot$sc1_0_4 +
#     SC1_fixtures_sot$sc1_1_4 + SC1_fixtures_sot$sc1_2_4 + SC1_fixtures_sot$sc1_3_4 + SC1_fixtures_sot$sc1_4_4 + SC1_fixtures_sot$sc1_5_0 +
#     SC1_fixtures_sot$sc1_5_1 + SC1_fixtures_sot$sc1_5_2 + SC1_fixtures_sot$sc1_5_3 + SC1_fixtures_sot$sc1_5_4 + SC1_fixtures_sot$sc1_0_5 +
#     SC1_fixtures_sot$sc1_1_5 + SC1_fixtures_sot$sc1_2_5 + SC1_fixtures_sot$sc1_3_5 + SC1_fixtures_sot$sc1_4_5 + SC1_fixtures_sot$sc1_5_5 +
#     SC1_fixtures_sot$sc1_6_0 + SC1_fixtures_sot$sc1_6_1 + SC1_fixtures_sot$sc1_6_2 + SC1_fixtures_sot$sc1_6_3 + SC1_fixtures_sot$sc1_6_4 +
#     SC1_fixtures_sot$sc1_6_5 + SC1_fixtures_sot$sc1_0_6 + SC1_fixtures_sot$sc1_1_6 + SC1_fixtures_sot$sc1_2_6 + SC1_fixtures_sot$sc1_3_6 +
#     SC1_fixtures_sot$sc1_4_6 + SC1_fixtures_sot$sc1_5_6 + SC1_fixtures_sot$sc1_6_6
# )
# #un25
# SC1_fixtures_sot$sc1_un25 <- (
#   SC1_fixtures_sot$sc1_0_0 + SC1_fixtures_sot$sc1_1_0 + SC1_fixtures_sot$sc1_0_1 + SC1_fixtures_sot$sc1_1_1 + SC1_fixtures_sot$sc1_2_0 + SC1_fixtures_sot$sc1_0_2
# )
# #odds
# SC1_fixtures_sot$sc1_ov25_odds <- round((1/SC1_fixtures_sot$sc1_ov25),digits = 2)
# SC1_fixtures_sot$sc1_un25_odds <- round((1/SC1_fixtures_sot$sc1_un25),digits = 2)
#
# SC1_fixtures_sot$sc1_ov25_odds
# SC1_fixtures_sot$sc1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC1_fixtures_sot$sc1_ov25 <- percent(SC1_fixtures_sot$sc1_ov25, accuracy = 0.1)
#
# SC1_fixtures_sot$sc1_un25 <- percent(SC1_fixtures_sot$sc1_un25, accuracy = 0.1)
# SC1_fixtures_sot$sc1_pssotre <- paste(round(SC1_fixtures_sot$sc1_xHST,digits = 0),round(SC1_fixtures_sot$sc1_xAST,digits = 0),sep = "-")
# #write.xlsx(MLS_fixtures,'Divisions/MLS.xlsx',sheetName = "MLS", append = TRUE)
# ##########################################################################################################################################
# #SC2
# HomeTeam_sc2_sot <- rep(sc2_teams, each = length(sc2_teams))
# AwayTeam_sc2_sot <- rep(sc2_teams, length(sc2_teams))
# SC2_fixtures_sot <- cbind(HomeTeam_sc2_sot,AwayTeam_sc2_sot)
# SC2_fixtures_sot <- as.data.frame(SC2_fixtures_sot)
# SC2_fixtures_sot <- SC2_fixtures_sot[!SC2_fixtures_sot$HomeTeam_sc2_sot == SC2_fixtures_sot$AwayTeam_sc2_sot,]
# rownames(SC2_fixtures_sot) <- NULL
# SC2_fixtures_sot$Div <- "SC2"
# SC2_fixtures_sot <- SC2_fixtures_sot[,c(3,1,2)]
#
# SC2_fixtures_sot$avg_HST_sc2 <- sc2_avg_HST
#
# SC2_fixtures_sot$sc2_homesotas <- rep(sc2_home_sotas,each = length(sc2_teams)-1)
#
# sc2_awaysods_lookup <- cbind(sc2_teams,sc2_away_sods)
#
# sc2_awaysods_lookup <- as.data.frame(sc2_awaysods_lookup)
#
# colnames(sc2_awaysods_lookup) <- c("AwayTeam_sc2_sot","sc2_awaysods")
#
#
# require('RH2')
# SC2_fixtures_sot$sc2_awaysods <- sqldf("SELECT sc2_awaysods_lookup.sc2_awaysods FROM sc2_awaysods_lookup INNER JOIN SC2_fixtures_sot ON sc2_awaysods_lookup.AwayTeam_sc2_sot = SC2_fixtures_sot.AwayTeam_sc2_sot")
#
# SC2_fixtures_sot$avg_AST_sc2 <- sc2_avg_AST
#
# sc2_awaysotas_lookup <- cbind(sc2_teams,sc2_away_sotas)
#
# sc2_awaysotas_lookup <- as.data.frame(sc2_awaysotas_lookup)
#
# colnames(sc2_awaysotas_lookup) <- c("AwayTeam_sc2_sot","sc2_awaysotas")
#
# SC2_fixtures_sot$sc2_awaysotas <- sqldf("SELECT sc2_awaysotas_lookup.sc2_awaysotas FROM sc2_awaysotas_lookup INNER JOIN SC2_fixtures_sot ON sc2_awaysotas_lookup.AwayTeam_sc2_sot = SC2_fixtures_sot.AwayTeam_sc2_sot")
#
# SC2_fixtures_sot$sc2_homesods <- rep(sc2_home_sods,each = length(sc2_teams)-1)
#
# SC2_fixtures_sot$sc2_awaysods <- as.numeric(unlist(SC2_fixtures_sot$sc2_awaysods))
# #xGH
# SC2_fixtures_sot$sc2_xHST <- SC2_fixtures_sot$avg_HST_sc2 * SC2_fixtures_sot$sc2_homesotas * SC2_fixtures_sot$sc2_awaysods
# #xGA
#
# SC2_fixtures_sot$sc2_awaysotas <- as.numeric(unlist(SC2_fixtures_sot$sc2_awaysotas))
#
# SC2_fixtures_sot$sc2_xAST <- SC2_fixtures_sot$avg_AST_sc2 * SC2_fixtures_sot$sc2_awaysotas * SC2_fixtures_sot$sc2_homesods
#
# SC2_fixtures_sot$sc2_0_0 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_0 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_0_1 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_1 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_0 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_0_2 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_2 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_1 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_2 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_3 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_0 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_1 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_2 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_0_3 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_3 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_3 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_4 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_0 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_1 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_2 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_3 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_0_4 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_4 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_4 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_4 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_5 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_0 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_1 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_2 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_3 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_4 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_0_5 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_5 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_5 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_5 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_5 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_6 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_0 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(0,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_1 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(1,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_2 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(2,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_3 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(3,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_4 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(4,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_6_5 <- round(stats::dpois(6,SC2_fixtures_sot$sc2_xHST) * stats::dpois(5,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_0_6 <- round(stats::dpois(0,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_1_6 <- round(stats::dpois(1,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_2_6 <- round(stats::dpois(2,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_3_6 <- round(stats::dpois(3,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_4_6 <- round(stats::dpois(4,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# SC2_fixtures_sot$sc2_5_6 <- round(stats::dpois(5,SC2_fixtures_sot$sc2_xHST) * stats::dpois(6,SC2_fixtures_sot$sc2_xAST), digits = 4)
# #Home win
# SC2_fixtures_sot$sc2_H <- (
#   SC2_fixtures_sot$sc2_1_0 + SC2_fixtures_sot$sc2_2_0 + SC2_fixtures_sot$sc2_2_1 + SC2_fixtures_sot$sc2_3_0 + SC2_fixtures_sot$sc2_3_1 +
#     SC2_fixtures_sot$sc2_3_2 + SC2_fixtures_sot$sc2_4_0 + SC2_fixtures_sot$sc2_4_1 + SC2_fixtures_sot$sc2_4_2 + SC2_fixtures_sot$sc2_4_3 +
#     SC2_fixtures_sot$sc2_5_0 + SC2_fixtures_sot$sc2_5_1 + SC2_fixtures_sot$sc2_5_2 + SC2_fixtures_sot$sc2_5_3 + SC2_fixtures_sot$sc2_5_4 +
#     SC2_fixtures_sot$sc2_6_0 + SC2_fixtures_sot$sc2_6_1 + SC2_fixtures_sot$sc2_6_2 + SC2_fixtures_sot$sc2_6_3 + SC2_fixtures_sot$sc2_6_4 +
#     SC2_fixtures_sot$sc2_6_5
# )
#
# SC2_fixtures_sot$sc2_H <- percent(SC2_fixtures_sot$sc2_H, accuracy = 0.1)
#
# #Draw
# SC2_fixtures_sot$sc2_D <- (
#
#   SC2_fixtures_sot$sc2_0_0 + SC2_fixtures_sot$sc2_1_1 + SC2_fixtures_sot$sc2_2_2 + SC2_fixtures_sot$sc2_3_3 + SC2_fixtures_sot$sc2_4_4 +
#     SC2_fixtures_sot$sc2_5_5 + SC2_fixtures_sot$sc2_6_6
# )
#
# SC2_fixtures_sot$sc2_D <- percent(SC2_fixtures_sot$sc2_D, accuracy = 0.1)
#
# #Away
#
# SC2_fixtures_sot$sc2_A <- (
#   SC2_fixtures_sot$sc2_0_1 + SC2_fixtures_sot$sc2_0_2 + SC2_fixtures_sot$sc2_1_2 + SC2_fixtures_sot$sc2_0_3 + SC2_fixtures_sot$sc2_1_3 +
#     SC2_fixtures_sot$sc2_2_3 + SC2_fixtures_sot$sc2_0_4 + SC2_fixtures_sot$sc2_1_4 + SC2_fixtures_sot$sc2_2_4 + SC2_fixtures_sot$sc2_3_4 +
#     SC2_fixtures_sot$sc2_0_5 + SC2_fixtures_sot$sc2_1_5 + SC2_fixtures_sot$sc2_2_5 + SC2_fixtures_sot$sc2_3_5 + SC2_fixtures_sot$sc2_4_5 +
#     SC2_fixtures_sot$sc2_0_6 + SC2_fixtures_sot$sc2_1_6 + SC2_fixtures_sot$sc2_2_6 + SC2_fixtures_sot$sc2_3_6 + SC2_fixtures_sot$sc2_4_6 +
#     SC2_fixtures_sot$sc2_5_6
# )
#
# SC2_fixtures_sot$sc2_A <- percent(SC2_fixtures_sot$sc2_A, accuracy = 0.1)
#
# #ov25
# SC2_fixtures_sot$sc2_ov25 <- (
#   SC2_fixtures_sot$sc2_2_1 + SC2_fixtures_sot$sc2_1_2 + SC2_fixtures_sot$sc2_2_2 + SC2_fixtures_sot$sc2_3_0 + SC2_fixtures_sot$sc2_3_1 +
#     SC2_fixtures_sot$sc2_3_2 + SC2_fixtures_sot$sc2_0_3 + SC2_fixtures_sot$sc2_1_3 + SC2_fixtures_sot$sc2_2_3 + SC2_fixtures_sot$sc2_3_3 +
#     SC2_fixtures_sot$sc2_4_0 + SC2_fixtures_sot$sc2_4_1 + SC2_fixtures_sot$sc2_4_2 + SC2_fixtures_sot$sc2_4_3 + SC2_fixtures_sot$sc2_0_4 +
#     SC2_fixtures_sot$sc2_1_4 + SC2_fixtures_sot$sc2_2_4 + SC2_fixtures_sot$sc2_3_4 + SC2_fixtures_sot$sc2_4_4 + SC2_fixtures_sot$sc2_5_0 +
#     SC2_fixtures_sot$sc2_5_1 + SC2_fixtures_sot$sc2_5_2 + SC2_fixtures_sot$sc2_5_3 + SC2_fixtures_sot$sc2_5_4 + SC2_fixtures_sot$sc2_0_5 +
#     SC2_fixtures_sot$sc2_1_5 + SC2_fixtures_sot$sc2_2_5 + SC2_fixtures_sot$sc2_3_5 + SC2_fixtures_sot$sc2_4_5 + SC2_fixtures_sot$sc2_5_5 +
#     SC2_fixtures_sot$sc2_6_0 + SC2_fixtures_sot$sc2_6_1 + SC2_fixtures_sot$sc2_6_2 + SC2_fixtures_sot$sc2_6_3 + SC2_fixtures_sot$sc2_6_4 +
#     SC2_fixtures_sot$sc2_6_5 + SC2_fixtures_sot$sc2_0_6 + SC2_fixtures_sot$sc2_1_6 + SC2_fixtures_sot$sc2_2_6 + SC2_fixtures_sot$sc2_3_6 +
#     SC2_fixtures_sot$sc2_4_6 + SC2_fixtures_sot$sc2_5_6 + SC2_fixtures_sot$sc2_6_6
# )
# #un25
# SC2_fixtures_sot$sc2_un25 <- (
#   SC2_fixtures_sot$sc2_0_0 + SC2_fixtures_sot$sc2_1_0 + SC2_fixtures_sot$sc2_0_1 + SC2_fixtures_sot$sc2_1_1 + SC2_fixtures_sot$sc2_2_0 + SC2_fixtures_sot$sc2_0_2
# )
# #odds
# SC2_fixtures_sot$sc2_ov25_odds <- round((1/SC2_fixtures_sot$sc2_ov25),digits = 2)
# SC2_fixtures_sot$sc2_un25_odds <- round((1/SC2_fixtures_sot$sc2_un25),digits = 2)
#
# SC2_fixtures_sot$sc2_ov25_odds
# SC2_fixtures_sot$sc2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC2_fixtures_sot$sc2_ov25 <- percent(SC2_fixtures_sot$sc2_ov25, accuracy = 0.1)
#
# SC2_fixtures_sot$sc2_un25 <- percent(SC2_fixtures_sot$sc2_un25, accuracy = 0.1)
# SC2_fixtures_sot$sc2_pssotre <- paste(round(SC2_fixtures_sot$sc2_xHST,digits = 0),round(SC2_fixtures_sot$sc2_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(SC2_fixtures,'Divisions/SC2.xlsx',sheetName = "SC2", append = TRUE)
# ################################################################################################################################################################################################################################
# #SC3
# HomeTeam_sc3_sot <- rep(sc3_teams, each = length(sc3_teams))
# AwayTeam_sc3_sot <- rep(sc3_teams, length(sc3_teams))
# SC3_fixtures_sot <- cbind(HomeTeam_sc3_sot,AwayTeam_sc3_sot)
# SC3_fixtures_sot <- as.data.frame(SC3_fixtures_sot)
# SC3_fixtures_sot <- SC3_fixtures_sot[!SC3_fixtures_sot$HomeTeam_sc3_sot == SC3_fixtures_sot$AwayTeam_sc3_sot,]
# rownames(SC3_fixtures_sot) <- NULL
# SC3_fixtures_sot$Div <- "SC3"
# SC3_fixtures_sot <- SC3_fixtures_sot[,c(3,1,2)]
#
# SC3_fixtures_sot$avg_HST_sc3 <- sc3_avg_HST
#
# SC3_fixtures_sot$sc3_homesotas <- rep(sc3_home_sotas,each = length(sc3_teams)-1)
#
# sc3_awaysods_lookup <- cbind(sc3_teams,sc3_away_sods)
#
# sc3_awaysods_lookup <- as.data.frame(sc3_awaysods_lookup)
#
# colnames(sc3_awaysods_lookup) <- c("AwayTeam_sc3_sot","sc3_awaysods")
#
#
# require('RH2')
# SC3_fixtures_sot$sc3_awaysods <- sqldf("SELECT sc3_awaysods_lookup.sc3_awaysods FROM sc3_awaysods_lookup INNER JOIN SC3_fixtures_sot ON sc3_awaysods_lookup.AwayTeam_sc3_sot = SC3_fixtures_sot.AwayTeam_sc3_sot")
#
# SC3_fixtures_sot$avg_AST_sc3 <- sc3_avg_AST
#
# sc3_awaysotas_lookup <- cbind(sc3_teams,sc3_away_sotas)
#
# sc3_awaysotas_lookup <- as.data.frame(sc3_awaysotas_lookup)
#
# colnames(sc3_awaysotas_lookup) <- c("AwayTeam_sc3_sot","sc3_awaysotas")
#
# SC3_fixtures_sot$sc3_awaysotas <- sqldf("SELECT sc3_awaysotas_lookup.sc3_awaysotas FROM sc3_awaysotas_lookup INNER JOIN SC3_fixtures_sot ON sc3_awaysotas_lookup.AwayTeam_sc3_sot = SC3_fixtures_sot.AwayTeam_sc3_sot")
#
# SC3_fixtures_sot$sc3_homesods <- rep(sc3_home_sods,each = length(sc3_teams)-1)
#
# SC3_fixtures_sot$sc3_awaysods <- as.numeric(unlist(SC3_fixtures_sot$sc3_awaysods))
# #xGH
# SC3_fixtures_sot$sc3_xHST <- SC3_fixtures_sot$avg_HST_sc3 * SC3_fixtures_sot$sc3_homesotas * SC3_fixtures_sot$sc3_awaysods
# #xGA
#
# SC3_fixtures_sot$sc3_awaysotas <- as.numeric(unlist(SC3_fixtures_sot$sc3_awaysotas))
#
# SC3_fixtures_sot$sc3_xAST <- SC3_fixtures_sot$avg_AST_sc3 * SC3_fixtures_sot$sc3_awaysotas * SC3_fixtures_sot$sc3_homesods
#
# SC3_fixtures_sot$sc3_0_0 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_0 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_0_1 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_1 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_0 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_0_2 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_2 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_1 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_2 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_3 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_0 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_1 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_2 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_0_3 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_3 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_3 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_4 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_0 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_1 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_2 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_3 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_0_4 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_4 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_4 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_4 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_5 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_0 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_1 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_2 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_3 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_4 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_0_5 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_5 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_5 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_5 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_5 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_6 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_0 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(0,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_1 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(1,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_2 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(2,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_3 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(3,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_4 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(4,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_6_5 <- round(stats::dpois(6,SC3_fixtures_sot$sc3_xHST) * stats::dpois(5,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_0_6 <- round(stats::dpois(0,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_1_6 <- round(stats::dpois(1,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_2_6 <- round(stats::dpois(2,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_3_6 <- round(stats::dpois(3,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_4_6 <- round(stats::dpois(4,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# SC3_fixtures_sot$sc3_5_6 <- round(stats::dpois(5,SC3_fixtures_sot$sc3_xHST) * stats::dpois(6,SC3_fixtures_sot$sc3_xAST), digits = 4)
# #Home win
# SC3_fixtures_sot$sc3_H <- (
#   SC3_fixtures_sot$sc3_1_0 + SC3_fixtures_sot$sc3_2_0 + SC3_fixtures_sot$sc3_2_1 + SC3_fixtures_sot$sc3_3_0 + SC3_fixtures_sot$sc3_3_1 +
#     SC3_fixtures_sot$sc3_3_2 + SC3_fixtures_sot$sc3_4_0 + SC3_fixtures_sot$sc3_4_1 + SC3_fixtures_sot$sc3_4_2 + SC3_fixtures_sot$sc3_4_3 +
#     SC3_fixtures_sot$sc3_5_0 + SC3_fixtures_sot$sc3_5_1 + SC3_fixtures_sot$sc3_5_2 + SC3_fixtures_sot$sc3_5_3 + SC3_fixtures_sot$sc3_5_4 +
#     SC3_fixtures_sot$sc3_6_0 + SC3_fixtures_sot$sc3_6_1 + SC3_fixtures_sot$sc3_6_2 + SC3_fixtures_sot$sc3_6_3 + SC3_fixtures_sot$sc3_6_4 +
#     SC3_fixtures_sot$sc3_6_5
# )
#
# SC3_fixtures_sot$sc3_H <- percent(SC3_fixtures_sot$sc3_H, accuracy = 0.1)
#
# #Draw
# SC3_fixtures_sot$sc3_D <- (
#
#   SC3_fixtures_sot$sc3_0_0 + SC3_fixtures_sot$sc3_1_1 + SC3_fixtures_sot$sc3_2_2 + SC3_fixtures_sot$sc3_3_3 + SC3_fixtures_sot$sc3_4_4 +
#     SC3_fixtures_sot$sc3_5_5 + SC3_fixtures_sot$sc3_6_6
# )
#
# SC3_fixtures_sot$sc3_D <- percent(SC3_fixtures_sot$sc3_D, accuracy = 0.1)
#
# #Away
#
# SC3_fixtures_sot$sc3_A <- (
#   SC3_fixtures_sot$sc3_0_1 + SC3_fixtures_sot$sc3_0_2 + SC3_fixtures_sot$sc3_1_2 + SC3_fixtures_sot$sc3_0_3 + SC3_fixtures_sot$sc3_1_3 +
#     SC3_fixtures_sot$sc3_2_3 + SC3_fixtures_sot$sc3_0_4 + SC3_fixtures_sot$sc3_1_4 + SC3_fixtures_sot$sc3_2_4 + SC3_fixtures_sot$sc3_3_4 +
#     SC3_fixtures_sot$sc3_0_5 + SC3_fixtures_sot$sc3_1_5 + SC3_fixtures_sot$sc3_2_5 + SC3_fixtures_sot$sc3_3_5 + SC3_fixtures_sot$sc3_4_5 +
#     SC3_fixtures_sot$sc3_0_6 + SC3_fixtures_sot$sc3_1_6 + SC3_fixtures_sot$sc3_2_6 + SC3_fixtures_sot$sc3_3_6 + SC3_fixtures_sot$sc3_4_6 +
#     SC3_fixtures_sot$sc3_5_6
# )
#
# SC3_fixtures_sot$sc3_A <- percent(SC3_fixtures_sot$sc3_A, accuracy = 0.1)
#
# #ov25
# SC3_fixtures_sot$sc3_ov25 <- (
#   SC3_fixtures_sot$sc3_2_1 + SC3_fixtures_sot$sc3_1_2 + SC3_fixtures_sot$sc3_2_2 + SC3_fixtures_sot$sc3_3_0 + SC3_fixtures_sot$sc3_3_1 +
#     SC3_fixtures_sot$sc3_3_2 + SC3_fixtures_sot$sc3_0_3 + SC3_fixtures_sot$sc3_1_3 + SC3_fixtures_sot$sc3_2_3 + SC3_fixtures_sot$sc3_3_3 +
#     SC3_fixtures_sot$sc3_4_0 + SC3_fixtures_sot$sc3_4_1 + SC3_fixtures_sot$sc3_4_2 + SC3_fixtures_sot$sc3_4_3 + SC3_fixtures_sot$sc3_0_4 +
#     SC3_fixtures_sot$sc3_1_4 + SC3_fixtures_sot$sc3_2_4 + SC3_fixtures_sot$sc3_3_4 + SC3_fixtures_sot$sc3_4_4 + SC3_fixtures_sot$sc3_5_0 +
#     SC3_fixtures_sot$sc3_5_1 + SC3_fixtures_sot$sc3_5_2 + SC3_fixtures_sot$sc3_5_3 + SC3_fixtures_sot$sc3_5_4 + SC3_fixtures_sot$sc3_0_5 +
#     SC3_fixtures_sot$sc3_1_5 + SC3_fixtures_sot$sc3_2_5 + SC3_fixtures_sot$sc3_3_5 + SC3_fixtures_sot$sc3_4_5 + SC3_fixtures_sot$sc3_5_5 +
#     SC3_fixtures_sot$sc3_6_0 + SC3_fixtures_sot$sc3_6_1 + SC3_fixtures_sot$sc3_6_2 + SC3_fixtures_sot$sc3_6_3 + SC3_fixtures_sot$sc3_6_4 +
#     SC3_fixtures_sot$sc3_6_5 + SC3_fixtures_sot$sc3_0_6 + SC3_fixtures_sot$sc3_1_6 + SC3_fixtures_sot$sc3_2_6 + SC3_fixtures_sot$sc3_3_6 +
#     SC3_fixtures_sot$sc3_4_6 + SC3_fixtures_sot$sc3_5_6 + SC3_fixtures_sot$sc3_6_6
# )
# #un25
# SC3_fixtures_sot$sc3_un25 <- (
#   SC3_fixtures_sot$sc3_0_0 + SC3_fixtures_sot$sc3_1_0 + SC3_fixtures_sot$sc3_0_1 + SC3_fixtures_sot$sc3_1_1 + SC3_fixtures_sot$sc3_2_0 + SC3_fixtures_sot$sc3_0_2
# )
# #odds
# SC3_fixtures_sot$sc3_ov25_odds <- round((1/SC3_fixtures_sot$sc3_ov25),digits = 2)
# SC3_fixtures_sot$sc3_un25_odds <- round((1/SC3_fixtures_sot$sc3_un25),digits = 2)
#
# SC3_fixtures_sot$sc3_ov25_odds
# SC3_fixtures_sot$sc3_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SC3_fixtures_sot$sc3_ov25 <- percent(SC3_fixtures_sot$sc3_ov25, accuracy = 0.1)
#
# SC3_fixtures_sot$sc3_un25 <- percent(SC3_fixtures_sot$sc3_un25, accuracy = 0.1)
# SC3_fixtures_sot$sc3_pssotre <- paste(round(SC3_fixtures_sot$sc3_xHST,digits = 0),round(SC3_fixtures_sot$sc3_xAST,digits = 0),sep = "-")
# #write.xlsx(SC3_fixtures,'Divisions/SC3.xlsx',sheetName = "SC3", append = TRUE)
# #################################################################################################################
# ################################################################################################################
# #################################################################################################################
# #SP1
# HomeTeam_sp1_sot <- rep(sp1_teams, each = length(sp1_teams))
# AwayTeam_sp1_sot <- rep(sp1_teams, length(sp1_teams))
# SP1_fixtures_sot <- cbind(HomeTeam_sp1_sot,AwayTeam_sp1_sot)
# SP1_fixtures_sot <- as.data.frame(SP1_fixtures_sot)
# SP1_fixtures_sot <- SP1_fixtures_sot[!SP1_fixtures_sot$HomeTeam_sp1_sot == SP1_fixtures_sot$AwayTeam_sp1_sot,]
# rownames(SP1_fixtures_sot) <- NULL
# SP1_fixtures_sot$Div <- "SP1"
# SP1_fixtures_sot <- SP1_fixtures_sot[,c(3,1,2)]
#
# SP1_fixtures_sot$avg_HST_sp1 <- sp1_avg_HST
#
# SP1_fixtures_sot$sp1_homesotas <- rep(sp1_home_sotas,each = length(sp1_teams)-1)
#
# sp1_awaysods_lookup <- cbind(sp1_teams,sp1_away_sods)
#
# sp1_awaysods_lookup <- as.data.frame(sp1_awaysods_lookup)
#
# colnames(sp1_awaysods_lookup) <- c("AwayTeam_sp1_sot","sp1_awaysods")
#
#
# require('RH2')
# SP1_fixtures_sot$sp1_awaysods <- sqldf("SELECT sp1_awaysods_lookup.sp1_awaysods FROM sp1_awaysods_lookup INNER JOIN SP1_fixtures_sot ON sp1_awaysods_lookup.AwayTeam_sp1_sot = SP1_fixtures_sot.AwayTeam_sp1_sot")
#
# SP1_fixtures_sot$avg_AST_sp1 <- sp1_avg_AST
#
# sp1_awaysotas_lookup <- cbind(sp1_teams,sp1_away_sotas)
#
# sp1_awaysotas_lookup <- as.data.frame(sp1_awaysotas_lookup)
#
# colnames(sp1_awaysotas_lookup) <- c("AwayTeam_sp1_sot","sp1_awaysotas")
#
# SP1_fixtures_sot$sp1_awaysotas <- sqldf("SELECT sp1_awaysotas_lookup.sp1_awaysotas FROM sp1_awaysotas_lookup INNER JOIN SP1_fixtures_sot ON sp1_awaysotas_lookup.AwayTeam_sp1_sot = SP1_fixtures_sot.AwayTeam_sp1_sot")
#
# SP1_fixtures_sot$sp1_homesods <- rep(sp1_home_sods,each = length(sp1_teams)-1)
#
# SP1_fixtures_sot$sp1_awaysods <- as.numeric(unlist(SP1_fixtures_sot$sp1_awaysods))
# #xGH
# SP1_fixtures_sot$sp1_xHST <- SP1_fixtures_sot$avg_HST_sp1 * SP1_fixtures_sot$sp1_homesotas * SP1_fixtures_sot$sp1_awaysods
# #xGA
#
# SP1_fixtures_sot$sp1_awaysotas <- as.numeric(unlist(SP1_fixtures_sot$sp1_awaysotas))
#
# SP1_fixtures_sot$sp1_xAST <- SP1_fixtures_sot$avg_AST_sp1 * SP1_fixtures_sot$sp1_awaysotas * SP1_fixtures_sot$sp1_homesods
#
# SP1_fixtures_sot$sp1_0_0 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_0 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_0_1 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_1 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_0 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_0_2 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_2 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_1 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_2 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_3 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_0 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_1 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_2 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_0_3 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_3 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_3 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_4 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_0 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_1 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_2 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_3 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_0_4 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_4 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_4 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_4 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_5 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_0 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_1 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_2 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_3 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_4 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_0_5 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_5 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_5 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_5 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_5 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_6 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_0 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(0,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_1 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(1,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_2 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(2,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_3 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(3,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_4 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(4,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_6_5 <- round(stats::dpois(6,SP1_fixtures_sot$sp1_xHST) * stats::dpois(5,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_0_6 <- round(stats::dpois(0,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_1_6 <- round(stats::dpois(1,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_2_6 <- round(stats::dpois(2,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_3_6 <- round(stats::dpois(3,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_4_6 <- round(stats::dpois(4,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# SP1_fixtures_sot$sp1_5_6 <- round(stats::dpois(5,SP1_fixtures_sot$sp1_xHST) * stats::dpois(6,SP1_fixtures_sot$sp1_xAST), digits = 4)
# #Home win
# SP1_fixtures_sot$sp1_H <- (
#   SP1_fixtures_sot$sp1_1_0 + SP1_fixtures_sot$sp1_2_0 + SP1_fixtures_sot$sp1_2_1 + SP1_fixtures_sot$sp1_3_0 + SP1_fixtures_sot$sp1_3_1 +
#     SP1_fixtures_sot$sp1_3_2 + SP1_fixtures_sot$sp1_4_0 + SP1_fixtures_sot$sp1_4_1 + SP1_fixtures_sot$sp1_4_2 + SP1_fixtures_sot$sp1_4_3 +
#     SP1_fixtures_sot$sp1_5_0 + SP1_fixtures_sot$sp1_5_1 + SP1_fixtures_sot$sp1_5_2 + SP1_fixtures_sot$sp1_5_3 + SP1_fixtures_sot$sp1_5_4 +
#     SP1_fixtures_sot$sp1_6_0 + SP1_fixtures_sot$sp1_6_1 + SP1_fixtures_sot$sp1_6_2 + SP1_fixtures_sot$sp1_6_3 + SP1_fixtures_sot$sp1_6_4 +
#     SP1_fixtures_sot$sp1_6_5
# )
#
# SP1_fixtures_sot$sp1_H <- percent(SP1_fixtures_sot$sp1_H, accuracy = 0.1)
#
# #Draw
# SP1_fixtures_sot$sp1_D <- (
#
#   SP1_fixtures_sot$sp1_0_0 + SP1_fixtures_sot$sp1_1_1 + SP1_fixtures_sot$sp1_2_2 + SP1_fixtures_sot$sp1_3_3 + SP1_fixtures_sot$sp1_4_4 +
#     SP1_fixtures_sot$sp1_5_5 + SP1_fixtures_sot$sp1_6_6
# )
#
# SP1_fixtures_sot$sp1_D <- percent(SP1_fixtures_sot$sp1_D, accuracy = 0.1)
#
# #Away
#
# SP1_fixtures_sot$sp1_A <- (
#   SP1_fixtures_sot$sp1_0_1 + SP1_fixtures_sot$sp1_0_2 + SP1_fixtures_sot$sp1_1_2 + SP1_fixtures_sot$sp1_0_3 + SP1_fixtures_sot$sp1_1_3 +
#     SP1_fixtures_sot$sp1_2_3 + SP1_fixtures_sot$sp1_0_4 + SP1_fixtures_sot$sp1_1_4 + SP1_fixtures_sot$sp1_2_4 + SP1_fixtures_sot$sp1_3_4 +
#     SP1_fixtures_sot$sp1_0_5 + SP1_fixtures_sot$sp1_1_5 + SP1_fixtures_sot$sp1_2_5 + SP1_fixtures_sot$sp1_3_5 + SP1_fixtures_sot$sp1_4_5 +
#     SP1_fixtures_sot$sp1_0_6 + SP1_fixtures_sot$sp1_1_6 + SP1_fixtures_sot$sp1_2_6 + SP1_fixtures_sot$sp1_3_6 + SP1_fixtures_sot$sp1_4_6 +
#     SP1_fixtures_sot$sp1_5_6
# )
#
# SP1_fixtures_sot$sp1_A <- percent(SP1_fixtures_sot$sp1_A, accuracy = 0.1)
#
# #ov25
# SP1_fixtures_sot$sp1_ov25 <- (
#   SP1_fixtures_sot$sp1_2_1 + SP1_fixtures_sot$sp1_1_2 + SP1_fixtures_sot$sp1_2_2 + SP1_fixtures_sot$sp1_3_0 + SP1_fixtures_sot$sp1_3_1 +
#     SP1_fixtures_sot$sp1_3_2 + SP1_fixtures_sot$sp1_0_3 + SP1_fixtures_sot$sp1_1_3 + SP1_fixtures_sot$sp1_2_3 + SP1_fixtures_sot$sp1_3_3 +
#     SP1_fixtures_sot$sp1_4_0 + SP1_fixtures_sot$sp1_4_1 + SP1_fixtures_sot$sp1_4_2 + SP1_fixtures_sot$sp1_4_3 + SP1_fixtures_sot$sp1_0_4 +
#     SP1_fixtures_sot$sp1_1_4 + SP1_fixtures_sot$sp1_2_4 + SP1_fixtures_sot$sp1_3_4 + SP1_fixtures_sot$sp1_4_4 + SP1_fixtures_sot$sp1_5_0 +
#     SP1_fixtures_sot$sp1_5_1 + SP1_fixtures_sot$sp1_5_2 + SP1_fixtures_sot$sp1_5_3 + SP1_fixtures_sot$sp1_5_4 + SP1_fixtures_sot$sp1_0_5 +
#     SP1_fixtures_sot$sp1_1_5 + SP1_fixtures_sot$sp1_2_5 + SP1_fixtures_sot$sp1_3_5 + SP1_fixtures_sot$sp1_4_5 + SP1_fixtures_sot$sp1_5_5 +
#     SP1_fixtures_sot$sp1_6_0 + SP1_fixtures_sot$sp1_6_1 + SP1_fixtures_sot$sp1_6_2 + SP1_fixtures_sot$sp1_6_3 + SP1_fixtures_sot$sp1_6_4 +
#     SP1_fixtures_sot$sp1_6_5 + SP1_fixtures_sot$sp1_0_6 + SP1_fixtures_sot$sp1_1_6 + SP1_fixtures_sot$sp1_2_6 + SP1_fixtures_sot$sp1_3_6 +
#     SP1_fixtures_sot$sp1_4_6 + SP1_fixtures_sot$sp1_5_6 + SP1_fixtures_sot$sp1_6_6
# )
# #un25
# SP1_fixtures_sot$sp1_un25 <- (
#   SP1_fixtures_sot$sp1_0_0 + SP1_fixtures_sot$sp1_1_0 + SP1_fixtures_sot$sp1_0_1 + SP1_fixtures_sot$sp1_1_1 + SP1_fixtures_sot$sp1_2_0 + SP1_fixtures_sot$sp1_0_2
# )
# #odds
# SP1_fixtures_sot$sp1_ov25_odds <- round((1/SP1_fixtures_sot$sp1_ov25),digits = 2)
# SP1_fixtures_sot$sp1_un25_odds <- round((1/SP1_fixtures_sot$sp1_un25),digits = 2)
#
# SP1_fixtures_sot$sp1_ov25_odds
# SP1_fixtures_sot$sp1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP1_fixtures_sot$sp1_ov25 <- percent(SP1_fixtures_sot$sp1_ov25, accuracy = 0.1)
#
# SP1_fixtures_sot$sp1_un25 <- percent(SP1_fixtures_sot$sp1_un25, accuracy = 0.1)
# SP1_fixtures_sot$sp1_pssotre <- paste(round(SP1_fixtures_sot$sp1_xHST,digits = 0),round(SP1_fixtures_sot$sp1_xAST,digits = 0),sep = "-")
# #write.xlsx(SP1_fixtures,'Divisions/SP1.xlsx',sheetName = "SP1", append = TRUE)
# #################################################################################################################
# #SP2
# HomeTeam_sp2_sot <- rep(sp2_teams, each = length(sp2_teams))
# AwayTeam_sp2_sot <- rep(sp2_teams, length(sp2_teams))
# SP2_fixtures_sot <- cbind(HomeTeam_sp2_sot,AwayTeam_sp2_sot)
# SP2_fixtures_sot <- as.data.frame(SP2_fixtures_sot)
# SP2_fixtures_sot <- SP2_fixtures_sot[!SP2_fixtures_sot$HomeTeam_sp2_sot == SP2_fixtures_sot$AwayTeam_sp2_sot,]
# rownames(SP2_fixtures_sot) <- NULL
# SP2_fixtures_sot$Div <- "SP2"
# SP2_fixtures_sot <- SP2_fixtures_sot[,c(3,1,2)]
#
# SP2_fixtures_sot$avg_HST_sp2 <- sp2_avg_HST
#
# SP2_fixtures_sot$sp2_homesotas <- rep(sp2_home_sotas,each = length(sp2_teams)-1)
#
# sp2_awaysods_lookup <- cbind(sp2_teams,sp2_away_sods)
#
# sp2_awaysods_lookup <- as.data.frame(sp2_awaysods_lookup)
#
# colnames(sp2_awaysods_lookup) <- c("AwayTeam_sp2_sot","sp2_awaysods")
#
#
# require('RH2')
# SP2_fixtures_sot$sp2_awaysods <- sqldf("SELECT sp2_awaysods_lookup.sp2_awaysods FROM sp2_awaysods_lookup INNER JOIN SP2_fixtures_sot ON sp2_awaysods_lookup.AwayTeam_sp2_sot = SP2_fixtures_sot.AwayTeam_sp2_sot")
#
# SP2_fixtures_sot$avg_AST_sp2 <- sp2_avg_AST
#
# sp2_awaysotas_lookup <- cbind(sp2_teams,sp2_away_sotas)
#
# sp2_awaysotas_lookup <- as.data.frame(sp2_awaysotas_lookup)
#
# colnames(sp2_awaysotas_lookup) <- c("AwayTeam_sp2_sot","sp2_awaysotas")
#
# SP2_fixtures_sot$sp2_awaysotas <- sqldf("SELECT sp2_awaysotas_lookup.sp2_awaysotas FROM sp2_awaysotas_lookup INNER JOIN SP2_fixtures_sot ON sp2_awaysotas_lookup.AwayTeam_sp2_sot = SP2_fixtures_sot.AwayTeam_sp2_sot")
#
# SP2_fixtures_sot$sp2_homesods <- rep(sp2_home_sods,each = length(sp2_teams)-1)
#
# SP2_fixtures_sot$sp2_awaysods <- as.numeric(unlist(SP2_fixtures_sot$sp2_awaysods))
# #xGH
# SP2_fixtures_sot$sp2_xHST <- SP2_fixtures_sot$avg_HST_sp2 * SP2_fixtures_sot$sp2_homesotas * SP2_fixtures_sot$sp2_awaysods
# #xGA
#
# SP2_fixtures_sot$sp2_awaysotas <- as.numeric(unlist(SP2_fixtures_sot$sp2_awaysotas))
#
# SP2_fixtures_sot$sp2_xAST <- SP2_fixtures_sot$avg_AST_sp2 * SP2_fixtures_sot$sp2_awaysotas * SP2_fixtures_sot$sp2_homesods
#
# SP2_fixtures_sot$sp2_0_0 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_0 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_0_1 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_1 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_0 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_0_2 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_2 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_1 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_2 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_3 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_0 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_1 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_2 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_0_3 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_3 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_3 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_4 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_0 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_1 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_2 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_3 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_0_4 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_4 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_4 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_4 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_5 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_0 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_1 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_2 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_3 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_4 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_0_5 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_5 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_5 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_5 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_5 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_6 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_0 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(0,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_1 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(1,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_2 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(2,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_3 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(3,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_4 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(4,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_6_5 <- round(stats::dpois(6,SP2_fixtures_sot$sp2_xHST) * stats::dpois(5,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_0_6 <- round(stats::dpois(0,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_1_6 <- round(stats::dpois(1,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_2_6 <- round(stats::dpois(2,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_3_6 <- round(stats::dpois(3,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_4_6 <- round(stats::dpois(4,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# SP2_fixtures_sot$sp2_5_6 <- round(stats::dpois(5,SP2_fixtures_sot$sp2_xHST) * stats::dpois(6,SP2_fixtures_sot$sp2_xAST), digits = 4)
# #Home win
# SP2_fixtures_sot$sp2_H <- (
#   SP2_fixtures_sot$sp2_1_0 + SP2_fixtures_sot$sp2_2_0 + SP2_fixtures_sot$sp2_2_1 + SP2_fixtures_sot$sp2_3_0 + SP2_fixtures_sot$sp2_3_1 +
#     SP2_fixtures_sot$sp2_3_2 + SP2_fixtures_sot$sp2_4_0 + SP2_fixtures_sot$sp2_4_1 + SP2_fixtures_sot$sp2_4_2 + SP2_fixtures_sot$sp2_4_3 +
#     SP2_fixtures_sot$sp2_5_0 + SP2_fixtures_sot$sp2_5_1 + SP2_fixtures_sot$sp2_5_2 + SP2_fixtures_sot$sp2_5_3 + SP2_fixtures_sot$sp2_5_4 +
#     SP2_fixtures_sot$sp2_6_0 + SP2_fixtures_sot$sp2_6_1 + SP2_fixtures_sot$sp2_6_2 + SP2_fixtures_sot$sp2_6_3 + SP2_fixtures_sot$sp2_6_4 +
#     SP2_fixtures_sot$sp2_6_5
# )
#
# SP2_fixtures_sot$sp2_H <- percent(SP2_fixtures_sot$sp2_H, accuracy = 0.1)
#
# #Draw
# SP2_fixtures_sot$sp2_D <- (
#
#   SP2_fixtures_sot$sp2_0_0 + SP2_fixtures_sot$sp2_1_1 + SP2_fixtures_sot$sp2_2_2 + SP2_fixtures_sot$sp2_3_3 + SP2_fixtures_sot$sp2_4_4 +
#     SP2_fixtures_sot$sp2_5_5 + SP2_fixtures_sot$sp2_6_6
# )
#
# SP2_fixtures_sot$sp2_D <- percent(SP2_fixtures_sot$sp2_D, accuracy = 0.1)
#
# #Away
#
# SP2_fixtures_sot$sp2_A <- (
#   SP2_fixtures_sot$sp2_0_1 + SP2_fixtures_sot$sp2_0_2 + SP2_fixtures_sot$sp2_1_2 + SP2_fixtures_sot$sp2_0_3 + SP2_fixtures_sot$sp2_1_3 +
#     SP2_fixtures_sot$sp2_2_3 + SP2_fixtures_sot$sp2_0_4 + SP2_fixtures_sot$sp2_1_4 + SP2_fixtures_sot$sp2_2_4 + SP2_fixtures_sot$sp2_3_4 +
#     SP2_fixtures_sot$sp2_0_5 + SP2_fixtures_sot$sp2_1_5 + SP2_fixtures_sot$sp2_2_5 + SP2_fixtures_sot$sp2_3_5 + SP2_fixtures_sot$sp2_4_5 +
#     SP2_fixtures_sot$sp2_0_6 + SP2_fixtures_sot$sp2_1_6 + SP2_fixtures_sot$sp2_2_6 + SP2_fixtures_sot$sp2_3_6 + SP2_fixtures_sot$sp2_4_6 +
#     SP2_fixtures_sot$sp2_5_6
# )
#
# SP2_fixtures_sot$sp2_A <- percent(SP2_fixtures_sot$sp2_A, accuracy = 0.1)
#
# #ov25
# SP2_fixtures_sot$sp2_ov25 <- (
#   SP2_fixtures_sot$sp2_2_1 + SP2_fixtures_sot$sp2_1_2 + SP2_fixtures_sot$sp2_2_2 + SP2_fixtures_sot$sp2_3_0 + SP2_fixtures_sot$sp2_3_1 +
#     SP2_fixtures_sot$sp2_3_2 + SP2_fixtures_sot$sp2_0_3 + SP2_fixtures_sot$sp2_1_3 + SP2_fixtures_sot$sp2_2_3 + SP2_fixtures_sot$sp2_3_3 +
#     SP2_fixtures_sot$sp2_4_0 + SP2_fixtures_sot$sp2_4_1 + SP2_fixtures_sot$sp2_4_2 + SP2_fixtures_sot$sp2_4_3 + SP2_fixtures_sot$sp2_0_4 +
#     SP2_fixtures_sot$sp2_1_4 + SP2_fixtures_sot$sp2_2_4 + SP2_fixtures_sot$sp2_3_4 + SP2_fixtures_sot$sp2_4_4 + SP2_fixtures_sot$sp2_5_0 +
#     SP2_fixtures_sot$sp2_5_1 + SP2_fixtures_sot$sp2_5_2 + SP2_fixtures_sot$sp2_5_3 + SP2_fixtures_sot$sp2_5_4 + SP2_fixtures_sot$sp2_0_5 +
#     SP2_fixtures_sot$sp2_1_5 + SP2_fixtures_sot$sp2_2_5 + SP2_fixtures_sot$sp2_3_5 + SP2_fixtures_sot$sp2_4_5 + SP2_fixtures_sot$sp2_5_5 +
#     SP2_fixtures_sot$sp2_6_0 + SP2_fixtures_sot$sp2_6_1 + SP2_fixtures_sot$sp2_6_2 + SP2_fixtures_sot$sp2_6_3 + SP2_fixtures_sot$sp2_6_4 +
#     SP2_fixtures_sot$sp2_6_5 + SP2_fixtures_sot$sp2_0_6 + SP2_fixtures_sot$sp2_1_6 + SP2_fixtures_sot$sp2_2_6 + SP2_fixtures_sot$sp2_3_6 +
#     SP2_fixtures_sot$sp2_4_6 + SP2_fixtures_sot$sp2_5_6 + SP2_fixtures_sot$sp2_6_6
# )
# #un25
# SP2_fixtures_sot$sp2_un25 <- (
#   SP2_fixtures_sot$sp2_0_0 + SP2_fixtures_sot$sp2_1_0 + SP2_fixtures_sot$sp2_0_1 + SP2_fixtures_sot$sp2_1_1 + SP2_fixtures_sot$sp2_2_0 + SP2_fixtures_sot$sp2_0_2
# )
# #odds
# SP2_fixtures_sot$sp2_ov25_odds <- round((1/SP2_fixtures_sot$sp2_ov25),digits = 2)
# SP2_fixtures_sot$sp2_un25_odds <- round((1/SP2_fixtures_sot$sp2_un25),digits = 2)
#
# SP2_fixtures_sot$sp2_ov25_odds
# SP2_fixtures_sot$sp2_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# SP2_fixtures_sot$sp2_ov25 <- percent(SP2_fixtures_sot$sp2_ov25, accuracy = 0.1)
#
# SP2_fixtures_sot$sp2_un25 <- percent(SP2_fixtures_sot$sp2_un25, accuracy = 0.1)
# SP2_fixtures_sot$sp2_pssotre <- paste(round(SP2_fixtures_sot$sp2_xHST,digits = 0),round(SP2_fixtures_sot$sp2_xAST,digits = 0),sep = "-")
# #write out
# #write.xlsx(SP2_fixtures,'Divisions/SP2.xlsx',sheetName = "SP2", append = TRUE)
# #################################################################################################################
# #T1
# HomeTeam_t1_sot <- rep(t1_teams, each = length(t1_teams))
# AwayTeam_t1_sot <- rep(t1_teams, length(t1_teams))
# T1_fixtures_sot <- cbind(HomeTeam_t1_sot,AwayTeam_t1_sot)
# T1_fixtures_sot <- as.data.frame(T1_fixtures_sot)
# T1_fixtures_sot <- T1_fixtures_sot[!T1_fixtures_sot$HomeTeam_t1_sot == T1_fixtures_sot$AwayTeam_t1_sot,]
# rownames(T1_fixtures_sot) <- NULL
# T1_fixtures_sot$Div <- "T1"
# T1_fixtures_sot <- T1_fixtures_sot[,c(3,1,2)]
#
# T1_fixtures_sot$avg_HST_t1 <- t1_avg_HST
#
# T1_fixtures_sot$t1_homesotas <- rep(t1_home_sotas,each = length(t1_teams)-1)
#
# t1_awaysods_lookup <- cbind(t1_teams,t1_away_sods)
#
# t1_awaysods_lookup <- as.data.frame(t1_awaysods_lookup)
#
# colnames(t1_awaysods_lookup) <- c("AwayTeam_t1_sot","t1_awaysods")
#
#
# require('RH2')
# T1_fixtures_sot$t1_awaysods <- sqldf("SELECT t1_awaysods_lookup.t1_awaysods FROM t1_awaysods_lookup INNER JOIN T1_fixtures_sot ON t1_awaysods_lookup.AwayTeam_t1_sot = T1_fixtures_sot.AwayTeam_t1_sot")
#
# T1_fixtures_sot$avg_AST_t1 <- t1_avg_AST
#
# t1_awaysotas_lookup <- cbind(t1_teams,t1_away_sotas)
#
# t1_awaysotas_lookup <- as.data.frame(t1_awaysotas_lookup)
#
# colnames(t1_awaysotas_lookup) <- c("AwayTeam_t1_sot","t1_awaysotas")
#
# T1_fixtures_sot$t1_awaysotas <- sqldf("SELECT t1_awaysotas_lookup.t1_awaysotas FROM t1_awaysotas_lookup INNER JOIN T1_fixtures_sot ON t1_awaysotas_lookup.AwayTeam_t1_sot = T1_fixtures_sot.AwayTeam_t1_sot")
#
# T1_fixtures_sot$t1_homesods <- rep(t1_home_sods,each = length(t1_teams)-1)
#
# T1_fixtures_sot$t1_awaysods <- as.numeric(unlist(T1_fixtures_sot$t1_awaysods))
# #xGH
# T1_fixtures_sot$t1_xHST <- T1_fixtures_sot$avg_HST_t1 * T1_fixtures_sot$t1_homesotas * T1_fixtures_sot$t1_awaysods
# #xGA
#
# T1_fixtures_sot$t1_awaysotas <- as.numeric(unlist(T1_fixtures_sot$t1_awaysotas))
#
# T1_fixtures_sot$t1_xAST <- T1_fixtures_sot$avg_AST_t1 * T1_fixtures_sot$t1_awaysotas * T1_fixtures_sot$t1_homesods
#
# T1_fixtures_sot$t1_0_0 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_0 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_0_1 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_1 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_0 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_0_2 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_2 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_1 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_2 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_3 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_0 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_1 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_2 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_0_3 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_3 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_3 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_4 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_0 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_1 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_2 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_3 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_0_4 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_4 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_4 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_4 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_5 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_0 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_1 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_2 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_3 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_4 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_0_5 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_5 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_5 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_5 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_5 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_6 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_0 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(0,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_1 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(1,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_2 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(2,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_3 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(3,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_4 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(4,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_6_5 <- round(stats::dpois(6,T1_fixtures_sot$t1_xHST) * stats::dpois(5,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_0_6 <- round(stats::dpois(0,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_1_6 <- round(stats::dpois(1,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_2_6 <- round(stats::dpois(2,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_3_6 <- round(stats::dpois(3,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_4_6 <- round(stats::dpois(4,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# T1_fixtures_sot$t1_5_6 <- round(stats::dpois(5,T1_fixtures_sot$t1_xHST) * stats::dpois(6,T1_fixtures_sot$t1_xAST), digits = 4)
# #Home win
# T1_fixtures_sot$t1_H <- (
#   T1_fixtures_sot$t1_1_0 + T1_fixtures_sot$t1_2_0 + T1_fixtures_sot$t1_2_1 + T1_fixtures_sot$t1_3_0 + T1_fixtures_sot$t1_3_1 +
#     T1_fixtures_sot$t1_3_2 + T1_fixtures_sot$t1_4_0 + T1_fixtures_sot$t1_4_1 + T1_fixtures_sot$t1_4_2 + T1_fixtures_sot$t1_4_3 +
#     T1_fixtures_sot$t1_5_0 + T1_fixtures_sot$t1_5_1 + T1_fixtures_sot$t1_5_2 + T1_fixtures_sot$t1_5_3 + T1_fixtures_sot$t1_5_4 +
#     T1_fixtures_sot$t1_6_0 + T1_fixtures_sot$t1_6_1 + T1_fixtures_sot$t1_6_2 + T1_fixtures_sot$t1_6_3 + T1_fixtures_sot$t1_6_4 +
#     T1_fixtures_sot$t1_6_5
# )
#
# T1_fixtures_sot$t1_H <- percent(T1_fixtures_sot$t1_H, accuracy = 0.1)
#
# #Draw
# T1_fixtures_sot$t1_D <- (
#
#   T1_fixtures_sot$t1_0_0 + T1_fixtures_sot$t1_1_1 + T1_fixtures_sot$t1_2_2 + T1_fixtures_sot$t1_3_3 + T1_fixtures_sot$t1_4_4 +
#     T1_fixtures_sot$t1_5_5 + T1_fixtures_sot$t1_6_6
# )
#
# T1_fixtures_sot$t1_D <- percent(T1_fixtures_sot$t1_D, accuracy = 0.1)
#
# #Away
#
# T1_fixtures_sot$t1_A <- (
#   T1_fixtures_sot$t1_0_1 + T1_fixtures_sot$t1_0_2 + T1_fixtures_sot$t1_1_2 + T1_fixtures_sot$t1_0_3 + T1_fixtures_sot$t1_1_3 +
#     T1_fixtures_sot$t1_2_3 + T1_fixtures_sot$t1_0_4 + T1_fixtures_sot$t1_1_4 + T1_fixtures_sot$t1_2_4 + T1_fixtures_sot$t1_3_4 +
#     T1_fixtures_sot$t1_0_5 + T1_fixtures_sot$t1_1_5 + T1_fixtures_sot$t1_2_5 + T1_fixtures_sot$t1_3_5 + T1_fixtures_sot$t1_4_5 +
#     T1_fixtures_sot$t1_0_6 + T1_fixtures_sot$t1_1_6 + T1_fixtures_sot$t1_2_6 + T1_fixtures_sot$t1_3_6 + T1_fixtures_sot$t1_4_6 +
#     T1_fixtures_sot$t1_5_6
# )
#
# T1_fixtures_sot$t1_A <- percent(T1_fixtures_sot$t1_A, accuracy = 0.1)
#
# #ov25
# T1_fixtures_sot$t1_ov25 <- (
#   T1_fixtures_sot$t1_2_1 + T1_fixtures_sot$t1_1_2 + T1_fixtures_sot$t1_2_2 + T1_fixtures_sot$t1_3_0 + T1_fixtures_sot$t1_3_1 +
#     T1_fixtures_sot$t1_3_2 + T1_fixtures_sot$t1_0_3 + T1_fixtures_sot$t1_1_3 + T1_fixtures_sot$t1_2_3 + T1_fixtures_sot$t1_3_3 +
#     T1_fixtures_sot$t1_4_0 + T1_fixtures_sot$t1_4_1 + T1_fixtures_sot$t1_4_2 + T1_fixtures_sot$t1_4_3 + T1_fixtures_sot$t1_0_4 +
#     T1_fixtures_sot$t1_1_4 + T1_fixtures_sot$t1_2_4 + T1_fixtures_sot$t1_3_4 + T1_fixtures_sot$t1_4_4 + T1_fixtures_sot$t1_5_0 +
#     T1_fixtures_sot$t1_5_1 + T1_fixtures_sot$t1_5_2 + T1_fixtures_sot$t1_5_3 + T1_fixtures_sot$t1_5_4 + T1_fixtures_sot$t1_0_5 +
#     T1_fixtures_sot$t1_1_5 + T1_fixtures_sot$t1_2_5 + T1_fixtures_sot$t1_3_5 + T1_fixtures_sot$t1_4_5 + T1_fixtures_sot$t1_5_5 +
#     T1_fixtures_sot$t1_6_0 + T1_fixtures_sot$t1_6_1 + T1_fixtures_sot$t1_6_2 + T1_fixtures_sot$t1_6_3 + T1_fixtures_sot$t1_6_4 +
#     T1_fixtures_sot$t1_6_5 + T1_fixtures_sot$t1_0_6 + T1_fixtures_sot$t1_1_6 + T1_fixtures_sot$t1_2_6 + T1_fixtures_sot$t1_3_6 +
#     T1_fixtures_sot$t1_4_6 + T1_fixtures_sot$t1_5_6 + T1_fixtures_sot$t1_6_6
# )
# #un25
# T1_fixtures_sot$t1_un25 <- (
#   T1_fixtures_sot$t1_0_0 + T1_fixtures_sot$t1_1_0 + T1_fixtures_sot$t1_0_1 + T1_fixtures_sot$t1_1_1 + T1_fixtures_sot$t1_2_0 + T1_fixtures_sot$t1_0_2
# )
# #odds
# T1_fixtures_sot$t1_ov25_odds <- round((1/T1_fixtures_sot$t1_ov25),digits = 2)
# T1_fixtures_sot$t1_un25_odds <- round((1/T1_fixtures_sot$t1_un25),digits = 2)
#
# T1_fixtures_sot$t1_ov25_odds
# T1_fixtures_sot$t1_un25_odds
# ###############################################################################
#
# ########Asian Handicaps######################################################################################################
# #percentages
# T1_fixtures_sot$t1_ov25 <- percent(T1_fixtures_sot$t1_ov25, accuracy = 0.1)
#
# T1_fixtures_sot$t1_un25 <- percent(T1_fixtures_sot$t1_un25, accuracy = 0.1)
# T1_fixtures_sot$t1_pssotre <- paste(round(T1_fixtures_sot$t1_xHST,digits = 0),round(T1_fixtures_sot$t1_xAST,digits = 0),sep = "-")


















