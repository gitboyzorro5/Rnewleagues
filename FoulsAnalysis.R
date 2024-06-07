library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
#Total fouls for
#MLS
#home fouls for
mls_home_fouls <- aggregate(MLS$HF, by = list(MLS$HomeTeam), FUN = sum)
mls_home_fouls_avg <- aggregate(MLS$HF, by = list(MLS$HomeTeam),mean)
mls_home_foulsdata <- merge(mls_home_fouls,mls_home_fouls_avg, by='Group.1',all = T)
names(mls_home_foulsdata)[names(mls_home_foulsdata) == "x.x"] <- "THfouls"
names(mls_home_foulsdata)[names(mls_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
mls_away_fouls <- aggregate(MLS$HF, by = list(MLS$AwayTeam), FUN = sum)
mls_away_fouls_avg <- aggregate(MLS$HF, by = list(MLS$AwayTeam),mean)
mls_away_foulsdata <- merge(mls_away_fouls,mls_away_fouls_avg, by='Group.1',all = T)
names(mls_away_foulsdata)[names(mls_away_foulsdata) == "x.x"] <- "TAfouls"
names(mls_away_foulsdata)[names(mls_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
mls_fouls <- merge(mls_home_foulsdata,mls_away_foulsdata,by='Group.1',all = T)
mls_fouls$TotalFouls <- mls_fouls$THfouls + mls_fouls$TAfouls

#yellow cards
mls_home_hyc <- aggregate(MLS$HY, by = list(MLS$HomeTeam), FUN = sum)
mls_away_ayc <- aggregate(MLS$AY, by = list(MLS$AwayTeam), FUN = sum)
mls_tyc <- merge(mls_home_hyc,mls_away_ayc, by='Group.1',all = T)
names(mls_tyc)[names(mls_tyc) == "x.x"] <- "hyc"
names(mls_tyc)[names(mls_tyc) == "x.y"] <- "ayc"
mls_tyc$TotalYellows <- mls_tyc$hyc + mls_tyc$ayc

#merge fouls for and yellow cards
mls_fouls_conversion <- merge(mls_tyc,mls_fouls,by='Group.1',all = T)
mls_fouls_conversion$YcPerfoul <- round((mls_fouls_conversion$TotalYellows/mls_fouls_conversion$TotalFouls), digits = 2)

###################################################################################################################################
#ARG
#home fouls for
arg_home_fouls <- aggregate(ARG$HF, by = list(ARG$HomeTeam), FUN = sum)
arg_home_fouls_avg <- aggregate(ARG$HF, by = list(ARG$HomeTeam),mean)
arg_home_foulsdata <- merge(arg_home_fouls,arg_home_fouls_avg, by='Group.1',all = T)
names(arg_home_foulsdata)[names(arg_home_foulsdata) == "x.x"] <- "THfouls"
names(arg_home_foulsdata)[names(arg_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
#away fouls for
arg_away_fouls <- aggregate(ARG$HF, by = list(ARG$AwayTeam), FUN = sum)
arg_away_fouls_avg <- aggregate(ARG$HF, by = list(ARG$AwayTeam),mean)
arg_away_foulsdata <- merge(arg_away_fouls,arg_away_fouls_avg, by='Group.1',all = T)
names(arg_away_foulsdata)[names(arg_away_foulsdata) == "x.x"] <- "TAfouls"
names(arg_away_foulsdata)[names(arg_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
#total fouls for
arg_fouls <- merge(arg_home_foulsdata,arg_away_foulsdata,by='Group.1',all = T)
arg_fouls$TotalFouls <- arg_fouls$THfouls + arg_fouls$TAfouls

#yellow cards
arg_home_hyc <- aggregate(ARG$HY, by = list(ARG$HomeTeam), FUN = sum)
arg_away_ayc <- aggregate(ARG$AY, by = list(ARG$AwayTeam), FUN = sum)
arg_tyc <- merge(arg_home_hyc,arg_away_ayc, by='Group.1',all = T)
names(arg_tyc)[names(arg_tyc) == "x.x"] <- "hyc"
names(arg_tyc)[names(arg_tyc) == "x.y"] <- "ayc"
arg_tyc$TotalYellows <- arg_tyc$hyc + arg_tyc$ayc

#merge fouls for and yellow cards
arg_fouls_conversion <- merge(arg_tyc,arg_fouls,by='Group.1',all = T)
arg_fouls_conversion$YcPerfoul <- round((arg_fouls_conversion$TotalYellows/arg_fouls_conversion$TotalFouls), digits = 2)
########################################################################################################################
#D2
#home fouls for
# d2_home_fouls <- aggregate(D2$HF, by = list(D2$HomeTeam), FUN = sum)
# d2_home_fouls_avg <- aggregate(D2$HF, by = list(D2$HomeTeam),mean)
# d2_home_foulsdata <- merge(d2_home_fouls,d2_home_fouls_avg, by='Group.1',all = T)
# names(d2_home_foulsdata)[names(d2_home_foulsdata) == "x.x"] <- "THfouls"
# names(d2_home_foulsdata)[names(d2_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# d2_away_fouls <- aggregate(D2$HF, by = list(D2$AwayTeam), FUN = sum)
# d2_away_fouls_avg <- aggregate(D2$HF, by = list(D2$AwayTeam),mean)
# d2_away_foulsdata <- merge(d2_away_fouls,d2_away_fouls_avg, by='Group.1',all = T)
# names(d2_away_foulsdata)[names(d2_away_foulsdata) == "x.x"] <- "TAfouls"
# names(d2_away_foulsdata)[names(d2_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# d2_fouls <- merge(d2_home_foulsdata,d2_away_foulsdata,by='Group.1',all = T)
# d2_fouls$TotalFouls <- d2_fouls$THfouls + d2_fouls$TAfouls
#
# #yellow cards
# d2_home_hyc <- aggregate(D2$HY, by = list(D2$HomeTeam), FUN = sum)
# d2_away_ayc <- aggregate(D2$AY, by = list(D2$AwayTeam), FUN = sum)
# d2_tyc <- merge(d2_home_hyc,d2_away_ayc, by='Group.1',all = T)
# names(d2_tyc)[names(d2_tyc) == "x.x"] <- "hyc"
# names(d2_tyc)[names(d2_tyc) == "x.y"] <- "ayc"
# d2_tyc$TotalYellows <- d2_tyc$hyc + d2_tyc$ayc
#
# #merge fouls for and yellow cards
# d2_fouls_conversion <- merge(d2_tyc,d2_fouls,by='Group.1',all = T)
# d2_fouls_conversion$YcPerfoul <- round((d2_fouls_conversion$TotalYellows/d2_fouls_conversion$TotalFouls), digits = 2)
# ############################################################################################################################
# #E0
# #home fouls for
# e0_home_fouls <- aggregate(E0$HF, by = list(E0$HomeTeam), FUN = sum)
# e0_home_fouls_avg <- aggregate(E0$HF, by = list(E0$HomeTeam),mean)
# e0_home_foulsdata <- merge(e0_home_fouls,e0_home_fouls_avg, by='Group.1',all = T)
# names(e0_home_foulsdata)[names(e0_home_foulsdata) == "x.x"] <- "THfouls"
# names(e0_home_foulsdata)[names(e0_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# e0_away_fouls <- aggregate(E0$HF, by = list(E0$AwayTeam), FUN = sum)
# e0_away_fouls_avg <- aggregate(E0$HF, by = list(E0$AwayTeam),mean)
# e0_away_foulsdata <- merge(e0_away_fouls,e0_away_fouls_avg, by='Group.1',all = T)
# names(e0_away_foulsdata)[names(e0_away_foulsdata) == "x.x"] <- "TAfouls"
# names(e0_away_foulsdata)[names(e0_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# e0_fouls <- merge(e0_home_foulsdata,e0_away_foulsdata,by='Group.1',all = T)
# e0_fouls$TotalFouls <- e0_fouls$THfouls + e0_fouls$TAfouls
#
# #yellow cards
# e0_home_hyc <- aggregate(E0$HY, by = list(E0$HomeTeam), FUN = sum)
# e0_away_ayc <- aggregate(E0$AY, by = list(E0$AwayTeam), FUN = sum)
# e0_tyc <- merge(e0_home_hyc,e0_away_ayc, by='Group.1',all = T)
# names(e0_tyc)[names(e0_tyc) == "x.x"] <- "hyc"
# names(e0_tyc)[names(e0_tyc) == "x.y"] <- "ayc"
# e0_tyc$TotalYellows <- e0_tyc$hyc + e0_tyc$ayc
#
# #merge fouls for and yellow cards
# e0_fouls_conversion <- merge(e0_tyc,e0_fouls,by='Group.1',all = T)
# e0_fouls_conversion$YcPerfoul <- round((e0_fouls_conversion$TotalYellows/e0_fouls_conversion$TotalFouls), digits = 2)
# #######################################################################################################################
# #E1
# #home fouls for
# e1_home_fouls <- aggregate(E1$HF, by = list(E1$HomeTeam), FUN = sum)
# e1_home_fouls_avg <- aggregate(E1$HF, by = list(E1$HomeTeam),mean)
# e1_home_foulsdata <- merge(e1_home_fouls,e1_home_fouls_avg, by='Group.1',all = T)
# names(e1_home_foulsdata)[names(e1_home_foulsdata) == "x.x"] <- "THfouls"
# names(e1_home_foulsdata)[names(e1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# e1_away_fouls <- aggregate(E1$HF, by = list(E1$AwayTeam), FUN = sum)
# e1_away_fouls_avg <- aggregate(E1$HF, by = list(E1$AwayTeam),mean)
# e1_away_foulsdata <- merge(e1_away_fouls,e1_away_fouls_avg, by='Group.1',all = T)
# names(e1_away_foulsdata)[names(e1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(e1_away_foulsdata)[names(e1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# e1_fouls <- merge(e1_home_foulsdata,e1_away_foulsdata,by='Group.1',all = T)
# e1_fouls$TotalFouls <- e1_fouls$THfouls + e1_fouls$TAfouls
#
# #yellow cards
# e1_home_hyc <- aggregate(E1$HY, by = list(E1$HomeTeam), FUN = sum)
# e1_away_ayc <- aggregate(E1$AY, by = list(E1$AwayTeam), FUN = sum)
# e1_tyc <- merge(e1_home_hyc,e1_away_ayc, by='Group.1',all = T)
# names(e1_tyc)[names(e1_tyc) == "x.x"] <- "hyc"
# names(e1_tyc)[names(e1_tyc) == "x.y"] <- "ayc"
# e1_tyc$TotalYellows <- e1_tyc$hyc + e1_tyc$ayc
#
# #merge fouls for and yellow cards
# e1_fouls_conversion <- merge(e1_tyc,e1_fouls,by='Group.1',all = T)
# e1_fouls_conversion$YcPerfoul <- round((e1_fouls_conversion$TotalYellows/e1_fouls_conversion$TotalFouls), digits = 2)
# #############################################################################################################################
# #E2
# #home fouls for
# e2_home_fouls <- aggregate(E2$HF, by = list(E2$HomeTeam), FUN = sum)
# e2_home_fouls_avg <- aggregate(E2$HF, by = list(E2$HomeTeam),mean)
# e2_home_foulsdata <- merge(e2_home_fouls,e2_home_fouls_avg, by='Group.1',all = T)
# names(e2_home_foulsdata)[names(e2_home_foulsdata) == "x.x"] <- "THfouls"
# names(e2_home_foulsdata)[names(e2_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# e2_away_fouls <- aggregate(E2$HF, by = list(E2$AwayTeam), FUN = sum)
# e2_away_fouls_avg <- aggregate(E2$HF, by = list(E2$AwayTeam),mean)
# e2_away_foulsdata <- merge(e2_away_fouls,e2_away_fouls_avg, by='Group.1',all = T)
# names(e2_away_foulsdata)[names(e2_away_foulsdata) == "x.x"] <- "TAfouls"
# names(e2_away_foulsdata)[names(e2_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# e2_fouls <- merge(e2_home_foulsdata,e2_away_foulsdata,by='Group.1',all = T)
# e2_fouls$TotalFouls <- e2_fouls$THfouls + e2_fouls$TAfouls
#
# #yellow cards
# e2_home_hyc <- aggregate(E2$HY, by = list(E2$HomeTeam), FUN = sum)
# e2_away_ayc <- aggregate(E2$AY, by = list(E2$AwayTeam), FUN = sum)
# e2_tyc <- merge(e2_home_hyc,e2_away_ayc, by='Group.1',all = T)
# names(e2_tyc)[names(e2_tyc) == "x.x"] <- "hyc"
# names(e2_tyc)[names(e2_tyc) == "x.y"] <- "ayc"
# e2_tyc$TotalYellows <- e2_tyc$hyc + e2_tyc$ayc
#
# #merge fouls for and yellow cards
# e2_fouls_conversion <- merge(e2_tyc,e2_fouls,by='Group.1',all = T)
# e2_fouls_conversion$YcPerfoul <- round((e2_fouls_conversion$TotalYellows/e2_fouls_conversion$TotalFouls), digits = 2)
# #######################################################################################################################
# #E3
# #home fouls for
# e3_home_fouls <- aggregate(E3$HF, by = list(E3$HomeTeam), FUN = sum)
# e3_home_fouls_avg <- aggregate(E3$HF, by = list(E3$HomeTeam),mean)
# e3_home_foulsdata <- merge(e3_home_fouls,e3_home_fouls_avg, by='Group.1',all = T)
# names(e3_home_foulsdata)[names(e3_home_foulsdata) == "x.x"] <- "THfouls"
# names(e3_home_foulsdata)[names(e3_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# e3_away_fouls <- aggregate(E3$HF, by = list(E3$AwayTeam), FUN = sum)
# e3_away_fouls_avg <- aggregate(E3$HF, by = list(E3$AwayTeam),mean)
# e3_away_foulsdata <- merge(e3_away_fouls,e3_away_fouls_avg, by='Group.1',all = T)
# names(e3_away_foulsdata)[names(e3_away_foulsdata) == "x.x"] <- "TAfouls"
# names(e3_away_foulsdata)[names(e3_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# e3_fouls <- merge(e3_home_foulsdata,e3_away_foulsdata,by='Group.1',all = T)
# e3_fouls$TotalFouls <- e3_fouls$THfouls + e3_fouls$TAfouls
#
# #yellow cards
# e3_home_hyc <- aggregate(E3$HY, by = list(E3$HomeTeam), FUN = sum)
# e3_away_ayc <- aggregate(E3$AY, by = list(E3$AwayTeam), FUN = sum)
# e3_tyc <- merge(e3_home_hyc,e3_away_ayc, by='Group.1',all = T)
# names(e3_tyc)[names(e3_tyc) == "x.x"] <- "hyc"
# names(e3_tyc)[names(e3_tyc) == "x.y"] <- "ayc"
# e3_tyc$TotalYellows <- e3_tyc$hyc + e3_tyc$ayc
#
# #merge fouls for and yellow cards
# e3_fouls_conversion <- merge(e3_tyc,e3_fouls,by='Group.1',all = T)
# e3_fouls_conversion$YcPerfoul <- round((e3_fouls_conversion$TotalYellows/e3_fouls_conversion$TotalFouls), digits = 2)
# ########################################################################################################################
# #EC
# #home fouls for
# ec_home_fouls <- aggregate(EC$HF, by = list(EC$HomeTeam), FUN = sum)
# ec_home_fouls_avg <- aggregate(EC$HF, by = list(EC$HomeTeam),mean)
# ec_home_foulsdata <- merge(ec_home_fouls,ec_home_fouls_avg, by='Group.1',all = T)
# names(ec_home_foulsdata)[names(ec_home_foulsdata) == "x.x"] <- "THfouls"
# names(ec_home_foulsdata)[names(ec_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# ec_away_fouls <- aggregate(EC$HF, by = list(EC$AwayTeam), FUN = sum)
# ec_away_fouls_avg <- aggregate(EC$HF, by = list(EC$AwayTeam),mean)
# ec_away_foulsdata <- merge(ec_away_fouls,ec_away_fouls_avg, by='Group.1',all = T)
# names(ec_away_foulsdata)[names(ec_away_foulsdata) == "x.x"] <- "TAfouls"
# names(ec_away_foulsdata)[names(ec_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# ec_fouls <- merge(ec_home_foulsdata,ec_away_foulsdata,by='Group.1',all = T)
# ec_fouls$TotalFouls <- ec_fouls$THfouls + ec_fouls$TAfouls
#
# #yellow cards
# ec_home_hyc <- aggregate(EC$HY, by = list(EC$HomeTeam), FUN = sum)
# ec_away_ayc <- aggregate(EC$AY, by = list(EC$AwayTeam), FUN = sum)
# ec_tyc <- merge(ec_home_hyc,ec_away_ayc, by='Group.1',all = T)
# names(ec_tyc)[names(ec_tyc) == "x.x"] <- "hyc"
# names(ec_tyc)[names(ec_tyc) == "x.y"] <- "ayc"
# ec_tyc$TotalYellows <- ec_tyc$hyc + ec_tyc$ayc
#
# #merge fouls for and yellow cards
# ec_fouls_conversion <- merge(ec_tyc,ec_fouls,by='Group.1',all = T)
# ec_fouls_conversion$YcPerfoul <- round((ec_fouls_conversion$TotalYellows/ec_fouls_conversion$TotalFouls), digits = 2)
# ##################################################################################################################################
# #F1
# #home fouls for
# f1_home_fouls <- aggregate(F1$HF, by = list(F1$HomeTeam), FUN = sum)
# f1_home_fouls_avg <- aggregate(F1$HF, by = list(F1$HomeTeam),mean)
# f1_home_foulsdata <- merge(f1_home_fouls,f1_home_fouls_avg, by='Group.1',all = T)
# names(f1_home_foulsdata)[names(f1_home_foulsdata) == "x.x"] <- "THfouls"
# names(f1_home_foulsdata)[names(f1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# f1_away_fouls <- aggregate(F1$HF, by = list(F1$AwayTeam), FUN = sum)
# f1_away_fouls_avg <- aggregate(F1$HF, by = list(F1$AwayTeam),mean)
# f1_away_foulsdata <- merge(f1_away_fouls,f1_away_fouls_avg, by='Group.1',all = T)
# names(f1_away_foulsdata)[names(f1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(f1_away_foulsdata)[names(f1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# f1_fouls <- merge(f1_home_foulsdata,f1_away_foulsdata,by='Group.1',all = T)
# f1_fouls$TotalFouls <- f1_fouls$THfouls + f1_fouls$TAfouls
#
# #yellow cards
# f1_home_hyc <- aggregate(F1$HY, by = list(F1$HomeTeam), FUN = sum)
# f1_away_ayc <- aggregate(F1$AY, by = list(F1$AwayTeam), FUN = sum)
# f1_tyc <- merge(f1_home_hyc,f1_away_ayc, by='Group.1',all = T)
# names(f1_tyc)[names(f1_tyc) == "x.x"] <- "hyc"
# names(f1_tyc)[names(f1_tyc) == "x.y"] <- "ayc"
# f1_tyc$TotalYellows <- f1_tyc$hyc + f1_tyc$ayc
#
# #merge fouls for and yellow cards
# f1_fouls_conversion <- merge(f1_tyc,f1_fouls,by='Group.1',all = T)
# f1_fouls_conversion$YcPerfoul <- round((f1_fouls_conversion$TotalYellows/f1_fouls_conversion$TotalFouls), digits = 2)
# ########################################################################################################################################
# #F2
# #home fouls for
# f2_home_fouls <- aggregate(F2$HF, by = list(F2$HomeTeam), FUN = sum)
# f2_home_fouls_avg <- aggregate(F2$HF, by = list(F2$HomeTeam),mean)
# f2_home_foulsdata <- merge(f2_home_fouls,f2_home_fouls_avg, by='Group.1',all = T)
# names(f2_home_foulsdata)[names(f2_home_foulsdata) == "x.x"] <- "THfouls"
# names(f2_home_foulsdata)[names(f2_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# f2_away_fouls <- aggregate(F2$HF, by = list(F2$AwayTeam), FUN = sum)
# f2_away_fouls_avg <- aggregate(F2$HF, by = list(F2$AwayTeam),mean)
# f2_away_foulsdata <- merge(f2_away_fouls,f2_away_fouls_avg, by='Group.1',all = T)
# names(f2_away_foulsdata)[names(f2_away_foulsdata) == "x.x"] <- "TAfouls"
# names(f2_away_foulsdata)[names(f2_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# f2_fouls <- merge(f2_home_foulsdata,f2_away_foulsdata,by='Group.1',all = T)
# f2_fouls$TotalFouls <- f2_fouls$THfouls + f2_fouls$TAfouls
#
# #yellow cards
# f2_home_hyc <- aggregate(F2$HY, by = list(F2$HomeTeam), FUN = sum)
# f2_away_ayc <- aggregate(F2$AY, by = list(F2$AwayTeam), FUN = sum)
# f2_tyc <- merge(f2_home_hyc,f2_away_ayc, by='Group.1',all = T)
# names(f2_tyc)[names(f2_tyc) == "x.x"] <- "hyc"
# names(f2_tyc)[names(f2_tyc) == "x.y"] <- "ayc"
# f2_tyc$TotalYellows <- f2_tyc$hyc + f2_tyc$ayc
#
# #merge fouls for and yellow cards
# f2_fouls_conversion <- merge(f2_tyc,f2_fouls,by='Group.1',all = T)
# f2_fouls_conversion$YcPerfoul <- round((f2_fouls_conversion$TotalYellows/f2_fouls_conversion$TotalFouls), digits = 2)
# #######################################################################################################################
# #G1
# #home fouls for
# g1_home_fouls <- aggregate(G1$HF, by = list(G1$HomeTeam), FUN = sum)
# g1_home_fouls_avg <- aggregate(G1$HF, by = list(G1$HomeTeam),mean)
# g1_home_foulsdata <- merge(g1_home_fouls,g1_home_fouls_avg, by='Group.1',all = T)
# names(g1_home_foulsdata)[names(g1_home_foulsdata) == "x.x"] <- "THfouls"
# names(g1_home_foulsdata)[names(g1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# g1_away_fouls <- aggregate(G1$HF, by = list(G1$AwayTeam), FUN = sum)
# g1_away_fouls_avg <- aggregate(G1$HF, by = list(G1$AwayTeam),mean)
# g1_away_foulsdata <- merge(g1_away_fouls,g1_away_fouls_avg, by='Group.1',all = T)
# names(g1_away_foulsdata)[names(g1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(g1_away_foulsdata)[names(g1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# g1_fouls <- merge(g1_home_foulsdata,g1_away_foulsdata,by='Group.1',all = T)
# g1_fouls$TotalFouls <- g1_fouls$THfouls + g1_fouls$TAfouls
#
# #yellow cards
# g1_home_hyc <- aggregate(G1$HY, by = list(G1$HomeTeam), FUN = sum)
# g1_away_ayc <- aggregate(G1$AY, by = list(G1$AwayTeam), FUN = sum)
# g1_tyc <- merge(g1_home_hyc,g1_away_ayc, by='Group.1',all = T)
# names(g1_tyc)[names(g1_tyc) == "x.x"] <- "hyc"
# names(g1_tyc)[names(g1_tyc) == "x.y"] <- "ayc"
# g1_tyc$TotalYellows <- g1_tyc$hyc + g1_tyc$ayc
#
# #merge fouls for and yellow cards
# g1_fouls_conversion <- merge(g1_tyc,g1_fouls,by='Group.1',all = T)
# g1_fouls_conversion$YcPerfoul <- round((g1_fouls_conversion$TotalYellows/g1_fouls_conversion$TotalFouls), digits = 2)
# #########################################################################################################################
# #I1
# #home fouls for
# i1_home_fouls <- aggregate(I1$HF, by = list(I1$HomeTeam), FUN = sum)
# i1_home_fouls_avg <- aggregate(I1$HF, by = list(I1$HomeTeam),mean)
# i1_home_foulsdata <- merge(i1_home_fouls,i1_home_fouls_avg, by='Group.1',all = T)
# names(i1_home_foulsdata)[names(i1_home_foulsdata) == "x.x"] <- "THfouls"
# names(i1_home_foulsdata)[names(i1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# i1_away_fouls <- aggregate(I1$HF, by = list(I1$AwayTeam), FUN = sum)
# i1_away_fouls_avg <- aggregate(I1$HF, by = list(I1$AwayTeam),mean)
# i1_away_foulsdata <- merge(i1_away_fouls,i1_away_fouls_avg, by='Group.1',all = T)
# names(i1_away_foulsdata)[names(i1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(i1_away_foulsdata)[names(i1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# i1_fouls <- merge(i1_home_foulsdata,i1_away_foulsdata,by='Group.1',all = T)
# i1_fouls$TotalFouls <- i1_fouls$THfouls + i1_fouls$TAfouls
#
# #yellow cards
# i1_home_hyc <- aggregate(I1$HY, by = list(I1$HomeTeam), FUN = sum)
# i1_away_ayc <- aggregate(I1$AY, by = list(I1$AwayTeam), FUN = sum)
# i1_tyc <- merge(i1_home_hyc,i1_away_ayc, by='Group.1',all = T)
# names(i1_tyc)[names(i1_tyc) == "x.x"] <- "hyc"
# names(i1_tyc)[names(i1_tyc) == "x.y"] <- "ayc"
# i1_tyc$TotalYellows <- i1_tyc$hyc + i1_tyc$ayc
#
# #merge fouls for and yellow cards
# i1_fouls_conversion <- merge(i1_tyc,i1_fouls,by='Group.1',all = T)
# i1_fouls_conversion$YcPerfoul <- round((i1_fouls_conversion$TotalYellows/i1_fouls_conversion$TotalFouls), digits = 2)
# #######################################################################################################################
# #I2
# #home fouls for
# i2_home_fouls <- aggregate(I2$HF, by = list(I2$HomeTeam), FUN = sum)
# i2_home_fouls_avg <- aggregate(I2$HF, by = list(I2$HomeTeam),mean)
# i2_home_foulsdata <- merge(i2_home_fouls,i2_home_fouls_avg, by='Group.1',all = T)
# names(i2_home_foulsdata)[names(i2_home_foulsdata) == "x.x"] <- "THfouls"
# names(i2_home_foulsdata)[names(i2_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# i2_away_fouls <- aggregate(I2$HF, by = list(I2$AwayTeam), FUN = sum)
# i2_away_fouls_avg <- aggregate(I2$HF, by = list(I2$AwayTeam),mean)
# i2_away_foulsdata <- merge(i2_away_fouls,i2_away_fouls_avg, by='Group.1',all = T)
# names(i2_away_foulsdata)[names(i2_away_foulsdata) == "x.x"] <- "TAfouls"
# names(i2_away_foulsdata)[names(i2_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# i2_fouls <- merge(i2_home_foulsdata,i2_away_foulsdata,by='Group.1',all = T)
# i2_fouls$TotalFouls <- i2_fouls$THfouls + i2_fouls$TAfouls
#
# #yellow cards
# i2_home_hyc <- aggregate(I2$HY, by = list(I2$HomeTeam), FUN = sum)
# i2_away_ayc <- aggregate(I2$AY, by = list(I2$AwayTeam), FUN = sum)
# i2_tyc <- merge(i2_home_hyc,i2_away_ayc, by='Group.1',all = T)
# names(i2_tyc)[names(i2_tyc) == "x.x"] <- "hyc"
# names(i2_tyc)[names(i2_tyc) == "x.y"] <- "ayc"
# i2_tyc$TotalYellows <- i2_tyc$hyc + i2_tyc$ayc
#
# #merge fouls for and yellow cards
# i2_fouls_conversion <- merge(i2_tyc,i2_fouls,by='Group.1',all = T)
# i2_fouls_conversion$YcPerfoul <- round((i2_fouls_conversion$TotalYellows/i2_fouls_conversion$TotalFouls), digits = 2)
# ############################################################################################################################
# #N1
# #home fouls for
# n1_home_fouls <- aggregate(N1$HF, by = list(N1$HomeTeam), FUN = sum)
# n1_home_fouls_avg <- aggregate(N1$HF, by = list(N1$HomeTeam),mean)
# n1_home_foulsdata <- merge(n1_home_fouls,n1_home_fouls_avg, by='Group.1',all = T)
# names(n1_home_foulsdata)[names(n1_home_foulsdata) == "x.x"] <- "THfouls"
# names(n1_home_foulsdata)[names(n1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# n1_away_fouls <- aggregate(N1$HF, by = list(N1$AwayTeam), FUN = sum)
# n1_away_fouls_avg <- aggregate(N1$HF, by = list(N1$AwayTeam),mean)
# n1_away_foulsdata <- merge(n1_away_fouls,n1_away_fouls_avg, by='Group.1',all = T)
# names(n1_away_foulsdata)[names(n1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(n1_away_foulsdata)[names(n1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# n1_fouls <- merge(n1_home_foulsdata,n1_away_foulsdata,by='Group.1',all = T)
# n1_fouls$TotalFouls <- n1_fouls$THfouls + n1_fouls$TAfouls
#
# #yellow cards
# n1_home_hyc <- aggregate(N1$HY, by = list(N1$HomeTeam), FUN = sum)
# n1_away_ayc <- aggregate(N1$AY, by = list(N1$AwayTeam), FUN = sum)
# n1_tyc <- merge(n1_home_hyc,n1_away_ayc, by='Group.1',all = T)
# names(n1_tyc)[names(n1_tyc) == "x.x"] <- "hyc"
# names(n1_tyc)[names(n1_tyc) == "x.y"] <- "ayc"
# n1_tyc$TotalYellows <- n1_tyc$hyc + n1_tyc$ayc
#
# #merge fouls for and yellow cards
# n1_fouls_conversion <- merge(n1_tyc,n1_fouls,by='Group.1',all = T)
# n1_fouls_conversion$YcPerfoul <- round((n1_fouls_conversion$TotalYellows/n1_fouls_conversion$TotalFouls), digits = 2)
# ######################################################################################################################
# #P1
# #home fouls for
# p1_home_fouls <- aggregate(P1$HF, by = list(P1$HomeTeam), FUN = sum)
# p1_home_fouls_avg <- aggregate(P1$HF, by = list(P1$HomeTeam),mean)
# p1_home_foulsdata <- merge(p1_home_fouls,p1_home_fouls_avg, by='Group.1',all = T)
# names(p1_home_foulsdata)[names(p1_home_foulsdata) == "x.x"] <- "THfouls"
# names(p1_home_foulsdata)[names(p1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# p1_away_fouls <- aggregate(P1$HF, by = list(P1$AwayTeam), FUN = sum)
# p1_away_fouls_avg <- aggregate(P1$HF, by = list(P1$AwayTeam),mean)
# p1_away_foulsdata <- merge(p1_away_fouls,p1_away_fouls_avg, by='Group.1',all = T)
# names(p1_away_foulsdata)[names(p1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(p1_away_foulsdata)[names(p1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# p1_fouls <- merge(p1_home_foulsdata,p1_away_foulsdata,by='Group.1',all = T)
# p1_fouls$TotalFouls <- p1_fouls$THfouls + p1_fouls$TAfouls
#
# #yellow cards
# p1_home_hyc <- aggregate(P1$HY, by = list(P1$HomeTeam), FUN = sum)
# p1_away_ayc <- aggregate(P1$AY, by = list(P1$AwayTeam), FUN = sum)
# p1_tyc <- merge(p1_home_hyc,p1_away_ayc, by='Group.1',all = T)
# names(p1_tyc)[names(p1_tyc) == "x.x"] <- "hyc"
# names(p1_tyc)[names(p1_tyc) == "x.y"] <- "ayc"
# p1_tyc$TotalYellows <- p1_tyc$hyc + p1_tyc$ayc
#
# #merge fouls for and yellow cards
# p1_fouls_conversion <- merge(p1_tyc,p1_fouls,by='Group.1',all = T)
# p1_fouls_conversion$YcPerfoul <- round((p1_fouls_conversion$TotalYellows/p1_fouls_conversion$TotalFouls), digits = 2)
# #########################################################################################################################
# #SC0
# #home fouls for
# sc0_home_fouls <- aggregate(SC0$HF, by = list(SC0$HomeTeam), FUN = sum)
# sc0_home_fouls_avg <- aggregate(SC0$HF, by = list(SC0$HomeTeam),mean)
# sc0_home_foulsdata <- merge(sc0_home_fouls,sc0_home_fouls_avg, by='Group.1',all = T)
# names(sc0_home_foulsdata)[names(sc0_home_foulsdata) == "x.x"] <- "THfouls"
# names(sc0_home_foulsdata)[names(sc0_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# sc0_away_fouls <- aggregate(SC0$HF, by = list(SC0$AwayTeam), FUN = sum)
# sc0_away_fouls_avg <- aggregate(SC0$HF, by = list(SC0$AwayTeam),mean)
# sc0_away_foulsdata <- merge(sc0_away_fouls,sc0_away_fouls_avg, by='Group.1',all = T)
# names(sc0_away_foulsdata)[names(sc0_away_foulsdata) == "x.x"] <- "TAfouls"
# names(sc0_away_foulsdata)[names(sc0_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# sc0_fouls <- merge(sc0_home_foulsdata,sc0_away_foulsdata,by='Group.1',all = T)
# sc0_fouls$TotalFouls <- sc0_fouls$THfouls + sc0_fouls$TAfouls
#
# #yellow cards
# sc0_home_hyc <- aggregate(SC0$HY, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_ayc <- aggregate(SC0$AY, by = list(SC0$AwayTeam), FUN = sum)
# sc0_tyc <- merge(sc0_home_hyc,sc0_away_ayc, by='Group.1',all = T)
# names(sc0_tyc)[names(sc0_tyc) == "x.x"] <- "hyc"
# names(sc0_tyc)[names(sc0_tyc) == "x.y"] <- "ayc"
# sc0_tyc$TotalYellows <- sc0_tyc$hyc + sc0_tyc$ayc
#
# #merge fouls for and yellow cards
# sc0_fouls_conversion <- merge(sc0_tyc,sc0_fouls,by='Group.1',all = T)
# sc0_fouls_conversion$YcPerfoul <- round((sc0_fouls_conversion$TotalYellows/sc0_fouls_conversion$TotalFouls), digits = 2)
# ###########################################################################################################################
# #SC1
# #home fouls for
# sc1_home_fouls <- aggregate(SC1$HF, by = list(SC1$HomeTeam), FUN = sum)
# sc1_home_fouls_avg <- aggregate(SC1$HF, by = list(SC1$HomeTeam),mean)
# sc1_home_foulsdata <- merge(sc1_home_fouls,sc1_home_fouls_avg, by='Group.1',all = T)
# names(sc1_home_foulsdata)[names(sc1_home_foulsdata) == "x.x"] <- "THfouls"
# names(sc1_home_foulsdata)[names(sc1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# sc1_away_fouls <- aggregate(SC1$HF, by = list(SC1$AwayTeam), FUN = sum)
# sc1_away_fouls_avg <- aggregate(SC1$HF, by = list(SC1$AwayTeam),mean)
# sc1_away_foulsdata <- merge(sc1_away_fouls,sc1_away_fouls_avg, by='Group.1',all = T)
# names(sc1_away_foulsdata)[names(sc1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(sc1_away_foulsdata)[names(sc1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# sc1_fouls <- merge(sc1_home_foulsdata,sc1_away_foulsdata,by='Group.1',all = T)
# sc1_fouls$TotalFouls <- sc1_fouls$THfouls + sc1_fouls$TAfouls
#
# #yellow cards
# sc1_home_hyc <- aggregate(SC1$HY, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_ayc <- aggregate(SC1$AY, by = list(SC1$AwayTeam), FUN = sum)
# sc1_tyc <- merge(sc1_home_hyc,sc1_away_ayc, by='Group.1',all = T)
# names(sc1_tyc)[names(sc1_tyc) == "x.x"] <- "hyc"
# names(sc1_tyc)[names(sc1_tyc) == "x.y"] <- "ayc"
# sc1_tyc$TotalYellows <- sc1_tyc$hyc + sc1_tyc$ayc
#
# #merge fouls for and yellow cards
# sc1_fouls_conversion <- merge(sc1_tyc,sc1_fouls,by='Group.1',all = T)
# sc1_fouls_conversion$YcPerfoul <- round((sc1_fouls_conversion$TotalYellows/sc1_fouls_conversion$TotalFouls), digits = 2)
# ##########################################################################################################################
# #SC2
# #home fouls for
# sc2_home_fouls <- aggregate(SC2$HF, by = list(SC2$HomeTeam), FUN = sum)
# sc2_home_fouls_avg <- aggregate(SC2$HF, by = list(SC2$HomeTeam),mean)
# sc2_home_foulsdata <- merge(sc2_home_fouls,sc2_home_fouls_avg, by='Group.1',all = T)
# names(sc2_home_foulsdata)[names(sc2_home_foulsdata) == "x.x"] <- "THfouls"
# names(sc2_home_foulsdata)[names(sc2_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# sc2_away_fouls <- aggregate(SC2$HF, by = list(SC2$AwayTeam), FUN = sum)
# sc2_away_fouls_avg <- aggregate(SC2$HF, by = list(SC2$AwayTeam),mean)
# sc2_away_foulsdata <- merge(sc2_away_fouls,sc2_away_fouls_avg, by='Group.1',all = T)
# names(sc2_away_foulsdata)[names(sc2_away_foulsdata) == "x.x"] <- "TAfouls"
# names(sc2_away_foulsdata)[names(sc2_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# sc2_fouls <- merge(sc2_home_foulsdata,sc2_away_foulsdata,by='Group.1',all = T)
# sc2_fouls$TotalFouls <- sc2_fouls$THfouls + sc2_fouls$TAfouls
#
# #yellow cards
# sc2_home_hyc <- aggregate(SC2$HY, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_ayc <- aggregate(SC2$AY, by = list(SC2$AwayTeam), FUN = sum)
# sc2_tyc <- merge(sc2_home_hyc,sc2_away_ayc, by='Group.1',all = T)
# names(sc2_tyc)[names(sc2_tyc) == "x.x"] <- "hyc"
# names(sc2_tyc)[names(sc2_tyc) == "x.y"] <- "ayc"
# sc2_tyc$TotalYellows <- sc2_tyc$hyc + sc2_tyc$ayc
#
# #merge fouls for and yellow cards
# sc2_fouls_conversion <- merge(sc2_tyc,sc2_fouls,by='Group.1',all = T)
# sc2_fouls_conversion$YcPerfoul <- round((sc2_fouls_conversion$TotalYellows/sc2_fouls_conversion$TotalFouls), digits = 2)
# ###########################################################################################################################
# #SC3
# #home fouls for
# sc3_home_fouls <- aggregate(SC3$HF, by = list(SC3$HomeTeam), FUN = sum)
# sc3_home_fouls_avg <- aggregate(SC3$HF, by = list(SC3$HomeTeam),mean)
# sc3_home_foulsdata <- merge(sc3_home_fouls,sc3_home_fouls_avg, by='Group.1',all = T)
# names(sc3_home_foulsdata)[names(sc3_home_foulsdata) == "x.x"] <- "THfouls"
# names(sc3_home_foulsdata)[names(sc3_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# sc3_away_fouls <- aggregate(SC3$HF, by = list(SC3$AwayTeam), FUN = sum)
# sc3_away_fouls_avg <- aggregate(SC3$HF, by = list(SC3$AwayTeam),mean)
# sc3_away_foulsdata <- merge(sc3_away_fouls,sc3_away_fouls_avg, by='Group.1',all = T)
# names(sc3_away_foulsdata)[names(sc3_away_foulsdata) == "x.x"] <- "TAfouls"
# names(sc3_away_foulsdata)[names(sc3_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# sc3_fouls <- merge(sc3_home_foulsdata,sc3_away_foulsdata,by='Group.1',all = T)
# sc3_fouls$TotalFouls <- sc3_fouls$THfouls + sc3_fouls$TAfouls
#
# #yellow cards
# sc3_home_hyc <- aggregate(SC3$HY, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_ayc <- aggregate(SC3$AY, by = list(SC3$AwayTeam), FUN = sum)
# sc3_tyc <- merge(sc3_home_hyc,sc3_away_ayc, by='Group.1',all = T)
# names(sc3_tyc)[names(sc3_tyc) == "x.x"] <- "hyc"
# names(sc3_tyc)[names(sc3_tyc) == "x.y"] <- "ayc"
# sc3_tyc$TotalYellows <- sc3_tyc$hyc + sc3_tyc$ayc
#
# #merge fouls for and yellow cards
# sc3_fouls_conversion <- merge(sc3_tyc,sc3_fouls,by='Group.1',all = T)
# sc3_fouls_conversion$YcPerfoul <- round((sc3_fouls_conversion$TotalYellows/sc3_fouls_conversion$TotalFouls), digits = 2)
# ##########################################################################################################################
# #SP1
# #home fouls for
# sp1_home_fouls <- aggregate(SP1$HF, by = list(SP1$HomeTeam), FUN = sum)
# sp1_home_fouls_avg <- aggregate(SP1$HF, by = list(SP1$HomeTeam),mean)
# sp1_home_foulsdata <- merge(sp1_home_fouls,sp1_home_fouls_avg, by='Group.1',all = T)
# names(sp1_home_foulsdata)[names(sp1_home_foulsdata) == "x.x"] <- "THfouls"
# names(sp1_home_foulsdata)[names(sp1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# sp1_away_fouls <- aggregate(SP1$HF, by = list(SP1$AwayTeam), FUN = sum)
# sp1_away_fouls_avg <- aggregate(SP1$HF, by = list(SP1$AwayTeam),mean)
# sp1_away_foulsdata <- merge(sp1_away_fouls,sp1_away_fouls_avg, by='Group.1',all = T)
# names(sp1_away_foulsdata)[names(sp1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(sp1_away_foulsdata)[names(sp1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# sp1_fouls <- merge(sp1_home_foulsdata,sp1_away_foulsdata,by='Group.1',all = T)
# sp1_fouls$TotalFouls <- sp1_fouls$THfouls + sp1_fouls$TAfouls
#
# #yellow cards
# sp1_home_hyc <- aggregate(SP1$HY, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_ayc <- aggregate(SP1$AY, by = list(SP1$AwayTeam), FUN = sum)
# sp1_tyc <- merge(sp1_home_hyc,sp1_away_ayc, by='Group.1',all = T)
# names(sp1_tyc)[names(sp1_tyc) == "x.x"] <- "hyc"
# names(sp1_tyc)[names(sp1_tyc) == "x.y"] <- "ayc"
# sp1_tyc$TotalYellows <- sp1_tyc$hyc + sp1_tyc$ayc
#
# #merge fouls for and yellow cards
# sp1_fouls_conversion <- merge(sp1_tyc,sp1_fouls,by='Group.1',all = T)
# sp1_fouls_conversion$YcPerfoul <- round((sp1_fouls_conversion$TotalYellows/sp1_fouls_conversion$TotalFouls), digits = 2)
# ############################################################################################################################
# #SP2
# #home fouls for
# sp2_home_fouls <- aggregate(SP2$HF, by = list(SP2$HomeTeam), FUN = sum)
# sp2_home_fouls_avg <- aggregate(SP2$HF, by = list(SP2$HomeTeam),mean)
# sp2_home_foulsdata <- merge(sp2_home_fouls,sp2_home_fouls_avg, by='Group.1',all = T)
# names(sp2_home_foulsdata)[names(sp2_home_foulsdata) == "x.x"] <- "THfouls"
# names(sp2_home_foulsdata)[names(sp2_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# sp2_away_fouls <- aggregate(SP2$HF, by = list(SP2$AwayTeam), FUN = sum)
# sp2_away_fouls_avg <- aggregate(SP2$HF, by = list(SP2$AwayTeam),mean)
# sp2_away_foulsdata <- merge(sp2_away_fouls,sp2_away_fouls_avg, by='Group.1',all = T)
# names(sp2_away_foulsdata)[names(sp2_away_foulsdata) == "x.x"] <- "TAfouls"
# names(sp2_away_foulsdata)[names(sp2_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# sp2_fouls <- merge(sp2_home_foulsdata,sp2_away_foulsdata,by='Group.1',all = T)
# sp2_fouls$TotalFouls <- sp2_fouls$THfouls + sp2_fouls$TAfouls
#
# #yellow cards
# sp2_home_hyc <- aggregate(SP2$HY, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_ayc <- aggregate(SP2$AY, by = list(SP2$AwayTeam), FUN = sum)
# sp2_tyc <- merge(sp2_home_hyc,sp2_away_ayc, by='Group.1',all = T)
# names(sp2_tyc)[names(sp2_tyc) == "x.x"] <- "hyc"
# names(sp2_tyc)[names(sp2_tyc) == "x.y"] <- "ayc"
# sp2_tyc$TotalYellows <- sp2_tyc$hyc + sp2_tyc$ayc
#
# #merge fouls for and yellow cards
# sp2_fouls_conversion <- merge(sp2_tyc,sp2_fouls,by='Group.1',all = T)
# sp2_fouls_conversion$YcPerfoul <- round((sp2_fouls_conversion$TotalYellows/sp2_fouls_conversion$TotalFouls), digits = 2)
# ####################################################################################################################################
# #T1
# #home fouls for
# t1_home_fouls <- aggregate(T1$HF, by = list(T1$HomeTeam), FUN = sum)
# t1_home_fouls_avg <- aggregate(T1$HF, by = list(T1$HomeTeam),mean)
# t1_home_foulsdata <- merge(t1_home_fouls,t1_home_fouls_avg, by='Group.1',all = T)
# names(t1_home_foulsdata)[names(t1_home_foulsdata) == "x.x"] <- "THfouls"
# names(t1_home_foulsdata)[names(t1_home_foulsdata) == "x.y"] <- "Avg_FTHfouls"
# #away fouls for
# t1_away_fouls <- aggregate(T1$HF, by = list(T1$AwayTeam), FUN = sum)
# t1_away_fouls_avg <- aggregate(T1$HF, by = list(T1$AwayTeam),mean)
# t1_away_foulsdata <- merge(t1_away_fouls,t1_away_fouls_avg, by='Group.1',all = T)
# names(t1_away_foulsdata)[names(t1_away_foulsdata) == "x.x"] <- "TAfouls"
# names(t1_away_foulsdata)[names(t1_away_foulsdata) == "x.y"] <- "Avg_FTAfouls"
# #total fouls for
# t1_fouls <- merge(t1_home_foulsdata,t1_away_foulsdata,by='Group.1',all = T)
# t1_fouls$TotalFouls <- t1_fouls$THfouls + t1_fouls$TAfouls
#
# #yellow cards
# t1_home_hyc <- aggregate(T1$HY, by = list(T1$HomeTeam), FUN = sum)
# t1_away_ayc <- aggregate(T1$AY, by = list(T1$AwayTeam), FUN = sum)
# t1_tyc <- merge(t1_home_hyc,t1_away_ayc, by='Group.1',all = T)
# names(t1_tyc)[names(t1_tyc) == "x.x"] <- "hyc"
# names(t1_tyc)[names(t1_tyc) == "x.y"] <- "ayc"
# t1_tyc$TotalYellows <- t1_tyc$hyc + t1_tyc$ayc
#
# #merge fouls for and yellow cards
# t1_fouls_conversion <- merge(t1_tyc,t1_fouls,by='Group.1',all = T)
# t1_fouls_conversion$YcPerfoul <- round((t1_fouls_conversion$TotalYellows/t1_fouls_conversion$TotalFouls), digits = 2)


















