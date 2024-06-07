library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
#Total goals scored
#MLS
#home goals scored
mls_home_gs <- aggregate(MLS$FTHG, by = list(MLS$HomeTeam), FUN = sum)
mls_home_gs_avg <- aggregate(MLS$FTHG, by = list(MLS$HomeTeam),mean)
mls_home_scoring <- merge(mls_home_gs,mls_home_gs_avg, by='Group.1',all = T)
names(mls_home_scoring)[names(mls_home_scoring) == "x.x"] <- "TFthg"
names(mls_home_scoring)[names(mls_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
mls_away_gs <- aggregate(MLS$FTAG, by = list(MLS$AwayTeam), FUN = sum)
mls_away_gs_avg <- aggregate(MLS$FTAG, by = list(MLS$AwayTeam),mean)
mls_away_scoring <- merge(mls_away_gs,mls_away_gs_avg, by='Group.1',all = T)
names(mls_away_scoring)[names(mls_away_scoring) == "x.x"] <- "TFtag"
names(mls_away_scoring)[names(mls_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
mls_scoring <- merge(mls_home_scoring,mls_away_scoring,by='Group.1',all = T)
mls_scoring$TGS <- mls_scoring$TFthg + mls_scoring$TFtag

#Home shots on target
mls_home_hst <- aggregate(MLS$HST, by = list(MLS$HomeTeam), FUN = sum)
mls_away_ast <- aggregate(MLS$AST, by = list(MLS$AwayTeam), FUN = sum)
mls_tst <- merge(mls_home_hst,mls_away_ast, by='Group.1',all = T)
names(mls_tst)[names(mls_tst) == "x.x"] <- "hst"
names(mls_tst)[names(mls_tst) == "x.y"] <- "ast"
mls_tst$TST <- mls_tst$hst + mls_tst$ast
#merge goals scored and shots on target
mls_scoring_conversion <- merge(mls_tst,mls_scoring,by='Group.1',all = T)
#add HSC ASC TSC
mls_scoring_conversion$HSTC <- percent(mls_scoring_conversion$TFthg/mls_scoring_conversion$hst, accuracy = 0.01)
mls_scoring_conversion$ASTC <- percent(mls_scoring_conversion$TFtag/mls_scoring_conversion$ast, accuracy = 0.01)
mls_scoring_conversion$TSTC <- percent(mls_scoring_conversion$TGS/mls_scoring_conversion$TST, accuracy = 0.01)
#merge games played
mls_scoring_conversion <- cbind(mls_scoring_conversion,mls_games_played)
#create the second part
#home goals conceded
mls_home_gc <- aggregate(MLS$FTAG, by = list(MLS$HomeTeam), FUN = sum)
mls_home_gc_avg <- aggregate(MLS$FTAG, by = list(MLS$HomeTeam),mean)
mls_home_conceding <- merge(mls_home_gc,mls_home_gc_avg, by='Group.1',all = T)
names(mls_home_conceding)[names(mls_home_conceding) == "x.x"] <- "TFthc"
names(mls_home_conceding)[names(mls_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
mls_away_gc <- aggregate(MLS$FTHG, by = list(MLS$AwayTeam), FUN = sum)
mls_away_gc_avg <- aggregate(MLS$FTHG, by = list(MLS$AwayTeam),mean)
mls_away_conceding <- merge(mls_away_gc,mls_away_gc_avg, by='Group.1',all = T)
names(mls_away_conceding)[names(mls_away_conceding) == "x.x"] <- "TFtac"
names(mls_away_conceding)[names(mls_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
mls_conceding <- merge(mls_home_conceding,mls_away_conceding,by='Group.1',all = T)
mls_conceding$TGC <- mls_conceding$TFthc + mls_conceding$TFtac
mls_home_hst
#Home shots conceded
mls_home_hsc <- aggregate(MLS$AST, by = list(MLS$HomeTeam), FUN = sum)
mls_away_asc <- aggregate(MLS$HST, by = list(MLS$AwayTeam), FUN = sum)
mls_tsc <- merge(mls_home_hsc,mls_away_asc, by='Group.1',all = T)
names(mls_tsc)[names(mls_tsc) == "x.x"] <- "hsc"
names(mls_tsc)[names(mls_tsc) == "x.y"] <- "asc"
mls_tsc$TSC <- mls_tsc$hsc + mls_tsc$asc
#merge goals conceded and shots conceded
mls_conceding_conversion <- merge(mls_tsc,mls_conceding,by='Group.1',all = T)

#add HSC ASC TSC
mls_conceding_conversion$HSCC <- percent(mls_conceding_conversion$TFthc/mls_conceding_conversion$hsc, accuracy = 0.01)
mls_conceding_conversion$ASCC <- percent(mls_conceding_conversion$TFtac/mls_conceding_conversion$asc, accuracy = 0.01)
mls_conceding_conversion$TSCC <- percent(mls_conceding_conversion$TGC/mls_conceding_conversion$TSC, accuracy = 0.01)
mls_conceding_conversion$XSTC <- round(mls_scoring$TGS/(mls_tst$TST - mls_scoring$TGS), digits = 2)

#merge the two parts
mls_shots_analysis <- merge(mls_scoring_conversion,mls_conceding_conversion,by='Group.1',all = T)
################################################################################################
mls_shots_analysis
#ARG
#home goals scored
arg_home_gs <- aggregate(ARG$FTHG, by = list(ARG$HomeTeam), FUN = sum)
arg_home_gs_avg <- aggregate(ARG$FTHG, by = list(ARG$HomeTeam),mean)
arg_home_scoring <- merge(arg_home_gs,arg_home_gs_avg, by='Group.1',all = T)
names(arg_home_scoring)[names(arg_home_scoring) == "x.x"] <- "TFthg"
names(arg_home_scoring)[names(arg_home_scoring) == "x.y"] <- "Avg_Fthg"
#away goals scored
arg_away_gs <- aggregate(ARG$FTAG, by = list(ARG$AwayTeam), FUN = sum)
arg_away_gs_avg <- aggregate(ARG$FTAG, by = list(ARG$AwayTeam),mean)
arg_away_scoring <- merge(arg_away_gs,arg_away_gs_avg, by='Group.1',all = T)
names(arg_away_scoring)[names(arg_away_scoring) == "x.x"] <- "TFtag"
names(arg_away_scoring)[names(arg_away_scoring) == "x.y"] <- "Avg_Ftag"
#total goals scored
arg_scoring <- merge(arg_home_scoring,arg_away_scoring,by='Group.1',all = T)
arg_scoring$TGS <- arg_scoring$TFthg + arg_scoring$TFtag

#Home shots on target
arg_home_hst <- aggregate(ARG$HST, by = list(ARG$HomeTeam), FUN = sum)
arg_away_ast <- aggregate(ARG$AST, by = list(ARG$AwayTeam), FUN = sum)
arg_tst <- merge(arg_home_hst,arg_away_ast, by='Group.1',all = T)
names(arg_tst)[names(arg_tst) == "x.x"] <- "hst"
names(arg_tst)[names(arg_tst) == "x.y"] <- "ast"
arg_tst$TST <- arg_tst$hst + arg_tst$ast
#merge goals scored and shots on target
arg_scoring_conversion <- merge(arg_tst,arg_scoring,by='Group.1',all = T)
#add HSC ASC TSC
arg_scoring_conversion$HSTC <- percent(arg_scoring_conversion$TFthg/arg_scoring_conversion$hst, accuracy = 0.01)
arg_scoring_conversion$ASTC <- percent(arg_scoring_conversion$TFtag/arg_scoring_conversion$ast, accuracy = 0.01)
arg_scoring_conversion$TSTC <- percent(arg_scoring_conversion$TGS/arg_scoring_conversion$TST, accuracy = 0.01)
#merge games played
arg_scoring_conversion <- cbind(arg_scoring_conversion,arg_games_played)
#create the second part
#home goals conceded
arg_home_gc <- aggregate(ARG$FTAG, by = list(ARG$HomeTeam), FUN = sum)
arg_home_gc_avg <- aggregate(ARG$FTAG, by = list(ARG$HomeTeam),mean)
arg_home_conceding <- merge(arg_home_gc,arg_home_gc_avg, by='Group.1',all = T)
names(arg_home_conceding)[names(arg_home_conceding) == "x.x"] <- "TFthc"
names(arg_home_conceding)[names(arg_home_conceding) == "x.y"] <- "Avg_Fthc"
#away goals conceded
arg_away_gc <- aggregate(ARG$FTHG, by = list(ARG$AwayTeam), FUN = sum)
arg_away_gc_avg <- aggregate(ARG$FTHG, by = list(ARG$AwayTeam),mean)
arg_away_conceding <- merge(arg_away_gc,arg_away_gc_avg, by='Group.1',all = T)
names(arg_away_conceding)[names(arg_away_conceding) == "x.x"] <- "TFtac"
names(arg_away_conceding)[names(arg_away_conceding) == "x.y"] <- "Avg_Ftac"
#total goals conceded
arg_conceding <- merge(arg_home_conceding,arg_away_conceding,by='Group.1',all = T)
arg_conceding$TGC <- arg_conceding$TFthc + arg_conceding$TFtac

#Home shots conceded
arg_home_hsc <- aggregate(ARG$AST, by = list(ARG$HomeTeam), FUN = sum)
arg_away_asc <- aggregate(ARG$HST, by = list(ARG$AwayTeam), FUN = sum)
arg_tsc <- merge(arg_home_hsc,arg_away_asc, by='Group.1',all = T)
names(arg_tsc)[names(arg_tsc) == "x.x"] <- "hsc"
names(arg_tsc)[names(arg_tsc) == "x.y"] <- "asc"
arg_tsc$TSC <- arg_tsc$hsc + arg_tsc$asc
#merge goals conceded and shots conceded
arg_conceding_conversion <- merge(arg_tsc,arg_conceding,by='Group.1',all = T)

#add HSC ASC TSC
arg_conceding_conversion$HSCC <- percent(arg_conceding_conversion$TFthc/arg_conceding_conversion$hsc, accuracy = 0.01)
arg_conceding_conversion$ASCC <- percent(arg_conceding_conversion$TFtac/arg_conceding_conversion$asc, accuracy = 0.01)
arg_conceding_conversion$TSCC <- percent(arg_conceding_conversion$TGC/arg_conceding_conversion$TSC, accuracy = 0.01)
arg_conceding_conversion$XSTC <- round(arg_scoring$TGS/(arg_tst$TST - arg_scoring$TGS), digits = 2)

#merge the two parts
arg_shots_analysis <- merge(arg_scoring_conversion,arg_conceding_conversion,by='Group.1',all = T)
#####################################################################################################
#D2
#home goals scored
# d2_home_gs <- aggregate(D2$FTHG, by = list(D2$HomeTeam), FUN = sum)
# d2_home_gs_avg <- aggregate(D2$FTHG, by = list(D2$HomeTeam),mean)
# d2_home_scoring <- merge(d2_home_gs,d2_home_gs_avg, by='Group.1',all = T)
# names(d2_home_scoring)[names(d2_home_scoring) == "x.x"] <- "TFthg"
# names(d2_home_scoring)[names(d2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# d2_away_gs <- aggregate(D2$FTAG, by = list(D2$AwayTeam), FUN = sum)
# d2_away_gs_avg <- aggregate(D2$FTAG, by = list(D2$AwayTeam),mean)
# d2_away_scoring <- merge(d2_away_gs,d2_away_gs_avg, by='Group.1',all = T)
# names(d2_away_scoring)[names(d2_away_scoring) == "x.x"] <- "TFtag"
# names(d2_away_scoring)[names(d2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# d2_scoring <- merge(d2_home_scoring,d2_away_scoring,by='Group.1',all = T)
# d2_scoring$TGS <- d2_scoring$TFthg + d2_scoring$TFtag
#
# #Home shots on target
# d2_home_hst <- aggregate(D2$HST, by = list(D2$HomeTeam), FUN = sum)
# d2_away_ast <- aggregate(D2$AST, by = list(D2$AwayTeam), FUN = sum)
# d2_tst <- merge(d2_home_hst,d2_away_ast, by='Group.1',all = T)
# names(d2_tst)[names(d2_tst) == "x.x"] <- "hst"
# names(d2_tst)[names(d2_tst) == "x.y"] <- "ast"
# d2_tst$TST <- d2_tst$hst + d2_tst$ast
# #merge goals scored and shots on target
# d2_scoring_conversion <- merge(d2_tst,d2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# d2_scoring_conversion$HSTC <- percent(d2_scoring_conversion$TFthg/d2_scoring_conversion$hst, accuracy = 0.01)
# d2_scoring_conversion$ASTC <- percent(d2_scoring_conversion$TFtag/d2_scoring_conversion$ast, accuracy = 0.01)
# d2_scoring_conversion$TSTC <- percent(d2_scoring_conversion$TGS/d2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# d2_scoring_conversion <- cbind(d2_scoring_conversion,d2_games_played)
# #create the second part
# #home goals conceded
# d2_home_gc <- aggregate(D2$FTAG, by = list(D2$HomeTeam), FUN = sum)
# d2_home_gc_avg <- aggregate(D2$FTAG, by = list(D2$HomeTeam),mean)
# d2_home_conceding <- merge(d2_home_gc,d2_home_gc_avg, by='Group.1',all = T)
# names(d2_home_conceding)[names(d2_home_conceding) == "x.x"] <- "TFthc"
# names(d2_home_conceding)[names(d2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# d2_away_gc <- aggregate(D2$FTHG, by = list(D2$AwayTeam), FUN = sum)
# d2_away_gc_avg <- aggregate(D2$FTHG, by = list(D2$AwayTeam),mean)
# d2_away_conceding <- merge(d2_away_gc,d2_away_gc_avg, by='Group.1',all = T)
# names(d2_away_conceding)[names(d2_away_conceding) == "x.x"] <- "TFtac"
# names(d2_away_conceding)[names(d2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# d2_conceding <- merge(d2_home_conceding,d2_away_conceding,by='Group.1',all = T)
# d2_conceding$TGC <- d2_conceding$TFthc + d2_conceding$TFtac
#
# #Home shots conceded
# d2_home_hsc <- aggregate(D2$AST, by = list(D2$HomeTeam), FUN = sum)
# d2_away_asc <- aggregate(D2$HST, by = list(D2$AwayTeam), FUN = sum)
# d2_tsc <- merge(d2_home_hsc,d2_away_asc, by='Group.1',all = T)
# names(d2_tsc)[names(d2_tsc) == "x.x"] <- "hsc"
# names(d2_tsc)[names(d2_tsc) == "x.y"] <- "asc"
# d2_tsc$TSC <- d2_tsc$hsc + d2_tsc$asc
# #merge goals conceded and shots conceded
# d2_conceding_conversion <- merge(d2_tsc,d2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# d2_conceding_conversion$HSCC <- percent(d2_conceding_conversion$TFthc/d2_conceding_conversion$hsc, accuracy = 0.01)
# d2_conceding_conversion$ASCC <- percent(d2_conceding_conversion$TFtac/d2_conceding_conversion$asc, accuracy = 0.01)
# d2_conceding_conversion$TSCC <- percent(d2_conceding_conversion$TGC/d2_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# d2_shots_analysis <- merge(d2_scoring_conversion,d2_conceding_conversion,by='Group.1',all = T)
# #D2
# #home goals scored
# d2_home_gs <- aggregate(D2$FTHG, by = list(D2$HomeTeam), FUN = sum)
# d2_home_gs_avg <- aggregate(D2$FTHG, by = list(D2$HomeTeam),mean)
# d2_home_scoring <- merge(d2_home_gs,d2_home_gs_avg, by='Group.1',all = T)
# names(d2_home_scoring)[names(d2_home_scoring) == "x.x"] <- "TFthg"
# names(d2_home_scoring)[names(d2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# d2_away_gs <- aggregate(D2$FTAG, by = list(D2$AwayTeam), FUN = sum)
# d2_away_gs_avg <- aggregate(D2$FTAG, by = list(D2$AwayTeam),mean)
# d2_away_scoring <- merge(d2_away_gs,d2_away_gs_avg, by='Group.1',all = T)
# names(d2_away_scoring)[names(d2_away_scoring) == "x.x"] <- "TFtag"
# names(d2_away_scoring)[names(d2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# d2_scoring <- merge(d2_home_scoring,d2_away_scoring,by='Group.1',all = T)
# d2_scoring$TGS <- d2_scoring$TFthg + d2_scoring$TFtag
#
# #Home shots on target
# d2_home_hst <- aggregate(D2$HST, by = list(D2$HomeTeam), FUN = sum)
# d2_away_ast <- aggregate(D2$AST, by = list(D2$AwayTeam), FUN = sum)
# d2_tst <- merge(d2_home_hst,d2_away_ast, by='Group.1',all = T)
# names(d2_tst)[names(d2_tst) == "x.x"] <- "hst"
# names(d2_tst)[names(d2_tst) == "x.y"] <- "ast"
# d2_tst$TST <- d2_tst$hst + d2_tst$ast
# #merge goals scored and shots on target
# d2_scoring_conversion <- merge(d2_tst,d2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# d2_scoring_conversion$HSTC <- percent(d2_scoring_conversion$TFthg/d2_scoring_conversion$hst, accuracy = 0.01)
# d2_scoring_conversion$ASTC <- percent(d2_scoring_conversion$TFtag/d2_scoring_conversion$ast, accuracy = 0.01)
# d2_scoring_conversion$TSTC <- percent(d2_scoring_conversion$TGS/d2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# d2_scoring_conversion <- cbind(d2_scoring_conversion,d2_games_played)
# #create the second part
# #home goals conceded
# d2_home_gc <- aggregate(D2$FTAG, by = list(D2$HomeTeam), FUN = sum)
# d2_home_gc_avg <- aggregate(D2$FTAG, by = list(D2$HomeTeam),mean)
# d2_home_conceding <- merge(d2_home_gc,d2_home_gc_avg, by='Group.1',all = T)
# names(d2_home_conceding)[names(d2_home_conceding) == "x.x"] <- "TFthc"
# names(d2_home_conceding)[names(d2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# d2_away_gc <- aggregate(D2$FTHG, by = list(D2$AwayTeam), FUN = sum)
# d2_away_gc_avg <- aggregate(D2$FTHG, by = list(D2$AwayTeam),mean)
# d2_away_conceding <- merge(d2_away_gc,d2_away_gc_avg, by='Group.1',all = T)
# names(d2_away_conceding)[names(d2_away_conceding) == "x.x"] <- "TFtac"
# names(d2_away_conceding)[names(d2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# d2_conceding <- merge(d2_home_conceding,d2_away_conceding,by='Group.1',all = T)
# d2_conceding$TGC <- d2_conceding$TFthc + d2_conceding$TFtac
#
# #Home shots conceded
# d2_home_hsc <- aggregate(D2$AST, by = list(D2$HomeTeam), FUN = sum)
# d2_away_asc <- aggregate(D2$HST, by = list(D2$AwayTeam), FUN = sum)
# d2_tsc <- merge(d2_home_hsc,d2_away_asc, by='Group.1',all = T)
# names(d2_tsc)[names(d2_tsc) == "x.x"] <- "hsc"
# names(d2_tsc)[names(d2_tsc) == "x.y"] <- "asc"
# d2_tsc$TSC <- d2_tsc$hsc + d2_tsc$asc
# #merge goals conceded and shots conceded
# d2_conceding_conversion <- merge(d2_tsc,d2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# d2_conceding_conversion$HSCC <- percent(d2_conceding_conversion$TFthc/d2_conceding_conversion$hsc, accuracy = 0.01)
# d2_conceding_conversion$ASCC <- percent(d2_conceding_conversion$TFtac/d2_conceding_conversion$asc, accuracy = 0.01)
# d2_conceding_conversion$TSCC <- percent(d2_conceding_conversion$TGC/d2_conceding_conversion$TSC, accuracy = 0.01)
# d2_conceding_conversion$XSTC <- round(d2_scoring$TGS/(d2_tst$TST - d2_scoring$TGS), digits = 2)
#
# #merge the two parts
# d2_shots_analysis <- merge(d2_scoring_conversion,d2_conceding_conversion,by='Group.1',all = T)
# ##############################################################################################################################
# #E0
# #home goals scored
# e0_home_gs <- aggregate(E0$FTHG, by = list(E0$HomeTeam), FUN = sum)
# e0_home_gs_avg <- aggregate(E0$FTHG, by = list(E0$HomeTeam),mean)
# e0_home_scoring <- merge(e0_home_gs,e0_home_gs_avg, by='Group.1',all = T)
# names(e0_home_scoring)[names(e0_home_scoring) == "x.x"] <- "TFthg"
# names(e0_home_scoring)[names(e0_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e0_away_gs <- aggregate(E0$FTAG, by = list(E0$AwayTeam), FUN = sum)
# e0_away_gs_avg <- aggregate(E0$FTAG, by = list(E0$AwayTeam),mean)
# e0_away_scoring <- merge(e0_away_gs,e0_away_gs_avg, by='Group.1',all = T)
# names(e0_away_scoring)[names(e0_away_scoring) == "x.x"] <- "TFtag"
# names(e0_away_scoring)[names(e0_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e0_scoring <- merge(e0_home_scoring,e0_away_scoring,by='Group.1',all = T)
# e0_scoring$TGS <- e0_scoring$TFthg + e0_scoring$TFtag
#
# #Home shots on target
# e0_home_hst <- aggregate(E0$HST, by = list(E0$HomeTeam), FUN = sum)
# e0_away_ast <- aggregate(E0$AST, by = list(E0$AwayTeam), FUN = sum)
# e0_tst <- merge(e0_home_hst,e0_away_ast, by='Group.1',all = T)
# names(e0_tst)[names(e0_tst) == "x.x"] <- "hst"
# names(e0_tst)[names(e0_tst) == "x.y"] <- "ast"
# e0_tst$TST <- e0_tst$hst + e0_tst$ast
# #merge goals scored and shots on target
# e0_scoring_conversion <- merge(e0_tst,e0_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e0_scoring_conversion$HSTC <- percent(e0_scoring_conversion$TFthg/e0_scoring_conversion$hst, accuracy = 0.01)
# e0_scoring_conversion$ASTC <- percent(e0_scoring_conversion$TFtag/e0_scoring_conversion$ast, accuracy = 0.01)
# e0_scoring_conversion$TSTC <- percent(e0_scoring_conversion$TGS/e0_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e0_scoring_conversion <- cbind(e0_scoring_conversion,e0_games_played)
# #create the second part
# #home goals conceded
# e0_home_gc <- aggregate(E0$FTAG, by = list(E0$HomeTeam), FUN = sum)
# e0_home_gc_avg <- aggregate(E0$FTAG, by = list(E0$HomeTeam),mean)
# e0_home_conceding <- merge(e0_home_gc,e0_home_gc_avg, by='Group.1',all = T)
# names(e0_home_conceding)[names(e0_home_conceding) == "x.x"] <- "TFthc"
# names(e0_home_conceding)[names(e0_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e0_away_gc <- aggregate(E0$FTHG, by = list(E0$AwayTeam), FUN = sum)
# e0_away_gc_avg <- aggregate(E0$FTHG, by = list(E0$AwayTeam),mean)
# e0_away_conceding <- merge(e0_away_gc,e0_away_gc_avg, by='Group.1',all = T)
# names(e0_away_conceding)[names(e0_away_conceding) == "x.x"] <- "TFtac"
# names(e0_away_conceding)[names(e0_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e0_conceding <- merge(e0_home_conceding,e0_away_conceding,by='Group.1',all = T)
# e0_conceding$TGC <- e0_conceding$TFthc + e0_conceding$TFtac
#
# #Home shots conceded
# e0_home_hsc <- aggregate(E0$AST, by = list(E0$HomeTeam), FUN = sum)
# e0_away_asc <- aggregate(E0$HST, by = list(E0$AwayTeam), FUN = sum)
# e0_tsc <- merge(e0_home_hsc,e0_away_asc, by='Group.1',all = T)
# names(e0_tsc)[names(e0_tsc) == "x.x"] <- "hsc"
# names(e0_tsc)[names(e0_tsc) == "x.y"] <- "asc"
# e0_tsc$TSC <- e0_tsc$hsc + e0_tsc$asc
# #merge goals conceded and shots conceded
# e0_conceding_conversion <- merge(e0_tsc,e0_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e0_conceding_conversion$HSCC <- percent(e0_conceding_conversion$TFthc/e0_conceding_conversion$hsc, accuracy = 0.01)
# e0_conceding_conversion$ASCC <- percent(e0_conceding_conversion$TFtac/e0_conceding_conversion$asc, accuracy = 0.01)
# e0_conceding_conversion$TSCC <- percent(e0_conceding_conversion$TGC/e0_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# e0_shots_analysis <- merge(e0_scoring_conversion,e0_conceding_conversion,by='Group.1',all = T)
# #E0
# #home goals scored
# e0_home_gs <- aggregate(E0$FTHG, by = list(E0$HomeTeam), FUN = sum)
# e0_home_gs_avg <- aggregate(E0$FTHG, by = list(E0$HomeTeam),mean)
# e0_home_scoring <- merge(e0_home_gs,e0_home_gs_avg, by='Group.1',all = T)
# names(e0_home_scoring)[names(e0_home_scoring) == "x.x"] <- "TFthg"
# names(e0_home_scoring)[names(e0_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e0_away_gs <- aggregate(E0$FTAG, by = list(E0$AwayTeam), FUN = sum)
# e0_away_gs_avg <- aggregate(E0$FTAG, by = list(E0$AwayTeam),mean)
# e0_away_scoring <- merge(e0_away_gs,e0_away_gs_avg, by='Group.1',all = T)
# names(e0_away_scoring)[names(e0_away_scoring) == "x.x"] <- "TFtag"
# names(e0_away_scoring)[names(e0_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e0_scoring <- merge(e0_home_scoring,e0_away_scoring,by='Group.1',all = T)
# e0_scoring$TGS <- e0_scoring$TFthg + e0_scoring$TFtag
#
# #Home shots on target
# e0_home_hst <- aggregate(E0$HST, by = list(E0$HomeTeam), FUN = sum)
# e0_away_ast <- aggregate(E0$AST, by = list(E0$AwayTeam), FUN = sum)
# e0_tst <- merge(e0_home_hst,e0_away_ast, by='Group.1',all = T)
# names(e0_tst)[names(e0_tst) == "x.x"] <- "hst"
# names(e0_tst)[names(e0_tst) == "x.y"] <- "ast"
# e0_tst$TST <- e0_tst$hst + e0_tst$ast
# #merge goals scored and shots on target
# e0_scoring_conversion <- merge(e0_tst,e0_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e0_scoring_conversion$HSTC <- percent(e0_scoring_conversion$TFthg/e0_scoring_conversion$hst, accuracy = 0.01)
# e0_scoring_conversion$ASTC <- percent(e0_scoring_conversion$TFtag/e0_scoring_conversion$ast, accuracy = 0.01)
# e0_scoring_conversion$TSTC <- percent(e0_scoring_conversion$TGS/e0_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e0_scoring_conversion <- cbind(e0_scoring_conversion,e0_games_played)
# #create the second part
# #home goals conceded
# e0_home_gc <- aggregate(E0$FTAG, by = list(E0$HomeTeam), FUN = sum)
# e0_home_gc_avg <- aggregate(E0$FTAG, by = list(E0$HomeTeam),mean)
# e0_home_conceding <- merge(e0_home_gc,e0_home_gc_avg, by='Group.1',all = T)
# names(e0_home_conceding)[names(e0_home_conceding) == "x.x"] <- "TFthc"
# names(e0_home_conceding)[names(e0_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e0_away_gc <- aggregate(E0$FTHG, by = list(E0$AwayTeam), FUN = sum)
# e0_away_gc_avg <- aggregate(E0$FTHG, by = list(E0$AwayTeam),mean)
# e0_away_conceding <- merge(e0_away_gc,e0_away_gc_avg, by='Group.1',all = T)
# names(e0_away_conceding)[names(e0_away_conceding) == "x.x"] <- "TFtac"
# names(e0_away_conceding)[names(e0_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e0_conceding <- merge(e0_home_conceding,e0_away_conceding,by='Group.1',all = T)
# e0_conceding$TGC <- e0_conceding$TFthc + e0_conceding$TFtac
#
# #Home shots conceded
# e0_home_hsc <- aggregate(E0$AST, by = list(E0$HomeTeam), FUN = sum)
# e0_away_asc <- aggregate(E0$HST, by = list(E0$AwayTeam), FUN = sum)
# e0_tsc <- merge(e0_home_hsc,e0_away_asc, by='Group.1',all = T)
# names(e0_tsc)[names(e0_tsc) == "x.x"] <- "hsc"
# names(e0_tsc)[names(e0_tsc) == "x.y"] <- "asc"
# e0_tsc$TSC <- e0_tsc$hsc + e0_tsc$asc
# #merge goals conceded and shots conceded
# e0_conceding_conversion <- merge(e0_tsc,e0_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e0_conceding_conversion$HSCC <- percent(e0_conceding_conversion$TFthc/e0_conceding_conversion$hsc, accuracy = 0.01)
# e0_conceding_conversion$ASCC <- percent(e0_conceding_conversion$TFtac/e0_conceding_conversion$asc, accuracy = 0.01)
# e0_conceding_conversion$TSCC <- percent(e0_conceding_conversion$TGC/e0_conceding_conversion$TSC, accuracy = 0.01)
# e0_conceding_conversion$XSTC <- round(e0_scoring$TGS/(e0_tst$TST - e0_scoring$TGS), digits = 2)
#
#
# #merge the two parts
# e0_shots_analysis <- merge(e0_scoring_conversion,e0_conceding_conversion,by='Group.1',all = T)
# #############################################################################################################################
# #E1
# #home goals scored
# e1_home_gs <- aggregate(E1$FTHG, by = list(E1$HomeTeam), FUN = sum)
# e1_home_gs_avg <- aggregate(E1$FTHG, by = list(E1$HomeTeam),mean)
# e1_home_scoring <- merge(e1_home_gs,e1_home_gs_avg, by='Group.1',all = T)
# names(e1_home_scoring)[names(e1_home_scoring) == "x.x"] <- "TFthg"
# names(e1_home_scoring)[names(e1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e1_away_gs <- aggregate(E1$FTAG, by = list(E1$AwayTeam), FUN = sum)
# e1_away_gs_avg <- aggregate(E1$FTAG, by = list(E1$AwayTeam),mean)
# e1_away_scoring <- merge(e1_away_gs,e1_away_gs_avg, by='Group.1',all = T)
# names(e1_away_scoring)[names(e1_away_scoring) == "x.x"] <- "TFtag"
# names(e1_away_scoring)[names(e1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e1_scoring <- merge(e1_home_scoring,e1_away_scoring,by='Group.1',all = T)
# e1_scoring$TGS <- e1_scoring$TFthg + e1_scoring$TFtag
#
# #Home shots on target
# e1_home_hst <- aggregate(E1$HST, by = list(E1$HomeTeam), FUN = sum)
# e1_away_ast <- aggregate(E1$AST, by = list(E1$AwayTeam), FUN = sum)
# e1_tst <- merge(e1_home_hst,e1_away_ast, by='Group.1',all = T)
# names(e1_tst)[names(e1_tst) == "x.x"] <- "hst"
# names(e1_tst)[names(e1_tst) == "x.y"] <- "ast"
# e1_tst$TST <- e1_tst$hst + e1_tst$ast
# #merge goals scored and shots on target
# e1_scoring_conversion <- merge(e1_tst,e1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e1_scoring_conversion$HSTC <- percent(e1_scoring_conversion$TFthg/e1_scoring_conversion$hst, accuracy = 0.01)
# e1_scoring_conversion$ASTC <- percent(e1_scoring_conversion$TFtag/e1_scoring_conversion$ast, accuracy = 0.01)
# e1_scoring_conversion$TSTC <- percent(e1_scoring_conversion$TGS/e1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e1_scoring_conversion <- cbind(e1_scoring_conversion,e1_games_played)
# #create the second part
# #home goals conceded
# e1_home_gc <- aggregate(E1$FTAG, by = list(E1$HomeTeam), FUN = sum)
# e1_home_gc_avg <- aggregate(E1$FTAG, by = list(E1$HomeTeam),mean)
# e1_home_conceding <- merge(e1_home_gc,e1_home_gc_avg, by='Group.1',all = T)
# names(e1_home_conceding)[names(e1_home_conceding) == "x.x"] <- "TFthc"
# names(e1_home_conceding)[names(e1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e1_away_gc <- aggregate(E1$FTHG, by = list(E1$AwayTeam), FUN = sum)
# e1_away_gc_avg <- aggregate(E1$FTHG, by = list(E1$AwayTeam),mean)
# e1_away_conceding <- merge(e1_away_gc,e1_away_gc_avg, by='Group.1',all = T)
# names(e1_away_conceding)[names(e1_away_conceding) == "x.x"] <- "TFtac"
# names(e1_away_conceding)[names(e1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e1_conceding <- merge(e1_home_conceding,e1_away_conceding,by='Group.1',all = T)
# e1_conceding$TGC <- e1_conceding$TFthc + e1_conceding$TFtac
#
# #Home shots conceded
# e1_home_hsc <- aggregate(E1$AST, by = list(E1$HomeTeam), FUN = sum)
# e1_away_asc <- aggregate(E1$HST, by = list(E1$AwayTeam), FUN = sum)
# e1_tsc <- merge(e1_home_hsc,e1_away_asc, by='Group.1',all = T)
# names(e1_tsc)[names(e1_tsc) == "x.x"] <- "hsc"
# names(e1_tsc)[names(e1_tsc) == "x.y"] <- "asc"
# e1_tsc$TSC <- e1_tsc$hsc + e1_tsc$asc
# #merge goals conceded and shots conceded
# e1_conceding_conversion <- merge(e1_tsc,e1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e1_conceding_conversion$HSCC <- percent(e1_conceding_conversion$TFthc/e1_conceding_conversion$hsc, accuracy = 0.01)
# e1_conceding_conversion$ASCC <- percent(e1_conceding_conversion$TFtac/e1_conceding_conversion$asc, accuracy = 0.01)
# e1_conceding_conversion$TSCC <- percent(e1_conceding_conversion$TGC/e1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# e1_shots_analysis <- merge(e1_scoring_conversion,e1_conceding_conversion,by='Group.1',all = T)
# #E1
# #home goals scored
# e1_home_gs <- aggregate(E1$FTHG, by = list(E1$HomeTeam), FUN = sum)
# e1_home_gs_avg <- aggregate(E1$FTHG, by = list(E1$HomeTeam),mean)
# e1_home_scoring <- merge(e1_home_gs,e1_home_gs_avg, by='Group.1',all = T)
# names(e1_home_scoring)[names(e1_home_scoring) == "x.x"] <- "TFthg"
# names(e1_home_scoring)[names(e1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e1_away_gs <- aggregate(E1$FTAG, by = list(E1$AwayTeam), FUN = sum)
# e1_away_gs_avg <- aggregate(E1$FTAG, by = list(E1$AwayTeam),mean)
# e1_away_scoring <- merge(e1_away_gs,e1_away_gs_avg, by='Group.1',all = T)
# names(e1_away_scoring)[names(e1_away_scoring) == "x.x"] <- "TFtag"
# names(e1_away_scoring)[names(e1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e1_scoring <- merge(e1_home_scoring,e1_away_scoring,by='Group.1',all = T)
# e1_scoring$TGS <- e1_scoring$TFthg + e1_scoring$TFtag
#
# #Home shots on target
# e1_home_hst <- aggregate(E1$HST, by = list(E1$HomeTeam), FUN = sum)
# e1_away_ast <- aggregate(E1$AST, by = list(E1$AwayTeam), FUN = sum)
# e1_tst <- merge(e1_home_hst,e1_away_ast, by='Group.1',all = T)
# names(e1_tst)[names(e1_tst) == "x.x"] <- "hst"
# names(e1_tst)[names(e1_tst) == "x.y"] <- "ast"
# e1_tst$TST <- e1_tst$hst + e1_tst$ast
# #merge goals scored and shots on target
# e1_scoring_conversion <- merge(e1_tst,e1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e1_scoring_conversion$HSTC <- percent(e1_scoring_conversion$TFthg/e1_scoring_conversion$hst, accuracy = 0.01)
# e1_scoring_conversion$ASTC <- percent(e1_scoring_conversion$TFtag/e1_scoring_conversion$ast, accuracy = 0.01)
# e1_scoring_conversion$TSTC <- percent(e1_scoring_conversion$TGS/e1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e1_scoring_conversion <- cbind(e1_scoring_conversion,e1_games_played)
# #create the second part
# #home goals conceded
# e1_home_gc <- aggregate(E1$FTAG, by = list(E1$HomeTeam), FUN = sum)
# e1_home_gc_avg <- aggregate(E1$FTAG, by = list(E1$HomeTeam),mean)
# e1_home_conceding <- merge(e1_home_gc,e1_home_gc_avg, by='Group.1',all = T)
# names(e1_home_conceding)[names(e1_home_conceding) == "x.x"] <- "TFthc"
# names(e1_home_conceding)[names(e1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e1_away_gc <- aggregate(E1$FTHG, by = list(E1$AwayTeam), FUN = sum)
# e1_away_gc_avg <- aggregate(E1$FTHG, by = list(E1$AwayTeam),mean)
# e1_away_conceding <- merge(e1_away_gc,e1_away_gc_avg, by='Group.1',all = T)
# names(e1_away_conceding)[names(e1_away_conceding) == "x.x"] <- "TFtac"
# names(e1_away_conceding)[names(e1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e1_conceding <- merge(e1_home_conceding,e1_away_conceding,by='Group.1',all = T)
# e1_conceding$TGC <- e1_conceding$TFthc + e1_conceding$TFtac
#
# #Home shots conceded
# e1_home_hsc <- aggregate(E1$AST, by = list(E1$HomeTeam), FUN = sum)
# e1_away_asc <- aggregate(E1$HST, by = list(E1$AwayTeam), FUN = sum)
# e1_tsc <- merge(e1_home_hsc,e1_away_asc, by='Group.1',all = T)
# names(e1_tsc)[names(e1_tsc) == "x.x"] <- "hsc"
# names(e1_tsc)[names(e1_tsc) == "x.y"] <- "asc"
# e1_tsc$TSC <- e1_tsc$hsc + e1_tsc$asc
# #merge goals conceded and shots conceded
# e1_conceding_conversion <- merge(e1_tsc,e1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e1_conceding_conversion$HSCC <- percent(e1_conceding_conversion$TFthc/e1_conceding_conversion$hsc, accuracy = 0.01)
# e1_conceding_conversion$ASCC <- percent(e1_conceding_conversion$TFtac/e1_conceding_conversion$asc, accuracy = 0.01)
# e1_conceding_conversion$TSCC <- percent(e1_conceding_conversion$TGC/e1_conceding_conversion$TSC, accuracy = 0.01)
# e1_conceding_conversion$XSTC <- round(e1_scoring$TGS/(e1_tst$TST - e1_scoring$TGS), digits = 2)
#
# #merge the two parts
# e1_shots_analysis <- merge(e1_scoring_conversion,e1_conceding_conversion,by='Group.1',all = T)
# #################################################################################################################################
# #E2
# #home goals scored
# e2_home_gs <- aggregate(E2$FTHG, by = list(E2$HomeTeam), FUN = sum)
# e2_home_gs_avg <- aggregate(E2$FTHG, by = list(E2$HomeTeam),mean)
# e2_home_scoring <- merge(e2_home_gs,e2_home_gs_avg, by='Group.1',all = T)
# names(e2_home_scoring)[names(e2_home_scoring) == "x.x"] <- "TFthg"
# names(e2_home_scoring)[names(e2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e2_away_gs <- aggregate(E2$FTAG, by = list(E2$AwayTeam), FUN = sum)
# e2_away_gs_avg <- aggregate(E2$FTAG, by = list(E2$AwayTeam),mean)
# e2_away_scoring <- merge(e2_away_gs,e2_away_gs_avg, by='Group.1',all = T)
# names(e2_away_scoring)[names(e2_away_scoring) == "x.x"] <- "TFtag"
# names(e2_away_scoring)[names(e2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e2_scoring <- merge(e2_home_scoring,e2_away_scoring,by='Group.1',all = T)
# e2_scoring$TGS <- e2_scoring$TFthg + e2_scoring$TFtag
#
# #Home shots on target
# e2_home_hst <- aggregate(E2$HST, by = list(E2$HomeTeam), FUN = sum)
# e2_away_ast <- aggregate(E2$AST, by = list(E2$AwayTeam), FUN = sum)
# e2_tst <- merge(e2_home_hst,e2_away_ast, by='Group.1',all = T)
# names(e2_tst)[names(e2_tst) == "x.x"] <- "hst"
# names(e2_tst)[names(e2_tst) == "x.y"] <- "ast"
# e2_tst$TST <- e2_tst$hst + e2_tst$ast
# #merge goals scored and shots on target
# e2_scoring_conversion <- merge(e2_tst,e2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e2_scoring_conversion$HSTC <- percent(e2_scoring_conversion$TFthg/e2_scoring_conversion$hst, accuracy = 0.01)
# e2_scoring_conversion$ASTC <- percent(e2_scoring_conversion$TFtag/e2_scoring_conversion$ast, accuracy = 0.01)
# e2_scoring_conversion$TSTC <- percent(e2_scoring_conversion$TGS/e2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e2_scoring_conversion <- cbind(e2_scoring_conversion,e2_games_played)
# #create the second part
# #home goals conceded
# e2_home_gc <- aggregate(E2$FTAG, by = list(E2$HomeTeam), FUN = sum)
# e2_home_gc_avg <- aggregate(E2$FTAG, by = list(E2$HomeTeam),mean)
# e2_home_conceding <- merge(e2_home_gc,e2_home_gc_avg, by='Group.1',all = T)
# names(e2_home_conceding)[names(e2_home_conceding) == "x.x"] <- "TFthc"
# names(e2_home_conceding)[names(e2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e2_away_gc <- aggregate(E2$FTHG, by = list(E2$AwayTeam), FUN = sum)
# e2_away_gc_avg <- aggregate(E2$FTHG, by = list(E2$AwayTeam),mean)
# e2_away_conceding <- merge(e2_away_gc,e2_away_gc_avg, by='Group.1',all = T)
# names(e2_away_conceding)[names(e2_away_conceding) == "x.x"] <- "TFtac"
# names(e2_away_conceding)[names(e2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e2_conceding <- merge(e2_home_conceding,e2_away_conceding,by='Group.1',all = T)
# e2_conceding$TGC <- e2_conceding$TFthc + e2_conceding$TFtac
#
# #Home shots conceded
# e2_home_hsc <- aggregate(E2$AST, by = list(E2$HomeTeam), FUN = sum)
# e2_away_asc <- aggregate(E2$HST, by = list(E2$AwayTeam), FUN = sum)
# e2_tsc <- merge(e2_home_hsc,e2_away_asc, by='Group.1',all = T)
# names(e2_tsc)[names(e2_tsc) == "x.x"] <- "hsc"
# names(e2_tsc)[names(e2_tsc) == "x.y"] <- "asc"
# e2_tsc$TSC <- e2_tsc$hsc + e2_tsc$asc
# #merge goals conceded and shots conceded
# e2_conceding_conversion <- merge(e2_tsc,e2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e2_conceding_conversion$HSCC <- percent(e2_conceding_conversion$TFthc/e2_conceding_conversion$hsc, accuracy = 0.01)
# e2_conceding_conversion$ASCC <- percent(e2_conceding_conversion$TFtac/e2_conceding_conversion$asc, accuracy = 0.01)
# e2_conceding_conversion$TSCC <- percent(e2_conceding_conversion$TGC/e2_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# e2_shots_analysis <- merge(e2_scoring_conversion,e2_conceding_conversion,by='Group.1',all = T)
# #E2
# #home goals scored
# e2_home_gs <- aggregate(E2$FTHG, by = list(E2$HomeTeam), FUN = sum)
# e2_home_gs_avg <- aggregate(E2$FTHG, by = list(E2$HomeTeam),mean)
# e2_home_scoring <- merge(e2_home_gs,e2_home_gs_avg, by='Group.1',all = T)
# names(e2_home_scoring)[names(e2_home_scoring) == "x.x"] <- "TFthg"
# names(e2_home_scoring)[names(e2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e2_away_gs <- aggregate(E2$FTAG, by = list(E2$AwayTeam), FUN = sum)
# e2_away_gs_avg <- aggregate(E2$FTAG, by = list(E2$AwayTeam),mean)
# e2_away_scoring <- merge(e2_away_gs,e2_away_gs_avg, by='Group.1',all = T)
# names(e2_away_scoring)[names(e2_away_scoring) == "x.x"] <- "TFtag"
# names(e2_away_scoring)[names(e2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e2_scoring <- merge(e2_home_scoring,e2_away_scoring,by='Group.1',all = T)
# e2_scoring$TGS <- e2_scoring$TFthg + e2_scoring$TFtag
#
# #Home shots on target
# e2_home_hst <- aggregate(E2$HST, by = list(E2$HomeTeam), FUN = sum)
# e2_away_ast <- aggregate(E2$AST, by = list(E2$AwayTeam), FUN = sum)
# e2_tst <- merge(e2_home_hst,e2_away_ast, by='Group.1',all = T)
# names(e2_tst)[names(e2_tst) == "x.x"] <- "hst"
# names(e2_tst)[names(e2_tst) == "x.y"] <- "ast"
# e2_tst$TST <- e2_tst$hst + e2_tst$ast
# #merge goals scored and shots on target
# e2_scoring_conversion <- merge(e2_tst,e2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e2_scoring_conversion$HSTC <- percent(e2_scoring_conversion$TFthg/e2_scoring_conversion$hst, accuracy = 0.01)
# e2_scoring_conversion$ASTC <- percent(e2_scoring_conversion$TFtag/e2_scoring_conversion$ast, accuracy = 0.01)
# e2_scoring_conversion$TSTC <- percent(e2_scoring_conversion$TGS/e2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e2_scoring_conversion <- cbind(e2_scoring_conversion,e2_games_played)
# #create the second part
# #home goals conceded
# e2_home_gc <- aggregate(E2$FTAG, by = list(E2$HomeTeam), FUN = sum)
# e2_home_gc_avg <- aggregate(E2$FTAG, by = list(E2$HomeTeam),mean)
# e2_home_conceding <- merge(e2_home_gc,e2_home_gc_avg, by='Group.1',all = T)
# names(e2_home_conceding)[names(e2_home_conceding) == "x.x"] <- "TFthc"
# names(e2_home_conceding)[names(e2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e2_away_gc <- aggregate(E2$FTHG, by = list(E2$AwayTeam), FUN = sum)
# e2_away_gc_avg <- aggregate(E2$FTHG, by = list(E2$AwayTeam),mean)
# e2_away_conceding <- merge(e2_away_gc,e2_away_gc_avg, by='Group.1',all = T)
# names(e2_away_conceding)[names(e2_away_conceding) == "x.x"] <- "TFtac"
# names(e2_away_conceding)[names(e2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e2_conceding <- merge(e2_home_conceding,e2_away_conceding,by='Group.1',all = T)
# e2_conceding$TGC <- e2_conceding$TFthc + e2_conceding$TFtac
#
# #Home shots conceded
# e2_home_hsc <- aggregate(E2$AST, by = list(E2$HomeTeam), FUN = sum)
# e2_away_asc <- aggregate(E2$HST, by = list(E2$AwayTeam), FUN = sum)
# e2_tsc <- merge(e2_home_hsc,e2_away_asc, by='Group.1',all = T)
# names(e2_tsc)[names(e2_tsc) == "x.x"] <- "hsc"
# names(e2_tsc)[names(e2_tsc) == "x.y"] <- "asc"
# e2_tsc$TSC <- e2_tsc$hsc + e2_tsc$asc
# #merge goals conceded and shots conceded
# e2_conceding_conversion <- merge(e2_tsc,e2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e2_conceding_conversion$HSCC <- percent(e2_conceding_conversion$TFthc/e2_conceding_conversion$hsc, accuracy = 0.01)
# e2_conceding_conversion$ASCC <- percent(e2_conceding_conversion$TFtac/e2_conceding_conversion$asc, accuracy = 0.01)
# e2_conceding_conversion$TSCC <- percent(e2_conceding_conversion$TGC/e2_conceding_conversion$TSC, accuracy = 0.01)
# e2_conceding_conversion$XSTC <- round(e2_scoring$TGS/(e2_tst$TST - e2_scoring$TGS), digits = 2)
#
# #merge the two parts
# e2_shots_analysis <- merge(e2_scoring_conversion,e2_conceding_conversion,by='Group.1',all = T)
# ###############################################################################################################################
# #E3
# #home goals scored
# e3_home_gs <- aggregate(E3$FTHG, by = list(E3$HomeTeam), FUN = sum)
# e3_home_gs_avg <- aggregate(E3$FTHG, by = list(E3$HomeTeam),mean)
# e3_home_scoring <- merge(e3_home_gs,e3_home_gs_avg, by='Group.1',all = T)
# names(e3_home_scoring)[names(e3_home_scoring) == "x.x"] <- "TFthg"
# names(e3_home_scoring)[names(e3_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e3_away_gs <- aggregate(E3$FTAG, by = list(E3$AwayTeam), FUN = sum)
# e3_away_gs_avg <- aggregate(E3$FTAG, by = list(E3$AwayTeam),mean)
# e3_away_scoring <- merge(e3_away_gs,e3_away_gs_avg, by='Group.1',all = T)
# names(e3_away_scoring)[names(e3_away_scoring) == "x.x"] <- "TFtag"
# names(e3_away_scoring)[names(e3_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e3_scoring <- merge(e3_home_scoring,e3_away_scoring,by='Group.1',all = T)
# e3_scoring$TGS <- e3_scoring$TFthg + e3_scoring$TFtag
#
# #Home shots on target
# e3_home_hst <- aggregate(E3$HST, by = list(E3$HomeTeam), FUN = sum)
# e3_away_ast <- aggregate(E3$AST, by = list(E3$AwayTeam), FUN = sum)
# e3_tst <- merge(e3_home_hst,e3_away_ast, by='Group.1',all = T)
# names(e3_tst)[names(e3_tst) == "x.x"] <- "hst"
# names(e3_tst)[names(e3_tst) == "x.y"] <- "ast"
# e3_tst$TST <- e3_tst$hst + e3_tst$ast
# #merge goals scored and shots on target
# e3_scoring_conversion <- merge(e3_tst,e3_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e3_scoring_conversion$HSTC <- percent(e3_scoring_conversion$TFthg/e3_scoring_conversion$hst, accuracy = 0.01)
# e3_scoring_conversion$ASTC <- percent(e3_scoring_conversion$TFtag/e3_scoring_conversion$ast, accuracy = 0.01)
# e3_scoring_conversion$TSTC <- percent(e3_scoring_conversion$TGS/e3_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e3_scoring_conversion <- cbind(e3_scoring_conversion,e3_games_played)
# #create the second part
# #home goals conceded
# e3_home_gc <- aggregate(E3$FTAG, by = list(E3$HomeTeam), FUN = sum)
# e3_home_gc_avg <- aggregate(E3$FTAG, by = list(E3$HomeTeam),mean)
# e3_home_conceding <- merge(e3_home_gc,e3_home_gc_avg, by='Group.1',all = T)
# names(e3_home_conceding)[names(e3_home_conceding) == "x.x"] <- "TFthc"
# names(e3_home_conceding)[names(e3_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e3_away_gc <- aggregate(E3$FTHG, by = list(E3$AwayTeam), FUN = sum)
# e3_away_gc_avg <- aggregate(E3$FTHG, by = list(E3$AwayTeam),mean)
# e3_away_conceding <- merge(e3_away_gc,e3_away_gc_avg, by='Group.1',all = T)
# names(e3_away_conceding)[names(e3_away_conceding) == "x.x"] <- "TFtac"
# names(e3_away_conceding)[names(e3_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e3_conceding <- merge(e3_home_conceding,e3_away_conceding,by='Group.1',all = T)
# e3_conceding$TGC <- e3_conceding$TFthc + e3_conceding$TFtac
#
# #Home shots conceded
# e3_home_hsc <- aggregate(E3$AST, by = list(E3$HomeTeam), FUN = sum)
# e3_away_asc <- aggregate(E3$HST, by = list(E3$AwayTeam), FUN = sum)
# e3_tsc <- merge(e3_home_hsc,e3_away_asc, by='Group.1',all = T)
# names(e3_tsc)[names(e3_tsc) == "x.x"] <- "hsc"
# names(e3_tsc)[names(e3_tsc) == "x.y"] <- "asc"
# e3_tsc$TSC <- e3_tsc$hsc + e3_tsc$asc
# #merge goals conceded and shots conceded
# e3_conceding_conversion <- merge(e3_tsc,e3_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e3_conceding_conversion$HSCC <- percent(e3_conceding_conversion$TFthc/e3_conceding_conversion$hsc, accuracy = 0.01)
# e3_conceding_conversion$ASCC <- percent(e3_conceding_conversion$TFtac/e3_conceding_conversion$asc, accuracy = 0.01)
# e3_conceding_conversion$TSCC <- percent(e3_conceding_conversion$TGC/e3_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# e3_shots_analysis <- merge(e3_scoring_conversion,e3_conceding_conversion,by='Group.1',all = T)
# #E3
# #home goals scored
# e3_home_gs <- aggregate(E3$FTHG, by = list(E3$HomeTeam), FUN = sum)
# e3_home_gs_avg <- aggregate(E3$FTHG, by = list(E3$HomeTeam),mean)
# e3_home_scoring <- merge(e3_home_gs,e3_home_gs_avg, by='Group.1',all = T)
# names(e3_home_scoring)[names(e3_home_scoring) == "x.x"] <- "TFthg"
# names(e3_home_scoring)[names(e3_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# e3_away_gs <- aggregate(E3$FTAG, by = list(E3$AwayTeam), FUN = sum)
# e3_away_gs_avg <- aggregate(E3$FTAG, by = list(E3$AwayTeam),mean)
# e3_away_scoring <- merge(e3_away_gs,e3_away_gs_avg, by='Group.1',all = T)
# names(e3_away_scoring)[names(e3_away_scoring) == "x.x"] <- "TFtag"
# names(e3_away_scoring)[names(e3_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# e3_scoring <- merge(e3_home_scoring,e3_away_scoring,by='Group.1',all = T)
# e3_scoring$TGS <- e3_scoring$TFthg + e3_scoring$TFtag
#
# #Home shots on target
# e3_home_hst <- aggregate(E3$HST, by = list(E3$HomeTeam), FUN = sum)
# e3_away_ast <- aggregate(E3$AST, by = list(E3$AwayTeam), FUN = sum)
# e3_tst <- merge(e3_home_hst,e3_away_ast, by='Group.1',all = T)
# names(e3_tst)[names(e3_tst) == "x.x"] <- "hst"
# names(e3_tst)[names(e3_tst) == "x.y"] <- "ast"
# e3_tst$TST <- e3_tst$hst + e3_tst$ast
# #merge goals scored and shots on target
# e3_scoring_conversion <- merge(e3_tst,e3_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# e3_scoring_conversion$HSTC <- percent(e3_scoring_conversion$TFthg/e3_scoring_conversion$hst, accuracy = 0.01)
# e3_scoring_conversion$ASTC <- percent(e3_scoring_conversion$TFtag/e3_scoring_conversion$ast, accuracy = 0.01)
# e3_scoring_conversion$TSTC <- percent(e3_scoring_conversion$TGS/e3_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# e3_scoring_conversion <- cbind(e3_scoring_conversion,e3_games_played)
# #create the second part
# #home goals conceded
# e3_home_gc <- aggregate(E3$FTAG, by = list(E3$HomeTeam), FUN = sum)
# e3_home_gc_avg <- aggregate(E3$FTAG, by = list(E3$HomeTeam),mean)
# e3_home_conceding <- merge(e3_home_gc,e3_home_gc_avg, by='Group.1',all = T)
# names(e3_home_conceding)[names(e3_home_conceding) == "x.x"] <- "TFthc"
# names(e3_home_conceding)[names(e3_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# e3_away_gc <- aggregate(E3$FTHG, by = list(E3$AwayTeam), FUN = sum)
# e3_away_gc_avg <- aggregate(E3$FTHG, by = list(E3$AwayTeam),mean)
# e3_away_conceding <- merge(e3_away_gc,e3_away_gc_avg, by='Group.1',all = T)
# names(e3_away_conceding)[names(e3_away_conceding) == "x.x"] <- "TFtac"
# names(e3_away_conceding)[names(e3_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# e3_conceding <- merge(e3_home_conceding,e3_away_conceding,by='Group.1',all = T)
# e3_conceding$TGC <- e3_conceding$TFthc + e3_conceding$TFtac
#
# #Home shots conceded
# e3_home_hsc <- aggregate(E3$AST, by = list(E3$HomeTeam), FUN = sum)
# e3_away_asc <- aggregate(E3$HST, by = list(E3$AwayTeam), FUN = sum)
# e3_tsc <- merge(e3_home_hsc,e3_away_asc, by='Group.1',all = T)
# names(e3_tsc)[names(e3_tsc) == "x.x"] <- "hsc"
# names(e3_tsc)[names(e3_tsc) == "x.y"] <- "asc"
# e3_tsc$TSC <- e3_tsc$hsc + e3_tsc$asc
# #merge goals conceded and shots conceded
# e3_conceding_conversion <- merge(e3_tsc,e3_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# e3_conceding_conversion$HSCC <- percent(e3_conceding_conversion$TFthc/e3_conceding_conversion$hsc, accuracy = 0.01)
# e3_conceding_conversion$ASCC <- percent(e3_conceding_conversion$TFtac/e3_conceding_conversion$asc, accuracy = 0.01)
# e3_conceding_conversion$TSCC <- percent(e3_conceding_conversion$TGC/e3_conceding_conversion$TSC, accuracy = 0.01)
# e3_conceding_conversion$XSTC <- round(e3_scoring$TGS/(e3_tst$TST - e3_scoring$TGS), digits = 2)
#
# #merge the two parts
# e3_shots_analysis <- merge(e3_scoring_conversion,e3_conceding_conversion,by='Group.1',all = T)
# ################################################################################################################################
# #EC
# #home goals scored
# ec_home_gs <- aggregate(EC$FTHG, by = list(EC$HomeTeam), FUN = sum)
# ec_home_gs_avg <- aggregate(EC$FTHG, by = list(EC$HomeTeam),mean)
# ec_home_scoring <- merge(ec_home_gs,ec_home_gs_avg, by='Group.1',all = T)
# names(ec_home_scoring)[names(ec_home_scoring) == "x.x"] <- "TFthg"
# names(ec_home_scoring)[names(ec_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# ec_away_gs <- aggregate(EC$FTAG, by = list(EC$AwayTeam), FUN = sum)
# ec_away_gs_avg <- aggregate(EC$FTAG, by = list(EC$AwayTeam),mean)
# ec_away_scoring <- merge(ec_away_gs,ec_away_gs_avg, by='Group.1',all = T)
# names(ec_away_scoring)[names(ec_away_scoring) == "x.x"] <- "TFtag"
# names(ec_away_scoring)[names(ec_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# ec_scoring <- merge(ec_home_scoring,ec_away_scoring,by='Group.1',all = T)
# ec_scoring$TGS <- ec_scoring$TFthg + ec_scoring$TFtag
#
# #Home shots on target
# ec_home_hst <- aggregate(EC$HST, by = list(EC$HomeTeam), FUN = sum)
# ec_away_ast <- aggregate(EC$AST, by = list(EC$AwayTeam), FUN = sum)
# ec_tst <- merge(ec_home_hst,ec_away_ast, by='Group.1',all = T)
# names(ec_tst)[names(ec_tst) == "x.x"] <- "hst"
# names(ec_tst)[names(ec_tst) == "x.y"] <- "ast"
# ec_tst$TST <- ec_tst$hst + ec_tst$ast
# #merge goals scored and shots on target
# ec_scoring_conversion <- merge(ec_tst,ec_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# ec_scoring_conversion$HSTC <- percent(ec_scoring_conversion$TFthg/ec_scoring_conversion$hst, accuracy = 0.01)
# ec_scoring_conversion$ASTC <- percent(ec_scoring_conversion$TFtag/ec_scoring_conversion$ast, accuracy = 0.01)
# ec_scoring_conversion$TSTC <- percent(ec_scoring_conversion$TGS/ec_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# ec_scoring_conversion <- cbind(ec_scoring_conversion,ec_games_played)
# #create the second part
# #home goals conceded
# ec_home_gc <- aggregate(EC$FTAG, by = list(EC$HomeTeam), FUN = sum)
# ec_home_gc_avg <- aggregate(EC$FTAG, by = list(EC$HomeTeam),mean)
# ec_home_conceding <- merge(ec_home_gc,ec_home_gc_avg, by='Group.1',all = T)
# names(ec_home_conceding)[names(ec_home_conceding) == "x.x"] <- "TFthc"
# names(ec_home_conceding)[names(ec_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# ec_away_gc <- aggregate(EC$FTHG, by = list(EC$AwayTeam), FUN = sum)
# ec_away_gc_avg <- aggregate(EC$FTHG, by = list(EC$AwayTeam),mean)
# ec_away_conceding <- merge(ec_away_gc,ec_away_gc_avg, by='Group.1',all = T)
# names(ec_away_conceding)[names(ec_away_conceding) == "x.x"] <- "TFtac"
# names(ec_away_conceding)[names(ec_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# ec_conceding <- merge(ec_home_conceding,ec_away_conceding,by='Group.1',all = T)
# ec_conceding$TGC <- ec_conceding$TFthc + ec_conceding$TFtac
#
# #Home shots conceded
# ec_home_hsc <- aggregate(EC$AST, by = list(EC$HomeTeam), FUN = sum)
# ec_away_asc <- aggregate(EC$HST, by = list(EC$AwayTeam), FUN = sum)
# ec_tsc <- merge(ec_home_hsc,ec_away_asc, by='Group.1',all = T)
# names(ec_tsc)[names(ec_tsc) == "x.x"] <- "hsc"
# names(ec_tsc)[names(ec_tsc) == "x.y"] <- "asc"
# ec_tsc$TSC <- ec_tsc$hsc + ec_tsc$asc
# #merge goals conceded and shots conceded
# ec_conceding_conversion <- merge(ec_tsc,ec_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# ec_conceding_conversion$HSCC <- percent(ec_conceding_conversion$TFthc/ec_conceding_conversion$hsc, accuracy = 0.01)
# ec_conceding_conversion$ASCC <- percent(ec_conceding_conversion$TFtac/ec_conceding_conversion$asc, accuracy = 0.01)
# ec_conceding_conversion$TSCC <- percent(ec_conceding_conversion$TGC/ec_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# ec_shots_analysis <- merge(ec_scoring_conversion,ec_conceding_conversion,by='Group.1',all = T)
# #EC
# #home goals scored
# ec_home_gs <- aggregate(EC$FTHG, by = list(EC$HomeTeam), FUN = sum)
# ec_home_gs_avg <- aggregate(EC$FTHG, by = list(EC$HomeTeam),mean)
# ec_home_scoring <- merge(ec_home_gs,ec_home_gs_avg, by='Group.1',all = T)
# names(ec_home_scoring)[names(ec_home_scoring) == "x.x"] <- "TFthg"
# names(ec_home_scoring)[names(ec_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# ec_away_gs <- aggregate(EC$FTAG, by = list(EC$AwayTeam), FUN = sum)
# ec_away_gs_avg <- aggregate(EC$FTAG, by = list(EC$AwayTeam),mean)
# ec_away_scoring <- merge(ec_away_gs,ec_away_gs_avg, by='Group.1',all = T)
# names(ec_away_scoring)[names(ec_away_scoring) == "x.x"] <- "TFtag"
# names(ec_away_scoring)[names(ec_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# ec_scoring <- merge(ec_home_scoring,ec_away_scoring,by='Group.1',all = T)
# ec_scoring$TGS <- ec_scoring$TFthg + ec_scoring$TFtag
#
# #Home shots on target
# ec_home_hst <- aggregate(EC$HST, by = list(EC$HomeTeam), FUN = sum)
# ec_away_ast <- aggregate(EC$AST, by = list(EC$AwayTeam), FUN = sum)
# ec_tst <- merge(ec_home_hst,ec_away_ast, by='Group.1',all = T)
# names(ec_tst)[names(ec_tst) == "x.x"] <- "hst"
# names(ec_tst)[names(ec_tst) == "x.y"] <- "ast"
# ec_tst$TST <- ec_tst$hst + ec_tst$ast
# #merge goals scored and shots on target
# ec_scoring_conversion <- merge(ec_tst,ec_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# ec_scoring_conversion$HSTC <- percent(ec_scoring_conversion$TFthg/ec_scoring_conversion$hst, accuracy = 0.01)
# ec_scoring_conversion$ASTC <- percent(ec_scoring_conversion$TFtag/ec_scoring_conversion$ast, accuracy = 0.01)
# ec_scoring_conversion$TSTC <- percent(ec_scoring_conversion$TGS/ec_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# ec_scoring_conversion <- cbind(ec_scoring_conversion,ec_games_played)
# #create the second part
# #home goals conceded
# ec_home_gc <- aggregate(EC$FTAG, by = list(EC$HomeTeam), FUN = sum)
# ec_home_gc_avg <- aggregate(EC$FTAG, by = list(EC$HomeTeam),mean)
# ec_home_conceding <- merge(ec_home_gc,ec_home_gc_avg, by='Group.1',all = T)
# names(ec_home_conceding)[names(ec_home_conceding) == "x.x"] <- "TFthc"
# names(ec_home_conceding)[names(ec_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# ec_away_gc <- aggregate(EC$FTHG, by = list(EC$AwayTeam), FUN = sum)
# ec_away_gc_avg <- aggregate(EC$FTHG, by = list(EC$AwayTeam),mean)
# ec_away_conceding <- merge(ec_away_gc,ec_away_gc_avg, by='Group.1',all = T)
# names(ec_away_conceding)[names(ec_away_conceding) == "x.x"] <- "TFtac"
# names(ec_away_conceding)[names(ec_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# ec_conceding <- merge(ec_home_conceding,ec_away_conceding,by='Group.1',all = T)
# ec_conceding$TGC <- ec_conceding$TFthc + ec_conceding$TFtac
#
# #Home shots conceded
# ec_home_hsc <- aggregate(EC$AST, by = list(EC$HomeTeam), FUN = sum)
# ec_away_asc <- aggregate(EC$HST, by = list(EC$AwayTeam), FUN = sum)
# ec_tsc <- merge(ec_home_hsc,ec_away_asc, by='Group.1',all = T)
# names(ec_tsc)[names(ec_tsc) == "x.x"] <- "hsc"
# names(ec_tsc)[names(ec_tsc) == "x.y"] <- "asc"
# ec_tsc$TSC <- ec_tsc$hsc + ec_tsc$asc
# #merge goals conceded and shots conceded
# ec_conceding_conversion <- merge(ec_tsc,ec_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# ec_conceding_conversion$HSCC <- percent(ec_conceding_conversion$TFthc/ec_conceding_conversion$hsc, accuracy = 0.01)
# ec_conceding_conversion$ASCC <- percent(ec_conceding_conversion$TFtac/ec_conceding_conversion$asc, accuracy = 0.01)
# ec_conceding_conversion$TSCC <- percent(ec_conceding_conversion$TGC/ec_conceding_conversion$TSC, accuracy = 0.01)
# ec_conceding_conversion$XSTC <- round(ec_scoring$TGS/(ec_tst$TST - ec_scoring$TGS), digits = 2)
#
# #merge the two parts
# ec_shots_analysis <- merge(ec_scoring_conversion,ec_conceding_conversion,by='Group.1',all = T)
# ################################################################################################################################
# #F1
# #home goals scored
# f1_home_gs <- aggregate(F1$FTHG, by = list(F1$HomeTeam), FUN = sum)
# f1_home_gs_avg <- aggregate(F1$FTHG, by = list(F1$HomeTeam),mean)
# f1_home_scoring <- merge(f1_home_gs,f1_home_gs_avg, by='Group.1',all = T)
# names(f1_home_scoring)[names(f1_home_scoring) == "x.x"] <- "TFthg"
# names(f1_home_scoring)[names(f1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# f1_away_gs <- aggregate(F1$FTAG, by = list(F1$AwayTeam), FUN = sum)
# f1_away_gs_avg <- aggregate(F1$FTAG, by = list(F1$AwayTeam),mean)
# f1_away_scoring <- merge(f1_away_gs,f1_away_gs_avg, by='Group.1',all = T)
# names(f1_away_scoring)[names(f1_away_scoring) == "x.x"] <- "TFtag"
# names(f1_away_scoring)[names(f1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# f1_scoring <- merge(f1_home_scoring,f1_away_scoring,by='Group.1',all = T)
# f1_scoring$TGS <- f1_scoring$TFthg + f1_scoring$TFtag
#
# #Home shots on target
# f1_home_hst <- aggregate(F1$HST, by = list(F1$HomeTeam), FUN = sum)
# f1_away_ast <- aggregate(F1$AST, by = list(F1$AwayTeam), FUN = sum)
# f1_tst <- merge(f1_home_hst,f1_away_ast, by='Group.1',all = T)
# names(f1_tst)[names(f1_tst) == "x.x"] <- "hst"
# names(f1_tst)[names(f1_tst) == "x.y"] <- "ast"
# f1_tst$TST <- f1_tst$hst + f1_tst$ast
# #merge goals scored and shots on target
# f1_scoring_conversion <- merge(f1_tst,f1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# f1_scoring_conversion$HSTC <- percent(f1_scoring_conversion$TFthg/f1_scoring_conversion$hst, accuracy = 0.01)
# f1_scoring_conversion$ASTC <- percent(f1_scoring_conversion$TFtag/f1_scoring_conversion$ast, accuracy = 0.01)
# f1_scoring_conversion$TSTC <- percent(f1_scoring_conversion$TGS/f1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# f1_scoring_conversion <- cbind(f1_scoring_conversion,f1_games_played)
# #create the second part
# #home goals conceded
# f1_home_gc <- aggregate(F1$FTAG, by = list(F1$HomeTeam), FUN = sum)
# f1_home_gc_avg <- aggregate(F1$FTAG, by = list(F1$HomeTeam),mean)
# f1_home_conceding <- merge(f1_home_gc,f1_home_gc_avg, by='Group.1',all = T)
# names(f1_home_conceding)[names(f1_home_conceding) == "x.x"] <- "TFthc"
# names(f1_home_conceding)[names(f1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# f1_away_gc <- aggregate(F1$FTHG, by = list(F1$AwayTeam), FUN = sum)
# f1_away_gc_avg <- aggregate(F1$FTHG, by = list(F1$AwayTeam),mean)
# f1_away_conceding <- merge(f1_away_gc,f1_away_gc_avg, by='Group.1',all = T)
# names(f1_away_conceding)[names(f1_away_conceding) == "x.x"] <- "TFtac"
# names(f1_away_conceding)[names(f1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# f1_conceding <- merge(f1_home_conceding,f1_away_conceding,by='Group.1',all = T)
# f1_conceding$TGC <- f1_conceding$TFthc + f1_conceding$TFtac
#
# #Home shots conceded
# f1_home_hsc <- aggregate(F1$AST, by = list(F1$HomeTeam), FUN = sum)
# f1_away_asc <- aggregate(F1$HST, by = list(F1$AwayTeam), FUN = sum)
# f1_tsc <- merge(f1_home_hsc,f1_away_asc, by='Group.1',all = T)
# names(f1_tsc)[names(f1_tsc) == "x.x"] <- "hsc"
# names(f1_tsc)[names(f1_tsc) == "x.y"] <- "asc"
# f1_tsc$TSC <- f1_tsc$hsc + f1_tsc$asc
# #merge goals conceded and shots conceded
# f1_conceding_conversion <- merge(f1_tsc,f1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# f1_conceding_conversion$HSCC <- percent(f1_conceding_conversion$TFthc/f1_conceding_conversion$hsc, accuracy = 0.01)
# f1_conceding_conversion$ASCC <- percent(f1_conceding_conversion$TFtac/f1_conceding_conversion$asc, accuracy = 0.01)
# f1_conceding_conversion$TSCC <- percent(f1_conceding_conversion$TGC/f1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# f1_shots_analysis <- merge(f1_scoring_conversion,f1_conceding_conversion,by='Group.1',all = T)
# #F1
# #home goals scored
# f1_home_gs <- aggregate(F1$FTHG, by = list(F1$HomeTeam), FUN = sum)
# f1_home_gs_avg <- aggregate(F1$FTHG, by = list(F1$HomeTeam),mean)
# f1_home_scoring <- merge(f1_home_gs,f1_home_gs_avg, by='Group.1',all = T)
# names(f1_home_scoring)[names(f1_home_scoring) == "x.x"] <- "TFthg"
# names(f1_home_scoring)[names(f1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# f1_away_gs <- aggregate(F1$FTAG, by = list(F1$AwayTeam), FUN = sum)
# f1_away_gs_avg <- aggregate(F1$FTAG, by = list(F1$AwayTeam),mean)
# f1_away_scoring <- merge(f1_away_gs,f1_away_gs_avg, by='Group.1',all = T)
# names(f1_away_scoring)[names(f1_away_scoring) == "x.x"] <- "TFtag"
# names(f1_away_scoring)[names(f1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# f1_scoring <- merge(f1_home_scoring,f1_away_scoring,by='Group.1',all = T)
# f1_scoring$TGS <- f1_scoring$TFthg + f1_scoring$TFtag
#
# #Home shots on target
# f1_home_hst <- aggregate(F1$HST, by = list(F1$HomeTeam), FUN = sum)
# f1_away_ast <- aggregate(F1$AST, by = list(F1$AwayTeam), FUN = sum)
# f1_tst <- merge(f1_home_hst,f1_away_ast, by='Group.1',all = T)
# names(f1_tst)[names(f1_tst) == "x.x"] <- "hst"
# names(f1_tst)[names(f1_tst) == "x.y"] <- "ast"
# f1_tst$TST <- f1_tst$hst + f1_tst$ast
# #merge goals scored and shots on target
# f1_scoring_conversion <- merge(f1_tst,f1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# f1_scoring_conversion$HSTC <- percent(f1_scoring_conversion$TFthg/f1_scoring_conversion$hst, accuracy = 0.01)
# f1_scoring_conversion$ASTC <- percent(f1_scoring_conversion$TFtag/f1_scoring_conversion$ast, accuracy = 0.01)
# f1_scoring_conversion$TSTC <- percent(f1_scoring_conversion$TGS/f1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# f1_scoring_conversion <- cbind(f1_scoring_conversion,f1_games_played)
# #create the second part
# #home goals conceded
# f1_home_gc <- aggregate(F1$FTAG, by = list(F1$HomeTeam), FUN = sum)
# f1_home_gc_avg <- aggregate(F1$FTAG, by = list(F1$HomeTeam),mean)
# f1_home_conceding <- merge(f1_home_gc,f1_home_gc_avg, by='Group.1',all = T)
# names(f1_home_conceding)[names(f1_home_conceding) == "x.x"] <- "TFthc"
# names(f1_home_conceding)[names(f1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# f1_away_gc <- aggregate(F1$FTHG, by = list(F1$AwayTeam), FUN = sum)
# f1_away_gc_avg <- aggregate(F1$FTHG, by = list(F1$AwayTeam),mean)
# f1_away_conceding <- merge(f1_away_gc,f1_away_gc_avg, by='Group.1',all = T)
# names(f1_away_conceding)[names(f1_away_conceding) == "x.x"] <- "TFtac"
# names(f1_away_conceding)[names(f1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# f1_conceding <- merge(f1_home_conceding,f1_away_conceding,by='Group.1',all = T)
# f1_conceding$TGC <- f1_conceding$TFthc + f1_conceding$TFtac
#
# #Home shots conceded
# f1_home_hsc <- aggregate(F1$AST, by = list(F1$HomeTeam), FUN = sum)
# f1_away_asc <- aggregate(F1$HST, by = list(F1$AwayTeam), FUN = sum)
# f1_tsc <- merge(f1_home_hsc,f1_away_asc, by='Group.1',all = T)
# names(f1_tsc)[names(f1_tsc) == "x.x"] <- "hsc"
# names(f1_tsc)[names(f1_tsc) == "x.y"] <- "asc"
# f1_tsc$TSC <- f1_tsc$hsc + f1_tsc$asc
# #merge goals conceded and shots conceded
# f1_conceding_conversion <- merge(f1_tsc,f1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# f1_conceding_conversion$HSCC <- percent(f1_conceding_conversion$TFthc/f1_conceding_conversion$hsc, accuracy = 0.01)
# f1_conceding_conversion$ASCC <- percent(f1_conceding_conversion$TFtac/f1_conceding_conversion$asc, accuracy = 0.01)
# f1_conceding_conversion$TSCC <- percent(f1_conceding_conversion$TGC/f1_conceding_conversion$TSC, accuracy = 0.01)
# f1_conceding_conversion$XSTC <- round(f1_scoring$TGS/(f1_tst$TST - f1_scoring$TGS), digits = 2)
#
# #merge the two parts
# f1_shots_analysis <- merge(f1_scoring_conversion,f1_conceding_conversion,by='Group.1',all = T)
# #############################################################################################################################
# #F2
# #home goals scored
# f2_home_gs <- aggregate(F2$FTHG, by = list(F2$HomeTeam), FUN = sum)
# f2_home_gs_avg <- aggregate(F2$FTHG, by = list(F2$HomeTeam),mean)
# f2_home_scoring <- merge(f2_home_gs,f2_home_gs_avg, by='Group.1',all = T)
# names(f2_home_scoring)[names(f2_home_scoring) == "x.x"] <- "TFthg"
# names(f2_home_scoring)[names(f2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# f2_away_gs <- aggregate(F2$FTAG, by = list(F2$AwayTeam), FUN = sum)
# f2_away_gs_avg <- aggregate(F2$FTAG, by = list(F2$AwayTeam),mean)
# f2_away_scoring <- merge(f2_away_gs,f2_away_gs_avg, by='Group.1',all = T)
# names(f2_away_scoring)[names(f2_away_scoring) == "x.x"] <- "TFtag"
# names(f2_away_scoring)[names(f2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# f2_scoring <- merge(f2_home_scoring,f2_away_scoring,by='Group.1',all = T)
# f2_scoring$TGS <- f2_scoring$TFthg + f2_scoring$TFtag
#
# #Home shots on target
# f2_home_hst <- aggregate(F2$HST, by = list(F2$HomeTeam), FUN = sum)
# f2_away_ast <- aggregate(F2$AST, by = list(F2$AwayTeam), FUN = sum)
# f2_tst <- merge(f2_home_hst,f2_away_ast, by='Group.1',all = T)
# names(f2_tst)[names(f2_tst) == "x.x"] <- "hst"
# names(f2_tst)[names(f2_tst) == "x.y"] <- "ast"
# f2_tst$TST <- f2_tst$hst + f2_tst$ast
# #merge goals scored and shots on target
# f2_scoring_conversion <- merge(f2_tst,f2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# f2_scoring_conversion$HSTC <- percent(f2_scoring_conversion$TFthg/f2_scoring_conversion$hst, accuracy = 0.01)
# f2_scoring_conversion$ASTC <- percent(f2_scoring_conversion$TFtag/f2_scoring_conversion$ast, accuracy = 0.01)
# f2_scoring_conversion$TSTC <- percent(f2_scoring_conversion$TGS/f2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# f2_scoring_conversion <- cbind(f2_scoring_conversion,f2_games_played)
# #create the second part
# #home goals conceded
# f2_home_gc <- aggregate(F2$FTAG, by = list(F2$HomeTeam), FUN = sum)
# f2_home_gc_avg <- aggregate(F2$FTAG, by = list(F2$HomeTeam),mean)
# f2_home_conceding <- merge(f2_home_gc,f2_home_gc_avg, by='Group.1',all = T)
# names(f2_home_conceding)[names(f2_home_conceding) == "x.x"] <- "TFthc"
# names(f2_home_conceding)[names(f2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# f2_away_gc <- aggregate(F2$FTHG, by = list(F2$AwayTeam), FUN = sum)
# f2_away_gc_avg <- aggregate(F2$FTHG, by = list(F2$AwayTeam),mean)
# f2_away_conceding <- merge(f2_away_gc,f2_away_gc_avg, by='Group.1',all = T)
# names(f2_away_conceding)[names(f2_away_conceding) == "x.x"] <- "TFtac"
# names(f2_away_conceding)[names(f2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# f2_conceding <- merge(f2_home_conceding,f2_away_conceding,by='Group.1',all = T)
# f2_conceding$TGC <- f2_conceding$TFthc + f2_conceding$TFtac
#
# #Home shots conceded
# f2_home_hsc <- aggregate(F2$AST, by = list(F2$HomeTeam), FUN = sum)
# f2_away_asc <- aggregate(F2$HST, by = list(F2$AwayTeam), FUN = sum)
# f2_tsc <- merge(f2_home_hsc,f2_away_asc, by='Group.1',all = T)
# names(f2_tsc)[names(f2_tsc) == "x.x"] <- "hsc"
# names(f2_tsc)[names(f2_tsc) == "x.y"] <- "asc"
# f2_tsc$TSC <- f2_tsc$hsc + f2_tsc$asc
# #merge goals conceded and shots conceded
# f2_conceding_conversion <- merge(f2_tsc,f2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# f2_conceding_conversion$HSCC <- percent(f2_conceding_conversion$TFthc/f2_conceding_conversion$hsc, accuracy = 0.01)
# f2_conceding_conversion$ASCC <- percent(f2_conceding_conversion$TFtac/f2_conceding_conversion$asc, accuracy = 0.01)
# f2_conceding_conversion$TSCC <- percent(f2_conceding_conversion$TGC/f2_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# f2_shots_analysis <- merge(f2_scoring_conversion,f2_conceding_conversion,by='Group.1',all = T)
# #F2
# #home goals scored
# f2_home_gs <- aggregate(F2$FTHG, by = list(F2$HomeTeam), FUN = sum)
# f2_home_gs_avg <- aggregate(F2$FTHG, by = list(F2$HomeTeam),mean)
# f2_home_scoring <- merge(f2_home_gs,f2_home_gs_avg, by='Group.1',all = T)
# names(f2_home_scoring)[names(f2_home_scoring) == "x.x"] <- "TFthg"
# names(f2_home_scoring)[names(f2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# f2_away_gs <- aggregate(F2$FTAG, by = list(F2$AwayTeam), FUN = sum)
# f2_away_gs_avg <- aggregate(F2$FTAG, by = list(F2$AwayTeam),mean)
# f2_away_scoring <- merge(f2_away_gs,f2_away_gs_avg, by='Group.1',all = T)
# names(f2_away_scoring)[names(f2_away_scoring) == "x.x"] <- "TFtag"
# names(f2_away_scoring)[names(f2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# f2_scoring <- merge(f2_home_scoring,f2_away_scoring,by='Group.1',all = T)
# f2_scoring$TGS <- f2_scoring$TFthg + f2_scoring$TFtag
#
# #Home shots on target
# f2_home_hst <- aggregate(F2$HST, by = list(F2$HomeTeam), FUN = sum)
# f2_away_ast <- aggregate(F2$AST, by = list(F2$AwayTeam), FUN = sum)
# f2_tst <- merge(f2_home_hst,f2_away_ast, by='Group.1',all = T)
# names(f2_tst)[names(f2_tst) == "x.x"] <- "hst"
# names(f2_tst)[names(f2_tst) == "x.y"] <- "ast"
# f2_tst$TST <- f2_tst$hst + f2_tst$ast
# #merge goals scored and shots on target
# f2_scoring_conversion <- merge(f2_tst,f2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# f2_scoring_conversion$HSTC <- percent(f2_scoring_conversion$TFthg/f2_scoring_conversion$hst, accuracy = 0.01)
# f2_scoring_conversion$ASTC <- percent(f2_scoring_conversion$TFtag/f2_scoring_conversion$ast, accuracy = 0.01)
# f2_scoring_conversion$TSTC <- percent(f2_scoring_conversion$TGS/f2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# f2_scoring_conversion <- cbind(f2_scoring_conversion,f2_games_played)
# #create the second part
# #home goals conceded
# f2_home_gc <- aggregate(F2$FTAG, by = list(F2$HomeTeam), FUN = sum)
# f2_home_gc_avg <- aggregate(F2$FTAG, by = list(F2$HomeTeam),mean)
# f2_home_conceding <- merge(f2_home_gc,f2_home_gc_avg, by='Group.1',all = T)
# names(f2_home_conceding)[names(f2_home_conceding) == "x.x"] <- "TFthc"
# names(f2_home_conceding)[names(f2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# f2_away_gc <- aggregate(F2$FTHG, by = list(F2$AwayTeam), FUN = sum)
# f2_away_gc_avg <- aggregate(F2$FTHG, by = list(F2$AwayTeam),mean)
# f2_away_conceding <- merge(f2_away_gc,f2_away_gc_avg, by='Group.1',all = T)
# names(f2_away_conceding)[names(f2_away_conceding) == "x.x"] <- "TFtac"
# names(f2_away_conceding)[names(f2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# f2_conceding <- merge(f2_home_conceding,f2_away_conceding,by='Group.1',all = T)
# f2_conceding$TGC <- f2_conceding$TFthc + f2_conceding$TFtac
#
# #Home shots conceded
# f2_home_hsc <- aggregate(F2$AST, by = list(F2$HomeTeam), FUN = sum)
# f2_away_asc <- aggregate(F2$HST, by = list(F2$AwayTeam), FUN = sum)
# f2_tsc <- merge(f2_home_hsc,f2_away_asc, by='Group.1',all = T)
# names(f2_tsc)[names(f2_tsc) == "x.x"] <- "hsc"
# names(f2_tsc)[names(f2_tsc) == "x.y"] <- "asc"
# f2_tsc$TSC <- f2_tsc$hsc + f2_tsc$asc
# #merge goals conceded and shots conceded
# f2_conceding_conversion <- merge(f2_tsc,f2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# f2_conceding_conversion$HSCC <- percent(f2_conceding_conversion$TFthc/f2_conceding_conversion$hsc, accuracy = 0.01)
# f2_conceding_conversion$ASCC <- percent(f2_conceding_conversion$TFtac/f2_conceding_conversion$asc, accuracy = 0.01)
# f2_conceding_conversion$TSCC <- percent(f2_conceding_conversion$TGC/f2_conceding_conversion$TSC, accuracy = 0.01)
# f2_conceding_conversion$XSTC <- round(f2_scoring$TGS/(f2_tst$TST - f2_scoring$TGS), digits = 2)
#
# #merge the two parts
# f2_shots_analysis <- merge(f2_scoring_conversion,f2_conceding_conversion,by='Group.1',all = T)
# ###########################################################################################################################
# #G1
# #home goals scored
# g1_home_gs <- aggregate(G1$FTHG, by = list(G1$HomeTeam), FUN = sum)
# g1_home_gs_avg <- aggregate(G1$FTHG, by = list(G1$HomeTeam),mean)
# g1_home_scoring <- merge(g1_home_gs,g1_home_gs_avg, by='Group.1',all = T)
# names(g1_home_scoring)[names(g1_home_scoring) == "x.x"] <- "TFthg"
# names(g1_home_scoring)[names(g1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# g1_away_gs <- aggregate(G1$FTAG, by = list(G1$AwayTeam), FUN = sum)
# g1_away_gs_avg <- aggregate(G1$FTAG, by = list(G1$AwayTeam),mean)
# g1_away_scoring <- merge(g1_away_gs,g1_away_gs_avg, by='Group.1',all = T)
# names(g1_away_scoring)[names(g1_away_scoring) == "x.x"] <- "TFtag"
# names(g1_away_scoring)[names(g1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# g1_scoring <- merge(g1_home_scoring,g1_away_scoring,by='Group.1',all = T)
# g1_scoring$TGS <- g1_scoring$TFthg + g1_scoring$TFtag
#
# #Home shots on target
# g1_home_hst <- aggregate(G1$HST, by = list(G1$HomeTeam), FUN = sum)
# g1_away_ast <- aggregate(G1$AST, by = list(G1$AwayTeam), FUN = sum)
# g1_tst <- merge(g1_home_hst,g1_away_ast, by='Group.1',all = T)
# names(g1_tst)[names(g1_tst) == "x.x"] <- "hst"
# names(g1_tst)[names(g1_tst) == "x.y"] <- "ast"
# g1_tst$TST <- g1_tst$hst + g1_tst$ast
# #merge goals scored and shots on target
# g1_scoring_conversion <- merge(g1_tst,g1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# g1_scoring_conversion$HSTC <- percent(g1_scoring_conversion$TFthg/g1_scoring_conversion$hst, accuracy = 0.01)
# g1_scoring_conversion$ASTC <- percent(g1_scoring_conversion$TFtag/g1_scoring_conversion$ast, accuracy = 0.01)
# g1_scoring_conversion$TSTC <- percent(g1_scoring_conversion$TGS/g1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# g1_scoring_conversion <- cbind(g1_scoring_conversion,g1_games_played)
# #create the second part
# #home goals conceded
# g1_home_gc <- aggregate(G1$FTAG, by = list(G1$HomeTeam), FUN = sum)
# g1_home_gc_avg <- aggregate(G1$FTAG, by = list(G1$HomeTeam),mean)
# g1_home_conceding <- merge(g1_home_gc,g1_home_gc_avg, by='Group.1',all = T)
# names(g1_home_conceding)[names(g1_home_conceding) == "x.x"] <- "TFthc"
# names(g1_home_conceding)[names(g1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# g1_away_gc <- aggregate(G1$FTHG, by = list(G1$AwayTeam), FUN = sum)
# g1_away_gc_avg <- aggregate(G1$FTHG, by = list(G1$AwayTeam),mean)
# g1_away_conceding <- merge(g1_away_gc,g1_away_gc_avg, by='Group.1',all = T)
# names(g1_away_conceding)[names(g1_away_conceding) == "x.x"] <- "TFtac"
# names(g1_away_conceding)[names(g1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# g1_conceding <- merge(g1_home_conceding,g1_away_conceding,by='Group.1',all = T)
# g1_conceding$TGC <- g1_conceding$TFthc + g1_conceding$TFtac
#
# #Home shots conceded
# g1_home_hsc <- aggregate(G1$AST, by = list(G1$HomeTeam), FUN = sum)
# g1_away_asc <- aggregate(G1$HST, by = list(G1$AwayTeam), FUN = sum)
# g1_tsc <- merge(g1_home_hsc,g1_away_asc, by='Group.1',all = T)
# names(g1_tsc)[names(g1_tsc) == "x.x"] <- "hsc"
# names(g1_tsc)[names(g1_tsc) == "x.y"] <- "asc"
# g1_tsc$TSC <- g1_tsc$hsc + g1_tsc$asc
# #merge goals conceded and shots conceded
# g1_conceding_conversion <- merge(g1_tsc,g1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# g1_conceding_conversion$HSCC <- percent(g1_conceding_conversion$TFthc/g1_conceding_conversion$hsc, accuracy = 0.01)
# g1_conceding_conversion$ASCC <- percent(g1_conceding_conversion$TFtac/g1_conceding_conversion$asc, accuracy = 0.01)
# g1_conceding_conversion$TSCC <- percent(g1_conceding_conversion$TGC/g1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# g1_shots_analysis <- merge(g1_scoring_conversion,g1_conceding_conversion,by='Group.1',all = T)
# #G1
# #home goals scored
# g1_home_gs <- aggregate(G1$FTHG, by = list(G1$HomeTeam), FUN = sum)
# g1_home_gs_avg <- aggregate(G1$FTHG, by = list(G1$HomeTeam),mean)
# g1_home_scoring <- merge(g1_home_gs,g1_home_gs_avg, by='Group.1',all = T)
# names(g1_home_scoring)[names(g1_home_scoring) == "x.x"] <- "TFthg"
# names(g1_home_scoring)[names(g1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# g1_away_gs <- aggregate(G1$FTAG, by = list(G1$AwayTeam), FUN = sum)
# g1_away_gs_avg <- aggregate(G1$FTAG, by = list(G1$AwayTeam),mean)
# g1_away_scoring <- merge(g1_away_gs,g1_away_gs_avg, by='Group.1',all = T)
# names(g1_away_scoring)[names(g1_away_scoring) == "x.x"] <- "TFtag"
# names(g1_away_scoring)[names(g1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# g1_scoring <- merge(g1_home_scoring,g1_away_scoring,by='Group.1',all = T)
# g1_scoring$TGS <- g1_scoring$TFthg + g1_scoring$TFtag
#
# #Home shots on target
# g1_home_hst <- aggregate(G1$HST, by = list(G1$HomeTeam), FUN = sum)
# g1_away_ast <- aggregate(G1$AST, by = list(G1$AwayTeam), FUN = sum)
# g1_tst <- merge(g1_home_hst,g1_away_ast, by='Group.1',all = T)
# names(g1_tst)[names(g1_tst) == "x.x"] <- "hst"
# names(g1_tst)[names(g1_tst) == "x.y"] <- "ast"
# g1_tst$TST <- g1_tst$hst + g1_tst$ast
# #merge goals scored and shots on target
# g1_scoring_conversion <- merge(g1_tst,g1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# g1_scoring_conversion$HSTC <- percent(g1_scoring_conversion$TFthg/g1_scoring_conversion$hst, accuracy = 0.01)
# g1_scoring_conversion$ASTC <- percent(g1_scoring_conversion$TFtag/g1_scoring_conversion$ast, accuracy = 0.01)
# g1_scoring_conversion$TSTC <- percent(g1_scoring_conversion$TGS/g1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# g1_scoring_conversion <- cbind(g1_scoring_conversion,g1_games_played)
# #create the second part
# #home goals conceded
# g1_home_gc <- aggregate(G1$FTAG, by = list(G1$HomeTeam), FUN = sum)
# g1_home_gc_avg <- aggregate(G1$FTAG, by = list(G1$HomeTeam),mean)
# g1_home_conceding <- merge(g1_home_gc,g1_home_gc_avg, by='Group.1',all = T)
# names(g1_home_conceding)[names(g1_home_conceding) == "x.x"] <- "TFthc"
# names(g1_home_conceding)[names(g1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# g1_away_gc <- aggregate(G1$FTHG, by = list(G1$AwayTeam), FUN = sum)
# g1_away_gc_avg <- aggregate(G1$FTHG, by = list(G1$AwayTeam),mean)
# g1_away_conceding <- merge(g1_away_gc,g1_away_gc_avg, by='Group.1',all = T)
# names(g1_away_conceding)[names(g1_away_conceding) == "x.x"] <- "TFtac"
# names(g1_away_conceding)[names(g1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# g1_conceding <- merge(g1_home_conceding,g1_away_conceding,by='Group.1',all = T)
# g1_conceding$TGC <- g1_conceding$TFthc + g1_conceding$TFtac
#
# #Home shots conceded
# g1_home_hsc <- aggregate(G1$AST, by = list(G1$HomeTeam), FUN = sum)
# g1_away_asc <- aggregate(G1$HST, by = list(G1$AwayTeam), FUN = sum)
# g1_tsc <- merge(g1_home_hsc,g1_away_asc, by='Group.1',all = T)
# names(g1_tsc)[names(g1_tsc) == "x.x"] <- "hsc"
# names(g1_tsc)[names(g1_tsc) == "x.y"] <- "asc"
# g1_tsc$TSC <- g1_tsc$hsc + g1_tsc$asc
# #merge goals conceded and shots conceded
# g1_conceding_conversion <- merge(g1_tsc,g1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# g1_conceding_conversion$HSCC <- percent(g1_conceding_conversion$TFthc/g1_conceding_conversion$hsc, accuracy = 0.01)
# g1_conceding_conversion$ASCC <- percent(g1_conceding_conversion$TFtac/g1_conceding_conversion$asc, accuracy = 0.01)
# g1_conceding_conversion$TSCC <- percent(g1_conceding_conversion$TGC/g1_conceding_conversion$TSC, accuracy = 0.01)
# g1_conceding_conversion$XSTC <- round(g1_scoring$TGS/(g1_tst$TST - g1_scoring$TGS), digits = 2)
#
# #merge the two parts
# g1_shots_analysis <- merge(g1_scoring_conversion,g1_conceding_conversion,by='Group.1',all = T)
# ###############################################################################################################################
# #I1
# #home goals scored
# i1_home_gs <- aggregate(I1$FTHG, by = list(I1$HomeTeam), FUN = sum)
# i1_home_gs_avg <- aggregate(I1$FTHG, by = list(I1$HomeTeam),mean)
# i1_home_scoring <- merge(i1_home_gs,i1_home_gs_avg, by='Group.1',all = T)
# names(i1_home_scoring)[names(i1_home_scoring) == "x.x"] <- "TFthg"
# names(i1_home_scoring)[names(i1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# i1_away_gs <- aggregate(I1$FTAG, by = list(I1$AwayTeam), FUN = sum)
# i1_away_gs_avg <- aggregate(I1$FTAG, by = list(I1$AwayTeam),mean)
# i1_away_scoring <- merge(i1_away_gs,i1_away_gs_avg, by='Group.1',all = T)
# names(i1_away_scoring)[names(i1_away_scoring) == "x.x"] <- "TFtag"
# names(i1_away_scoring)[names(i1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# i1_scoring <- merge(i1_home_scoring,i1_away_scoring,by='Group.1',all = T)
# i1_scoring$TGS <- i1_scoring$TFthg + i1_scoring$TFtag
#
# #Home shots on target
# i1_home_hst <- aggregate(I1$HST, by = list(I1$HomeTeam), FUN = sum)
# i1_away_ast <- aggregate(I1$AST, by = list(I1$AwayTeam), FUN = sum)
# i1_tst <- merge(i1_home_hst,i1_away_ast, by='Group.1',all = T)
# names(i1_tst)[names(i1_tst) == "x.x"] <- "hst"
# names(i1_tst)[names(i1_tst) == "x.y"] <- "ast"
# i1_tst$TST <- i1_tst$hst + i1_tst$ast
# #merge goals scored and shots on target
# i1_scoring_conversion <- merge(i1_tst,i1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# i1_scoring_conversion$HSTC <- percent(i1_scoring_conversion$TFthg/i1_scoring_conversion$hst, accuracy = 0.01)
# i1_scoring_conversion$ASTC <- percent(i1_scoring_conversion$TFtag/i1_scoring_conversion$ast, accuracy = 0.01)
# i1_scoring_conversion$TSTC <- percent(i1_scoring_conversion$TGS/i1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# i1_scoring_conversion <- cbind(i1_scoring_conversion,i1_games_played)
# #create the second part
# #home goals conceded
# i1_home_gc <- aggregate(I1$FTAG, by = list(I1$HomeTeam), FUN = sum)
# i1_home_gc_avg <- aggregate(I1$FTAG, by = list(I1$HomeTeam),mean)
# i1_home_conceding <- merge(i1_home_gc,i1_home_gc_avg, by='Group.1',all = T)
# names(i1_home_conceding)[names(i1_home_conceding) == "x.x"] <- "TFthc"
# names(i1_home_conceding)[names(i1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# i1_away_gc <- aggregate(I1$FTHG, by = list(I1$AwayTeam), FUN = sum)
# i1_away_gc_avg <- aggregate(I1$FTHG, by = list(I1$AwayTeam),mean)
# i1_away_conceding <- merge(i1_away_gc,i1_away_gc_avg, by='Group.1',all = T)
# names(i1_away_conceding)[names(i1_away_conceding) == "x.x"] <- "TFtac"
# names(i1_away_conceding)[names(i1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# i1_conceding <- merge(i1_home_conceding,i1_away_conceding,by='Group.1',all = T)
# i1_conceding$TGC <- i1_conceding$TFthc + i1_conceding$TFtac
#
# #Home shots conceded
# i1_home_hsc <- aggregate(I1$AST, by = list(I1$HomeTeam), FUN = sum)
# i1_away_asc <- aggregate(I1$HST, by = list(I1$AwayTeam), FUN = sum)
# i1_tsc <- merge(i1_home_hsc,i1_away_asc, by='Group.1',all = T)
# names(i1_tsc)[names(i1_tsc) == "x.x"] <- "hsc"
# names(i1_tsc)[names(i1_tsc) == "x.y"] <- "asc"
# i1_tsc$TSC <- i1_tsc$hsc + i1_tsc$asc
# #merge goals conceded and shots conceded
# i1_conceding_conversion <- merge(i1_tsc,i1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# i1_conceding_conversion$HSCC <- percent(i1_conceding_conversion$TFthc/i1_conceding_conversion$hsc, accuracy = 0.01)
# i1_conceding_conversion$ASCC <- percent(i1_conceding_conversion$TFtac/i1_conceding_conversion$asc, accuracy = 0.01)
# i1_conceding_conversion$TSCC <- percent(i1_conceding_conversion$TGC/i1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# i1_shots_analysis <- merge(i1_scoring_conversion,i1_conceding_conversion,by='Group.1',all = T)
# #I1
# #home goals scored
# i1_home_gs <- aggregate(I1$FTHG, by = list(I1$HomeTeam), FUN = sum)
# i1_home_gs_avg <- aggregate(I1$FTHG, by = list(I1$HomeTeam),mean)
# i1_home_scoring <- merge(i1_home_gs,i1_home_gs_avg, by='Group.1',all = T)
# names(i1_home_scoring)[names(i1_home_scoring) == "x.x"] <- "TFthg"
# names(i1_home_scoring)[names(i1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# i1_away_gs <- aggregate(I1$FTAG, by = list(I1$AwayTeam), FUN = sum)
# i1_away_gs_avg <- aggregate(I1$FTAG, by = list(I1$AwayTeam),mean)
# i1_away_scoring <- merge(i1_away_gs,i1_away_gs_avg, by='Group.1',all = T)
# names(i1_away_scoring)[names(i1_away_scoring) == "x.x"] <- "TFtag"
# names(i1_away_scoring)[names(i1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# i1_scoring <- merge(i1_home_scoring,i1_away_scoring,by='Group.1',all = T)
# i1_scoring$TGS <- i1_scoring$TFthg + i1_scoring$TFtag
#
# #Home shots on target
# i1_home_hst <- aggregate(I1$HST, by = list(I1$HomeTeam), FUN = sum)
# i1_away_ast <- aggregate(I1$AST, by = list(I1$AwayTeam), FUN = sum)
# i1_tst <- merge(i1_home_hst,i1_away_ast, by='Group.1',all = T)
# names(i1_tst)[names(i1_tst) == "x.x"] <- "hst"
# names(i1_tst)[names(i1_tst) == "x.y"] <- "ast"
# i1_tst$TST <- i1_tst$hst + i1_tst$ast
# #merge goals scored and shots on target
# i1_scoring_conversion <- merge(i1_tst,i1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# i1_scoring_conversion$HSTC <- percent(i1_scoring_conversion$TFthg/i1_scoring_conversion$hst, accuracy = 0.01)
# i1_scoring_conversion$ASTC <- percent(i1_scoring_conversion$TFtag/i1_scoring_conversion$ast, accuracy = 0.01)
# i1_scoring_conversion$TSTC <- percent(i1_scoring_conversion$TGS/i1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# i1_scoring_conversion <- cbind(i1_scoring_conversion,i1_games_played)
# #create the second part
# #home goals conceded
# i1_home_gc <- aggregate(I1$FTAG, by = list(I1$HomeTeam), FUN = sum)
# i1_home_gc_avg <- aggregate(I1$FTAG, by = list(I1$HomeTeam),mean)
# i1_home_conceding <- merge(i1_home_gc,i1_home_gc_avg, by='Group.1',all = T)
# names(i1_home_conceding)[names(i1_home_conceding) == "x.x"] <- "TFthc"
# names(i1_home_conceding)[names(i1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# i1_away_gc <- aggregate(I1$FTHG, by = list(I1$AwayTeam), FUN = sum)
# i1_away_gc_avg <- aggregate(I1$FTHG, by = list(I1$AwayTeam),mean)
# i1_away_conceding <- merge(i1_away_gc,i1_away_gc_avg, by='Group.1',all = T)
# names(i1_away_conceding)[names(i1_away_conceding) == "x.x"] <- "TFtac"
# names(i1_away_conceding)[names(i1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# i1_conceding <- merge(i1_home_conceding,i1_away_conceding,by='Group.1',all = T)
# i1_conceding$TGC <- i1_conceding$TFthc + i1_conceding$TFtac
#
# #Home shots conceded
# i1_home_hsc <- aggregate(I1$AST, by = list(I1$HomeTeam), FUN = sum)
# i1_away_asc <- aggregate(I1$HST, by = list(I1$AwayTeam), FUN = sum)
# i1_tsc <- merge(i1_home_hsc,i1_away_asc, by='Group.1',all = T)
# names(i1_tsc)[names(i1_tsc) == "x.x"] <- "hsc"
# names(i1_tsc)[names(i1_tsc) == "x.y"] <- "asc"
# i1_tsc$TSC <- i1_tsc$hsc + i1_tsc$asc
# #merge goals conceded and shots conceded
# i1_conceding_conversion <- merge(i1_tsc,i1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# i1_conceding_conversion$HSCC <- percent(i1_conceding_conversion$TFthc/i1_conceding_conversion$hsc, accuracy = 0.01)
# i1_conceding_conversion$ASCC <- percent(i1_conceding_conversion$TFtac/i1_conceding_conversion$asc, accuracy = 0.01)
# i1_conceding_conversion$TSCC <- percent(i1_conceding_conversion$TGC/i1_conceding_conversion$TSC, accuracy = 0.01)
# i1_conceding_conversion$XSTC <- round(i1_scoring$TGS/(i1_tst$TST - i1_scoring$TGS), digits = 2)
#
# #merge the two parts
# i1_shots_analysis <- merge(i1_scoring_conversion,i1_conceding_conversion,by='Group.1',all = T)
# #########################################################################################################################
# #I2
# #home goals scored
# i2_home_gs <- aggregate(I2$FTHG, by = list(I2$HomeTeam), FUN = sum)
# i2_home_gs_avg <- aggregate(I2$FTHG, by = list(I2$HomeTeam),mean)
# i2_home_scoring <- merge(i2_home_gs,i2_home_gs_avg, by='Group.1',all = T)
# names(i2_home_scoring)[names(i2_home_scoring) == "x.x"] <- "TFthg"
# names(i2_home_scoring)[names(i2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# i2_away_gs <- aggregate(I2$FTAG, by = list(I2$AwayTeam), FUN = sum)
# i2_away_gs_avg <- aggregate(I2$FTAG, by = list(I2$AwayTeam),mean)
# i2_away_scoring <- merge(i2_away_gs,i2_away_gs_avg, by='Group.1',all = T)
# names(i2_away_scoring)[names(i2_away_scoring) == "x.x"] <- "TFtag"
# names(i2_away_scoring)[names(i2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# i2_scoring <- merge(i2_home_scoring,i2_away_scoring,by='Group.1',all = T)
# i2_scoring$TGS <- i2_scoring$TFthg + i2_scoring$TFtag
#
# #Home shots on target
# i2_home_hst <- aggregate(I2$HST, by = list(I2$HomeTeam), FUN = sum)
# i2_away_ast <- aggregate(I2$AST, by = list(I2$AwayTeam), FUN = sum)
# i2_tst <- merge(i2_home_hst,i2_away_ast, by='Group.1',all = T)
# names(i2_tst)[names(i2_tst) == "x.x"] <- "hst"
# names(i2_tst)[names(i2_tst) == "x.y"] <- "ast"
# i2_tst$TST <- i2_tst$hst + i2_tst$ast
# #merge goals scored and shots on target
# i2_scoring_conversion <- merge(i2_tst,i2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# i2_scoring_conversion$HSTC <- percent(i2_scoring_conversion$TFthg/i2_scoring_conversion$hst, accuracy = 0.01)
# i2_scoring_conversion$ASTC <- percent(i2_scoring_conversion$TFtag/i2_scoring_conversion$ast, accuracy = 0.01)
# i2_scoring_conversion$TSTC <- percent(i2_scoring_conversion$TGS/i2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# i2_scoring_conversion <- cbind(i2_scoring_conversion,i2_games_played)
# #create the second part
# #home goals conceded
# i2_home_gc <- aggregate(I2$FTAG, by = list(I2$HomeTeam), FUN = sum)
# i2_home_gc_avg <- aggregate(I2$FTAG, by = list(I2$HomeTeam),mean)
# i2_home_conceding <- merge(i2_home_gc,i2_home_gc_avg, by='Group.1',all = T)
# names(i2_home_conceding)[names(i2_home_conceding) == "x.x"] <- "TFthc"
# names(i2_home_conceding)[names(i2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# i2_away_gc <- aggregate(I2$FTHG, by = list(I2$AwayTeam), FUN = sum)
# i2_away_gc_avg <- aggregate(I2$FTHG, by = list(I2$AwayTeam),mean)
# i2_away_conceding <- merge(i2_away_gc,i2_away_gc_avg, by='Group.1',all = T)
# names(i2_away_conceding)[names(i2_away_conceding) == "x.x"] <- "TFtac"
# names(i2_away_conceding)[names(i2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# i2_conceding <- merge(i2_home_conceding,i2_away_conceding,by='Group.1',all = T)
# i2_conceding$TGC <- i2_conceding$TFthc + i2_conceding$TFtac
#
# #Home shots conceded
# i2_home_hsc <- aggregate(I2$AST, by = list(I2$HomeTeam), FUN = sum)
# i2_away_asc <- aggregate(I2$HST, by = list(I2$AwayTeam), FUN = sum)
# i2_tsc <- merge(i2_home_hsc,i2_away_asc, by='Group.1',all = T)
# names(i2_tsc)[names(i2_tsc) == "x.x"] <- "hsc"
# names(i2_tsc)[names(i2_tsc) == "x.y"] <- "asc"
# i2_tsc$TSC <- i2_tsc$hsc + i2_tsc$asc
# #merge goals conceded and shots conceded
# i2_conceding_conversion <- merge(i2_tsc,i2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# i2_conceding_conversion$HSCC <- percent(i2_conceding_conversion$TFthc/i2_conceding_conversion$hsc, accuracy = 0.01)
# i2_conceding_conversion$ASCC <- percent(i2_conceding_conversion$TFtac/i2_conceding_conversion$asc, accuracy = 0.01)
# i2_conceding_conversion$TSCC <- percent(i2_conceding_conversion$TGC/i2_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# i2_shots_analysis <- merge(i2_scoring_conversion,i2_conceding_conversion,by='Group.1',all = T)
# #I2
# #home goals scored
# i2_home_gs <- aggregate(I2$FTHG, by = list(I2$HomeTeam), FUN = sum)
# i2_home_gs_avg <- aggregate(I2$FTHG, by = list(I2$HomeTeam),mean)
# i2_home_scoring <- merge(i2_home_gs,i2_home_gs_avg, by='Group.1',all = T)
# names(i2_home_scoring)[names(i2_home_scoring) == "x.x"] <- "TFthg"
# names(i2_home_scoring)[names(i2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# i2_away_gs <- aggregate(I2$FTAG, by = list(I2$AwayTeam), FUN = sum)
# i2_away_gs_avg <- aggregate(I2$FTAG, by = list(I2$AwayTeam),mean)
# i2_away_scoring <- merge(i2_away_gs,i2_away_gs_avg, by='Group.1',all = T)
# names(i2_away_scoring)[names(i2_away_scoring) == "x.x"] <- "TFtag"
# names(i2_away_scoring)[names(i2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# i2_scoring <- merge(i2_home_scoring,i2_away_scoring,by='Group.1',all = T)
# i2_scoring$TGS <- i2_scoring$TFthg + i2_scoring$TFtag
#
# #Home shots on target
# i2_home_hst <- aggregate(I2$HST, by = list(I2$HomeTeam), FUN = sum)
# i2_away_ast <- aggregate(I2$AST, by = list(I2$AwayTeam), FUN = sum)
# i2_tst <- merge(i2_home_hst,i2_away_ast, by='Group.1',all = T)
# names(i2_tst)[names(i2_tst) == "x.x"] <- "hst"
# names(i2_tst)[names(i2_tst) == "x.y"] <- "ast"
# i2_tst$TST <- i2_tst$hst + i2_tst$ast
# #merge goals scored and shots on target
# i2_scoring_conversion <- merge(i2_tst,i2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# i2_scoring_conversion$HSTC <- percent(i2_scoring_conversion$TFthg/i2_scoring_conversion$hst, accuracy = 0.01)
# i2_scoring_conversion$ASTC <- percent(i2_scoring_conversion$TFtag/i2_scoring_conversion$ast, accuracy = 0.01)
# i2_scoring_conversion$TSTC <- percent(i2_scoring_conversion$TGS/i2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# i2_scoring_conversion <- cbind(i2_scoring_conversion,i2_games_played)
# #create the second part
# #home goals conceded
# i2_home_gc <- aggregate(I2$FTAG, by = list(I2$HomeTeam), FUN = sum)
# i2_home_gc_avg <- aggregate(I2$FTAG, by = list(I2$HomeTeam),mean)
# i2_home_conceding <- merge(i2_home_gc,i2_home_gc_avg, by='Group.1',all = T)
# names(i2_home_conceding)[names(i2_home_conceding) == "x.x"] <- "TFthc"
# names(i2_home_conceding)[names(i2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# i2_away_gc <- aggregate(I2$FTHG, by = list(I2$AwayTeam), FUN = sum)
# i2_away_gc_avg <- aggregate(I2$FTHG, by = list(I2$AwayTeam),mean)
# i2_away_conceding <- merge(i2_away_gc,i2_away_gc_avg, by='Group.1',all = T)
# names(i2_away_conceding)[names(i2_away_conceding) == "x.x"] <- "TFtac"
# names(i2_away_conceding)[names(i2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# i2_conceding <- merge(i2_home_conceding,i2_away_conceding,by='Group.1',all = T)
# i2_conceding$TGC <- i2_conceding$TFthc + i2_conceding$TFtac
#
# #Home shots conceded
# i2_home_hsc <- aggregate(I2$AST, by = list(I2$HomeTeam), FUN = sum)
# i2_away_asc <- aggregate(I2$HST, by = list(I2$AwayTeam), FUN = sum)
# i2_tsc <- merge(i2_home_hsc,i2_away_asc, by='Group.1',all = T)
# names(i2_tsc)[names(i2_tsc) == "x.x"] <- "hsc"
# names(i2_tsc)[names(i2_tsc) == "x.y"] <- "asc"
# i2_tsc$TSC <- i2_tsc$hsc + i2_tsc$asc
# #merge goals conceded and shots conceded
# i2_conceding_conversion <- merge(i2_tsc,i2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# i2_conceding_conversion$HSCC <- percent(i2_conceding_conversion$TFthc/i2_conceding_conversion$hsc, accuracy = 0.01)
# i2_conceding_conversion$ASCC <- percent(i2_conceding_conversion$TFtac/i2_conceding_conversion$asc, accuracy = 0.01)
# i2_conceding_conversion$TSCC <- percent(i2_conceding_conversion$TGC/i2_conceding_conversion$TSC, accuracy = 0.01)
# i2_conceding_conversion$XSTC <- round(i2_scoring$TGS/(i2_tst$TST - i2_scoring$TGS), digits = 2)
#
# #merge the two parts
# i2_shots_analysis <- merge(i2_scoring_conversion,i2_conceding_conversion,by='Group.1',all = T)
# ###############################################################################################################################
# #N1
# #home goals scored
# n1_home_gs <- aggregate(N1$FTHG, by = list(N1$HomeTeam), FUN = sum)
# n1_home_gs_avg <- aggregate(N1$FTHG, by = list(N1$HomeTeam),mean)
# n1_home_scoring <- merge(n1_home_gs,n1_home_gs_avg, by='Group.1',all = T)
# names(n1_home_scoring)[names(n1_home_scoring) == "x.x"] <- "TFthg"
# names(n1_home_scoring)[names(n1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# n1_away_gs <- aggregate(N1$FTAG, by = list(N1$AwayTeam), FUN = sum)
# n1_away_gs_avg <- aggregate(N1$FTAG, by = list(N1$AwayTeam),mean)
# n1_away_scoring <- merge(n1_away_gs,n1_away_gs_avg, by='Group.1',all = T)
# names(n1_away_scoring)[names(n1_away_scoring) == "x.x"] <- "TFtag"
# names(n1_away_scoring)[names(n1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# n1_scoring <- merge(n1_home_scoring,n1_away_scoring,by='Group.1',all = T)
# n1_scoring$TGS <- n1_scoring$TFthg + n1_scoring$TFtag
#
# #Home shots on target
# n1_home_hst <- aggregate(N1$HST, by = list(N1$HomeTeam), FUN = sum)
# n1_away_ast <- aggregate(N1$AST, by = list(N1$AwayTeam), FUN = sum)
# n1_tst <- merge(n1_home_hst,n1_away_ast, by='Group.1',all = T)
# names(n1_tst)[names(n1_tst) == "x.x"] <- "hst"
# names(n1_tst)[names(n1_tst) == "x.y"] <- "ast"
# n1_tst$TST <- n1_tst$hst + n1_tst$ast
# #merge goals scored and shots on target
# n1_scoring_conversion <- merge(n1_tst,n1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# n1_scoring_conversion$HSTC <- percent(n1_scoring_conversion$TFthg/n1_scoring_conversion$hst, accuracy = 0.01)
# n1_scoring_conversion$ASTC <- percent(n1_scoring_conversion$TFtag/n1_scoring_conversion$ast, accuracy = 0.01)
# n1_scoring_conversion$TSTC <- percent(n1_scoring_conversion$TGS/n1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# n1_scoring_conversion <- cbind(n1_scoring_conversion,n1_games_played)
# #create the second part
# #home goals conceded
# n1_home_gc <- aggregate(N1$FTAG, by = list(N1$HomeTeam), FUN = sum)
# n1_home_gc_avg <- aggregate(N1$FTAG, by = list(N1$HomeTeam),mean)
# n1_home_conceding <- merge(n1_home_gc,n1_home_gc_avg, by='Group.1',all = T)
# names(n1_home_conceding)[names(n1_home_conceding) == "x.x"] <- "TFthc"
# names(n1_home_conceding)[names(n1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# n1_away_gc <- aggregate(N1$FTHG, by = list(N1$AwayTeam), FUN = sum)
# n1_away_gc_avg <- aggregate(N1$FTHG, by = list(N1$AwayTeam),mean)
# n1_away_conceding <- merge(n1_away_gc,n1_away_gc_avg, by='Group.1',all = T)
# names(n1_away_conceding)[names(n1_away_conceding) == "x.x"] <- "TFtac"
# names(n1_away_conceding)[names(n1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# n1_conceding <- merge(n1_home_conceding,n1_away_conceding,by='Group.1',all = T)
# n1_conceding$TGC <- n1_conceding$TFthc + n1_conceding$TFtac
#
# #Home shots conceded
# n1_home_hsc <- aggregate(N1$AST, by = list(N1$HomeTeam), FUN = sum)
# n1_away_asc <- aggregate(N1$HST, by = list(N1$AwayTeam), FUN = sum)
# n1_tsc <- merge(n1_home_hsc,n1_away_asc, by='Group.1',all = T)
# names(n1_tsc)[names(n1_tsc) == "x.x"] <- "hsc"
# names(n1_tsc)[names(n1_tsc) == "x.y"] <- "asc"
# n1_tsc$TSC <- n1_tsc$hsc + n1_tsc$asc
# #merge goals conceded and shots conceded
# n1_conceding_conversion <- merge(n1_tsc,n1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# n1_conceding_conversion$HSCC <- percent(n1_conceding_conversion$TFthc/n1_conceding_conversion$hsc, accuracy = 0.01)
# n1_conceding_conversion$ASCC <- percent(n1_conceding_conversion$TFtac/n1_conceding_conversion$asc, accuracy = 0.01)
# n1_conceding_conversion$TSCC <- percent(n1_conceding_conversion$TGC/n1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# n1_shots_analysis <- merge(n1_scoring_conversion,n1_conceding_conversion,by='Group.1',all = T)
# #N1
# #home goals scored
# n1_home_gs <- aggregate(N1$FTHG, by = list(N1$HomeTeam), FUN = sum)
# n1_home_gs_avg <- aggregate(N1$FTHG, by = list(N1$HomeTeam),mean)
# n1_home_scoring <- merge(n1_home_gs,n1_home_gs_avg, by='Group.1',all = T)
# names(n1_home_scoring)[names(n1_home_scoring) == "x.x"] <- "TFthg"
# names(n1_home_scoring)[names(n1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# n1_away_gs <- aggregate(N1$FTAG, by = list(N1$AwayTeam), FUN = sum)
# n1_away_gs_avg <- aggregate(N1$FTAG, by = list(N1$AwayTeam),mean)
# n1_away_scoring <- merge(n1_away_gs,n1_away_gs_avg, by='Group.1',all = T)
# names(n1_away_scoring)[names(n1_away_scoring) == "x.x"] <- "TFtag"
# names(n1_away_scoring)[names(n1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# n1_scoring <- merge(n1_home_scoring,n1_away_scoring,by='Group.1',all = T)
# n1_scoring$TGS <- n1_scoring$TFthg + n1_scoring$TFtag
#
# #Home shots on target
# n1_home_hst <- aggregate(N1$HST, by = list(N1$HomeTeam), FUN = sum)
# n1_away_ast <- aggregate(N1$AST, by = list(N1$AwayTeam), FUN = sum)
# n1_tst <- merge(n1_home_hst,n1_away_ast, by='Group.1',all = T)
# names(n1_tst)[names(n1_tst) == "x.x"] <- "hst"
# names(n1_tst)[names(n1_tst) == "x.y"] <- "ast"
# n1_tst$TST <- n1_tst$hst + n1_tst$ast
# #merge goals scored and shots on target
# n1_scoring_conversion <- merge(n1_tst,n1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# n1_scoring_conversion$HSTC <- percent(n1_scoring_conversion$TFthg/n1_scoring_conversion$hst, accuracy = 0.01)
# n1_scoring_conversion$ASTC <- percent(n1_scoring_conversion$TFtag/n1_scoring_conversion$ast, accuracy = 0.01)
# n1_scoring_conversion$TSTC <- percent(n1_scoring_conversion$TGS/n1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# n1_scoring_conversion <- cbind(n1_scoring_conversion,n1_games_played)
# #create the second part
# #home goals conceded
# n1_home_gc <- aggregate(N1$FTAG, by = list(N1$HomeTeam), FUN = sum)
# n1_home_gc_avg <- aggregate(N1$FTAG, by = list(N1$HomeTeam),mean)
# n1_home_conceding <- merge(n1_home_gc,n1_home_gc_avg, by='Group.1',all = T)
# names(n1_home_conceding)[names(n1_home_conceding) == "x.x"] <- "TFthc"
# names(n1_home_conceding)[names(n1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# n1_away_gc <- aggregate(N1$FTHG, by = list(N1$AwayTeam), FUN = sum)
# n1_away_gc_avg <- aggregate(N1$FTHG, by = list(N1$AwayTeam),mean)
# n1_away_conceding <- merge(n1_away_gc,n1_away_gc_avg, by='Group.1',all = T)
# names(n1_away_conceding)[names(n1_away_conceding) == "x.x"] <- "TFtac"
# names(n1_away_conceding)[names(n1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# n1_conceding <- merge(n1_home_conceding,n1_away_conceding,by='Group.1',all = T)
# n1_conceding$TGC <- n1_conceding$TFthc + n1_conceding$TFtac
#
# #Home shots conceded
# n1_home_hsc <- aggregate(N1$AST, by = list(N1$HomeTeam), FUN = sum)
# n1_away_asc <- aggregate(N1$HST, by = list(N1$AwayTeam), FUN = sum)
# n1_tsc <- merge(n1_home_hsc,n1_away_asc, by='Group.1',all = T)
# names(n1_tsc)[names(n1_tsc) == "x.x"] <- "hsc"
# names(n1_tsc)[names(n1_tsc) == "x.y"] <- "asc"
# n1_tsc$TSC <- n1_tsc$hsc + n1_tsc$asc
# #merge goals conceded and shots conceded
# n1_conceding_conversion <- merge(n1_tsc,n1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# n1_conceding_conversion$HSCC <- percent(n1_conceding_conversion$TFthc/n1_conceding_conversion$hsc, accuracy = 0.01)
# n1_conceding_conversion$ASCC <- percent(n1_conceding_conversion$TFtac/n1_conceding_conversion$asc, accuracy = 0.01)
# n1_conceding_conversion$TSCC <- percent(n1_conceding_conversion$TGC/n1_conceding_conversion$TSC, accuracy = 0.01)
# n1_conceding_conversion$XSTC <- round(n1_scoring$TGS/(n1_tst$TST - n1_scoring$TGS), digits = 2)
#
# #merge the two parts
# n1_shots_analysis <- merge(n1_scoring_conversion,n1_conceding_conversion,by='Group.1',all = T)
# ################################################################################################################################
# #P1
# #home goals scored
# p1_home_gs <- aggregate(P1$FTHG, by = list(P1$HomeTeam), FUN = sum)
# p1_home_gs_avg <- aggregate(P1$FTHG, by = list(P1$HomeTeam),mean)
# p1_home_scoring <- merge(p1_home_gs,p1_home_gs_avg, by='Group.1',all = T)
# names(p1_home_scoring)[names(p1_home_scoring) == "x.x"] <- "TFthg"
# names(p1_home_scoring)[names(p1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# p1_away_gs <- aggregate(P1$FTAG, by = list(P1$AwayTeam), FUN = sum)
# p1_away_gs_avg <- aggregate(P1$FTAG, by = list(P1$AwayTeam),mean)
# p1_away_scoring <- merge(p1_away_gs,p1_away_gs_avg, by='Group.1',all = T)
# names(p1_away_scoring)[names(p1_away_scoring) == "x.x"] <- "TFtag"
# names(p1_away_scoring)[names(p1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# p1_scoring <- merge(p1_home_scoring,p1_away_scoring,by='Group.1',all = T)
# p1_scoring$TGS <- p1_scoring$TFthg + p1_scoring$TFtag
#
# #Home shots on target
# p1_home_hst <- aggregate(P1$HST, by = list(P1$HomeTeam), FUN = sum)
# p1_away_ast <- aggregate(P1$AST, by = list(P1$AwayTeam), FUN = sum)
# p1_tst <- merge(p1_home_hst,p1_away_ast, by='Group.1',all = T)
# names(p1_tst)[names(p1_tst) == "x.x"] <- "hst"
# names(p1_tst)[names(p1_tst) == "x.y"] <- "ast"
# p1_tst$TST <- p1_tst$hst + p1_tst$ast
# #merge goals scored and shots on target
# p1_scoring_conversion <- merge(p1_tst,p1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# p1_scoring_conversion$HSTC <- percent(p1_scoring_conversion$TFthg/p1_scoring_conversion$hst, accuracy = 0.01)
# p1_scoring_conversion$ASTC <- percent(p1_scoring_conversion$TFtag/p1_scoring_conversion$ast, accuracy = 0.01)
# p1_scoring_conversion$TSTC <- percent(p1_scoring_conversion$TGS/p1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# p1_scoring_conversion <- cbind(p1_scoring_conversion,p1_games_played)
# #create the second part
# #home goals conceded
# p1_home_gc <- aggregate(P1$FTAG, by = list(P1$HomeTeam), FUN = sum)
# p1_home_gc_avg <- aggregate(P1$FTAG, by = list(P1$HomeTeam),mean)
# p1_home_conceding <- merge(p1_home_gc,p1_home_gc_avg, by='Group.1',all = T)
# names(p1_home_conceding)[names(p1_home_conceding) == "x.x"] <- "TFthc"
# names(p1_home_conceding)[names(p1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# p1_away_gc <- aggregate(P1$FTHG, by = list(P1$AwayTeam), FUN = sum)
# p1_away_gc_avg <- aggregate(P1$FTHG, by = list(P1$AwayTeam),mean)
# p1_away_conceding <- merge(p1_away_gc,p1_away_gc_avg, by='Group.1',all = T)
# names(p1_away_conceding)[names(p1_away_conceding) == "x.x"] <- "TFtac"
# names(p1_away_conceding)[names(p1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# p1_conceding <- merge(p1_home_conceding,p1_away_conceding,by='Group.1',all = T)
# p1_conceding$TGC <- p1_conceding$TFthc + p1_conceding$TFtac
#
# #Home shots conceded
# p1_home_hsc <- aggregate(P1$AST, by = list(P1$HomeTeam), FUN = sum)
# p1_away_asc <- aggregate(P1$HST, by = list(P1$AwayTeam), FUN = sum)
# p1_tsc <- merge(p1_home_hsc,p1_away_asc, by='Group.1',all = T)
# names(p1_tsc)[names(p1_tsc) == "x.x"] <- "hsc"
# names(p1_tsc)[names(p1_tsc) == "x.y"] <- "asc"
# p1_tsc$TSC <- p1_tsc$hsc + p1_tsc$asc
# #merge goals conceded and shots conceded
# p1_conceding_conversion <- merge(p1_tsc,p1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# p1_conceding_conversion$HSCC <- percent(p1_conceding_conversion$TFthc/p1_conceding_conversion$hsc, accuracy = 0.01)
# p1_conceding_conversion$ASCC <- percent(p1_conceding_conversion$TFtac/p1_conceding_conversion$asc, accuracy = 0.01)
# p1_conceding_conversion$TSCC <- percent(p1_conceding_conversion$TGC/p1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# p1_shots_analysis <- merge(p1_scoring_conversion,p1_conceding_conversion,by='Group.1',all = T)
# #P1
# #home goals scored
# p1_home_gs <- aggregate(P1$FTHG, by = list(P1$HomeTeam), FUN = sum)
# p1_home_gs_avg <- aggregate(P1$FTHG, by = list(P1$HomeTeam),mean)
# p1_home_scoring <- merge(p1_home_gs,p1_home_gs_avg, by='Group.1',all = T)
# names(p1_home_scoring)[names(p1_home_scoring) == "x.x"] <- "TFthg"
# names(p1_home_scoring)[names(p1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# p1_away_gs <- aggregate(P1$FTAG, by = list(P1$AwayTeam), FUN = sum)
# p1_away_gs_avg <- aggregate(P1$FTAG, by = list(P1$AwayTeam),mean)
# p1_away_scoring <- merge(p1_away_gs,p1_away_gs_avg, by='Group.1',all = T)
# names(p1_away_scoring)[names(p1_away_scoring) == "x.x"] <- "TFtag"
# names(p1_away_scoring)[names(p1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# p1_scoring <- merge(p1_home_scoring,p1_away_scoring,by='Group.1',all = T)
# p1_scoring$TGS <- p1_scoring$TFthg + p1_scoring$TFtag
#
# #Home shots on target
# p1_home_hst <- aggregate(P1$HST, by = list(P1$HomeTeam), FUN = sum)
# p1_away_ast <- aggregate(P1$AST, by = list(P1$AwayTeam), FUN = sum)
# p1_tst <- merge(p1_home_hst,p1_away_ast, by='Group.1',all = T)
# names(p1_tst)[names(p1_tst) == "x.x"] <- "hst"
# names(p1_tst)[names(p1_tst) == "x.y"] <- "ast"
# p1_tst$TST <- p1_tst$hst + p1_tst$ast
# #merge goals scored and shots on target
# p1_scoring_conversion <- merge(p1_tst,p1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# p1_scoring_conversion$HSTC <- percent(p1_scoring_conversion$TFthg/p1_scoring_conversion$hst, accuracy = 0.01)
# p1_scoring_conversion$ASTC <- percent(p1_scoring_conversion$TFtag/p1_scoring_conversion$ast, accuracy = 0.01)
# p1_scoring_conversion$TSTC <- percent(p1_scoring_conversion$TGS/p1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# p1_scoring_conversion <- cbind(p1_scoring_conversion,p1_games_played)
# #create the second part
# #home goals conceded
# p1_home_gc <- aggregate(P1$FTAG, by = list(P1$HomeTeam), FUN = sum)
# p1_home_gc_avg <- aggregate(P1$FTAG, by = list(P1$HomeTeam),mean)
# p1_home_conceding <- merge(p1_home_gc,p1_home_gc_avg, by='Group.1',all = T)
# names(p1_home_conceding)[names(p1_home_conceding) == "x.x"] <- "TFthc"
# names(p1_home_conceding)[names(p1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# p1_away_gc <- aggregate(P1$FTHG, by = list(P1$AwayTeam), FUN = sum)
# p1_away_gc_avg <- aggregate(P1$FTHG, by = list(P1$AwayTeam),mean)
# p1_away_conceding <- merge(p1_away_gc,p1_away_gc_avg, by='Group.1',all = T)
# names(p1_away_conceding)[names(p1_away_conceding) == "x.x"] <- "TFtac"
# names(p1_away_conceding)[names(p1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# p1_conceding <- merge(p1_home_conceding,p1_away_conceding,by='Group.1',all = T)
# p1_conceding$TGC <- p1_conceding$TFthc + p1_conceding$TFtac
#
# #Home shots conceded
# p1_home_hsc <- aggregate(P1$AST, by = list(P1$HomeTeam), FUN = sum)
# p1_away_asc <- aggregate(P1$HST, by = list(P1$AwayTeam), FUN = sum)
# p1_tsc <- merge(p1_home_hsc,p1_away_asc, by='Group.1',all = T)
# names(p1_tsc)[names(p1_tsc) == "x.x"] <- "hsc"
# names(p1_tsc)[names(p1_tsc) == "x.y"] <- "asc"
# p1_tsc$TSC <- p1_tsc$hsc + p1_tsc$asc
# #merge goals conceded and shots conceded
# p1_conceding_conversion <- merge(p1_tsc,p1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# p1_conceding_conversion$HSCC <- percent(p1_conceding_conversion$TFthc/p1_conceding_conversion$hsc, accuracy = 0.01)
# p1_conceding_conversion$ASCC <- percent(p1_conceding_conversion$TFtac/p1_conceding_conversion$asc, accuracy = 0.01)
# p1_conceding_conversion$TSCC <- percent(p1_conceding_conversion$TGC/p1_conceding_conversion$TSC, accuracy = 0.01)
# p1_conceding_conversion$XSTC <- round(p1_scoring$TGS/(p1_tst$TST - p1_scoring$TGS), digits = 2)
#
# #merge the two parts
# p1_shots_analysis <- merge(p1_scoring_conversion,p1_conceding_conversion,by='Group.1',all = T)
# ##########################################################################################################################
# #SC0
# #home goals scored
# sc0_home_gs <- aggregate(SC0$FTHG, by = list(SC0$HomeTeam), FUN = sum)
# sc0_home_gs_avg <- aggregate(SC0$FTHG, by = list(SC0$HomeTeam),mean)
# sc0_home_scoring <- merge(sc0_home_gs,sc0_home_gs_avg, by='Group.1',all = T)
# names(sc0_home_scoring)[names(sc0_home_scoring) == "x.x"] <- "TFthg"
# names(sc0_home_scoring)[names(sc0_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc0_away_gs <- aggregate(SC0$FTAG, by = list(SC0$AwayTeam), FUN = sum)
# sc0_away_gs_avg <- aggregate(SC0$FTAG, by = list(SC0$AwayTeam),mean)
# sc0_away_scoring <- merge(sc0_away_gs,sc0_away_gs_avg, by='Group.1',all = T)
# names(sc0_away_scoring)[names(sc0_away_scoring) == "x.x"] <- "TFtag"
# names(sc0_away_scoring)[names(sc0_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc0_scoring <- merge(sc0_home_scoring,sc0_away_scoring,by='Group.1',all = T)
# sc0_scoring$TGS <- sc0_scoring$TFthg + sc0_scoring$TFtag
#
# #Home shots on target
# sc0_home_hst <- aggregate(SC0$HST, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_ast <- aggregate(SC0$AST, by = list(SC0$AwayTeam), FUN = sum)
# sc0_tst <- merge(sc0_home_hst,sc0_away_ast, by='Group.1',all = T)
# names(sc0_tst)[names(sc0_tst) == "x.x"] <- "hst"
# names(sc0_tst)[names(sc0_tst) == "x.y"] <- "ast"
# sc0_tst$TST <- sc0_tst$hst + sc0_tst$ast
# #merge goals scored and shots on target
# sc0_scoring_conversion <- merge(sc0_tst,sc0_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc0_scoring_conversion$HSTC <- percent(sc0_scoring_conversion$TFthg/sc0_scoring_conversion$hst, accuracy = 0.01)
# sc0_scoring_conversion$ASTC <- percent(sc0_scoring_conversion$TFtag/sc0_scoring_conversion$ast, accuracy = 0.01)
# sc0_scoring_conversion$TSTC <- percent(sc0_scoring_conversion$TGS/sc0_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc0_scoring_conversion <- cbind(sc0_scoring_conversion,sc0_games_played)
# #create the second part
# #home goals conceded
# sc0_home_gc <- aggregate(SC0$FTAG, by = list(SC0$HomeTeam), FUN = sum)
# sc0_home_gc_avg <- aggregate(SC0$FTAG, by = list(SC0$HomeTeam),mean)
# sc0_home_conceding <- merge(sc0_home_gc,sc0_home_gc_avg, by='Group.1',all = T)
# names(sc0_home_conceding)[names(sc0_home_conceding) == "x.x"] <- "TFthc"
# names(sc0_home_conceding)[names(sc0_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc0_away_gc <- aggregate(SC0$FTHG, by = list(SC0$AwayTeam), FUN = sum)
# sc0_away_gc_avg <- aggregate(SC0$FTHG, by = list(SC0$AwayTeam),mean)
# sc0_away_conceding <- merge(sc0_away_gc,sc0_away_gc_avg, by='Group.1',all = T)
# names(sc0_away_conceding)[names(sc0_away_conceding) == "x.x"] <- "TFtac"
# names(sc0_away_conceding)[names(sc0_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc0_conceding <- merge(sc0_home_conceding,sc0_away_conceding,by='Group.1',all = T)
# sc0_conceding$TGC <- sc0_conceding$TFthc + sc0_conceding$TFtac
#
# #Home shots conceded
# sc0_home_hsc <- aggregate(SC0$AST, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_asc <- aggregate(SC0$HST, by = list(SC0$AwayTeam), FUN = sum)
# sc0_tsc <- merge(sc0_home_hsc,sc0_away_asc, by='Group.1',all = T)
# names(sc0_tsc)[names(sc0_tsc) == "x.x"] <- "hsc"
# names(sc0_tsc)[names(sc0_tsc) == "x.y"] <- "asc"
# sc0_tsc$TSC <- sc0_tsc$hsc + sc0_tsc$asc
# #merge goals conceded and shots conceded
# sc0_conceding_conversion <- merge(sc0_tsc,sc0_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc0_conceding_conversion$HSCC <- percent(sc0_conceding_conversion$TFthc/sc0_conceding_conversion$hsc, accuracy = 0.01)
# sc0_conceding_conversion$ASCC <- percent(sc0_conceding_conversion$TFtac/sc0_conceding_conversion$asc, accuracy = 0.01)
# sc0_conceding_conversion$TSCC <- percent(sc0_conceding_conversion$TGC/sc0_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# sc0_shots_analysis <- merge(sc0_scoring_conversion,sc0_conceding_conversion,by='Group.1',all = T)
# #SC0
# #home goals scored
# sc0_home_gs <- aggregate(SC0$FTHG, by = list(SC0$HomeTeam), FUN = sum)
# sc0_home_gs_avg <- aggregate(SC0$FTHG, by = list(SC0$HomeTeam),mean)
# sc0_home_scoring <- merge(sc0_home_gs,sc0_home_gs_avg, by='Group.1',all = T)
# names(sc0_home_scoring)[names(sc0_home_scoring) == "x.x"] <- "TFthg"
# names(sc0_home_scoring)[names(sc0_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc0_away_gs <- aggregate(SC0$FTAG, by = list(SC0$AwayTeam), FUN = sum)
# sc0_away_gs_avg <- aggregate(SC0$FTAG, by = list(SC0$AwayTeam),mean)
# sc0_away_scoring <- merge(sc0_away_gs,sc0_away_gs_avg, by='Group.1',all = T)
# names(sc0_away_scoring)[names(sc0_away_scoring) == "x.x"] <- "TFtag"
# names(sc0_away_scoring)[names(sc0_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc0_scoring <- merge(sc0_home_scoring,sc0_away_scoring,by='Group.1',all = T)
# sc0_scoring$TGS <- sc0_scoring$TFthg + sc0_scoring$TFtag
#
# #Home shots on target
# sc0_home_hst <- aggregate(SC0$HST, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_ast <- aggregate(SC0$AST, by = list(SC0$AwayTeam), FUN = sum)
# sc0_tst <- merge(sc0_home_hst,sc0_away_ast, by='Group.1',all = T)
# names(sc0_tst)[names(sc0_tst) == "x.x"] <- "hst"
# names(sc0_tst)[names(sc0_tst) == "x.y"] <- "ast"
# sc0_tst$TST <- sc0_tst$hst + sc0_tst$ast
# #merge goals scored and shots on target
# sc0_scoring_conversion <- merge(sc0_tst,sc0_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc0_scoring_conversion$HSTC <- percent(sc0_scoring_conversion$TFthg/sc0_scoring_conversion$hst, accuracy = 0.01)
# sc0_scoring_conversion$ASTC <- percent(sc0_scoring_conversion$TFtag/sc0_scoring_conversion$ast, accuracy = 0.01)
# sc0_scoring_conversion$TSTC <- percent(sc0_scoring_conversion$TGS/sc0_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc0_scoring_conversion <- cbind(sc0_scoring_conversion,sc0_games_played)
# #create the second part
# #home goals conceded
# sc0_home_gc <- aggregate(SC0$FTAG, by = list(SC0$HomeTeam), FUN = sum)
# sc0_home_gc_avg <- aggregate(SC0$FTAG, by = list(SC0$HomeTeam),mean)
# sc0_home_conceding <- merge(sc0_home_gc,sc0_home_gc_avg, by='Group.1',all = T)
# names(sc0_home_conceding)[names(sc0_home_conceding) == "x.x"] <- "TFthc"
# names(sc0_home_conceding)[names(sc0_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc0_away_gc <- aggregate(SC0$FTHG, by = list(SC0$AwayTeam), FUN = sum)
# sc0_away_gc_avg <- aggregate(SC0$FTHG, by = list(SC0$AwayTeam),mean)
# sc0_away_conceding <- merge(sc0_away_gc,sc0_away_gc_avg, by='Group.1',all = T)
# names(sc0_away_conceding)[names(sc0_away_conceding) == "x.x"] <- "TFtac"
# names(sc0_away_conceding)[names(sc0_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc0_conceding <- merge(sc0_home_conceding,sc0_away_conceding,by='Group.1',all = T)
# sc0_conceding$TGC <- sc0_conceding$TFthc + sc0_conceding$TFtac
#
# #Home shots conceded
# sc0_home_hsc <- aggregate(SC0$AST, by = list(SC0$HomeTeam), FUN = sum)
# sc0_away_asc <- aggregate(SC0$HST, by = list(SC0$AwayTeam), FUN = sum)
# sc0_tsc <- merge(sc0_home_hsc,sc0_away_asc, by='Group.1',all = T)
# names(sc0_tsc)[names(sc0_tsc) == "x.x"] <- "hsc"
# names(sc0_tsc)[names(sc0_tsc) == "x.y"] <- "asc"
# sc0_tsc$TSC <- sc0_tsc$hsc + sc0_tsc$asc
# #merge goals conceded and shots conceded
# sc0_conceding_conversion <- merge(sc0_tsc,sc0_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc0_conceding_conversion$HSCC <- percent(sc0_conceding_conversion$TFthc/sc0_conceding_conversion$hsc, accuracy = 0.01)
# sc0_conceding_conversion$ASCC <- percent(sc0_conceding_conversion$TFtac/sc0_conceding_conversion$asc, accuracy = 0.01)
# sc0_conceding_conversion$TSCC <- percent(sc0_conceding_conversion$TGC/sc0_conceding_conversion$TSC, accuracy = 0.01)
# sc0_conceding_conversion$XSTC <- round(sc0_scoring$TGS/(sc0_tst$TST - sc0_scoring$TGS), digits = 2)
#
# #merge the two parts
#
# sc0_shots_analysis <- merge(sc0_scoring_conversion,sc0_conceding_conversion,by='Group.1',all = T)
# #######################################################################################################################################
# #SC1
# #home goals scored
# sc1_home_gs <- aggregate(SC1$FTHG, by = list(SC1$HomeTeam), FUN = sum)
# sc1_home_gs_avg <- aggregate(SC1$FTHG, by = list(SC1$HomeTeam),mean)
# sc1_home_scoring <- merge(sc1_home_gs,sc1_home_gs_avg, by='Group.1',all = T)
# names(sc1_home_scoring)[names(sc1_home_scoring) == "x.x"] <- "TFthg"
# names(sc1_home_scoring)[names(sc1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc1_away_gs <- aggregate(SC1$FTAG, by = list(SC1$AwayTeam), FUN = sum)
# sc1_away_gs_avg <- aggregate(SC1$FTAG, by = list(SC1$AwayTeam),mean)
# sc1_away_scoring <- merge(sc1_away_gs,sc1_away_gs_avg, by='Group.1',all = T)
# names(sc1_away_scoring)[names(sc1_away_scoring) == "x.x"] <- "TFtag"
# names(sc1_away_scoring)[names(sc1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc1_scoring <- merge(sc1_home_scoring,sc1_away_scoring,by='Group.1',all = T)
# sc1_scoring$TGS <- sc1_scoring$TFthg + sc1_scoring$TFtag
#
# #Home shots on target
# sc1_home_hst <- aggregate(SC1$HST, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_ast <- aggregate(SC1$AST, by = list(SC1$AwayTeam), FUN = sum)
# sc1_tst <- merge(sc1_home_hst,sc1_away_ast, by='Group.1',all = T)
# names(sc1_tst)[names(sc1_tst) == "x.x"] <- "hst"
# names(sc1_tst)[names(sc1_tst) == "x.y"] <- "ast"
# sc1_tst$TST <- sc1_tst$hst + sc1_tst$ast
# #merge goals scored and shots on target
# sc1_scoring_conversion <- merge(sc1_tst,sc1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc1_scoring_conversion$HSTC <- percent(sc1_scoring_conversion$TFthg/sc1_scoring_conversion$hst, accuracy = 0.01)
# sc1_scoring_conversion$ASTC <- percent(sc1_scoring_conversion$TFtag/sc1_scoring_conversion$ast, accuracy = 0.01)
# sc1_scoring_conversion$TSTC <- percent(sc1_scoring_conversion$TGS/sc1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc1_scoring_conversion <- cbind(sc1_scoring_conversion,sc1_games_played)
# #create the second part
# #home goals conceded
# sc1_home_gc <- aggregate(SC1$FTAG, by = list(SC1$HomeTeam), FUN = sum)
# sc1_home_gc_avg <- aggregate(SC1$FTAG, by = list(SC1$HomeTeam),mean)
# sc1_home_conceding <- merge(sc1_home_gc,sc1_home_gc_avg, by='Group.1',all = T)
# names(sc1_home_conceding)[names(sc1_home_conceding) == "x.x"] <- "TFthc"
# names(sc1_home_conceding)[names(sc1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc1_away_gc <- aggregate(SC1$FTHG, by = list(SC1$AwayTeam), FUN = sum)
# sc1_away_gc_avg <- aggregate(SC1$FTHG, by = list(SC1$AwayTeam),mean)
# sc1_away_conceding <- merge(sc1_away_gc,sc1_away_gc_avg, by='Group.1',all = T)
# names(sc1_away_conceding)[names(sc1_away_conceding) == "x.x"] <- "TFtac"
# names(sc1_away_conceding)[names(sc1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc1_conceding <- merge(sc1_home_conceding,sc1_away_conceding,by='Group.1',all = T)
# sc1_conceding$TGC <- sc1_conceding$TFthc + sc1_conceding$TFtac
#
# #Home shots conceded
# sc1_home_hsc <- aggregate(SC1$AST, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_asc <- aggregate(SC1$HST, by = list(SC1$AwayTeam), FUN = sum)
# sc1_tsc <- merge(sc1_home_hsc,sc1_away_asc, by='Group.1',all = T)
# names(sc1_tsc)[names(sc1_tsc) == "x.x"] <- "hsc"
# names(sc1_tsc)[names(sc1_tsc) == "x.y"] <- "asc"
# sc1_tsc$TSC <- sc1_tsc$hsc + sc1_tsc$asc
# #merge goals conceded and shots conceded
# sc1_conceding_conversion <- merge(sc1_tsc,sc1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc1_conceding_conversion$HSCC <- percent(sc1_conceding_conversion$TFthc/sc1_conceding_conversion$hsc, accuracy = 0.01)
# sc1_conceding_conversion$ASCC <- percent(sc1_conceding_conversion$TFtac/sc1_conceding_conversion$asc, accuracy = 0.01)
# sc1_conceding_conversion$TSCC <- percent(sc1_conceding_conversion$TGC/sc1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# sc1_shots_analysis <- merge(sc1_scoring_conversion,sc1_conceding_conversion,by='Group.1',all = T)
# #SC1
# #home goals scored
# sc1_home_gs <- aggregate(SC1$FTHG, by = list(SC1$HomeTeam), FUN = sum)
# sc1_home_gs_avg <- aggregate(SC1$FTHG, by = list(SC1$HomeTeam),mean)
# sc1_home_scoring <- merge(sc1_home_gs,sc1_home_gs_avg, by='Group.1',all = T)
# names(sc1_home_scoring)[names(sc1_home_scoring) == "x.x"] <- "TFthg"
# names(sc1_home_scoring)[names(sc1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc1_away_gs <- aggregate(SC1$FTAG, by = list(SC1$AwayTeam), FUN = sum)
# sc1_away_gs_avg <- aggregate(SC1$FTAG, by = list(SC1$AwayTeam),mean)
# sc1_away_scoring <- merge(sc1_away_gs,sc1_away_gs_avg, by='Group.1',all = T)
# names(sc1_away_scoring)[names(sc1_away_scoring) == "x.x"] <- "TFtag"
# names(sc1_away_scoring)[names(sc1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc1_scoring <- merge(sc1_home_scoring,sc1_away_scoring,by='Group.1',all = T)
# sc1_scoring$TGS <- sc1_scoring$TFthg + sc1_scoring$TFtag
#
# #Home shots on target
# sc1_home_hst <- aggregate(SC1$HST, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_ast <- aggregate(SC1$AST, by = list(SC1$AwayTeam), FUN = sum)
# sc1_tst <- merge(sc1_home_hst,sc1_away_ast, by='Group.1',all = T)
# names(sc1_tst)[names(sc1_tst) == "x.x"] <- "hst"
# names(sc1_tst)[names(sc1_tst) == "x.y"] <- "ast"
# sc1_tst$TST <- sc1_tst$hst + sc1_tst$ast
# #merge goals scored and shots on target
# sc1_scoring_conversion <- merge(sc1_tst,sc1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc1_scoring_conversion$HSTC <- percent(sc1_scoring_conversion$TFthg/sc1_scoring_conversion$hst, accuracy = 0.01)
# sc1_scoring_conversion$ASTC <- percent(sc1_scoring_conversion$TFtag/sc1_scoring_conversion$ast, accuracy = 0.01)
# sc1_scoring_conversion$TSTC <- percent(sc1_scoring_conversion$TGS/sc1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc1_scoring_conversion <- cbind(sc1_scoring_conversion,sc1_games_played)
# #create the second part
# #home goals conceded
# sc1_home_gc <- aggregate(SC1$FTAG, by = list(SC1$HomeTeam), FUN = sum)
# sc1_home_gc_avg <- aggregate(SC1$FTAG, by = list(SC1$HomeTeam),mean)
# sc1_home_conceding <- merge(sc1_home_gc,sc1_home_gc_avg, by='Group.1',all = T)
# names(sc1_home_conceding)[names(sc1_home_conceding) == "x.x"] <- "TFthc"
# names(sc1_home_conceding)[names(sc1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc1_away_gc <- aggregate(SC1$FTHG, by = list(SC1$AwayTeam), FUN = sum)
# sc1_away_gc_avg <- aggregate(SC1$FTHG, by = list(SC1$AwayTeam),mean)
# sc1_away_conceding <- merge(sc1_away_gc,sc1_away_gc_avg, by='Group.1',all = T)
# names(sc1_away_conceding)[names(sc1_away_conceding) == "x.x"] <- "TFtac"
# names(sc1_away_conceding)[names(sc1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc1_conceding <- merge(sc1_home_conceding,sc1_away_conceding,by='Group.1',all = T)
# sc1_conceding$TGC <- sc1_conceding$TFthc + sc1_conceding$TFtac
#
# #Home shots conceded
# sc1_home_hsc <- aggregate(SC1$AST, by = list(SC1$HomeTeam), FUN = sum)
# sc1_away_asc <- aggregate(SC1$HST, by = list(SC1$AwayTeam), FUN = sum)
# sc1_tsc <- merge(sc1_home_hsc,sc1_away_asc, by='Group.1',all = T)
# names(sc1_tsc)[names(sc1_tsc) == "x.x"] <- "hsc"
# names(sc1_tsc)[names(sc1_tsc) == "x.y"] <- "asc"
# sc1_tsc$TSC <- sc1_tsc$hsc + sc1_tsc$asc
# #merge goals conceded and shots conceded
# sc1_conceding_conversion <- merge(sc1_tsc,sc1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc1_conceding_conversion$HSCC <- percent(sc1_conceding_conversion$TFthc/sc1_conceding_conversion$hsc, accuracy = 0.01)
# sc1_conceding_conversion$ASCC <- percent(sc1_conceding_conversion$TFtac/sc1_conceding_conversion$asc, accuracy = 0.01)
# sc1_conceding_conversion$TSCC <- percent(sc1_conceding_conversion$TGC/sc1_conceding_conversion$TSC, accuracy = 0.01)
# sc1_conceding_conversion$XSTC <- round(sc1_scoring$TGS/(sc1_tst$TST - sc1_scoring$TGS), digits = 2)
#
# #merge the two parts
# sc1_shots_analysis <- merge(sc1_scoring_conversion,sc1_conceding_conversion,by='Group.1',all = T)
# ########################################################################################################################################
# #SC2
# #home goals scored
# sc2_home_gs <- aggregate(SC2$FTHG, by = list(SC2$HomeTeam), FUN = sum)
# sc2_home_gs_avg <- aggregate(SC2$FTHG, by = list(SC2$HomeTeam),mean)
# sc2_home_scoring <- merge(sc2_home_gs,sc2_home_gs_avg, by='Group.1',all = T)
# names(sc2_home_scoring)[names(sc2_home_scoring) == "x.x"] <- "TFthg"
# names(sc2_home_scoring)[names(sc2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc2_away_gs <- aggregate(SC2$FTAG, by = list(SC2$AwayTeam), FUN = sum)
# sc2_away_gs_avg <- aggregate(SC2$FTAG, by = list(SC2$AwayTeam),mean)
# sc2_away_scoring <- merge(sc2_away_gs,sc2_away_gs_avg, by='Group.1',all = T)
# names(sc2_away_scoring)[names(sc2_away_scoring) == "x.x"] <- "TFtag"
# names(sc2_away_scoring)[names(sc2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc2_scoring <- merge(sc2_home_scoring,sc2_away_scoring,by='Group.1',all = T)
# sc2_scoring$TGS <- sc2_scoring$TFthg + sc2_scoring$TFtag
#
# #Home shots on target
# sc2_home_hst <- aggregate(SC2$HST, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_ast <- aggregate(SC2$AST, by = list(SC2$AwayTeam), FUN = sum)
# sc2_tst <- merge(sc2_home_hst,sc2_away_ast, by='Group.1',all = T)
# names(sc2_tst)[names(sc2_tst) == "x.x"] <- "hst"
# names(sc2_tst)[names(sc2_tst) == "x.y"] <- "ast"
# sc2_tst$TST <- sc2_tst$hst + sc2_tst$ast
# #merge goals scored and shots on target
# sc2_scoring_conversion <- merge(sc2_tst,sc2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc2_scoring_conversion$HSTC <- percent(sc2_scoring_conversion$TFthg/sc2_scoring_conversion$hst, accuracy = 0.01)
# sc2_scoring_conversion$ASTC <- percent(sc2_scoring_conversion$TFtag/sc2_scoring_conversion$ast, accuracy = 0.01)
# sc2_scoring_conversion$TSTC <- percent(sc2_scoring_conversion$TGS/sc2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc2_scoring_conversion <- cbind(sc2_scoring_conversion,sc2_games_played)
#
# #create the second part
# #home goals conceded
# sc2_home_gc <- aggregate(SC2$FTAG, by = list(SC2$HomeTeam), FUN = sum)
# sc2_home_gc_avg <- aggregate(SC2$FTAG, by = list(SC2$HomeTeam),mean)
# sc2_home_conceding <- merge(sc2_home_gc,sc2_home_gc_avg, by='Group.1',all = T)
# names(sc2_home_conceding)[names(sc2_home_conceding) == "x.x"] <- "TFthc"
# names(sc2_home_conceding)[names(sc2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc2_away_gc <- aggregate(SC2$FTHG, by = list(SC2$AwayTeam), FUN = sum)
# sc2_away_gc_avg <- aggregate(SC2$FTHG, by = list(SC2$AwayTeam),mean)
# sc2_away_conceding <- merge(sc2_away_gc,sc2_away_gc_avg, by='Group.1',all = T)
# names(sc2_away_conceding)[names(sc2_away_conceding) == "x.x"] <- "TFtac"
# names(sc2_away_conceding)[names(sc2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc2_conceding <- merge(sc2_home_conceding,sc2_away_conceding,by='Group.1',all = T)
# sc2_conceding$TGC <- sc2_conceding$TFthc + sc2_conceding$TFtac
#
# #Home shots conceded
# sc2_home_hsc <- aggregate(SC2$AST, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_asc <- aggregate(SC2$HST, by = list(SC2$AwayTeam), FUN = sum)
# sc2_tsc <- merge(sc2_home_hsc,sc2_away_asc, by='Group.1',all = T)
# names(sc2_tsc)[names(sc2_tsc) == "x.x"] <- "hsc"
# names(sc2_tsc)[names(sc2_tsc) == "x.y"] <- "asc"
# sc2_tsc$TSC <- sc2_tsc$hsc + sc2_tsc$asc
# #merge goals conceded and shots conceded
# sc2_conceding_conversion <- merge(sc2_tsc,sc2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc2_conceding_conversion$HSCC <- percent(sc2_conceding_conversion$TFthc/sc2_conceding_conversion$hsc, accuracy = 0.01)
# sc2_conceding_conversion$ASCC <- percent(sc2_conceding_conversion$TFtac/sc2_conceding_conversion$asc, accuracy = 0.01)
# sc2_conceding_conversion$TSCC <- percent(sc2_conceding_conversion$TGC/sc2_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# sc2_shots_analysis <- merge(sc2_scoring_conversion,sc2_conceding_conversion,by='Group.1',all = T)
# #SC2
# #home goals scored
# sc2_home_gs <- aggregate(SC2$FTHG, by = list(SC2$HomeTeam), FUN = sum)
# sc2_home_gs_avg <- aggregate(SC2$FTHG, by = list(SC2$HomeTeam),mean)
# sc2_home_scoring <- merge(sc2_home_gs,sc2_home_gs_avg, by='Group.1',all = T)
# names(sc2_home_scoring)[names(sc2_home_scoring) == "x.x"] <- "TFthg"
# names(sc2_home_scoring)[names(sc2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc2_away_gs <- aggregate(SC2$FTAG, by = list(SC2$AwayTeam), FUN = sum)
# sc2_away_gs_avg <- aggregate(SC2$FTAG, by = list(SC2$AwayTeam),mean)
# sc2_away_scoring <- merge(sc2_away_gs,sc2_away_gs_avg, by='Group.1',all = T)
# names(sc2_away_scoring)[names(sc2_away_scoring) == "x.x"] <- "TFtag"
# names(sc2_away_scoring)[names(sc2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc2_scoring <- merge(sc2_home_scoring,sc2_away_scoring,by='Group.1',all = T)
# sc2_scoring$TGS <- sc2_scoring$TFthg + sc2_scoring$TFtag
#
# #Home shots on target
# sc2_home_hst <- aggregate(SC2$HST, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_ast <- aggregate(SC2$AST, by = list(SC2$AwayTeam), FUN = sum)
# sc2_tst <- merge(sc2_home_hst,sc2_away_ast, by='Group.1',all = T)
# names(sc2_tst)[names(sc2_tst) == "x.x"] <- "hst"
# names(sc2_tst)[names(sc2_tst) == "x.y"] <- "ast"
# sc2_tst$TST <- sc2_tst$hst + sc2_tst$ast
# #merge goals scored and shots on target
# sc2_scoring_conversion <- merge(sc2_tst,sc2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc2_scoring_conversion$HSTC <- percent(sc2_scoring_conversion$TFthg/sc2_scoring_conversion$hst, accuracy = 0.01)
# sc2_scoring_conversion$ASTC <- percent(sc2_scoring_conversion$TFtag/sc2_scoring_conversion$ast, accuracy = 0.01)
# sc2_scoring_conversion$TSTC <- percent(sc2_scoring_conversion$TGS/sc2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc2_scoring_conversion <- cbind(sc2_scoring_conversion,sc2_games_played)
# #create the second part
# #home goals conceded
# sc2_home_gc <- aggregate(SC2$FTAG, by = list(SC2$HomeTeam), FUN = sum)
# sc2_home_gc_avg <- aggregate(SC2$FTAG, by = list(SC2$HomeTeam),mean)
# sc2_home_conceding <- merge(sc2_home_gc,sc2_home_gc_avg, by='Group.1',all = T)
# names(sc2_home_conceding)[names(sc2_home_conceding) == "x.x"] <- "TFthc"
# names(sc2_home_conceding)[names(sc2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc2_away_gc <- aggregate(SC2$FTHG, by = list(SC2$AwayTeam), FUN = sum)
# sc2_away_gc_avg <- aggregate(SC2$FTHG, by = list(SC2$AwayTeam),mean)
# sc2_away_conceding <- merge(sc2_away_gc,sc2_away_gc_avg, by='Group.1',all = T)
# names(sc2_away_conceding)[names(sc2_away_conceding) == "x.x"] <- "TFtac"
# names(sc2_away_conceding)[names(sc2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc2_conceding <- merge(sc2_home_conceding,sc2_away_conceding,by='Group.1',all = T)
# sc2_conceding$TGC <- sc2_conceding$TFthc + sc2_conceding$TFtac
#
# #Home shots conceded
# sc2_home_hsc <- aggregate(SC2$AST, by = list(SC2$HomeTeam), FUN = sum)
# sc2_away_asc <- aggregate(SC2$HST, by = list(SC2$AwayTeam), FUN = sum)
# sc2_tsc <- merge(sc2_home_hsc,sc2_away_asc, by='Group.1',all = T)
# names(sc2_tsc)[names(sc2_tsc) == "x.x"] <- "hsc"
# names(sc2_tsc)[names(sc2_tsc) == "x.y"] <- "asc"
# sc2_tsc$TSC <- sc2_tsc$hsc + sc2_tsc$asc
# #merge goals conceded and shots conceded
# sc2_conceding_conversion <- merge(sc2_tsc,sc2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc2_conceding_conversion$HSCC <- percent(sc2_conceding_conversion$TFthc/sc2_conceding_conversion$hsc, accuracy = 0.01)
# sc2_conceding_conversion$ASCC <- percent(sc2_conceding_conversion$TFtac/sc2_conceding_conversion$asc, accuracy = 0.01)
# sc2_conceding_conversion$TSCC <- percent(sc2_conceding_conversion$TGC/sc2_conceding_conversion$TSC, accuracy = 0.01)
# sc2_conceding_conversion$XSTC <- round(sc2_scoring$TGS/(sc2_tst$TST - sc2_scoring$TGS), digits = 2)
#
# #merge the two parts
# sc2_shots_analysis <- merge(sc2_scoring_conversion,sc2_conceding_conversion,by='Group.1',all = T)
# ##############################################################################################################################
# #SC3
# #home goals scored
# sc3_home_gs <- aggregate(SC3$FTHG, by = list(SC3$HomeTeam), FUN = sum)
# sc3_home_gs_avg <- aggregate(SC3$FTHG, by = list(SC3$HomeTeam),mean)
# sc3_home_scoring <- merge(sc3_home_gs,sc3_home_gs_avg, by='Group.1',all = T)
# names(sc3_home_scoring)[names(sc3_home_scoring) == "x.x"] <- "TFthg"
# names(sc3_home_scoring)[names(sc3_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc3_away_gs <- aggregate(SC3$FTAG, by = list(SC3$AwayTeam), FUN = sum)
# sc3_away_gs_avg <- aggregate(SC3$FTAG, by = list(SC3$AwayTeam),mean)
# sc3_away_scoring <- merge(sc3_away_gs,sc3_away_gs_avg, by='Group.1',all = T)
# names(sc3_away_scoring)[names(sc3_away_scoring) == "x.x"] <- "TFtag"
# names(sc3_away_scoring)[names(sc3_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc3_scoring <- merge(sc3_home_scoring,sc3_away_scoring,by='Group.1',all = T)
# sc3_scoring$TGS <- sc3_scoring$TFthg + sc3_scoring$TFtag
#
# #Home shots on target
# sc3_home_hst <- aggregate(SC3$HST, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_ast <- aggregate(SC3$AST, by = list(SC3$AwayTeam), FUN = sum)
# sc3_tst <- merge(sc3_home_hst,sc3_away_ast, by='Group.1',all = T)
# names(sc3_tst)[names(sc3_tst) == "x.x"] <- "hst"
# names(sc3_tst)[names(sc3_tst) == "x.y"] <- "ast"
# sc3_tst$TST <- sc3_tst$hst + sc3_tst$ast
# #merge goals scored and shots on target
# sc3_scoring_conversion <- merge(sc3_tst,sc3_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc3_scoring_conversion$HSTC <- percent(sc3_scoring_conversion$TFthg/sc3_scoring_conversion$hst, accuracy = 0.01)
# sc3_scoring_conversion$ASTC <- percent(sc3_scoring_conversion$TFtag/sc3_scoring_conversion$ast, accuracy = 0.01)
# sc3_scoring_conversion$TSTC <- percent(sc3_scoring_conversion$TGS/sc3_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc3_scoring_conversion <- cbind(sc3_scoring_conversion,sc3_games_played)
# #create the second part
# #home goals conceded
# sc3_home_gc <- aggregate(SC3$FTAG, by = list(SC3$HomeTeam), FUN = sum)
# sc3_home_gc_avg <- aggregate(SC3$FTAG, by = list(SC3$HomeTeam),mean)
# sc3_home_conceding <- merge(sc3_home_gc,sc3_home_gc_avg, by='Group.1',all = T)
# names(sc3_home_conceding)[names(sc3_home_conceding) == "x.x"] <- "TFthc"
# names(sc3_home_conceding)[names(sc3_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc3_away_gc <- aggregate(SC3$FTHG, by = list(SC3$AwayTeam), FUN = sum)
# sc3_away_gc_avg <- aggregate(SC3$FTHG, by = list(SC3$AwayTeam),mean)
# sc3_away_conceding <- merge(sc3_away_gc,sc3_away_gc_avg, by='Group.1',all = T)
# names(sc3_away_conceding)[names(sc3_away_conceding) == "x.x"] <- "TFtac"
# names(sc3_away_conceding)[names(sc3_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc3_conceding <- merge(sc3_home_conceding,sc3_away_conceding,by='Group.1',all = T)
# sc3_conceding$TGC <- sc3_conceding$TFthc + sc3_conceding$TFtac
#
# #Home shots conceded
# sc3_home_hsc <- aggregate(SC3$AST, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_asc <- aggregate(SC3$HST, by = list(SC3$AwayTeam), FUN = sum)
# sc3_tsc <- merge(sc3_home_hsc,sc3_away_asc, by='Group.1',all = T)
# names(sc3_tsc)[names(sc3_tsc) == "x.x"] <- "hsc"
# names(sc3_tsc)[names(sc3_tsc) == "x.y"] <- "asc"
# sc3_tsc$TSC <- sc3_tsc$hsc + sc3_tsc$asc
# #merge goals conceded and shots conceded
# sc3_conceding_conversion <- merge(sc3_tsc,sc3_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc3_conceding_conversion$HSCC <- percent(sc3_conceding_conversion$TFthc/sc3_conceding_conversion$hsc, accuracy = 0.01)
# sc3_conceding_conversion$ASCC <- percent(sc3_conceding_conversion$TFtac/sc3_conceding_conversion$asc, accuracy = 0.01)
# sc3_conceding_conversion$TSCC <- percent(sc3_conceding_conversion$TGC/sc3_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# sc3_shots_analysis <- merge(sc3_scoring_conversion,sc3_conceding_conversion,by='Group.1',all = T)
# #SC3
# #home goals scored
# sc3_home_gs <- aggregate(SC3$FTHG, by = list(SC3$HomeTeam), FUN = sum)
# sc3_home_gs_avg <- aggregate(SC3$FTHG, by = list(SC3$HomeTeam),mean)
# sc3_home_scoring <- merge(sc3_home_gs,sc3_home_gs_avg, by='Group.1',all = T)
# names(sc3_home_scoring)[names(sc3_home_scoring) == "x.x"] <- "TFthg"
# names(sc3_home_scoring)[names(sc3_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sc3_away_gs <- aggregate(SC3$FTAG, by = list(SC3$AwayTeam), FUN = sum)
# sc3_away_gs_avg <- aggregate(SC3$FTAG, by = list(SC3$AwayTeam),mean)
# sc3_away_scoring <- merge(sc3_away_gs,sc3_away_gs_avg, by='Group.1',all = T)
# names(sc3_away_scoring)[names(sc3_away_scoring) == "x.x"] <- "TFtag"
# names(sc3_away_scoring)[names(sc3_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sc3_scoring <- merge(sc3_home_scoring,sc3_away_scoring,by='Group.1',all = T)
# sc3_scoring$TGS <- sc3_scoring$TFthg + sc3_scoring$TFtag
#
# #Home shots on target
# sc3_home_hst <- aggregate(SC3$HST, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_ast <- aggregate(SC3$AST, by = list(SC3$AwayTeam), FUN = sum)
# sc3_tst <- merge(sc3_home_hst,sc3_away_ast, by='Group.1',all = T)
# names(sc3_tst)[names(sc3_tst) == "x.x"] <- "hst"
# names(sc3_tst)[names(sc3_tst) == "x.y"] <- "ast"
# sc3_tst$TST <- sc3_tst$hst + sc3_tst$ast
# #merge goals scored and shots on target
# sc3_scoring_conversion <- merge(sc3_tst,sc3_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sc3_scoring_conversion$HSTC <- percent(sc3_scoring_conversion$TFthg/sc3_scoring_conversion$hst, accuracy = 0.01)
# sc3_scoring_conversion$ASTC <- percent(sc3_scoring_conversion$TFtag/sc3_scoring_conversion$ast, accuracy = 0.01)
# sc3_scoring_conversion$TSTC <- percent(sc3_scoring_conversion$TGS/sc3_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sc3_scoring_conversion <- cbind(sc3_scoring_conversion,sc3_games_played)
# #create the second part
# #home goals conceded
# sc3_home_gc <- aggregate(SC3$FTAG, by = list(SC3$HomeTeam), FUN = sum)
# sc3_home_gc_avg <- aggregate(SC3$FTAG, by = list(SC3$HomeTeam),mean)
# sc3_home_conceding <- merge(sc3_home_gc,sc3_home_gc_avg, by='Group.1',all = T)
# names(sc3_home_conceding)[names(sc3_home_conceding) == "x.x"] <- "TFthc"
# names(sc3_home_conceding)[names(sc3_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sc3_away_gc <- aggregate(SC3$FTHG, by = list(SC3$AwayTeam), FUN = sum)
# sc3_away_gc_avg <- aggregate(SC3$FTHG, by = list(SC3$AwayTeam),mean)
# sc3_away_conceding <- merge(sc3_away_gc,sc3_away_gc_avg, by='Group.1',all = T)
# names(sc3_away_conceding)[names(sc3_away_conceding) == "x.x"] <- "TFtac"
# names(sc3_away_conceding)[names(sc3_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sc3_conceding <- merge(sc3_home_conceding,sc3_away_conceding,by='Group.1',all = T)
# sc3_conceding$TGC <- sc3_conceding$TFthc + sc3_conceding$TFtac
#
# #Home shots conceded
# sc3_home_hsc <- aggregate(SC3$AST, by = list(SC3$HomeTeam), FUN = sum)
# sc3_away_asc <- aggregate(SC3$HST, by = list(SC3$AwayTeam), FUN = sum)
# sc3_tsc <- merge(sc3_home_hsc,sc3_away_asc, by='Group.1',all = T)
# names(sc3_tsc)[names(sc3_tsc) == "x.x"] <- "hsc"
# names(sc3_tsc)[names(sc3_tsc) == "x.y"] <- "asc"
# sc3_tsc$TSC <- sc3_tsc$hsc + sc3_tsc$asc
# #merge goals conceded and shots conceded
# sc3_conceding_conversion <- merge(sc3_tsc,sc3_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sc3_conceding_conversion$HSCC <- percent(sc3_conceding_conversion$TFthc/sc3_conceding_conversion$hsc, accuracy = 0.01)
# sc3_conceding_conversion$ASCC <- percent(sc3_conceding_conversion$TFtac/sc3_conceding_conversion$asc, accuracy = 0.01)
# sc3_conceding_conversion$TSCC <- percent(sc3_conceding_conversion$TGC/sc3_conceding_conversion$TSC, accuracy = 0.01)
# sc3_conceding_conversion$XSTC <- round(sc3_scoring$TGS/(sc3_tst$TST - sc3_scoring$TGS), digits = 2)
#
# #merge the two parts
# sc3_shots_analysis <- merge(sc3_scoring_conversion,sc3_conceding_conversion,by='Group.1',all = T)
# #######################################################################################################################################
# #SP1
# #home goals scored
# sp1_home_gs <- aggregate(SP1$FTHG, by = list(SP1$HomeTeam), FUN = sum)
# sp1_home_gs_avg <- aggregate(SP1$FTHG, by = list(SP1$HomeTeam),mean)
# sp1_home_scoring <- merge(sp1_home_gs,sp1_home_gs_avg, by='Group.1',all = T)
# names(sp1_home_scoring)[names(sp1_home_scoring) == "x.x"] <- "TFthg"
# names(sp1_home_scoring)[names(sp1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sp1_away_gs <- aggregate(SP1$FTAG, by = list(SP1$AwayTeam), FUN = sum)
# sp1_away_gs_avg <- aggregate(SP1$FTAG, by = list(SP1$AwayTeam),mean)
# sp1_away_scoring <- merge(sp1_away_gs,sp1_away_gs_avg, by='Group.1',all = T)
# names(sp1_away_scoring)[names(sp1_away_scoring) == "x.x"] <- "TFtag"
# names(sp1_away_scoring)[names(sp1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sp1_scoring <- merge(sp1_home_scoring,sp1_away_scoring,by='Group.1',all = T)
# sp1_scoring$TGS <- sp1_scoring$TFthg + sp1_scoring$TFtag
#
# #Home shots on target
# sp1_home_hst <- aggregate(SP1$HST, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_ast <- aggregate(SP1$AST, by = list(SP1$AwayTeam), FUN = sum)
# sp1_tst <- merge(sp1_home_hst,sp1_away_ast, by='Group.1',all = T)
# names(sp1_tst)[names(sp1_tst) == "x.x"] <- "hst"
# names(sp1_tst)[names(sp1_tst) == "x.y"] <- "ast"
# sp1_tst$TST <- sp1_tst$hst + sp1_tst$ast
# #merge goals scored and shots on target
# sp1_scoring_conversion <- merge(sp1_tst,sp1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sp1_scoring_conversion$HSTC <- percent(sp1_scoring_conversion$TFthg/sp1_scoring_conversion$hst, accuracy = 0.01)
# sp1_scoring_conversion$ASTC <- percent(sp1_scoring_conversion$TFtag/sp1_scoring_conversion$ast, accuracy = 0.01)
# sp1_scoring_conversion$TSTC <- percent(sp1_scoring_conversion$TGS/sp1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sp1_scoring_conversion <- cbind(sp1_scoring_conversion,sp1_games_played)
# #create the second part
# #home goals conceded
# sp1_home_gc <- aggregate(SP1$FTAG, by = list(SP1$HomeTeam), FUN = sum)
# sp1_home_gc_avg <- aggregate(SP1$FTAG, by = list(SP1$HomeTeam),mean)
# sp1_home_conceding <- merge(sp1_home_gc,sp1_home_gc_avg, by='Group.1',all = T)
# names(sp1_home_conceding)[names(sp1_home_conceding) == "x.x"] <- "TFthc"
# names(sp1_home_conceding)[names(sp1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sp1_away_gc <- aggregate(SP1$FTHG, by = list(SP1$AwayTeam), FUN = sum)
# sp1_away_gc_avg <- aggregate(SP1$FTHG, by = list(SP1$AwayTeam),mean)
# sp1_away_conceding <- merge(sp1_away_gc,sp1_away_gc_avg, by='Group.1',all = T)
# names(sp1_away_conceding)[names(sp1_away_conceding) == "x.x"] <- "TFtac"
# names(sp1_away_conceding)[names(sp1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sp1_conceding <- merge(sp1_home_conceding,sp1_away_conceding,by='Group.1',all = T)
# sp1_conceding$TGC <- sp1_conceding$TFthc + sp1_conceding$TFtac
#
# #Home shots conceded
# sp1_home_hsc <- aggregate(SP1$AST, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_asc <- aggregate(SP1$HST, by = list(SP1$AwayTeam), FUN = sum)
# sp1_tsc <- merge(sp1_home_hsc,sp1_away_asc, by='Group.1',all = T)
# names(sp1_tsc)[names(sp1_tsc) == "x.x"] <- "hsc"
# names(sp1_tsc)[names(sp1_tsc) == "x.y"] <- "asc"
# sp1_tsc$TSC <- sp1_tsc$hsc + sp1_tsc$asc
# #merge goals conceded and shots conceded
# sp1_conceding_conversion <- merge(sp1_tsc,sp1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sp1_conceding_conversion$HSCC <- percent(sp1_conceding_conversion$TFthc/sp1_conceding_conversion$hsc, accuracy = 0.01)
# sp1_conceding_conversion$ASCC <- percent(sp1_conceding_conversion$TFtac/sp1_conceding_conversion$asc, accuracy = 0.01)
# sp1_conceding_conversion$TSCC <- percent(sp1_conceding_conversion$TGC/sp1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# sp1_shots_analysis <- merge(sp1_scoring_conversion,sp1_conceding_conversion,by='Group.1',all = T)
# #SP1
# #home goals scored
# sp1_home_gs <- aggregate(SP1$FTHG, by = list(SP1$HomeTeam), FUN = sum)
# sp1_home_gs_avg <- aggregate(SP1$FTHG, by = list(SP1$HomeTeam),mean)
# sp1_home_scoring <- merge(sp1_home_gs,sp1_home_gs_avg, by='Group.1',all = T)
# names(sp1_home_scoring)[names(sp1_home_scoring) == "x.x"] <- "TFthg"
# names(sp1_home_scoring)[names(sp1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sp1_away_gs <- aggregate(SP1$FTAG, by = list(SP1$AwayTeam), FUN = sum)
# sp1_away_gs_avg <- aggregate(SP1$FTAG, by = list(SP1$AwayTeam),mean)
# sp1_away_scoring <- merge(sp1_away_gs,sp1_away_gs_avg, by='Group.1',all = T)
# names(sp1_away_scoring)[names(sp1_away_scoring) == "x.x"] <- "TFtag"
# names(sp1_away_scoring)[names(sp1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sp1_scoring <- merge(sp1_home_scoring,sp1_away_scoring,by='Group.1',all = T)
# sp1_scoring$TGS <- sp1_scoring$TFthg + sp1_scoring$TFtag
#
# #Home shots on target
# sp1_home_hst <- aggregate(SP1$HST, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_ast <- aggregate(SP1$AST, by = list(SP1$AwayTeam), FUN = sum)
# sp1_tst <- merge(sp1_home_hst,sp1_away_ast, by='Group.1',all = T)
# names(sp1_tst)[names(sp1_tst) == "x.x"] <- "hst"
# names(sp1_tst)[names(sp1_tst) == "x.y"] <- "ast"
# sp1_tst$TST <- sp1_tst$hst + sp1_tst$ast
# #merge goals scored and shots on target
# sp1_scoring_conversion <- merge(sp1_tst,sp1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sp1_scoring_conversion$HSTC <- percent(sp1_scoring_conversion$TFthg/sp1_scoring_conversion$hst, accuracy = 0.01)
# sp1_scoring_conversion$ASTC <- percent(sp1_scoring_conversion$TFtag/sp1_scoring_conversion$ast, accuracy = 0.01)
# sp1_scoring_conversion$TSTC <- percent(sp1_scoring_conversion$TGS/sp1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sp1_scoring_conversion <- cbind(sp1_scoring_conversion,sp1_games_played)
# #create the second part
# #home goals conceded
# sp1_home_gc <- aggregate(SP1$FTAG, by = list(SP1$HomeTeam), FUN = sum)
# sp1_home_gc_avg <- aggregate(SP1$FTAG, by = list(SP1$HomeTeam),mean)
# sp1_home_conceding <- merge(sp1_home_gc,sp1_home_gc_avg, by='Group.1',all = T)
# names(sp1_home_conceding)[names(sp1_home_conceding) == "x.x"] <- "TFthc"
# names(sp1_home_conceding)[names(sp1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sp1_away_gc <- aggregate(SP1$FTHG, by = list(SP1$AwayTeam), FUN = sum)
# sp1_away_gc_avg <- aggregate(SP1$FTHG, by = list(SP1$AwayTeam),mean)
# sp1_away_conceding <- merge(sp1_away_gc,sp1_away_gc_avg, by='Group.1',all = T)
# names(sp1_away_conceding)[names(sp1_away_conceding) == "x.x"] <- "TFtac"
# names(sp1_away_conceding)[names(sp1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sp1_conceding <- merge(sp1_home_conceding,sp1_away_conceding,by='Group.1',all = T)
# sp1_conceding$TGC <- sp1_conceding$TFthc + sp1_conceding$TFtac
#
# #Home shots conceded
# sp1_home_hsc <- aggregate(SP1$AST, by = list(SP1$HomeTeam), FUN = sum)
# sp1_away_asc <- aggregate(SP1$HST, by = list(SP1$AwayTeam), FUN = sum)
# sp1_tsc <- merge(sp1_home_hsc,sp1_away_asc, by='Group.1',all = T)
# names(sp1_tsc)[names(sp1_tsc) == "x.x"] <- "hsc"
# names(sp1_tsc)[names(sp1_tsc) == "x.y"] <- "asc"
# sp1_tsc$TSC <- sp1_tsc$hsc + sp1_tsc$asc
# #merge goals conceded and shots conceded
# sp1_conceding_conversion <- merge(sp1_tsc,sp1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sp1_conceding_conversion$HSCC <- percent(sp1_conceding_conversion$TFthc/sp1_conceding_conversion$hsc, accuracy = 0.01)
# sp1_conceding_conversion$ASCC <- percent(sp1_conceding_conversion$TFtac/sp1_conceding_conversion$asc, accuracy = 0.01)
# sp1_conceding_conversion$TSCC <- percent(sp1_conceding_conversion$TGC/sp1_conceding_conversion$TSC, accuracy = 0.01)
# sp1_conceding_conversion$XSTC <- round(sp1_scoring$TGS/(sp1_tst$TST - sp1_scoring$TGS), digits = 2)
#
# #merge the two parts
# sp1_shots_analysis <- merge(sp1_scoring_conversion,sp1_conceding_conversion,by='Group.1',all = T)
# ###############################################################################################################################
# #SP2
# #home goals scored
# sp2_home_gs <- aggregate(SP2$FTHG, by = list(SP2$HomeTeam), FUN = sum)
# sp2_home_gs_avg <- aggregate(SP2$FTHG, by = list(SP2$HomeTeam),mean)
# sp2_home_scoring <- merge(sp2_home_gs,sp2_home_gs_avg, by='Group.1',all = T)
# names(sp2_home_scoring)[names(sp2_home_scoring) == "x.x"] <- "TFthg"
# names(sp2_home_scoring)[names(sp2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sp2_away_gs <- aggregate(SP2$FTAG, by = list(SP2$AwayTeam), FUN = sum)
# sp2_away_gs_avg <- aggregate(SP2$FTAG, by = list(SP2$AwayTeam),mean)
# sp2_away_scoring <- merge(sp2_away_gs,sp2_away_gs_avg, by='Group.1',all = T)
# names(sp2_away_scoring)[names(sp2_away_scoring) == "x.x"] <- "TFtag"
# names(sp2_away_scoring)[names(sp2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sp2_scoring <- merge(sp2_home_scoring,sp2_away_scoring,by='Group.1',all = T)
# sp2_scoring$TGS <- sp2_scoring$TFthg + sp2_scoring$TFtag
#
# #Home shots on target
# sp2_home_hst <- aggregate(SP2$HST, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_ast <- aggregate(SP2$AST, by = list(SP2$AwayTeam), FUN = sum)
# sp2_tst <- merge(sp2_home_hst,sp2_away_ast, by='Group.1',all = T)
# names(sp2_tst)[names(sp2_tst) == "x.x"] <- "hst"
# names(sp2_tst)[names(sp2_tst) == "x.y"] <- "ast"
# sp2_tst$TST <- sp2_tst$hst + sp2_tst$ast
# #merge goals scored and shots on target
# sp2_scoring_conversion <- merge(sp2_tst,sp2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sp2_scoring_conversion$HSTC <- percent(sp2_scoring_conversion$TFthg/sp2_scoring_conversion$hst, accuracy = 0.01)
# sp2_scoring_conversion$ASTC <- percent(sp2_scoring_conversion$TFtag/sp2_scoring_conversion$ast, accuracy = 0.01)
# sp2_scoring_conversion$TSTC <- percent(sp2_scoring_conversion$TGS/sp2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sp2_scoring_conversion <- cbind(sp2_scoring_conversion,sp2_games_played)
# #create the second part
# #home goals conceded
# sp2_home_gc <- aggregate(SP2$FTAG, by = list(SP2$HomeTeam), FUN = sum)
# sp2_home_gc_avg <- aggregate(SP2$FTAG, by = list(SP2$HomeTeam),mean)
# sp2_home_conceding <- merge(sp2_home_gc,sp2_home_gc_avg, by='Group.1',all = T)
# names(sp2_home_conceding)[names(sp2_home_conceding) == "x.x"] <- "TFthc"
# names(sp2_home_conceding)[names(sp2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sp2_away_gc <- aggregate(SP2$FTHG, by = list(SP2$AwayTeam), FUN = sum)
# sp2_away_gc_avg <- aggregate(SP2$FTHG, by = list(SP2$AwayTeam),mean)
# sp2_away_conceding <- merge(sp2_away_gc,sp2_away_gc_avg, by='Group.1',all = T)
# names(sp2_away_conceding)[names(sp2_away_conceding) == "x.x"] <- "TFtac"
# names(sp2_away_conceding)[names(sp2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sp2_conceding <- merge(sp2_home_conceding,sp2_away_conceding,by='Group.1',all = T)
# sp2_conceding$TGC <- sp2_conceding$TFthc + sp2_conceding$TFtac
#
# #Home shots conceded
# sp2_home_hsc <- aggregate(SP2$AST, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_asc <- aggregate(SP2$HST, by = list(SP2$AwayTeam), FUN = sum)
# sp2_tsc <- merge(sp2_home_hsc,sp2_away_asc, by='Group.1',all = T)
# names(sp2_tsc)[names(sp2_tsc) == "x.x"] <- "hsc"
# names(sp2_tsc)[names(sp2_tsc) == "x.y"] <- "asc"
# sp2_tsc$TSC <- sp2_tsc$hsc + sp2_tsc$asc
# #merge goals conceded and shots conceded
# sp2_conceding_conversion <- merge(sp2_tsc,sp2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sp2_conceding_conversion$HSCC <- percent(sp2_conceding_conversion$TFthc/sp2_conceding_conversion$hsc, accuracy = 0.01)
# sp2_conceding_conversion$ASCC <- percent(sp2_conceding_conversion$TFtac/sp2_conceding_conversion$asc, accuracy = 0.01)
# sp2_conceding_conversion$TSCC <- percent(sp2_conceding_conversion$TGC/sp2_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# sp2_shots_analysis <- merge(sp2_scoring_conversion,sp2_conceding_conversion,by='Group.1',all = T)
# #SP2
# #home goals scored
# sp2_home_gs <- aggregate(SP2$FTHG, by = list(SP2$HomeTeam), FUN = sum)
# sp2_home_gs_avg <- aggregate(SP2$FTHG, by = list(SP2$HomeTeam),mean)
# sp2_home_scoring <- merge(sp2_home_gs,sp2_home_gs_avg, by='Group.1',all = T)
# names(sp2_home_scoring)[names(sp2_home_scoring) == "x.x"] <- "TFthg"
# names(sp2_home_scoring)[names(sp2_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# sp2_away_gs <- aggregate(SP2$FTAG, by = list(SP2$AwayTeam), FUN = sum)
# sp2_away_gs_avg <- aggregate(SP2$FTAG, by = list(SP2$AwayTeam),mean)
# sp2_away_scoring <- merge(sp2_away_gs,sp2_away_gs_avg, by='Group.1',all = T)
# names(sp2_away_scoring)[names(sp2_away_scoring) == "x.x"] <- "TFtag"
# names(sp2_away_scoring)[names(sp2_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# sp2_scoring <- merge(sp2_home_scoring,sp2_away_scoring,by='Group.1',all = T)
# sp2_scoring$TGS <- sp2_scoring$TFthg + sp2_scoring$TFtag
#
# #Home shots on target
# sp2_home_hst <- aggregate(SP2$HST, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_ast <- aggregate(SP2$AST, by = list(SP2$AwayTeam), FUN = sum)
# sp2_tst <- merge(sp2_home_hst,sp2_away_ast, by='Group.1',all = T)
# names(sp2_tst)[names(sp2_tst) == "x.x"] <- "hst"
# names(sp2_tst)[names(sp2_tst) == "x.y"] <- "ast"
# sp2_tst$TST <- sp2_tst$hst + sp2_tst$ast
# #merge goals scored and shots on target
# sp2_scoring_conversion <- merge(sp2_tst,sp2_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# sp2_scoring_conversion$HSTC <- percent(sp2_scoring_conversion$TFthg/sp2_scoring_conversion$hst, accuracy = 0.01)
# sp2_scoring_conversion$ASTC <- percent(sp2_scoring_conversion$TFtag/sp2_scoring_conversion$ast, accuracy = 0.01)
# sp2_scoring_conversion$TSTC <- percent(sp2_scoring_conversion$TGS/sp2_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# sp2_scoring_conversion <- cbind(sp2_scoring_conversion,sp2_games_played)
# #create the second part
# #home goals conceded
# sp2_home_gc <- aggregate(SP2$FTAG, by = list(SP2$HomeTeam), FUN = sum)
# sp2_home_gc_avg <- aggregate(SP2$FTAG, by = list(SP2$HomeTeam),mean)
# sp2_home_conceding <- merge(sp2_home_gc,sp2_home_gc_avg, by='Group.1',all = T)
# names(sp2_home_conceding)[names(sp2_home_conceding) == "x.x"] <- "TFthc"
# names(sp2_home_conceding)[names(sp2_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# sp2_away_gc <- aggregate(SP2$FTHG, by = list(SP2$AwayTeam), FUN = sum)
# sp2_away_gc_avg <- aggregate(SP2$FTHG, by = list(SP2$AwayTeam),mean)
# sp2_away_conceding <- merge(sp2_away_gc,sp2_away_gc_avg, by='Group.1',all = T)
# names(sp2_away_conceding)[names(sp2_away_conceding) == "x.x"] <- "TFtac"
# names(sp2_away_conceding)[names(sp2_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# sp2_conceding <- merge(sp2_home_conceding,sp2_away_conceding,by='Group.1',all = T)
# sp2_conceding$TGC <- sp2_conceding$TFthc + sp2_conceding$TFtac
#
# #Home shots conceded
# sp2_home_hsc <- aggregate(SP2$AST, by = list(SP2$HomeTeam), FUN = sum)
# sp2_away_asc <- aggregate(SP2$HST, by = list(SP2$AwayTeam), FUN = sum)
# sp2_tsc <- merge(sp2_home_hsc,sp2_away_asc, by='Group.1',all = T)
# names(sp2_tsc)[names(sp2_tsc) == "x.x"] <- "hsc"
# names(sp2_tsc)[names(sp2_tsc) == "x.y"] <- "asc"
# sp2_tsc$TSC <- sp2_tsc$hsc + sp2_tsc$asc
# #merge goals conceded and shots conceded
# sp2_conceding_conversion <- merge(sp2_tsc,sp2_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# sp2_conceding_conversion$HSCC <- percent(sp2_conceding_conversion$TFthc/sp2_conceding_conversion$hsc, accuracy = 0.01)
# sp2_conceding_conversion$ASCC <- percent(sp2_conceding_conversion$TFtac/sp2_conceding_conversion$asc, accuracy = 0.01)
# sp2_conceding_conversion$TSCC <- percent(sp2_conceding_conversion$TGC/sp2_conceding_conversion$TSC, accuracy = 0.01)
# sp2_conceding_conversion$XSTC <- round(sp2_scoring$TGS/(sp2_tst$TST - sp2_scoring$TGS), digits = 2)
#
# #merge the two parts
# sp2_shots_analysis <- merge(sp2_scoring_conversion,sp2_conceding_conversion,by='Group.1',all = T)
###################################################################################################################################
# #T1 version 1
# #home goals scored
# t1_home_gs <- aggregate(T1$FTHG, by = list(T1$HomeTeam), FUN = sum)
# t1_home_gs_avg <- aggregate(T1$FTHG, by = list(T1$HomeTeam),mean)
# t1_home_scoring <- merge(t1_home_gs,t1_home_gs_avg, by='Group.1',all = T)
# names(t1_home_scoring)[names(t1_home_scoring) == "x.x"] <- "TFthg"
# names(t1_home_scoring)[names(t1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# t1_away_gs <- aggregate(T1$FTAG, by = list(T1$AwayTeam), FUN = sum)
# t1_away_gs_avg <- aggregate(T1$FTAG, by = list(T1$AwayTeam),mean)
# t1_away_scoring <- merge(t1_away_gs,t1_away_gs_avg, by='Group.1',all = T)
# names(t1_away_scoring)[names(t1_away_scoring) == "x.x"] <- "TFtag"
# names(t1_away_scoring)[names(t1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# t1_scoring <- merge(t1_home_scoring,t1_away_scoring,by='Group.1',all = T)
# t1_scoring$TGS <- t1_scoring$TFthg + t1_scoring$TFtag
#
# #Home shots on target
# t1_home_hst <- aggregate(T1$HST, by = list(T1$HomeTeam), FUN = sum)
# t1_away_ast <- aggregate(T1$AST, by = list(T1$AwayTeam), FUN = sum)
# t1_tst <- merge(t1_home_hst,t1_away_ast, by='Group.1',all = T)
# names(t1_tst)[names(t1_tst) == "x.x"] <- "hst"
# names(t1_tst)[names(t1_tst) == "x.y"] <- "ast"
# t1_tst$TST <- t1_tst$hst + t1_tst$ast
# #merge goals scored and shots on target
# t1_scoring_conversion <- merge(t1_tst,t1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# t1_scoring_conversion$HSTC <- percent(t1_scoring_conversion$TFthg/t1_scoring_conversion$hst, accuracy = 0.01)
# t1_scoring_conversion$ASTC <- percent(t1_scoring_conversion$TFtag/t1_scoring_conversion$ast, accuracy = 0.01)
# t1_scoring_conversion$TSTC <- percent(t1_scoring_conversion$TGS/t1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# t1_scoring_conversion <- cbind(t1_scoring_conversion,t1_games_played)
# #create the second part
# #home goals conceded
# t1_home_gc <- aggregate(T1$FTAG, by = list(T1$HomeTeam), FUN = sum)
# t1_home_gc_avg <- aggregate(T1$FTAG, by = list(T1$HomeTeam),mean)
# t1_home_conceding <- merge(t1_home_gc,t1_home_gc_avg, by='Group.1',all = T)
# names(t1_home_conceding)[names(t1_home_conceding) == "x.x"] <- "TFthc"
# names(t1_home_conceding)[names(t1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# t1_away_gc <- aggregate(T1$FTHG, by = list(T1$AwayTeam), FUN = sum)
# t1_away_gc_avg <- aggregate(T1$FTHG, by = list(T1$AwayTeam),mean)
# t1_away_conceding <- merge(t1_away_gc,t1_away_gc_avg, by='Group.1',all = T)
# names(t1_away_conceding)[names(t1_away_conceding) == "x.x"] <- "TFtac"
# names(t1_away_conceding)[names(t1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# t1_conceding <- merge(t1_home_conceding,t1_away_conceding,by='Group.1',all = T)
# t1_conceding$TGC <- t1_conceding$TFthc + t1_conceding$TFtac
#
# #Home shots conceded
# t1_home_hsc <- aggregate(T1$AST, by = list(T1$HomeTeam), FUN = sum)
# t1_away_asc <- aggregate(T1$HST, by = list(T1$AwayTeam), FUN = sum)
# t1_tsc <- merge(t1_home_hsc,t1_away_asc, by='Group.1',all = T)
# names(t1_tsc)[names(t1_tsc) == "x.x"] <- "hsc"
# names(t1_tsc)[names(t1_tsc) == "x.y"] <- "asc"
# t1_tsc$TSC <- t1_tsc$hsc + t1_tsc$asc
# #merge goals conceded and shots conceded
# t1_conceding_conversion <- merge(t1_tsc,t1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# t1_conceding_conversion$HSCC <- percent(t1_conceding_conversion$TFthc/t1_conceding_conversion$hsc, accuracy = 0.01)
# t1_conceding_conversion$ASCC <- percent(t1_conceding_conversion$TFtac/t1_conceding_conversion$asc, accuracy = 0.01)
# t1_conceding_conversion$TSCC <- percent(t1_conceding_conversion$TGC/t1_conceding_conversion$TSC, accuracy = 0.01)
#
# #merge the two parts
# t1_shots_analysis <- merge(t1_scoring_conversion,t1_conceding_conversion,by='Group.1',all = T)
# #T1
# #home goals scored
# t1_home_gs <- aggregate(T1$FTHG, by = list(T1$HomeTeam), FUN = sum)
# t1_home_gs_avg <- aggregate(T1$FTHG, by = list(T1$HomeTeam),mean)
# t1_home_scoring <- merge(t1_home_gs,t1_home_gs_avg, by='Group.1',all = T)
# names(t1_home_scoring)[names(t1_home_scoring) == "x.x"] <- "TFthg"
# names(t1_home_scoring)[names(t1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# t1_away_gs <- aggregate(T1$FTAG, by = list(T1$AwayTeam), FUN = sum)
# t1_away_gs_avg <- aggregate(T1$FTAG, by = list(T1$AwayTeam),mean)
# t1_away_scoring <- merge(t1_away_gs,t1_away_gs_avg, by='Group.1',all = T)
# names(t1_away_scoring)[names(t1_away_scoring) == "x.x"] <- "TFtag"
# names(t1_away_scoring)[names(t1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# t1_scoring <- merge(t1_home_scoring,t1_away_scoring,by='Group.1',all = T)
# t1_scoring$TGS <- t1_scoring$TFthg + t1_scoring$TFtag
#
# #Home shots on target
# t1_home_hst <- aggregate(T1$HST, by = list(T1$HomeTeam), FUN = sum)
# t1_away_ast <- aggregate(T1$AST, by = list(T1$AwayTeam), FUN = sum)
# t1_tst <- merge(t1_home_hst,t1_away_ast, by='Group.1',all = T)
# names(t1_tst)[names(t1_tst) == "x.x"] <- "hst"
# names(t1_tst)[names(t1_tst) == "x.y"] <- "ast"
# t1_tst$TST <- t1_tst$hst + t1_tst$ast
# #merge goals scored and shots on target
# t1_scoring_conversion <- merge(t1_tst,t1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# t1_scoring_conversion$HSTC <- percent(t1_scoring_conversion$TFthg/t1_scoring_conversion$hst, accuracy = 0.01)
# t1_scoring_conversion$ASTC <- percent(t1_scoring_conversion$TFtag/t1_scoring_conversion$ast, accuracy = 0.01)
# t1_scoring_conversion$TSTC <- percent(t1_scoring_conversion$TGS/t1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# t1_scoring_conversion <- cbind(t1_scoring_conversion,t1_games_played)
# #create the second part
# #home goals conceded
# t1_home_gc <- aggregate(T1$FTAG, by = list(T1$HomeTeam), FUN = sum)
# t1_home_gc_avg <- aggregate(T1$FTAG, by = list(T1$HomeTeam),mean)
# t1_home_conceding <- merge(t1_home_gc,t1_home_gc_avg, by='Group.1',all = T)
# names(t1_home_conceding)[names(t1_home_conceding) == "x.x"] <- "TFthc"
# names(t1_home_conceding)[names(t1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# t1_away_gc <- aggregate(T1$FTHG, by = list(T1$AwayTeam), FUN = sum)
# t1_away_gc_avg <- aggregate(T1$FTHG, by = list(T1$AwayTeam),mean)
# t1_away_conceding <- merge(t1_away_gc,t1_away_gc_avg, by='Group.1',all = T)
# names(t1_away_conceding)[names(t1_away_conceding) == "x.x"] <- "TFtac"
# names(t1_away_conceding)[names(t1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# t1_conceding <- merge(t1_home_conceding,t1_away_conceding,by='Group.1',all = T)
# t1_conceding$TGC <- t1_conceding$TFthc + t1_conceding$TFtac
#
# #Home shots conceded
# t1_home_hsc <- aggregate(T1$AST, by = list(T1$HomeTeam), FUN = sum)
# t1_away_asc <- aggregate(T1$HST, by = list(T1$AwayTeam), FUN = sum)
# t1_tsc <- merge(t1_home_hsc,t1_away_asc, by='Group.1',all = T)
# names(t1_tsc)[names(t1_tsc) == "x.x"] <- "hsc"
# names(t1_tsc)[names(t1_tsc) == "x.y"] <- "asc"
# t1_tsc$TSC <- t1_tsc$hsc + t1_tsc$asc
# #merge goals conceded and shots conceded
# t1_conceding_conversion <- merge(t1_tsc,t1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# t1_conceding_conversion$HSCC <- percent(t1_conceding_conversion$TFthc/t1_conceding_conversion$hsc, accuracy = 0.01)
# t1_conceding_conversion$ASCC <- percent(t1_conceding_conversion$TFtac/t1_conceding_conversion$asc, accuracy = 0.01)
# t1_conceding_conversion$TSCC <- percent(t1_conceding_conversion$TGC/t1_conceding_conversion$TSC, accuracy = 0.01)
# t1_conceding_conversion$XSTC <- round(t1_scoring$TGS/(t1_tst$TST - t1_scoring$TGS), digits = 2)
#
# #merge the two parts
# t1_shots_analysis <- merge(t1_scoring_conversion,t1_conceding_conversion,by='Group.1',all = T)

#T1
# t1_home_gs <- aggregate(T1$FTHG, by = list(T1$HomeTeam), FUN = sum)
# t1_home_gs_avg <- aggregate(T1$FTHG, by = list(T1$HomeTeam),mean)
# t1_home_scoring <- merge(t1_home_gs,t1_home_gs_avg, by='Group.1',all = T)
# names(t1_home_scoring)[names(t1_home_scoring) == "x.x"] <- "TFthg"
# names(t1_home_scoring)[names(t1_home_scoring) == "x.y"] <- "Avg_Fthg"
# #away goals scored
# t1_away_gs <- aggregate(T1$FTAG, by = list(T1$AwayTeam), FUN = sum)
# t1_away_gs_avg <- aggregate(T1$FTAG, by = list(T1$AwayTeam),mean)
# t1_away_scoring <- merge(t1_away_gs,t1_away_gs_avg, by='Group.1',all = T)
# names(t1_away_scoring)[names(t1_away_scoring) == "x.x"] <- "TFtag"
# names(t1_away_scoring)[names(t1_away_scoring) == "x.y"] <- "Avg_Ftag"
# #total goals scored
# t1_scoring <- merge(t1_home_scoring,t1_away_scoring,by='Group.1',all = T)
# t1_scoring$TGS <- t1_scoring$TFthg + t1_scoring$TFtag
#
# #Home shots on target
# t1_home_hst <- aggregate(T1$HST, by = list(T1$HomeTeam), FUN = sum)
# t1_away_ast <- aggregate(T1$AST, by = list(T1$AwayTeam), FUN = sum)
# t1_tst <- merge(t1_home_hst,t1_away_ast, by='Group.1',all = T)
# names(t1_tst)[names(t1_tst) == "x.x"] <- "hst"
# names(t1_tst)[names(t1_tst) == "x.y"] <- "ast"
# t1_tst$TST <- t1_tst$hst + t1_tst$ast
# #merge goals scored and shots on target
# t1_scoring_conversion <- merge(t1_tst,t1_scoring,by='Group.1',all = T)
# #add HSC ASC TSC
# t1_scoring_conversion$HSTC <- percent(t1_scoring_conversion$TFthg/t1_scoring_conversion$hst, accuracy = 0.01)
# t1_scoring_conversion$ASTC <- percent(t1_scoring_conversion$TFtag/t1_scoring_conversion$ast, accuracy = 0.01)
# t1_scoring_conversion$TSTC <- percent(t1_scoring_conversion$TGS/t1_scoring_conversion$TST, accuracy = 0.01)
# #merge games played
# t1_scoring_conversion <- cbind(t1_scoring_conversion,t1_games_played)
# #create the second part
# #home goals conceded
# t1_home_gc <- aggregate(T1$FTAG, by = list(T1$HomeTeam), FUN = sum)
# t1_home_gc_avg <- aggregate(T1$FTAG, by = list(T1$HomeTeam),mean)
# t1_home_conceding <- merge(t1_home_gc,t1_home_gc_avg, by='Group.1',all = T)
# names(t1_home_conceding)[names(t1_home_conceding) == "x.x"] <- "TFthc"
# names(t1_home_conceding)[names(t1_home_conceding) == "x.y"] <- "Avg_Fthc"
# #away goals conceded
# t1_away_gc <- aggregate(T1$FTHG, by = list(T1$AwayTeam), FUN = sum)
# t1_away_gc_avg <- aggregate(T1$FTHG, by = list(T1$AwayTeam),mean)
# t1_away_conceding <- merge(t1_away_gc,t1_away_gc_avg, by='Group.1',all = T)
# names(t1_away_conceding)[names(t1_away_conceding) == "x.x"] <- "TFtac"
# names(t1_away_conceding)[names(t1_away_conceding) == "x.y"] <- "Avg_Ftac"
# #total goals conceded
# t1_conceding <- merge(t1_home_conceding,t1_away_conceding,by='Group.1',all = T)
# t1_conceding$TGC <- t1_conceding$TFthc + t1_conceding$TFtac
#
# #Home shots conceded
# t1_home_hsc <- aggregate(T1$AST, by = list(T1$HomeTeam), FUN = sum)
# t1_away_asc <- aggregate(T1$HST, by = list(T1$AwayTeam), FUN = sum)
# t1_tsc <- merge(t1_home_hsc,t1_away_asc, by='Group.1',all = T)
# names(t1_tsc)[names(t1_tsc) == "x.x"] <- "hsc"
# names(t1_tsc)[names(t1_tsc) == "x.y"] <- "asc"
# t1_tsc$TSC <- t1_tsc$hsc + t1_tsc$asc
# #merge goals conceded and shots conceded
# t1_conceding_conversion <- merge(t1_tsc,t1_conceding,by='Group.1',all = T)
#
# #add HSC ASC TSC
# t1_conceding_conversion$HSCC <- percent(t1_conceding_conversion$TFthc/t1_conceding_conversion$hsc, accuracy = 0.01)
# t1_conceding_conversion$ASCC <- percent(t1_conceding_conversion$TFtac/t1_conceding_conversion$asc, accuracy = 0.01)
# t1_conceding_conversion$TSCC <- percent(t1_conceding_conversion$TGC/t1_conceding_conversion$TSC, accuracy = 0.01)
# t1_conceding_conversion$XSTC <- round(t1_scoring$TGS/(t1_tst$TST - t1_scoring$TGS), digits = 2)
#
# #merge the two parts
# t1_shots_analysis <- merge(t1_scoring_conversion,t1_conceding_conversion,by='Group.1',all = T)



# #Write out the data to excel
# write.xlsx(mls_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "MLS")
# write.xlsx(arg_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "ARG", append = TRUE)
# write.xlsx(d2_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "D2", append = TRUE)
# write.xlsx(e0_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "E0", append = TRUE)
# write.xlsx(e1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "E1", append = TRUE)
# write.xlsx(e2_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "E2", append = TRUE)
# write.xlsx(e3_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "E3", append = TRUE)
# write.xlsx(ec_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "EC", append = TRUE)
# write.xlsx(f1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "F1", append = TRUE)
# write.xlsx(f2_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "F2", append = TRUE)
# write.xlsx(g1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "G1", append = TRUE)
# write.xlsx(i1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "I1", append = TRUE)
# write.xlsx(i2_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "I2", append = TRUE)
# write.xlsx(n1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "N1", append = TRUE)
# write.xlsx(p1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "P1", append = TRUE)
# write.xlsx(sc0_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "SC0", append = TRUE)
# write.xlsx(sc1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "SC1", append = TRUE)
# write.xlsx(sc2_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "SC2", append = TRUE)
# write.xlsx(sc3_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "SC3", append = TRUE)
# write.xlsx(sp1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "SP1", append = TRUE)
# write.xlsx(sp2_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "SP2", append = TRUE)
# write.xlsx(t1_shots_analysis,'ShotsAnalysis.xlsx',sheetName = "T1", append = TRUE)
