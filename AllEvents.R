library('xlsx')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

MLS_fixtures_clone_final <- MLS_fixtures_clone[,-c(8,9,10,27)]
MLS_fixtures_clone_final[,'sep'] <- ''

mls_dmprediction <-  mls_picks[,c(4,5,6,7,8)]
mls_dmprediction[,'sep2'] <- ''

mls_avgyellow <- mls_picks[,c(9,10)]
mls_avgyellow[,'sep3'] <- ''

mls_avgcorners <- mls_picks[,c(11,12)]
mls_avgcorners[,'sep4'] <- ''

mls_goals <- MLS_fixtures[,c(10,11)]
mls_goals$mls_xGH <- round(mls_goals$mls_xGH, digits = 2)
mls_goals$mls_xGA <- round(mls_goals$mls_xGA, digits = 2)
mls_goals$mls_TxG <- mls_goals$mls_xGH + mls_goals$mls_xGA
mls_goals[,'sep5'] <- ''

mls_shots <- MLS_fixtures_sot[,c(10,11)]
mls_shots$mls_xHST <- round(mls_shots$mls_xHST, digits = 2)
mls_shots$mls_xAST <- round(mls_shots$mls_xAST, digits = 2)
mls_shots$TxSOT <- mls_shots$mls_xHST + mls_shots$mls_xAST
mls_shots[,'sep6'] <- ''

mls_fouls <- MLS_fixtures_fo[,c(10,11)]
mls_fouls$mls_xHF <- round(mls_fouls$mls_xHF, digits = 2)
mls_fouls$mls_xAF <- round(mls_fouls$mls_xAF, digits = 2)
mls_fouls$mls_TxF <- mls_fouls$mls_xHF + mls_fouls$mls_xAF

mls_ycpf <- mls_picks[,c(15,16)]
mls_fouls <- cbind(mls_fouls,mls_ycpf)
mls_fouls$HYCPF <- as.numeric(mls_fouls$HYCPF)
mls_fouls$AYCPF <- as.numeric(mls_fouls$AYCPF)
mls_fouls$x_hyc <- (mls_fouls$mls_xHF) * (mls_fouls$HYCPF)
mls_fouls$x_ayc <- (mls_fouls$mls_xAF) * (mls_fouls$AYCPF)
mls_fouls$x_TYC <- round((mls_fouls$x_hyc + mls_fouls$x_ayc),digits = 2)
mls_fouls[,'sep7'] <- ''

mls_bookings <- MLS_fixtures_yc[,c(10,11)]
mls_bookings$mls_xHYC <- round(mls_bookings$mls_xHYC, digits = 2)
mls_bookings$mls_xAYC <- round(mls_bookings$mls_xAYC, digits = 2)
mls_bookings$mls_TYcards <- mls_bookings$mls_xHYC + mls_bookings$mls_xAYC
mls_bookings[,'sep8'] <- ''

mls_corners <- MLS_fixtures_co[,c(10,11)]
mls_corners$mls_xHCOC <- round(mls_corners$mls_xHCOC, digits = 2)
mls_corners$mls_xACOC <- round(mls_corners$mls_xACOC, digits = 2)
mls_corners$mls_TCOs <- mls_corners$mls_xHCOC + mls_corners$mls_xACOC
mls_corners[,'sep9'] <- ''

mls_shotsconversion <- mls_picks[,c(13,14)]
mls_shotsconversion <- cbind(mls_shotsconversion,mls_shots)
mls_shotsconversion$HXSC <- as.numeric(mls_shotsconversion$HXSC)
mls_shotsconversion$AXSC <- as.numeric(mls_shotsconversion$AXSC)
mls_shotsconversion$mls_hXgoals <- round((mls_shotsconversion$HXSC * mls_shotsconversion$mls_xHST), digits = 2)
mls_shotsconversion$mls_aXgoals <- round((mls_shotsconversion$AXSC * mls_shotsconversion$mls_xAST), digits = 2)
mls_shotsconversion$Xgoals <- mls_shotsconversion$mls_hXgoals + mls_shotsconversion$mls_aXgoals

MLS_all <- cbind(MLS_fixtures_clone_final,mls_dmprediction,mls_avgyellow,mls_avgcorners,mls_goals,mls_shots,mls_fouls,mls_bookings,mls_corners,mls_shotsconversion)
write.xlsx(MLS_all,'Divisions/MLS.xlsx', sheetName = "MLS_all", append = TRUE)
###############################################################################################################################################################
ARG_fixtures_clone_final <- ARG_fixtures_clone[,-c(8,9,10,27)]
ARG_fixtures_clone_final[,'sep'] <- ''

arg_dmprediction <-  arg_picks[,c(4,5,6,7,8)]
arg_dmprediction[,'sep2'] <- ''

arg_avgyellow <- arg_picks[,c(9,10)]
arg_avgyellow[,'sep3'] <- ''

arg_avgcorners <- arg_picks[,c(11,12)]
arg_avgcorners[,'sep4'] <- ''

arg_goals <- ARG_fixtures[,c(10,11)]
arg_goals$arg_xGH <- round(arg_goals$arg_xGH, digits = 2)
arg_goals$arg_xGA <- round(arg_goals$arg_xGA, digits = 2)
arg_goals$arg_TxG <- arg_goals$arg_xGH + arg_goals$arg_xGA
arg_goals[,'sep5'] <- ''

arg_shots <- ARG_fixtures_sot[,c(10,11)]
arg_shots$arg_xHST <- round(arg_shots$arg_xHST, digits = 2)
arg_shots$arg_xAST <- round(arg_shots$arg_xAST, digits = 2)
arg_shots$TxSOT <- arg_shots$arg_xHST + arg_shots$arg_xAST
arg_shots[,'sep6'] <- ''

arg_fouls <- ARG_fixtures_fo[,c(10,11)]
arg_fouls$arg_xHF <- round(arg_fouls$arg_xHF, digits = 2)
arg_fouls$arg_xAF <- round(arg_fouls$arg_xAF, digits = 2)
arg_fouls$arg_TxF <- arg_fouls$arg_xHF + arg_fouls$arg_xAF

arg_ycpf <- arg_picks[,c(15,16)]
arg_fouls <- cbind(arg_fouls,arg_ycpf)
arg_fouls$HYCPF <- as.numeric(arg_fouls$HYCPF)
arg_fouls$AYCPF <- as.numeric(arg_fouls$AYCPF)
arg_fouls$x_hyc <- (arg_fouls$arg_xHF) * (arg_fouls$HYCPF)
arg_fouls$x_ayc <- (arg_fouls$arg_xAF) * (arg_fouls$AYCPF)
arg_fouls$x_TYC <- round((arg_fouls$x_hyc + arg_fouls$x_ayc),digits = 2)
arg_fouls[,'sep7'] <- ''

arg_bookings <- ARG_fixtures_yc[,c(10,11)]
arg_bookings$arg_xHYC <- round(arg_bookings$arg_xHYC, digits = 2)
arg_bookings$arg_xAYC <- round(arg_bookings$arg_xAYC, digits = 2)
arg_bookings$arg_TYcards <- arg_bookings$arg_xHYC + arg_bookings$arg_xAYC
arg_bookings[,'sep8'] <- ''

arg_corners <- ARG_fixtures_co[,c(10,11)]
arg_corners$arg_xHCOC <- round(arg_corners$arg_xHCOC, digits = 2)
arg_corners$arg_xACOC <- round(arg_corners$arg_xACOC, digits = 2)
arg_corners$arg_TCOs <- arg_corners$arg_xHCOC + arg_corners$arg_xACOC
arg_corners[,'sep9'] <- ''

arg_shotsconversion <- arg_picks[,c(13,14)]
arg_shotsconversion <- cbind(arg_shotsconversion,arg_shots)
arg_shotsconversion$HXSC <- as.numeric(arg_shotsconversion$HXSC)
arg_shotsconversion$AXSC <- as.numeric(arg_shotsconversion$AXSC)
arg_shotsconversion$arg_hXgoals <- round((arg_shotsconversion$HXSC * arg_shotsconversion$arg_xHST), digits = 2)
arg_shotsconversion$arg_aXgoals <- round((arg_shotsconversion$AXSC * arg_shotsconversion$arg_xAST), digits = 2)
arg_shotsconversion$Xgoals <- arg_shotsconversion$arg_hXgoals + arg_shotsconversion$arg_aXgoals

ARG_all <- cbind(ARG_fixtures_clone_final,arg_dmprediction,arg_avgyellow,arg_avgcorners,arg_goals,arg_shots,arg_fouls,arg_bookings,arg_corners,arg_shotsconversion)
write.xlsx(ARG_all,'Divisions/ARG.xlsx', sheetName = "ARG_all", append = TRUE)
#########################################################################################################################################################
# D2_fixtures_clone_final <- D2_fixtures_clone[,-c(8,9,10,27)]
# D2_fixtures_clone_final[,'sep'] <- ''
#
# d2_dmprediction <-  d2_picks[,c(4,5,6,7,8)]
# d2_dmprediction[,'sep2'] <- ''
#
# d2_avgyellow <- d2_picks[,c(9,10)]
# d2_avgyellow[,'sep3'] <- ''
#
# d2_avgcorners <- d2_picks[,c(11,12)]
# d2_avgcorners[,'sep4'] <- ''
#
# d2_goals <- D2_fixtures[,c(10,11)]
# d2_goals$d2_xGH <- round(d2_goals$d2_xGH, digits = 2)
# d2_goals$d2_xGA <- round(d2_goals$d2_xGA, digits = 2)
# d2_goals$d2_TxG <- d2_goals$d2_xGH + d2_goals$d2_xGA
# d2_goals[,'sep5'] <- ''
#
# d2_shots <- D2_fixtures_sot[,c(10,11)]
# d2_shots$d2_xHST <- round(d2_shots$d2_xHST, digits = 2)
# d2_shots$d2_xAST <- round(d2_shots$d2_xAST, digits = 2)
# d2_shots$TxSOT <- d2_shots$d2_xHST + d2_shots$d2_xAST
# d2_shots[,'sep6'] <- ''
#
# d2_fouls <- D2_fixtures_fo[,c(10,11)]
# d2_fouls$d2_xHF <- round(d2_fouls$d2_xHF, digits = 2)
# d2_fouls$d2_xAF <- round(d2_fouls$d2_xAF, digits = 2)
# d2_fouls$d2_TxF <- d2_fouls$d2_xHF + d2_fouls$d2_xAF
#
# d2_ycpf <- d2_picks[,c(15,16)]
# d2_fouls <- cbind(d2_fouls,d2_ycpf)
# d2_fouls$HYCPF <- as.numeric(d2_fouls$HYCPF)
# d2_fouls$AYCPF <- as.numeric(d2_fouls$AYCPF)
# d2_fouls$x_hyc <- (d2_fouls$d2_xHF) * (d2_fouls$HYCPF)
# d2_fouls$x_ayc <- (d2_fouls$d2_xAF) * (d2_fouls$AYCPF)
# d2_fouls$x_TYC <- round((d2_fouls$x_hyc + d2_fouls$x_ayc),digits = 2)
# d2_fouls[,'sep7'] <- ''
#
# d2_bookings <- D2_fixtures_yc[,c(10,11)]
# d2_bookings$d2_xHYC <- round(d2_bookings$d2_xHYC, digits = 2)
# d2_bookings$d2_xAYC <- round(d2_bookings$d2_xAYC, digits = 2)
# d2_bookings$d2_TYcards <- d2_bookings$d2_xHYC + d2_bookings$d2_xAYC
# d2_bookings[,'sep8'] <- ''
#
# d2_corners <- D2_fixtures_co[,c(10,11)]
# d2_corners$d2_xHCOC <- round(d2_corners$d2_xHCOC, digits = 2)
# d2_corners$d2_xACOC <- round(d2_corners$d2_xACOC, digits = 2)
# d2_corners$d2_TCOs <- d2_corners$d2_xHCOC + d2_corners$d2_xACOC
# d2_corners[,'sep9'] <- ''
#
# d2_shotsconversion <- d2_picks[,c(13,14)]
# d2_shotsconversion <- cbind(d2_shotsconversion,d2_shots)
# d2_shotsconversion$HXSC <- as.numeric(d2_shotsconversion$HXSC)
# d2_shotsconversion$AXSC <- as.numeric(d2_shotsconversion$AXSC)
# d2_shotsconversion$d2_hXgoals <- round((d2_shotsconversion$HXSC * d2_shotsconversion$d2_xHST), digits = 2)
# d2_shotsconversion$d2_aXgoals <- round((d2_shotsconversion$AXSC * d2_shotsconversion$d2_xAST), digits = 2)
# d2_shotsconversion$Xgoals <- d2_shotsconversion$d2_hXgoals + d2_shotsconversion$d2_aXgoals
#
# D2_all <- cbind(D2_fixtures_clone_final,d2_dmprediction,d2_avgyellow,d2_avgcorners,d2_goals,d2_shots,d2_fouls,d2_bookings,d2_corners,d2_shotsconversion)
# write.xlsx(D2_all,'Divisions/D2.xlsx', sheetName = "D2_all", append = TRUE)
# ########################################################################################################################################################
# E0_fixtures_clone_final <- E0_fixtures_clone[,-c(8,9,10,27)]
# E0_fixtures_clone_final[,'sep'] <- ''
#
# e0_dmprediction <-  e0_picks[,c(4,5,6,7,8)]
# e0_dmprediction[,'sep2'] <- ''
#
# e0_avgyellow <- e0_picks[,c(9,10)]
# e0_avgyellow[,'sep3'] <- ''
#
# e0_avgcorners <- e0_picks[,c(11,12)]
# e0_avgcorners[,'sep4'] <- ''
#
# e0_goals <- E0_fixtures[,c(10,11)]
# e0_goals$e0_xGH <- round(e0_goals$e0_xGH, digits = 2)
# e0_goals$e0_xGA <- round(e0_goals$e0_xGA, digits = 2)
# e0_goals$e0_TxG <- e0_goals$e0_xGH + e0_goals$e0_xGA
# e0_goals[,'sep5'] <- ''
#
# e0_shots <- E0_fixtures_sot[,c(10,11)]
# e0_shots$e0_xHST <- round(e0_shots$e0_xHST, digits = 2)
# e0_shots$e0_xAST <- round(e0_shots$e0_xAST, digits = 2)
# e0_shots$TxSOT <- e0_shots$e0_xHST + e0_shots$e0_xAST
# e0_shots[,'sep6'] <- ''
#
# e0_fouls <- E0_fixtures_fo[,c(10,11)]
# e0_fouls$e0_xHF <- round(e0_fouls$e0_xHF, digits = 2)
# e0_fouls$e0_xAF <- round(e0_fouls$e0_xAF, digits = 2)
# e0_fouls$e0_TxF <- e0_fouls$e0_xHF + e0_fouls$e0_xAF
#
# e0_ycpf <- e0_picks[,c(15,16)]
# e0_fouls <- cbind(e0_fouls,e0_ycpf)
# e0_fouls$HYCPF <- as.numeric(e0_fouls$HYCPF)
# e0_fouls$AYCPF <- as.numeric(e0_fouls$AYCPF)
# e0_fouls$x_hyc <- (e0_fouls$e0_xHF) * (e0_fouls$HYCPF)
# e0_fouls$x_ayc <- (e0_fouls$e0_xAF) * (e0_fouls$AYCPF)
# e0_fouls$x_TYC <- round((e0_fouls$x_hyc + e0_fouls$x_ayc),digits = 2)
# e0_fouls[,'sep7'] <- ''
#
# e0_bookings <- E0_fixtures_yc[,c(10,11)]
# e0_bookings$e0_xHYC <- round(e0_bookings$e0_xHYC, digits = 2)
# e0_bookings$e0_xAYC <- round(e0_bookings$e0_xAYC, digits = 2)
# e0_bookings$e0_TYcards <- e0_bookings$e0_xHYC + e0_bookings$e0_xAYC
# e0_bookings[,'sep8'] <- ''
#
# e0_corners <- E0_fixtures_co[,c(10,11)]
# e0_corners$e0_xHCOC <- round(e0_corners$e0_xHCOC, digits = 2)
# e0_corners$e0_xACOC <- round(e0_corners$e0_xACOC, digits = 2)
# e0_corners$e0_TCOs <- e0_corners$e0_xHCOC + e0_corners$e0_xACOC
# e0_corners[,'sep9'] <- ''
#
# e0_shotsconversion <- e0_picks[,c(13,14)]
# e0_shotsconversion <- cbind(e0_shotsconversion,e0_shots)
# e0_shotsconversion$HXSC <- as.numeric(e0_shotsconversion$HXSC)
# e0_shotsconversion$AXSC <- as.numeric(e0_shotsconversion$AXSC)
# e0_shotsconversion$e0_hXgoals <- round((e0_shotsconversion$HXSC * e0_shotsconversion$e0_xHST), digits = 2)
# e0_shotsconversion$e0_aXgoals <- round((e0_shotsconversion$AXSC * e0_shotsconversion$e0_xAST), digits = 2)
# e0_shotsconversion$Xgoals <- e0_shotsconversion$e0_hXgoals + e0_shotsconversion$e0_aXgoals
#
# E0_all <- cbind(E0_fixtures_clone_final,e0_dmprediction,e0_avgyellow,e0_avgcorners,e0_goals,e0_shots,e0_fouls,e0_bookings,e0_corners,e0_shotsconversion)
# write.xlsx(E0_all,'Divisions/E0.xlsx', sheetName = "E0_all", append = TRUE)
# #########################################################################################################################################################
# E1_fixtures_clone_final <- E1_fixtures_clone[,-c(8,9,10,27)]
# E1_fixtures_clone_final[,'sep'] <- ''
#
# e1_dmprediction <-  e1_picks[,c(4,5,6,7,8)]
# e1_dmprediction[,'sep2'] <- ''
#
# e1_avgyellow <- e1_picks[,c(9,10)]
# e1_avgyellow[,'sep3'] <- ''
#
# e1_avgcorners <- e1_picks[,c(11,12)]
# e1_avgcorners[,'sep4'] <- ''
#
# e1_goals <- E1_fixtures[,c(10,11)]
# e1_goals$e1_xGH <- round(e1_goals$e1_xGH, digits = 2)
# e1_goals$e1_xGA <- round(e1_goals$e1_xGA, digits = 2)
# e1_goals$e1_TxG <- e1_goals$e1_xGH + e1_goals$e1_xGA
# e1_goals[,'sep5'] <- ''
#
# e1_shots <- E1_fixtures_sot[,c(10,11)]
# e1_shots$e1_xHST <- round(e1_shots$e1_xHST, digits = 2)
# e1_shots$e1_xAST <- round(e1_shots$e1_xAST, digits = 2)
# e1_shots$TxSOT <- e1_shots$e1_xHST + e1_shots$e1_xAST
# e1_shots[,'sep6'] <- ''
#
# e1_fouls <- E1_fixtures_fo[,c(10,11)]
# e1_fouls$e1_xHF <- round(e1_fouls$e1_xHF, digits = 2)
# e1_fouls$e1_xAF <- round(e1_fouls$e1_xAF, digits = 2)
# e1_fouls$e1_TxF <- e1_fouls$e1_xHF + e1_fouls$e1_xAF
#
# e1_ycpf <- e1_picks[,c(15,16)]
# e1_fouls <- cbind(e1_fouls,e1_ycpf)
# e1_fouls$HYCPF <- as.numeric(e1_fouls$HYCPF)
# e1_fouls$AYCPF <- as.numeric(e1_fouls$AYCPF)
# e1_fouls$x_hyc <- (e1_fouls$e1_xHF) * (e1_fouls$HYCPF)
# e1_fouls$x_ayc <- (e1_fouls$e1_xAF) * (e1_fouls$AYCPF)
# e1_fouls$x_TYC <- round((e1_fouls$x_hyc + e1_fouls$x_ayc),digits = 2)
# e1_fouls[,'sep7'] <- ''
#
# e1_bookings <- E1_fixtures_yc[,c(10,11)]
# e1_bookings$e1_xHYC <- round(e1_bookings$e1_xHYC, digits = 2)
# e1_bookings$e1_xAYC <- round(e1_bookings$e1_xAYC, digits = 2)
# e1_bookings$e1_TYcards <- e1_bookings$e1_xHYC + e1_bookings$e1_xAYC
# e1_bookings[,'sep8'] <- ''
#
# e1_corners <- E1_fixtures_co[,c(10,11)]
# e1_corners$e1_xHCOC <- round(e1_corners$e1_xHCOC, digits = 2)
# e1_corners$e1_xACOC <- round(e1_corners$e1_xACOC, digits = 2)
# e1_corners$e1_TCOs <- e1_corners$e1_xHCOC + e1_corners$e1_xACOC
# e1_corners[,'sep9'] <- ''
#
# e1_shotsconversion <- e1_picks[,c(13,14)]
# e1_shotsconversion <- cbind(e1_shotsconversion,e1_shots)
# e1_shotsconversion$HXSC <- as.numeric(e1_shotsconversion$HXSC)
# e1_shotsconversion$AXSC <- as.numeric(e1_shotsconversion$AXSC)
# e1_shotsconversion$e1_hXgoals <- round((e1_shotsconversion$HXSC * e1_shotsconversion$e1_xHST), digits = 2)
# e1_shotsconversion$e1_aXgoals <- round((e1_shotsconversion$AXSC * e1_shotsconversion$e1_xAST), digits = 2)
# e1_shotsconversion$Xgoals <- e1_shotsconversion$e1_hXgoals + e1_shotsconversion$e1_aXgoals
#
# E1_all <- cbind(E1_fixtures_clone_final,e1_dmprediction,e1_avgyellow,e1_avgcorners,e1_goals,e1_shots,e1_fouls,e1_bookings,e1_corners,e1_shotsconversion)
# write.xlsx(E1_all,'Divisions/E1.xlsx', sheetName = "E1_all", append = TRUE)
# ########################################################################################################################################################
# E2_fixtures_clone_final <- E2_fixtures_clone[,-c(8,9,10,27)]
# E2_fixtures_clone_final[,'sep'] <- ''
#
# e2_dmprediction <-  e2_picks[,c(4,5,6,7,8)]
# e2_dmprediction[,'sep2'] <- ''
#
# e2_avgyellow <- e2_picks[,c(9,10)]
# e2_avgyellow[,'sep3'] <- ''
#
# e2_avgcorners <- e2_picks[,c(11,12)]
# e2_avgcorners[,'sep4'] <- ''
#
# e2_goals <- E2_fixtures[,c(10,11)]
# e2_goals$e2_xGH <- round(e2_goals$e2_xGH, digits = 2)
# e2_goals$e2_xGA <- round(e2_goals$e2_xGA, digits = 2)
# e2_goals$e2_TxG <- e2_goals$e2_xGH + e2_goals$e2_xGA
# e2_goals[,'sep5'] <- ''
#
# e2_shots <- E2_fixtures_sot[,c(10,11)]
# e2_shots$e2_xHST <- round(e2_shots$e2_xHST, digits = 2)
# e2_shots$e2_xAST <- round(e2_shots$e2_xAST, digits = 2)
# e2_shots$TxSOT <- e2_shots$e2_xHST + e2_shots$e2_xAST
# e2_shots[,'sep6'] <- ''
#
# e2_fouls <- E2_fixtures_fo[,c(10,11)]
# e2_fouls$e2_xHF <- round(e2_fouls$e2_xHF, digits = 2)
# e2_fouls$e2_xAF <- round(e2_fouls$e2_xAF, digits = 2)
# e2_fouls$e2_TxF <- e2_fouls$e2_xHF + e2_fouls$e2_xAF
#
# e2_ycpf <- e2_picks[,c(15,16)]
# e2_fouls <- cbind(e2_fouls,e2_ycpf)
# e2_fouls$HYCPF <- as.numeric(e2_fouls$HYCPF)
# e2_fouls$AYCPF <- as.numeric(e2_fouls$AYCPF)
# e2_fouls$x_hyc <- (e2_fouls$e2_xHF) * (e2_fouls$HYCPF)
# e2_fouls$x_ayc <- (e2_fouls$e2_xAF) * (e2_fouls$AYCPF)
# e2_fouls$x_TYC <- round((e2_fouls$x_hyc + e2_fouls$x_ayc),digits = 2)
# e2_fouls[,'sep7'] <- ''
#
# e2_bookings <- E2_fixtures_yc[,c(10,11)]
# e2_bookings$e2_xHYC <- round(e2_bookings$e2_xHYC, digits = 2)
# e2_bookings$e2_xAYC <- round(e2_bookings$e2_xAYC, digits = 2)
# e2_bookings$e2_TYcards <- e2_bookings$e2_xHYC + e2_bookings$e2_xAYC
# e2_bookings[,'sep8'] <- ''
#
# e2_corners <- E2_fixtures_co[,c(10,11)]
# e2_corners$e2_xHCOC <- round(e2_corners$e2_xHCOC, digits = 2)
# e2_corners$e2_xACOC <- round(e2_corners$e2_xACOC, digits = 2)
# e2_corners$e2_TCOs <- e2_corners$e2_xHCOC + e2_corners$e2_xACOC
# e2_corners[,'sep9'] <- ''
#
# e2_shotsconversion <- e2_picks[,c(13,14)]
# e2_shotsconversion <- cbind(e2_shotsconversion,e2_shots)
# e2_shotsconversion$HXSC <- as.numeric(e2_shotsconversion$HXSC)
# e2_shotsconversion$AXSC <- as.numeric(e2_shotsconversion$AXSC)
# e2_shotsconversion$e2_hXgoals <- round((e2_shotsconversion$HXSC * e2_shotsconversion$e2_xHST), digits = 2)
# e2_shotsconversion$e2_aXgoals <- round((e2_shotsconversion$AXSC * e2_shotsconversion$e2_xAST), digits = 2)
# e2_shotsconversion$Xgoals <- e2_shotsconversion$e2_hXgoals + e2_shotsconversion$e2_aXgoals
#
# E2_all <- cbind(E2_fixtures_clone_final,e2_dmprediction,e2_avgyellow,e2_avgcorners,e2_goals,e2_shots,e2_fouls,e2_bookings,e2_corners,e2_shotsconversion)
# write.xlsx(E2_all,'Divisions/E2.xlsx', sheetName = "E2_all", append = TRUE)
# #######################################################################################################################################################
# E3_fixtures_clone_final <- E3_fixtures_clone[,-c(8,9,10,27)]
# E3_fixtures_clone_final[,'sep'] <- ''
#
# e3_dmprediction <-  e3_picks[,c(4,5,6,7,8)]
# e3_dmprediction[,'sep2'] <- ''
#
# e3_avgyellow <- e3_picks[,c(9,10)]
# e3_avgyellow[,'sep3'] <- ''
#
# e3_avgcorners <- e3_picks[,c(11,12)]
# e3_avgcorners[,'sep4'] <- ''
#
# e3_goals <- E3_fixtures[,c(10,11)]
# e3_goals$e3_xGH <- round(e3_goals$e3_xGH, digits = 2)
# e3_goals$e3_xGA <- round(e3_goals$e3_xGA, digits = 2)
# e3_goals$e3_TxG <- e3_goals$e3_xGH + e3_goals$e3_xGA
# e3_goals[,'sep5'] <- ''
#
# e3_shots <- E3_fixtures_sot[,c(10,11)]
# e3_shots$e3_xHST <- round(e3_shots$e3_xHST, digits = 2)
# e3_shots$e3_xAST <- round(e3_shots$e3_xAST, digits = 2)
# e3_shots$TxSOT <- e3_shots$e3_xHST + e3_shots$e3_xAST
# e3_shots[,'sep6'] <- ''
#
# e3_fouls <- E3_fixtures_fo[,c(10,11)]
# e3_fouls$e3_xHF <- round(e3_fouls$e3_xHF, digits = 2)
# e3_fouls$e3_xAF <- round(e3_fouls$e3_xAF, digits = 2)
# e3_fouls$e3_TxF <- e3_fouls$e3_xHF + e3_fouls$e3_xAF
#
# e3_ycpf <- e3_picks[,c(15,16)]
# e3_fouls <- cbind(e3_fouls,e3_ycpf)
# e3_fouls$HYCPF <- as.numeric(e3_fouls$HYCPF)
# e3_fouls$AYCPF <- as.numeric(e3_fouls$AYCPF)
# e3_fouls$x_hyc <- (e3_fouls$e3_xHF) * (e3_fouls$HYCPF)
# e3_fouls$x_ayc <- (e3_fouls$e3_xAF) * (e3_fouls$AYCPF)
# e3_fouls$x_TYC <- round((e3_fouls$x_hyc + e3_fouls$x_ayc),digits = 2)
# e3_fouls[,'sep7'] <- ''
#
# e3_bookings <- E3_fixtures_yc[,c(10,11)]
# e3_bookings$e3_xHYC <- round(e3_bookings$e3_xHYC, digits = 2)
# e3_bookings$e3_xAYC <- round(e3_bookings$e3_xAYC, digits = 2)
# e3_bookings$e3_TYcards <- e3_bookings$e3_xHYC + e3_bookings$e3_xAYC
# e3_bookings[,'sep8'] <- ''
#
# e3_corners <- E3_fixtures_co[,c(10,11)]
# e3_corners$e3_xHCOC <- round(e3_corners$e3_xHCOC, digits = 2)
# e3_corners$e3_xACOC <- round(e3_corners$e3_xACOC, digits = 2)
# e3_corners$e3_TCOs <- e3_corners$e3_xHCOC + e3_corners$e3_xACOC
# e3_corners[,'sep9'] <- ''
#
# e3_shotsconversion <- e3_picks[,c(13,14)]
# e3_shotsconversion <- cbind(e3_shotsconversion,e3_shots)
# e3_shotsconversion$HXSC <- as.numeric(e3_shotsconversion$HXSC)
# e3_shotsconversion$AXSC <- as.numeric(e3_shotsconversion$AXSC)
# e3_shotsconversion$e3_hXgoals <- round((e3_shotsconversion$HXSC * e3_shotsconversion$e3_xHST), digits = 2)
# e3_shotsconversion$e3_aXgoals <- round((e3_shotsconversion$AXSC * e3_shotsconversion$e3_xAST), digits = 2)
# e3_shotsconversion$Xgoals <- e3_shotsconversion$e3_hXgoals + e3_shotsconversion$e3_aXgoals
#
# E3_all <- cbind(E3_fixtures_clone_final,e3_dmprediction,e3_avgyellow,e3_avgcorners,e3_goals,e3_shots,e3_fouls,e3_bookings,e3_corners,e3_shotsconversion)
# write.xlsx(E3_all,'Divisions/E3.xlsx', sheetName = "E3_all", append = TRUE)
# ########################################################################################################################################################
# EC_fixtures_clone_final <- EC_fixtures_clone[,-c(8,9,10,27)]
# EC_fixtures_clone_final[,'sep'] <- ''
#
# ec_dmprediction <-  ec_picks[,c(4,5,6,7,8)]
# ec_dmprediction[,'sep2'] <- ''
#
# ec_avgyellow <- ec_picks[,c(9,10)]
# ec_avgyellow[,'sep3'] <- ''
#
# ec_avgcorners <- ec_picks[,c(11,12)]
# ec_avgcorners[,'sep4'] <- ''
#
# ec_goals <- EC_fixtures[,c(10,11)]
# ec_goals$ec_xGH <- round(ec_goals$ec_xGH, digits = 2)
# ec_goals$ec_xGA <- round(ec_goals$ec_xGA, digits = 2)
# ec_goals$ec_TxG <- ec_goals$ec_xGH + ec_goals$ec_xGA
# ec_goals[,'sep5'] <- ''
#
# ec_shots <- EC_fixtures_sot[,c(10,11)]
# ec_shots$ec_xHST <- round(ec_shots$ec_xHST, digits = 2)
# ec_shots$ec_xAST <- round(ec_shots$ec_xAST, digits = 2)
# ec_shots$TxSOT <- ec_shots$ec_xHST + ec_shots$ec_xAST
# ec_shots[,'sep6'] <- ''
#
# ec_fouls <- EC_fixtures_fo[,c(10,11)]
# ec_fouls$ec_xHF <- round(ec_fouls$ec_xHF, digits = 2)
# ec_fouls$ec_xAF <- round(ec_fouls$ec_xAF, digits = 2)
# ec_fouls$ec_TxF <- ec_fouls$ec_xHF + ec_fouls$ec_xAF
#
# ec_ycpf <- ec_picks[,c(15,16)]
# ec_fouls <- cbind(ec_fouls,ec_ycpf)
# ec_fouls$HYCPF <- as.numeric(ec_fouls$HYCPF)
# ec_fouls$AYCPF <- as.numeric(ec_fouls$AYCPF)
# ec_fouls$x_hyc <- (ec_fouls$ec_xHF) * (ec_fouls$HYCPF)
# ec_fouls$x_ayc <- (ec_fouls$ec_xAF) * (ec_fouls$AYCPF)
# ec_fouls$x_TYC <- round((ec_fouls$x_hyc + ec_fouls$x_ayc),digits = 2)
# ec_fouls[,'sep7'] <- ''
#
# ec_bookings <- EC_fixtures_yc[,c(10,11)]
# ec_bookings$ec_xHYC <- round(ec_bookings$ec_xHYC, digits = 2)
# ec_bookings$ec_xAYC <- round(ec_bookings$ec_xAYC, digits = 2)
# ec_bookings$ec_TYcards <- ec_bookings$ec_xHYC + ec_bookings$ec_xAYC
# ec_bookings[,'sep8'] <- ''
#
# ec_corners <- EC_fixtures_co[,c(10,11)]
# ec_corners$ec_xHCOC <- round(ec_corners$ec_xHCOC, digits = 2)
# ec_corners$ec_xACOC <- round(ec_corners$ec_xACOC, digits = 2)
# ec_corners$ec_TCOs <- ec_corners$ec_xHCOC + ec_corners$ec_xACOC
# ec_corners[,'sep9'] <- ''
#
# ec_shotsconversion <- ec_picks[,c(13,14)]
# ec_shotsconversion <- cbind(ec_shotsconversion,ec_shots)
# ec_shotsconversion$HXSC <- as.numeric(ec_shotsconversion$HXSC)
# ec_shotsconversion$AXSC <- as.numeric(ec_shotsconversion$AXSC)
# ec_shotsconversion$ec_hXgoals <- round((ec_shotsconversion$HXSC * ec_shotsconversion$ec_xHST), digits = 2)
# ec_shotsconversion$ec_aXgoals <- round((ec_shotsconversion$AXSC * ec_shotsconversion$ec_xAST), digits = 2)
# ec_shotsconversion$Xgoals <- ec_shotsconversion$ec_hXgoals + ec_shotsconversion$ec_aXgoals
#
# EC_all <- cbind(EC_fixtures_clone_final,ec_dmprediction,ec_avgyellow,ec_avgcorners,ec_goals,ec_shots,ec_fouls,ec_bookings,ec_corners,ec_shotsconversion)
# write.xlsx(EC_all,'Divisions/EC.xlsx', sheetName = "EC_all", append = TRUE)
# ######################################################################################################################################################
# F1_fixtures_clone_final <- F1_fixtures_clone[,-c(8,9,10,27)]
# F1_fixtures_clone_final[,'sep'] <- ''
#
# f1_dmprediction <-  f1_picks[,c(4,5,6,7,8)]
# f1_dmprediction[,'sep2'] <- ''
#
# f1_avgyellow <- f1_picks[,c(9,10)]
# f1_avgyellow[,'sep3'] <- ''
#
# f1_avgcorners <- f1_picks[,c(11,12)]
# f1_avgcorners[,'sep4'] <- ''
#
# f1_goals <- F1_fixtures[,c(10,11)]
# f1_goals$f1_xGH <- round(f1_goals$f1_xGH, digits = 2)
# f1_goals$f1_xGA <- round(f1_goals$f1_xGA, digits = 2)
# f1_goals$f1_TxG <- f1_goals$f1_xGH + f1_goals$f1_xGA
# f1_goals[,'sep5'] <- ''
#
# f1_shots <- F1_fixtures_sot[,c(10,11)]
# f1_shots$f1_xHST <- round(f1_shots$f1_xHST, digits = 2)
# f1_shots$f1_xAST <- round(f1_shots$f1_xAST, digits = 2)
# f1_shots$TxSOT <- f1_shots$f1_xHST + f1_shots$f1_xAST
# f1_shots[,'sep6'] <- ''
#
# f1_fouls <- F1_fixtures_fo[,c(10,11)]
# f1_fouls$f1_xHF <- round(f1_fouls$f1_xHF, digits = 2)
# f1_fouls$f1_xAF <- round(f1_fouls$f1_xAF, digits = 2)
# f1_fouls$f1_TxF <- f1_fouls$f1_xHF + f1_fouls$f1_xAF
#
# f1_ycpf <- f1_picks[,c(15,16)]
# f1_fouls <- cbind(f1_fouls,f1_ycpf)
# f1_fouls$HYCPF <- as.numeric(f1_fouls$HYCPF)
# f1_fouls$AYCPF <- as.numeric(f1_fouls$AYCPF)
# f1_fouls$x_hyc <- (f1_fouls$f1_xHF) * (f1_fouls$HYCPF)
# f1_fouls$x_ayc <- (f1_fouls$f1_xAF) * (f1_fouls$AYCPF)
# f1_fouls$x_TYC <- round((f1_fouls$x_hyc + f1_fouls$x_ayc),digits = 2)
# f1_fouls[,'sep7'] <- ''
#
# f1_bookings <- F1_fixtures_yc[,c(10,11)]
# f1_bookings$f1_xHYC <- round(f1_bookings$f1_xHYC, digits = 2)
# f1_bookings$f1_xAYC <- round(f1_bookings$f1_xAYC, digits = 2)
# f1_bookings$f1_TYcards <- f1_bookings$f1_xHYC + f1_bookings$f1_xAYC
# f1_bookings[,'sep8'] <- ''
#
# f1_corners <- F1_fixtures_co[,c(10,11)]
# f1_corners$f1_xHCOC <- round(f1_corners$f1_xHCOC, digits = 2)
# f1_corners$f1_xACOC <- round(f1_corners$f1_xACOC, digits = 2)
# f1_corners$f1_TCOs <- f1_corners$f1_xHCOC + f1_corners$f1_xACOC
# f1_corners[,'sep9'] <- ''
#
# f1_shotsconversion <- f1_picks[,c(13,14)]
# f1_shotsconversion <- cbind(f1_shotsconversion,f1_shots)
# f1_shotsconversion$HXSC <- as.numeric(f1_shotsconversion$HXSC)
# f1_shotsconversion$AXSC <- as.numeric(f1_shotsconversion$AXSC)
# f1_shotsconversion$f1_hXgoals <- round((f1_shotsconversion$HXSC * f1_shotsconversion$f1_xHST), digits = 2)
# f1_shotsconversion$f1_aXgoals <- round((f1_shotsconversion$AXSC * f1_shotsconversion$f1_xAST), digits = 2)
# f1_shotsconversion$Xgoals <- f1_shotsconversion$f1_hXgoals + f1_shotsconversion$f1_aXgoals
#
# F1_all <- cbind(F1_fixtures_clone_final,f1_dmprediction,f1_avgyellow,f1_avgcorners,f1_goals,f1_shots,f1_fouls,f1_bookings,f1_corners,f1_shotsconversion)
# write.xlsx(F1_all,'Divisions/F1.xlsx', sheetName = "F1_all", append = TRUE)
# #########################################################################################################################################################
# F2_fixtures_clone_final <- F2_fixtures_clone[,-c(8,9,10,27)]
# F2_fixtures_clone_final[,'sep'] <- ''
#
# f2_dmprediction <-  f2_picks[,c(4,5,6,7,8)]
# f2_dmprediction[,'sep2'] <- ''
#
# f2_avgyellow <- f2_picks[,c(9,10)]
# f2_avgyellow[,'sep3'] <- ''
#
# f2_avgcorners <- f2_picks[,c(11,12)]
# f2_avgcorners[,'sep4'] <- ''
#
# f2_goals <- F2_fixtures[,c(10,11)]
# f2_goals$f2_xGH <- round(f2_goals$f2_xGH, digits = 2)
# f2_goals$f2_xGA <- round(f2_goals$f2_xGA, digits = 2)
# f2_goals$f2_TxG <- f2_goals$f2_xGH + f2_goals$f2_xGA
# f2_goals[,'sep5'] <- ''
#
# f2_shots <- F2_fixtures_sot[,c(10,11)]
# f2_shots$f2_xHST <- round(f2_shots$f2_xHST, digits = 2)
# f2_shots$f2_xAST <- round(f2_shots$f2_xAST, digits = 2)
# f2_shots$TxSOT <- f2_shots$f2_xHST + f2_shots$f2_xAST
# f2_shots[,'sep6'] <- ''
#
# f2_fouls <- F2_fixtures_fo[,c(10,11)]
# f2_fouls$f2_xHF <- round(f2_fouls$f2_xHF, digits = 2)
# f2_fouls$f2_xAF <- round(f2_fouls$f2_xAF, digits = 2)
# f2_fouls$f2_TxF <- f2_fouls$f2_xHF + f2_fouls$f2_xAF
#
# f2_ycpf <- f2_picks[,c(15,16)]
# f2_fouls <- cbind(f2_fouls,f2_ycpf)
# f2_fouls$HYCPF <- as.numeric(f2_fouls$HYCPF)
# f2_fouls$AYCPF <- as.numeric(f2_fouls$AYCPF)
# f2_fouls$x_hyc <- (f2_fouls$f2_xHF) * (f2_fouls$HYCPF)
# f2_fouls$x_ayc <- (f2_fouls$f2_xAF) * (f2_fouls$AYCPF)
# f2_fouls$x_TYC <- round((f2_fouls$x_hyc + f2_fouls$x_ayc),digits = 2)
# f2_fouls[,'sep7'] <- ''
#
# f2_bookings <- F2_fixtures_yc[,c(10,11)]
# f2_bookings$f2_xHYC <- round(f2_bookings$f2_xHYC, digits = 2)
# f2_bookings$f2_xAYC <- round(f2_bookings$f2_xAYC, digits = 2)
# f2_bookings$f2_TYcards <- f2_bookings$f2_xHYC + f2_bookings$f2_xAYC
# f2_bookings[,'sep8'] <- ''
#
# f2_corners <- F2_fixtures_co[,c(10,11)]
# f2_corners$f2_xHCOC <- round(f2_corners$f2_xHCOC, digits = 2)
# f2_corners$f2_xACOC <- round(f2_corners$f2_xACOC, digits = 2)
# f2_corners$f2_TCOs <- f2_corners$f2_xHCOC + f2_corners$f2_xACOC
# f2_corners[,'sep9'] <- ''
#
# f2_shotsconversion <- f2_picks[,c(13,14)]
# f2_shotsconversion <- cbind(f2_shotsconversion,f2_shots)
# f2_shotsconversion$HXSC <- as.numeric(f2_shotsconversion$HXSC)
# f2_shotsconversion$AXSC <- as.numeric(f2_shotsconversion$AXSC)
# f2_shotsconversion$f2_hXgoals <- round((f2_shotsconversion$HXSC * f2_shotsconversion$f2_xHST), digits = 2)
# f2_shotsconversion$f2_aXgoals <- round((f2_shotsconversion$AXSC * f2_shotsconversion$f2_xAST), digits = 2)
# f2_shotsconversion$Xgoals <- f2_shotsconversion$f2_hXgoals + f2_shotsconversion$f2_aXgoals
#
# F2_all <- cbind(F2_fixtures_clone_final,f2_dmprediction,f2_avgyellow,f2_avgcorners,f2_goals,f2_shots,f2_fouls,f2_bookings,f2_corners,f2_shotsconversion)
# write.xlsx(F2_all,'Divisions/F2.xlsx', sheetName = "F2_all", append = TRUE)
# ########################################################################################################################################################
# G1_fixtures_clone_final <- G1_fixtures_clone[,-c(8,9,10,27)]
# G1_fixtures_clone_final[,'sep'] <- ''
#
# g1_dmprediction <-  g1_picks[,c(4,5,6,7,8)]
# g1_dmprediction[,'sep2'] <- ''
#
# g1_avgyellow <- g1_picks[,c(9,10)]
# g1_avgyellow[,'sep3'] <- ''
#
# g1_avgcorners <- g1_picks[,c(11,12)]
# g1_avgcorners[,'sep4'] <- ''
#
# g1_goals <- G1_fixtures[,c(10,11)]
# g1_goals$g1_xGH <- round(g1_goals$g1_xGH, digits = 2)
# g1_goals$g1_xGA <- round(g1_goals$g1_xGA, digits = 2)
# g1_goals$g1_TxG <- g1_goals$g1_xGH + g1_goals$g1_xGA
# g1_goals[,'sep5'] <- ''
#
# g1_shots <- G1_fixtures_sot[,c(10,11)]
# g1_shots$g1_xHST <- round(g1_shots$g1_xHST, digits = 2)
# g1_shots$g1_xAST <- round(g1_shots$g1_xAST, digits = 2)
# g1_shots$TxSOT <- g1_shots$g1_xHST + g1_shots$g1_xAST
# g1_shots[,'sep6'] <- ''
#
# g1_fouls <- G1_fixtures_fo[,c(10,11)]
# g1_fouls$g1_xHF <- round(g1_fouls$g1_xHF, digits = 2)
# g1_fouls$g1_xAF <- round(g1_fouls$g1_xAF, digits = 2)
# g1_fouls$g1_TxF <- g1_fouls$g1_xHF + g1_fouls$g1_xAF
#
# g1_ycpf <- g1_picks[,c(15,16)]
# g1_fouls <- cbind(g1_fouls,g1_ycpf)
# g1_fouls$HYCPF <- as.numeric(g1_fouls$HYCPF)
# g1_fouls$AYCPF <- as.numeric(g1_fouls$AYCPF)
# g1_fouls$x_hyc <- (g1_fouls$g1_xHF) * (g1_fouls$HYCPF)
# g1_fouls$x_ayc <- (g1_fouls$g1_xAF) * (g1_fouls$AYCPF)
# g1_fouls$x_TYC <- round((g1_fouls$x_hyc + g1_fouls$x_ayc),digits = 2)
# g1_fouls[,'sep7'] <- ''
#
# g1_bookings <- G1_fixtures_yc[,c(10,11)]
# g1_bookings$g1_xHYC <- round(g1_bookings$g1_xHYC, digits = 2)
# g1_bookings$g1_xAYC <- round(g1_bookings$g1_xAYC, digits = 2)
# g1_bookings$g1_TYcards <- g1_bookings$g1_xHYC + g1_bookings$g1_xAYC
# g1_bookings[,'sep8'] <- ''
#
# g1_corners <- G1_fixtures_co[,c(10,11)]
# g1_corners$g1_xHCOC <- round(g1_corners$g1_xHCOC, digits = 2)
# g1_corners$g1_xACOC <- round(g1_corners$g1_xACOC, digits = 2)
# g1_corners$g1_TCOs <- g1_corners$g1_xHCOC + g1_corners$g1_xACOC
# g1_corners[,'sep9'] <- ''
#
# g1_shotsconversion <- g1_picks[,c(13,14)]
# g1_shotsconversion <- cbind(g1_shotsconversion,g1_shots)
# g1_shotsconversion$HXSC <- as.numeric(g1_shotsconversion$HXSC)
# g1_shotsconversion$AXSC <- as.numeric(g1_shotsconversion$AXSC)
# g1_shotsconversion$g1_hXgoals <- round((g1_shotsconversion$HXSC * g1_shotsconversion$g1_xHST), digits = 2)
# g1_shotsconversion$g1_aXgoals <- round((g1_shotsconversion$AXSC * g1_shotsconversion$g1_xAST), digits = 2)
# g1_shotsconversion$Xgoals <- g1_shotsconversion$g1_hXgoals + g1_shotsconversion$g1_aXgoals
#
# G1_all <- cbind(G1_fixtures_clone_final,g1_dmprediction,g1_avgyellow,g1_avgcorners,g1_goals,g1_shots,g1_fouls,g1_bookings,g1_corners,g1_shotsconversion)
# write.xlsx(G1_all,'Divisions/G1.xlsx', sheetName = "G1_all", append = TRUE)
# ########################################################################################################################################################
# I1_fixtures_clone_final <- I1_fixtures_clone[,-c(8,9,10,27)]
# I1_fixtures_clone_final[,'sep'] <- ''
#
# i1_dmprediction <-  i1_picks[,c(4,5,6,7,8)]
# i1_dmprediction[,'sep2'] <- ''
#
# i1_avgyellow <- i1_picks[,c(9,10)]
# i1_avgyellow[,'sep3'] <- ''
#
# i1_avgcorners <- i1_picks[,c(11,12)]
# i1_avgcorners[,'sep4'] <- ''
#
# i1_goals <- I1_fixtures[,c(10,11)]
# i1_goals$i1_xGH <- round(i1_goals$i1_xGH, digits = 2)
# i1_goals$i1_xGA <- round(i1_goals$i1_xGA, digits = 2)
# i1_goals$i1_TxG <- i1_goals$i1_xGH + i1_goals$i1_xGA
# i1_goals[,'sep5'] <- ''
#
# i1_shots <- I1_fixtures_sot[,c(10,11)]
# i1_shots$i1_xHST <- round(i1_shots$i1_xHST, digits = 2)
# i1_shots$i1_xAST <- round(i1_shots$i1_xAST, digits = 2)
# i1_shots$TxSOT <- i1_shots$i1_xHST + i1_shots$i1_xAST
# i1_shots[,'sep6'] <- ''
#
# i1_fouls <- I1_fixtures_fo[,c(10,11)]
# i1_fouls$i1_xHF <- round(i1_fouls$i1_xHF, digits = 2)
# i1_fouls$i1_xAF <- round(i1_fouls$i1_xAF, digits = 2)
# i1_fouls$i1_TxF <- i1_fouls$i1_xHF + i1_fouls$i1_xAF
#
# i1_ycpf <- i1_picks[,c(15,16)]
# i1_fouls <- cbind(i1_fouls,i1_ycpf)
# i1_fouls$HYCPF <- as.numeric(i1_fouls$HYCPF)
# i1_fouls$AYCPF <- as.numeric(i1_fouls$AYCPF)
# i1_fouls$x_hyc <- (i1_fouls$i1_xHF) * (i1_fouls$HYCPF)
# i1_fouls$x_ayc <- (i1_fouls$i1_xAF) * (i1_fouls$AYCPF)
# i1_fouls$x_TYC <- round((i1_fouls$x_hyc + i1_fouls$x_ayc),digits = 2)
# i1_fouls[,'sep7'] <- ''
#
# i1_bookings <- I1_fixtures_yc[,c(10,11)]
# i1_bookings$i1_xHYC <- round(i1_bookings$i1_xHYC, digits = 2)
# i1_bookings$i1_xAYC <- round(i1_bookings$i1_xAYC, digits = 2)
# i1_bookings$i1_TYcards <- i1_bookings$i1_xHYC + i1_bookings$i1_xAYC
# i1_bookings[,'sep8'] <- ''
#
# i1_corners <- I1_fixtures_co[,c(10,11)]
# i1_corners$i1_xHCOC <- round(i1_corners$i1_xHCOC, digits = 2)
# i1_corners$i1_xACOC <- round(i1_corners$i1_xACOC, digits = 2)
# i1_corners$i1_TCOs <- i1_corners$i1_xHCOC + i1_corners$i1_xACOC
# i1_corners[,'sep9'] <- ''
#
# i1_shotsconversion <- i1_picks[,c(13,14)]
# i1_shotsconversion <- cbind(i1_shotsconversion,i1_shots)
# i1_shotsconversion$HXSC <- as.numeric(i1_shotsconversion$HXSC)
# i1_shotsconversion$AXSC <- as.numeric(i1_shotsconversion$AXSC)
# i1_shotsconversion$i1_hXgoals <- round((i1_shotsconversion$HXSC * i1_shotsconversion$i1_xHST), digits = 2)
# i1_shotsconversion$i1_aXgoals <- round((i1_shotsconversion$AXSC * i1_shotsconversion$i1_xAST), digits = 2)
# i1_shotsconversion$Xgoals <- i1_shotsconversion$i1_hXgoals + i1_shotsconversion$i1_aXgoals
#
# I1_all <- cbind(I1_fixtures_clone_final,i1_dmprediction,i1_avgyellow,i1_avgcorners,i1_goals,i1_shots,i1_fouls,i1_bookings,i1_corners,i1_shotsconversion)
# write.xlsx(I1_all,'Divisions/I1.xlsx', sheetName = "I1_all", append = TRUE)
# #######################################################################################################################################################
# I2_fixtures_clone_final <- I2_fixtures_clone[,-c(8,9,10,27)]
# I2_fixtures_clone_final[,'sep'] <- ''
#
# i2_dmprediction <-  i2_picks[,c(4,5,6,7,8)]
# i2_dmprediction[,'sep2'] <- ''
#
# i2_avgyellow <- i2_picks[,c(9,10)]
# i2_avgyellow[,'sep3'] <- ''
#
# i2_avgcorners <- i2_picks[,c(11,12)]
# i2_avgcorners[,'sep4'] <- ''
#
# i2_goals <- I2_fixtures[,c(10,11)]
# i2_goals$i2_xGH <- round(i2_goals$i2_xGH, digits = 2)
# i2_goals$i2_xGA <- round(i2_goals$i2_xGA, digits = 2)
# i2_goals$i2_TxG <- i2_goals$i2_xGH + i2_goals$i2_xGA
# i2_goals[,'sep5'] <- ''
#
# i2_shots <- I2_fixtures_sot[,c(10,11)]
# i2_shots$i2_xHST <- round(i2_shots$i2_xHST, digits = 2)
# i2_shots$i2_xAST <- round(i2_shots$i2_xAST, digits = 2)
# i2_shots$TxSOT <- i2_shots$i2_xHST + i2_shots$i2_xAST
# i2_shots[,'sep6'] <- ''
#
# i2_fouls <- I2_fixtures_fo[,c(10,11)]
# i2_fouls$i2_xHF <- round(i2_fouls$i2_xHF, digits = 2)
# i2_fouls$i2_xAF <- round(i2_fouls$i2_xAF, digits = 2)
# i2_fouls$i2_TxF <- i2_fouls$i2_xHF + i2_fouls$i2_xAF
#
# i2_ycpf <- i2_picks[,c(15,16)]
# i2_fouls <- cbind(i2_fouls,i2_ycpf)
# i2_fouls$HYCPF <- as.numeric(i2_fouls$HYCPF)
# i2_fouls$AYCPF <- as.numeric(i2_fouls$AYCPF)
# i2_fouls$x_hyc <- (i2_fouls$i2_xHF) * (i2_fouls$HYCPF)
# i2_fouls$x_ayc <- (i2_fouls$i2_xAF) * (i2_fouls$AYCPF)
# i2_fouls$x_TYC <- round((i2_fouls$x_hyc + i2_fouls$x_ayc),digits = 2)
# i2_fouls[,'sep7'] <- ''
#
# i2_bookings <- I2_fixtures_yc[,c(10,11)]
# i2_bookings$i2_xHYC <- round(i2_bookings$i2_xHYC, digits = 2)
# i2_bookings$i2_xAYC <- round(i2_bookings$i2_xAYC, digits = 2)
# i2_bookings$i2_TYcards <- i2_bookings$i2_xHYC + i2_bookings$i2_xAYC
# i2_bookings[,'sep8'] <- ''
#
# i2_corners <- I2_fixtures_co[,c(10,11)]
# i2_corners$i2_xHCOC <- round(i2_corners$i2_xHCOC, digits = 2)
# i2_corners$i2_xACOC <- round(i2_corners$i2_xACOC, digits = 2)
# i2_corners$i2_TCOs <- i2_corners$i2_xHCOC + i2_corners$i2_xACOC
# i2_corners[,'sep9'] <- ''
#
# i2_shotsconversion <- i2_picks[,c(13,14)]
# i2_shotsconversion <- cbind(i2_shotsconversion,i2_shots)
# i2_shotsconversion$HXSC <- as.numeric(i2_shotsconversion$HXSC)
# i2_shotsconversion$AXSC <- as.numeric(i2_shotsconversion$AXSC)
# i2_shotsconversion$i2_hXgoals <- round((i2_shotsconversion$HXSC * i2_shotsconversion$i2_xHST), digits = 2)
# i2_shotsconversion$i2_aXgoals <- round((i2_shotsconversion$AXSC * i2_shotsconversion$i2_xAST), digits = 2)
# i2_shotsconversion$Xgoals <- i2_shotsconversion$i2_hXgoals + i2_shotsconversion$i2_aXgoals
#
# I2_all <- cbind(I2_fixtures_clone_final,i2_dmprediction,i2_avgyellow,i2_avgcorners,i2_goals,i2_shots,i2_fouls,i2_bookings,i2_corners,i2_shotsconversion)
# write.xlsx(I2_all,'Divisions/I2.xlsx', sheetName = "I2_all", append = TRUE)
# ########################################################################################################################################################
# N1_fixtures_clone_final <- N1_fixtures_clone[,-c(8,9,10,27)]
# N1_fixtures_clone_final[,'sep'] <- ''
#
# n1_dmprediction <-  n1_picks[,c(4,5,6,7,8)]
# n1_dmprediction[,'sep2'] <- ''
#
# n1_avgyellow <- n1_picks[,c(9,10)]
# n1_avgyellow[,'sep3'] <- ''
#
# n1_avgcorners <- n1_picks[,c(11,12)]
# n1_avgcorners[,'sep4'] <- ''
#
# n1_goals <- N1_fixtures[,c(10,11)]
# n1_goals$n1_xGH <- round(n1_goals$n1_xGH, digits = 2)
# n1_goals$n1_xGA <- round(n1_goals$n1_xGA, digits = 2)
# n1_goals$n1_TxG <- n1_goals$n1_xGH + n1_goals$n1_xGA
# n1_goals[,'sep5'] <- ''
#
# n1_shots <- N1_fixtures_sot[,c(10,11)]
# n1_shots$n1_xHST <- round(n1_shots$n1_xHST, digits = 2)
# n1_shots$n1_xAST <- round(n1_shots$n1_xAST, digits = 2)
# n1_shots$TxSOT <- n1_shots$n1_xHST + n1_shots$n1_xAST
# n1_shots[,'sep6'] <- ''
#
# n1_fouls <- N1_fixtures_fo[,c(10,11)]
# n1_fouls$n1_xHF <- round(n1_fouls$n1_xHF, digits = 2)
# n1_fouls$n1_xAF <- round(n1_fouls$n1_xAF, digits = 2)
# n1_fouls$n1_TxF <- n1_fouls$n1_xHF + n1_fouls$n1_xAF
#
# n1_ycpf <- n1_picks[,c(15,16)]
# n1_fouls <- cbind(n1_fouls,n1_ycpf)
# n1_fouls$HYCPF <- as.numeric(n1_fouls$HYCPF)
# n1_fouls$AYCPF <- as.numeric(n1_fouls$AYCPF)
# n1_fouls$x_hyc <- (n1_fouls$n1_xHF) * (n1_fouls$HYCPF)
# n1_fouls$x_ayc <- (n1_fouls$n1_xAF) * (n1_fouls$AYCPF)
# n1_fouls$x_TYC <- round((n1_fouls$x_hyc + n1_fouls$x_ayc),digits = 2)
# n1_fouls[,'sep7'] <- ''
#
# n1_bookings <- N1_fixtures_yc[,c(10,11)]
# n1_bookings$n1_xHYC <- round(n1_bookings$n1_xHYC, digits = 2)
# n1_bookings$n1_xAYC <- round(n1_bookings$n1_xAYC, digits = 2)
# n1_bookings$n1_TYcards <- n1_bookings$n1_xHYC + n1_bookings$n1_xAYC
# n1_bookings[,'sep8'] <- ''
#
# n1_corners <- N1_fixtures_co[,c(10,11)]
# n1_corners$n1_xHCOC <- round(n1_corners$n1_xHCOC, digits = 2)
# n1_corners$n1_xACOC <- round(n1_corners$n1_xACOC, digits = 2)
# n1_corners$n1_TCOs <- n1_corners$n1_xHCOC + n1_corners$n1_xACOC
# n1_corners[,'sep9'] <- ''
#
# n1_shotsconversion <- n1_picks[,c(13,14)]
# n1_shotsconversion <- cbind(n1_shotsconversion,n1_shots)
# n1_shotsconversion$HXSC <- as.numeric(n1_shotsconversion$HXSC)
# n1_shotsconversion$AXSC <- as.numeric(n1_shotsconversion$AXSC)
# n1_shotsconversion$n1_hXgoals <- round((n1_shotsconversion$HXSC * n1_shotsconversion$n1_xHST), digits = 2)
# n1_shotsconversion$n1_aXgoals <- round((n1_shotsconversion$AXSC * n1_shotsconversion$n1_xAST), digits = 2)
# n1_shotsconversion$Xgoals <- n1_shotsconversion$n1_hXgoals + n1_shotsconversion$n1_aXgoals
#
# N1_all <- cbind(N1_fixtures_clone_final,n1_dmprediction,n1_avgyellow,n1_avgcorners,n1_goals,n1_shots,n1_fouls,n1_bookings,n1_corners,n1_shotsconversion)
# write.xlsx(N1_all,'Divisions/N1.xlsx', sheetName = "N1_all", append = TRUE)
# ###################################################################################################################################################
# P1_fixtures_clone_final <- P1_fixtures_clone[,-c(8,9,10,27)]
# P1_fixtures_clone_final[,'sep'] <- ''
#
# p1_dmprediction <-  p1_picks[,c(4,5,6,7,8)]
# p1_dmprediction[,'sep2'] <- ''
#
# p1_avgyellow <- p1_picks[,c(9,10)]
# p1_avgyellow[,'sep3'] <- ''
#
# p1_avgcorners <- p1_picks[,c(11,12)]
# p1_avgcorners[,'sep4'] <- ''
#
# p1_goals <- P1_fixtures[,c(10,11)]
# p1_goals$p1_xGH <- round(p1_goals$p1_xGH, digits = 2)
# p1_goals$p1_xGA <- round(p1_goals$p1_xGA, digits = 2)
# p1_goals$p1_TxG <- p1_goals$p1_xGH + p1_goals$p1_xGA
# p1_goals[,'sep5'] <- ''
#
# p1_shots <- P1_fixtures_sot[,c(10,11)]
# p1_shots$p1_xHST <- round(p1_shots$p1_xHST, digits = 2)
# p1_shots$p1_xAST <- round(p1_shots$p1_xAST, digits = 2)
# p1_shots$TxSOT <- p1_shots$p1_xHST + p1_shots$p1_xAST
# p1_shots[,'sep6'] <- ''
#
# p1_fouls <- P1_fixtures_fo[,c(10,11)]
# p1_fouls$p1_xHF <- round(p1_fouls$p1_xHF, digits = 2)
# p1_fouls$p1_xAF <- round(p1_fouls$p1_xAF, digits = 2)
# p1_fouls$p1_TxF <- p1_fouls$p1_xHF + p1_fouls$p1_xAF
#
# p1_ycpf <- p1_picks[,c(15,16)]
# p1_fouls <- cbind(p1_fouls,p1_ycpf)
# p1_fouls$HYCPF <- as.numeric(p1_fouls$HYCPF)
# p1_fouls$AYCPF <- as.numeric(p1_fouls$AYCPF)
# p1_fouls$x_hyc <- (p1_fouls$p1_xHF) * (p1_fouls$HYCPF)
# p1_fouls$x_ayc <- (p1_fouls$p1_xAF) * (p1_fouls$AYCPF)
# p1_fouls$x_TYC <- round((p1_fouls$x_hyc + p1_fouls$x_ayc),digits = 2)
# p1_fouls[,'sep7'] <- ''
#
# p1_bookings <- P1_fixtures_yc[,c(10,11)]
# p1_bookings$p1_xHYC <- round(p1_bookings$p1_xHYC, digits = 2)
# p1_bookings$p1_xAYC <- round(p1_bookings$p1_xAYC, digits = 2)
# p1_bookings$p1_TYcards <- p1_bookings$p1_xHYC + p1_bookings$p1_xAYC
# p1_bookings[,'sep8'] <- ''
#
# p1_corners <- P1_fixtures_co[,c(10,11)]
# p1_corners$p1_xHCOC <- round(p1_corners$p1_xHCOC, digits = 2)
# p1_corners$p1_xACOC <- round(p1_corners$p1_xACOC, digits = 2)
# p1_corners$p1_TCOs <- p1_corners$p1_xHCOC + p1_corners$p1_xACOC
# p1_corners[,'sep9'] <- ''
#
# p1_shotsconversion <- p1_picks[,c(13,14)]
# p1_shotsconversion <- cbind(p1_shotsconversion,p1_shots)
# p1_shotsconversion$HXSC <- as.numeric(p1_shotsconversion$HXSC)
# p1_shotsconversion$AXSC <- as.numeric(p1_shotsconversion$AXSC)
# p1_shotsconversion$p1_hXgoals <- round((p1_shotsconversion$HXSC * p1_shotsconversion$p1_xHST), digits = 2)
# p1_shotsconversion$p1_aXgoals <- round((p1_shotsconversion$AXSC * p1_shotsconversion$p1_xAST), digits = 2)
# p1_shotsconversion$Xgoals <- p1_shotsconversion$p1_hXgoals + p1_shotsconversion$p1_aXgoals
#
# P1_all <- cbind(P1_fixtures_clone_final,p1_dmprediction,p1_avgyellow,p1_avgcorners,p1_goals,p1_shots,p1_fouls,p1_bookings,p1_corners,p1_shotsconversion)
# write.xlsx(P1_all,'Divisions/P1.xlsx', sheetName = "P1_all", append = TRUE)
# #########################################################################################################################################################
# SC0_fixtures_clone_final <- SC0_fixtures_clone[,-c(8,9,10,27)]
# SC0_fixtures_clone_final[,'sep'] <- ''
#
# sc0_dmprediction <-  sc0_picks[,c(4,5,6,7,8)]
# sc0_dmprediction[,'sep2'] <- ''
#
# sc0_avgyellow <- sc0_picks[,c(9,10)]
# sc0_avgyellow[,'sep3'] <- ''
#
# sc0_avgcorners <- sc0_picks[,c(11,12)]
# sc0_avgcorners[,'sep4'] <- ''
#
# sc0_goals <- SC0_fixtures[,c(10,11)]
# sc0_goals$sc0_xGH <- round(sc0_goals$sc0_xGH, digits = 2)
# sc0_goals$sc0_xGA <- round(sc0_goals$sc0_xGA, digits = 2)
# sc0_goals$sc0_TxG <- sc0_goals$sc0_xGH + sc0_goals$sc0_xGA
# sc0_goals[,'sep5'] <- ''
#
# sc0_shots <- SC0_fixtures_sot[,c(10,11)]
# sc0_shots$sc0_xHST <- round(sc0_shots$sc0_xHST, digits = 2)
# sc0_shots$sc0_xAST <- round(sc0_shots$sc0_xAST, digits = 2)
# sc0_shots$TxSOT <- sc0_shots$sc0_xHST + sc0_shots$sc0_xAST
# sc0_shots[,'sep6'] <- ''
#
# sc0_fouls <- SC0_fixtures_fo[,c(10,11)]
# sc0_fouls$sc0_xHF <- round(sc0_fouls$sc0_xHF, digits = 2)
# sc0_fouls$sc0_xAF <- round(sc0_fouls$sc0_xAF, digits = 2)
# sc0_fouls$sc0_TxF <- sc0_fouls$sc0_xHF + sc0_fouls$sc0_xAF
#
# sc0_ycpf <- sc0_picks[,c(15,16)]
# sc0_fouls <- cbind(sc0_fouls,sc0_ycpf)
# sc0_fouls$HYCPF <- as.numeric(sc0_fouls$HYCPF)
# sc0_fouls$AYCPF <- as.numeric(sc0_fouls$AYCPF)
# sc0_fouls$x_hyc <- (sc0_fouls$sc0_xHF) * (sc0_fouls$HYCPF)
# sc0_fouls$x_ayc <- (sc0_fouls$sc0_xAF) * (sc0_fouls$AYCPF)
# sc0_fouls$x_TYC <- round((sc0_fouls$x_hyc + sc0_fouls$x_ayc),digits = 2)
# sc0_fouls[,'sep7'] <- ''
#
# sc0_bookings <- SC0_fixtures_yc[,c(10,11)]
# sc0_bookings$sc0_xHYC <- round(sc0_bookings$sc0_xHYC, digits = 2)
# sc0_bookings$sc0_xAYC <- round(sc0_bookings$sc0_xAYC, digits = 2)
# sc0_bookings$sc0_TYcards <- sc0_bookings$sc0_xHYC + sc0_bookings$sc0_xAYC
# sc0_bookings[,'sep8'] <- ''
#
# sc0_corners <- SC0_fixtures_co[,c(10,11)]
# sc0_corners$sc0_xHCOC <- round(sc0_corners$sc0_xHCOC, digits = 2)
# sc0_corners$sc0_xACOC <- round(sc0_corners$sc0_xACOC, digits = 2)
# sc0_corners$sc0_TCOs <- sc0_corners$sc0_xHCOC + sc0_corners$sc0_xACOC
# sc0_corners[,'sep9'] <- ''
#
# sc0_shotsconversion <- sc0_picks[,c(13,14)]
# sc0_shotsconversion <- cbind(sc0_shotsconversion,sc0_shots)
# sc0_shotsconversion$HXSC <- as.numeric(sc0_shotsconversion$HXSC)
# sc0_shotsconversion$AXSC <- as.numeric(sc0_shotsconversion$AXSC)
# sc0_shotsconversion$sc0_hXgoals <- round((sc0_shotsconversion$HXSC * sc0_shotsconversion$sc0_xHST), digits = 2)
# sc0_shotsconversion$sc0_aXgoals <- round((sc0_shotsconversion$AXSC * sc0_shotsconversion$sc0_xAST), digits = 2)
# sc0_shotsconversion$Xgoals <- sc0_shotsconversion$sc0_hXgoals + sc0_shotsconversion$sc0_aXgoals
#
# SC0_all <- cbind(SC0_fixtures_clone_final,sc0_dmprediction,sc0_avgyellow,sc0_avgcorners,sc0_goals,sc0_shots,sc0_fouls,sc0_bookings,sc0_corners,sc0_shotsconversion)
# write.xlsx(SC0_all,'Divisions/SC0.xlsx', sheetName = "SC0_all", append = TRUE)
# ########################################################################################################################################################
# SC1_fixtures_clone_final <- SC1_fixtures_clone[,-c(8,9,10,27)]
# SC1_fixtures_clone_final[,'sep'] <- ''
#
# sc1_dmprediction <-  sc1_picks[,c(4,5,6,7,8)]
# sc1_dmprediction[,'sep2'] <- ''
#
# sc1_avgyellow <- sc1_picks[,c(9,10)]
# sc1_avgyellow[,'sep3'] <- ''
#
# sc1_avgcorners <- sc1_picks[,c(11,12)]
# sc1_avgcorners[,'sep4'] <- ''
#
# sc1_goals <- SC1_fixtures[,c(10,11)]
# sc1_goals$sc1_xGH <- round(sc1_goals$sc1_xGH, digits = 2)
# sc1_goals$sc1_xGA <- round(sc1_goals$sc1_xGA, digits = 2)
# sc1_goals$sc1_TxG <- sc1_goals$sc1_xGH + sc1_goals$sc1_xGA
# sc1_goals[,'sep5'] <- ''
#
# sc1_shots <- SC1_fixtures_sot[,c(10,11)]
# sc1_shots$sc1_xHST <- round(sc1_shots$sc1_xHST, digits = 2)
# sc1_shots$sc1_xAST <- round(sc1_shots$sc1_xAST, digits = 2)
# sc1_shots$TxSOT <- sc1_shots$sc1_xHST + sc1_shots$sc1_xAST
# sc1_shots[,'sep6'] <- ''
#
# sc1_fouls <- SC1_fixtures_fo[,c(10,11)]
# sc1_fouls$sc1_xHF <- round(sc1_fouls$sc1_xHF, digits = 2)
# sc1_fouls$sc1_xAF <- round(sc1_fouls$sc1_xAF, digits = 2)
# sc1_fouls$sc1_TxF <- sc1_fouls$sc1_xHF + sc1_fouls$sc1_xAF
#
# sc1_ycpf <- sc1_picks[,c(15,16)]
# sc1_fouls <- cbind(sc1_fouls,sc1_ycpf)
# sc1_fouls$HYCPF <- as.numeric(sc1_fouls$HYCPF)
# sc1_fouls$AYCPF <- as.numeric(sc1_fouls$AYCPF)
# sc1_fouls$x_hyc <- (sc1_fouls$sc1_xHF) * (sc1_fouls$HYCPF)
# sc1_fouls$x_ayc <- (sc1_fouls$sc1_xAF) * (sc1_fouls$AYCPF)
# sc1_fouls$x_TYC <- round((sc1_fouls$x_hyc + sc1_fouls$x_ayc),digits = 2)
# sc1_fouls[,'sep7'] <- ''
#
# sc1_bookings <- SC1_fixtures_yc[,c(10,11)]
# sc1_bookings$sc1_xHYC <- round(sc1_bookings$sc1_xHYC, digits = 2)
# sc1_bookings$sc1_xAYC <- round(sc1_bookings$sc1_xAYC, digits = 2)
# sc1_bookings$sc1_TYcards <- sc1_bookings$sc1_xHYC + sc1_bookings$sc1_xAYC
# sc1_bookings[,'sep8'] <- ''
#
# sc1_corners <- SC1_fixtures_co[,c(10,11)]
# sc1_corners$sc1_xHCOC <- round(sc1_corners$sc1_xHCOC, digits = 2)
# sc1_corners$sc1_xACOC <- round(sc1_corners$sc1_xACOC, digits = 2)
# sc1_corners$sc1_TCOs <- sc1_corners$sc1_xHCOC + sc1_corners$sc1_xACOC
# sc1_corners[,'sep9'] <- ''
#
# sc1_shotsconversion <- sc1_picks[,c(13,14)]
# sc1_shotsconversion <- cbind(sc1_shotsconversion,sc1_shots)
# sc1_shotsconversion$HXSC <- as.numeric(sc1_shotsconversion$HXSC)
# sc1_shotsconversion$AXSC <- as.numeric(sc1_shotsconversion$AXSC)
# sc1_shotsconversion$sc1_hXgoals <- round((sc1_shotsconversion$HXSC * sc1_shotsconversion$sc1_xHST), digits = 2)
# sc1_shotsconversion$sc1_aXgoals <- round((sc1_shotsconversion$AXSC * sc1_shotsconversion$sc1_xAST), digits = 2)
# sc1_shotsconversion$Xgoals <- sc1_shotsconversion$sc1_hXgoals + sc1_shotsconversion$sc1_aXgoals
#
# SC1_all <- cbind(SC1_fixtures_clone_final,sc1_dmprediction,sc1_avgyellow,sc1_avgcorners,sc1_goals,sc1_shots,sc1_fouls,sc1_bookings,sc1_corners,sc1_shotsconversion)
# write.xlsx(SC1_all,'Divisions/SC1.xlsx', sheetName = "SC1_all", append = TRUE)
# ########################################################################################################################################################
# SC2_fixtures_clone_final <- SC2_fixtures_clone[,-c(8,9,10,27)]
# SC2_fixtures_clone_final[,'sep'] <- ''
#
# sc2_dmprediction <-  sc2_picks[,c(4,5,6,7,8)]
# sc2_dmprediction[,'sep2'] <- ''
#
# sc2_avgyellow <- sc2_picks[,c(9,10)]
# sc2_avgyellow[,'sep3'] <- ''
#
# sc2_avgcorners <- sc2_picks[,c(11,12)]
# sc2_avgcorners[,'sep4'] <- ''
#
# sc2_goals <- SC2_fixtures[,c(10,11)]
# sc2_goals$sc2_xGH <- round(sc2_goals$sc2_xGH, digits = 2)
# sc2_goals$sc2_xGA <- round(sc2_goals$sc2_xGA, digits = 2)
# sc2_goals$sc2_TxG <- sc2_goals$sc2_xGH + sc2_goals$sc2_xGA
# sc2_goals[,'sep5'] <- ''
#
# sc2_shots <- SC2_fixtures_sot[,c(10,11)]
# sc2_shots$sc2_xHST <- round(sc2_shots$sc2_xHST, digits = 2)
# sc2_shots$sc2_xAST <- round(sc2_shots$sc2_xAST, digits = 2)
# sc2_shots$TxSOT <- sc2_shots$sc2_xHST + sc2_shots$sc2_xAST
# sc2_shots[,'sep6'] <- ''
#
# sc2_fouls <- SC2_fixtures_fo[,c(10,11)]
# sc2_fouls$sc2_xHF <- round(sc2_fouls$sc2_xHF, digits = 2)
# sc2_fouls$sc2_xAF <- round(sc2_fouls$sc2_xAF, digits = 2)
# sc2_fouls$sc2_TxF <- sc2_fouls$sc2_xHF + sc2_fouls$sc2_xAF
#
# sc2_ycpf <- sc2_picks[,c(15,16)]
# sc2_fouls <- cbind(sc2_fouls,sc2_ycpf)
# sc2_fouls$HYCPF <- as.numeric(sc2_fouls$HYCPF)
# sc2_fouls$AYCPF <- as.numeric(sc2_fouls$AYCPF)
# sc2_fouls$x_hyc <- (sc2_fouls$sc2_xHF) * (sc2_fouls$HYCPF)
# sc2_fouls$x_ayc <- (sc2_fouls$sc2_xAF) * (sc2_fouls$AYCPF)
# sc2_fouls$x_TYC <- round((sc2_fouls$x_hyc + sc2_fouls$x_ayc),digits = 2)
# sc2_fouls[,'sep7'] <- ''
#
# sc2_bookings <- SC2_fixtures_yc[,c(10,11)]
# sc2_bookings$sc2_xHYC <- round(sc2_bookings$sc2_xHYC, digits = 2)
# sc2_bookings$sc2_xAYC <- round(sc2_bookings$sc2_xAYC, digits = 2)
# sc2_bookings$sc2_TYcards <- sc2_bookings$sc2_xHYC + sc2_bookings$sc2_xAYC
# sc2_bookings[,'sep8'] <- ''
#
# sc2_corners <- SC2_fixtures_co[,c(10,11)]
# sc2_corners$sc2_xHCOC <- round(sc2_corners$sc2_xHCOC, digits = 2)
# sc2_corners$sc2_xACOC <- round(sc2_corners$sc2_xACOC, digits = 2)
# sc2_corners$sc2_TCOs <- sc2_corners$sc2_xHCOC + sc2_corners$sc2_xACOC
# sc2_corners[,'sep9'] <- ''
#
# sc2_shotsconversion <- sc2_picks[,c(13,14)]
# sc2_shotsconversion <- cbind(sc2_shotsconversion,sc2_shots)
# sc2_shotsconversion$HXSC <- as.numeric(sc2_shotsconversion$HXSC)
# sc2_shotsconversion$AXSC <- as.numeric(sc2_shotsconversion$AXSC)
# sc2_shotsconversion$sc2_hXgoals <- round((sc2_shotsconversion$HXSC * sc2_shotsconversion$sc2_xHST), digits = 2)
# sc2_shotsconversion$sc2_aXgoals <- round((sc2_shotsconversion$AXSC * sc2_shotsconversion$sc2_xAST), digits = 2)
# sc2_shotsconversion$Xgoals <- sc2_shotsconversion$sc2_hXgoals + sc2_shotsconversion$sc2_aXgoals
#
# SC2_all <- cbind(SC2_fixtures_clone_final,sc2_dmprediction,sc2_avgyellow,sc2_avgcorners,sc2_goals,sc2_shots,sc2_fouls,sc2_bookings,sc2_corners,sc2_shotsconversion)
# write.xlsx(SC2_all,'Divisions/SC2.xlsx', sheetName = "SC2_all", append = TRUE)
# ####################################################################################################################################################
# SC3_fixtures_clone_final <- SC3_fixtures_clone[,-c(8,9,10,27)]
# SC3_fixtures_clone_final[,'sep'] <- ''
#
# sc3_dmprediction <-  sc3_picks[,c(4,5,6,7,8)]
# sc3_dmprediction[,'sep2'] <- ''
#
# sc3_avgyellow <- sc3_picks[,c(9,10)]
# sc3_avgyellow[,'sep3'] <- ''
#
# sc3_avgcorners <- sc3_picks[,c(11,12)]
# sc3_avgcorners[,'sep4'] <- ''
#
# sc3_goals <- SC3_fixtures[,c(10,11)]
# sc3_goals$sc3_xGH <- round(sc3_goals$sc3_xGH, digits = 2)
# sc3_goals$sc3_xGA <- round(sc3_goals$sc3_xGA, digits = 2)
# sc3_goals$sc3_TxG <- sc3_goals$sc3_xGH + sc3_goals$sc3_xGA
# sc3_goals[,'sep5'] <- ''
#
# sc3_shots <- SC3_fixtures_sot[,c(10,11)]
# sc3_shots$sc3_xHST <- round(sc3_shots$sc3_xHST, digits = 2)
# sc3_shots$sc3_xAST <- round(sc3_shots$sc3_xAST, digits = 2)
# sc3_shots$TxSOT <- sc3_shots$sc3_xHST + sc3_shots$sc3_xAST
# sc3_shots[,'sep6'] <- ''
#
# sc3_fouls <- SC3_fixtures_fo[,c(10,11)]
# sc3_fouls$sc3_xHF <- round(sc3_fouls$sc3_xHF, digits = 2)
# sc3_fouls$sc3_xAF <- round(sc3_fouls$sc3_xAF, digits = 2)
# sc3_fouls$sc3_TxF <- sc3_fouls$sc3_xHF + sc3_fouls$sc3_xAF
#
# sc3_ycpf <- sc3_picks[,c(15,16)]
# sc3_fouls <- cbind(sc3_fouls,sc3_ycpf)
# sc3_fouls$HYCPF <- as.numeric(sc3_fouls$HYCPF)
# sc3_fouls$AYCPF <- as.numeric(sc3_fouls$AYCPF)
# sc3_fouls$x_hyc <- (sc3_fouls$sc3_xHF) * (sc3_fouls$HYCPF)
# sc3_fouls$x_ayc <- (sc3_fouls$sc3_xAF) * (sc3_fouls$AYCPF)
# sc3_fouls$x_TYC <- round((sc3_fouls$x_hyc + sc3_fouls$x_ayc),digits = 2)
# sc3_fouls[,'sep7'] <- ''
#
# sc3_bookings <- SC3_fixtures_yc[,c(10,11)]
# sc3_bookings$sc3_xHYC <- round(sc3_bookings$sc3_xHYC, digits = 2)
# sc3_bookings$sc3_xAYC <- round(sc3_bookings$sc3_xAYC, digits = 2)
# sc3_bookings$sc3_TYcards <- sc3_bookings$sc3_xHYC + sc3_bookings$sc3_xAYC
# sc3_bookings[,'sep8'] <- ''
#
# sc3_corners <- SC3_fixtures_co[,c(10,11)]
# sc3_corners$sc3_xHCOC <- round(sc3_corners$sc3_xHCOC, digits = 2)
# sc3_corners$sc3_xACOC <- round(sc3_corners$sc3_xACOC, digits = 2)
# sc3_corners$sc3_TCOs <- sc3_corners$sc3_xHCOC + sc3_corners$sc3_xACOC
# sc3_corners[,'sep9'] <- ''
#
# sc3_shotsconversion <- sc3_picks[,c(13,14)]
# sc3_shotsconversion <- cbind(sc3_shotsconversion,sc3_shots)
# sc3_shotsconversion$HXSC <- as.numeric(sc3_shotsconversion$HXSC)
# sc3_shotsconversion$AXSC <- as.numeric(sc3_shotsconversion$AXSC)
# sc3_shotsconversion$sc3_hXgoals <- round((sc3_shotsconversion$HXSC * sc3_shotsconversion$sc3_xHST), digits = 2)
# sc3_shotsconversion$sc3_aXgoals <- round((sc3_shotsconversion$AXSC * sc3_shotsconversion$sc3_xAST), digits = 2)
# sc3_shotsconversion$Xgoals <- sc3_shotsconversion$sc3_hXgoals + sc3_shotsconversion$sc3_aXgoals
#
# SC3_all <- cbind(SC3_fixtures_clone_final,sc3_dmprediction,sc3_avgyellow,sc3_avgcorners,sc3_goals,sc3_shots,sc3_fouls,sc3_bookings,sc3_corners,sc3_shotsconversion)
# write.xlsx(SC3_all,'Divisions/SC3.xlsx', sheetName = "SC3_all", append = TRUE)
# ######################################################################################################################################################
# SP1_fixtures_clone_final <- SP1_fixtures_clone[,-c(8,9,10,27)]
# SP1_fixtures_clone_final[,'sep'] <- ''
#
# sp1_dmprediction <-  sp1_picks[,c(4,5,6,7,8)]
# sp1_dmprediction[,'sep2'] <- ''
#
# sp1_avgyellow <- sp1_picks[,c(9,10)]
# sp1_avgyellow[,'sep3'] <- ''
#
# sp1_avgcorners <- sp1_picks[,c(11,12)]
# sp1_avgcorners[,'sep4'] <- ''
#
# sp1_goals <- SP1_fixtures[,c(10,11)]
# sp1_goals$sp1_xGH <- round(sp1_goals$sp1_xGH, digits = 2)
# sp1_goals$sp1_xGA <- round(sp1_goals$sp1_xGA, digits = 2)
# sp1_goals$sp1_TxG <- sp1_goals$sp1_xGH + sp1_goals$sp1_xGA
# sp1_goals[,'sep5'] <- ''
#
# sp1_shots <- SP1_fixtures_sot[,c(10,11)]
# sp1_shots$sp1_xHST <- round(sp1_shots$sp1_xHST, digits = 2)
# sp1_shots$sp1_xAST <- round(sp1_shots$sp1_xAST, digits = 2)
# sp1_shots$TxSOT <- sp1_shots$sp1_xHST + sp1_shots$sp1_xAST
# sp1_shots[,'sep6'] <- ''
#
# sp1_fouls <- SP1_fixtures_fo[,c(10,11)]
# sp1_fouls$sp1_xHF <- round(sp1_fouls$sp1_xHF, digits = 2)
# sp1_fouls$sp1_xAF <- round(sp1_fouls$sp1_xAF, digits = 2)
# sp1_fouls$sp1_TxF <- sp1_fouls$sp1_xHF + sp1_fouls$sp1_xAF
#
# sp1_ycpf <- sp1_picks[,c(15,16)]
# sp1_fouls <- cbind(sp1_fouls,sp1_ycpf)
# sp1_fouls$HYCPF <- as.numeric(sp1_fouls$HYCPF)
# sp1_fouls$AYCPF <- as.numeric(sp1_fouls$AYCPF)
# sp1_fouls$x_hyc <- (sp1_fouls$sp1_xHF) * (sp1_fouls$HYCPF)
# sp1_fouls$x_ayc <- (sp1_fouls$sp1_xAF) * (sp1_fouls$AYCPF)
# sp1_fouls$x_TYC <- round((sp1_fouls$x_hyc + sp1_fouls$x_ayc),digits = 2)
# sp1_fouls[,'sep7'] <- ''
#
# sp1_bookings <- SP1_fixtures_yc[,c(10,11)]
# sp1_bookings$sp1_xHYC <- round(sp1_bookings$sp1_xHYC, digits = 2)
# sp1_bookings$sp1_xAYC <- round(sp1_bookings$sp1_xAYC, digits = 2)
# sp1_bookings$sp1_TYcards <- sp1_bookings$sp1_xHYC + sp1_bookings$sp1_xAYC
# sp1_bookings[,'sep8'] <- ''
#
# sp1_corners <- SP1_fixtures_co[,c(10,11)]
# sp1_corners$sp1_xHCOC <- round(sp1_corners$sp1_xHCOC, digits = 2)
# sp1_corners$sp1_xACOC <- round(sp1_corners$sp1_xACOC, digits = 2)
# sp1_corners$sp1_TCOs <- sp1_corners$sp1_xHCOC + sp1_corners$sp1_xACOC
# sp1_corners[,'sep9'] <- ''
#
# sp1_shotsconversion <- sp1_picks[,c(13,14)]
# sp1_shotsconversion <- cbind(sp1_shotsconversion,sp1_shots)
# sp1_shotsconversion$HXSC <- as.numeric(sp1_shotsconversion$HXSC)
# sp1_shotsconversion$AXSC <- as.numeric(sp1_shotsconversion$AXSC)
# sp1_shotsconversion$sp1_hXgoals <- round((sp1_shotsconversion$HXSC * sp1_shotsconversion$sp1_xHST), digits = 2)
# sp1_shotsconversion$sp1_aXgoals <- round((sp1_shotsconversion$AXSC * sp1_shotsconversion$sp1_xAST), digits = 2)
# sp1_shotsconversion$Xgoals <- sp1_shotsconversion$sp1_hXgoals + sp1_shotsconversion$sp1_aXgoals
#
# SP1_all <- cbind(SP1_fixtures_clone_final,sp1_dmprediction,sp1_avgyellow,sp1_avgcorners,sp1_goals,sp1_shots,sp1_fouls,sp1_bookings,sp1_corners,sp1_shotsconversion)
# write.xlsx(SP1_all,'Divisions/SP1.xlsx', sheetName = "SP1_all", append = TRUE)
# ###############################################################################################################################################
# SP2_fixtures_clone_final <- SP2_fixtures_clone[,-c(8,9,10,27)]
# SP2_fixtures_clone_final[,'sep'] <- ''
#
# sp2_dmprediction <-  sp2_picks[,c(4,5,6,7,8)]
# sp2_dmprediction[,'sep2'] <- ''
#
# sp2_avgyellow <- sp2_picks[,c(9,10)]
# sp2_avgyellow[,'sep3'] <- ''
#
# sp2_avgcorners <- sp2_picks[,c(11,12)]
# sp2_avgcorners[,'sep4'] <- ''
#
# sp2_goals <- SP2_fixtures[,c(10,11)]
# sp2_goals$sp2_xGH <- round(sp2_goals$sp2_xGH, digits = 2)
# sp2_goals$sp2_xGA <- round(sp2_goals$sp2_xGA, digits = 2)
# sp2_goals$sp2_TxG <- sp2_goals$sp2_xGH + sp2_goals$sp2_xGA
# sp2_goals[,'sep5'] <- ''
#
# sp2_shots <- SP2_fixtures_sot[,c(10,11)]
# sp2_shots$sp2_xHST <- round(sp2_shots$sp2_xHST, digits = 2)
# sp2_shots$sp2_xAST <- round(sp2_shots$sp2_xAST, digits = 2)
# sp2_shots$TxSOT <- sp2_shots$sp2_xHST + sp2_shots$sp2_xAST
# sp2_shots[,'sep6'] <- ''
#
# sp2_fouls <- SP2_fixtures_fo[,c(10,11)]
# sp2_fouls$sp2_xHF <- round(sp2_fouls$sp2_xHF, digits = 2)
# sp2_fouls$sp2_xAF <- round(sp2_fouls$sp2_xAF, digits = 2)
# sp2_fouls$sp2_TxF <- sp2_fouls$sp2_xHF + sp2_fouls$sp2_xAF
#
# sp2_ycpf <- sp2_picks[,c(15,16)]
# sp2_fouls <- cbind(sp2_fouls,sp2_ycpf)
# sp2_fouls$HYCPF <- as.numeric(sp2_fouls$HYCPF)
# sp2_fouls$AYCPF <- as.numeric(sp2_fouls$AYCPF)
# sp2_fouls$x_hyc <- (sp2_fouls$sp2_xHF) * (sp2_fouls$HYCPF)
# sp2_fouls$x_ayc <- (sp2_fouls$sp2_xAF) * (sp2_fouls$AYCPF)
# sp2_fouls$x_TYC <- round((sp2_fouls$x_hyc + sp2_fouls$x_ayc),digits = 2)
# sp2_fouls[,'sep7'] <- ''
#
# sp2_bookings <- SP2_fixtures_yc[,c(10,11)]
# sp2_bookings$sp2_xHYC <- round(sp2_bookings$sp2_xHYC, digits = 2)
# sp2_bookings$sp2_xAYC <- round(sp2_bookings$sp2_xAYC, digits = 2)
# sp2_bookings$sp2_TYcards <- sp2_bookings$sp2_xHYC + sp2_bookings$sp2_xAYC
# sp2_bookings[,'sep8'] <- ''
#
# sp2_corners <- SP2_fixtures_co[,c(10,11)]
# sp2_corners$sp2_xHCOC <- round(sp2_corners$sp2_xHCOC, digits = 2)
# sp2_corners$sp2_xACOC <- round(sp2_corners$sp2_xACOC, digits = 2)
# sp2_corners$sp2_TCOs <- sp2_corners$sp2_xHCOC + sp2_corners$sp2_xACOC
# sp2_corners[,'sep9'] <- ''
#
# sp2_shotsconversion <- sp2_picks[,c(13,14)]
# sp2_shotsconversion <- cbind(sp2_shotsconversion,sp2_shots)
# sp2_shotsconversion$HXSC <- as.numeric(sp2_shotsconversion$HXSC)
# sp2_shotsconversion$AXSC <- as.numeric(sp2_shotsconversion$AXSC)
# sp2_shotsconversion$sp2_hXgoals <- round((sp2_shotsconversion$HXSC * sp2_shotsconversion$sp2_xHST), digits = 2)
# sp2_shotsconversion$sp2_aXgoals <- round((sp2_shotsconversion$AXSC * sp2_shotsconversion$sp2_xAST), digits = 2)
# sp2_shotsconversion$Xgoals <- sp2_shotsconversion$sp2_hXgoals + sp2_shotsconversion$sp2_aXgoals
#
# SP2_all <- cbind(SP2_fixtures_clone_final,sp2_dmprediction,sp2_avgyellow,sp2_avgcorners,sp2_goals,sp2_shots,sp2_fouls,sp2_bookings,sp2_corners,sp2_shotsconversion)
# write.xlsx(SP2_all,'Divisions/SP2.xlsx', sheetName = "SP2_all", append = TRUE)
# ####################################################################################################################################################
# T1_fixtures_clone_final <- T1_fixtures_clone[,-c(8,9,10,27)]
# T1_fixtures_clone_final[,'sep'] <- ''
#
# t1_dmprediction <-  t1_picks[,c(4,5,6,7,8)]
# t1_dmprediction[,'sep2'] <- ''
#
# t1_avgyellow <- t1_picks[,c(9,10)]
# t1_avgyellow[,'sep3'] <- ''
#
# t1_avgcorners <- t1_picks[,c(11,12)]
# t1_avgcorners[,'sep4'] <- ''
#
# t1_goals <- T1_fixtures[,c(10,11)]
# t1_goals$t1_xGH <- round(t1_goals$t1_xGH, digits = 2)
# t1_goals$t1_xGA <- round(t1_goals$t1_xGA, digits = 2)
# t1_goals$t1_TxG <- t1_goals$t1_xGH + t1_goals$t1_xGA
# t1_goals[,'sep5'] <- ''
#
# t1_shots <- T1_fixtures_sot[,c(10,11)]
# t1_shots$t1_xHST <- round(t1_shots$t1_xHST, digits = 2)
# t1_shots$t1_xAST <- round(t1_shots$t1_xAST, digits = 2)
# t1_shots$TxSOT <- t1_shots$t1_xHST + t1_shots$t1_xAST
# t1_shots[,'sep6'] <- ''
#
# t1_fouls <- T1_fixtures_fo[,c(10,11)]
# t1_fouls$t1_xHF <- round(t1_fouls$t1_xHF, digits = 2)
# t1_fouls$t1_xAF <- round(t1_fouls$t1_xAF, digits = 2)
# t1_fouls$t1_TxF <- t1_fouls$t1_xHF + t1_fouls$t1_xAF
#
# t1_ycpf <- t1_picks[,c(15,16)]
# t1_fouls <- cbind(t1_fouls,t1_ycpf)
# t1_fouls$HYCPF <- as.numeric(t1_fouls$HYCPF)
# t1_fouls$AYCPF <- as.numeric(t1_fouls$AYCPF)
# t1_fouls$x_hyc <- (t1_fouls$t1_xHF) * (t1_fouls$HYCPF)
# t1_fouls$x_ayc <- (t1_fouls$t1_xAF) * (t1_fouls$AYCPF)
# t1_fouls$x_TYC <- round((t1_fouls$x_hyc + t1_fouls$x_ayc),digits = 2)
# t1_fouls[,'sep7'] <- ''
#
# t1_bookings <- T1_fixtures_yc[,c(10,11)]
# t1_bookings$t1_xHYC <- round(t1_bookings$t1_xHYC, digits = 2)
# t1_bookings$t1_xAYC <- round(t1_bookings$t1_xAYC, digits = 2)
# t1_bookings$t1_TYcards <- t1_bookings$t1_xHYC + t1_bookings$t1_xAYC
# t1_bookings[,'sep8'] <- ''
#
# t1_corners <- T1_fixtures_co[,c(10,11)]
# t1_corners$t1_xHCOC <- round(t1_corners$t1_xHCOC, digits = 2)
# t1_corners$t1_xACOC <- round(t1_corners$t1_xACOC, digits = 2)
# t1_corners$t1_TCOs <- t1_corners$t1_xHCOC + t1_corners$t1_xACOC
# t1_corners[,'sep9'] <- ''
#
# t1_shotsconversion <- t1_picks[,c(13,14)]
# t1_shotsconversion <- cbind(t1_shotsconversion,t1_shots)
# t1_shotsconversion$HXSC <- as.numeric(t1_shotsconversion$HXSC)
# t1_shotsconversion$AXSC <- as.numeric(t1_shotsconversion$AXSC)
# t1_shotsconversion$t1_hXgoals <- round((t1_shotsconversion$HXSC * t1_shotsconversion$t1_xHST), digits = 2)
# t1_shotsconversion$t1_aXgoals <- round((t1_shotsconversion$AXSC * t1_shotsconversion$t1_xAST), digits = 2)
# t1_shotsconversion$Xgoals <- t1_shotsconversion$t1_hXgoals + t1_shotsconversion$t1_aXgoals
#
# T1_all <- cbind(T1_fixtures_clone_final,t1_dmprediction,t1_avgyellow,t1_avgcorners,t1_goals,t1_shots,t1_fouls,t1_bookings,t1_corners,t1_shotsconversion)
# write.xlsx(T1_all,'Divisions/T1.xlsx', sheetName = "T1_all", append = TRUE)
#















































