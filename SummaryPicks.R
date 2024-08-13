library('xlsx')
#MLS
advstatsn <- 6
MLS_advstats <- readxl::read_excel('MLS_SPREAD.xlsx')
MLS_advstats <- MLS_advstats[,-1]

unlink('Summaries/MLS/*')
for(mls_sn in 1:28){
  df <- tail(MLS_advstats[MLS_advstats$HomeTeam == final_doublefixture_mls[mls_sn,1] | MLS_advstats$AwayTeam == final_doublefixture_mls[mls_sn,1] ,],advstatsn)

  df2 <- tail(MLS_advstats[MLS_advstats$HomeTeam == final_doublefixture_mls[mls_sn + 1,1] | MLS_advstats$AwayTeam == final_doublefixture_mls[mls_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:31]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rnewleagues\\Summaries\\MLS"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_mls[mls_sn,1],final_doublefixture_mls[mls_sn + 1,1],"adv.xlsx",sep = "_")))

}

View(MLS_advstats)
###################
#ARG
advstatsn <- 6
ARG_advstats <- readxl::read_excel('ARG_SPREAD.xlsx')
ARG_advstats <- ARG_advstats[,-1]

unlink('Summaries/ARG/*')
for(arg_sn in 1:27){
  df <- tail(ARG_advstats[ARG_advstats$HomeTeam == final_doublefixture_arg[arg_sn,1] | ARG_advstats$AwayTeam == final_doublefixture_arg[arg_sn,1] ,],advstatsn)

  df2 <- tail(ARG_advstats[ARG_advstats$HomeTeam == final_doublefixture_arg[arg_sn + 1,1] | ARG_advstats$AwayTeam == final_doublefixture_arg[arg_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:31]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rnewleagues\\Summaries\\ARG"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_arg[arg_sn,1],final_doublefixture_arg[arg_sn + 1,1],"adv.xlsx",sep = "_")))

}
##################################################################################################################################################
#BRA
advstatsn <- 6
BRA_advstats <- readxl::read_excel('BRA_SPREAD.xlsx')
BRA_advstats <- BRA_advstats[,-1]

unlink('Summaries/BRA/*')
for(bra_sn in 1:19){
  df <- tail(BRA_advstats[BRA_advstats$HomeTeam == final_doublefixture_bra[bra_sn,1] | BRA_advstats$AwayTeam == final_doublefixture_bra[bra_sn,1] ,],advstatsn)

  df2 <- tail(BRA_advstats[BRA_advstats$HomeTeam == final_doublefixture_bra[bra_sn + 1,1] | BRA_advstats$AwayTeam == final_doublefixture_bra[bra_sn + 1,1],],advstatsn)

  temp_analysis <- rbind(df,df2)

  temp_analysis <- as.data.frame(temp_analysis)
  temp_colmeans <- colMeans(temp_analysis[,c(32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67)])
  temp_sliced <- tail(temp_analysis,1)
  temp_sliced <- temp_sliced[1:31]

  temp_analyis_combined <- c(temp_sliced,temp_colmeans)
  temp_analysis <- rbind(temp_analysis,temp_analyis_combined)

  path = "C:\\Users\\Kovan\\Rnewleagues\\Summaries\\BRA"
  write.xlsx(temp_analysis,file.path(path,paste(final_doublefixture_bra[bra_sn,1],final_doublefixture_bra[bra_sn + 1,1],"adv.xlsx",sep = "_")))

}




##################################################
#different leagues union
##################################################

# advstatsn <- 6
# D1_advstats <- readxl::read_excel('BUNDES_SPREAD.xlsx')
# D1_advstats <- D1_advstats[,-1]
# I1_advstats <- readxl::read_excel('SERIEA_SPREAD.xlsx')
# I1_advstats <- I1_advstats[,-1]
# E0_advstats <- readxl::read_excel('EPL_SPREAD.xlsx')
# E0_advstats <- E0_advstats[,-1]
#
# UCL_advstats <- rbind(D1_advstats,I1_advstats,E0_advstats)
#
# df <- tail(UCL_advstats[UCL_advstats$HomeTeam == "Man United" | UCL_advstats$AwayTeam == "Man United",],advstatsn)
#
# df2 <- tail(UCL_advstats[UCL_advstats$HomeTeam == "Man City" | UCL_advstats$AwayTeam == "Man City",],advstatsn)
#
# temp_analysis <- rbind(df,df2)
#
# temp_analysis <- as.data.frame(temp_analysis)
# temp_colmeans <- colMeans(temp_analysis[,c(39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67)])
# temp_sliced <- tail(temp_analysis,1)
# temp_sliced <- temp_sliced[1:38]
#
# temp_analyis_combined <- c(temp_sliced,temp_colmeans)
# temp_analysis <- rbind(temp_analysis,temp_analyis_combined)
#
# write.xlsx(temp_analysis,"Temp/manunitedVcity.xlsx")










