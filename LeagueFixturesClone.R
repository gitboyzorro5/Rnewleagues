library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_281")
options(java.parameters = "-Xmx2048m")
library('xlsx')
library('scales')
library('lubridate')

#########################################################################################################################################
unlink('clonedprediction.xlsx')
unlink('picks_fixtures_prediction_cloned.csv')
#########################################################################################################################################
MLS_fixtures_clone <- MLS_fixtures
colnames(MLS_fixtures_clone)[61] <- "Hwin"
colnames(MLS_fixtures_clone)[62] <- "Draw"
colnames(MLS_fixtures_clone)[63] <- "Awin"

MLS_fixtures_clone$Hwinodds <-   MLS_fixtures$mls_1_0 + MLS_fixtures$mls_2_0 + MLS_fixtures$mls_2_1 + MLS_fixtures$mls_3_0 + MLS_fixtures$mls_3_1 +
  MLS_fixtures$mls_3_2 + MLS_fixtures$mls_4_0 + MLS_fixtures$mls_4_1 + MLS_fixtures$mls_4_2 + MLS_fixtures$mls_4_3 +
  MLS_fixtures$mls_5_0 + MLS_fixtures$mls_5_1 + MLS_fixtures$mls_5_2 + MLS_fixtures$mls_5_3 + MLS_fixtures$mls_5_4 +
  MLS_fixtures$mls_6_0 + MLS_fixtures$mls_6_1 + MLS_fixtures$mls_6_2 + MLS_fixtures$mls_6_3 + MLS_fixtures$mls_6_4 +
  MLS_fixtures$mls_6_5
MLS_fixtures_clone$Hwinodds <- round(1/MLS_fixtures_clone$Hwinodds, digits = 3)

MLS_fixtures_clone$Drawodds <-  MLS_fixtures$mls_0_0 + MLS_fixtures$mls_1_1 + MLS_fixtures$mls_2_2 + MLS_fixtures$mls_3_3 + MLS_fixtures$mls_4_4 +
  MLS_fixtures$mls_5_5 + MLS_fixtures$mls_6_6

MLS_fixtures_clone$Drawodds <- round(1/MLS_fixtures_clone$Drawodds, digits = 3)

MLS_fixtures_clone$Awinodds <-   MLS_fixtures$mls_0_1 + MLS_fixtures$mls_0_2 + MLS_fixtures$mls_1_2 + MLS_fixtures$mls_0_3 + MLS_fixtures$mls_1_3 +
  MLS_fixtures$mls_2_3 + MLS_fixtures$mls_0_4 + MLS_fixtures$mls_1_4 + MLS_fixtures$mls_2_4 + MLS_fixtures$mls_3_4 +
  MLS_fixtures$mls_0_5 + MLS_fixtures$mls_1_5 + MLS_fixtures$mls_2_5 + MLS_fixtures$mls_3_5 + MLS_fixtures$mls_4_5 +
  MLS_fixtures$mls_0_6 + MLS_fixtures$mls_1_6 + MLS_fixtures$mls_2_6 + MLS_fixtures$mls_3_6 + MLS_fixtures$mls_4_6 +
  MLS_fixtures$mls_5_6

MLS_fixtures_clone$Awinodds <- round(1/MLS_fixtures_clone$Awinodds, digits = 3)

colnames(MLS_fixtures_clone)[15] <- "CS_1-1"
colnames(MLS_fixtures_clone)[13] <- "CS_1-0"
colnames(MLS_fixtures_clone)[14] <- "CS_0-1"
colnames(MLS_fixtures_clone)[16] <- "CS_2-0"
colnames(MLS_fixtures_clone)[17] <- "CS_0-2"
colnames(MLS_fixtures_clone)[19] <- "CS_2-1"
colnames(MLS_fixtures_clone)[20] <- "CS_1-2"

MLS_fixtures_clone$`CS_1-1` <- round(1/MLS_fixtures_clone$`CS_1-1`, digits = 3)
MLS_fixtures_clone$`CS_1-0` <- round(1/MLS_fixtures_clone$`CS_1-0`, digits = 3)
MLS_fixtures_clone$`CS_0-1` <- round(1/MLS_fixtures_clone$`CS_0-1`, digits = 3)
MLS_fixtures_clone$`CS_2-0` <- round(1/MLS_fixtures_clone$`CS_2-0`, digits = 3)
MLS_fixtures_clone$`CS_0-2` <- round(1/MLS_fixtures_clone$`CS_0-2`, digits = 3)
MLS_fixtures_clone$`CS_2-1` <- round(1/MLS_fixtures_clone$`CS_2-1`, digits = 3)
MLS_fixtures_clone$`CS_1-2` <- round(1/MLS_fixtures_clone$`CS_1-2`, digits = 3)

colnames(MLS_fixtures_clone)[1] <- "league"
colnames(MLS_fixtures_clone)[2] <- "Hometeam"
colnames(MLS_fixtures_clone)[3] <- "Awayteam"
colnames(MLS_fixtures_clone)[92] <- "predscore"
colnames(MLS_fixtures_clone)[64] <- "ov25"
colnames(MLS_fixtures_clone)[66] <- "ov25odds"
colnames(MLS_fixtures_clone)[65] <- "un25"
colnames(MLS_fixtures_clone)[67] <- "un25odds"
colnames(MLS_fixtures_clone)[68] <- "BTTSY"
colnames(MLS_fixtures_clone)[69] <- "BTTSN"
colnames(MLS_fixtures_clone)[70] <- "BTTSYodds"
colnames(MLS_fixtures_clone)[71] <- "BTTSNodds"



MLS_fixtures_clone <- MLS_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
MLS_fixtures_clone$matchid <- paste(MLS_fixtures_clone$Hometeam,MLS_fixtures_clone$Awayteam,sep = '-')
#######################################################################################################################
#######################################################################################################################
#ARG
ARG_fixtures_clone <- ARG_fixtures
colnames(ARG_fixtures_clone)[61] <- "Hwin"
colnames(ARG_fixtures_clone)[62] <- "Draw"
colnames(ARG_fixtures_clone)[63] <- "Awin"

ARG_fixtures_clone$Hwinodds <-   ARG_fixtures$arg_1_0 + ARG_fixtures$arg_2_0 + ARG_fixtures$arg_2_1 + ARG_fixtures$arg_3_0 + ARG_fixtures$arg_3_1 +
  ARG_fixtures$arg_3_2 + ARG_fixtures$arg_4_0 + ARG_fixtures$arg_4_1 + ARG_fixtures$arg_4_2 + ARG_fixtures$arg_4_3 +
  ARG_fixtures$arg_5_0 + ARG_fixtures$arg_5_1 + ARG_fixtures$arg_5_2 + ARG_fixtures$arg_5_3 + ARG_fixtures$arg_5_4 +
  ARG_fixtures$arg_6_0 + ARG_fixtures$arg_6_1 + ARG_fixtures$arg_6_2 + ARG_fixtures$arg_6_3 + ARG_fixtures$arg_6_4 +
  ARG_fixtures$arg_6_5
ARG_fixtures_clone$Hwinodds <- round(1/ARG_fixtures_clone$Hwinodds, digits = 3)

ARG_fixtures_clone$Drawodds <-  ARG_fixtures$arg_0_0 + ARG_fixtures$arg_1_1 + ARG_fixtures$arg_2_2 + ARG_fixtures$arg_3_3 + ARG_fixtures$arg_4_4 +
  ARG_fixtures$arg_5_5 + ARG_fixtures$arg_6_6

ARG_fixtures_clone$Drawodds <- round(1/ARG_fixtures_clone$Drawodds, digits = 3)

ARG_fixtures_clone$Awinodds <-   ARG_fixtures$arg_0_1 + ARG_fixtures$arg_0_2 + ARG_fixtures$arg_1_2 + ARG_fixtures$arg_0_3 + ARG_fixtures$arg_1_3 +
  ARG_fixtures$arg_2_3 + ARG_fixtures$arg_0_4 + ARG_fixtures$arg_1_4 + ARG_fixtures$arg_2_4 + ARG_fixtures$arg_3_4 +
  ARG_fixtures$arg_0_5 + ARG_fixtures$arg_1_5 + ARG_fixtures$arg_2_5 + ARG_fixtures$arg_3_5 + ARG_fixtures$arg_4_5 +
  ARG_fixtures$arg_0_6 + ARG_fixtures$arg_1_6 + ARG_fixtures$arg_2_6 + ARG_fixtures$arg_3_6 + ARG_fixtures$arg_4_6 +
  ARG_fixtures$arg_5_6

ARG_fixtures_clone$Awinodds <- round(1/ARG_fixtures_clone$Awinodds, digits = 3)

colnames(ARG_fixtures_clone)[15] <- "CS_1-1"
colnames(ARG_fixtures_clone)[13] <- "CS_1-0"
colnames(ARG_fixtures_clone)[14] <- "CS_0-1"
colnames(ARG_fixtures_clone)[16] <- "CS_2-0"
colnames(ARG_fixtures_clone)[17] <- "CS_0-2"
colnames(ARG_fixtures_clone)[19] <- "CS_2-1"
colnames(ARG_fixtures_clone)[20] <- "CS_1-2"

ARG_fixtures_clone$`CS_1-1` <- round(1/ARG_fixtures_clone$`CS_1-1`, digits = 3)
ARG_fixtures_clone$`CS_1-0` <- round(1/ARG_fixtures_clone$`CS_1-0`, digits = 3)
ARG_fixtures_clone$`CS_0-1` <- round(1/ARG_fixtures_clone$`CS_0-1`, digits = 3)
ARG_fixtures_clone$`CS_2-0` <- round(1/ARG_fixtures_clone$`CS_2-0`, digits = 3)
ARG_fixtures_clone$`CS_0-2` <- round(1/ARG_fixtures_clone$`CS_0-2`, digits = 3)
ARG_fixtures_clone$`CS_2-1` <- round(1/ARG_fixtures_clone$`CS_2-1`, digits = 3)
ARG_fixtures_clone$`CS_1-2` <- round(1/ARG_fixtures_clone$`CS_1-2`, digits = 3)

colnames(ARG_fixtures_clone)[1] <- "league"
colnames(ARG_fixtures_clone)[2] <- "Hometeam"
colnames(ARG_fixtures_clone)[3] <- "Awayteam"
colnames(ARG_fixtures_clone)[92] <- "predscore"
colnames(ARG_fixtures_clone)[64] <- "ov25"
colnames(ARG_fixtures_clone)[66] <- "ov25odds"
colnames(ARG_fixtures_clone)[65] <- "un25"
colnames(ARG_fixtures_clone)[67] <- "un25odds"
colnames(ARG_fixtures_clone)[68] <- "BTTSY"
colnames(ARG_fixtures_clone)[69] <- "BTTSN"
colnames(ARG_fixtures_clone)[70] <- "BTTSYodds"
colnames(ARG_fixtures_clone)[71] <- "BTTSNodds"



ARG_fixtures_clone <- ARG_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
ARG_fixtures_clone$matchid <- paste(ARG_fixtures_clone$Hometeam,ARG_fixtures_clone$Awayteam,sep = '-')
######################################################################################################################
######################################################################################################################
#D2
# D2_fixtures_clone <- D2_fixtures
# colnames(D2_fixtures_clone)[61] <- "Hwin"
# colnames(D2_fixtures_clone)[62] <- "Draw"
# colnames(D2_fixtures_clone)[63] <- "Awin"
#
# D2_fixtures_clone$Hwinodds <-   D2_fixtures$d2_1_0 + D2_fixtures$d2_2_0 + D2_fixtures$d2_2_1 + D2_fixtures$d2_3_0 + D2_fixtures$d2_3_1 +
#   D2_fixtures$d2_3_2 + D2_fixtures$d2_4_0 + D2_fixtures$d2_4_1 + D2_fixtures$d2_4_2 + D2_fixtures$d2_4_3 +
#   D2_fixtures$d2_5_0 + D2_fixtures$d2_5_1 + D2_fixtures$d2_5_2 + D2_fixtures$d2_5_3 + D2_fixtures$d2_5_4 +
#   D2_fixtures$d2_6_0 + D2_fixtures$d2_6_1 + D2_fixtures$d2_6_2 + D2_fixtures$d2_6_3 + D2_fixtures$d2_6_4 +
#   D2_fixtures$d2_6_5
# D2_fixtures_clone$Hwinodds <- round(1/D2_fixtures_clone$Hwinodds, digits = 3)
#
# D2_fixtures_clone$Drawodds <-  D2_fixtures$d2_0_0 + D2_fixtures$d2_1_1 + D2_fixtures$d2_2_2 + D2_fixtures$d2_3_3 + D2_fixtures$d2_4_4 +
#   D2_fixtures$d2_5_5 + D2_fixtures$d2_6_6
#
# D2_fixtures_clone$Drawodds <- round(1/D2_fixtures_clone$Drawodds, digits = 3)
#
# D2_fixtures_clone$Awinodds <-   D2_fixtures$d2_0_1 + D2_fixtures$d2_0_2 + D2_fixtures$d2_1_2 + D2_fixtures$d2_0_3 + D2_fixtures$d2_1_3 +
#   D2_fixtures$d2_2_3 + D2_fixtures$d2_0_4 + D2_fixtures$d2_1_4 + D2_fixtures$d2_2_4 + D2_fixtures$d2_3_4 +
#   D2_fixtures$d2_0_5 + D2_fixtures$d2_1_5 + D2_fixtures$d2_2_5 + D2_fixtures$d2_3_5 + D2_fixtures$d2_4_5 +
#   D2_fixtures$d2_0_6 + D2_fixtures$d2_1_6 + D2_fixtures$d2_2_6 + D2_fixtures$d2_3_6 + D2_fixtures$d2_4_6 +
#   D2_fixtures$d2_5_6
#
# D2_fixtures_clone$Awinodds <- round(1/D2_fixtures_clone$Awinodds, digits = 3)
#
# colnames(D2_fixtures_clone)[15] <- "CS_1-1"
# colnames(D2_fixtures_clone)[13] <- "CS_1-0"
# colnames(D2_fixtures_clone)[14] <- "CS_0-1"
# colnames(D2_fixtures_clone)[16] <- "CS_2-0"
# colnames(D2_fixtures_clone)[17] <- "CS_0-2"
# colnames(D2_fixtures_clone)[19] <- "CS_2-1"
# colnames(D2_fixtures_clone)[20] <- "CS_1-2"
#
# D2_fixtures_clone$`CS_1-1` <- round(1/D2_fixtures_clone$`CS_1-1`, digits = 3)
# D2_fixtures_clone$`CS_1-0` <- round(1/D2_fixtures_clone$`CS_1-0`, digits = 3)
# D2_fixtures_clone$`CS_0-1` <- round(1/D2_fixtures_clone$`CS_0-1`, digits = 3)
# D2_fixtures_clone$`CS_2-0` <- round(1/D2_fixtures_clone$`CS_2-0`, digits = 3)
# D2_fixtures_clone$`CS_0-2` <- round(1/D2_fixtures_clone$`CS_0-2`, digits = 3)
# D2_fixtures_clone$`CS_2-1` <- round(1/D2_fixtures_clone$`CS_2-1`, digits = 3)
# D2_fixtures_clone$`CS_1-2` <- round(1/D2_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(D2_fixtures_clone)[1] <- "league"
# colnames(D2_fixtures_clone)[2] <- "Hometeam"
# colnames(D2_fixtures_clone)[3] <- "Awayteam"
# colnames(D2_fixtures_clone)[92] <- "predscore"
# colnames(D2_fixtures_clone)[64] <- "ov25"
# colnames(D2_fixtures_clone)[66] <- "ov25odds"
# colnames(D2_fixtures_clone)[65] <- "un25"
# colnames(D2_fixtures_clone)[67] <- "un25odds"
# colnames(D2_fixtures_clone)[68] <- "BTTSY"
# colnames(D2_fixtures_clone)[69] <- "BTTSN"
# colnames(D2_fixtures_clone)[70] <- "BTTSYodds"
# colnames(D2_fixtures_clone)[71] <- "BTTSNodds"
#
#
# D2_fixtures_clone <- D2_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# D2_fixtures_clone$matchid <- paste(D2_fixtures_clone$Hometeam,D2_fixtures_clone$Awayteam,sep = '-')
# ######################################################################################################################
# ######################################################################################################################
# #E0
# E0_fixtures_clone <- E0_fixtures
# colnames(E0_fixtures_clone)[61] <- "Hwin"
# colnames(E0_fixtures_clone)[62] <- "Draw"
# colnames(E0_fixtures_clone)[63] <- "Awin"
#
# E0_fixtures_clone$Hwinodds <-   E0_fixtures$e0_1_0 + E0_fixtures$e0_2_0 + E0_fixtures$e0_2_1 + E0_fixtures$e0_3_0 + E0_fixtures$e0_3_1 +
#   E0_fixtures$e0_3_2 + E0_fixtures$e0_4_0 + E0_fixtures$e0_4_1 + E0_fixtures$e0_4_2 + E0_fixtures$e0_4_3 +
#   E0_fixtures$e0_5_0 + E0_fixtures$e0_5_1 + E0_fixtures$e0_5_2 + E0_fixtures$e0_5_3 + E0_fixtures$e0_5_4 +
#   E0_fixtures$e0_6_0 + E0_fixtures$e0_6_1 + E0_fixtures$e0_6_2 + E0_fixtures$e0_6_3 + E0_fixtures$e0_6_4 +
#   E0_fixtures$e0_6_5
# E0_fixtures_clone$Hwinodds <- round(1/E0_fixtures_clone$Hwinodds, digits = 3)
#
# E0_fixtures_clone$Drawodds <-  E0_fixtures$e0_0_0 + E0_fixtures$e0_1_1 + E0_fixtures$e0_2_2 + E0_fixtures$e0_3_3 + E0_fixtures$e0_4_4 +
#   E0_fixtures$e0_5_5 + E0_fixtures$e0_6_6
#
# E0_fixtures_clone$Drawodds <- round(1/E0_fixtures_clone$Drawodds, digits = 3)
#
# E0_fixtures_clone$Awinodds <-   E0_fixtures$e0_0_1 + E0_fixtures$e0_0_2 + E0_fixtures$e0_1_2 + E0_fixtures$e0_0_3 + E0_fixtures$e0_1_3 +
#   E0_fixtures$e0_2_3 + E0_fixtures$e0_0_4 + E0_fixtures$e0_1_4 + E0_fixtures$e0_2_4 + E0_fixtures$e0_3_4 +
#   E0_fixtures$e0_0_5 + E0_fixtures$e0_1_5 + E0_fixtures$e0_2_5 + E0_fixtures$e0_3_5 + E0_fixtures$e0_4_5 +
#   E0_fixtures$e0_0_6 + E0_fixtures$e0_1_6 + E0_fixtures$e0_2_6 + E0_fixtures$e0_3_6 + E0_fixtures$e0_4_6 +
#   E0_fixtures$e0_5_6
#
# E0_fixtures_clone$Awinodds <- round(1/E0_fixtures_clone$Awinodds, digits = 3)
#
# colnames(E0_fixtures_clone)[15] <- "CS_1-1"
# colnames(E0_fixtures_clone)[13] <- "CS_1-0"
# colnames(E0_fixtures_clone)[14] <- "CS_0-1"
# colnames(E0_fixtures_clone)[16] <- "CS_2-0"
# colnames(E0_fixtures_clone)[17] <- "CS_0-2"
# colnames(E0_fixtures_clone)[19] <- "CS_2-1"
# colnames(E0_fixtures_clone)[20] <- "CS_1-2"
#
# E0_fixtures_clone$`CS_1-1` <- round(1/E0_fixtures_clone$`CS_1-1`, digits = 3)
# E0_fixtures_clone$`CS_1-0` <- round(1/E0_fixtures_clone$`CS_1-0`, digits = 3)
# E0_fixtures_clone$`CS_0-1` <- round(1/E0_fixtures_clone$`CS_0-1`, digits = 3)
# E0_fixtures_clone$`CS_2-0` <- round(1/E0_fixtures_clone$`CS_2-0`, digits = 3)
# E0_fixtures_clone$`CS_0-2` <- round(1/E0_fixtures_clone$`CS_0-2`, digits = 3)
# E0_fixtures_clone$`CS_2-1` <- round(1/E0_fixtures_clone$`CS_2-1`, digits = 3)
# E0_fixtures_clone$`CS_1-2` <- round(1/E0_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(E0_fixtures_clone)[1] <- "league"
# colnames(E0_fixtures_clone)[2] <- "Hometeam"
# colnames(E0_fixtures_clone)[3] <- "Awayteam"
# colnames(E0_fixtures_clone)[92] <- "predscore"
# colnames(E0_fixtures_clone)[64] <- "ov25"
# colnames(E0_fixtures_clone)[66] <- "ov25odds"
# colnames(E0_fixtures_clone)[65] <- "un25"
# colnames(E0_fixtures_clone)[67] <- "un25odds"
# colnames(E0_fixtures_clone)[68] <- "BTTSY"
# colnames(E0_fixtures_clone)[69] <- "BTTSN"
# colnames(E0_fixtures_clone)[70] <- "BTTSYodds"
# colnames(E0_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# E0_fixtures_clone <- E0_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# E0_fixtures_clone$matchid <- paste(E0_fixtures_clone$Hometeam,E0_fixtures_clone$Awayteam,sep = '-')
# ######################################################################################################################
# #E1
# E1_fixtures_clone <- E1_fixtures
# colnames(E1_fixtures_clone)[61] <- "Hwin"
# colnames(E1_fixtures_clone)[62] <- "Draw"
# colnames(E1_fixtures_clone)[63] <- "Awin"
#
# E1_fixtures_clone$Hwinodds <-   E1_fixtures$e1_1_0 + E1_fixtures$e1_2_0 + E1_fixtures$e1_2_1 + E1_fixtures$e1_3_0 + E1_fixtures$e1_3_1 +
#   E1_fixtures$e1_3_2 + E1_fixtures$e1_4_0 + E1_fixtures$e1_4_1 + E1_fixtures$e1_4_2 + E1_fixtures$e1_4_3 +
#   E1_fixtures$e1_5_0 + E1_fixtures$e1_5_1 + E1_fixtures$e1_5_2 + E1_fixtures$e1_5_3 + E1_fixtures$e1_5_4 +
#   E1_fixtures$e1_6_0 + E1_fixtures$e1_6_1 + E1_fixtures$e1_6_2 + E1_fixtures$e1_6_3 + E1_fixtures$e1_6_4 +
#   E1_fixtures$e1_6_5
# E1_fixtures_clone$Hwinodds <- round(1/E1_fixtures_clone$Hwinodds, digits = 3)
#
# E1_fixtures_clone$Drawodds <-  E1_fixtures$e1_0_0 + E1_fixtures$e1_1_1 + E1_fixtures$e1_2_2 + E1_fixtures$e1_3_3 + E1_fixtures$e1_4_4 +
#   E1_fixtures$e1_5_5 + E1_fixtures$e1_6_6
#
# E1_fixtures_clone$Drawodds <- round(1/E1_fixtures_clone$Drawodds, digits = 3)
#
# E1_fixtures_clone$Awinodds <-   E1_fixtures$e1_0_1 + E1_fixtures$e1_0_2 + E1_fixtures$e1_1_2 + E1_fixtures$e1_0_3 + E1_fixtures$e1_1_3 +
#   E1_fixtures$e1_2_3 + E1_fixtures$e1_0_4 + E1_fixtures$e1_1_4 + E1_fixtures$e1_2_4 + E1_fixtures$e1_3_4 +
#   E1_fixtures$e1_0_5 + E1_fixtures$e1_1_5 + E1_fixtures$e1_2_5 + E1_fixtures$e1_3_5 + E1_fixtures$e1_4_5 +
#   E1_fixtures$e1_0_6 + E1_fixtures$e1_1_6 + E1_fixtures$e1_2_6 + E1_fixtures$e1_3_6 + E1_fixtures$e1_4_6 +
#   E1_fixtures$e1_5_6
#
# E1_fixtures_clone$Awinodds <- round(1/E1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(E1_fixtures_clone)[15] <- "CS_1-1"
# colnames(E1_fixtures_clone)[13] <- "CS_1-0"
# colnames(E1_fixtures_clone)[14] <- "CS_0-1"
# colnames(E1_fixtures_clone)[16] <- "CS_2-0"
# colnames(E1_fixtures_clone)[17] <- "CS_0-2"
# colnames(E1_fixtures_clone)[19] <- "CS_2-1"
# colnames(E1_fixtures_clone)[20] <- "CS_1-2"
# colnames(E0_fixtures)
# E1_fixtures_clone$`CS_1-1` <- round(1/E1_fixtures_clone$`CS_1-1`, digits = 3)
# E1_fixtures_clone$`CS_1-0` <- round(1/E1_fixtures_clone$`CS_1-0`, digits = 3)
# E1_fixtures_clone$`CS_0-1` <- round(1/E1_fixtures_clone$`CS_0-1`, digits = 3)
# E1_fixtures_clone$`CS_2-0` <- round(1/E1_fixtures_clone$`CS_2-0`, digits = 3)
# E1_fixtures_clone$`CS_0-2` <- round(1/E1_fixtures_clone$`CS_0-2`, digits = 3)
# E1_fixtures_clone$`CS_2-1` <- round(1/E1_fixtures_clone$`CS_2-1`, digits = 3)
# E1_fixtures_clone$`CS_1-2` <- round(1/E1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(E1_fixtures_clone)[1] <- "league"
# colnames(E1_fixtures_clone)[2] <- "Hometeam"
# colnames(E1_fixtures_clone)[3] <- "Awayteam"
# colnames(E1_fixtures_clone)[92] <- "predscore"
# colnames(E1_fixtures_clone)[64] <- "ov25"
# colnames(E1_fixtures_clone)[66] <- "ov25odds"
# colnames(E1_fixtures_clone)[65] <- "un25"
# colnames(E1_fixtures_clone)[67] <- "un25odds"
# colnames(E1_fixtures_clone)[68] <- "BTTSY"
# colnames(E1_fixtures_clone)[69] <- "BTTSN"
# colnames(E1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(E1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# E1_fixtures_clone <- E1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# E1_fixtures_clone$matchid <- paste(E1_fixtures_clone$Hometeam,E1_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #######################################################################################################################
# #E2
# E2_fixtures_clone <- E2_fixtures
# colnames(E2_fixtures_clone)[61] <- "Hwin"
# colnames(E2_fixtures_clone)[62] <- "Draw"
# colnames(E2_fixtures_clone)[63] <- "Awin"
#
# E2_fixtures_clone$Hwinodds <-   E2_fixtures$e2_1_0 + E2_fixtures$e2_2_0 + E2_fixtures$e2_2_1 + E2_fixtures$e2_3_0 + E2_fixtures$e2_3_1 +
#   E2_fixtures$e2_3_2 + E2_fixtures$e2_4_0 + E2_fixtures$e2_4_1 + E2_fixtures$e2_4_2 + E2_fixtures$e2_4_3 +
#   E2_fixtures$e2_5_0 + E2_fixtures$e2_5_1 + E2_fixtures$e2_5_2 + E2_fixtures$e2_5_3 + E2_fixtures$e2_5_4 +
#   E2_fixtures$e2_6_0 + E2_fixtures$e2_6_1 + E2_fixtures$e2_6_2 + E2_fixtures$e2_6_3 + E2_fixtures$e2_6_4 +
#   E2_fixtures$e2_6_5
# E2_fixtures_clone$Hwinodds <- round(1/E2_fixtures_clone$Hwinodds, digits = 3)
#
# E2_fixtures_clone$Drawodds <-  E2_fixtures$e2_0_0 + E2_fixtures$e2_1_1 + E2_fixtures$e2_2_2 + E2_fixtures$e2_3_3 + E2_fixtures$e2_4_4 +
#   E2_fixtures$e2_5_5 + E2_fixtures$e2_6_6
#
# E2_fixtures_clone$Drawodds <- round(1/E2_fixtures_clone$Drawodds, digits = 3)
#
# E2_fixtures_clone$Awinodds <-   E2_fixtures$e2_0_1 + E2_fixtures$e2_0_2 + E2_fixtures$e2_1_2 + E2_fixtures$e2_0_3 + E2_fixtures$e2_1_3 +
#   E2_fixtures$e2_2_3 + E2_fixtures$e2_0_4 + E2_fixtures$e2_1_4 + E2_fixtures$e2_2_4 + E2_fixtures$e2_3_4 +
#   E2_fixtures$e2_0_5 + E2_fixtures$e2_1_5 + E2_fixtures$e2_2_5 + E2_fixtures$e2_3_5 + E2_fixtures$e2_4_5 +
#   E2_fixtures$e2_0_6 + E2_fixtures$e2_1_6 + E2_fixtures$e2_2_6 + E2_fixtures$e2_3_6 + E2_fixtures$e2_4_6 +
#   E2_fixtures$e2_5_6
#
# E2_fixtures_clone$Awinodds <- round(1/E2_fixtures_clone$Awinodds, digits = 3)
#
# colnames(E2_fixtures_clone)[15] <- "CS_1-1"
# colnames(E2_fixtures_clone)[13] <- "CS_1-0"
# colnames(E2_fixtures_clone)[14] <- "CS_0-1"
# colnames(E2_fixtures_clone)[16] <- "CS_2-0"
# colnames(E2_fixtures_clone)[17] <- "CS_0-2"
# colnames(E2_fixtures_clone)[19] <- "CS_2-1"
# colnames(E2_fixtures_clone)[20] <- "CS_1-2"
#
# E2_fixtures_clone$`CS_1-1` <- round(1/E2_fixtures_clone$`CS_1-1`, digits = 3)
# E2_fixtures_clone$`CS_1-0` <- round(1/E2_fixtures_clone$`CS_1-0`, digits = 3)
# E2_fixtures_clone$`CS_0-1` <- round(1/E2_fixtures_clone$`CS_0-1`, digits = 3)
# E2_fixtures_clone$`CS_2-0` <- round(1/E2_fixtures_clone$`CS_2-0`, digits = 3)
# E2_fixtures_clone$`CS_0-2` <- round(1/E2_fixtures_clone$`CS_0-2`, digits = 3)
# E2_fixtures_clone$`CS_2-1` <- round(1/E2_fixtures_clone$`CS_2-1`, digits = 3)
# E2_fixtures_clone$`CS_1-2` <- round(1/E2_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(E2_fixtures_clone)[1] <- "league"
# colnames(E2_fixtures_clone)[2] <- "Hometeam"
# colnames(E2_fixtures_clone)[3] <- "Awayteam"
# colnames(E2_fixtures_clone)[92] <- "predscore"
# colnames(E2_fixtures_clone)[64] <- "ov25"
# colnames(E2_fixtures_clone)[66] <- "ov25odds"
# colnames(E2_fixtures_clone)[65] <- "un25"
# colnames(E2_fixtures_clone)[67] <- "un25odds"
# colnames(E2_fixtures_clone)[68] <- "BTTSY"
# colnames(E2_fixtures_clone)[69] <- "BTTSN"
# colnames(E2_fixtures_clone)[70] <- "BTTSYodds"
# colnames(E2_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# E2_fixtures_clone <- E2_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# E2_fixtures_clone$matchid <- paste(E2_fixtures_clone$Hometeam,E2_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #######################################################################################################################
# #E3
# E3_fixtures_clone <- E3_fixtures
# colnames(E3_fixtures_clone)[61] <- "Hwin"
# colnames(E3_fixtures_clone)[62] <- "Draw"
# colnames(E3_fixtures_clone)[63] <- "Awin"
#
# E3_fixtures_clone$Hwinodds <-   E3_fixtures$e3_1_0 + E3_fixtures$e3_2_0 + E3_fixtures$e3_2_1 + E3_fixtures$e3_3_0 + E3_fixtures$e3_3_1 +
#   E3_fixtures$e3_3_2 + E3_fixtures$e3_4_0 + E3_fixtures$e3_4_1 + E3_fixtures$e3_4_2 + E3_fixtures$e3_4_3 +
#   E3_fixtures$e3_5_0 + E3_fixtures$e3_5_1 + E3_fixtures$e3_5_2 + E3_fixtures$e3_5_3 + E3_fixtures$e3_5_4 +
#   E3_fixtures$e3_6_0 + E3_fixtures$e3_6_1 + E3_fixtures$e3_6_2 + E3_fixtures$e3_6_3 + E3_fixtures$e3_6_4 +
#   E3_fixtures$e3_6_5
# E3_fixtures_clone$Hwinodds <- round(1/E3_fixtures_clone$Hwinodds, digits = 3)
#
# E3_fixtures_clone$Drawodds <-  E3_fixtures$e3_0_0 + E3_fixtures$e3_1_1 + E3_fixtures$e3_2_2 + E3_fixtures$e3_3_3 + E3_fixtures$e3_4_4 +
#   E3_fixtures$e3_5_5 + E3_fixtures$e3_6_6
#
# E3_fixtures_clone$Drawodds <- round(1/E3_fixtures_clone$Drawodds, digits = 3)
#
# E3_fixtures_clone$Awinodds <-   E3_fixtures$e3_0_1 + E3_fixtures$e3_0_2 + E3_fixtures$e3_1_2 + E3_fixtures$e3_0_3 + E3_fixtures$e3_1_3 +
#   E3_fixtures$e3_2_3 + E3_fixtures$e3_0_4 + E3_fixtures$e3_1_4 + E3_fixtures$e3_2_4 + E3_fixtures$e3_3_4 +
#   E3_fixtures$e3_0_5 + E3_fixtures$e3_1_5 + E3_fixtures$e3_2_5 + E3_fixtures$e3_3_5 + E3_fixtures$e3_4_5 +
#   E3_fixtures$e3_0_6 + E3_fixtures$e3_1_6 + E3_fixtures$e3_2_6 + E3_fixtures$e3_3_6 + E3_fixtures$e3_4_6 +
#   E3_fixtures$e3_5_6
#
# E3_fixtures_clone$Awinodds <- round(1/E3_fixtures_clone$Awinodds, digits = 3)
#
# colnames(E3_fixtures_clone)[15] <- "CS_1-1"
# colnames(E3_fixtures_clone)[13] <- "CS_1-0"
# colnames(E3_fixtures_clone)[14] <- "CS_0-1"
# colnames(E3_fixtures_clone)[16] <- "CS_2-0"
# colnames(E3_fixtures_clone)[17] <- "CS_0-2"
# colnames(E3_fixtures_clone)[19] <- "CS_2-1"
# colnames(E3_fixtures_clone)[20] <- "CS_1-2"
#
# E3_fixtures_clone$`CS_1-1` <- round(1/E3_fixtures_clone$`CS_1-1`, digits = 3)
# E3_fixtures_clone$`CS_1-0` <- round(1/E3_fixtures_clone$`CS_1-0`, digits = 3)
# E3_fixtures_clone$`CS_0-1` <- round(1/E3_fixtures_clone$`CS_0-1`, digits = 3)
# E3_fixtures_clone$`CS_2-0` <- round(1/E3_fixtures_clone$`CS_2-0`, digits = 3)
# E3_fixtures_clone$`CS_0-2` <- round(1/E3_fixtures_clone$`CS_0-2`, digits = 3)
# E3_fixtures_clone$`CS_2-1` <- round(1/E3_fixtures_clone$`CS_2-1`, digits = 3)
# E3_fixtures_clone$`CS_1-2` <- round(1/E3_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(E3_fixtures_clone)[1] <- "league"
# colnames(E3_fixtures_clone)[2] <- "Hometeam"
# colnames(E3_fixtures_clone)[3] <- "Awayteam"
# colnames(E3_fixtures_clone)[92] <- "predscore"
# colnames(E3_fixtures_clone)[64] <- "ov25"
# colnames(E3_fixtures_clone)[66] <- "ov25odds"
# colnames(E3_fixtures_clone)[65] <- "un25"
# colnames(E3_fixtures_clone)[67] <- "un25odds"
# colnames(E3_fixtures_clone)[68] <- "BTTSY"
# colnames(E3_fixtures_clone)[69] <- "BTTSN"
# colnames(E3_fixtures_clone)[70] <- "BTTSYodds"
# colnames(E3_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# E3_fixtures_clone <- E3_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# E3_fixtures_clone$matchid <- paste(E3_fixtures_clone$Hometeam,E3_fixtures_clone$Awayteam,sep = '-')
# ########################################################################################################################
# #Ec
# EC_fixtures_clone <- EC_fixtures
# colnames(EC_fixtures_clone)[61] <- "Hwin"
# colnames(EC_fixtures_clone)[62] <- "Draw"
# colnames(EC_fixtures_clone)[63] <- "Awin"
#
# EC_fixtures_clone$Hwinodds <-   EC_fixtures$ec_1_0 + EC_fixtures$ec_2_0 + EC_fixtures$ec_2_1 + EC_fixtures$ec_3_0 + EC_fixtures$ec_3_1 +
#   EC_fixtures$ec_3_2 + EC_fixtures$ec_4_0 + EC_fixtures$ec_4_1 + EC_fixtures$ec_4_2 + EC_fixtures$ec_4_3 +
#   EC_fixtures$ec_5_0 + EC_fixtures$ec_5_1 + EC_fixtures$ec_5_2 + EC_fixtures$ec_5_3 + EC_fixtures$ec_5_4 +
#   EC_fixtures$ec_6_0 + EC_fixtures$ec_6_1 + EC_fixtures$ec_6_2 + EC_fixtures$ec_6_3 + EC_fixtures$ec_6_4 +
#   EC_fixtures$ec_6_5
# EC_fixtures_clone$Hwinodds <- round(1/EC_fixtures_clone$Hwinodds, digits = 3)
#
# EC_fixtures_clone$Drawodds <-  EC_fixtures$ec_0_0 + EC_fixtures$ec_1_1 + EC_fixtures$ec_2_2 + EC_fixtures$ec_3_3 + EC_fixtures$ec_4_4 +
#   EC_fixtures$ec_5_5 + EC_fixtures$ec_6_6
#
# EC_fixtures_clone$Drawodds <- round(1/EC_fixtures_clone$Drawodds, digits = 3)
#
# EC_fixtures_clone$Awinodds <-   EC_fixtures$ec_0_1 + EC_fixtures$ec_0_2 + EC_fixtures$ec_1_2 + EC_fixtures$ec_0_3 + EC_fixtures$ec_1_3 +
#   EC_fixtures$ec_2_3 + EC_fixtures$ec_0_4 + EC_fixtures$ec_1_4 + EC_fixtures$ec_2_4 + EC_fixtures$ec_3_4 +
#   EC_fixtures$ec_0_5 + EC_fixtures$ec_1_5 + EC_fixtures$ec_2_5 + EC_fixtures$ec_3_5 + EC_fixtures$ec_4_5 +
#   EC_fixtures$ec_0_6 + EC_fixtures$ec_1_6 + EC_fixtures$ec_2_6 + EC_fixtures$ec_3_6 + EC_fixtures$ec_4_6 +
#   EC_fixtures$ec_5_6
#
# EC_fixtures_clone$Awinodds <- round(1/EC_fixtures_clone$Awinodds, digits = 3)
#
# colnames(EC_fixtures_clone)[15] <- "CS_1-1"
# colnames(EC_fixtures_clone)[13] <- "CS_1-0"
# colnames(EC_fixtures_clone)[14] <- "CS_0-1"
# colnames(EC_fixtures_clone)[16] <- "CS_2-0"
# colnames(EC_fixtures_clone)[17] <- "CS_0-2"
# colnames(EC_fixtures_clone)[19] <- "CS_2-1"
# colnames(EC_fixtures_clone)[20] <- "CS_1-2"
#
# EC_fixtures_clone$`CS_1-1` <- round(1/EC_fixtures_clone$`CS_1-1`, digits = 3)
# EC_fixtures_clone$`CS_1-0` <- round(1/EC_fixtures_clone$`CS_1-0`, digits = 3)
# EC_fixtures_clone$`CS_0-1` <- round(1/EC_fixtures_clone$`CS_0-1`, digits = 3)
# EC_fixtures_clone$`CS_2-0` <- round(1/EC_fixtures_clone$`CS_2-0`, digits = 3)
# EC_fixtures_clone$`CS_0-2` <- round(1/EC_fixtures_clone$`CS_0-2`, digits = 3)
# EC_fixtures_clone$`CS_2-1` <- round(1/EC_fixtures_clone$`CS_2-1`, digits = 3)
# EC_fixtures_clone$`CS_1-2` <- round(1/EC_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(EC_fixtures_clone)[1] <- "league"
# colnames(EC_fixtures_clone)[2] <- "Hometeam"
# colnames(EC_fixtures_clone)[3] <- "Awayteam"
# colnames(EC_fixtures_clone)[92] <- "predscore"
# colnames(EC_fixtures_clone)[64] <- "ov25"
# colnames(EC_fixtures_clone)[66] <- "ov25odds"
# colnames(EC_fixtures_clone)[65] <- "un25"
# colnames(EC_fixtures_clone)[67] <- "un25odds"
# colnames(EC_fixtures_clone)[68] <- "BTTSY"
# colnames(EC_fixtures_clone)[69] <- "BTTSN"
# colnames(EC_fixtures_clone)[70] <- "BTTSYodds"
# colnames(EC_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# EC_fixtures_clone <- EC_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# EC_fixtures_clone$matchid <- paste(EC_fixtures_clone$Hometeam,EC_fixtures_clone$Awayteam,sep = '-')
# ########################################################################################################################
# #F1
# F1_fixtures_clone <- F1_fixtures
# colnames(F1_fixtures_clone)[61] <- "Hwin"
# colnames(F1_fixtures_clone)[62] <- "Draw"
# colnames(F1_fixtures_clone)[63] <- "Awin"
#
# F1_fixtures_clone$Hwinodds <-   F1_fixtures$f1_1_0 + F1_fixtures$f1_2_0 + F1_fixtures$f1_2_1 + F1_fixtures$f1_3_0 + F1_fixtures$f1_3_1 +
#   F1_fixtures$f1_3_2 + F1_fixtures$f1_4_0 + F1_fixtures$f1_4_1 + F1_fixtures$f1_4_2 + F1_fixtures$f1_4_3 +
#   F1_fixtures$f1_5_0 + F1_fixtures$f1_5_1 + F1_fixtures$f1_5_2 + F1_fixtures$f1_5_3 + F1_fixtures$f1_5_4 +
#   F1_fixtures$f1_6_0 + F1_fixtures$f1_6_1 + F1_fixtures$f1_6_2 + F1_fixtures$f1_6_3 + F1_fixtures$f1_6_4 +
#   F1_fixtures$f1_6_5
# F1_fixtures_clone$Hwinodds <- round(1/F1_fixtures_clone$Hwinodds, digits = 3)
#
# F1_fixtures_clone$Drawodds <-  F1_fixtures$f1_0_0 + F1_fixtures$f1_1_1 + F1_fixtures$f1_2_2 + F1_fixtures$f1_3_3 + F1_fixtures$f1_4_4 +
#   F1_fixtures$f1_5_5 + F1_fixtures$f1_6_6
#
# F1_fixtures_clone$Drawodds <- round(1/F1_fixtures_clone$Drawodds, digits = 3)
#
# F1_fixtures_clone$Awinodds <-   F1_fixtures$f1_0_1 + F1_fixtures$f1_0_2 + F1_fixtures$f1_1_2 + F1_fixtures$f1_0_3 + F1_fixtures$f1_1_3 +
#   F1_fixtures$f1_2_3 + F1_fixtures$f1_0_4 + F1_fixtures$f1_1_4 + F1_fixtures$f1_2_4 + F1_fixtures$f1_3_4 +
#   F1_fixtures$f1_0_5 + F1_fixtures$f1_1_5 + F1_fixtures$f1_2_5 + F1_fixtures$f1_3_5 + F1_fixtures$f1_4_5 +
#   F1_fixtures$f1_0_6 + F1_fixtures$f1_1_6 + F1_fixtures$f1_2_6 + F1_fixtures$f1_3_6 + F1_fixtures$f1_4_6 +
#   F1_fixtures$f1_5_6
#
# F1_fixtures_clone$Awinodds <- round(1/F1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(F1_fixtures_clone)[15] <- "CS_1-1"
# colnames(F1_fixtures_clone)[13] <- "CS_1-0"
# colnames(F1_fixtures_clone)[14] <- "CS_0-1"
# colnames(F1_fixtures_clone)[16] <- "CS_2-0"
# colnames(F1_fixtures_clone)[17] <- "CS_0-2"
# colnames(F1_fixtures_clone)[19] <- "CS_2-1"
# colnames(F1_fixtures_clone)[20] <- "CS_1-2"
#
# F1_fixtures_clone$`CS_1-1` <- round(1/F1_fixtures_clone$`CS_1-1`, digits = 3)
# F1_fixtures_clone$`CS_1-0` <- round(1/F1_fixtures_clone$`CS_1-0`, digits = 3)
# F1_fixtures_clone$`CS_0-1` <- round(1/F1_fixtures_clone$`CS_0-1`, digits = 3)
# F1_fixtures_clone$`CS_2-0` <- round(1/F1_fixtures_clone$`CS_2-0`, digits = 3)
# F1_fixtures_clone$`CS_0-2` <- round(1/F1_fixtures_clone$`CS_0-2`, digits = 3)
# F1_fixtures_clone$`CS_2-1` <- round(1/F1_fixtures_clone$`CS_2-1`, digits = 3)
# F1_fixtures_clone$`CS_1-2` <- round(1/F1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(F1_fixtures_clone)[1] <- "league"
# colnames(F1_fixtures_clone)[2] <- "Hometeam"
# colnames(F1_fixtures_clone)[3] <- "Awayteam"
# colnames(F1_fixtures_clone)[92] <- "predscore"
# colnames(F1_fixtures_clone)[64] <- "ov25"
# colnames(F1_fixtures_clone)[66] <- "ov25odds"
# colnames(F1_fixtures_clone)[65] <- "un25"
# colnames(F1_fixtures_clone)[67] <- "un25odds"
# colnames(F1_fixtures_clone)[68] <- "BTTSY"
# colnames(F1_fixtures_clone)[69] <- "BTTSN"
# colnames(F1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(F1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# F1_fixtures_clone <- F1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# F1_fixtures_clone$matchid <- paste(F1_fixtures_clone$Hometeam,F1_fixtures_clone$Awayteam,sep = '-')
# ######################################################################################################################
# ######################################################################################################################
# #F2
# F2_fixtures_clone <- F2_fixtures
# colnames(F2_fixtures_clone)[61] <- "Hwin"
# colnames(F2_fixtures_clone)[62] <- "Draw"
# colnames(F2_fixtures_clone)[63] <- "Awin"
#
# F2_fixtures_clone$Hwinodds <-   F2_fixtures$f2_1_0 + F2_fixtures$f2_2_0 + F2_fixtures$f2_2_1 + F2_fixtures$f2_3_0 + F2_fixtures$f2_3_1 +
#   F2_fixtures$f2_3_2 + F2_fixtures$f2_4_0 + F2_fixtures$f2_4_1 + F2_fixtures$f2_4_2 + F2_fixtures$f2_4_3 +
#   F2_fixtures$f2_5_0 + F2_fixtures$f2_5_1 + F2_fixtures$f2_5_2 + F2_fixtures$f2_5_3 + F2_fixtures$f2_5_4 +
#   F2_fixtures$f2_6_0 + F2_fixtures$f2_6_1 + F2_fixtures$f2_6_2 + F2_fixtures$f2_6_3 + F2_fixtures$f2_6_4 +
#   F2_fixtures$f2_6_5
# F2_fixtures_clone$Hwinodds <- round(1/F2_fixtures_clone$Hwinodds, digits = 3)
#
# F2_fixtures_clone$Drawodds <-  F2_fixtures$f2_0_0 + F2_fixtures$f2_1_1 + F2_fixtures$f2_2_2 + F2_fixtures$f2_3_3 + F2_fixtures$f2_4_4 +
#   F2_fixtures$f2_5_5 + F2_fixtures$f2_6_6
#
# F2_fixtures_clone$Drawodds <- round(1/F2_fixtures_clone$Drawodds, digits = 3)
#
# F2_fixtures_clone$Awinodds <-   F2_fixtures$f2_0_1 + F2_fixtures$f2_0_2 + F2_fixtures$f2_1_2 + F2_fixtures$f2_0_3 + F2_fixtures$f2_1_3 +
#   F2_fixtures$f2_2_3 + F2_fixtures$f2_0_4 + F2_fixtures$f2_1_4 + F2_fixtures$f2_2_4 + F2_fixtures$f2_3_4 +
#   F2_fixtures$f2_0_5 + F2_fixtures$f2_1_5 + F2_fixtures$f2_2_5 + F2_fixtures$f2_3_5 + F2_fixtures$f2_4_5 +
#   F2_fixtures$f2_0_6 + F2_fixtures$f2_1_6 + F2_fixtures$f2_2_6 + F2_fixtures$f2_3_6 + F2_fixtures$f2_4_6 +
#   F2_fixtures$f2_5_6
#
# F2_fixtures_clone$Awinodds <- round(1/F2_fixtures_clone$Awinodds, digits = 3)
#
# colnames(F2_fixtures_clone)[15] <- "CS_1-1"
# colnames(F2_fixtures_clone)[13] <- "CS_1-0"
# colnames(F2_fixtures_clone)[14] <- "CS_0-1"
# colnames(F2_fixtures_clone)[16] <- "CS_2-0"
# colnames(F2_fixtures_clone)[17] <- "CS_0-2"
# colnames(F2_fixtures_clone)[19] <- "CS_2-1"
# colnames(F2_fixtures_clone)[20] <- "CS_1-2"
#
# F2_fixtures_clone$`CS_1-1` <- round(1/F2_fixtures_clone$`CS_1-1`, digits = 3)
# F2_fixtures_clone$`CS_1-0` <- round(1/F2_fixtures_clone$`CS_1-0`, digits = 3)
# F2_fixtures_clone$`CS_0-1` <- round(1/F2_fixtures_clone$`CS_0-1`, digits = 3)
# F2_fixtures_clone$`CS_2-0` <- round(1/F2_fixtures_clone$`CS_2-0`, digits = 3)
# F2_fixtures_clone$`CS_0-2` <- round(1/F2_fixtures_clone$`CS_0-2`, digits = 3)
# F2_fixtures_clone$`CS_2-1` <- round(1/F2_fixtures_clone$`CS_2-1`, digits = 3)
# F2_fixtures_clone$`CS_1-2` <- round(1/F2_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(F2_fixtures_clone)[1] <- "league"
# colnames(F2_fixtures_clone)[2] <- "Hometeam"
# colnames(F2_fixtures_clone)[3] <- "Awayteam"
# colnames(F2_fixtures_clone)[92] <- "predscore"
# colnames(F2_fixtures_clone)[64] <- "ov25"
# colnames(F2_fixtures_clone)[66] <- "ov25odds"
# colnames(F2_fixtures_clone)[65] <- "un25"
# colnames(F2_fixtures_clone)[67] <- "un25odds"
# colnames(F2_fixtures_clone)[68] <- "BTTSY"
# colnames(F2_fixtures_clone)[69] <- "BTTSN"
# colnames(F2_fixtures_clone)[70] <- "BTTSYodds"
# colnames(F2_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# F2_fixtures_clone <- F2_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# F2_fixtures_clone$matchid <- paste(F2_fixtures_clone$Hometeam,F2_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #G1
# G1_fixtures_clone <- G1_fixtures
# colnames(G1_fixtures_clone)[61] <- "Hwin"
# colnames(G1_fixtures_clone)[62] <- "Draw"
# colnames(G1_fixtures_clone)[63] <- "Awin"
#
# G1_fixtures_clone$Hwinodds <-   G1_fixtures$g1_1_0 + G1_fixtures$g1_2_0 + G1_fixtures$g1_2_1 + G1_fixtures$g1_3_0 + G1_fixtures$g1_3_1 +
#   G1_fixtures$g1_3_2 + G1_fixtures$g1_4_0 + G1_fixtures$g1_4_1 + G1_fixtures$g1_4_2 + G1_fixtures$g1_4_3 +
#   G1_fixtures$g1_5_0 + G1_fixtures$g1_5_1 + G1_fixtures$g1_5_2 + G1_fixtures$g1_5_3 + G1_fixtures$g1_5_4 +
#   G1_fixtures$g1_6_0 + G1_fixtures$g1_6_1 + G1_fixtures$g1_6_2 + G1_fixtures$g1_6_3 + G1_fixtures$g1_6_4 +
#   G1_fixtures$g1_6_5
# G1_fixtures_clone$Hwinodds <- round(1/G1_fixtures_clone$Hwinodds, digits = 3)
#
# G1_fixtures_clone$Drawodds <-  G1_fixtures$g1_0_0 + G1_fixtures$g1_1_1 + G1_fixtures$g1_2_2 + G1_fixtures$g1_3_3 + G1_fixtures$g1_4_4 +
#   G1_fixtures$g1_5_5 + G1_fixtures$g1_6_6
#
# G1_fixtures_clone$Drawodds <- round(1/G1_fixtures_clone$Drawodds, digits = 3)
#
# G1_fixtures_clone$Awinodds <-   G1_fixtures$g1_0_1 + G1_fixtures$g1_0_2 + G1_fixtures$g1_1_2 + G1_fixtures$g1_0_3 + G1_fixtures$g1_1_3 +
#   G1_fixtures$g1_2_3 + G1_fixtures$g1_0_4 + G1_fixtures$g1_1_4 + G1_fixtures$g1_2_4 + G1_fixtures$g1_3_4 +
#   G1_fixtures$g1_0_5 + G1_fixtures$g1_1_5 + G1_fixtures$g1_2_5 + G1_fixtures$g1_3_5 + G1_fixtures$g1_4_5 +
#   G1_fixtures$g1_0_6 + G1_fixtures$g1_1_6 + G1_fixtures$g1_2_6 + G1_fixtures$g1_3_6 + G1_fixtures$g1_4_6 +
#   G1_fixtures$g1_5_6
#
# G1_fixtures_clone$Awinodds <- round(1/G1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(G1_fixtures_clone)[15] <- "CS_1-1"
# colnames(G1_fixtures_clone)[13] <- "CS_1-0"
# colnames(G1_fixtures_clone)[14] <- "CS_0-1"
# colnames(G1_fixtures_clone)[16] <- "CS_2-0"
# colnames(G1_fixtures_clone)[17] <- "CS_0-2"
# colnames(G1_fixtures_clone)[19] <- "CS_2-1"
# colnames(G1_fixtures_clone)[20] <- "CS_1-2"
#
# G1_fixtures_clone$`CS_1-1` <- round(1/G1_fixtures_clone$`CS_1-1`, digits = 3)
# G1_fixtures_clone$`CS_1-0` <- round(1/G1_fixtures_clone$`CS_1-0`, digits = 3)
# G1_fixtures_clone$`CS_0-1` <- round(1/G1_fixtures_clone$`CS_0-1`, digits = 3)
# G1_fixtures_clone$`CS_2-0` <- round(1/G1_fixtures_clone$`CS_2-0`, digits = 3)
# G1_fixtures_clone$`CS_0-2` <- round(1/G1_fixtures_clone$`CS_0-2`, digits = 3)
# G1_fixtures_clone$`CS_2-1` <- round(1/G1_fixtures_clone$`CS_2-1`, digits = 3)
# G1_fixtures_clone$`CS_1-2` <- round(1/G1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(G1_fixtures_clone)[1] <- "league"
# colnames(G1_fixtures_clone)[2] <- "Hometeam"
# colnames(G1_fixtures_clone)[3] <- "Awayteam"
# colnames(G1_fixtures_clone)[92] <- "predscore"
# colnames(G1_fixtures_clone)[64] <- "ov25"
# colnames(G1_fixtures_clone)[66] <- "ov25odds"
# colnames(G1_fixtures_clone)[65] <- "un25"
# colnames(G1_fixtures_clone)[67] <- "un25odds"
# colnames(G1_fixtures_clone)[68] <- "BTTSY"
# colnames(G1_fixtures_clone)[69] <- "BTTSN"
# colnames(G1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(G1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# G1_fixtures_clone <- G1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# G1_fixtures_clone$matchid <- paste(G1_fixtures_clone$Hometeam,G1_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# I1_fixtures_clone <- I1_fixtures
# colnames(I1_fixtures_clone)[61] <- "Hwin"
# colnames(I1_fixtures_clone)[62] <- "Draw"
# colnames(I1_fixtures_clone)[63] <- "Awin"
#
# I1_fixtures_clone$Hwinodds <-   I1_fixtures$i1_1_0 + I1_fixtures$i1_2_0 + I1_fixtures$i1_2_1 + I1_fixtures$i1_3_0 + I1_fixtures$i1_3_1 +
#   I1_fixtures$i1_3_2 + I1_fixtures$i1_4_0 + I1_fixtures$i1_4_1 + I1_fixtures$i1_4_2 + I1_fixtures$i1_4_3 +
#   I1_fixtures$i1_5_0 + I1_fixtures$i1_5_1 + I1_fixtures$i1_5_2 + I1_fixtures$i1_5_3 + I1_fixtures$i1_5_4 +
#   I1_fixtures$i1_6_0 + I1_fixtures$i1_6_1 + I1_fixtures$i1_6_2 + I1_fixtures$i1_6_3 + I1_fixtures$i1_6_4 +
#   I1_fixtures$i1_6_5
# I1_fixtures_clone$Hwinodds <- round(1/I1_fixtures_clone$Hwinodds, digits = 3)
#
# I1_fixtures_clone$Drawodds <-  I1_fixtures$i1_0_0 + I1_fixtures$i1_1_1 + I1_fixtures$i1_2_2 + I1_fixtures$i1_3_3 + I1_fixtures$i1_4_4 +
#   I1_fixtures$i1_5_5 + I1_fixtures$i1_6_6
#
# I1_fixtures_clone$Drawodds <- round(1/I1_fixtures_clone$Drawodds, digits = 3)
#
# I1_fixtures_clone$Awinodds <-   I1_fixtures$i1_0_1 + I1_fixtures$i1_0_2 + I1_fixtures$i1_1_2 + I1_fixtures$i1_0_3 + I1_fixtures$i1_1_3 +
#   I1_fixtures$i1_2_3 + I1_fixtures$i1_0_4 + I1_fixtures$i1_1_4 + I1_fixtures$i1_2_4 + I1_fixtures$i1_3_4 +
#   I1_fixtures$i1_0_5 + I1_fixtures$i1_1_5 + I1_fixtures$i1_2_5 + I1_fixtures$i1_3_5 + I1_fixtures$i1_4_5 +
#   I1_fixtures$i1_0_6 + I1_fixtures$i1_1_6 + I1_fixtures$i1_2_6 + I1_fixtures$i1_3_6 + I1_fixtures$i1_4_6 +
#   I1_fixtures$i1_5_6
#
# I1_fixtures_clone$Awinodds <- round(1/I1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(I1_fixtures_clone)[15] <- "CS_1-1"
# colnames(I1_fixtures_clone)[13] <- "CS_1-0"
# colnames(I1_fixtures_clone)[14] <- "CS_0-1"
# colnames(I1_fixtures_clone)[16] <- "CS_2-0"
# colnames(I1_fixtures_clone)[17] <- "CS_0-2"
# colnames(I1_fixtures_clone)[19] <- "CS_2-1"
# colnames(I1_fixtures_clone)[20] <- "CS_1-2"
#
# I1_fixtures_clone$`CS_1-1` <- round(1/I1_fixtures_clone$`CS_1-1`, digits = 3)
# I1_fixtures_clone$`CS_1-0` <- round(1/I1_fixtures_clone$`CS_1-0`, digits = 3)
# I1_fixtures_clone$`CS_0-1` <- round(1/I1_fixtures_clone$`CS_0-1`, digits = 3)
# I1_fixtures_clone$`CS_2-0` <- round(1/I1_fixtures_clone$`CS_2-0`, digits = 3)
# I1_fixtures_clone$`CS_0-2` <- round(1/I1_fixtures_clone$`CS_0-2`, digits = 3)
# I1_fixtures_clone$`CS_2-1` <- round(1/I1_fixtures_clone$`CS_2-1`, digits = 3)
# I1_fixtures_clone$`CS_1-2` <- round(1/I1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(I1_fixtures_clone)[1] <- "league"
# colnames(I1_fixtures_clone)[2] <- "Hometeam"
# colnames(I1_fixtures_clone)[3] <- "Awayteam"
# colnames(I1_fixtures_clone)[92] <- "predscore"
# colnames(I1_fixtures_clone)[64] <- "ov25"
# colnames(I1_fixtures_clone)[66] <- "ov25odds"
# colnames(I1_fixtures_clone)[65] <- "un25"
# colnames(I1_fixtures_clone)[67] <- "un25odds"
# colnames(I1_fixtures_clone)[68] <- "BTTSY"
# colnames(I1_fixtures_clone)[69] <- "BTTSN"
# colnames(I1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(I1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# I1_fixtures_clone <- I1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# I1_fixtures_clone$matchid <- paste(I1_fixtures_clone$Hometeam,I1_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #I2
# I2_fixtures_clone <- I2_fixtures
# colnames(I2_fixtures_clone)[61] <- "Hwin"
# colnames(I2_fixtures_clone)[62] <- "Draw"
# colnames(I2_fixtures_clone)[63] <- "Awin"
#
# I2_fixtures_clone$Hwinodds <-   I2_fixtures$i2_1_0 + I2_fixtures$i2_2_0 + I2_fixtures$i2_2_1 + I2_fixtures$i2_3_0 + I2_fixtures$i2_3_1 +
#   I2_fixtures$i2_3_2 + I2_fixtures$i2_4_0 + I2_fixtures$i2_4_1 + I2_fixtures$i2_4_2 + I2_fixtures$i2_4_3 +
#   I2_fixtures$i2_5_0 + I2_fixtures$i2_5_1 + I2_fixtures$i2_5_2 + I2_fixtures$i2_5_3 + I2_fixtures$i2_5_4 +
#   I2_fixtures$i2_6_0 + I2_fixtures$i2_6_1 + I2_fixtures$i2_6_2 + I2_fixtures$i2_6_3 + I2_fixtures$i2_6_4 +
#   I2_fixtures$i2_6_5
# I2_fixtures_clone$Hwinodds <- round(1/I2_fixtures_clone$Hwinodds, digits = 3)
#
# I2_fixtures_clone$Drawodds <-  I2_fixtures$i2_0_0 + I2_fixtures$i2_1_1 + I2_fixtures$i2_2_2 + I2_fixtures$i2_3_3 + I2_fixtures$i2_4_4 +
#   I2_fixtures$i2_5_5 + I2_fixtures$i2_6_6
#
# I2_fixtures_clone$Drawodds <- round(1/I2_fixtures_clone$Drawodds, digits = 3)
#
# I2_fixtures_clone$Awinodds <-   I2_fixtures$i2_0_1 + I2_fixtures$i2_0_2 + I2_fixtures$i2_1_2 + I2_fixtures$i2_0_3 + I2_fixtures$i2_1_3 +
#   I2_fixtures$i2_2_3 + I2_fixtures$i2_0_4 + I2_fixtures$i2_1_4 + I2_fixtures$i2_2_4 + I2_fixtures$i2_3_4 +
#   I2_fixtures$i2_0_5 + I2_fixtures$i2_1_5 + I2_fixtures$i2_2_5 + I2_fixtures$i2_3_5 + I2_fixtures$i2_4_5 +
#   I2_fixtures$i2_0_6 + I2_fixtures$i2_1_6 + I2_fixtures$i2_2_6 + I2_fixtures$i2_3_6 + I2_fixtures$i2_4_6 +
#   I2_fixtures$i2_5_6
#
# I2_fixtures_clone$Awinodds <- round(1/I2_fixtures_clone$Awinodds, digits = 3)
#
# colnames(I2_fixtures_clone)[15] <- "CS_1-1"
# colnames(I2_fixtures_clone)[13] <- "CS_1-0"
# colnames(I2_fixtures_clone)[14] <- "CS_0-1"
# colnames(I2_fixtures_clone)[16] <- "CS_2-0"
# colnames(I2_fixtures_clone)[17] <- "CS_0-2"
# colnames(I2_fixtures_clone)[19] <- "CS_2-1"
# colnames(I2_fixtures_clone)[20] <- "CS_1-2"
#
# I2_fixtures_clone$`CS_1-1` <- round(1/I2_fixtures_clone$`CS_1-1`, digits = 3)
# I2_fixtures_clone$`CS_1-0` <- round(1/I2_fixtures_clone$`CS_1-0`, digits = 3)
# I2_fixtures_clone$`CS_0-1` <- round(1/I2_fixtures_clone$`CS_0-1`, digits = 3)
# I2_fixtures_clone$`CS_2-0` <- round(1/I2_fixtures_clone$`CS_2-0`, digits = 3)
# I2_fixtures_clone$`CS_0-2` <- round(1/I2_fixtures_clone$`CS_0-2`, digits = 3)
# I2_fixtures_clone$`CS_2-1` <- round(1/I2_fixtures_clone$`CS_2-1`, digits = 3)
# I2_fixtures_clone$`CS_1-2` <- round(1/I2_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(I2_fixtures_clone)[1] <- "league"
# colnames(I2_fixtures_clone)[2] <- "Hometeam"
# colnames(I2_fixtures_clone)[3] <- "Awayteam"
# colnames(I2_fixtures_clone)[92] <- "predscore"
# colnames(I2_fixtures_clone)[64] <- "ov25"
# colnames(I2_fixtures_clone)[66] <- "ov25odds"
# colnames(I2_fixtures_clone)[65] <- "un25"
# colnames(I2_fixtures_clone)[67] <- "un25odds"
# colnames(I2_fixtures_clone)[68] <- "BTTSY"
# colnames(I2_fixtures_clone)[69] <- "BTTSN"
# colnames(I2_fixtures_clone)[70] <- "BTTSYodds"
# colnames(I2_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# I2_fixtures_clone <- I2_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# I2_fixtures_clone$matchid <- paste(I2_fixtures_clone$Hometeam,I2_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #N1
# N1_fixtures_clone <- N1_fixtures
# colnames(N1_fixtures_clone)[61] <- "Hwin"
# colnames(N1_fixtures_clone)[62] <- "Draw"
# colnames(N1_fixtures_clone)[63] <- "Awin"
#
# N1_fixtures_clone$Hwinodds <-   N1_fixtures$n1_1_0 + N1_fixtures$n1_2_0 + N1_fixtures$n1_2_1 + N1_fixtures$n1_3_0 + N1_fixtures$n1_3_1 +
#   N1_fixtures$n1_3_2 + N1_fixtures$n1_4_0 + N1_fixtures$n1_4_1 + N1_fixtures$n1_4_2 + N1_fixtures$n1_4_3 +
#   N1_fixtures$n1_5_0 + N1_fixtures$n1_5_1 + N1_fixtures$n1_5_2 + N1_fixtures$n1_5_3 + N1_fixtures$n1_5_4 +
#   N1_fixtures$n1_6_0 + N1_fixtures$n1_6_1 + N1_fixtures$n1_6_2 + N1_fixtures$n1_6_3 + N1_fixtures$n1_6_4 +
#   N1_fixtures$n1_6_5
# N1_fixtures_clone$Hwinodds <- round(1/N1_fixtures_clone$Hwinodds, digits = 3)
#
# N1_fixtures_clone$Drawodds <-  N1_fixtures$n1_0_0 + N1_fixtures$n1_1_1 + N1_fixtures$n1_2_2 + N1_fixtures$n1_3_3 + N1_fixtures$n1_4_4 +
#   N1_fixtures$n1_5_5 + N1_fixtures$n1_6_6
#
# N1_fixtures_clone$Drawodds <- round(1/N1_fixtures_clone$Drawodds, digits = 3)
#
# N1_fixtures_clone$Awinodds <-   N1_fixtures$n1_0_1 + N1_fixtures$n1_0_2 + N1_fixtures$n1_1_2 + N1_fixtures$n1_0_3 + N1_fixtures$n1_1_3 +
#   N1_fixtures$n1_2_3 + N1_fixtures$n1_0_4 + N1_fixtures$n1_1_4 + N1_fixtures$n1_2_4 + N1_fixtures$n1_3_4 +
#   N1_fixtures$n1_0_5 + N1_fixtures$n1_1_5 + N1_fixtures$n1_2_5 + N1_fixtures$n1_3_5 + N1_fixtures$n1_4_5 +
#   N1_fixtures$n1_0_6 + N1_fixtures$n1_1_6 + N1_fixtures$n1_2_6 + N1_fixtures$n1_3_6 + N1_fixtures$n1_4_6 +
#   N1_fixtures$n1_5_6
#
# N1_fixtures_clone$Awinodds <- round(1/N1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(N1_fixtures_clone)[15] <- "CS_1-1"
# colnames(N1_fixtures_clone)[13] <- "CS_1-0"
# colnames(N1_fixtures_clone)[14] <- "CS_0-1"
# colnames(N1_fixtures_clone)[16] <- "CS_2-0"
# colnames(N1_fixtures_clone)[17] <- "CS_0-2"
# colnames(N1_fixtures_clone)[19] <- "CS_2-1"
# colnames(N1_fixtures_clone)[20] <- "CS_1-2"
#
# N1_fixtures_clone$`CS_1-1` <- round(1/N1_fixtures_clone$`CS_1-1`, digits = 3)
# N1_fixtures_clone$`CS_1-0` <- round(1/N1_fixtures_clone$`CS_1-0`, digits = 3)
# N1_fixtures_clone$`CS_0-1` <- round(1/N1_fixtures_clone$`CS_0-1`, digits = 3)
# N1_fixtures_clone$`CS_2-0` <- round(1/N1_fixtures_clone$`CS_2-0`, digits = 3)
# N1_fixtures_clone$`CS_0-2` <- round(1/N1_fixtures_clone$`CS_0-2`, digits = 3)
# N1_fixtures_clone$`CS_2-1` <- round(1/N1_fixtures_clone$`CS_2-1`, digits = 3)
# N1_fixtures_clone$`CS_1-2` <- round(1/N1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(N1_fixtures_clone)[1] <- "league"
# colnames(N1_fixtures_clone)[2] <- "Hometeam"
# colnames(N1_fixtures_clone)[3] <- "Awayteam"
# colnames(N1_fixtures_clone)[92] <- "predscore"
# colnames(N1_fixtures_clone)[64] <- "ov25"
# colnames(N1_fixtures_clone)[66] <- "ov25odds"
# colnames(N1_fixtures_clone)[65] <- "un25"
# colnames(N1_fixtures_clone)[67] <- "un25odds"
# colnames(N1_fixtures_clone)[68] <- "BTTSY"
# colnames(N1_fixtures_clone)[69] <- "BTTSN"
# colnames(N1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(N1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# N1_fixtures_clone <- N1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# N1_fixtures_clone$matchid <- paste(N1_fixtures_clone$Hometeam,N1_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #P1
# P1_fixtures_clone <- P1_fixtures
# colnames(P1_fixtures_clone)[61] <- "Hwin"
# colnames(P1_fixtures_clone)[62] <- "Draw"
# colnames(P1_fixtures_clone)[63] <- "Awin"
#
# P1_fixtures_clone$Hwinodds <-   P1_fixtures$p1_1_0 + P1_fixtures$p1_2_0 + P1_fixtures$p1_2_1 + P1_fixtures$p1_3_0 + P1_fixtures$p1_3_1 +
#   P1_fixtures$p1_3_2 + P1_fixtures$p1_4_0 + P1_fixtures$p1_4_1 + P1_fixtures$p1_4_2 + P1_fixtures$p1_4_3 +
#   P1_fixtures$p1_5_0 + P1_fixtures$p1_5_1 + P1_fixtures$p1_5_2 + P1_fixtures$p1_5_3 + P1_fixtures$p1_5_4 +
#   P1_fixtures$p1_6_0 + P1_fixtures$p1_6_1 + P1_fixtures$p1_6_2 + P1_fixtures$p1_6_3 + P1_fixtures$p1_6_4 +
#   P1_fixtures$p1_6_5
# P1_fixtures_clone$Hwinodds <- round(1/P1_fixtures_clone$Hwinodds, digits = 3)
#
# P1_fixtures_clone$Drawodds <-  P1_fixtures$p1_0_0 + P1_fixtures$p1_1_1 + P1_fixtures$p1_2_2 + P1_fixtures$p1_3_3 + P1_fixtures$p1_4_4 +
#   P1_fixtures$p1_5_5 + P1_fixtures$p1_6_6
#
# P1_fixtures_clone$Drawodds <- round(1/P1_fixtures_clone$Drawodds, digits = 3)
#
# P1_fixtures_clone$Awinodds <-   P1_fixtures$p1_0_1 + P1_fixtures$p1_0_2 + P1_fixtures$p1_1_2 + P1_fixtures$p1_0_3 + P1_fixtures$p1_1_3 +
#   P1_fixtures$p1_2_3 + P1_fixtures$p1_0_4 + P1_fixtures$p1_1_4 + P1_fixtures$p1_2_4 + P1_fixtures$p1_3_4 +
#   P1_fixtures$p1_0_5 + P1_fixtures$p1_1_5 + P1_fixtures$p1_2_5 + P1_fixtures$p1_3_5 + P1_fixtures$p1_4_5 +
#   P1_fixtures$p1_0_6 + P1_fixtures$p1_1_6 + P1_fixtures$p1_2_6 + P1_fixtures$p1_3_6 + P1_fixtures$p1_4_6 +
#   P1_fixtures$p1_5_6
#
# P1_fixtures_clone$Awinodds <- round(1/P1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(P1_fixtures_clone)[15] <- "CS_1-1"
# colnames(P1_fixtures_clone)[13] <- "CS_1-0"
# colnames(P1_fixtures_clone)[14] <- "CS_0-1"
# colnames(P1_fixtures_clone)[16] <- "CS_2-0"
# colnames(P1_fixtures_clone)[17] <- "CS_0-2"
# colnames(P1_fixtures_clone)[19] <- "CS_2-1"
# colnames(P1_fixtures_clone)[20] <- "CS_1-2"
#
# P1_fixtures_clone$`CS_1-1` <- round(1/P1_fixtures_clone$`CS_1-1`, digits = 3)
# P1_fixtures_clone$`CS_1-0` <- round(1/P1_fixtures_clone$`CS_1-0`, digits = 3)
# P1_fixtures_clone$`CS_0-1` <- round(1/P1_fixtures_clone$`CS_0-1`, digits = 3)
# P1_fixtures_clone$`CS_2-0` <- round(1/P1_fixtures_clone$`CS_2-0`, digits = 3)
# P1_fixtures_clone$`CS_0-2` <- round(1/P1_fixtures_clone$`CS_0-2`, digits = 3)
# P1_fixtures_clone$`CS_2-1` <- round(1/P1_fixtures_clone$`CS_2-1`, digits = 3)
# P1_fixtures_clone$`CS_1-2` <- round(1/P1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(P1_fixtures_clone)[1] <- "league"
# colnames(P1_fixtures_clone)[2] <- "Hometeam"
# colnames(P1_fixtures_clone)[3] <- "Awayteam"
# colnames(P1_fixtures_clone)[92] <- "predscore"
# colnames(P1_fixtures_clone)[64] <- "ov25"
# colnames(P1_fixtures_clone)[66] <- "ov25odds"
# colnames(P1_fixtures_clone)[65] <- "un25"
# colnames(P1_fixtures_clone)[67] <- "un25odds"
# colnames(P1_fixtures_clone)[68] <- "BTTSY"
# colnames(P1_fixtures_clone)[69] <- "BTTSN"
# colnames(P1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(P1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# P1_fixtures_clone <- P1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# P1_fixtures_clone$matchid <- paste(P1_fixtures_clone$Hometeam,P1_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #SP1
# SP1_fixtures_clone <- SP1_fixtures
# colnames(SP1_fixtures_clone)[61] <- "Hwin"
# colnames(SP1_fixtures_clone)[62] <- "Draw"
# colnames(SP1_fixtures_clone)[63] <- "Awin"
#
# SP1_fixtures_clone$Hwinodds <-   SP1_fixtures$sp1_1_0 + SP1_fixtures$sp1_2_0 + SP1_fixtures$sp1_2_1 + SP1_fixtures$sp1_3_0 + SP1_fixtures$sp1_3_1 +
#   SP1_fixtures$sp1_3_2 + SP1_fixtures$sp1_4_0 + SP1_fixtures$sp1_4_1 + SP1_fixtures$sp1_4_2 + SP1_fixtures$sp1_4_3 +
#   SP1_fixtures$sp1_5_0 + SP1_fixtures$sp1_5_1 + SP1_fixtures$sp1_5_2 + SP1_fixtures$sp1_5_3 + SP1_fixtures$sp1_5_4 +
#   SP1_fixtures$sp1_6_0 + SP1_fixtures$sp1_6_1 + SP1_fixtures$sp1_6_2 + SP1_fixtures$sp1_6_3 + SP1_fixtures$sp1_6_4 +
#   SP1_fixtures$sp1_6_5
# SP1_fixtures_clone$Hwinodds <- round(1/SP1_fixtures_clone$Hwinodds, digits = 3)
#
# SP1_fixtures_clone$Drawodds <-  SP1_fixtures$sp1_0_0 + SP1_fixtures$sp1_1_1 + SP1_fixtures$sp1_2_2 + SP1_fixtures$sp1_3_3 + SP1_fixtures$sp1_4_4 +
#   SP1_fixtures$sp1_5_5 + SP1_fixtures$sp1_6_6
#
# SP1_fixtures_clone$Drawodds <- round(1/SP1_fixtures_clone$Drawodds, digits = 3)
#
# SP1_fixtures_clone$Awinodds <-   SP1_fixtures$sp1_0_1 + SP1_fixtures$sp1_0_2 + SP1_fixtures$sp1_1_2 + SP1_fixtures$sp1_0_3 + SP1_fixtures$sp1_1_3 +
#   SP1_fixtures$sp1_2_3 + SP1_fixtures$sp1_0_4 + SP1_fixtures$sp1_1_4 + SP1_fixtures$sp1_2_4 + SP1_fixtures$sp1_3_4 +
#   SP1_fixtures$sp1_0_5 + SP1_fixtures$sp1_1_5 + SP1_fixtures$sp1_2_5 + SP1_fixtures$sp1_3_5 + SP1_fixtures$sp1_4_5 +
#   SP1_fixtures$sp1_0_6 + SP1_fixtures$sp1_1_6 + SP1_fixtures$sp1_2_6 + SP1_fixtures$sp1_3_6 + SP1_fixtures$sp1_4_6 +
#   SP1_fixtures$sp1_5_6
#
# SP1_fixtures_clone$Awinodds <- round(1/SP1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(SP1_fixtures_clone)[15] <- "CS_1-1"
# colnames(SP1_fixtures_clone)[13] <- "CS_1-0"
# colnames(SP1_fixtures_clone)[14] <- "CS_0-1"
# colnames(SP1_fixtures_clone)[16] <- "CS_2-0"
# colnames(SP1_fixtures_clone)[17] <- "CS_0-2"
# colnames(SP1_fixtures_clone)[19] <- "CS_2-1"
# colnames(SP1_fixtures_clone)[20] <- "CS_1-2"
#
# SP1_fixtures_clone$`CS_1-1` <- round(1/SP1_fixtures_clone$`CS_1-1`, digits = 3)
# SP1_fixtures_clone$`CS_1-0` <- round(1/SP1_fixtures_clone$`CS_1-0`, digits = 3)
# SP1_fixtures_clone$`CS_0-1` <- round(1/SP1_fixtures_clone$`CS_0-1`, digits = 3)
# SP1_fixtures_clone$`CS_2-0` <- round(1/SP1_fixtures_clone$`CS_2-0`, digits = 3)
# SP1_fixtures_clone$`CS_0-2` <- round(1/SP1_fixtures_clone$`CS_0-2`, digits = 3)
# SP1_fixtures_clone$`CS_2-1` <- round(1/SP1_fixtures_clone$`CS_2-1`, digits = 3)
# SP1_fixtures_clone$`CS_1-2` <- round(1/SP1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(SP1_fixtures_clone)[1] <- "league"
# colnames(SP1_fixtures_clone)[2] <- "Hometeam"
# colnames(SP1_fixtures_clone)[3] <- "Awayteam"
# colnames(SP1_fixtures_clone)[92] <- "predscore"
# colnames(SP1_fixtures_clone)[64] <- "ov25"
# colnames(SP1_fixtures_clone)[66] <- "ov25odds"
# colnames(SP1_fixtures_clone)[65] <- "un25"
# colnames(SP1_fixtures_clone)[67] <- "un25odds"
# colnames(SP1_fixtures_clone)[68] <- "BTTSY"
# colnames(SP1_fixtures_clone)[69] <- "BTTSN"
# colnames(SP1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(SP1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# SP1_fixtures_clone <- SP1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# SP1_fixtures_clone$matchid <- paste(SP1_fixtures_clone$Hometeam,SP1_fixtures_clone$Awayteam,sep = '-')
# ##########################################################################################################################
# #SP2
# SP2_fixtures_clone <- SP2_fixtures
# colnames(SP2_fixtures_clone)[61] <- "Hwin"
# colnames(SP2_fixtures_clone)[62] <- "Draw"
# colnames(SP2_fixtures_clone)[63] <- "Awin"
#
# SP2_fixtures_clone$Hwinodds <-   SP2_fixtures$sp2_1_0 + SP2_fixtures$sp2_2_0 + SP2_fixtures$sp2_2_1 + SP2_fixtures$sp2_3_0 + SP2_fixtures$sp2_3_1 +
#   SP2_fixtures$sp2_3_2 + SP2_fixtures$sp2_4_0 + SP2_fixtures$sp2_4_1 + SP2_fixtures$sp2_4_2 + SP2_fixtures$sp2_4_3 +
#   SP2_fixtures$sp2_5_0 + SP2_fixtures$sp2_5_1 + SP2_fixtures$sp2_5_2 + SP2_fixtures$sp2_5_3 + SP2_fixtures$sp2_5_4 +
#   SP2_fixtures$sp2_6_0 + SP2_fixtures$sp2_6_1 + SP2_fixtures$sp2_6_2 + SP2_fixtures$sp2_6_3 + SP2_fixtures$sp2_6_4 +
#   SP2_fixtures$sp2_6_5
# SP2_fixtures_clone$Hwinodds <- round(1/SP2_fixtures_clone$Hwinodds, digits = 3)
#
# SP2_fixtures_clone$Drawodds <-  SP2_fixtures$sp2_0_0 + SP2_fixtures$sp2_1_1 + SP2_fixtures$sp2_2_2 + SP2_fixtures$sp2_3_3 + SP2_fixtures$sp2_4_4 +
#   SP2_fixtures$sp2_5_5 + SP2_fixtures$sp2_6_6
#
# SP2_fixtures_clone$Drawodds <- round(1/SP2_fixtures_clone$Drawodds, digits = 3)
#
# SP2_fixtures_clone$Awinodds <-   SP2_fixtures$sp2_0_1 + SP2_fixtures$sp2_0_2 + SP2_fixtures$sp2_1_2 + SP2_fixtures$sp2_0_3 + SP2_fixtures$sp2_1_3 +
#   SP2_fixtures$sp2_2_3 + SP2_fixtures$sp2_0_4 + SP2_fixtures$sp2_1_4 + SP2_fixtures$sp2_2_4 + SP2_fixtures$sp2_3_4 +
#   SP2_fixtures$sp2_0_5 + SP2_fixtures$sp2_1_5 + SP2_fixtures$sp2_2_5 + SP2_fixtures$sp2_3_5 + SP2_fixtures$sp2_4_5 +
#   SP2_fixtures$sp2_0_6 + SP2_fixtures$sp2_1_6 + SP2_fixtures$sp2_2_6 + SP2_fixtures$sp2_3_6 + SP2_fixtures$sp2_4_6 +
#   SP2_fixtures$sp2_5_6
#
# SP2_fixtures_clone$Awinodds <- round(1/SP2_fixtures_clone$Awinodds, digits = 3)
#
# colnames(SP2_fixtures_clone)[15] <- "CS_1-1"
# colnames(SP2_fixtures_clone)[13] <- "CS_1-0"
# colnames(SP2_fixtures_clone)[14] <- "CS_0-1"
# colnames(SP2_fixtures_clone)[16] <- "CS_2-0"
# colnames(SP2_fixtures_clone)[17] <- "CS_0-2"
# colnames(SP2_fixtures_clone)[19] <- "CS_2-1"
# colnames(SP2_fixtures_clone)[20] <- "CS_1-2"
#
# SP2_fixtures_clone$`CS_1-1` <- round(1/SP2_fixtures_clone$`CS_1-1`, digits = 3)
# SP2_fixtures_clone$`CS_1-0` <- round(1/SP2_fixtures_clone$`CS_1-0`, digits = 3)
# SP2_fixtures_clone$`CS_0-1` <- round(1/SP2_fixtures_clone$`CS_0-1`, digits = 3)
# SP2_fixtures_clone$`CS_2-0` <- round(1/SP2_fixtures_clone$`CS_2-0`, digits = 3)
# SP2_fixtures_clone$`CS_0-2` <- round(1/SP2_fixtures_clone$`CS_0-2`, digits = 3)
# SP2_fixtures_clone$`CS_2-1` <- round(1/SP2_fixtures_clone$`CS_2-1`, digits = 3)
# SP2_fixtures_clone$`CS_1-2` <- round(1/SP2_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(SP2_fixtures_clone)[1] <- "league"
# colnames(SP2_fixtures_clone)[2] <- "Hometeam"
# colnames(SP2_fixtures_clone)[3] <- "Awayteam"
# colnames(SP2_fixtures_clone)[92] <- "predscore"
# colnames(SP2_fixtures_clone)[64] <- "ov25"
# colnames(SP2_fixtures_clone)[66] <- "ov25odds"
# colnames(SP2_fixtures_clone)[65] <- "un25"
# colnames(SP2_fixtures_clone)[67] <- "un25odds"
# colnames(SP2_fixtures_clone)[68] <- "BTTSY"
# colnames(SP2_fixtures_clone)[69] <- "BTTSN"
# colnames(SP2_fixtures_clone)[70] <- "BTTSYodds"
# colnames(SP2_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# SP2_fixtures_clone <- SP2_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# SP2_fixtures_clone$matchid <- paste(SP2_fixtures_clone$Hometeam,SP2_fixtures_clone$Awayteam,sep = '-')
# ##########################################################################################################################
# SC0_fixtures_clone <- SC0_fixtures
# colnames(SC0_fixtures_clone)[61] <- "Hwin"
# colnames(SC0_fixtures_clone)[62] <- "Draw"
# colnames(SC0_fixtures_clone)[63] <- "Awin"
#
# SC0_fixtures_clone$Hwinodds <-   SC0_fixtures$sc0_1_0 + SC0_fixtures$sc0_2_0 + SC0_fixtures$sc0_2_1 + SC0_fixtures$sc0_3_0 + SC0_fixtures$sc0_3_1 +
#   SC0_fixtures$sc0_3_2 + SC0_fixtures$sc0_4_0 + SC0_fixtures$sc0_4_1 + SC0_fixtures$sc0_4_2 + SC0_fixtures$sc0_4_3 +
#   SC0_fixtures$sc0_5_0 + SC0_fixtures$sc0_5_1 + SC0_fixtures$sc0_5_2 + SC0_fixtures$sc0_5_3 + SC0_fixtures$sc0_5_4 +
#   SC0_fixtures$sc0_6_0 + SC0_fixtures$sc0_6_1 + SC0_fixtures$sc0_6_2 + SC0_fixtures$sc0_6_3 + SC0_fixtures$sc0_6_4 +
#   SC0_fixtures$sc0_6_5
# SC0_fixtures_clone$Hwinodds <- round(1/SC0_fixtures_clone$Hwinodds, digits = 3)
#
# SC0_fixtures_clone$Drawodds <-  SC0_fixtures$sc0_0_0 + SC0_fixtures$sc0_1_1 + SC0_fixtures$sc0_2_2 + SC0_fixtures$sc0_3_3 + SC0_fixtures$sc0_4_4 +
#   SC0_fixtures$sc0_5_5 + SC0_fixtures$sc0_6_6
#
# SC0_fixtures_clone$Drawodds <- round(1/SC0_fixtures_clone$Drawodds, digits = 3)
#
# SC0_fixtures_clone$Awinodds <-   SC0_fixtures$sc0_0_1 + SC0_fixtures$sc0_0_2 + SC0_fixtures$sc0_1_2 + SC0_fixtures$sc0_0_3 + SC0_fixtures$sc0_1_3 +
#   SC0_fixtures$sc0_2_3 + SC0_fixtures$sc0_0_4 + SC0_fixtures$sc0_1_4 + SC0_fixtures$sc0_2_4 + SC0_fixtures$sc0_3_4 +
#   SC0_fixtures$sc0_0_5 + SC0_fixtures$sc0_1_5 + SC0_fixtures$sc0_2_5 + SC0_fixtures$sc0_3_5 + SC0_fixtures$sc0_4_5 +
#   SC0_fixtures$sc0_0_6 + SC0_fixtures$sc0_1_6 + SC0_fixtures$sc0_2_6 + SC0_fixtures$sc0_3_6 + SC0_fixtures$sc0_4_6 +
#   SC0_fixtures$sc0_5_6
#
# SC0_fixtures_clone$Awinodds <- round(1/SC0_fixtures_clone$Awinodds, digits = 3)
#
# colnames(SC0_fixtures_clone)[15] <- "CS_1-1"
# colnames(SC0_fixtures_clone)[13] <- "CS_1-0"
# colnames(SC0_fixtures_clone)[14] <- "CS_0-1"
# colnames(SC0_fixtures_clone)[16] <- "CS_2-0"
# colnames(SC0_fixtures_clone)[17] <- "CS_0-2"
# colnames(SC0_fixtures_clone)[19] <- "CS_2-1"
# colnames(SC0_fixtures_clone)[20] <- "CS_1-2"
#
# SC0_fixtures_clone$`CS_1-1` <- round(1/SC0_fixtures_clone$`CS_1-1`, digits = 3)
# SC0_fixtures_clone$`CS_1-0` <- round(1/SC0_fixtures_clone$`CS_1-0`, digits = 3)
# SC0_fixtures_clone$`CS_0-1` <- round(1/SC0_fixtures_clone$`CS_0-1`, digits = 3)
# SC0_fixtures_clone$`CS_2-0` <- round(1/SC0_fixtures_clone$`CS_2-0`, digits = 3)
# SC0_fixtures_clone$`CS_0-2` <- round(1/SC0_fixtures_clone$`CS_0-2`, digits = 3)
# SC0_fixtures_clone$`CS_2-1` <- round(1/SC0_fixtures_clone$`CS_2-1`, digits = 3)
# SC0_fixtures_clone$`CS_1-2` <- round(1/SC0_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(SC0_fixtures_clone)[1] <- "league"
# colnames(SC0_fixtures_clone)[2] <- "Hometeam"
# colnames(SC0_fixtures_clone)[3] <- "Awayteam"
# colnames(SC0_fixtures_clone)[92] <- "predscore"
# colnames(SC0_fixtures_clone)[64] <- "ov25"
# colnames(SC0_fixtures_clone)[66] <- "ov25odds"
# colnames(SC0_fixtures_clone)[65] <- "un25"
# colnames(SC0_fixtures_clone)[67] <- "un25odds"
# colnames(SC0_fixtures_clone)[68] <- "BTTSY"
# colnames(SC0_fixtures_clone)[69] <- "BTTSN"
# colnames(SC0_fixtures_clone)[70] <- "BTTSYodds"
# colnames(SC0_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# SC0_fixtures_clone <- SC0_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# SC0_fixtures_clone$matchid <- paste(SC0_fixtures_clone$Hometeam,SC0_fixtures_clone$Awayteam,sep = '-')
# #########################################################################################################################
# #SC1
# SC1_fixtures_clone <- SC1_fixtures
# colnames(SC1_fixtures_clone)[61] <- "Hwin"
# colnames(SC1_fixtures_clone)[62] <- "Draw"
# colnames(SC1_fixtures_clone)[63] <- "Awin"
#
# SC1_fixtures_clone$Hwinodds <-   SC1_fixtures$sc1_1_0 + SC1_fixtures$sc1_2_0 + SC1_fixtures$sc1_2_1 + SC1_fixtures$sc1_3_0 + SC1_fixtures$sc1_3_1 +
#   SC1_fixtures$sc1_3_2 + SC1_fixtures$sc1_4_0 + SC1_fixtures$sc1_4_1 + SC1_fixtures$sc1_4_2 + SC1_fixtures$sc1_4_3 +
#   SC1_fixtures$sc1_5_0 + SC1_fixtures$sc1_5_1 + SC1_fixtures$sc1_5_2 + SC1_fixtures$sc1_5_3 + SC1_fixtures$sc1_5_4 +
#   SC1_fixtures$sc1_6_0 + SC1_fixtures$sc1_6_1 + SC1_fixtures$sc1_6_2 + SC1_fixtures$sc1_6_3 + SC1_fixtures$sc1_6_4 +
#   SC1_fixtures$sc1_6_5
# SC1_fixtures_clone$Hwinodds <- round(1/SC1_fixtures_clone$Hwinodds, digits = 3)
#
# SC1_fixtures_clone$Drawodds <-  SC1_fixtures$sc1_0_0 + SC1_fixtures$sc1_1_1 + SC1_fixtures$sc1_2_2 + SC1_fixtures$sc1_3_3 + SC1_fixtures$sc1_4_4 +
#   SC1_fixtures$sc1_5_5 + SC1_fixtures$sc1_6_6
#
# SC1_fixtures_clone$Drawodds <- round(1/SC1_fixtures_clone$Drawodds, digits = 3)
#
# SC1_fixtures_clone$Awinodds <-   SC1_fixtures$sc1_0_1 + SC1_fixtures$sc1_0_2 + SC1_fixtures$sc1_1_2 + SC1_fixtures$sc1_0_3 + SC1_fixtures$sc1_1_3 +
#   SC1_fixtures$sc1_2_3 + SC1_fixtures$sc1_0_4 + SC1_fixtures$sc1_1_4 + SC1_fixtures$sc1_2_4 + SC1_fixtures$sc1_3_4 +
#   SC1_fixtures$sc1_0_5 + SC1_fixtures$sc1_1_5 + SC1_fixtures$sc1_2_5 + SC1_fixtures$sc1_3_5 + SC1_fixtures$sc1_4_5 +
#   SC1_fixtures$sc1_0_6 + SC1_fixtures$sc1_1_6 + SC1_fixtures$sc1_2_6 + SC1_fixtures$sc1_3_6 + SC1_fixtures$sc1_4_6 +
#   SC1_fixtures$sc1_5_6
#
# SC1_fixtures_clone$Awinodds <- round(1/SC1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(SC1_fixtures_clone)[15] <- "CS_1-1"
# colnames(SC1_fixtures_clone)[13] <- "CS_1-0"
# colnames(SC1_fixtures_clone)[14] <- "CS_0-1"
# colnames(SC1_fixtures_clone)[16] <- "CS_2-0"
# colnames(SC1_fixtures_clone)[17] <- "CS_0-2"
# colnames(SC1_fixtures_clone)[19] <- "CS_2-1"
# colnames(SC1_fixtures_clone)[20] <- "CS_1-2"
#
# SC1_fixtures_clone$`CS_1-1` <- round(1/SC1_fixtures_clone$`CS_1-1`, digits = 3)
# SC1_fixtures_clone$`CS_1-0` <- round(1/SC1_fixtures_clone$`CS_1-0`, digits = 3)
# SC1_fixtures_clone$`CS_0-1` <- round(1/SC1_fixtures_clone$`CS_0-1`, digits = 3)
# SC1_fixtures_clone$`CS_2-0` <- round(1/SC1_fixtures_clone$`CS_2-0`, digits = 3)
# SC1_fixtures_clone$`CS_0-2` <- round(1/SC1_fixtures_clone$`CS_0-2`, digits = 3)
# SC1_fixtures_clone$`CS_2-1` <- round(1/SC1_fixtures_clone$`CS_2-1`, digits = 3)
# SC1_fixtures_clone$`CS_1-2` <- round(1/SC1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(SC1_fixtures_clone)[1] <- "league"
# colnames(SC1_fixtures_clone)[2] <- "Hometeam"
# colnames(SC1_fixtures_clone)[3] <- "Awayteam"
# colnames(SC1_fixtures_clone)[92] <- "predscore"
# colnames(SC1_fixtures_clone)[64] <- "ov25"
# colnames(SC1_fixtures_clone)[66] <- "ov25odds"
# colnames(SC1_fixtures_clone)[65] <- "un25"
# colnames(SC1_fixtures_clone)[67] <- "un25odds"
# colnames(SC1_fixtures_clone)[68] <- "BTTSY"
# colnames(SC1_fixtures_clone)[69] <- "BTTSN"
# colnames(SC1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(SC1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# SC1_fixtures_clone <- SC1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# SC1_fixtures_clone$matchid <- paste(SC1_fixtures_clone$Hometeam,SC1_fixtures_clone$Awayteam,sep = '-')
# #########################################################################################################################
# #SC2
# SC2_fixtures_clone <- SC2_fixtures
# colnames(SC2_fixtures_clone)[61] <- "Hwin"
# colnames(SC2_fixtures_clone)[62] <- "Draw"
# colnames(SC2_fixtures_clone)[63] <- "Awin"
#
# SC2_fixtures_clone$Hwinodds <-   SC2_fixtures$sc2_1_0 + SC2_fixtures$sc2_2_0 + SC2_fixtures$sc2_2_1 + SC2_fixtures$sc2_3_0 + SC2_fixtures$sc2_3_1 +
#   SC2_fixtures$sc2_3_2 + SC2_fixtures$sc2_4_0 + SC2_fixtures$sc2_4_1 + SC2_fixtures$sc2_4_2 + SC2_fixtures$sc2_4_3 +
#   SC2_fixtures$sc2_5_0 + SC2_fixtures$sc2_5_1 + SC2_fixtures$sc2_5_2 + SC2_fixtures$sc2_5_3 + SC2_fixtures$sc2_5_4 +
#   SC2_fixtures$sc2_6_0 + SC2_fixtures$sc2_6_1 + SC2_fixtures$sc2_6_2 + SC2_fixtures$sc2_6_3 + SC2_fixtures$sc2_6_4 +
#   SC2_fixtures$sc2_6_5
# SC2_fixtures_clone$Hwinodds <- round(1/SC2_fixtures_clone$Hwinodds, digits = 3)
#
# SC2_fixtures_clone$Drawodds <-  SC2_fixtures$sc2_0_0 + SC2_fixtures$sc2_1_1 + SC2_fixtures$sc2_2_2 + SC2_fixtures$sc2_3_3 + SC2_fixtures$sc2_4_4 +
#   SC2_fixtures$sc2_5_5 + SC2_fixtures$sc2_6_6
#
# SC2_fixtures_clone$Drawodds <- round(1/SC2_fixtures_clone$Drawodds, digits = 3)
#
# SC2_fixtures_clone$Awinodds <-   SC2_fixtures$sc2_0_1 + SC2_fixtures$sc2_0_2 + SC2_fixtures$sc2_1_2 + SC2_fixtures$sc2_0_3 + SC2_fixtures$sc2_1_3 +
#   SC2_fixtures$sc2_2_3 + SC2_fixtures$sc2_0_4 + SC2_fixtures$sc2_1_4 + SC2_fixtures$sc2_2_4 + SC2_fixtures$sc2_3_4 +
#   SC2_fixtures$sc2_0_5 + SC2_fixtures$sc2_1_5 + SC2_fixtures$sc2_2_5 + SC2_fixtures$sc2_3_5 + SC2_fixtures$sc2_4_5 +
#   SC2_fixtures$sc2_0_6 + SC2_fixtures$sc2_1_6 + SC2_fixtures$sc2_2_6 + SC2_fixtures$sc2_3_6 + SC2_fixtures$sc2_4_6 +
#   SC2_fixtures$sc2_5_6
#
# SC2_fixtures_clone$Awinodds <- round(1/SC2_fixtures_clone$Awinodds, digits = 3)
#
# colnames(SC2_fixtures_clone)[15] <- "CS_1-1"
# colnames(SC2_fixtures_clone)[13] <- "CS_1-0"
# colnames(SC2_fixtures_clone)[14] <- "CS_0-1"
# colnames(SC2_fixtures_clone)[16] <- "CS_2-0"
# colnames(SC2_fixtures_clone)[17] <- "CS_0-2"
# colnames(SC2_fixtures_clone)[19] <- "CS_2-1"
# colnames(SC2_fixtures_clone)[20] <- "CS_1-2"
#
# SC2_fixtures_clone$`CS_1-1` <- round(1/SC2_fixtures_clone$`CS_1-1`, digits = 3)
# SC2_fixtures_clone$`CS_1-0` <- round(1/SC2_fixtures_clone$`CS_1-0`, digits = 3)
# SC2_fixtures_clone$`CS_0-1` <- round(1/SC2_fixtures_clone$`CS_0-1`, digits = 3)
# SC2_fixtures_clone$`CS_2-0` <- round(1/SC2_fixtures_clone$`CS_2-0`, digits = 3)
# SC2_fixtures_clone$`CS_0-2` <- round(1/SC2_fixtures_clone$`CS_0-2`, digits = 3)
# SC2_fixtures_clone$`CS_2-1` <- round(1/SC2_fixtures_clone$`CS_2-1`, digits = 3)
# SC2_fixtures_clone$`CS_1-2` <- round(1/SC2_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(SC2_fixtures_clone)[1] <- "league"
# colnames(SC2_fixtures_clone)[2] <- "Hometeam"
# colnames(SC2_fixtures_clone)[3] <- "Awayteam"
# colnames(SC2_fixtures_clone)[92] <- "predscore"
# colnames(SC2_fixtures_clone)[64] <- "ov25"
# colnames(SC2_fixtures_clone)[66] <- "ov25odds"
# colnames(SC2_fixtures_clone)[65] <- "un25"
# colnames(SC2_fixtures_clone)[67] <- "un25odds"
# colnames(SC2_fixtures_clone)[68] <- "BTTSY"
# colnames(SC2_fixtures_clone)[69] <- "BTTSN"
# colnames(SC2_fixtures_clone)[70] <- "BTTSYodds"
# colnames(SC2_fixtures_clone)[71] <- "BTTSNodds"
#
#
# SC2_fixtures_clone <- SC2_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# SC2_fixtures_clone$matchid <- paste(SC2_fixtures_clone$Hometeam,SC2_fixtures_clone$Awayteam,sep = '-')
# ##########################################################################################################################
# SC3_fixtures_clone <- SC3_fixtures
# colnames(SC3_fixtures_clone)[61] <- "Hwin"
# colnames(SC3_fixtures_clone)[62] <- "Draw"
# colnames(SC3_fixtures_clone)[63] <- "Awin"
#
# SC3_fixtures_clone$Hwinodds <-   SC3_fixtures$sc3_1_0 + SC3_fixtures$sc3_2_0 + SC3_fixtures$sc3_2_1 + SC3_fixtures$sc3_3_0 + SC3_fixtures$sc3_3_1 +
#   SC3_fixtures$sc3_3_2 + SC3_fixtures$sc3_4_0 + SC3_fixtures$sc3_4_1 + SC3_fixtures$sc3_4_2 + SC3_fixtures$sc3_4_3 +
#   SC3_fixtures$sc3_5_0 + SC3_fixtures$sc3_5_1 + SC3_fixtures$sc3_5_2 + SC3_fixtures$sc3_5_3 + SC3_fixtures$sc3_5_4 +
#   SC3_fixtures$sc3_6_0 + SC3_fixtures$sc3_6_1 + SC3_fixtures$sc3_6_2 + SC3_fixtures$sc3_6_3 + SC3_fixtures$sc3_6_4 +
#   SC3_fixtures$sc3_6_5
# SC3_fixtures_clone$Hwinodds <- round(1/SC3_fixtures_clone$Hwinodds, digits = 3)
#
# SC3_fixtures_clone$Drawodds <-  SC3_fixtures$sc3_0_0 + SC3_fixtures$sc3_1_1 + SC3_fixtures$sc3_2_2 + SC3_fixtures$sc3_3_3 + SC3_fixtures$sc3_4_4 +
#   SC3_fixtures$sc3_5_5 + SC3_fixtures$sc3_6_6
#
# SC3_fixtures_clone$Drawodds <- round(1/SC3_fixtures_clone$Drawodds, digits = 3)
#
# SC3_fixtures_clone$Awinodds <-   SC3_fixtures$sc3_0_1 + SC3_fixtures$sc3_0_2 + SC3_fixtures$sc3_1_2 + SC3_fixtures$sc3_0_3 + SC3_fixtures$sc3_1_3 +
#   SC3_fixtures$sc3_2_3 + SC3_fixtures$sc3_0_4 + SC3_fixtures$sc3_1_4 + SC3_fixtures$sc3_2_4 + SC3_fixtures$sc3_3_4 +
#   SC3_fixtures$sc3_0_5 + SC3_fixtures$sc3_1_5 + SC3_fixtures$sc3_2_5 + SC3_fixtures$sc3_3_5 + SC3_fixtures$sc3_4_5 +
#   SC3_fixtures$sc3_0_6 + SC3_fixtures$sc3_1_6 + SC3_fixtures$sc3_2_6 + SC3_fixtures$sc3_3_6 + SC3_fixtures$sc3_4_6 +
#   SC3_fixtures$sc3_5_6
#
# SC3_fixtures_clone$Awinodds <- round(1/SC3_fixtures_clone$Awinodds, digits = 3)
#
# colnames(SC3_fixtures_clone)[15] <- "CS_1-1"
# colnames(SC3_fixtures_clone)[13] <- "CS_1-0"
# colnames(SC3_fixtures_clone)[14] <- "CS_0-1"
# colnames(SC3_fixtures_clone)[16] <- "CS_2-0"
# colnames(SC3_fixtures_clone)[17] <- "CS_0-2"
# colnames(SC3_fixtures_clone)[19] <- "CS_2-1"
# colnames(SC3_fixtures_clone)[20] <- "CS_1-2"
#
# SC3_fixtures_clone$`CS_1-1` <- round(1/SC3_fixtures_clone$`CS_1-1`, digits = 3)
# SC3_fixtures_clone$`CS_1-0` <- round(1/SC3_fixtures_clone$`CS_1-0`, digits = 3)
# SC3_fixtures_clone$`CS_0-1` <- round(1/SC3_fixtures_clone$`CS_0-1`, digits = 3)
# SC3_fixtures_clone$`CS_2-0` <- round(1/SC3_fixtures_clone$`CS_2-0`, digits = 3)
# SC3_fixtures_clone$`CS_0-2` <- round(1/SC3_fixtures_clone$`CS_0-2`, digits = 3)
# SC3_fixtures_clone$`CS_2-1` <- round(1/SC3_fixtures_clone$`CS_2-1`, digits = 3)
# SC3_fixtures_clone$`CS_1-2` <- round(1/SC3_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(SC3_fixtures_clone)[1] <- "league"
# colnames(SC3_fixtures_clone)[2] <- "Hometeam"
# colnames(SC3_fixtures_clone)[3] <- "Awayteam"
# colnames(SC3_fixtures_clone)[92] <- "predscore"
# colnames(SC3_fixtures_clone)[64] <- "ov25"
# colnames(SC3_fixtures_clone)[66] <- "ov25odds"
# colnames(SC3_fixtures_clone)[65] <- "un25"
# colnames(SC3_fixtures_clone)[67] <- "un25odds"
# colnames(SC3_fixtures_clone)[68] <- "BTTSY"
# colnames(SC3_fixtures_clone)[69] <- "BTTSN"
# colnames(SC3_fixtures_clone)[70] <- "BTTSYodds"
# colnames(SC3_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# SC3_fixtures_clone <- SC3_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# SC3_fixtures_clone$matchid <- paste(SC3_fixtures_clone$Hometeam,SC3_fixtures_clone$Awayteam,sep = '-')
# #############################################################################################################################
# T1_fixtures_clone <- T1_fixtures
# colnames(T1_fixtures_clone)[61] <- "Hwin"
# colnames(T1_fixtures_clone)[62] <- "Draw"
# colnames(T1_fixtures_clone)[63] <- "Awin"
#
# T1_fixtures_clone$Hwinodds <-   T1_fixtures$t1_1_0 + T1_fixtures$t1_2_0 + T1_fixtures$t1_2_1 + T1_fixtures$t1_3_0 + T1_fixtures$t1_3_1 +
#   T1_fixtures$t1_3_2 + T1_fixtures$t1_4_0 + T1_fixtures$t1_4_1 + T1_fixtures$t1_4_2 + T1_fixtures$t1_4_3 +
#   T1_fixtures$t1_5_0 + T1_fixtures$t1_5_1 + T1_fixtures$t1_5_2 + T1_fixtures$t1_5_3 + T1_fixtures$t1_5_4 +
#   T1_fixtures$t1_6_0 + T1_fixtures$t1_6_1 + T1_fixtures$t1_6_2 + T1_fixtures$t1_6_3 + T1_fixtures$t1_6_4 +
#   T1_fixtures$t1_6_5
# T1_fixtures_clone$Hwinodds <- round(1/T1_fixtures_clone$Hwinodds, digits = 3)
#
# T1_fixtures_clone$Drawodds <-  T1_fixtures$t1_0_0 + T1_fixtures$t1_1_1 + T1_fixtures$t1_2_2 + T1_fixtures$t1_3_3 + T1_fixtures$t1_4_4 +
#   T1_fixtures$t1_5_5 + T1_fixtures$t1_6_6
#
# T1_fixtures_clone$Drawodds <- round(1/T1_fixtures_clone$Drawodds, digits = 3)
#
# T1_fixtures_clone$Awinodds <-   T1_fixtures$t1_0_1 + T1_fixtures$t1_0_2 + T1_fixtures$t1_1_2 + T1_fixtures$t1_0_3 + T1_fixtures$t1_1_3 +
#   T1_fixtures$t1_2_3 + T1_fixtures$t1_0_4 + T1_fixtures$t1_1_4 + T1_fixtures$t1_2_4 + T1_fixtures$t1_3_4 +
#   T1_fixtures$t1_0_5 + T1_fixtures$t1_1_5 + T1_fixtures$t1_2_5 + T1_fixtures$t1_3_5 + T1_fixtures$t1_4_5 +
#   T1_fixtures$t1_0_6 + T1_fixtures$t1_1_6 + T1_fixtures$t1_2_6 + T1_fixtures$t1_3_6 + T1_fixtures$t1_4_6 +
#   T1_fixtures$t1_5_6
#
# T1_fixtures_clone$Awinodds <- round(1/T1_fixtures_clone$Awinodds, digits = 3)
#
# colnames(T1_fixtures_clone)[15] <- "CS_1-1"
# colnames(T1_fixtures_clone)[13] <- "CS_1-0"
# colnames(T1_fixtures_clone)[14] <- "CS_0-1"
# colnames(T1_fixtures_clone)[16] <- "CS_2-0"
# colnames(T1_fixtures_clone)[17] <- "CS_0-2"
# colnames(T1_fixtures_clone)[19] <- "CS_2-1"
# colnames(T1_fixtures_clone)[20] <- "CS_1-2"
#
# T1_fixtures_clone$`CS_1-1` <- round(1/T1_fixtures_clone$`CS_1-1`, digits = 3)
# T1_fixtures_clone$`CS_1-0` <- round(1/T1_fixtures_clone$`CS_1-0`, digits = 3)
# T1_fixtures_clone$`CS_0-1` <- round(1/T1_fixtures_clone$`CS_0-1`, digits = 3)
# T1_fixtures_clone$`CS_2-0` <- round(1/T1_fixtures_clone$`CS_2-0`, digits = 3)
# T1_fixtures_clone$`CS_0-2` <- round(1/T1_fixtures_clone$`CS_0-2`, digits = 3)
# T1_fixtures_clone$`CS_2-1` <- round(1/T1_fixtures_clone$`CS_2-1`, digits = 3)
# T1_fixtures_clone$`CS_1-2` <- round(1/T1_fixtures_clone$`CS_1-2`, digits = 3)
#
# colnames(T1_fixtures_clone)[1] <- "league"
# colnames(T1_fixtures_clone)[2] <- "Hometeam"
# colnames(T1_fixtures_clone)[3] <- "Awayteam"
# colnames(T1_fixtures_clone)[92] <- "predscore"
# colnames(T1_fixtures_clone)[64] <- "ov25"
# colnames(T1_fixtures_clone)[66] <- "ov25odds"
# colnames(T1_fixtures_clone)[65] <- "un25"
# colnames(T1_fixtures_clone)[67] <- "un25odds"
# colnames(T1_fixtures_clone)[68] <- "BTTSY"
# colnames(T1_fixtures_clone)[69] <- "BTTSN"
# colnames(T1_fixtures_clone)[70] <- "BTTSYodds"
# colnames(T1_fixtures_clone)[71] <- "BTTSNodds"
#
#
#
# T1_fixtures_clone <- T1_fixtures_clone[,c(1,2,3,98,61,62,63,95,96,97,64,66,65,67,68,70,69,71,13,14,15,16,17,19,20,92)]
# T1_fixtures_clone$matchid <- paste(T1_fixtures_clone$Hometeam,T1_fixtures_clone$Awayteam,sep = '-')
# #######################################################################################################################
# #######################################################################################################################
#
# #################################################
colnames(MLS_fixtures_clone)[8] <- "gamestatuscloned"
colnames(MLS_fixtures_clone)[9] <- "hometeamcloned"
colnames(MLS_fixtures_clone)[10] <- "awayteamcloned"
###################################################
#################################################
colnames(ARG_fixtures_clone)[8] <- "gamestatuscloned"
colnames(ARG_fixtures_clone)[9] <- "hometeamcloned"
colnames(ARG_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(D2_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(D2_fixtures_clone)[9] <- "hometeamcloned"
# colnames(D2_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(E0_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(E0_fixtures_clone)[9] <- "hometeamcloned"
# colnames(E0_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(E1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(E1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(E1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(E2_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(E2_fixtures_clone)[9] <- "hometeamcloned"
# colnames(E2_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(E3_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(E3_fixtures_clone)[9] <- "hometeamcloned"
# colnames(E3_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(EC_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(EC_fixtures_clone)[9] <- "hometeamcloned"
# colnames(EC_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(F1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(F1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(F1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(F2_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(F2_fixtures_clone)[9] <- "hometeamcloned"
# colnames(F2_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(G1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(G1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(G1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(N1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(N1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(N1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(P1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(P1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(P1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(SP1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(SP1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(SP1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(SP2_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(SP2_fixtures_clone)[9] <- "hometeamcloned"
# colnames(SP2_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(SC0_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(SC0_fixtures_clone)[9] <- "hometeamcloned"
# colnames(SC0_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(SC1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(SC1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(SC1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(SC2_fixtures_clone)[8] <- "gamestatuscloned"
# #colnames(SC2_fixtures_clone)[4] <- "Hwinodds"
# colnames(SC2_fixtures_clone)[9] <- "hometeamcloned"
# colnames(SC2_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(SC3_fixtures_clone)[8] <- "gamestatuscloned"
# #colnames(SC3_fixtures_clone)[4] <- "Hwinodds"
# colnames(SC3_fixtures_clone)[9] <- "hometeamcloned"
# colnames(SC3_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(I1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(I1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(I1_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(I2_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(I2_fixtures_clone)[9] <- "hometeamcloned"
# colnames(I2_fixtures_clone)[10] <- "awayteamcloned"
# ###################################################
# #################################################
# colnames(T1_fixtures_clone)[8] <- "gamestatuscloned"
# colnames(T1_fixtures_clone)[9] <- "hometeamcloned"
# colnames(T1_fixtures_clone)[10] <- "awayteamcloned"
###################################################


 allteamsnewleagues024_clonefixtures <- rbind(MLS_fixtures_clone,ARG_fixtures_clone)
# write.xlsx(allteams20222023_clonefixtures,'allteams20222023clonedfixtures.xlsx')
#mycloned_prediction <- dplyr::left_join(myodds_fixtures,allteams20222023_clonefixtures)
#write.xlsx(mycloned_prediction,'clonedprediction.xlsx')


#picks_fixtures_cloned <- read.csv('myfixtures.csv')
#picks_fixtures_cloned$matchid <- paste(picks_fixtures_cloned$Home_Team,picks_fixtures_cloned$Away_Team, sep = "-")
#picks_fixtures_prediction_cloned <- dplyr::left_join(picks_fixtures_cloned,allteams20222023_clonefixtures)
#write.xlsx(picks_fixtures_prediction_cloned,'picks_fixtures_prediction_cloned.xlsx')

