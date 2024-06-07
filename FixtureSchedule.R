library('plyr')
library('dplyr')
library('janitor')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
library('xlsx')
library('scales')
library('lubridate')
library('mgsub')
unlink('myfixtures.csv')
#read the data

# arg_match_fixtures <- readxl::read_excel("arg_match_results.xlsx")
# arg_match_fixtures <- arg_match_fixtures[,c(-1)]
#
# arg_match_fixtures <- arg_match_fixtures[,c(1,10,13,8)]
#
# colnames(arg_match_fixtures)[1] <- "Div"
# colnames(arg_match_fixtures)[2] <- "HomeTeam"
# colnames(arg_match_fixtures)[3] <- "AwayTeam"
# arg_match_fixtures$Div <- "ARG"
# arg_match_fixtures$Date <- ymd(arg_match_fixtures$Date)
# arg_match_fixtures$HomeTeam <- mgsub(arg_match_fixtures$HomeTeam,c("Atlé Tucumán","Cen. Córdoba–SdE","Defensa y Just","Huracán","Lanús","Newell's OB","Unión","Vélez Sarsfield"),c("Atletico Tucuman","Central Cordoba","Defensa Justicia","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
# arg_match_fixtures$AwayTeam <- mgsub(arg_match_fixtures$AwayTeam,c("Atlé Tucumán","Cen. Córdoba–SdE","Defensa y Just","Huracán","Lanús","Newell's OB","Unión","Vélez Sarsfield"),c("Atletico Tucuman","Central Cordoba","Defensa Justicia","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
# arg_match_fixtures$HomeTeam <- mgsub(arg_match_fixtures$HomeTeam,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
# arg_match_fixtures$AwayTeam <- mgsub(arg_match_fixtures$AwayTeam,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
# write.csv(arg_match_fixtures,'FIXTURES/ARGFIXTURES.csv')
#


MLS_schedule2024 <- read.csv('FIXTURES/MLSFIXTURES.csv')
MLS_schedule2024 <- MLS_schedule2024[,c(-1)]
ARG_schedule2024 <- read.csv('FIXTURES/ARGFIXTURES.csv')
ARG_schedule2024 <- ARG_schedule2024[,c(-1)]

# D2_schedule2024 <- read.csv('FIXTURES/D2_schedule2024.csv')
# E0_schedule2024 <- read.csv('FIXTURES/E0_schedule2024.csv')
# E1_schedule2024 <- read.csv('FIXTURES/E1_schedule2024.csv')
# E2_schedule2024 <- read.csv('FIXTURES/E2_schedule2024.csv')
# E3_schedule2024 <- read.csv('FIXTURES/E3_schedule2024.csv')
# EC_schedule2024 <- read.csv('FIXTURES/EC_schedule2024.csv')
# F1_schedule2024 <- read.csv('FIXTURES/F1_schedule2024.csv')
# F2_schedule2024 <- read.csv('FIXTURES/F2_schedule2024.csv')
# G1_schedule2024 <- read.csv('FIXTURES/G1_schedule2024.csv')
# #UCL_schedule2024 <- read.csv('Downloads/UCL_schedule2024.csv')
# I1_schedule2024 <- read.csv('FIXTURES/I1_schedule2024.csv')
# I2_schedule2024 <- read.csv('FIXTURES/I2_schedule2024.csv')
# N1_schedule2024 <- read.csv('FIXTURES/N1_schedule2024.csv')
# P1_schedule2024 <- read.csv('FIXTURES/P1_schedule2024.csv')
# SP1_schedule2024 <- read.csv('FIXTURES/SP1_schedule2024.csv')
# SC0_schedule2024 <- read.csv('FIXTURES/SC0_schedule2024.csv')
# SC1_schedule2024 <- read.csv('FIXTURES/SC1_schedule2024.csv')
# SC2_schedule2024 <- read.csv('FIXTURES/SC2_schedule2024.csv')
# SC3_schedule2024 <- read.csv('FIXTURES/SC3_schedule2024.csv')
# SP2_schedule2024 <- read.csv('FIXTURES/SP2_schedule2024.csv')
# T1_schedule2024 <- read.csv('FIXTURES/T1_schedule2024.csv')
#UEL_schedule2024 <- read.csv('FIXTURES/UEL_schedule2024.csv')
#parse the dates
MLS_schedule2024$Date <- ymd(MLS_schedule2024$Date)
ARG_schedule2024$Date <- ymd(ARG_schedule2024$Date)
# D2_schedule2024$Date <- dmy(D2_schedule2024$Date)
# E0_schedule2024$Date <- dmy(E0_schedule2024$Date)
# E1_schedule2024$Date <- dmy(E1_schedule2024$Date)
# E2_schedule2024$Date <- dmy(E2_schedule2024$Date)
# E3_schedule2024$Date <- dmy(E3_schedule2024$Date)
# EC_schedule2024$Date <- dmy(EC_schedule2024$Date)
# F1_schedule2024$Date <- dmy(F1_schedule2024$Date)
# F2_schedule2024$Date <- dmy(F2_schedule2024$Date)
# G1_schedule2024$Date <- dmy(G1_schedule2024$Date)
# #UCL_schedule2024$Date_ucl <- mdy(UCL_schedule2024$Date_ucl)
# I1_schedule2024$Date <- dmy(I1_schedule2024$Date)
# I2_schedule2024$Date <- dmy(I2_schedule2024$Date)
# N1_schedule2024$Date <- dmy(N1_schedule2024$Date)
# P1_schedule2024$Date <- dmy(P1_schedule2024$Date)
# SC0_schedule2024$Date <- dmy(SC0_schedule2024$Date)
# SC1_schedule2024$Date <- dmy(SC1_schedule2024$Date)
# SC2_schedule2024$Date <- dmy(SC2_schedule2024$Date)
# SC3_schedule2024$Date <- dmy(SC3_schedule2024$Date)
# SP1_schedule2024$Date <- dmy(SP1_schedule2024$Date)
# SP2_schedule2024$Date <- dmy(SP2_schedule2024$Date)
# T1_schedule2024$Date <- dmy(T1_schedule2024$Date)
#UEL_schedule2024$Date_uel <- dmy(UEL_schedule2024$Date_uel)
#insert divisions
MLS_schedule2024$Div <- "MLS"
ARG_schedule2024$Div <- "ARG"
# D2_schedule2024$Div <- "D2"
# E0_schedule2024$Div <- "E0"
# E1_schedule2024$Div <- "E1"
# E2_schedule2024$Div <- "E2"
# E3_schedule2024$Div <- "E3"
# EC_schedule2024$Div <- "EC"
# F1_schedule2024$Div <- "F1"
# F2_schedule2024$Div <- "F2"
# G1_schedule2024$Div <- "G1"
# I1_schedule2024$Div <- "I1"
# I2_schedule2024$Div <- "I2"
# N1_schedule2024$Div <- "N1"
# P1_schedule2024$Div <- "P1"
# SC0_schedule2024$Div <- "SC0"
# SC1_schedule2024$Div <- "SC1"
# SC2_schedule2024$Div <- "SC2"
# SC3_schedule2024$Div <- "SC3"
# SP1_schedule2024$Div <- "SP1"
# SP2_schedule2024$Div <- "SP2"
# T1_schedule2024$Div <- "T1"
#bind the fixtures

all_schedule2024 <- rbind(MLS_schedule2024,ARG_schedule2024)


myfixtures <- all_schedule2024[all_schedule2024$Date >= '2024-05-24' & all_schedule2024$Date <= '2024-05-27',]
write.csv(myfixtures,'myfixtures.csv')
View(myfixtures)
