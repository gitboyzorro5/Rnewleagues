#by gitboyzorro5
#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

###MLS
mls_match_results <- fb_match_results(country = "USA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(mls_match_results,"mls_match_results.xlsx")
mls_urls <- fb_match_urls(country = "USA", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(mls_urls,"mls_urls.xlsx")

mls_match_sumary <- fb_match_summary(match_url = mls_urls)
write.xlsx(mls_match_sumary,"mls_match_summary.xlsx")

mls_match_shots <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "summary" , team_or_player = "team")

write.xlsx(mls_match_shots,"mls_shots.xlsx")

mls_match_corners <- fb_advanced_match_stats(match_url = mls_urls,
                                             stat_type = "passing_types" , team_or_player = "team")

write.xlsx(mls_match_corners,"mls_corners.xlsx")

mls_match_fouls <- fb_advanced_match_stats(match_url = mls_urls,
                                           stat_type = "misc" , team_or_player = "team")

write.xlsx(mls_match_fouls,"mls_fouls.xlsx")

################################################################################################################
MLS <- mls_match_results
colnames(MLS)[1] <- "Div"
MLS$Div <- "MLS"
MLS <- MLS %>% dplyr::relocate(8,.after = 1)
colnames(MLS)[10] <- "HomeTeam"
colnames(MLS)[13] <- "AwayTeam"
MLS <- MLS %>% dplyr::relocate(10,.after = 2)
MLS <- MLS %>% dplyr::relocate(13,.after = 3)

MLS$HomeTeam <- mgsub(MLS$HomeTeam,c("Vancouver W'caps","CF Montréal"),c("Vancouver Wcaps","CF Montreal"))
MLS$AwayTeam <- mgsub(MLS$AwayTeam,c("Vancouver W'caps","CF Montréal"),c("Vancouver Wcaps","CF Montreal"))

colnames(MLS)[12] <- "FTHG"
colnames(MLS)[14] <- "FTAG"

MLS <- MLS %>% dplyr::relocate(12,.after = 4)
MLS <- MLS %>% dplyr::relocate(14,.after = 5)

MLS$FTR <- with(MLS,
                ifelse(FTHG > FTAG ,FTR <- "H" , ifelse(FTAG > FTHG,FTR <- "A", FTR <- "D"))
)

MLS <- MLS %>% dplyr::relocate(21,.after = 6)

MLS <- MLS[,c(1,2,3,4,5,6,7,15,16,19)]

MLS$matchid <- paste(MLS$Date,MLS$HomeTeam,MLS$AwayTeam, sep = "-")

#shots
#read mls_match_shots
mls_match_shots <- readxl::read_excel("mls_shots.xlsx")
mls_match_shots <- mls_match_shots[,c(-1)]
mls_match_shots$Home_Team <- mgsub(mls_match_shots$Home_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_match_shots$Away_Team <- mgsub(mls_match_shots$Away_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_match_shots$Team <- mgsub(mls_match_shots$Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))

mls_match_shots$matchid <- paste(mls_match_shots$Match_Date,mls_match_shots$Home_Team,mls_match_shots$Away_Team,sep = "-")
#remove all NAs
MLS <- na.omit(MLS)

library('sqldf')
require('RH2')
####################################
#Home yellowcards
####################################
HY <- c()
HY <- sqldf("SELECT mls_match_shots.CrdY AS HY,mls_match_shots.matchid FROM mls_match_shots INNER JOIN MLS ON MLS.matchid = mls_match_shots.matchid WHERE mls_match_shots.Home_Team = MLS.HomeTeam  AND mls_match_shots.Home_Away = 'Home'")
MLS <- dplyr::left_join(MLS,HY)

MLS <- MLS %>% dplyr::relocate(12,.after = 9)
colnames(MLS)[10] <- "HY"
#away yellow cards

AY <- c()
AY <- sqldf("SELECT mls_match_shots.CrdY AS AY,mls_match_shots.matchid FROM mls_match_shots INNER JOIN MLS ON MLS.matchid = mls_match_shots.matchid WHERE mls_match_shots.Away_Team = MLS.AwayTeam  AND mls_match_shots.Home_Away = 'Away'")
MLS <- dplyr::left_join(MLS,AY)

MLS <- MLS %>% dplyr::relocate(13,.after = 10)
colnames(MLS)[11] <- "AY"

#########################################################################################################################################################################
#red cards
###########################################################################################################################################################################
#
HR <- c()
HR <- sqldf("SELECT mls_match_shots.CrdR AS HR,mls_match_shots.matchid FROM mls_match_shots INNER JOIN MLS ON MLS.matchid = mls_match_shots.matchid WHERE mls_match_shots.Home_Team = MLS.HomeTeam  AND mls_match_shots.Home_Away = 'Home'")
MLS <- dplyr::left_join(MLS,HR)

MLS <- MLS %>% dplyr::relocate(14,.after = 11)
colnames(MLS)[12] <- "HR"
#away red cards

AR <- c()
AR <- sqldf("SELECT mls_match_shots.CrdR AS AR,mls_match_shots.matchid FROM mls_match_shots INNER JOIN MLS ON MLS.matchid = mls_match_shots.matchid WHERE mls_match_shots.Away_Team = MLS.AwayTeam  AND mls_match_shots.Home_Away = 'Away'")
MLS <- dplyr::left_join(MLS,AR)
colnames(MLS)
MLS <- MLS %>% dplyr::relocate(15,.after = 12)
colnames(MLS)[13] <- "AR"

##################################################
#shots on target
HST <- c()
HST <- sqldf("SELECT mls_match_shots.SoT AS HST,mls_match_shots.matchid FROM mls_match_shots INNER JOIN MLS ON MLS.matchid = mls_match_shots.matchid WHERE mls_match_shots.Home_Team = MLS.HomeTeam  AND mls_match_shots.Home_Away = 'Home'")
MLS <- dplyr::left_join(MLS,HST)

MLS <- MLS %>% dplyr::relocate(16,.after = 13)
colnames(MLS)[14] <- "HST"


AST <- c()
AST <- sqldf("SELECT mls_match_shots.SoT AS AST,mls_match_shots.matchid FROM mls_match_shots INNER JOIN MLS ON MLS.matchid = mls_match_shots.matchid WHERE mls_match_shots.Away_Team = MLS.AwayTeam  AND mls_match_shots.Home_Away = 'Away'")
MLS <- dplyr::left_join(MLS,AST)

MLS <- MLS %>% dplyr::relocate(17,.after = 14)
colnames(MLS)[15] <- "AST"

###################################################################################################
#corners

mls_match_corners <- readxl::read_excel("mls_corners.xlsx")
mls_match_corners <- mls_match_corners[,c(-1)]
mls_match_corners$Home_Team <- mgsub(mls_match_corners$Home_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_match_corners$Away_Team <- mgsub(mls_match_corners$Away_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_match_corners$Team <- mgsub(mls_match_corners$Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))

mls_match_corners$matchid <- paste(mls_match_corners$Match_Date,mls_match_corners$Home_Team,mls_match_corners$Away_Team,sep = "-")

HC <- c()
HC <- sqldf("SELECT mls_match_corners.Ck_Pass_Types AS HC,mls_match_corners.matchid FROM mls_match_corners INNER JOIN MLS ON MLS.matchid = mls_match_corners.matchid WHERE mls_match_corners.Home_Team = MLS.HomeTeam  AND mls_match_corners.Home_Away = 'Home'")
MLS <- dplyr::left_join(MLS,HC)

MLS <- MLS %>% dplyr::relocate(18,.after = 15)
colnames(MLS)[16] <- "HCO"

AC <- c()
AC <- sqldf("SELECT mls_match_corners.Ck_Pass_Types AS AC,mls_match_corners.matchid FROM mls_match_corners INNER JOIN MLS ON MLS.matchid = mls_match_corners.matchid WHERE mls_match_corners.Away_Team = MLS.AwayTeam  AND mls_match_corners.Home_Away = 'Away'")
MLS <- dplyr::left_join(MLS,AC)
MLS <- MLS %>% dplyr::relocate(19,.after = 16)
colnames(MLS)[17] <- "ACO"
#####################################################################################################
#Fouls
#####################################################################################################
mls_match_fouls <- readxl::read_excel("mls_fouls.xlsx")
mls_match_fouls <- mls_match_fouls[,c(-1)]
mls_match_fouls$Home_Team <- mgsub(mls_match_fouls$Home_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_match_fouls$Away_Team <- mgsub(mls_match_fouls$Away_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_match_fouls$Team <- mgsub(mls_match_fouls$Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))

mls_match_fouls$matchid <- paste(mls_match_fouls$Match_Date,mls_match_fouls$Home_Team,mls_match_fouls$Away_Team,sep = "-")

HF <- c()
HF <- sqldf("SELECT mls_match_fouls.Fls AS HF,mls_match_fouls.matchid FROM mls_match_fouls INNER JOIN MLS ON MLS.matchid = mls_match_fouls.matchid WHERE mls_match_fouls.Home_Team = MLS.HomeTeam  AND mls_match_fouls.Home_Away = 'Home'")
MLS <- dplyr::left_join(MLS,HF)

MLS <- MLS %>% dplyr::relocate(20,.after = 17)
colnames(MLS)[18] <- "HF"

AF <- c()
AF <- sqldf("SELECT mls_match_fouls.Fls AS AF,mls_match_fouls.matchid FROM mls_match_fouls INNER JOIN MLS ON MLS.matchid = mls_match_fouls.matchid WHERE mls_match_fouls.Away_Team = MLS.AwayTeam  AND mls_match_fouls.Home_Away = 'Away'")
MLS <- dplyr::left_join(MLS,AF)
colnames(MLS)
MLS <- MLS %>% dplyr::relocate(21,.after = 18)
colnames(MLS)[19] <- "AF"
##################################
MLS$CS <- paste(MLS$FTHG,MLS$FTAG,sep = "-")
colnames(MLS)
MLS <- MLS %>% dplyr::relocate(22,.after = 19)
###########################
#write csv
unlink('LEAGUESCSV/MLS.csv')
write.csv(MLS,'LEAGUESCSV/MLS.csv')
####################################################################################################################################################################################
####################################################################################################################################################################################
###ARG
arg_match_results <- fb_match_results(country = "ARG", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(arg_match_results,"arg_match_results.xlsx")
arg_urls <- fb_match_urls(country = "ARG", gender = "M", season_end_year = 2024, tier="1st")
write.xlsx(arg_urls,"arg_urls.xlsx")

arg_match_sumary <- fb_match_summary(match_url = arg_urls)
write.xlsx(arg_match_sumary,"arg_match_summary.xlsx")

arg_match_shots <- fb_advanced_match_stats(match_url = arg_urls,
                                           stat_type = "summary" , team_or_player = "team")

write.xlsx(arg_match_shots,"arg_shots.xlsx")

arg_match_corners <- fb_advanced_match_stats(match_url = arg_urls,
                                             stat_type = "passing_types" , team_or_player = "team")

write.xlsx(arg_match_corners,"arg_corners.xlsx")

arg_match_fouls <- fb_advanced_match_stats(match_url = arg_urls,
                                           stat_type = "misc" , team_or_player = "team")

write.xlsx(arg_match_fouls,"arg_fouls.xlsx")

################################################################################################################
ARG <- arg_match_results
colnames(ARG)[1] <- "Div"
ARG$Div <- "ARG"
ARG <- ARG %>% dplyr::relocate(8,.after = 1)
colnames(ARG)[10] <- "HomeTeam"
colnames(ARG)[13] <- "AwayTeam"
ARG <- ARG %>% dplyr::relocate(10,.after = 2)
ARG <- ARG %>% dplyr::relocate(13,.after = 3)

ARG$HomeTeam <- mgsub(ARG$HomeTeam,c("Atlé Tucumán","Cen. Córdoba–SdE","Defensa y Just","Huracán","Lanús","Newell's OB","Unión","Vélez Sarsfield"),c("Atletico Tucuman","Central Cordoba","Defensa Justicia","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
ARG$AwayTeam <- mgsub(ARG$AwayTeam,c("Atlé Tucumán","Cen. Córdoba–SdE","Defensa y Just","Huracán","Lanús","Newell's OB","Unión","Vélez Sarsfield"),c("Atletico Tucuman","Central Cordoba","Defensa Justicia","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))

colnames(ARG)[12] <- "FTHG"
colnames(ARG)[14] <- "FTAG"

ARG <- ARG %>% dplyr::relocate(12,.after = 4)
ARG <- ARG %>% dplyr::relocate(14,.after = 5)

ARG$FTR <- with(ARG,
                ifelse(FTHG > FTAG ,FTR <- "H" , ifelse(FTAG > FTHG,FTR <- "A", FTR <- "D"))
)

ARG <- ARG %>% dplyr::relocate(21,.after = 6)

ARG <- ARG[,c(1,2,3,4,5,6,7,15,16,19)]

ARG$matchid <- paste(ARG$Date,ARG$HomeTeam,ARG$AwayTeam, sep = "-")
sort(unique(arg_match_shots$Away_Team))
#shots
#read arg_match_shots
arg_match_shots <- readxl::read_excel("arg_shots.xlsx")
arg_match_shots <- arg_match_shots[,c(-1)]
arg_match_shots$Home_Team <- mgsub(arg_match_shots$Home_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_match_shots$Away_Team <- mgsub(arg_match_shots$Away_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_match_shots$Team <- mgsub(arg_match_shots$Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))

arg_match_shots$matchid <- paste(arg_match_shots$Match_Date,arg_match_shots$Home_Team,arg_match_shots$Away_Team,sep = "-")
#remove all NAs
ARG <- na.omit(ARG)
library('sqldf')
require('RH2')
####################################
#Home yellowcards
####################################
HY <- c()
HY <- sqldf("SELECT arg_match_shots.CrdY AS HY,arg_match_shots.matchid FROM arg_match_shots INNER JOIN ARG ON ARG.matchid = arg_match_shots.matchid WHERE arg_match_shots.Home_Team = ARG.HomeTeam  AND arg_match_shots.Home_Away = 'Home'")
ARG <- dplyr::left_join(ARG,HY)

ARG <- ARG %>% dplyr::relocate(12,.after = 9)
colnames(ARG)[10] <- "HY"
#away yellow cards

AY <- c()
AY <- sqldf("SELECT arg_match_shots.CrdY AS AY,arg_match_shots.matchid FROM arg_match_shots INNER JOIN ARG ON ARG.matchid = arg_match_shots.matchid WHERE arg_match_shots.Away_Team = ARG.AwayTeam  AND arg_match_shots.Home_Away = 'Away'")
ARG <- dplyr::left_join(ARG,AY)

ARG <- ARG %>% dplyr::relocate(13,.after = 10)
colnames(ARG)[11] <- "AY"

#########################################################################################################################################################################
#red cards
###########################################################################################################################################################################

HR <- c()
HR <- sqldf("SELECT arg_match_shots.CrdR AS HR,arg_match_shots.matchid FROM arg_match_shots INNER JOIN ARG ON ARG.matchid = arg_match_shots.matchid WHERE arg_match_shots.Home_Team = ARG.HomeTeam  AND arg_match_shots.Home_Away = 'Home'")
ARG <- dplyr::left_join(ARG,HR)

ARG <- ARG %>% dplyr::relocate(14,.after = 11)
colnames(ARG)[12] <- "HR"
#away red cards

AR <- c()
AR <- sqldf("SELECT arg_match_shots.CrdR AS AR,arg_match_shots.matchid FROM arg_match_shots INNER JOIN ARG ON ARG.matchid = arg_match_shots.matchid WHERE arg_match_shots.Away_Team = ARG.AwayTeam  AND arg_match_shots.Home_Away = 'Away'")
ARG <- dplyr::left_join(ARG,AR)

ARG <- ARG %>% dplyr::relocate(15,.after = 12)
colnames(ARG)[13] <- "AR"

##################################################
#shots on target
HST <- c()
HST <- sqldf("SELECT arg_match_shots.SoT AS HST,arg_match_shots.matchid FROM arg_match_shots INNER JOIN ARG ON ARG.matchid = arg_match_shots.matchid WHERE arg_match_shots.Home_Team = ARG.HomeTeam  AND arg_match_shots.Home_Away = 'Home'")
ARG <- dplyr::left_join(ARG,HST)

ARG <- ARG %>% dplyr::relocate(16,.after = 13)
colnames(ARG)[14] <- "HST"


AST <- c()
AST <- sqldf("SELECT arg_match_shots.SoT AS AST,arg_match_shots.matchid FROM arg_match_shots INNER JOIN ARG ON ARG.matchid = arg_match_shots.matchid WHERE arg_match_shots.Away_Team = ARG.AwayTeam  AND arg_match_shots.Home_Away = 'Away'")
ARG <- dplyr::left_join(ARG,AST)

ARG <- ARG %>% dplyr::relocate(17,.after = 14)
colnames(ARG)[15] <- "AST"

###################################################################################################
#corners

arg_match_corners <- readxl::read_excel("arg_corners.xlsx")
arg_match_corners <- arg_match_corners[,c(-1)]
arg_match_corners$Home_Team <- mgsub(arg_match_corners$Home_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_match_corners$Away_Team <- mgsub(arg_match_corners$Away_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_match_corners$Team <- mgsub(arg_match_corners$Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))

arg_match_corners$matchid <- paste(arg_match_corners$Match_Date,arg_match_corners$Home_Team,arg_match_corners$Away_Team,sep = "-")

HC <- c()
HC <- sqldf("SELECT arg_match_corners.Ck_Pass_Types AS HC,arg_match_corners.matchid FROM arg_match_corners INNER JOIN ARG ON ARG.matchid = arg_match_corners.matchid WHERE arg_match_corners.Home_Team = ARG.HomeTeam  AND arg_match_corners.Home_Away = 'Home'")
ARG <- dplyr::left_join(ARG,HC)

ARG <- ARG %>% dplyr::relocate(18,.after = 15)
colnames(ARG)[16] <- "HCO"

AC <- c()
AC <- sqldf("SELECT arg_match_corners.Ck_Pass_Types AS AC,arg_match_corners.matchid FROM arg_match_corners INNER JOIN ARG ON ARG.matchid = arg_match_corners.matchid WHERE arg_match_corners.Away_Team = ARG.AwayTeam  AND arg_match_corners.Home_Away = 'Away'")
ARG <- dplyr::left_join(ARG,AC)
ARG <- ARG %>% dplyr::relocate(19,.after = 16)
colnames(ARG)[17] <- "ACO"
#####################################################################################################
#Fouls

#####################################################################################################
arg_match_fouls <- readxl::read_excel("arg_fouls.xlsx")
arg_match_fouls <- arg_match_fouls[,c(-1)]
arg_match_fouls$Home_Team <- mgsub(arg_match_fouls$Home_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_match_fouls$Away_Team <- mgsub(arg_match_fouls$Away_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_match_fouls$Team <- mgsub(arg_match_fouls$Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))

arg_match_fouls$matchid <- paste(arg_match_fouls$Match_Date,arg_match_fouls$Home_Team,arg_match_fouls$Away_Team,sep = "-")

HF <- c()
HF <- sqldf("SELECT arg_match_fouls.Fls AS HF,arg_match_fouls.matchid FROM arg_match_fouls INNER JOIN ARG ON ARG.matchid = arg_match_fouls.matchid WHERE arg_match_fouls.Home_Team = ARG.HomeTeam  AND arg_match_fouls.Home_Away = 'Home'")
ARG <- dplyr::left_join(ARG,HF)

ARG <- ARG %>% dplyr::relocate(20,.after = 17)
colnames(ARG)[18] <- "HF"

AF <- c()
AF <- sqldf("SELECT arg_match_fouls.Fls AS AF,arg_match_fouls.matchid FROM arg_match_fouls INNER JOIN ARG ON ARG.matchid = arg_match_fouls.matchid WHERE arg_match_fouls.Away_Team = ARG.AwayTeam  AND arg_match_fouls.Home_Away = 'Away'")
ARG <- dplyr::left_join(ARG,AF)
colnames(ARG)
ARG <- ARG %>% dplyr::relocate(21,.after = 18)
colnames(ARG)[19] <- "AF"

##################################
ARG$CS <- paste(ARG$FTHG,ARG$FTAG,sep = "-")
colnames(ARG)
ARG <- ARG %>% dplyr::relocate(22,.after = 19)
###########################
#write csv
unlink('LEAGUESCSV/ARG.csv')
write.csv(ARG,'LEAGUESCSV/ARG.csv')

View(ARG)



