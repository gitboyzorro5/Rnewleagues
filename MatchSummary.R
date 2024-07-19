#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")

#reading and writing
mls_summary <- readxl::read_excel('mls_match_summary.xlsx')
mls_summary <- mls_summary[,c(-1)]

mls_summary$Home_Team <- mgsub(mls_summary$Home_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_summary$Away_Team <- mgsub(mls_summary$Away_Team,c("Vancouver Whitecaps FC","CF Montréal","Austin FC","Charlotte FC","Chicago Fire","Colorado Rapids","Columbus Crew","Houston Dynamo","Los Angeles FC","Minnesota United","Nashville SC","New England Revolution","New York Red Bulls","New York City FC","Philadelphia Union","Real Salt Lake","San Jose Earthquakes","Seattle Sounders FC","Sporting Kansas City","St. Louis City","Vancouver Whitecaps FC","Atlanta United"),c("Vancouver Wcaps","CF Montreal","Austin","Charlotte","Fire","Rapids","Crew","Dynamo FC","LAFC","Minnesota Utd","Nashville","NE Revolution","NY Red Bulls","NYCFC","Philadelphia","RSL","SJ Earthquakes","Seattle","Sporting KC","St. Louis","Vancouver Wcaps","Atlanta Utd"))
mls_summary$Home_Team <- mgsub(mls_summary$Home_Team,c("Vancouver W'caps","CF Montréal"),c("Vancouver Wcaps","CF Montreal"))
mls_summary$Away_Team <- mgsub(mls_summary$Away_Team,c("Vancouver W'caps","CF Montréal"),c("Vancouver Wcaps","CF Montreal"))

mls_summary$matchid <- paste(mls_summary$Match_Date,mls_summary$Home_Team,mls_summary$Away_Team,sep = "-")


MLS_spread <- subset(allteamsnewleagues2024,Div =="MLS")
MLS_spread$matchid <- paste(MLS_spread$Date,MLS_spread$HomeTeam,MLS_spread$AwayTeam,sep = "-")

MLS_spread$n <- MLS_spread$TG

library('sqldf')
require('RH2')


Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT mls_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM mls_summary INNER JOIN MLS_spread ON mls_summary.matchid = MLS_spread.matchid WHERE mls_summary.Event_Type = 'Goal' GROUP BY mls_summary.matchid")
MLS_spread <- dplyr::left_join(MLS_spread,Total_Goalmins)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)
#Bookings
MLS_spread$Bookings <- (MLS_spread$HY *10 + MLS_spread$HR *25) + (MLS_spread$AY*10 + MLS_spread$AR*25)
#CrossBookings
MLS_spread$Crossbookings <- (MLS_spread$HY *10 + MLS_spread$HR *25)*(MLS_spread$AY*10 + MLS_spread$AR*25)
#GoalsXbookings
MLS_spread$GoalsXbookings <- (MLS_spread$Bookings)*(MLS_spread$TG)
#CornersXbookings
MLS_spread$CornersXbookings <- (MLS_spread$TC)*(MLS_spread$Bookings)
#GoalsXCorners
MLS_spread$GoalsXcorners <- (MLS_spread$TG)*(MLS_spread$TC)
#TGMxCorners
MLS_spread$TGMXcorners <- (MLS_spread$Total_Goalmins)*(MLS_spread$TC)
#GoalsXcornersXbookings
MLS_spread$GoalsXcornerXbookings <- (MLS_spread$TG)*(MLS_spread$TC)*(MLS_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS FH_HYC FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Event_Half = '1' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,FH_HYC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS FH_AYC FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Event_Half = '1' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,FH_AYC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS FH_HRC FROM mls_summary WHERE mls_summary.Event_Type = 'Red Card' AND mls_summary.Event_Half = '1' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,FH_HRC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS FH_ARC FROM mls_summary WHERE mls_summary.Event_Type = 'Red Card' AND mls_summary.Event_Half = '1' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,FH_ARC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS SH_HYC FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Event_Half = '2' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,SH_HYC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS SH_AYC FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Event_Half = '2' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,SH_AYC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS SH_HRC FROM mls_summary WHERE mls_summary.Event_Type = 'Red Card' AND mls_summary.Event_Half = '2' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,SH_HRC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS SH_ARC FROM mls_summary WHERE mls_summary.Event_Type = 'Red Card' AND mls_summary.Event_Half = '2' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,SH_ARC)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

#firsthalf
MLS_spread$FH_HomeBookings <- MLS_spread$FH_HYC *10 + MLS_spread$FH_HRC *25

MLS_spread$FH_AwayBookings <- MLS_spread$FH_AYC *10 + MLS_spread$FH_ARC *25

MLS_spread$FH_TotalBookings <- MLS_spread$FH_HomeBookings + MLS_spread$FH_AwayBookings

#second half
MLS_spread$SH_HomeBookings <- MLS_spread$SH_HYC *10 + MLS_spread$SH_HRC *25

MLS_spread$SH_AwayBookings <- MLS_spread$SH_AYC *10 + MLS_spread$SH_ARC *25

MLS_spread$SH_TotalBookings <- MLS_spread$SH_HomeBookings + MLS_spread$SH_AwayBookings


MLS_spread$MultiBookings <- MLS_spread$FH_TotalBookings * MLS_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT mls_summary.matchid,SUM(Event_time) AS Home_YCmins FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Home_YCmins)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT mls_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM mls_summary WHERE mls_summary.Event_Type = 'Red Card' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Home_RCmins)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT mls_summary.matchid,SUM(Event_time) AS Away_YCmins FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Away_YCmins)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT mls_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM mls_summary WHERE mls_summary.Event_Type = 'Red Card' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Away_RCmins)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

MLS_spread$Home_TotalCardmins <- MLS_spread$Home_YCmins + MLS_spread$Home_RCmins
MLS_spread$Away_TotalCardmins <- MLS_spread$Away_YCmins + MLS_spread$Away_RCmins
MLS_spread$match_TotalCardmins <- MLS_spread$Home_TotalCardmins + MLS_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT mls_summary.matchid,MIN(mls_summary.Event_Time) AS Home_first_YCTime FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Home_Away = 'Home' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Home_first_YCTime)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT mls_summary.matchid,MIN(mls_summary.Event_Time) AS Away_first_YCTime FROM mls_summary WHERE mls_summary.Event_Type = 'Yellow Card' AND mls_summary.Home_Away = 'Away' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Away_first_YCTime)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)

MLS_spread$match_First_YCTime <- pmin(MLS_spread$Home_first_YCTime,MLS_spread$Away_first_YCTime)

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT mls_summary.matchid,COUNT(*) AS Penalty FROM mls_summary WHERE mls_summary.Event_Type = 'Penalty' GROUP BY mls_summary.matchid ")
MLS_spread <- dplyr::left_join(MLS_spread,Penalty)
MLS_spread <- MLS_spread %>% replace(is.na(.),0)
#calculate match performance
MLS_spread$MatchPerfomance <- MLS_spread$TG *15 + MLS_spread$TY *5 + MLS_spread$TR *15 + MLS_spread$TC *3 + MLS_spread$Penalty *10


unlink('MLS_SPREAD.xlsx')
write.xlsx(MLS_spread,'MLS_SPREAD.xlsx')
####################################################################################################
####################################################################################################
#ARG
arg_summary <- readxl::read_excel('arg_match_summary.xlsx')
arg_summary <- arg_summary[,c(-1)]

arg_summary$Home_Team <- mgsub(arg_summary$Home_Team,c("Atlé Tucumán","Cen. Córdoba–SdE","Defensa y Just","Huracán","Lanús","Newell's OB","Unión","Vélez Sarsfield"),c("Atletico Tucuman","Central Cordoba","Defensa Justicia","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_summary$Away_Team <- mgsub(arg_summary$Away_Team,c("Atlé Tucumán","Cen. Córdoba–SdE","Defensa y Just","Huracán","Lanús","Newell's OB","Unión","Vélez Sarsfield"),c("Atletico Tucuman","Central Cordoba","Defensa Justicia","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_summary$Home_Team <- mgsub(arg_summary$Home_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))
arg_summary$Away_Team <- mgsub(arg_summary$Away_Team,c("Argentinos Juniors","Atlético Tucumán","Central Córdoba \\(SdE\\)","Defensa y Justicia","Estudiantes \\(LP\\)","Gimnasia y Esgrima \\(LP\\)","Huracán","Lanús","Newell's Old Boys","Unión","Vélez Sarsfield"),c("Arg Juniors","Atletico Tucuman","Central Cordoba","Defensa Justicia","Estudiantes","Gimnasia–LP","Huracan","Lanus","Newells OB","Union","Velez Sarsfield"))

arg_summary$matchid <- paste(arg_summary$Match_Date,arg_summary$Home_Team,arg_summary$Away_Team,sep = "-")


ARG_spread <- subset(allteamsnewleagues2024,Div =="ARG")
ARG_spread$matchid <- paste(ARG_spread$Date,ARG_spread$HomeTeam,ARG_spread$AwayTeam,sep = "-")

ARG_spread$n <- ARG_spread$TG

library('sqldf')
require('RH2')


Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT arg_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM arg_summary INNER JOIN ARG_spread ON arg_summary.matchid = ARG_spread.matchid WHERE arg_summary.Event_Type = 'Goal' GROUP BY arg_summary.matchid")
ARG_spread <- dplyr::left_join(ARG_spread,Total_Goalmins)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)
#Bookings
ARG_spread$Bookings <- (ARG_spread$HY *10 + ARG_spread$HR *25) + (ARG_spread$AY*10 + ARG_spread$AR*25)
#CrossBookings
ARG_spread$Crossbookings <- (ARG_spread$HY *10 + ARG_spread$HR *25)*(ARG_spread$AY*10 + ARG_spread$AR*25)
#GoalsXbookings
ARG_spread$GoalsXbookings <- (ARG_spread$Bookings)*(ARG_spread$TG)
#CornersXbookings
ARG_spread$CornersXbookings <- (ARG_spread$TC)*(ARG_spread$Bookings)
#GoalsXCorners
ARG_spread$GoalsXcorners <- (ARG_spread$TG)*(ARG_spread$TC)
#TGMxCorners
ARG_spread$TGMXcorners <- (ARG_spread$Total_Goalmins)*(ARG_spread$TC)
#GoalsXcornersXbookings
ARG_spread$GoalsXcornerXbookings <- (ARG_spread$TG)*(ARG_spread$TC)*(ARG_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS FH_HYC FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Event_Half = '1' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,FH_HYC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS FH_AYC FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Event_Half = '1' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,FH_AYC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS FH_HRC FROM arg_summary WHERE arg_summary.Event_Type = 'Red Card' AND arg_summary.Event_Half = '1' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,FH_HRC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS FH_ARC FROM arg_summary WHERE arg_summary.Event_Type = 'Red Card' AND arg_summary.Event_Half = '1' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,FH_ARC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS SH_HYC FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Event_Half = '2' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,SH_HYC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS SH_AYC FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Event_Half = '2' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,SH_AYC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS SH_HRC FROM arg_summary WHERE arg_summary.Event_Type = 'Red Card' AND arg_summary.Event_Half = '2' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,SH_HRC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS SH_ARC FROM arg_summary WHERE arg_summary.Event_Type = 'Red Card' AND arg_summary.Event_Half = '2' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,SH_ARC)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

#firsthalf
ARG_spread$FH_HomeBookings <- ARG_spread$FH_HYC *10 + ARG_spread$FH_HRC *25

ARG_spread$FH_AwayBookings <- ARG_spread$FH_AYC *10 + ARG_spread$FH_ARC *25

ARG_spread$FH_TotalBookings <- ARG_spread$FH_HomeBookings + ARG_spread$FH_AwayBookings

#second half
ARG_spread$SH_HomeBookings <- ARG_spread$SH_HYC *10 + ARG_spread$SH_HRC *25

ARG_spread$SH_AwayBookings <- ARG_spread$SH_AYC *10 + ARG_spread$SH_ARC *25

ARG_spread$SH_TotalBookings <- ARG_spread$SH_HomeBookings + ARG_spread$SH_AwayBookings


ARG_spread$MultiBookings <- ARG_spread$FH_TotalBookings * ARG_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT arg_summary.matchid,SUM(Event_time) AS Home_YCmins FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Home_YCmins)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT arg_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM arg_summary WHERE arg_summary.Event_Type = 'Red Card' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Home_RCmins)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT arg_summary.matchid,SUM(Event_time) AS Away_YCmins FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Away_YCmins)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT arg_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM arg_summary WHERE arg_summary.Event_Type = 'Red Card' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Away_RCmins)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

ARG_spread$Home_TotalCardmins <- ARG_spread$Home_YCmins + ARG_spread$Home_RCmins
ARG_spread$Away_TotalCardmins <- ARG_spread$Away_YCmins + ARG_spread$Away_RCmins
ARG_spread$match_TotalCardmins <- ARG_spread$Home_TotalCardmins + ARG_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT arg_summary.matchid,MIN(arg_summary.Event_Time) AS Home_first_YCTime FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Home_Away = 'Home' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Home_first_YCTime)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT arg_summary.matchid,MIN(arg_summary.Event_Time) AS Away_first_YCTime FROM arg_summary WHERE arg_summary.Event_Type = 'Yellow Card' AND arg_summary.Home_Away = 'Away' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Away_first_YCTime)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)

ARG_spread$match_First_YCTime <- pmin(ARG_spread$Home_first_YCTime,ARG_spread$Away_first_YCTime)

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT arg_summary.matchid,COUNT(*) AS Penalty FROM arg_summary WHERE arg_summary.Event_Type = 'Penalty' GROUP BY arg_summary.matchid ")
ARG_spread <- dplyr::left_join(ARG_spread,Penalty)
ARG_spread <- ARG_spread %>% replace(is.na(.),0)
#calculate match performance
ARG_spread$MatchPerfomance <- ARG_spread$TG *15 + ARG_spread$TY *5 + ARG_spread$TR *15 + ARG_spread$TC *3 + ARG_spread$Penalty *10


unlink('ARG_SPREAD.xlsx')
write.xlsx(ARG_spread,'ARG_SPREAD.xlsx')
##########################################################################################################################################
##########################################################################################################################################
#BRA
bra_summary <- readxl::read_excel('bra_match_summary.xlsx')
bra_summary <- bra_summary[,c(-1)]

bra_summary$Home_Team <- mgsub(bra_summary$Home_Team,c("Athletico Paranaense","Atlético Goianiense","Atlético Mineiro","Botafogo \\(RJ\\)","Criciúma","Cuiabá","Grêmio","São Paulo","Vitória"),c("Ath Paranaense","Atl Goianiense","Atletico Mineiro","Botafogo RJ","Criciuma","Cuiaba","Gremio","Sao Paulo","Vitoria"))
bra_summary$Away_Team <- mgsub(bra_summary$Away_Team,c("Athletico Paranaense","Atlético Goianiense","Atlético Mineiro","Botafogo \\(RJ\\)","Criciúma","Cuiabá","Grêmio","São Paulo","Vitória"),c("Ath Paranaense","Atl Goianiense","Atletico Mineiro","Botafogo RJ","Criciuma","Cuiaba","Gremio","Sao Paulo","Vitoria"))
bra_summary$Home_Team <- mgsub(bra_summary$Home_Team,c("Athletico Paranaense","Atlético Goianiense","Atlético Mineiro","Botafogo \\(RJ\\)","Criciúma","Cuiabá","Grêmio","São Paulo","Vitória"),c("Ath Paranaense","Atl Goianiense","Atletico Mineiro","Botafogo RJ","Criciuma","Cuiaba","Gremio","Sao Paulo","Vitoria"))
bra_summary$Away_Team <- mgsub(bra_summary$Away_Team,c("Athletico Paranaense","Atlético Goianiense","Atlético Mineiro","Botafogo \\(RJ\\)","Criciúma","Cuiabá","Grêmio","São Paulo","Vitória"),c("Ath Paranaense","Atl Goianiense","Atletico Mineiro","Botafogo RJ","Criciuma","Cuiaba","Gremio","Sao Paulo","Vitoria"))

bra_summary$matchid <- paste(bra_summary$Match_Date,bra_summary$Home_Team,bra_summary$Away_Team,sep = "-")


BRA_spread <- subset(allteamsnewleagues2024,Div =="BRA")
BRA_spread$matchid <- paste(BRA_spread$Date,BRA_spread$HomeTeam,BRA_spread$AwayTeam,sep = "-")

BRA_spread$n <- BRA_spread$TG

library('sqldf')
require('RH2')


Total_Goalmins <- c()
Total_Goalmins <- sqldf("SELECT bra_summary.matchid,SUM(Event_Time) AS Total_Goalmins FROM bra_summary INNER JOIN BRA_spread ON bra_summary.matchid = BRA_spread.matchid WHERE bra_summary.Event_Type = 'Goal' GROUP BY bra_summary.matchid")
BRA_spread <- dplyr::left_join(BRA_spread,Total_Goalmins)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)
#Bookings
BRA_spread$Bookings <- (BRA_spread$HY *10 + BRA_spread$HR *25) + (BRA_spread$AY*10 + BRA_spread$AR*25)
#CrossBookings
BRA_spread$Crossbookings <- (BRA_spread$HY *10 + BRA_spread$HR *25)*(BRA_spread$AY*10 + BRA_spread$AR*25)
#GoalsXbookings
BRA_spread$GoalsXbookings <- (BRA_spread$Bookings)*(BRA_spread$TG)
#CornersXbookings
BRA_spread$CornersXbookings <- (BRA_spread$TC)*(BRA_spread$Bookings)
#GoalsXCorners
BRA_spread$GoalsXcorners <- (BRA_spread$TG)*(BRA_spread$TC)
#TGMxCorners
BRA_spread$TGMXcorners <- (BRA_spread$Total_Goalmins)*(BRA_spread$TC)
#GoalsXcornersXbookings
BRA_spread$GoalsXcornerXbookings <- (BRA_spread$TG)*(BRA_spread$TC)*(BRA_spread$Bookings)

#first half
FH_HYC <- c()
FH_HYC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS FH_HYC FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Event_Half = '1' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,FH_HYC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

FH_AYC <- c()
FH_AYC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS FH_AYC FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Event_Half = '1' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,FH_AYC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

FH_HRC <- c()
FH_HRC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS FH_HRC FROM bra_summary WHERE bra_summary.Event_Type = 'Red Card' AND bra_summary.Event_Half = '1' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,FH_HRC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

FH_ARC <- c()
FH_ARC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS FH_ARC FROM bra_summary WHERE bra_summary.Event_Type = 'Red Card' AND bra_summary.Event_Half = '1' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,FH_ARC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

#second half
SH_HYC <- c()
SH_HYC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS SH_HYC FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Event_Half = '2' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,SH_HYC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

SH_AYC <- c()
SH_AYC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS SH_AYC FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Event_Half = '2' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,SH_AYC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

SH_HRC <- c()
SH_HRC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS SH_HRC FROM bra_summary WHERE bra_summary.Event_Type = 'Red Card' AND bra_summary.Event_Half = '2' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,SH_HRC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

SH_ARC <- c()
SH_ARC <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS SH_ARC FROM bra_summary WHERE bra_summary.Event_Type = 'Red Card' AND bra_summary.Event_Half = '2' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,SH_ARC)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

#firsthalf
BRA_spread$FH_HomeBookings <- BRA_spread$FH_HYC *10 + BRA_spread$FH_HRC *25

BRA_spread$FH_AwayBookings <- BRA_spread$FH_AYC *10 + BRA_spread$FH_ARC *25

BRA_spread$FH_TotalBookings <- BRA_spread$FH_HomeBookings + BRA_spread$FH_AwayBookings

#second half
BRA_spread$SH_HomeBookings <- BRA_spread$SH_HYC *10 + BRA_spread$SH_HRC *25

BRA_spread$SH_AwayBookings <- BRA_spread$SH_AYC *10 + BRA_spread$SH_ARC *25

BRA_spread$SH_TotalBookings <- BRA_spread$SH_HomeBookings + BRA_spread$SH_AwayBookings


BRA_spread$MultiBookings <- BRA_spread$FH_TotalBookings * BRA_spread$SH_TotalBookings



Home_YCmins <- c()
Home_YCmins <- sqldf("SELECT bra_summary.matchid,SUM(Event_time) AS Home_YCmins FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Home_YCmins)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

Home_RCmins <- c()
Home_RCmins <- sqldf("SELECT bra_summary.matchid,SUM(Event_time)*2 AS Home_RCmins FROM bra_summary WHERE bra_summary.Event_Type = 'Red Card' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Home_RCmins)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

Away_YCmins <- c()
Away_YCmins <- sqldf("SELECT bra_summary.matchid,SUM(Event_time) AS Away_YCmins FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Away_YCmins)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

Away_RCmins <- c()
Away_RCmins <- sqldf("SELECT bra_summary.matchid,SUM(Event_time)*2 AS Away_RCmins FROM bra_summary WHERE bra_summary.Event_Type = 'Red Card' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Away_RCmins)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

BRA_spread$Home_TotalCardmins <- BRA_spread$Home_YCmins + BRA_spread$Home_RCmins
BRA_spread$Away_TotalCardmins <- BRA_spread$Away_YCmins + BRA_spread$Away_RCmins
BRA_spread$match_TotalCardmins <- BRA_spread$Home_TotalCardmins + BRA_spread$Away_TotalCardmins

Home_first_YCTime <- c()
Home_first_YCTime <- sqldf("SELECT bra_summary.matchid,MIN(bra_summary.Event_Time) AS Home_first_YCTime FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Home_Away = 'Home' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Home_first_YCTime)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

Away_first_YCTime <- c()
Away_first_YCTime <- sqldf("SELECT bra_summary.matchid,MIN(bra_summary.Event_Time) AS Away_first_YCTime FROM bra_summary WHERE bra_summary.Event_Type = 'Yellow Card' AND bra_summary.Home_Away = 'Away' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Away_first_YCTime)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)

BRA_spread$match_First_YCTime <- pmin(BRA_spread$Home_first_YCTime,BRA_spread$Away_first_YCTime)

#count number of penalties in a match
Penalty <- c()
Penalty <- sqldf("SELECT bra_summary.matchid,COUNT(*) AS Penalty FROM bra_summary WHERE bra_summary.Event_Type = 'Penalty' GROUP BY bra_summary.matchid ")
BRA_spread <- dplyr::left_join(BRA_spread,Penalty)
BRA_spread <- BRA_spread %>% replace(is.na(.),0)
#calculate match performance
BRA_spread$MatchPerfomance <- BRA_spread$TG *15 + BRA_spread$TY *5 + BRA_spread$TR *15 + BRA_spread$TC *3 + BRA_spread$Penalty *10


unlink('BRA_SPREAD.xlsx')
write.xlsx(BRA_spread,'BRA_SPREAD.xlsx')


####################################################################################################
####################################################################################################
#REFEREES
MLS_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM MLS_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('MLS_refereestats.xlsx')
write.xlsx(MLS_refereestats,'MLS_refereestats.xlsx')

##############################################################
ARG_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM ARG_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('ARG_refereestats.xlsx')
write.xlsx(ARG_refereestats,'ARG_refereestats.xlsx')
#############################################################
BRA_refereestats <- sqldf("SELECT Referee,COUNT(*),SUM(Bookings),AVG(Bookings),SUM(CrossBookings),AVG(CrossBookings),SUM(MultiBookings),AVG(MultiBookings),AVG(Home_first_YCTime),AVG(Away_first_YCTime),AVG(Match_first_YCTime) FROM BRA_spread GROUP BY Referee ORDER BY COUNT(*) DESC")
unlink('BRA_refereestats.xlsx')
write.xlsx(BRA_refereestats,'BRA_refereestats.xlsx')


















