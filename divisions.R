#by gitboyzorro5
#create divisions and order by date
library('lubridate')
library('dplyr')
MLS <- read.csv('LEAGUESCSV/MLS.csv')
ARG <- read.csv('LEAGUESCSV/ARG.csv')
BRA <- read.csv('LEAGUESCSV/BRA.csv')
allteamsnewleagues2024 <- rbind(MLS,ARG,BRA)
allteamsnewleagues2024 <- allteamsnewleagues2024[,c(-1)]

#change date strings to Date objects
allteamsnewleagues2024$Date <- ymd(allteamsnewleagues2024$Date)

allteamsnewleagues2024 <- allteamsnewleagues2024[order(as.Date(allteamsnewleagues2024$Date, format = "%d/%m/%Y"), decreasing = FALSE),]
#calculate total goals
allteamsnewleagues2024$TG <- allteamsnewleagues2024$FTHG + allteamsnewleagues2024$FTAG
allteamsnewleagues2024$TC <- allteamsnewleagues2024$HCO + allteamsnewleagues2024$ACO
allteamsnewleagues2024$TF <- allteamsnewleagues2024$HF + allteamsnewleagues2024$AF
allteamsnewleagues2024$COSC <- paste(allteamsnewleagues2024$HCO,allteamsnewleagues2024$ACO,sep = "-")
allteamsnewleagues2024$OV15 <- ifelse(allteamsnewleagues2024$TG >= 2,"Y","N")
allteamsnewleagues2024$OV25 <- ifelse(allteamsnewleagues2024$TG >= 3,"Y","N")
allteamsnewleagues2024$OV35 <- ifelse(allteamsnewleagues2024$TG >= 4,"Y","N")
allteamsnewleagues2024$TY <- allteamsnewleagues2024$HY + allteamsnewleagues2024$AY
allteamsnewleagues2024$TR <- allteamsnewleagues2024$HR + allteamsnewleagues2024$AR

allteamsnewleagues2024 <- allteamsnewleagues2024 %>% dplyr::relocate(21,.after = 31)
allteamsnewleagues2024 <- allteamsnewleagues2024 %>% dplyr::relocate(21,.after = 31)
#create divisions subsets
MLS <- subset(allteamsnewleagues2024, Div == "MLS")
ARG <- subset(allteamsnewleagues2024, Div == "ARG")
BRA <- subset(allteamsnewleagues2024, Div == "BRA")

View(allteamsnewleagues2024)
