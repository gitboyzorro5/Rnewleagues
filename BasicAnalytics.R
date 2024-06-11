#by gitboyzorro5
#remotes::install_github("JaseZiv/worldfootballR")
#install.packages('worldfootballR')
library('worldfootballR')
library('dplyr')
library('xlsx')
library('mgsub')
Sys.setenv(JAVA_HOME ="C:\\Program Files\\Java\\jre1.8.0_221")
options(java.parameters = "-Xmx4g")


mls_match_shooting <- fb_match_shooting(match_url = "https://fbref.com/en/matches/6b8eefb5/Inter-Miami-Real-Salt-Lake-February-21-2024-Major-League-Soccer")

mls_squad_urls <- fb_teams_urls("https://fbref.com/en/comps/22/Major-League-Soccer-Stats")

fb_player_urls("https://fbref.com/en/squads/e9ea41b2/FC-Cincinnati-Stats")

View(fb_player_season_stats("https://fbref.com/en/players/363ba6d7/Luciano-Acosta", stat_type = "standard"))

mapped_players <- player_dictionary_mapping()
View(mapped_players)
