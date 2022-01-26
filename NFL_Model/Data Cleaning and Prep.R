#basic sports betting model
#Peeping data needed 

#Loading data 
library(httr)
library(rvest)
library(dplyr)
library(plyr)

#Scrape data off website 
tmp_user_agent<- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9'
page_response <- GET("https://www.footballdb.com/games/index.html?lg=NFL&yr=2021", user_agent(tmp_user_agent))
df_lists <-page_response%>%
  read_html() %>% 
  html_nodes(".statistics") %>% #classes are queries with dot
  html_table()

#Merge all data frames into 1 
df <- ldply(df_lists, data.frame)

#clean the date column
df$Date  <- substr(df$Date, 1, nchar(df$Date)-4)
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
head(df)

#clean the team names columns


#Filter out the playoffs (Only want 2021 season)

data <- df %>% filter(Date < "2022-01-10") 

#Changing the name of columns for easier reproducibility
names(data)[3] <- paste("Vis_Score")
names(data)[5] <- paste("Home_Score")
names(data)[6] <- paste("OT")


#Load 2021 Standings 
library(readxl)
AFC_2021_Standings <- read_excel("GitHub/sportsbettingmodel/NFL_Model/data/AFC-NFC Data - Not Prepared/AFC 2021 Standings.xlsx")
NFC_2021_Standings <- read_excel("GitHub/sportsbettingmodel/NFL_Model/data/AFC-NFC Data - Not Prepared/NFC 2021 Standings.xlsx")

#Simple Join
AFC_2021_Standings$Division <- "AFC"
NFC_2021_Standings$Division <- "NFC"

Final_Standing_2021 <- rbind(AFC_2021_Standings, NFC_2021_Standings)

#Playoff Seed
Final_Standing_2021 <- Final_Standing_2021 %>% 
  mutate(Playoff_Seed = case_when(
    Tm %in% c("Green Bay Packers*", "Tampa Bay Buccaneers*", "Tennessee Titans*","Kansas City Chiefs*","Dallas Cowboys*",
              "Los Angeles Rams*", "Buffalo Bills*", "Cincinnati Bengals*") ~ "Division Winner",
    Tm %in% c("New England Patriots+","Las Vegas Raiders+", "San Francisco 49ers+",
              "Pittsburgh Steelers+","Arizona Cardinals+", "Philadelphia Eagles+") ~ "Wildcard"
  ))



#Now in order to join this data - the team names may need to be changed 

data <- data %>% mutate(Visitor = case_when(
  Visitor == "Arizona CardinalsARI" ~ "Arizona Cardinals+",
  Visitor == "Atlanta FalconsATL" ~ "Atlanta Falcons",
  Visitor == "Baltimore RavensBAL" ~ "Baltimore Ravens",
  Visitor == "Buffalo BillsBUF" ~ "Buffalo Bills*",
  Visitor == "Carolina PanthersCAR" ~ "Carolina Panthers",
  Visitor == "Chicago BearsCHI" ~ "Chicago Bears",
  Visitor == "Cincinnati BengalsCIN" ~ "Cincinnati Bengals*",
  Visitor == "Cleveland BrownsCLE" ~ "Cleveland Browns",
  Visitor == "Dallas CowboysDAL" ~ "Dallas Cowboys*",
  Visitor == "Denver BroncosDEN" ~ "Denver Broncos",
  Visitor == "Detroit LionsDET" ~ "Detroit Lions",
  Visitor == "Green Bay PackersGB" ~ "Green Bay Packers*",
  Visitor == "Houston TexansHOU" ~ "Houston Texans",
  Visitor == "Indianapolis ColtsIND" ~ "Indianapolis Colts",
  Visitor == "Jacksonville JaguarsJAX" ~ "Jacksonville Jaguars",
  Visitor == "Kansas City ChiefsKC" ~ "Kansas City Chiefs*",
  Visitor == "Las Vegas RaidersLV" ~ "Las Vegas Raiders+",
  Visitor == "Los Angeles ChargersLAC" ~ "Los Angeles Chargers",
  Visitor == "Los Angeles RamsLA" ~ "Los Angeles Rams*",
  Visitor == "Miami DolphinsMIA" ~ "Miami Dolphins",
  Visitor == "Minnesota VikingsMIN" ~ "Minnesota Vikings",
  Visitor == "New England PatriotsNE" ~ "New England Patriots+",
  Visitor == "New Orleans SaintsNO" ~ "New Orleans Saints",
  Visitor == "New York GiantsNYG" ~ "New York Giants",
  Visitor == "New York JetsNYJ" ~ "New York Jets",
  Visitor == "Philadelphia EaglesPHI" ~ "Philadelphia Eagles+",
  Visitor == "Pittsburgh SteelersPIT" ~ "Pittsburgh Steelers+",
  Visitor == "San Francisco 49ersSF" ~ "San Francisco 49ers+",
  Visitor == "Seattle SeahawksSEA" ~ "Seattle Seahawks",
  Visitor == "Tampa Bay BuccaneersTB" ~ "Tampa Bay Buccaneers*",
  Visitor == "Tennessee TitansTEN" ~ "Tennessee Titans*",
  Visitor == "Washington Football TeamWAS" ~ "Washington Football Team"
  
)) %>% mutate(Home = case_when(
  Home == "Arizona CardinalsARI" ~ "Arizona Cardinals+",
  Home == "Atlanta FalconsATL" ~ "Atlanta Falcons",
  Home == "Baltimore RavensBAL" ~ "Baltimore Ravens",
  Home == "Buffalo BillsBUF" ~ "Buffalo Bills*",
  Home == "Carolina PanthersCAR" ~ "Carolina Panthers",
  Home == "Chicago BearsCHI" ~ "Chicago Bears",
  Home == "Cincinnati BengalsCIN" ~ "Cincinnati Bengals*",
  Home == "Cleveland BrownsCLE" ~ "Cleveland Browns",
  Home == "Dallas CowboysDAL" ~ "Dallas Cowboys*",
  Home == "Denver BroncosDEN" ~ "Denver Broncos",
  Home == "Detroit LionsDET" ~ "Detroit Lions",
  Home == "Green Bay PackersGB" ~ "Green Bay Packers*",
  Home == "Houston TexansHOU" ~ "Houston Texans",
  Home == "Indianapolis ColtsIND" ~ "Indianapolis Colts",
  Home == "Jacksonville JaguarsJAX" ~ "Jacksonville Jaguars",
  Home == "Kansas City ChiefsKC" ~ "Kansas City Chiefs*",
  Home == "Las Vegas RaidersLV" ~ "Las Vegas Raiders+",
  Home == "Los Angeles ChargersLAC" ~ "Los Angeles Chargers",
  Home == "Los Angeles RamsLA" ~ "Los Angeles Rams*",
  Home == "Miami DolphinsMIA" ~ "Miami Dolphins",
  Home == "Minnesota VikingsMIN" ~ "Minnesota Vikings",
  Home == "New England PatriotsNE" ~ "New England Patriots+",
  Home == "New Orleans SaintsNO" ~ "New Orleans Saints",
  Home == "New York GiantsNYG" ~ "New York Giants",
  Home == "New York JetsNYJ" ~ "New York Jets",
  Home == "Philadelphia EaglesPHI" ~ "Philadelphia Eagles+",
  Home == "Pittsburgh SteelersPIT" ~ "Pittsburgh Steelers+",
  Home == "San Francisco 49ersSF" ~ "San Francisco 49ers+",
  Home == "Seattle SeahawksSEA" ~ "Seattle Seahawks",
  Home == "Tampa Bay BuccaneersTB" ~ "Tampa Bay Buccaneers*",
  Home == "Tennessee TitansTEN" ~ "Tennessee Titans*",
  Home == "Washington Football TeamWAS" ~ "Washington Football Team"
  
))

#Write Data 

#Boxscores
setwd("~/GitHub/sportsbettingmodel/NFL_Model/data/Cleaned Data/Boxscores")
write.csv(data, "Boxscores_2021.csv")

#Final Standings 
setwd("~/GitHub/sportsbettingmodel/NFL_Model/data/Cleaned Data/Final Standings Table")
write.csv(Final_Standing_2021, "Final_Standing_2021.csv")

setwd("~/GitHub/sportsbettingmodel/NFL_Model")






