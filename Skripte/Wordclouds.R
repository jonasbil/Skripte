# Freizeitlärm: Wordclouds für offenen Angaben

#-----------------#
# 0. Settings ####
#-----------------#

# R Version
R.Version()

# Clean Environment
rm(list = ls())

packages <- c("wordcloud", "RColorBrewer", "wordcloud2", "tm", "haven", "SnowballC", "stopwords", "dplyr", "openxlsx")

load.packages <- lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}) 

rm(packages, load.packages)

#---------------------------------------#
# 1. Daten einlesen und aufbereiten ####
#--------------------------------------#

#path.data <- "Z:/Projekte/220.003_UBA_Freizeitlaerm/6_AP3_Belaestigungsbefragung/Daten uzbonn/"

# Pfad Home-Office
path.data <- "C:/Zeus/SynologyDrive/220.003_UBA_Freizeitlaerm/6_AP3_Belaestigungsbefragung/Daten uzbonn/"
path.save <- "C:/Zeus/SynologyDrive/220.003_UBA_Freizeitlaerm/6_AP3_Belaestigungsbefragung/Graphiken/Wordclouds/"

data <- read.xlsx(paste0(path.data,"wordcloud_prepare.xlsx"))


# Geräusche Beschreibung
set.seed(1234) # for reproducibility 
png(paste0(path.save,"Geräusche_beschreibung1.png"), res = 200)
wordcloud(words = data$word, freq = data$freq, min.freq = 1, max.words=23, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
dev.off()


# Geräusche Art
art <- read.xlsx(paste0(path.data,"wordcloud_prepare.xlsx"), sheet = 2, rows = 1:18)
png(paste0(path.save,"Geräusche_art1.png"), res = 200)
wordcloud(words = art$word, freq = art$freq, min.freq = 1, scale=c(3.5,0.25), max.words = 17, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
dev.off()
#wordcloud2(data = art, size = 1, color = "random-dark")


# Störung andere
andere <- read.xlsx(paste0(path.data,"wordcloud_prepare.xlsx"), sheet = 5)

# Parkplatzsituation durch Parksituation ersetzen (ist kürzer)
andere$word <- gsub("Parkplatzsituation", "Parksituation", andere$word)

png(paste0(path.save,"Störung_andere1.png"), res = 250)
wordcloud(words = andere$word, freq = andere$freq, min.freq = 1, scale=c(2.5,0.25), max.words = 17, random.order = FALSE, rot.per = 0.5, colors = brewer.pal(8, "Dark2"))
dev.off()

