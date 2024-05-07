# difference in difference analysis to compare regional differences in covid strategies

if(!require("dplyr")) install.packages("dplyr") # for %>%
library(dplyr)

if(!require("tidyverse")) install.packages("tidyverse") # for remove_rownames()
library(tidyverse)

if(!require("lubridate")) install.packages("lubridate")
library(lubridate)

if(!require("ggplot2")) install.packages("ggplot2") 
library(ggplot2)

if(!require("ggthemes")) install.packages("ggthemes") 
library(ggthemes)

install.packages("tidyquant")
library(tidyquant)



data1 <- read.csv2("C:/Users/jonas/sciebo2/Uni/11. Semester/COVID-19 regionale Disparit?ten/Hausarbeit/Datensatz_Teil1.csv")
data2 <- read.csv2("C:/Users/jonas/sciebo2/Uni/11. Semester/COVID-19 regionale Disparit?ten/Hausarbeit/Datensatz_Teil2.csv")
data <- rbind(data1, data2)

str(data)
data <- subset(data, select = c(IdBundesland, Bundesland, Landkreis, AnzahlFall, 
                                AnzahlTodesfall, Meldedatum, NeuerFall, Einwohner))

#names(data) [names(data) == "?..ObjectId"] <- "ObjectID"
data$AnzahlFall <- as.numeric(data$AnzahlFall)
data$AnzahlTodesfall <- as.numeric(data$AnzahlTodesfall)
data$NeuerFall <- as.numeric(data$NeuerFall)
data$Meldedatum <- as.Date(data$Meldedatum)
data$IdBundesland <- as.character(data$IdBundesland)
data$Einwohner <- as.character(data$Einwohner)

#aggregate(. ~Meldedatum+Bundesland+Landkreis+IdBundesland, data = data, sum)


rki <- data %>% group_by(IdBundesland, Bundesland, Meldedatum, Einwohner) %>%
  summarise_at(vars(AnzahlFall, AnzahlTodesfall, NeuerFall), 
               sum) %>%
  ungroup()

rm(data, data1, data2)

#test$week <- floor_date(test$Meldedatum, "week")

rki <- subset(rki, Meldedatum< "2020/07/15 00:00:00+00")
rki <- subset(rki, Meldedatum> "2020/02/29 00:00:00+00")

#test$Meldedatum <- gsub(x=test$Meldedatum, pattern = " 00:00:00+00", replacement = "", fixed = T)

SH <- subset(rki, IdBundesland==1)
SH <- SH %>% tq_transmute(select = AnzahlFall,
                       mutate_fun = apply.weekly,
                       FUN = sum)
SH$STI <- SH$AnzahlFall/2903773*100000
SH <- subset(SH, select = c(Meldedatum, STI))
SH$Bundesland <- c("SH")


HH <- subset(rki, IdBundesland==2)
HH <- HH %>% tq_transmute(select = AnzahlFall,
                        mutate_fun = apply.weekly,
                        FUN = sum)
HH$STI <- HH$AnzahlFall/1847253*100000
HH <- subset(HH, select = c(Meldedatum, STI))
HH$Bundesland <- c("HH")


NS <- subset(rki, IdBundesland==3)
NS <- NS %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
NS$STI <- NS$AnzahlFall/7993608*100000
NS <- subset(NS, select = c(Meldedatum, STI))
NS$Bundesland <- c("NS")


HB <- subset(rki, IdBundesland==4)
HB <- HB %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
HB$STI <- HB$AnzahlFall/681202*100000
HB <- subset(HB, select = c(Meldedatum, STI))
HB$Bundesland <- c("HB")


NW <- subset(rki, IdBundesland==5)
NW <- NW %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
NW$STI <- NW$AnzahlFall/17947221*100000
NW <- subset(NW, select = c(Meldedatum, STI))
NW$Bundesland <- c("NW")

HS <- subset(rki, IdBundesland==6)
HS <- HS %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
HS$STI <- HS$AnzahlFall/6288080*100000
HS <- subset(HS, select = c(Meldedatum, STI))
HS$Bundesland <- c("HS")

RP <- subset(rki, IdBundesland==7)
RP <- RP %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
RP$STI <- RP$AnzahlFall/4093903*100000
RP <- subset(RP, select = c(Meldedatum, STI))
RP$Bundesland <- ("RP")

BW <- subset(rki, IdBundesland==8)
BW <- BW %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
BW$STI <- BW$AnzahlFall/11100394*100000
BW <- subset(BW, select = c(Meldedatum, STI))
BW$Bundesland <- c("BW")

BY <- subset(rki, IdBundesland==9)
BY <- BY %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
BY$STI <- BY$AnzahlFall/13124737*100000
BY <- subset(BY, select = c(Meldedatum, STI))
BY$Bundesland <- c("BY")

SR <- subset(rki, IdBundesland==10)
SR <- SR %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
SR$STI <- SR$AnzahlFall/986887*100000
SR <- subset(SR, select = c(Meldedatum, STI))
SR$Bundesland <- c("SR")

BE <- subset(rki, IdBundesland==11)
BE <- BE %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
BE$STI <- BE$AnzahlFall/3669491*100000
BE <- subset(BE, select = c(Meldedatum, STI))
BE$Bundesland <- c("BE")

BR <- subset(rki, IdBundesland==12)
BR <- BR %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
BR$STI <- BR$AnzahlFall/2521893*100000
BR <- subset(BR, select = c(Meldedatum, STI))
BR$Bundesland <- c("BR")

MV <- subset(rki, IdBundesland==13)
MV <- MV %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
MV$STI <- MV$AnzahlFall/1608138*100000
MV <- subset(MV, select = c(Meldedatum, STI))
MV$Bundesland <- c("MV")

SA <- subset(rki, IdBundesland==14)
SA <- SA %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
SA$STI <- SA$AnzahlFall/4071971*100000
SA <- subset(SA, select = c(Meldedatum, STI))
SA$Bundesland <- c("SA")

SN <- subset(rki, IdBundesland==15)
SN <- SN %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
SN$STI <- SN$AnzahlFall/2194782*100000
SN <- subset(SN, select = c(Meldedatum, STI))
SN$Bundesland <- c("SN")

TH <- subset(rki, IdBundesland==16)
TH <- TH %>% tq_transmute(select = AnzahlFall,
                          mutate_fun = apply.weekly,
                          FUN = sum)
TH$STI <- TH$AnzahlFall/2133378*100000
TH <- subset(TH, select = c(Meldedatum, STI))
TH$Bundesland <- c("TH")


BL <- rbind(BE,BR,BW,BY,HB,HH,HS,MV,NS,NW,RP,SA,SH,SN,SR,TH)
BL1 <- rbind(BE,BR,BW,BY)
BL2 <- rbind(HB,HH,HS,MV)
BL3 <- rbind(NS,NW,RP,SA)
BL4 <- rbind(SH,SN,SR,TH)
BL5 <- rbind(BY,HH)


#BL <- left_join(BR,BL,BR,BW,BY,HB,HH,HS,MV,NS,NW,RP,SA,SN,SH,SR,TH, by="Meldedatum")

#test <- as.xts(data$AnzahlFall, order.by=as.Date(test$Meldedatum))

#test %>% group_by(year = year(Meldedatum), week = week(Meldedatum)) %>% summarise_if(is.numeric, sum)

# %>% group_by(week = week(Meldedatum)) %>% summarise(value = sum(AnzahlFall))

ggplot(BL, aes(x = Meldedatum, y = STI, group = Bundesland, color = Bundesland)) + 
  geom_line() +
  theme_stata() + 
  scale_color_stata()



ggplot(TH, aes(x = Meldedatum, y = STI)) + 
  geom_line() +
  geom_vline(xintercept = as.numeric(ymd("2020-03-22", linetype = "dashed"))) +
  theme_stata() + 
  scale_color_stata()

ggplot(BL2, aes(x = Meldedatum, y = STI, group = Bundesland, color = Bundesland)) + 
  geom_line() +
  theme_stata() + 
  scale_color_stata()


#rki <- data %>% group_by(ObjectID, IdBundesland, Bundesland, Landkreis, Meldedatum, IdLandkreis) %>%
#  summarise_at(vars(AnzahlFall, AnzahlTodesfall, NeuerFall, IstErkrankungsbeginn), 
 #              sum) %>%
#  ungroup()

#summarise(group_by(data1, ObjectID, IdBundesland, Bundesland, Landkreis, Meldedatum, IdLandkreis),
#          sum(AnzahlFall), sum(AnzahlTodesfall), sum(NeuerFall), sum(IstErkrankungsbeginn))




#data %>% group_by_if(is.numeric %>% Negate) %>% 
 # summarize_all(sum)

#aggregate(data["AnzahlFall"], by=data["Meldedatum"], sum)