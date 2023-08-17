#-----------------#
# 0. Settings ####
#-----------------#

# R Version
R.Version()

# Clean Environment
rm(list = ls())

# packages
packages <- c("tidyverse", "dplyr", "readxl", "openxlsx", "purrr", "foreign", "haven", "sjlabelled", "labelled")

load.packages <- lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#devtools::install_github("martinctc/surveytoolbox")
#library(surveytoolbox)

rm(packages, load.packages)



#-----------------------#
# 1. Daten einlesen ####
#----------------------#

path.data <- "Z:/Projekte/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Pegel_mopa/Pegel Sportanlagen NEU/" # Jonas Büro
#path.data <- "C:/Zeus/SynologyDrive/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Pegel_mopa/Pegel Sportanlagen/" #Jonas Home

path.spss.1 <- "Z:/Projekte/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Datensatz_uzbonn/" # Jonas Büro
#path.spss.1 <- "C:/Zeus/SynologyDrive/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Datensatz_uzbonn/" #Jonas Home

path.spss.2 <- "Z:/Projekte/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Datensatz_uzbonn_2 Welle/16_05_2022_ZEUS_Sportler_Final/" # Jonas Büro
#path.spss.2 <- "C:/Zeus/SynologyDrive/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Datensatz_uzbonn_2 Welle/16_05_2022_ZEUS_Sportler_Final/" #Jonas Home


# SPSS-Datensatz und Pegel-Datensatz einlesen 
sportler.1 <- read_sav(paste0(path.spss.1,"31_03_2022_ZEUS_Sportler_Final.sav"))
sportler.2 <- read_sav(paste0(path.spss.2,"21_09_2022_ZEUS_Sportler_Final.sav"))
#sportler <- data.frame((read.spss(paste0(path.spss,"31_03_2022_ZEUS_Sportler_Final.sav"))))
pegel.21 <- data.frame(read.xlsx(paste0(path.data,"Pegelwerte_hoechste.21.xlsx"))) #höchste Pegel
pegel.22 <- data.frame(read.xlsx(paste0(path.data,"Pegelwerte_hoechste.22.xlsx"))) #höchste Pegel
wz.21 <- data.frame(read.xlsx(paste0(path.data,"nur_wz_21.xlsx")))
wz.22 <- data.frame(read.xlsx(paste0(path.data,"nur_wz_22.xlsx")))


#-----------------------#
# 2. Daten bearbeiten ####
#----------------------#

#ID Variablen umbennen
sportler.1 <- sportler.1 %>%
  rename(ID_mopa = ID, 
         ID = Code_ZEUS)
sportler.2 <- sportler.2 %>%
  rename(ID_mopa = ID, 
         ID = Code)

# Leerzeichen hinter IDs im SPSS-Datensatz löschen
sportler.1$Code_ZEUS <- gsub(" ","", as.character(sportler.1$ID))
sportler.2$Code_ZEUS <- gsub(" ","", as.character(sportler.2$ID))

# Sportanlagen in Köln in beiden Datensätzen einheitlich benennen
sportler.1$Ort <- gsub("Köln, Höhenberg","Köln - Höhenberg", as.character(sportler.1$Ort))
sportler.1$Ort <- gsub("Köln, Vingst","Köln - Vingst", as.character(sportler.1$Ort))

# Liste mit fehlenden IDs im, die im SPSS aber nicht im "höchsten Pegeldatensatz" vorhanden sind
fehlende_id.21 <- data.frame(sportler.1$ID[!sportler.1$ID %in% pegel.21$ID])
colnames(fehlende_id.21) <- c("Welle1")
fehlende_id.22 <- data.frame(sportler.2$ID[!sportler.2$ID %in% pegel.22$ID])
colnames(fehlende_id.22) <- c("Welle2")
#write.xlsx(fehlende_id.21, paste0(path.data,"Fehlende_IDs.21.xlsx"))
#write.xlsx(fehlende_id.22, paste0(path.data,"Fehlende_IDs.22.xlsx"))


# Liste in fehlenden IDs im WZ-Pegeldatensatz
fehlende_id_wz.21 <- data.frame(sportler.1$ID[!sportler.1$ID %in% wz.21$ID])
colnames(fehlende_id_wz.21) <- c("ID")
fehlende_id_wz.22 <- data.frame(sportler.2$ID[!sportler.2$ID %in% wz.22$ID])
colnames(fehlende_id_wz.22) <- c("ID")


fehlend_spss <- data.frame(pegel.21$ID[!pegel.21$ID %in% sportler.1$ID])


#-----------------------------#
# 3. Daten zusammenf?hren ####
#----------------------------#

## Probelem!!!
# In den Pegeldaten von 2021 und 2022 sind auch Werte von ProbandenInnen enthalten, die im jeweiligen Jahr nicht teilgenommen haben
# z.B. Person x hat im Jahr 2022 an der Befragung teilgenommen, ihre Pegelwerte sind aber auch bei den 2021-Daten aufgef?hrt. 
# Vorschlag: In den Pegeldaten von 2021/2022 werden nur die F?lle behalten, die auch in den SPSS-Daten enthalten sind

pegel.21 <- semi_join(pegel.21, sportler.1, by = "ID")
wz.21 <- semi_join(wz.21, sportler.1, by = "ID")

#data.21 <- full_join(sportler.1, pegel.21, by = "ID")
#data.21 <- full_join(data.21, wz.21, by = "ID")

data.21 <- sportler.1 %>%
  full_join(pegel.21, by = "ID") %>%
  full_join(wz.21, by = "ID") %>%
  rename(Sportanlage = sportanlage)

pegel.22 <- semi_join(pegel.22, sportler.2, by = "ID")
wz.22 <- semi_join(wz.22, sportler.2, by = "ID")

data.22 <- sportler.2 %>%
  full_join(pegel.22, by = "ID") %>%
  full_join(wz.22, by = "ID")


save.data <- "Z:/Projekte/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Pegel_mopa/"


# Den Endungen der Spalten der Fragebögen wird .21 oder .22 angehängt 
colnames(data.21)[c(9:170)] <- paste0(colnames(data.21)[c(9:170)],".21")
colnames(data.22)[c(8:172)] <- paste0(colnames(data.22)[c(8:172)],".22")

# Highly Annoyed Variable bilden
data.21$HA_12 <- ifelse(data.21$LFN_45_F15_b.21>3,1,0)
data.22$HA_6 <- ifelse(data.22$LFN_45_F15_b.22>3,1,0)


# Variablen erstellen, in der abgebildet, an welcher Welle Probanden teilgenommen haben
data.21$Welle1 <- 1
data.22$Welle2 <- 1


# Variablen, die in beiden Datensätzen gleich heißen, ID daraus löschen
common <- intersect(names(data.21), names(data.22))
common <- common[!common %in% "ID"] 

# Die beiden Datensätze werden zunächst anhand der ID gemerged
data <- full_join(data.21, data.22, by = "ID")


data$Welle1[is.na(data$Welle1)] <- 0 
data$Welle2[is.na(data$Welle2)] <- 0
data$Panel[is.na(data$Panel)] <- 0
#data$Panel1 <- ifelse(data$Welle1 == data$Welle2, 1, 0)


# Überprüfen, ob es noch doppelte IDs gibt
anzahl <- as.data.frame(table(data$ID))

# In data wurden nach dem join-Befeh (da dieser nur anhand der IDs erfolgte) an alle weiteren doppelten Spalten ein .x bzw .y angehängt
# Diese Spalten sollen nun kombiniert werden
add_up <- data %>% 
  select(paste0(common,".x"), paste0(common,".y"), ID) %>%
  mutate(ID_mopa = coalesce(ID_mopa.x, ID_mopa.y),  
         PLZ = coalesce(PLZ.x, PLZ.y),
         Ort = coalesce(Ort.x, Ort.y),
         Quotavariable = coalesce(Quotavariable.x, Quotavariable.y),
         Code_ZEUS = coalesce(Code_ZEUS.x, Code_ZEUS.y),
         tag_6_22 = coalesce(tag_6_22.x, tag_6_22.x),
         nacht_22_6 = coalesce(nacht_22_6.x, nacht_22_6.y),
         tag_6_22.wz = coalesce(tag_6_22.wz.x, tag_6_22.wz.y),
         nacht_22_6.wz = coalesce(nacht_22_6.wz.x, nacht_22_6.wz.y),
         Sportanlage = coalesce(Sportanlage.y, Sportanlage.x)) %>%
  select(common, ID)


data <- data %>%
  select(!c(paste0(common,".y"), paste0(common,".x"))) %>%
  full_join(add_up, by = "ID") %>%
  relocate(ID_mopa, PLZ, Ort, Quotavariable, Code_ZEUS, Sportanlage)


# Panel-Variable an Welle 1 anfügen
data.21 <- merge(x = data.21, y = data[, c("ID", "Panel")], by = "ID", all.x = TRUE)
  
  

#-----------------------------#
# 4. Daten speichern ####
#----------------------------#


write.xlsx(data.21, paste0(save.data,"Welle.1_2021.xlsx"))
write.xlsx(data.22, paste0(save.data,"Welle.2_2022.xlsx"))
write_sav(data.21, paste0(save.data,"Welle_1.sav"))
write_sav(data.22, paste0(save.data,"Welle_2.sav"))

write.xlsx(data, paste0(save.data,"Daten_gesamt.xlsx"))  
write_sav(data, paste0(save.data,"Daten_gesamt.sav"))


test <- data.21 %>% filter(!data.21$ID %in% data.22$ID)

test1 <- data.22 %>% filter(!data.22$ID %in% data.21$ID)

test2 <- data.22 %>% filter(data.22$ID %in% data.21$ID)
