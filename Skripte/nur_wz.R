# Datenaufbereitung: Wenn es für eine ID zwei Zeilen gibt (Endung _a und _b), soll leiglich die mit Endung a beibehalten werden

#---------------------------#
# 1. Settings und Datensätze
#---------------------------#

# cleaning environment
rm(list =ls()) 

#--------------------------#
## 1.1 Settings 
#--------------------------#

packages <- c("tidyverse", "dplyr", "openxlsx", "readxl", "ggplot2")

load.packages <- lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
)

rm(packages)

#--------------------------#
#* 1.2  Daten einlesen ####
#--------------------------#
 
source("read_data.r")


#-----------------------#
# 2. Daten bearbeiten ####
#----------------------#

#----------------------------#
## 2.1 Daten aufarbeiten ####
#---------------------------#


data <- Filter(function(x) is(x, "data.frame"), mget(ls()))
#change <- c(Messpunkt = "X1", ID = "X2", werk.8_20)

# Ersten beiden Zeilen löschen und ersten beiden Spalten benennen
# Alle Zeilen mit "_SZ" löschen
data <- data %>% map(~ filter(., !row_number() %in% c(1,2))) %>%
  map(~ rename(., "Messpunkt" = "X1", "ID" = "X2")) %>%
  map(~ filter(., !grepl("_b", ID)))
list2env(data, envir = .GlobalEnv)



# Liste mit Datensätzen für Werktage, Samstage und Sonntage
cities <- c("land_a", "land_b", "land_c", "land_d", "land_e", "land_f")
werktag <- list()
samstag <- list()
sonntag <- list()
verkehr <- list()

for (city in cities) {
  for (year in 21:22) {
    werktag[[paste0(city,".werk.",year)]] <- eval(parse(text = paste0(city,".werk.",year)))
    samstag[[paste0(city,".sa.",year)]] <- eval(parse(text = paste0(city,".sa.",year)))
    sonntag[[paste0(city,".so.",year)]] <- eval(parse(text = paste0(city,".so.",year)))
    verkehr[[paste0(city,".ver")]] <- eval(parse(text = paste0(city,".ver")))
  }
}

## Werktag
# Spalten für Tageszeiten unbenennen
werktag <- werktag %>%
  map(~ rename(., "werk_8_20" = "Werktag.(8-20h)", "werk_20_22" = "Werktag,.RZ.(20-22h)")) %>%
  map(~ mutate(., werk_8_20 = as.numeric(werk_8_20))) %>%
  map(~ mutate(., werk_20_22 = as.numeric(werk_20_22))) %>%
  map(~ mutate(., werk_8_20 = replace(werk_8_20, werk_8_20 <= 25, 25))) %>%
  map(~ mutate(., werk_20_22 = replace(werk_20_22, werk_20_22 <=25, 25))) %>%
  map(~ select(., !Messpunkt))


list2env(werktag, envir = .GlobalEnv)


## Samstag
# Bei den Samstagen gibt es verschiedene Versionen der Spaltennamen
# Für die Städte, die keine Spalte für 20-22 haben, diese noch ergänzen
for (city in cities) {
  for (year in 21:22) {
    if(isTRUE(length(samstag[[paste0(city,".sa.",year)]])==3)) {
      samstag[[paste0(city,".sa.",year)]] <- add_column(samstag[[paste0(city,".sa.",year)]], sa.20_22 = NA)
    }
  }
}



# Spalten für Tageszeiten unbenennen
samstag <- samstag %>%
  map(~ rename(., "sa_8_20" = 3, "sa_20_22" = 4)) %>%
  map(~ mutate(., sa_8_20 = as.numeric(sa_8_20))) %>%
  map(~ mutate(., sa_20_22 = as.numeric(sa_20_22))) %>%
  map(~ mutate(., sa_8_20 = replace(sa_8_20, sa_8_20 <= 25, 25))) %>%
  map(~ mutate(., sa_20_22 = replace(sa_20_22, sa_20_22 <=25, 25))) %>%
  map(~ select(., !Messpunkt))


list2env(samstag, envir = .GlobalEnv)


## Sonntag 
# Für die Städte, die keine Spalte für 20-22 haben, diese noch ergänzen
for (city in cities) {
  for (year in 21:22) {
    if(isTRUE(length(sonntag[[paste0(city,".so.",year)]])==4)) {
      sonntag[[paste0(city,".so.",year)]] <- add_column(sonntag[[paste0(city,".so.",year)]], so_20_22 = NA)
    }
  }
}

sonntag <- sonntag %>%
  map(~ rename(., "so.9_13__15_20h" = "Sonntag.(9-13h,15-20h)", "so_13_15" = "Sonntag,.RZ.(13-15h)", "so_20_22" = 5)) %>%
  map(~ mutate(., so.9_13__15_20h = as.numeric(so.9_13__15_20h))) %>%
  map(~ mutate(., so_13_15 = as.numeric(so_13_15))) %>%
  map(~ mutate(., so_20_22 = as.numeric(so_20_22))) %>%
  map(~ mutate(., so.9_13__15_20h = replace(so.9_13__15_20h, so.9_13__15_20h <= 25, 25))) %>%
  map(~ mutate(., so_13_15 = replace(so_13_15, so_13_15 <=25, 25))) %>%
  map(~ mutate(., so_20_22 = replace(so_20_22, so_20_22 <=25, 25))) %>%
  map(~ select(., !Messpunkt))


list2env(sonntag, envir = .GlobalEnv)


## Verkehr
verkehr <- verkehr %>%
  map(~ rename(., "tag_6_22" = "Tag.(6h-22h)", "nacht_22_6" = "Nacht.(22h-6h)")) %>%
  map(~ mutate(., tag_6_22 = as.numeric(tag_6_22))) %>%
  map(~ mutate(., nacht_22_6 = as.numeric(nacht_22_6))) %>%
  map(~ mutate(., tag_6_22 = replace(tag_6_22, tag_6_22 <= 25, 25))) %>%
  map(~ mutate(., nacht_22_6 = replace(nacht_22_6, nacht_22_6 <= 25, 25)))  %>%
  map(~ select(., !Messpunkt))

list2env(verkehr, envir = .GlobalEnv)



#---------------------------#
# 3.  Daten zusammenführen ####
#--------------------------#


werktag.21 <- list()
samstag.21 <- list()
sonntag.21 <- list()
werktag.22 <- list()
samstag.22 <- list()
sonntag.22 <- list()
verkehr <- list()

for (city in cities) {
  werktag.21[[paste0(city,".werk.21")]] <- eval(parse(text = paste0(city,".werk.21")))
  samstag.21[[paste0(city,".sa.21")]] <- eval(parse(text = paste0(city,".sa.21")))
  sonntag.21[[paste0(city,".so.21")]] <- eval(parse(text = paste0(city,".so.21")))
  werktag.22[[paste0(city,".werk.22")]] <- eval(parse(text = paste0(city,".werk.22")))
  samstag.22[[paste0(city,".sa.22")]] <- eval(parse(text = paste0(city,".sa.22")))
  sonntag.22[[paste0(city,".so.22")]] <- eval(parse(text = paste0(city,".so.22")))
  verkehr[[paste0(city,".ver")]] <- eval(parse(text = paste0(city,".ver")))
}


werktag.21 <- werktag.21 %>% 
  map(~ rename(., "werk_8_20_2021" = "werk_8_20")) %>%
  map(~ rename(., "werk_20_22_2021" = "werk_20_22"))

werktag.22 <- werktag.22 %>%
  map(~ rename(., "werk_8_20_2022" = "werk_8_20")) %>%
  map(~ rename(., "werk_20_22_2022" = "werk_20_22"))

samstag.21 <- samstag.21 %>%
  map(~ rename(., "sa_8_20_2021" = "sa_8_20")) %>%
  map(~ rename(., "sa_20_22_2021" = "sa_20_22"))

samstag.22 <- samstag.22 %>%
  map(~ rename(., "sa_8_20_2022" = "sa_8_20")) %>%
  map(~ rename(., "sa_20_22_2022" = "sa_20_22"))

sonntag.21 <- sonntag.21 %>%
  map(~ rename(., "so.9_13__15_20_2021" = "so.9_13__15_20h")) %>%
  map(~ rename(., "so_13_15_2021" = "so_13_15")) %>%
  map(~ rename(., "so_20_22_2021" = "so_20_22"))

sonntag.22 <- sonntag.22 %>%
  map(~ rename(., "so.9_13__15_20_2022" = "so.9_13__15_20h")) %>%
  map(~ rename(., "so_13_15_2022" = "so_13_15")) %>%
  map(~ rename(., "so_20_22_2022" = "so_20_22"))



werktag.21 <- do.call("rbind", werktag.21)
werktag.22 <- do.call("rbind", werktag.22)
samstag.21 <- do.call("rbind", samstag.21)
samstag.22 <- do.call("rbind", samstag.22)
sonntag.21 <- do.call("rbind", sonntag.21)
sonntag.22 <- do.call("rbind", sonntag.22)
verkehr <- do.call("rbind", verkehr)

werktag <- werktag.21 %>%
  full_join(werktag.22)

samstag <- samstag.21 %>%
  full_join(samstag.22)

sonntag <- sonntag.21 %>%
  full_join(sonntag.22)

data <- werktag %>% 
  full_join(samstag) %>%
  full_join(sonntag) %>%
  full_join(verkehr) %>%
  filter(duplicated(ID) == FALSE)




# Endung WZ löschen
data$ID <- gsub("_WZ","",as.character(data$ID))


# Datensätze für 2021 und 2022 erstellen
data.21 <- werktag.21 %>%
  full_join(samstag.21) %>%
  full_join(sonntag.21) %>%
  full_join(verkehr)
data.22 <- werktag.22 %>%
  full_join(samstag.22) %>% 
  full_join(sonntag.22) %>%
  full_join(verkehr)

data.21$ID <- gsub("_WZ","",as.character(data.21$ID))
data.22$ID <- gsub("_WZ","",as.character(data.22$ID))


colnames(data)[colnames(data) != "ID"] <- paste0(colnames(data)[colnames(data) != "ID"],".wz")
colnames(data.21)[colnames(data.21) != "ID"] <- paste0(colnames(data.21)[colnames(data.21) != "ID"],".wz")
colnames(data.22)[colnames(data.21) != "ID"] <- paste0(colnames(data.22)[colnames(data.22) != "ID"],".wz")


path.spss <- "Z:/Projekte/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Pegel_mopa/Pegel Sportanlagen NEU/"
#path.spss <- "C:/Zeus/SynologyDrive/219.003_UBA-Sportlaerm/3_Feldarbeit/4_Hauptbefragung/Pegel_mopa/Pegel Sportanlagen/"



write.xlsx(data, paste0(path.spss,"nur_wz.xlsx"))

write.xlsx(data.21, paste0(path.spss,"nur_wz_21.xlsx"))
write.xlsx(data.22, paste0(path.spss,"nur_wz_22.xlsx"))

