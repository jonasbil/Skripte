# Datenaufbereitung: Wenn es für eine ID zwei Zeilen gibt (Endung _a und _b), soll leiglich die mit dem höheren Wert beinehlaten werden

#---------------------------------#
# 1. Settings und Datensätze ####
#--------------------------------#

# cleaning environment
rm(list =ls()) 

#--------------------------#
#* 1.1 Settings ####
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

# Ersten beiden Zeilen löschen und ersten beiden Spalten benennen
# Endungen _a und _b löschen
data <- data %>% map(~ filter(., !row_number() %in% c(1,2))) %>%
  map(~ rename(., "Datum" = "X1", "ID" = "X2")) %>%
  map(~ mutate_at(.,
                  .vars = "ID",
                  .funs = gsub,
                  pattern = "(.*?)_a|_b",
                  replacement = "\\1")) 

list2env(data, envir = .GlobalEnv)




# Liste mit Datensätzen für die unterschiedlichen Jahre
cities <- c("landa_a", "landa_b", "landa_c", "landa_d", "landa_e", "landa_f")
j_2018 <- list()
j_2019 <- list()
j_2020 <- list()
j_2021 <- list()

for (city in cities) {
  for (year in 21:22) {
    J_2018[[paste0(city,".2018.",year)]] <- eval(parse(text = paste0(city,".2018.",year)))
    J_2019[[paste0(city,".2019.",year)]] <- eval(parse(text = paste0(city,".2019.",year)))
    J_2020[[paste0(city,".2020.",year)]] <- eval(parse(text = paste0(city,".2020.",year)))
    J_2021[[paste0(city,".2021")]] <- eval(parse(text = paste0(city,".2021")))
  }
}

## J_2018
# Spalten für Tageszeiten unbenennen
# Pegelwerte unter 25 mit 25 gleich setzen
J_2018 <- J_2018 %>%
  map(~ rename(., "werk_8_20" = "J_2018.(8-20h)", "werk_20_22" = "J_2018,.RZ.(20-22h)")) %>%
  map(~ mutate(., werk_8_20 = as.numeric(werk_8_20))) %>%
  map(~ mutate(., werk_20_22 = as.numeric(werk_20_22))) %>%
  map(~ mutate(., werk_8_20 = replace(werk_8_20, werk_8_20 <= 25, 25))) %>%
  map(~ mutate(., werk_20_22 = replace(werk_20_22, werk_20_22 <=25, 25)))
  

list2env(J_2018, envir = .GlobalEnv)


## J_2019
# Bei den Samstagen gibt es verschiedene Versionen der Spaltennamen
# Für die Städte, die keine Spalte für 20-22 haben, diese noch ergänzen
for (city in cities) {
  for (year in 21:22) {
    if(isTRUE(length(J_2019[[paste0(city,".2019.",year)]])==3)) {
      J_2019[[paste0(city,".2019.",year)]] <- add_column(J_2019[[paste0(city,".2019.",year)]], sa.20_22 = NA)
    }
  }
}



# Spalten für Tageszeiten unbenennen
J_2019 <- J_2019 %>%
  map(~ rename(., "sa_8_20" = 3, "sa_20_22" = 4)) %>%
  map(~ mutate(., sa_8_20 = as.numeric(sa_8_20))) %>%
  map(~ mutate(., sa_20_22 = as.numeric(sa_20_22))) %>%
  map(~ mutate(., sa_8_20 = replace(sa_8_20, sa_8_20 <= 25, 25))) %>%
  map(~ mutate(., sa_20_22 = replace(sa_20_22, sa_20_22 <=25, 25)))


list2env(J_2019, envir = .GlobalEnv)


## J_2020 
# Für die Städte, die keine Spalte für 20-22 haben, diese noch ergänzen
for (city in cities) {
  for (year in 21:22) {
    if(isTRUE(length(J_2020[[paste0(city,".2020.",year)]])==4)) {
      J_2020[[paste0(city,".2020.",year)]] <- add_column(J_2020[[paste0(city,".2020.",year)]], so_20_22 = NA)
    }
  }
}

J_2020 <- J_2020 %>%
  map(~ rename(., "so.9_13__15_20h" = "Sonntag.(9-13h,15-20h)", "so_13_15" = "Sonntag,.RZ.(13-15h)", "so_20_22" = 5)) %>%
  map(~ mutate(., so.9_13__15_20h = as.numeric(so.9_13__15_20h))) %>%
  map(~ mutate(., so_13_15 = as.numeric(so_13_15))) %>%
  map(~ mutate(., so_20_22 = as.numeric(so_20_22))) %>%
  map(~ mutate(., so.9_13__15_20h = replace(so.9_13__15_20h, so.9_13__15_20h <= 25, 25))) %>%
  map(~ mutate(., so_13_15 = replace(so_13_15, so_13_15 <=25, 25))) %>%
  map(~ mutate(., so_20_22 = replace(so_20_22, so_20_22 <=25, 25)))
  
  
list2env(J_2020, envir = .GlobalEnv)


## J_2021
J_2021 <- J_2021 %>%
  map(~ rename(., "tag_6_22" = "Tag.(6h-22h)", "nacht_22_6" = "Nacht.(22h-6h)")) %>%
  map(~ mutate(., tag_6_22 = as.numeric(tag_6_22))) %>%
  map(~ mutate(., nacht_22_6 = as.numeric(nacht_22_6))) %>%
  map(~ mutate(., tag_6_22 = replace(tag_6_22, tag_6_22 <= 25, 25))) %>%
  map(~ mutate(., nacht_22_6 = replace(nacht_22_6, nacht_22_6 <= 25, 25))) 

list2env(J_2021, envir = .GlobalEnv)


#---------------------------#
## 2.2 Höchste Pegel ####
#--------------------------#

## Teildatensätze für jeden Zeitraum erstellen, Datensätze nach Pegel und ID sortieren (die höheren Pegel werden zuerst) aufgeführt
## Dopplungen (nach IDs) löschen
## Datensätze anschließend wieder zusammen führen


# J_2018  
J_2018_a <- J_2018 %>% map(~ select(., ID, werk_20_22)) %>%
  map(~ arrange(., ID, desc(werk_20_22))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
  names(J_2018_a) <- paste0(names(J_2018_a),".1")
list2env(J_2018_a, envir = .GlobalEnv)

J_2018 <- J_2018 %>% map(~ select(., ID, werk_8_20)) %>%
  map(~ arrange(., ID, desc(werk_8_20))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
list2env(J_2018, envir = .GlobalEnv)


for (city in cities) {
  assign(paste0(city,".2018.21"), eval(parse(text = paste0(city,".2018.21"))) %>%
           full_join(eval(parse(text = paste0(city,".2018.21.1")))))
  assign(paste0(city,".2018.22"), eval(parse(text = paste0(city,".2018.22"))) %>%
           full_join(eval(parse(text = paste0(city,".2018.22.1")))))
  
}
  
rm(J_2018_a, landa_a.2018.21.1, landa_a.2018.22.1, landa_b.2018.21.1, landa_b.2018.22.1, landa_c.2018.21.1, landa_c.2018.22.1, landa_e.2018.21.1, 
   landa_e.2018.22.1, landa_d.2018.21.1, landa_d.2018.22.1, landa_f.2018.21.1, landa_f.2018.22.1)


# J_2019 
J_2019_a <- J_2019 %>% map(~ select(., ID, sa_20_22)) %>%
  map(~ arrange(., ID, desc(sa_20_22))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
names(J_2019_a) <- paste0(names(J_2019_a),".1")
list2env(J_2019_a, envir = .GlobalEnv)

J_2019 <- J_2019 %>% map(~ select(., ID, sa_8_20)) %>%
  map(~ arrange(., ID, desc(sa_8_20))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
list2env(J_2019, envir = .GlobalEnv)


for (city in cities) {
  assign(paste0(city,".2019.21"), eval(parse(text = paste0(city,".2019.21"))) %>%
           full_join(eval(parse(text = paste0(city,".2019.21.1")))))
  assign(paste0(city,".2019.22"), eval(parse(text = paste0(city,".2019.22"))) %>%
           full_join(eval(parse(text = paste0(city,".2019.22.1")))))
  
}

rm(J_2019_a, landa_a.2019.21.1, landa_a.2019.22.1, landa_b.2019.21.1, landa_b.2019.22.1, landa_c.2019.21.1, landa_c.2019.22.1, landa_e.2019.21.1, 
   landa_e.2019.22.1, landa_d.2019.21.1, landa_d.2019.22.1, landa_f.2019.21.1, landa_f.2019.22.1)


# J_2020
J_2020_a <- J_2020 %>% map(~ select(., ID, so_13_15)) %>%
  map(~ arrange(., ID, desc(so_13_15))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
names(J_2020_a) <- paste0(names(J_2020_a),".1")
list2env(J_2020_a, envir = .GlobalEnv)

J_2020_b <- J_2020 %>% map(~ select(., ID, so_20_22)) %>%
  map(~ arrange(., ID, desc(so_20_22))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
names(J_2020_b) <- paste0(names(J_2020_b),".2")
list2env(J_2020_b, envir = .GlobalEnv)


J_2020 <- J_2020 %>% map(~ select(., ID, so.9_13__15_20h)) %>%
  map(~ arrange(., ID, desc(so.9_13__15_20h))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
list2env(J_2020, envir = .GlobalEnv)


for (city in cities) {
  assign(paste0(city,".2020.21"), eval(parse(text = paste0(city,".2020.21"))) %>%
           full_join(eval(parse(text = paste0(city,".2020.21.1")))) %>%
           full_join(eval(parse(text = paste0(city,".2020.21.2"))))) 
  assign(paste0(city,".2020.22"), eval(parse(text = paste0(city,".2020.22"))) %>%
           full_join(eval(parse(text = paste0(city,".2020.22.1")))) %>%
           full_join(eval(parse(text = paste0(city,".2020.22.2")))))
  
}

rm(J_2020_a, J_2020_b, landa_a.2020.21.1, landa_a.2020.22.1, landa_b.2020.21.1, landa_b.2020.22.1, landa_c.2020.21.1, landa_c.2020.22.1, landa_e.2020.21.1, 
   landa_e.2020.22.1, landa_d.2020.21.1, landa_d.2020.22.1, landa_f.2020.21.1, landa_f.2020.22.1,
   landa_a.2020.21.2, landa_a.2020.22.2, landa_b.2020.21.2, landa_b.2020.22.2, landa_c.2020.21.2, landa_c.2020.22.2, landa_e.2020.21.2, 
   landa_e.2020.22.2, landa_d.2020.21.2, landa_d.2020.22.2, landa_f.2020.21.2, landa_f.2020.22.2)


# J_2021
J_2021_a <- J_2021 %>% map(~ select(., ID, nacht_22_6)) %>%
  map(~ arrange(., ID, desc(nacht_22_6))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
names(J_2021_a) <- paste0(names(J_2021_a),".1")
list2env(J_2021_a, envir = .GlobalEnv)

J_2021 <- J_2021 %>% map(~ select(., ID, tag_6_22)) %>%
  map(~ arrange(., ID, desc(tag_6_22))) %>%
  map(~ filter(., duplicated(ID) == FALSE))
list2env(J_2021, envir = .GlobalEnv)
  
for (city in cities) {
  assign(paste0(city,".2021"), eval(parse(text = paste0(city,".2021"))) %>%
           full_join(eval(parse(text = paste0(city,".2021.1")))))
}

  

#---------------------------#
# 3.  Daten zusammenführen ####
#--------------------------#
# Halbjahre

J_2018_1 <- list()
J_2019_1 <- list()
J_2020_1 <- list()
J_2018_2 <- list()
J_2019_2 <- list()
J_2020_2 <- list()
J_2021 <- list()

for (city in cities) {
    J_2018_1[[paste0(city,".2018.21")]] <- eval(parse(text = paste0(city,".2018.21")))
    J_2019_1[[paste0(city,".2019.21")]] <- eval(parse(text = paste0(city,".2019.21")))
    J_2020_1[[paste0(city,".2020.21")]] <- eval(parse(text = paste0(city,".2020.21")))
    J_2018_2[[paste0(city,".2018.22")]] <- eval(parse(text = paste0(city,".2018.22")))
    J_2019_2[[paste0(city,".2019.22")]] <- eval(parse(text = paste0(city,".2019.22")))
    J_2020_2[[paste0(city,".2020.22")]] <- eval(parse(text = paste0(city,".2020.22")))
    J_2021[[paste0(city,".2021")]] <- eval(parse(text = paste0(city,".2021")))
}


J_2018_1 <- J_2018_1 %>% 
  map(~ rename(., "werk_8_20_2021" = "werk_8_20")) %>%
  map(~ rename(., "werk_20_22_2021" = "werk_20_22"))

J_2018_2 <- J_2018_2 %>%
  map(~ rename(., "werk_8_20_2022" = "werk_8_20")) %>%
  map(~ rename(., "werk_20_22_2022" = "werk_20_22"))

J_2019_1 <- J_2019_1 %>%
  map(~ rename(., "sa_8_20_2021" = "sa_8_20")) %>%
  map(~ rename(., "sa_20_22_2021" = "sa_20_22"))

J_2019_2 <- J_2019_2 %>%
  map(~ rename(., "sa_8_20_2022" = "sa_8_20")) %>%
  map(~ rename(., "sa_20_22_2022" = "sa_20_22"))

J_2020_1 <- J_2020_1 %>%
  map(~ rename(., "so.9_13__15_20_2021" = "so.9_13__15_20h")) %>%
  map(~ rename(., "so_13_15_2021" = "so_13_15")) %>%
  map(~ rename(., "so_20_22_2021" = "so_20_22"))

J_2020_2 <- J_2020_2 %>%
  map(~ rename(., "so.9_13__15_20_2022" = "so.9_13__15_20h")) %>%
  map(~ rename(., "so_13_15_2022" = "so_13_15")) %>%
  map(~ rename(., "so_20_22_2022" = "so_20_22"))



J_2018_1 <- do.call("rbind", J_2018_1)
J_2018_2 <- do.call("rbind", J_2018_2)
J_2019_1 <- do.call("rbind", J_2019_1)
J_2019_2 <- do.call("rbind", J_2019_2)
J_2020_1 <- do.call("rbind", J_2020_1)
J_2020_2 <- do.call("rbind", J_2020_2)
J_2021 <- do.call("rbind", J_2021)

J_2018 <- J_2018_1 %>%
  full_join(J_2018_2)

J_2019 <- J_2019_1 %>%
  full_join(J_2019_2)

J_2020 <- J_2020_1 %>%
  full_join(J_2020_2)

data <- J_2018 %>% 
  full_join(J_2019) %>%
  full_join(J_2020) %>%
  full_join(J_2021)

path.data <- as.character(getwd())

write.xlsx(data, paste0(path.data,"Pegelwerte_höchste.xlsx"))

# Datensätze für 2021 und 2022 erstellen
data.21 <- J_2018_1 %>%
  full_join(J_2019_1) %>%
  full_join(J_2020_1) %>%
  full_join(J_2021)

data.22 <- J_2018_2 %>%
  full_join(J_2019_2) %>% 
  full_join(J_2020_2) %>%
  full_join(J_2021)

write.xlsx(data.21, paste0(path.spss,"Pegelwerte_hoechste.21.xlsx"))
write.xlsx(data.22, paste0(path.spss,"Pegelwerte_hoechste.22.xlsx"))
