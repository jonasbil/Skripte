
# In dieser Arbeit sollen die Übergangsquoeten von Grundschulen auf Weiterführende Schulen in Nordrhein-Westfalen auf Kreisebene untersucht werden.
# Dabei werden die Übergänge auf Schulebene für die Jahre 2016 bis 2018 auf Kreisebene (Kreise und kreisfreie Städte) aggregiert.
# Als Datengrundlage dient das Forschungsprojekt "Bildungs- und Qualifikationsraum Ruhr 2040".
# Hier wurden u.a. für die Jahre 2015 bis 2018 die Übergänge von Grundschulen auf Weiterführende Schulen auf Schulebene erhoben.
# Berücksichtigt werden hier Versetzungen von der 4. Stufe in die 5. Stufe von Schulen aus jeweils Nordrhein-Westfalen. 
# Um auf Kreisebene aggregieren zu können, werden die Übergänge von allen Grundschulen eines Kreises summiert.
# Dabei wird unterschieden nach den unterschiedlichen Schulformen, sodass die Gesamtanzahl aller Übergänge pro Schulform erfasst wird.
# Anhand der Anzahl der Übergänge in die jeweiligen Schulform und der Gesamtanzahl aller Übergänge in einem Kreis kann für jede Schulform eine Übergangsquote bestimmt werden. 
# Dies geschieht für jedes Jahr des Untersuchungszeitraums (2015-2018). 
# Abschließend wird für jede Schulform die durchschnittliche Quote für die jeweiligen Jahre ermittelt. 
# Kreise/kreisf. Städte werden dabei anhand der amtlichen Schlüsselnummer zugeordnet. 
# Die berechneten Quoten sollen anschließend visualisiert werden. 
# Dafür werden mit den Paketen ggplot2 und sf Shapefiles von NRW erstellt in dem die Kreise nach den Übergangsquoten eingefärbt. 



#-----------------------------#
#--- Übergangsquoten ---#
#-----------------------------#

# R Version überprüfen
# Hier verwendet: "R version 4.2.0 (2022-05-28)"
R.Version()

# ggf. Aktuellste Version installieren
#installr::updateR()

# Installierte CRAN Pakete aktualisieren
#update.packages(ask=F) 

#------------------------------------#    
# 0. Settings und Datensätze ####
#------------------------------------#

# cleaning environment
rm(list =ls()) 


#------------------#
## 0.1 Settings ####
#------------------#

# Benötigte Pakete installieren/aktualisieren 

packages <- c("tidyverse", "dplyr", "ggplot2", "ggpubr", "openxlsx", 
              "data.table", "sf", "rgdal", "tigris", "plotly", "ggiraph", "viridis")


lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})


# Arbeitspfade/Directionary festlegen
path.data <- getwd()


rm(packages)


#--------------------------#
## 0.2 Datensätze einlesen 
#--------------------------#


## Regionen
# "regionen" ordnet jede amtlichen Schlüsselnummer eine der drei Regionen (Ruhrgebiet, Rheinland, Westfalen zu) 
regionen <- read.csv2(paste0(path.data,"Regionen NRW.csv"))  %>%
  select(c(AGS.Kreis, Zuordnung)) %>%
  mutate(AGS.Kreis = substring(AGS.Kreis,2,5)) %>%
  rename(asn.from = AGS.Kreis)


# Die transitions-Datensätze für 2015-2018 werden eingelesen. Diese stammen aus dem Forschungsprojekt "Bildungs- und Qualifikationsraum Ruhr 2040".
# Hier werden alle Übergange auf jeden Jahrgang jeder Schule unterteilt u.a. nach Herkunftsschule, Herkunftsjahrgang und Übergangsart aufgeführt. 
# Die Schulen werden mit einer ID und der amtlichen Schlüsselnummer des Kreises gekennzeichnet. 
# Die IDs, amtlichen Schlüsselnummern, Jahrgänge, Übergangsarten und Schulformen werden dabei jeweils für die Herkunfts- und auch für die Zugangsschule ausgegeben.  
# Für jedes Jahr wird jeweils ein transition-Datensatz erstellt. 

for (year in 2015:2018) {
  assign(paste0("transitions.",year),
         data.table(readRDS(paste0(path.data,"transitions.",year,".rds"))) %>%
           mutate(asn.from = as.character(asn.from)) %>%
           full_join(regionen) %>%
           rename(Region = Zuordnung),
         envir = .GlobalEnv)
}




#--------------------------#
# 1. Datenaufbereitung ####
#--------------------------#
# Hier werden die transition-Datensätze aufbereitet. Dafür werden für jedes Jahr neue Datensätze ("data") erstellt. 
#1.  Schleife für die Jahre 2015-2018
#2.  mit dem assign-Befehl können innerhalb der Schleife mehrere Elemente erstellt werden
#3.  mit "eval" werden die transition-Datensätze als "Quelle" angesprochen
#4.  Um Übergangsquoten von Grundschulen auf weiterführende Schulen bestimmen zu könnnen, werden die Herkunftsjahrgänge auf Stufe 4 und
#    die Übergangsarten auf "Versetzung" gefiltert. Bei den amtlichen Herkunftsschlüsseln werden Regionen aus dem Ausland ausgefiltert. 
#5.  Für die data-Datensätze werden zunächst von den transitions-Datensätzen nur die die Anzahl der Übergänge, die Urpsrungs-asn und die Schulform übernommen
#6.  Nach asn.from und form.to gruppieren
#7.  Übergänge pro Kreis und Schulform summieren:
#    Die Gesamtsumme aller Übergänge auf die verschiedenen Schulformen werden für jeden Kreis berechnet
#8.  Gruppierung auf asn.from beschränken
#9.  Neue Spalte erstellen: Für jeden Kreis wird die Gesamtsumme aller Übergänge auf weiterführende Schulen berechnet
#10. Zusätzliche Spalten: Summe aller Übergänge auf jeweilige Schulform für jeden Kreis
#11. Übergangsquoten für die Schulformen: Pro Kreis Summe der Übergänge für jeweulige Schulformen durch Gesamtsumme aller Übergänge dividieren
#12. Spalte "alle" löschen
#13. DAtensätze ins Environment übertragen
### Für die Jahre 2015 bis 2018 wurden pro Kreis die Übergangsquoten auf alle Schulformen berechnet


for (year in 2015:2018) {    #1.
  assign(paste0("data",year), #2. 
         eval(parse(text = paste0("transitions.",year))) %>% #3. 
           filter(jg.from %in% "04" & transition.type %in% "V" & !asn.from %in% c(980, 991, 993, 999)) %>% #4. 
           select(c(asn.from, transitioners, form.to)) %>% #5. 
           group_by(asn.from, form.to) %>% # 6. 
           summarise(transitioners = sum(transitioners)) %>% #7. 
           ungroup() %>% #8. 
           group_by(asn.from) %>%
           summarise(alle = sum(transitioners), #9. 
                     Hauptschule = sum(transitioners[form.to==2]), #10.
                     Volksschule = sum(transitioners[form.to==3]),
                     foer.grund.haupt = sum(transitioners[form.to==4]),
                     foer.real.gym = sum(transitioners[form.to==5]),
                     Realschule = sum(transitioners[form.to==6]),
                     prim = sum(transitioners[form.to==7]),
                     sek = sum(transitioners[form.to==8]),
                     Gesamtschule = sum(transitioners[form.to==9]),
                     Gemeinschaftsschule = sum(transitioners[form.to==10]),
                     Waldorfschule = sum(transitioners[form.to==11]),
                     Gymnasium = sum(transitioners[form.to==12])) %>%
           mutate(Hauptschule = Hauptschule/alle, #11.
                  Volksschule = Volksschule/alle,
                  foer.grund.haupt = foer.grund.haupt/alle,
                  foer.real.gym = foer.real.gym/alle,
                  Realschule = Realschule/alle,
                  prim = prim/alle,
                  sek = sek/alle,
                  Gesamtschule = Gesamtschule/alle,
                  Gemeinschaftsschule = Gemeinschaftsschule/alle,
                  Waldorfschule = Waldorfschule/alle,
                  Gymnasium = Gymnasium/alle,
                  asn.from = as.character(asn.from)) %>%
           select(!c(alle)), #12.
         envir = .GlobalEnv) #13. 
}


# Das gleiche wird auf Regionen-Ebene durchgeführt (also die drei Regionen Ruhrgebiet, Rheinland und Westfalen)
# Hier werden alle Übergänge einer Region auf eine Schulform durch die Gesamtübergänge in einer Region dividiert. 
for (year in 2015:2018) {
  assign(paste0("dat_reg",year),
         eval(parse(text = paste0("transitions.",year))) %>%
           filter(jg.from %in% "04" & transition.type %in% "V" & !asn.from %in% c(980, 991, 993, 999)) %>%
           select(Region, transitioners, form.to) %>%
           group_by(Region, form.to) %>%
           summarise(transitioners = sum(transitioners)) %>%
           ungroup() %>%
           group_by(Region) %>%
           summarise(alle = sum(transitioners), 
                     Hauptschule = sum(transitioners[form.to==2]), 
                     Volksschule = sum(transitioners[form.to==3]),
                     foer.grund.haupt = sum(transitioners[form.to==4]),
                     foer.real.gym = sum(transitioners[form.to==5]),
                     Realschule = sum(transitioners[form.to==6]),
                     prim = sum(transitioners[form.to==7]),
                     sek = sum(transitioners[form.to==8]),
                     Gesamtschule = sum(transitioners[form.to==9]),
                     Gemeinschaftsschule = sum(transitioners[form.to==10]),
                     Waldorfschule = sum(transitioners[form.to==11]),
                     Gymnasium = sum(transitioners[form.to==12])) %>%
           mutate(Hauptschule = Hauptschule/alle, 
                  Volksschule = Volksschule/alle,
                  foer.grund.haupt = foer.grund.haupt/alle,
                  foer.real.gym = foer.real.gym/alle,
                  Realschule = Realschule/alle,
                  prim = prim/alle,
                  sek = sek/alle,
                  Gesamtschule = Gesamtschule/alle,
                  Gemeinschaftsschule = Gemeinschaftsschule/alle,
                  Waldorfschule = Waldorfschule/alle,
                  Gymnasium = Gymnasium/alle) %>%
           select(!c(alle)) %>% 
           rename(Name = Region),
         envir = .GlobalEnv)
}



rm(transitions.2015, transitions.2016, transitions.2017, transitions.2018)

# Hier werden aus den Jahren 2015 bis 2018 die durchschnittlichen Übergangsquoten auf Kreisebene erstellt
# Datensatz "data" enthält die durchschnittlichen Übergangsquoten
data <- rbindlist(list(data2015, data2016, data2017, data2018))[,lapply(.SD,mean), list(asn.from)]
dat_reg <- rbindlist(list(dat_reg2015, dat_reg2016,dat_reg2017, dat_reg2018))[,lapply(.SD,mean), list(Name)]

# data auf drei Dezimalstellen reduzieren
data[,2:12] <- round(data[,2:12], digits = 3)
dat_reg[,2:12] <- round(dat_reg[,2:12], digits = 3)

# Spalte mit "Kreis" bzw. "Region" hinzufügen
data$Typ <- "Kreis"
dat_reg$Typ <- "Region"


rm(dat_reg2015, dat_reg2016, dat_reg2017, dat_reg2018, data2015, data2016, data2017, data2018)

#--------------------------#
# 2. Shape-File ####
#--------------------------#

#--------------------------#
## 2.1 Preparation ####
#--------------------------#

# Shape-Files für NRW (Kreisebene) einlesen und via ggplot anzeigen lassen
# Shape-file sind Datensätze, die auch geometrische Daten enthalten
shape_file_nrw <- read_sf(dsn = ".",layer = "shape_file_nrw") #Pfad ggf. ändern
ggplot(shape_file_nrw) + geom_sf() # mit geom_sf können shape-files mit ggplot herausgegeben werden

# asn.from anpassen: ersten beiden Ziffern (05) löschen
shape_file_nrw <- shape_file_nrw %>%
  mutate(asn_from = substring(asn_from, 3, 5)) %>%
  rename(asn.from = asn_from)


# Neuen shape-file erstellen: Datensatz data an shape-file-nrw mergen (via asn_from) 
shape_file_nrw_kreis <- geo_join(spatial_data = shape_file_nrw, data_frame = data, by_sp = "asn.from", by_df = "asn.from", how = "inner")

# ggplot 
ggplot(shape_file_nrw_kreis) + geom_sf()


# Optional: Die Quoten können auch in Prozentwerten ausgegeben werden
shape_file_nrw_kreis <- shape_file_nrw_kreis %>%
  mutate_at(vars(Hauptschule:Gymnasium),
            .funs = funs(. * 100))



#--------------------------#
## 2.2 Shape-File: Kreis ####
#--------------------------#

# Dezimalstellen auf 1 beschränken (falls vorher mit 100 multipliziert wurde)
# Dies dient später der optischen Darstellung
#shape_file_nrw_kreis[,8:18] <- round(shape_file_nrw_kreis[,8:18], digits = 1)

## Verschiedene Varianten für Plots werden hier mit beispielhaft mit den Übergangsquoten für Gymnasien erläutert
# Einfacher Plot mit angezeigten Labeln
# Problem: Bei 52 Kreisen wird die Graphik unübersichtlich, wenn jeder mit dem Label gekennzeichnet wird, und nicht jedes Label wird angezeigt
ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Gymnasium)) + 
  geom_sf_text(aes(label = Name), check_overlap = TRUE, color = "white") + 
  theme(panel.grid.major = element_line(colour = "transparent"), # Hier werden die Gitterlinien entfernt des shape-files entfernt
        axis.text.x = element_blank(), # Beschriftung der Achsen mit Längen- und Breitengraden entfernen
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + # Achsenabschnitte entfernen
  labs(x = "", y = "")

ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(color = Gymnasium)) + 
  #geom_sf_text(aes(label = Name), check_overlap = TRUE, color = "white") + 
  theme(panel.grid.major = element_line(colour = "transparent"), # Hier werden die Gitterlinien entfernt des shape-files entfernt
        axis.text.x = element_blank(), # Beschriftung der Achsen mit Längen- und Breitengraden entfernen
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + # Achsenabschnitte entfernen
  labs(x = "", y = "")

# Um die Graphiken übersichtlicher zu gestalten, wird den Kreisen eine Nummer zugewiesen, die anstelle der Namen der Kreise eingeblendet werden
shape_file_nrw_kreis$number <- 1:53

ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Gymnasium)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white") + 
  theme(panel.grid.major = element_line(colour = "transparent"), # Hier werden die Gitterlinien entfernt des shape-files entfernt
        axis.text.x = element_blank(), # Beschriftung der Achsen mit Längen- und Breitengraden entfernen
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + # Achsenabschnitte entfernen
  labs(x = "", y = "")


ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Hauptschule)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white") + 
  theme(panel.grid.major = element_line(colour = "transparent"), # Hier werden die Gitterlinien entfernt des shape-files entfernt
        axis.text.x = element_blank(), # Beschriftung der Achsen mit Längen- und Breitengraden entfernen
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + # Achsenabschnitte entfernen
  labs(x = "", y = "")


path.pics <- paste0(path.data,"Bilder/")


# Für Gymnasium
ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Gymnasium)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "black", size = 3) + 
  scale_fill_viridis_c(limits=c(0, 52), option = "C") +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        plot.margin = margin(2, 2, 2, 2)) + 
  labs(x = "", y = "") 

ggsave(paste0(path.pics,"Gymnasium_Kreis.png"), width = 18, height = 14, units = "cm")

# Für Hauptschule
ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Hauptschule)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white", size = 3) + 
  scale_fill_viridis_c(limits=c(0, 52), option = "C") +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(x = "", y = "")

ggsave(paste0(path.pics,"Hauptschule_Kreis.png"), width = 18, height = 14, units = "cm")



# Für Realschule
ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Realschule)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white", size = 3) + 
  scale_fill_viridis_c(limits=c(0, 52), option = "C") +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(x = "", y = "") 

ggsave(paste0(path.pics,"Realschule_Kreis.png"), width = 18, height = 14, units = "cm")



# Für Gesamtschule
ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Gesamtschule)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white", size = 3) + 
  scale_fill_viridis_c(limits=c(0, 52), option = "C") +
  theme(panel.grid.major = element_line(colour = "transparent"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + 
  labs(x = "", y = "") 

ggsave(paste0(path.pics,"Gesamtschule_Kreis.png"), width = 18, height = 14, units = "cm")



# Interaktive Graphik: Hier wird die Bezeichnung des Kreises angezeigt, wenn man den Mause-Curser auf das jeweilige Feld bewegt 

# Gymnasium
gg <- ggplot(shape_file_nrw_kreis) +
  geom_sf_interactive(size = 0.1, aes(fill = Gymnasium, data_id = Name, tooltip = paste0(Name,"; ",Gymnasium,"%"))) + # mit tooltip kann bestimmt werden, welche Information eingeblendet wird
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "", y = "")
gym <- girafe(ggobj = gg) # mit girafe wird der ggplot interkativ
gym


# Realschule
gg <- ggplot(shape_file_nrw_kreis) + 
  geom_sf_interactive(size = 0.1, aes(fill = Realschule, data_id = Name, tooltip = paste0(Name,"; ",Realschule,"%"))) + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "", y = "")
real <- girafe(ggobj = gg)
real



# Gesamtschuke
gg <- ggplot(shape_file_nrw_kreis) + 
  geom_sf_interactive(size = 0.1, aes(fill = Gesamtschule, data_id = Name, tooltip = paste0(Name,"; ",Gesamtschule,"%"))) + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = ", y = ")
gesamt <- girafe(ggobj = gg)
gesamt



# Hauptschule
gg <- ggplot(shape_file_nrw_kreis) +
  geom_sf_interactive(size = 0.1, aes(fill = Hauptschule, data_id = Name, tooltip = paste0(Name,"; ",Hauptschule,"%"))) + # mit tooltip kann bestimmt werden, welche Information eingeblendet wird
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "", y = "")
haupt <- girafe(ggobj = gg) # mit girafe wird der ggplot interkativ
haupt



#--------------------------#
## 2.3 Shape-File: Region ####
#--------------------------#

shape_file_nrw_reg <- shape_file_nrw %>%
  full_join(regionen) %>%
  select(c(geometry, Zuordnung)) %>%
  rename(Name = Zuordnung) %>%
  group_by(Name) %>%
  summarise() %>%
  full_join(dat_reg) %>%
  mutate_at(vars(Hauptschule:Gymnasium),
            .funs = funs(. * 100))
  
ggplot(shape_file_nrw_reg) + geom_sf()


shape_file_nrw <- shape_file_nrw_kreis %>%
  select(!c(GF, BSG, ARS, RS, number, asn.from)) %>%
  rbind(shape_file_nrw_reg)
  


ggplot(shape_file_nrw_reg) +
  geom_sf(aes(fill = Realschule)) + 
  geom_sf_text(aes(label = paste0(Name," ",Realschule,"%")), color = "white") + 
 # scale_fill_distiller(palette = "Oranges", direction = 1) +
  theme(panel.grid.major = element_line(colour = "transparent"), # Hier werden die Gitterlinien entfernt des shape-files entfernt
        axis.text.x = element_blank(), # Beschriftung der Achsen mit Längen- und Breitengraden entfernen
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + # Achsenabschnitte entfernen
  labs(x = "", y = "")


# Für Gymnasium
  ggplot(shape_file_nrw_reg) +
    geom_sf(aes(fill = Gymnasium)) +
    geom_sf_text(aes(label = paste0(Name," ",Gymnasium,"%")), color = "black", size = 3) +
    scale_fill_viridis_c(limits=c(0, 52), option = "C") + 
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "")
  
  ggsave(paste0(path.pics,"Gymnasium_reg.png"), width = 18, height = 14, units = "cm")

  
  # Für Realschule
  ggplot(shape_file_nrw_reg) +
    geom_sf(aes(fill = Realschule)) +
    geom_sf_text(aes(label = paste0(Name," ",Realschule,"%")), color = "white", size = 3) +
    scale_fill_viridis_c(limits=c(0, 52), option = "C") + 
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "")
  
  ggsave(paste0(path.pics,"Realschule_reg.png"), width = 18, height = 14, units = "cm")
  
  
  # Für Gesamtschule
  ggplot(shape_file_nrw_reg) +
    geom_sf(aes(fill = Gesamtschule)) +
    geom_sf_text(aes(label = paste0(Name," ",Gesamtschule,"%")), color = "white", size = 3) +
    scale_fill_viridis_c(limits=c(0, 52), option = "C") + 
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks = element_blank()) +
    labs(x = "", y = "")
  
  
  ggsave(paste0(path.pics,"Gesamtschule_reg.png"), width = 18, height = 14, units = "cm")
  
  
  # Für Hauptschule
  ggplot(shape_file_nrw_reg) +
    geom_sf(aes(fill = Hauptschule)) +
    geom_sf_text(aes(label = paste0(Name," ",Hauptschule,"%")), color = "white") +
    scale_fill_viridis_c(limits=c(0, 52), option = "C") + 
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "")
  
  ggsave(paste0(path.pics,"Hauptschule_reg.png"), width = 18, height = 14, units = "cm")
  
  
  
#--------------------------#
# 3 Übersichts-Tabelle ####
#--------------------------#

# Tabelle erstellen, in der die Kreise die jeweiligen Nummern und Regionen zugeordnet werden
path.table <- "C:/Users/jonas/sciebo2/Uni/11. Semester/Räumliche Statistik/Hausarbeit/Uebergangsquoten/"
  
tabelle <- shape_file_nrw_kreis %>%
  st_drop_geometry() %>%
  select(asn.from, number, Name) %>%
  full_join(regionen) %>%
  select(!(asn.from)) %>%
  rename(Nummer = number, 
         Region = Zuordnung)


#write.xlsx(tabelle, paste0(path.table,"Zuordnung.xlsx"))


# Tabellen mit Übergangsquoten auf einzelnen Schulformnen
  table_gym_kreis <- shape_file_nrw_kreis %>%
    st_drop_geometry() %>%
    select(number, Name, Gymnasium)
  
  write.xlsx(table_gym_kreis, paste0(path.table,"Gymnasium_Kreis.xlsx"))
  
  
  
  
  
  

# Erstelle eine benutzerdefinierte Farbskala, die von blau (1) bis rot (10) geht
# Erstelle eine benutzerdefinierte Farbskala, die von blau (1) bis rot (50) geht
common_scale <- scale_fill_gradient(low = "blue", high = "red", limits = c(1, 50), breaks = seq(1, 50, by = 10))

# Plot für "Hauptschule"
plot_hauptschule <- ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Hauptschule)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white") + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + 
  labs(x = "", y = "") +
  common_scale

# Plot für "Gymnasium"
plot_gymnasium <- ggplot(shape_file_nrw_kreis) +
  geom_sf(aes(fill = Gymnasium)) + 
  geom_sf_text(aes(label = number), check_overlap = TRUE, color = "white") + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) + 
  labs(x = "", y = "") +
  common_scale

