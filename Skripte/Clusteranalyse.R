


#----------------------------#
#### 0. Grundeinstellung ####
#---------------------------#

R.Version()


# Environment leeren
rm(list =ls())


# Arbeitsverzeichnis festlegen
setwd("C:/Users/jonas/sciebo/Uni/9. Semester/Methoden der Datenrepräsentation und Klassifikation/Hausarbeit/Datensatz_Zefir")


# Notwendige Palete installieren
if(!require(rstudioapi)) install.packages("rstudioapi")

if(!require("dplyr")) install.packages("dplyr") # for %>%
library(dplyr)

if(!require("tidyverse")) install.packages("tidyverse") # for remove_rownames()
library(tidyverse)

if(!require("data.table")) install.packages("data.table") # for %>%
library(data.table)   

if(!require("openxlsx")) install.packages("openxlsx") # for read.xlsx()
library(openxlsx)  


if(!require("csv")) install.packages("csv")
library(csv)

if(!require("expss")) install.packages("expss")
library(expss)

if(!require("cluster")) install.packages("cluster")
library(cluster)

if(!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

if(!require("condformat")) install.packages("condformat")
library(condformat)

if(!require("sjlabelled")) install.packages("sjlabelled")
library(sjlabelled)

if(!require("ggrepel")) install.packages("ggrepel")
library(ggrepel)

if(!require("clValid")) install.packages("clValid")
library(clValid)

#install.packages("janitor")
#library(janitor)



#------------------------------#
#### 1. Datensätze einlesen ####
#------------------------------#

# Scientific Use File einlesen
for (year in 2006:2017) {
  assign(paste0("wk_suf_",year),
         data.table(read.csv2(paste0("wk_suf_",year,".csv"))),
         envir = .GlobalEnv)
 
  assign(paste0("wk_suf_",year),
         Filter(function(x)!all(is.na(x)),
                eval(parse(text = paste0("wk_suf_",year)))
                ),
         envir = .GlobalEnv)
}



#----------------------------------#
#### 2. Datensätze vorbereiten ####
#---------------------------------#

# Nur kreisfreie Städte und Landkreise übernehmen
for (year in 2006:2017) {
  assign(paste0("data",year),
         eval(parse(text = paste0("wk_suf_",year))) %>%
           filter(typ_bezeichnung %in% c("kreisfreie Stadt", "Landkreis")),
         envir = .GlobalEnv)
}


# Teildatensätze erstellen mit den benötigten Variablen
for (year in 2006:2017) {
  assign(paste0("data",year),
           subset(
             eval(parse(text = paste0("data",year))),
             select=c(Name, ANTAUSL, I1_4f_wansaldo_ins, I5_36_aloq, SDG_BIP_Ew, #Ant_Pflbed, ANTSGB2
                      SDG_vorz_Sterblichkeit, I5_53_Kinderarm, I5_54_Jugarm, I5_55_Altarm, ANTABG_ABI) 
           ),
         envir = .GlobalEnv)

# Struktur der Datensätze überprüfen
  str(eval(parse(text = paste0("data",year))))
  
# Label vergeben
  assign(paste0("data",year),
         apply_labels(
           eval(parse(text = paste0("data",year))),
             Ant_Pflbed = "Anteil der Pflegebedürftigen an der Gesamtbevölkerung", 
             Ant_Pflbed_amb = "Anteil der ambulant Pflegebedürftigen",
             Ant_Pflbed_teilstat = "Anteil der teilstationär Pflegebedürftigen", 
             Ant_Pflbed_vollstat = "Anteil der vollstationär Pflegebedürftigen",
             Ant_Pflbed_vollstat_dauer = "Anteil der vollstationär dauerhaft Pflegebedürftigen",
             Ant_Pflbed_vollstat_kurz = "Anteil der vollstationär kurzzeitig Pflegebedürftigen",
             Ant_Pflgeldempf = "Anteil der Pflegegeldempfänger",
             ANTAUSL = "Anteil ausländischer Einwohner a.d. Bevölkerung",
             I1_4f_wansaldo_ins = "Wanderungssaldorate",
             I5_36_aloq = "Arbeitslosenanteil an allen Erwerbspersonen",
             SDG_BIP_Ew = "Bruttoinlandsprodukt je Einwohner",
             I5_53_Kinderarm = "Kinderarmut",
             I5_54_Jugarm = "Jugendarmut",
             I5_55_Altarm = "Altersarmut",
             SDG_vorz_Sterblichkeit = "Vorzeitige Sterblichkeit",
             ANTABG_ABI = "Anteil Schulabgänger allgbild. Schulen mit allgemeiner Hochschulreife"
             #I4_30_svp_hqwo = "Anteil Hochqualifizierte am Wohnort",
             #ANTSGB2 = "Anteil Empfänger von SGB II-Leistungen a.d. Bevölkerung unter 65 Jahre"
         ), 
         envir = .GlobalEnv)
} 



# leere Zeilen löschen
for (year in 2006:2017) {
  
  assign(paste0("data",year), 
         na.omit(
           eval(parse(text = paste0("data",year)))
         ),
         envir = .GlobalEnv)
}


installed.packages("dplyr")
library(dplyr)



# Character-Variablen entfernen
for (year in 2006:2017) {
  assign(paste0("dat",year),
         subset(
           eval(parse(text = paste0("data",year))),
           select = c(ANTAUSL, I1_4f_wansaldo_ins, I5_36_aloq, SDG_BIP_Ew, #Ant_Pflbed, ANTSGB2
                      SDG_vorz_Sterblichkeit, I5_53_Kinderarm, I5_54_Jugarm, I5_55_Altarm, ANTABG_ABI)
         ),
         envir = .GlobalEnv)
}



#----------------------------#
#### 3. Datenaufbereitung ####
#---------------------------#

# Z-Transformation durchführen
for (year in 2006:2017) {
  assign(paste0("z_data",year),
           as.data.frame(sapply(
             eval(parse(text = paste0("dat",year))),
                  scale)
           ), 
         envir = .GlobalEnv)
}


# Principent Component Analysis
for (year in 2006:2017) {
  assign(paste0("pca.",year),
         princomp(
           eval(parse(text = paste0("z_data",year)))
         ),
         envir = .GlobalEnv)
}

for (year in 2006:2017) {
  loadings(
    eval(parse(text = paste0("pca.",year)))
  )
}


loadings(pca.2006)


# Eigenwerte bestimmen
for (year in 2006:2017) {
  assign(paste0("eigen.",year),
         eval(parse(text = paste0("pca.",year))) $ sdev^2,
         envir = .GlobalEnv)
}

for (year in 2006:2017) {
  plot(
    eval(parse(text = paste0("pca.",year))), main = ""
  )
  title(main = "Principal Component Analysis",year)
  abline(h=1, lty="dashed")
}

for (year in 2006:2017) {
  head(
    eval(parse(text = paste0("pca.",year))) $ scores, 2
  )
}

# Nur Hauptkomponenten mit Eigenwerten größer 1 behalten
for (year in 2006:2017) {
  assign(paste0("dat.pca.",year),
         eval(parse(text = paste0("pca.",year))) $ scores[, 
                                                          eval(parse(text = paste0("eigen.",year)))>=1],
         envir = .GlobalEnv)
}



#-----------------------#
#### 4. Distanzmaße ####
#-----------------------#

# Euklidische Distanz berechnen
for (year in 2006:2017) {
  assign(paste0("eukl.",year),
         dist(x=
                eval(parse(text = paste0("dat.pca.",year))),
              method = "euclidean"),
         envir = .GlobalEnv)
}


#--------------------------#
#### 5. Clusteranalyse ####
#--------------------------#

# Hierarchische Cluster bilden nach Ward-Verfahren
for (year in 2006:2017) {
  assign(paste0("ward.",year),
         hclust(
           eval(parse(text = paste0("eukl.",year))),
           method = "ward.D2"
         ),
         envir = .GlobalEnv)
}

# Scree-Plot
plot(length(ward.2006$height):1, ward.2006$height,
     "b",
     xlab = "Clusteranzahl",
     ylab = expression(Delta * "S"^2),
     cex=0.5,
     pch=3,
     xlim = c(0,10)
)



# Clusteranzahl auf 5 festlegen
cluster5 <- cutree(ward.2006, k=5)
dat2006$clusterW5 <- cluster5


# Cluster beschreiben

desc.ward <- aggregate(dat2006, by=list(cluster5), FUN = mean)
desc.ward1 <- aggregate(dat2007, by=list(dat2007$ANTAUSL = dat2006$ANTAUSL), FUN = mean)


#----------------------------------------#
#### 6. Multidimensionale Skalierung ####
#---------------------------------------#


# Metrische MDS
for (year in 2006:2017) {
  assign(paste0("mds.",year),
         cmdscale(
           eval(parse(text = paste0("eukl.",year)))
         ),
         envir = .GlobalEnv)
}


for (year in 2006:2017) {
  plot(
    eval(parse(text = paste0("mds.",year))),
  main = "", pch=16, cex=1.2, xlab=NA, ylab=NA, xlim = c(-5,5), ylim = c(-5,5))
  title(main = "MDS",year)
  text(eval(parse(text = paste0("mds.",year))), 
       labels = data2006$Name, adj = c(-.15,-.15), cex = 0.6)
}


# farblich
for (year in 2006:2017) {
  plot(
    eval(parse(text = paste0("mds.",year))),
    main = "", pch=16, cex=1.2, xlab = NA, ylab = NA, xlim = c(-5,5), ylim = c(-5,5), col = as.factor(dat2006$clusterW5))
    title(main = "MDS",year)
  text(
   eval(parse(text = paste0("mds.",year))),
    y=NULL, labels = data2006$Name, adj = c(-.15,-.15), cex = 0.5
  )    
}


#---------------------#
#### 7. Validität ####
#---------------------#

partitions <- lapply(2:10, function(x) cutree(ward.2006, k=x))
names(partitions) <- paste0("k", 2:10)

# Konnektivität
connectivity(dist=dist(z_data2006), clusters = partitions$k5)
cons <- sapply(partitions, function(x) connectivity(dist = dist(z_data2006), 
                                                                clusters = x))
plot(2:10, cons, type = "b", xlim = c(1,10), xlab = "Clusteranzahl",
     ylab = "Connectivity")

# Dunn-Index
dunn(dist(z_data2006), partitions$k5)
ds <- sapply(partitions, function(x) dunn(dist = dist(z_data2006),
             clusters = x))
plot(2:10, ds, 
     type = "b",
     xlim = c(1, 10),
     ylim = c(0, 1),
     xlab = "Clusterzahl",
     ylab = "Dunn-Index")