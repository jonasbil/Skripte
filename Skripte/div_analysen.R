#----------------#
# 0. Settings ####
#----------------#

# R.Version 4.3.1
R.Version()

# clean environment
rm(list = ls())


packages <- c("tidyverse", "dplyr", "stringr", "tidyr", "openxlsx", "haven", "readxl", "lubridate", "psych", "geepack", "rmcorr", "pROC", "pscl", 
              "plotly", "rgl", "reshape2")

lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})

rm(packages)


path.data <- getwd()

data <- read_sav(paste0(path.data,"Daten_insgesamt_2024-06-10.sav"))

# Separate Datensätze für Werktag und Wochenende
werktag <- data %>% filter(weekend == 0)
weekend <- data %>% filter(weekend == 1)



#----------------------------------------#
# 1. deskriptive Statistik ####
#---------------------------------------#

#----------------------------#
#* 1.1 Datensätze lasden ####
#---------------------------#


descriptive <- data %>%
  select(Anz_Krad, NAT50:NAT80) %>%
  rename("Anzahl Motorräder pro Stunde" = Anz_Krad)


descriptive_stats <- psych::describe(descriptive)

descriptive_stats <- descriptive_stats %>%
  rownames_to_column(var = "Variablen") %>%
  select(-vars)

#write.xlsx(descriptive_stats, paste0(path.data,"deskriptive Statistiken.xlsx"), rowName = TRUE)




path.hist <- paste0(path.data,"Graphiken/Histogramm/")

# Erstelle das Verzeichnis, falls es nicht existiert
if (!dir.exists(path.hist)) {
  dir.create(path.hist, recursive = TRUE)
}

save_histogram <- function(data, var_name, output_dir, descriptive_stats) {
  data <- data[!is.na(data[[var_name]]), ]  # Remove NA values
  
  # Get descriptive statistics for the variable
  stats <- descriptive_stats[descriptive_stats$Variablen == var_name, ]
  n <- stats$n
  mean <- stats$mean
  sd <- stats$sd
  stats_label <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n, mean, sd)
  
  # Set the axis label according to the requirements
  var_label <- switch(var_name,
                      "Lmax" = expression(L[max] * ",1h"),
                      "Laeq" = expression(L[aeq] * ",1h"),
                      var_name)
  
  # Create the histogram
  p <- ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", boundary = 0) +
    theme_minimal() +
    labs(x = var_label, y = "Häufigkeit") +  # Adding a title for clarity
    scale_x_continuous(breaks = seq(0, 140, 20)) +
    coord_cartesian(xlim = c(0, 140), ylim = c(0, 2000)) +
    theme(axis.text = element_text(size = 24),
          axis.title.x = element_text(size = 26),
          axis.title.y = element_text(size = 26),
          plot.title = element_text(size = 26, face = "bold")) #+
    #annotate("text", x = Inf, y = Inf, label = stats_label, hjust = 1.1, vjust = 1.1, size = 8, color = "black")  # Adjust size as needed
  
  # Save the file
  ggsave(filename = file.path(output_dir, paste0(var_name, ".jpeg")), plot = p, device = "jpeg", width=15, height=15, dpi=200)
}

# Apply the function to each variable in the dataset
lapply(names(descriptive), function(var) save_histogram(descriptive, var, path.hist, descriptive_stats))

#----------------#
#* 1.2 Laeq ####
#----------------#


ggplot(befragung, aes(x = L_Krad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(x = expression(italic(L)["Aeq,1h"] * " "* "in dB"), y = "Häufigkeit") +
  scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(xlim = c(0, 140), ylim = c(0, 200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist,"Laeq.jpeg"), device = "jpeg", width=15, height=15, dpi=200)

ggplot(werktag, aes(x = L_Krad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Werktags (Mo-Fr)",x = expression(italic(L)["Aeq,1h"] * " "* "in dB"), y = "Häufigkeit") +
  scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(xlim = c(0, 140), ylim = c(0, 200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist,"Laeq_werk.jpeg"), device = "jpeg", width=15, height=15, dpi=200)


ggplot(weekend, aes(x = L_Krad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Wochenende (Sa-So)", x = expression(italic(L)["Aeq,1h"] * " "* "in dB"), y = "Häufigkeit") +
  scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(xlim = c(0, 140), ylim = c(0, 200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist,"Laeq_wochenende.jpeg"), device = "jpeg", width=15, height=15, dpi=200)


#---------------#
#* 1.3 Lmax ####
#---------------#

ggplot(befragung, aes(x = Lmax)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(x = expression(italic(L)["AFmax,1h"] * " "* "in dB"), y = "Häufigkeit") +
  scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(xlim = c(0, 140), ylim = c(0, 200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist,"Lmax.jpeg"), device = "jpeg", width=15, height=15, dpi=200)

ggplot(werktag, aes(x = Lmax)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Werktags (Mo-Fr)",x = expression(italic(L)["AFmax,1h"] * " "* "in dB"), y = "Häufigkeit") +
  scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(xlim = c(0, 140), ylim = c(0, 200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist,"Lmax_werk.jpeg"), device = "jpeg", width=15, height=15, dpi=200)


ggplot(weekend, aes(x = Lmax)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Wochenende (Sa-So)", x = expression(italic(L)["AFmax,1h"] * " "* "in dB"), y = "Häufigkeit") +
  scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(xlim = c(0, 140), ylim = c(0, 200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist,"Lmax _wochenende.jpeg"), device = "jpeg", width=15, height=15, dpi=200)




#-----------------------#
#* 1.4 Belästigung ####
#-----------------------#


# Berechnung der deskriptiven Statistiken
# motorrad_stats <- summary(befragung$Motorrad)
# n <- sum(!is.na(befragung$Motorrad))
# mean <- mean(befragung$Motorrad, na.rm = TRUE)
# sd <- sd(befragung$Motorrad, na.rm = TRUE)
# stats_label <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n, mean, sd)


# Erstelle das Histogramm
# ggplot(befragung, aes(x = Motorrad)) +
#   geom_histogram(binwidth = 1, fill = "blue", color = "black") +
#   theme_minimal() +
#   labs(x = "Lärmbelästigung Motorrad", y = "Häufigkeit") +
#   theme(axis.title = element_text(size = 22),
#         axis.text = element_text(size = 20),
#         plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
#   annotate("text", x = Inf, y = Inf, label = stats_label, hjust = 1.1, vjust = 1.1, size = 6, color = "black")

ggplot(befragung, aes(x = Motorrad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(x = "Lärmbelästigung pro Stunde", y = "Häufigkeit") +
  #scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(ylim = c(0, 2200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist, "Bel_Motorrad.jpeg"), device = "jpeg", width=15, height=15, dpi=200)


ggplot(werktag, aes(x = Motorrad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Werktags (Mo-Fr)", x = "Lärmbelästigung pro Stunde", y = "Häufigkeit") +
  #scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(ylim = c(0, 2200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist, "Bel_Motorrad_werk.jpeg"), device = "jpeg", width=15, height=15, dpi=200)



ggplot(weekend, aes(x = Motorrad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Wochenende (Sa-So)", x = "Lärmbelästigung pro Stunde", y = "Häufigkeit") +
  #scale_x_continuous(breaks = seq(10, 110, 20)) +
  coord_cartesian(ylim = c(0, 2200)) +
  theme(axis.title = element_text(size = 26),
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) 

ggsave(paste0(path.hist, "Bel_Motorrad_wochenende.jpeg"), device = "jpeg", width=15, height=15, dpi=200)









# 
# 
# # Berechnung der deskriptiven Statistiken
# emergenz_stats <- summary(befragung$Emergenz)
# n <- sum(!is.na(befragung$Emergenz))
# mean <- mean(befragung$Emergenz, na.rm = TRUE)
# sd <- sd(befragung$Emergenz, na.rm = TRUE)
# stats_label <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n, mean, sd)
# 
# 
# library(ggplot2)
# 
# # Erstelle das Histogramm
# p <- ggplot(befragung, aes(x = Emergenz)) +
#   geom_histogram(binwidth = 1, fill = "blue", color = "black") +
#   theme_minimal() +
#   labs(title = "Emergenz", x = "Emergenz", y = "Häufigkeit") +
#   theme(axis.title = element_text(size = 22),
#         axis.text = element_text(size = 20),
#         plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
#   annotate("text", x = Inf, y = Inf, label = stats_label, hjust = 1.1, vjust = 1.1, size = 6, color = "black")
# 
# # Speichern des Histogramms
# ggsave(paste0(path.hist, "Emergenz.jpeg"), plot = p, device = "jpeg", width=15, height=15, dpi=200)
# 



#---------------------#
#** 1.4.1 Werktag ####
#---------------------#

# Separate Datensätze für Werktag und Wochenende
werktag <- befragung %>% filter(weekend == 0)
weekend <- befragung %>% filter(weekend == 1)


descriptive <- werktag %>%
  select(L_Krad, Anz_Krad, Lmax:NAT80) %>%
  rename("Laeq" = L_Krad,
         "Anzahl Motorräder" = Anz_Krad)


descriptive_stats <- psych::describe(descriptive)

descriptive_stats <- descriptive_stats %>%
  rownames_to_column(var = "Variablen") %>%
  select(-vars)

#write.xlsx(descriptive_stats, paste0(path.data,"deskriptive Statistiken.xlsx"), rowName = TRUE)



# Histogramme

path.hist <- paste0(path.data,"Graphiken/Histogramm/")

# Erstelle das Verzeichnis, falls es nicht existiert
if (!dir.exists(path.hist)) {
  dir.create(path.hist, recursive = TRUE)
}

# Funktion zum Erstellen und Speichern eines Histogramms
save_histogram <- function(data, var_name, output_dir, descriptive_stats) {
  data <- data[!is.na(data[[var_name]]), ]  # Remove NA values
  
  # Retrieve descriptive statistics for the variable
  stats <- descriptive_stats[descriptive_stats$Variablen == var_name, ]
  n <- stats$n
  mean <- stats$mean
  sd <- stats$sd
  stats_label <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n, mean, sd)
  
  # Set the axis label according to the requirements
  var_label <- switch(var_name,
                      "Lmax" = expression(L[max] * ",1h"),
                      "Laeq" = expression(L[aeq] * ",1h"),
                      var_name)
  
  # Create the histogram
  p <- ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "Werktags (Mo-Fr)", x = var_label, y = "Häufigkeit") +  # Adding a title for clarity
    scale_x_continuous(breaks = seq(0, 140, 20), limits = c(0, 140)) +
    theme(axis.text = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size = 22),
          plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
    annotate("text", x = Inf, y = Inf, label = stats_label, hjust = 1.1, vjust = 1.1, size = 6, color = "black")  # Adjust size as needed
  
  # Save the file
  ggsave(filename = file.path(output_dir, paste0(var_name, "_werk", ".jpeg")), plot = p, device = "jpeg", width=15, height=15, dpi=200)
}

# Aufruf der Funktion
lapply(names(descriptive), function(var) save_histogram(descriptive, var, path.hist, descriptive_stats))


# Berechnung der deskriptiven Statistiken für 'Motorrad'
n_motorrad <- sum(!is.na(werktag$Motorrad))
mean_motorrad <- mean(werktag$Motorrad, na.rm = TRUE)
sd_motorrad <- sd(werktag$Motorrad, na.rm = TRUE)
stats_motorrad <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n_motorrad, mean_motorrad, sd_motorrad)

# Erstellung des Histogramms für 'Motorrad'
p_motorrad <- ggplot(werktag, aes(x = Motorrad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Werktags (Mo-Fr)", x = "Lärmbelästigung Motorrad", y = "Häufigkeit") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
  annotate("text", x = Inf, y = Inf, label = stats_motorrad, hjust = 1.1, vjust = 1.1, size = 6, color = "black")

# Speichern des Histogramms
ggsave(paste0(path.hist, "Bel_Motorrad_werk.jpeg"), plot = p_motorrad, device = "jpeg", width=15, height=15, dpi=200)


# Berechnung der deskriptiven Statistiken für 'Emergenz'
n_emergenz <- sum(!is.na(werktag$Emergenz))
mean_emergenz <- mean(werktag$Emergenz, na.rm = TRUE)
sd_emergenz <- sd(werktag$Emergenz, na.rm = TRUE)
stats_emergenz <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n_emergenz, mean_emergenz, sd_emergenz)

# Erstellung des Histogramms für 'Emergenz'
p_emergenz <- ggplot(werktag, aes(x = Emergenz)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Werktags (Mo-Fr)", x = "Emergenz", y = "Häufigkeit") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
  annotate("text", x = Inf, y = Inf, label = stats_emergenz, hjust = 1.1, vjust = 1.1, size = 6, color = "black")

# Speichern des Histogramms
ggsave(paste0(path.hist, "Emergenz_werk.jpeg"), plot = p_emergenz, device = "jpeg", width=15, height=15, dpi=200)



#------------------------#
#** 1.4.2 Wochenende ####
#-----------------------#


descriptive <- weekend %>%
  select(L_Krad, Anz_Krad, Lmax:NAT80) %>%
  rename("Laeq" = L_Krad,
         "Anzahl Motorräder" = Anz_Krad)


descriptive_stats <- psych::describe(descriptive)

descriptive_stats <- descriptive_stats %>%
  rownames_to_column(var = "Variablen") %>%
  select(-vars)

#write.xlsx(descriptive_stats, paste0(path.data,"deskriptive Statistiken.xlsx"), rowName = TRUE)



# Histogramme

path.hist <- paste0(path.data,"Graphiken/Histogramm/")

# Erstelle das Verzeichnis, falls es nicht existiert
if (!dir.exists(path.hist)) {
  dir.create(path.hist, recursive = TRUE)
}

# Funktion zum Erstellen und Speichern eines Histogramms
save_histogram <- function(data, var_name, output_dir, descriptive_stats) {
  data <- data[!is.na(data[[var_name]]), ]  # Remove NA values
  
  # Retrieve descriptive statistics for the variable
  stats <- descriptive_stats[descriptive_stats$Variablen == var_name, ]
  n <- stats$n
  mean <- stats$mean
  sd <- stats$sd
  stats_label <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n, mean, sd)
  
  # Set the axis label according to the requirements
  var_label <- switch(var_name,
                      "Lmax" = expression(L[max] * ",1h"),
                      "Laeq" = expression(L[aeq] * ",1h"),
                      var_name)
  
  # Create the histogram
  p <- ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "Wochenende (Sa-So)", x = var_label, y = "Häufigkeit") +
    scale_x_continuous(breaks = seq(0, 140, 20), limits = c(0, 140)) +
    theme(axis.text = element_text(size = 20),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size = 22),
          plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
    annotate("text", x = Inf, y = Inf, label = stats_label, hjust = 1.1, vjust = 1.1, size = 6, color = "black")  # Adjust size as needed
  
  # Save the file
  ggsave(filename = file.path(output_dir, paste0(var_name, "_wochenende", ".jpeg")), plot = p, device = "jpeg", width=15, height=15, dpi=200)
}

# Aufruf der Funktion
path.hist <- "/dein/speicherpfad"  # Setze deinen eigenen Speicherpfad
lapply(names(descriptive), function(var) save_histogram(descriptive, var, path.hist, descriptive_stats))



# Berechnung der deskriptiven Statistiken für 'Motorrad'
n_motorrad_weekend <- sum(!is.na(weekend$Motorrad))
mean_motorrad_weekend <- mean(weekend$Motorrad, na.rm = TRUE)
sd_motorrad_weekend <- sd(weekend$Motorrad, na.rm = TRUE)
stats_motorrad_weekend <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n_motorrad_weekend, mean_motorrad_weekend, sd_motorrad_weekend)

# Erstellung des Histogramms für 'Motorrad' am Wochenende
p_motorrad_weekend <- ggplot(weekend, aes(x = Motorrad)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Wochenende (Sa-So)", x = "Lärmbelästigung Motorrad", y = "Häufigkeit") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
  annotate("text", x = Inf, y = Inf, label = stats_motorrad_weekend, hjust = 1.1, vjust = 1.1, size = 6, color = "black")

# Speichern des Histogramms
ggsave(paste0(path.hist, "Bel_Motorrad_wochende.jpeg"), plot = p_motorrad_weekend, device = "jpeg", width=15, height=15, dpi=200)


# Berechnung der deskriptiven Statistiken für 'Emergenz'
n_emergenz_weekend <- sum(!is.na(weekend$Emergenz))
mean_emergenz_weekend <- mean(weekend$Emergenz, na.rm = TRUE)
sd_emergenz_weekend <- sd(weekend$Emergenz, na.rm = TRUE)
stats_emergenz_weekend <- sprintf("N: %d\nMittelwert: %.2f\nStd.-Abw.: %.2f", n_emergenz_weekend, mean_emergenz_weekend, sd_emergenz_weekend)

# Erstellung des Histogramms für 'Emergenz' am Wochenende
p_emergenz_weekend <- ggplot(weekend, aes(x = Emergenz)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Wochenende (Sa-So)", x = "Emergenz", y = "Häufigkeit") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold")) +
  annotate("text", x = Inf, y = Inf, label = stats_emergenz_weekend, hjust = 1.1, vjust = 1.1, size = 6, color = "black")

# Speichern des Histogramms
ggsave(paste0(path.hist, "Emergenz_wochende.jpeg"), plot = p_emergenz_weekend, device = "jpeg", width=15, height=15, dpi=200)




rm(path.hist, save_histogram)
rm(descriptive, descriptive_stats)



#---------------------------#
# 2. GEE-Modelle ####
#--------------------------#



# Pfad zum Speichern der Graphiken
path.pics <-  "Z:/Projekte/221.001_BW_Motorradlaerm/6_AP4_Quantitative Befragungen/6_Auswertung/4_Zusatzanalysen_2024/Graphiken/"
#path.pics <- "C:/Zeus/SynologyDrive/221.001_BW_Motorradlaerm/6_AP4_Quantitative Befragungen/6_Auswertung/4_Zusatzanalysen_2024/Graphiken/"

# Leerer Datensatz für die Gütemaße
results <- data.frame(
  Modell = character(),
  Entropy = numeric(),
  Deviance = numeric(),
  AUC = numeric(),
  Konkordanzkorrelationskoeffizient = numeric(),
  Pseudo_R2 = numeric(),
  stringsAsFactors = FALSE
)


# Funktion, um für jedes Regressionsmodell die Gütemaße berechnen zu können
berechne_guetemaße <- function(modellname, modell, daten) {
  # Nullmodell-Entropie
  null_model_prob <- mean(daten$HA_Motorrad)
  null_entropy <- - (null_model_prob * log(null_model_prob + 1e-15) + (1 - null_model_prob) * log(1 - null_model_prob + 1e-15))
  
  # Modell-Entropie
  predicted_probabilities <- predict(modell, type = "response")
  model_entropy <- mean(- (predicted_probabilities * log(predicted_probabilities + 1e-15) + (1 - predicted_probabilities) * log(1 - predicted_probabilities + 1e-15)))
  
  # Proportionale Reduktion der Entropie
  Hmarg <- 1 - (model_entropy / null_entropy)
  
  # Berechnung der Deviance für das Modell
  model_deviance <- sum((daten$HA_Motorrad - predicted_probabilities)^2)
  
  # Berechnung der Deviance für das Nullmodell
  null_model_prob <- mean(daten$HA_Motorrad)
  null_deviance <- sum((daten$HA_Motorrad - null_model_prob)^2)
  
  # Proportionale Reduktion der Abweichung
  D <- 1 - (model_deviance / null_deviance)
  
  # ROC-Kurve und AUC-Wert
  true_values <- daten$HA_Motorrad
  roc_obj <- roc(true_values, predicted_probabilities)
  auc_value <- auc(roc_obj)
  
  # Konkordanzkorrelationskoeffizient
  mean_observed <- mean(daten$HA_Motorrad)
  mean_predicted <- mean(predicted_probabilities)
  var_observed <- var(daten$HA_Motorrad)
  var_predicted <- var(predicted_probabilities)
  covariance <- cov(daten$HA_Motorrad, predicted_probabilities)
  rc <- (2 * covariance) / (var_observed + var_predicted + (mean_observed - mean_predicted)^2)
  
  # Log-Likelihood für das Nullmodell
  null_model <- geeglm(HA_Motorrad ~ 1, data = daten, family = binomial(link = "logit"), id = daten$userID, corstr = "exchangeable")
  logLik_null <- sum(daten$HA_Motorrad * log(predict(null_model, type = "response") + 1e-15) + 
                       (1 - daten$HA_Motorrad) * log(1 - predict(null_model, type = "response") + 1e-15))
  
  # Log-Likelihood für das angepasste Modell
  logLik_model <- sum(daten$HA_Motorrad * log(predict(modell, type = "response") + 1e-15) + 
                        (1 - daten$HA_Motorrad) * log(1 - predict(modell, type = "response") + 1e-15))
  
  # McFadden Pseudo-R²
  mcfadden_r2 <- 1 - (logLik_model / logLik_null)
  
  # Ergebnisse speichern
  results <<- rbind(results, data.frame(
    Modell = modellname,
    Entropy = Hmarg,
    Deviance = D,
    AUC = auc_value,
    Konkordanzkorrelationskoeffizient = rc,
    Pseudo_R2 = mcfadden_r2,
    stringsAsFactors = FALSE
  ))
}



#-----------------------------#
#* 2.1 Anzahl Motorräder ####
#-----------------------------#

anzahl_werk <- werktag %>%
  filter(!is.na(Anz_Krad))

anzahl_we <- weekend %>%
  filter(!is.na(Anz_Krad))

gee_werktag <- geeglm(HA_Motorrad ~ Anz_Krad, data = anzahl_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("Anz_Krad_werk", gee_werktag, anzahl_werk)


gee_weekend <- geeglm(HA_Motorrad ~ Anz_Krad, data = anzahl_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("Anz_Krad_wochenende", gee_weekend, anzahl_we)



mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(Anz_Krad=seq(1,138,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$Anz_Krad))) * 100
data$se <- sqrt(abs(a + (data$Anz_Krad)^2 * b + 2 * c * data$Anz_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$Anz_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$Anz_Krad + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(Anz_Krad=seq(1,138,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$Anz_Krad))) * 100
data$se <- sqrt(abs(a + (data$Anz_Krad)^2 * b + 2 * c * data$Anz_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$Anz_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$Anz_Krad + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all,aes(x=Anz_Krad,y=pred,fill=Zeitraum,))+
  geom_line(aes(col=Zeitraum),linewidth=1)+
  geom_ribbon(aes(ymin=inf,ymax=sup),alpha=0.15,show.legend = F)+
  # facet_grid(cols = vars(weekend),labeller = as_labeller(c('0'='Werktag (Mo - Fr)','1'='Wochenende')))+
  scale_color_manual(values=c("Werktag" = "coral2", "Wochenende" = "dodgerblue3"),
                     labels=c('Werktag','Wochenende'))+
  scale_fill_manual(values=c("Werktag" = "coral2", "Wochenende" = "dodgerblue3"),
                    labels=c('Werktag','Wochenende'))+
  scale_x_continuous(limits = c(0, 140), breaks=seq(0, 140, by=10), minor_breaks=seq(0, 140, by=5)) +
  scale_y_continuous(limits = c(0,100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('Anzahl Motorräder'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (',HA[v],') in %',sep='')))+
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22))


ggplot(mod_all, aes(x = Anz_Krad, y = pred))


#ggsave(paste0(path.pics,"Anzahl_Motorräder_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)

rm(anzahl_werk, anzahl_we)


#-----------------------------#
#* 2.2 Lmax ####
#-----------------------------#

lmax_werk <- werktag %>%
  filter(!is.na(Lmax))

lmax_we <- weekend %>%
  filter(!is.na(Lmax))

gee_werktag <- geeglm(HA_Motorrad ~ Lmax, data = lmax_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("lmax_werk", gee_werktag, lmax_werk)

gee_weekend <- geeglm(HA_Motorrad ~ Lmax, data = lmax_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("lmax_wochenende", gee_weekend, lmax_we)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(Lmax=seq(50,102,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$Lmax))) * 100
data$se <- sqrt(abs(a + (data$Lmax)^2 * b + 2 * c * data$Lmax))
data$inf <- 1 / (1 + exp(-(int + bet * data$Lmax - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$Lmax + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(Lmax=seq(50,102,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$Lmax))) * 100
data$se <- sqrt(abs(a + (data$Lmax)^2 * b + 2 * c * data$Lmax))
data$inf <- 1 / (1 + exp(-(int + bet * data$Lmax - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$Lmax + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all,aes(x=Lmax,y=pred,fill=Zeitraum,))+
  geom_line(aes(col=Zeitraum),linewidth=1)+
  geom_ribbon(aes(ymin=inf,ymax=sup),alpha=0.15,show.legend = F)+
  # facet_grid(cols = vars(weekend),labeller = as_labeller(c('0'='Werktag (Mo - Fr)','1'='Wochenende')))+
  scale_color_manual(values=c("Werktag" = "chocolate2", "Wochenende" = "deepskyblue3"),
                     labels=c('Werktag','Wochenende'))+
  scale_fill_manual(values=c("Werktag" = "chocolate2", "Wochenende" = "deepskyblue3"),
                    labels=c('Werktag','Wochenende'))+
  scale_x_continuous(limits = c(40, 110), breaks=seq(40, 110, by=10), minor_breaks=seq(40, 110, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('Lmax'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (',HA[v],') in %',sep='')))+
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22))


#ggsave(paste0(path.pics,"Lmax_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)

rm(lmax_we, lmax_werk)

#-----------------------------#
#* 2.3 NAT50 ####
#-----------------------------#

NAT50_werk <- werktag %>%
  filter(!is.na(NAT50))

NAT50_we <- weekend %>%
  filter(!is.na(NAT50))

gee_werktag <- geeglm(HA_Motorrad ~ NAT50, data = NAT50_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("NAT50_werk", gee_werktag, NAT50_werk)


gee_weekend <- geeglm(HA_Motorrad ~ NAT50, data = NAT50_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("NAT50_wochenende", gee_weekend, NAT50_we)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(NAT50=seq(1,125,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT50))) * 100
data$se <- sqrt(abs(a + (data$NAT50)^2 * b + 2 * c * data$NAT50))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT50 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT50 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(NAT50=seq(1,125,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT50))) * 100
data$se <- sqrt(abs(a + (data$NAT50)^2 * b + 2 * c * data$NAT50))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT50 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT50 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all,aes(x=NAT50,y=pred,fill=Zeitraum,))+
  geom_line(aes(col=Zeitraum),linewidth=1)+
  geom_ribbon(aes(ymin=inf,ymax=sup),alpha=0.15,show.legend = F)+
  # facet_grid(cols = vars(weekend),labeller = as_labeller(c('0'='Werktag (Mo - Fr)','1'='Wochenende')))+
  scale_color_manual(values=c("Werktag" = "brown3", "Wochenende" = "steelblue2"),
                     labels=c('Werktag','Wochenende'))+
  scale_fill_manual(values=c("Werktag" = "brown3", "Wochenende" = "steelblue2"),
                    labels=c('Werktag','Wochenende'))+
  scale_x_continuous(limits = c(0, 140), breaks=seq(0, 140, by=10), minor_breaks=seq(0, 140, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('NAT50'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (',HA[v],') in %',sep='')))+
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22))



#ggsave(paste0(path.pics,"NAT50_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)

rm(NAT50_we, NAT50_werk)


#-----------------------------#
#* 2.4 NAT60 ####
#-----------------------------#

NAT60_werk <- werktag %>%
  filter(!is.na(NAT60))

NAT60_we <- weekend %>%
  filter(!is.na(NAT60))

gee_werktag <- geeglm(HA_Motorrad ~ NAT60, data = NAT60_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("NAT60_werk", gee_werktag, NAT60_werk)

gee_weekend <- geeglm(HA_Motorrad ~ NAT60, data = NAT60_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("NAT60_wochenende", gee_weekend, NAT60_we)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(NAT60=seq(1,69,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT60))) * 100
data$se <- sqrt(abs(a + (data$NAT60)^2 * b + 2 * c * data$NAT60))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT60 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT60 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(NAT60=seq(1,69,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT60))) * 100
data$se <- sqrt(abs(a + (data$NAT60)^2 * b + 2 * c * data$NAT60))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT60 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT60 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=NAT60, y=pred, fill=Zeitraum)) +
  geom_line(aes(col=Zeitraum), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Werktag" = "hotpink3", "Wochenende" = "turquoise3"),
                     labels=c('Werktag', 'Wochenende')) +
  scale_fill_manual(values=c("Werktag" = "hotpink3", "Wochenende" = "turquoise3"),
                    labels=c('Werktag', 'Wochenende')) +
  scale_x_continuous(limits = c(0, 140), breaks=seq(0, 140, by=10), minor_breaks=seq(0, 140, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('NAT60'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"NAT60_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)

rm(NAT60_we, NAT60_werk)

#-----------------------------#
#* 2.5 NAT70 ####
#-----------------------------#

NAT70_werk <- werktag %>%
  filter(!is.na(NAT70))

NAT70_we <- weekend %>%
  filter(!is.na(NAT70))

gee_werktag <- geeglm(HA_Motorrad ~ NAT70, data = NAT70_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("NAT70_werk", gee_werktag, NAT70_werk)

gee_weekend <- geeglm(HA_Motorrad ~ NAT70, data = NAT70_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("NAT70_wochenende", gee_weekend, NAT70_we)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(NAT70=seq(1,69,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT70))) * 100
data$se <- sqrt(abs(a + (data$NAT70)^2 * b + 2 * c * data$NAT70))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT70 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT70 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(NAT70=seq(1,69,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT70))) * 100
data$se <- sqrt(abs(a + (data$NAT70)^2 * b + 2 * c * data$NAT70))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT70 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT70 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=NAT70, y=pred, fill=Zeitraum)) +
  geom_line(aes(col=Zeitraum), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Werktag" = "hotpink3", "Wochenende" = "turquoise3"),
                     labels=c('Werktag', 'Wochenende')) +
  scale_fill_manual(values=c("Werktag" = "hotpink3", "Wochenende" = "turquoise3"),
                    labels=c('Werktag', 'Wochenende')) +
  scale_x_continuous(limits = c(0, 140), breaks=seq(0, 140, by=10), minor_breaks=seq(0, 140, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('NAT70'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"NAT70_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)

rm(NAT70_we, NAT70_werk)

#--------------------------------------------#
#** 2.6.1 NAT80 - standartisierte x-Achse####
#-------------------------------------------#


NAT80_werk <- werktag %>%
  filter(!is.na(NAT80))

NAT80_we <- weekend %>%
  filter(!is.na(NAT80))

gee_werktag <- geeglm(HA_Motorrad ~ NAT80, data = NAT80_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("NAT80_werk", gee_werktag, NAT80_werk)

gee_weekend <- geeglm(HA_Motorrad ~ NAT80, data = NAT80_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("NAT80_wochenende", gee_weekend, NAT80_we)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(NAT80=seq(1,37,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT80))) * 100
data$se <- sqrt(abs(a + (data$NAT80)^2 * b + 2 * c * data$NAT80))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT80 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT80 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(NAT80=seq(1,37,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT80))) * 100
data$se <- sqrt(abs(a + (data$NAT80)^2 * b + 2 * c * data$NAT80))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT80 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT80 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=NAT80, y=pred, fill=Zeitraum)) +
  geom_line(aes(col=Zeitraum), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Werktag" = "gold2", "Wochenende" = "purple3"),
                     labels=c('Werktag', 'Wochenende')) +
  scale_fill_manual(values=c("Werktag" = "gold2", "Wochenende" = "purple3"),
                    labels=c('Werktag', 'Wochenende')) +
  scale_x_continuous(limits = c(0, 140), breaks=seq(0, 140, by=10), minor_breaks=seq(0, 140, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('NAT80'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"NAT80_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)


rm(NAT80_we, NAT80_werk)

#-------------------------------------#
#** 2.6.2 NAT80 - kürzere x-Achse ####
#------------------------------------#


NAT80_werk <- werktag %>%
  filter(!is.na(NAT80))

NAT80_we <- weekend %>%
  filter(!is.na(NAT80))

gee_werktag <- geeglm(HA_Motorrad ~ NAT80, data = NAT80_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)

gee_weekend <- geeglm(HA_Motorrad ~ NAT80, data = NAT80_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(NAT80=seq(1,37,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT80))) * 100
data$se <- sqrt(abs(a + (data$NAT80)^2 * b + 2 * c * data$NAT80))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT80 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT80 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(NAT80=seq(1,37,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$NAT80))) * 100
data$se <- sqrt(abs(a + (data$NAT80)^2 * b + 2 * c * data$NAT80))
data$inf <- 1 / (1 + exp(-(int + bet * data$NAT80 - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$NAT80 + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=NAT80, y=pred, fill=Zeitraum)) +
  geom_line(aes(col=Zeitraum), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Werktag" = "gold2", "Wochenende" = "purple3"),
                     labels=c('Werktag', 'Wochenende')) +
  scale_fill_manual(values=c("Werktag" = "gold2", "Wochenende" = "purple3"),
                    labels=c('Werktag', 'Wochenende')) +
  scale_x_continuous(limits = c(0, 40), breaks=seq(0, 40, by=10), minor_breaks=seq(0, 40, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('NAT80'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"NAT80_kurze_xAchse.jpeg"),device='jpeg',width=15,height=15,dpi=200)

rm(NAT80_we, NAT80_werk)


#-----------------------------#
#* 2.7 Emergenz ####
#-----------------------------#

#------------------------------------------#
#** 2.7.1 Emergenz als unabh. Variable ####
#------------------------------------------#


emergenz_werk <- werktag %>%
  filter(!is.na(Emergenz))

emergenz_we <- weekend %>%
  filter(!is.na(Emergenz))

gee_werktag <- geeglm(HA_Motorrad ~ Emergenz, data = emergenz_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)
berechne_guetemaße("Emergenz_werk", gee_werktag, emergenz_werk)

gee_weekend <- geeglm(HA_Motorrad ~ Emergenz, data = emergenz_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)
berechne_guetemaße("Emergenz_wochenende", gee_weekend, emergenz_we)


mod <- data.frame(tidy(gee_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_werktag$geese$vbeta[1,1]
b <- gee_werktag$geese$vbeta[2,2]
c <- gee_werktag$geese$vbeta[1,2]
data <- data.frame(Emergenz=seq(0,6,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$Emergenz))) * 100
data$se <- sqrt(abs(a + (data$Emergenz)^2 * b + 2 * c * data$Emergenz))
data$inf <- 1 / (1 + exp(-(int + bet * data$Emergenz - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$Emergenz + 1.96 * data$se))) * 100
data$Zeitraum <- 'Werktag'
data$weekend <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_weekend,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_weekend$geese$vbeta[1,1]
b <- gee_weekend$geese$vbeta[2,2]
c <- gee_weekend$geese$vbeta[1,2]
data <- data.frame(Emergenz=seq(0,6,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$Emergenz))) * 100
data$se <- sqrt(abs(a + (data$Emergenz)^2 * b + 2 * c * data$Emergenz))
data$inf <- 1 / (1 + exp(-(int + bet * data$Emergenz - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$Emergenz + 1.96 * data$se))) * 100
data$Zeitraum <- 'Wochenende'
data$weekend <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=Emergenz, y=pred, fill=Zeitraum)) +
  geom_line(aes(col=Zeitraum), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Werktag" = "gold2", "Wochenende" = "purple3"),
                     labels=c('Werktag', 'Wochenende')) +
  scale_fill_manual(values=c("Werktag" = "gold2", "Wochenende" = "purple3"),
                    labels=c('Werktag', 'Wochenende')) +
  scale_x_continuous(limits = c(0, 10), breaks=seq(0, 10, by=1), minor_breaks=seq(0, 10, by=1)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(col='Zeitraum: ',
       x=expression('Emergenz'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"Emergenz_neu.jpeg"),device='jpeg',width=15,height=15,dpi=200)



#-------------------------------------------------------#
#** 2.7.2 Leq mit Emergenz dichotomosiert - werktag ####
#-------------------------------------------------------#


leq_emergenz_unter3_werk <- werktag %>%
  filter(emergenz_ueber_3 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber3_werk <- werktag %>%
  filter(emergenz_ueber_3 == 1,
         !is.na(L_Krad))

gee_unter3_werktag <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_unter3_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_unter3_werktag)
berechne_guetemaße("Leq mit Emergenz unter 3 - werktag", gee_unter3_werktag, leq_emergenz_unter3_werk)

gee_ueber3_werktag <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_ueber3_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_ueber3_werktag)
berechne_guetemaße("Leq mit Emergenz über 3 - werktag", gee_ueber3_werktag, leq_emergenz_ueber3_werk)


mod <- data.frame(tidy(gee_unter3_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_unter3_werktag$geese$vbeta[1,1]
b <- gee_unter3_werktag$geese$vbeta[2,2]
c <- gee_unter3_werktag$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,65,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Unter 3 dB'
data$emergenz <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_ueber3_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_ueber3_werktag$geese$vbeta[1,1]
b <- gee_ueber3_werktag$geese$vbeta[2,2]
c <- gee_ueber3_werktag$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,65,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Über 3 dB'
data$emergenz <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=L_Krad, y=pred, fill=Emergenz)) +
  geom_line(aes(col=Emergenz), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Über 3 dB" = "palegreen3", "Unter 3 dB" = "lightcoral"),
                     labels=c('Über 3 dB', 'Unter 3 dB')) +
  scale_fill_manual(values=c("Über 3 dB" = "palegreen3", "Unter 3 dB" = "lightcoral"),
                    labels=c('Über 3 dB', 'Unter 3 dB')) +
  scale_x_continuous(limits = c(0, 75), breaks=seq(0, 75, by=10), minor_breaks=seq(0, 75, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(title = "Werktags" 
       ,col='Emergenz: ',
       x=expression(L['Aeq,1h']*' in dB'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size = 22)
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"Emergenz_diff_ueber3_werk.jpeg"),device='jpeg',width=15,height=15,dpi=200)


rm(leq_emergenz_ueber3_werk, leq_emergenz_unter3_werk)

#-------------------------------------------------------#
#** 2.7.3 Leq mit Emergenz dichotomosiert - wochenende ####
#-------------------------------------------------------#


leq_emergenz_unter3_we <- weekend %>%
  filter(emergenz_ueber_3 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber3_we <- weekend %>%
  filter(emergenz_ueber_3 == 1,
         !is.na(L_Krad))

gee_unter3_we <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_unter3_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_unter3_we)
berechne_guetemaße("Leq mit Emergenz unter 3 - wochenende", gee_unter3_we, leq_emergenz_unter3_we)

gee_ueber3_we <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_ueber3_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_ueber3_we)
berechne_guetemaße("Leq mit Emergenz über 3 - wochenende", gee_ueber3_we, leq_emergenz_ueber3_we)


mod <- data.frame(tidy(gee_unter3_we,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_unter3_we$geese$vbeta[1,1]
b <- gee_unter3_we$geese$vbeta[2,2]
c <- gee_unter3_we$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,73,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Unter 3 dB'
data$emergenz <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_ueber3_we,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_ueber3_we$geese$vbeta[1,1]
b <- gee_ueber3_we$geese$vbeta[2,2]
c <- gee_ueber3_we$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,73,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Über 3 dB'
data$emergenz <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=L_Krad, y=pred, fill=Emergenz)) +
  geom_line(aes(col=Emergenz), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Über 3 dB" = "forestgreen", "Unter 3 dB" = "rosybrown"),
                     labels=c('Über 3 dB', 'Unter 3 dB')) +
  scale_fill_manual(values=c("Über 3 dB" = "forestgreen", "Unter 3 dB" = "rosybrown"),
                    labels=c('Über 3 dB', 'Unter 3 dB')) +
  scale_x_continuous(limits = c(0, 75), breaks=seq(0, 75, by=10), minor_breaks=seq(0, 75, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(title = "Wochenende" 
       ,col='Emergenz: ',
       x=expression(L['Aeq,1h']*' in dB'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size = 22)
  )  # Anpassungen für kleinere Gitterlinien



#ggsave(paste0(path.pics,"Emergenz_diff_ueber3_wochenende.jpeg"),device='jpeg',width=15,height=15,dpi=200)



rm(leq_emergenz_ueber3_we, leq_emergenz_unter3_we)

#-------------------------------------------------------#
#** 2.7.4 Leq mit Emergenz dichotomosiert - werktag - Emergenz > 2 ####
#-------------------------------------------------------#


leq_emergenz_unter2_werk <- werktag %>%
  filter(emergenz_ueber_2 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber2_werk <- werktag %>%
  filter(emergenz_ueber_2 == 1,
         !is.na(L_Krad))

gee_unter2_werktag <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_unter2_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_unter2_werktag)
berechne_guetemaße("Leq mit Emergenz unter 2 - werktag", gee_unter2_werktag, leq_emergenz_unter2_werk)

gee_ueber3_werktag <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_ueber2_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_ueber3_werktag)
berechne_guetemaße("Leq mit Emergenz über 2 - werktag", gee_ueber3_werktag, leq_emergenz_ueber2_werk)


mod <- data.frame(tidy(gee_unter2_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_unter2_werktag$geese$vbeta[1,1]
b <- gee_unter2_werktag$geese$vbeta[2,2]
c <- gee_unter2_werktag$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,65,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Unter 2 dB'
data$emergenz <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_ueber3_werktag,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_ueber3_werktag$geese$vbeta[1,1]
b <- gee_ueber3_werktag$geese$vbeta[2,2]
c <- gee_ueber3_werktag$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,65,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Über 2 dB'
data$emergenz <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=L_Krad, y=pred, fill=Emergenz)) +
  geom_line(aes(col=Emergenz), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Unter 2 dB" = "palegreen3", "Über 2 dB" = "lightcoral"),
                     labels=c('Unter 2 dB', 'Über 2 dB')) +
  scale_fill_manual(values=c("Unter 2 dB" = "palegreen3", "Über 2 dB" = "lightcoral"),
                    labels=c('Unter 2 dB', 'Über 2 dB')) +
  scale_x_continuous(limits = c(0, 75), breaks=seq(0, 75, by=10), minor_breaks=seq(0, 75, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(title = "Werktags" 
       ,col='Emergenz: ',
       x=expression(L['Aeq,1h']*' in dB'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size = 22)
  )  # Anpassungen für kleinere Gitterlinien


#ggsave(paste0(path.pics,"Emergenz_diff_ueber2_werk.jpeg"),device='jpeg',width=15,height=15,dpi=200)


rm(leq_emergenz_unter2_werk, leq_emergenz_ueber2_werk)

#-------------------------------------------------------#
#** 2.7.5 Leq mit Emergenz dichotomosiert - wochenende - Emergenz > 2####
#-------------------------------------------------------#


leq_emergenz_unter2_we <- weekend %>%
  filter(emergenz_ueber_2 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber2_we <- weekend %>%
  filter(emergenz_ueber_2 == 1,
         !is.na(L_Krad))

gee_unter2_we <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_unter2_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_unter3_we)
berechne_guetemaße("Leq mit Emergenz unter 2 - wochenende", gee_unter2_we, leq_emergenz_unter2_we)

gee_ueber2_we <- geeglm(HA_Motorrad ~ L_Krad, data = leq_emergenz_ueber2_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_ueber3_we)
berechne_guetemaße("Leq mit Emergenz über 2 - wochenende", gee_ueber2_we, leq_emergenz_ueber2_we)


mod <- data.frame(tidy(gee_unter2_we,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_unter2_we$geese$vbeta[1,1]
b <- gee_unter2_we$geese$vbeta[2,2]
c <- gee_unter2_we$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,73,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Unter 2 dB'
data$emergenz <- 0

mod_Motorrad_WT <- data

rm(data,a,b,c,mod,int,bet)



mod <- data.frame(tidy(gee_ueber2_we,conf.int = T))
int <- mod[1,2]
bet <- mod[2,2]
a <- gee_ueber2_we$geese$vbeta[1,1]
b <- gee_ueber3_we$geese$vbeta[2,2]
c <- gee_ueber2_we$geese$vbeta[1,2]
data <- data.frame(L_Krad=seq(1,73,1))
data$pred <- 1 / (1 + exp(-(int + bet * data$L_Krad))) * 100
data$se <- sqrt(abs(a + (data$L_Krad)^2 * b + 2 * c * data$L_Krad))
data$inf <- 1 / (1 + exp(-(int + bet * data$L_Krad - 1.96 * data$se))) * 100
data$sup <- 1 / (1 + exp(-(int + bet * data$L_Krad + 1.96 * data$se))) * 100
data$Emergenz <- 'Über 2 dB'
data$emergenz <- 1

mod_Motorrad_WE <- data

rm(data,a,b,c,mod,int,bet)

mod_all <- bind_rows(mod_Motorrad_WT, mod_Motorrad_WE)


ggplot(mod_all, aes(x=L_Krad, y=pred, fill=Emergenz)) +
  geom_line(aes(col=Emergenz), linewidth=1) +
  geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.15, show.legend = F) +
  scale_color_manual(values=c("Unter 2 dB" = "forestgreen", "Über 2 dB" = "rosybrown"),
                     labels=c('Unter 2 dB', 'Über 2 dB')) +
  scale_fill_manual(values=c("Unter 2 dB" = "forestgreen", "Über 2 dB" = "rosybrown"),
                    labels=c('Unter 2 dB', 'Über 2 dB')) +
  scale_x_continuous(limits = c(0, 75), breaks=seq(0, 75, by=10), minor_breaks=seq(0, 75, by=5)) +
  scale_y_continuous(limits = c(0, 100), breaks=seq(0, 100, by=10), minor_breaks=seq(0, 100, by=5)) + 
  labs(title = "Wochenende" 
       ,col='Emergenz: ',
       x=expression(L['Aeq,1h']*' in dB'),
       y=expression(paste('Wahrscheinlichkeit hoher Lärmbelästigung (', HA[v], ') in %', sep=''))) +
  theme(legend.position = 'bottom',
        legend.title = element_text(size=22),
        legend.text = element_text(size=22),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22),
        axis.text = element_text(size=22),
        strip.text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size = 22)
  )  # Anpassungen für kleinere Gitterlinien



#ggsave(paste0(path.pics,"Emergenz_diff_ueber2_wochenende.jpeg"),device='jpeg',width=15,height=15,dpi=200)




# Tabelle für Gütemaße
#write.xlsx(results, paste0(path.data,"Gütemaße_GEE-Modelle.xlsx"))


rm(results, gee_ueber2_we, gee_ueber3_we, gee_ueber3_werktag, gee_unter2_we, gee_unter2_werktag, gee_unter3_we,gee_unter3_werktag, gee_weekend, gee_werktag, 
   emergenz_we, emergenz_werk, leq_emergenz_ueber2_we, leq_emergenz_unter2_we, mod_all, mod_Motorrad_WE, mod_Motorrad_WT, berechne_guetemaße, path.pics)


#--------------#
#* 2.8 Leq ####
#--------------#

# wird nur für die Formeln benätigt, daher wird hier auch keine Graphik erstellt


L_Krad_werk <- werktag %>%
  filter(!is.na(L_Krad))

L_Krad_we <- weekend %>%
  filter(!is.na(L_Krad))

gee_werktag <- geeglm(HA_Motorrad ~ L_Krad, data = L_Krad_werk, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_werktag)

gee_weekend <- geeglm(HA_Motorrad ~ L_Krad, data = L_Krad_we, family = binomial(link = "logit"), id = userID, corstr = "exchangeable")
summary(gee_weekend)


#-----------------------------#
# 3. repeated measurement ####
#----------------------------#

#kann erst mal auskommentiert werden

#rm(list = ls())

#path.data <- "Z:/Projekte/221.001_BW_Motorradlaerm/6_AP4_Quantitative Befragungen/6_Auswertung/4_Zusatzanalysen_2024/"
path.data <- "C:/Zeus/SynologyDrive/221.001_BW_Motorradlaerm/6_AP4_Quantitative Befragungen/6_Auswertung/4_Zusatzanalysen_2024/" # home office


data <- read_sav(paste0(path.data,"Daten_insgesamt_2024-06-10.sav"))

# r_corr <- rmcorr(ID, Motorrad, L_Krad, data, CI.level = 0.95)
# print(r_corr)

id <- data$ID
motorrad <- data$Motorrad

variables <- c("L_Krad" ,"Emergenz" ,"Lmax" ,"Anz_Krad" ,"NAT50" ,"NAT60", "NAT70" ,"NAT80")

corr_gesamt <- data.frame(
  Measure2 = character(),
  Korrelation = numeric(),
  DegreesOfFreedom = integer(),
  PValue = numeric(),
  ConfidenceIntervalLower = numeric(),
  ConfidenceIntervalUpper = numeric(),
  stringsAsFactors = FALSE)


for (var in variables) {
  measure2_column <- data[[var]]
  r_corr <- rmcorr(id, motorrad, measure2_column, data, CI.level = 0.95)

  corr_gesamt <- rbind(corr_gesamt, data.frame(
    Measure2 = var,
    Korrelation = r_corr$r,
    DegreesOfFreedom = r_corr$df,
    PValue = r_corr$p,
    ConfidenceIntervalLower = r_corr$CI[1],
    ConfidenceIntervalUpper = r_corr$CI[2]
  ))
}


write.xlsx(corr_gesamt, paste0(path.data,"rcorr_gesamt.xlsx"))



werktag <- data %>% filter(weekend == 0)
weekend <- data %>% filter(weekend == 1)



id <- werktag$ID
motorrad <- werktag$Motorrad

variables <- c("L_Krad" ,"Emergenz" ,"Lmax" ,"Anz_Krad" ,"NAT50" ,"NAT60", "NAT70" ,"NAT80")

corr_werktag <- data.frame(
  Measure2 = character(),
  Korrelation = numeric(),
  DegreesOfFreedom = integer(),
  PValue = numeric(),
  ConfidenceIntervalLower = numeric(),
  ConfidenceIntervalUpper = numeric(),
  stringsAsFactors = FALSE)


for (var in variables) {
  measure2_column <- werktag[[var]]
  r_corr <- rmcorr(id, motorrad, measure2_column, werktag, CI.level = 0.95)

  corr_werktag <- rbind(corr_werktag, data.frame(
    Measure2 = var,
    Korrelation = r_corr$r,
    DegreesOfFreedom = r_corr$df,
    PValue = r_corr$p,
    ConfidenceIntervalLower = r_corr$CI[1],
    ConfidenceIntervalUpper = r_corr$CI[2]
  ))
}

write.xlsx(corr_werktag, paste0(path.data,"rcorr_werktag.xlsx"))




id <- weekend$ID
motorrad <- weekend$Motorrad

variables <- c("L_Krad" ,"Emergenz" ,"Lmax" ,"Anz_Krad" ,"NAT50" ,"NAT60", "NAT70", "NAT80")

corr_weekend <- data.frame(
  Measure2 = character(),
  Korrelation = numeric(),
  DegreesOfFreedom = integer(),
  PValue = numeric(),
  ConfidenceIntervalLower = numeric(),
  ConfidenceIntervalUpper = numeric(),
  stringsAsFactors = FALSE)


for (var in variables) {
  measure2_column <- weekend[[var]]
  r_corr <- rmcorr(id, motorrad, measure2_column, weekend, CI.level = 0.95)

  corr_weekend <- rbind(corr_weekend, data.frame(
    Measure2 = var,
    Korrelation = r_corr$r,
    DegreesOfFreedom = r_corr$df,
    PValue = r_corr$p,
    ConfidenceIntervalLower = r_corr$CI[1],
    ConfidenceIntervalUpper = r_corr$CI[2]
  ))
}

write.xlsx(corr_weekend, paste0(path.data,"rcorr_wochenende.xlsx"))



#--------------------------------------------#
# 4. scatter plots with loess regression ####
#--------------------------------------------#


path.loess <- paste0(path.data,"Graphiken/loess/R/")

# 
# ggplot(weekend, aes(x = Lmax, y = Motorrad)) + 
#   geom_point() + 
#   stat_smooth(method = "loess", se = FALSE, span = 0.4, 
#               method.args = list(degree = 1) )
# 
# 
# 
# 
# ggplot(lmax_we, aes(x = Lmax, y = Motorrad)) +
#   geom_point() +  # Punkte hinzufügen
#   geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # LOESS-Glättungslinie ohne Standardfehler-Band
#   ggtitle("Wochenende") +
#   xlab("LAMax in dB") +
#   ylab("Lärmbelästigung")
# 
# 
# 
# ggplot(lmax_werk, aes(x = Lmax, y = Motorrad)) +
#   geom_point(alpha = 0.6, size = 2, color = "blue")  +
#   geom_smooth(method = "loess", span = 0.5, degree = 2, se = FALSE) +
#   ggtitle("Werktag (Mo-Fr)") +
#   xlab("LAMax in dB") +
#   ylab("Lärmbelästigung") +
#   theme_minimal() +  
#   theme(plot.title = element_text(hjust = 0.5))



#----------------#
#* 4.1 lmax ####
#----------------#

#--------------------------------------------#
#** 4.1.1 lmax werk ####
#--------------------------------------------#

lmax_werk <- werktag %>%
  filter(!is.na(Lmax))

lmax_we <- weekend %>%
  filter(!is.na(Lmax))


# Die Endungen 01 und 03 stehen für den Grad der Glättung

lmax_werk_01 <- loess(Motorrad ~ Lmax, data = lmax_werk, span = 0.1, degree = 2)

lmax_werk$loess_01 <- predict(lmax_werk_01)

ggplot(lmax_werk, aes(x = Lmax, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "LAMax in dB",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
#    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
 #   plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22),

  )


#ggsave(paste0(path.loess,"lmax_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



lmax_werk_03 <- loess(Motorrad ~ Lmax, data = lmax_werk, span = 0.3, degree = 2)

lmax_werk$loess_03 <- predict(lmax_werk_03)

ggplot(lmax_werk, aes(x = Lmax, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "LAMax in dB",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22),
    
  )


#ggsave(paste0(path.loess,"lmax_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)





#--------------------------------------------#
#** 4.1.2 lmax weekend ####
#--------------------------------------------#



lmax_we_01 <- loess(Motorrad ~ Lmax, data = lmax_we, span = 0.1, degree = 2)

lmax_we$loess_01 <- predict(lmax_we_01)

ggplot(lmax_we, aes(x = Lmax, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa_so)",
       x = "LAMax in dB",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22),
  )


#ggsave(paste0(path.loess,"lmax_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



lmax_we_filtered <- subset(lmax_we, Lmax <= 95)

# loess-Regression mit gefilterten Daten durchführen
loess_model_filtered <- loess(Motorrad ~ Lmax, data = lmax_we_filtered, span = 0.1, degree = 2)

# Vorhersagen basierend auf dem Modell mit gefilterten Daten berechnen
lmax_we_filtered$loess_fit <- predict(loess_model_filtered)

# Plot mit ggplot2
ggplot(lmax_we_filtered, aes(x = Lmax, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_fit), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa_so)",
       x = "LAMax in dB",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22),
    
  )


#ggsave(paste0(path.loess,"lmax_wochenende_01_filtered.jpeg"),device='jpeg',width=15,height=15,dpi=200)



lmax_we_03 <- loess(Motorrad ~ Lmax, data = lmax_we, span = 0.3, degree = 2)

lmax_we$loess_03 <- predict(lmax_we_03)

ggplot(lmax_we, aes(x = Lmax, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa_so)",
       x = "LAMax in dB",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22),
  )


#ggsave(paste0(path.loess,"lmax_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)



lmax_we_filtered <- subset(lmax_we, Lmax <= 95)

# loess-Regression mit gefilterten Daten durchführen
loess_model_filtered <- loess(Motorrad ~ Lmax, data = lmax_we_filtered, span = 0.3, degree = 2)

# Vorhersagen basierend auf dem Modell mit gefilterten Daten berechnen
lmax_we_filtered$loess_fit <- predict(loess_model_filtered)

# Plot mit ggplot2
ggplot(lmax_we_filtered, aes(x = Lmax, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_fit), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa_so)",
       x = "LAMax in dB",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"lmax_wochenende_03_filtered.jpeg"),device='jpeg',width=15,height=15,dpi=200)


rm(lmax_we, lmax_we_filtered, lmax_werk, lmax_we_01, lmax_we_03, lmax_werk_01, lmax_werk_03)




#----------------------------#
#* 4.2 Anzahl Motorräder ####
#--------------------------#


anz_werk <- werktag %>%
  filter(!is.na(Anz_Krad))

anz_we <- weekend %>%
  filter(!is.na(Anz_Krad))


#--------------------------------------------#
#** 4.2.1 anz_krad werk ####
#--------------------------------------------#



anz_werk_01 <- loess(Motorrad ~ Anz_Krad, data = anz_werk, span = 0.1, degree = 2)

anz_werk$loess_01 <- predict(anz_werk_01)

ggplot(anz_werk, aes(x = Anz_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "Anzahl Motorräder",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"anz_krad_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



anz_werk_03 <- loess(Motorrad ~ Anz_Krad, data = anz_werk, span = 0.3, degree = 2)

anz_werk$loess_03 <- predict(anz_werk_03)

ggplot(anz_werk, aes(x = Anz_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "Anzahl Motorräder",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"anz_krad_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)



#--------------------------------------------#
#** 4.2.2 anz_krad we ####
#--------------------------------------------#



anz_we_01 <- loess(Motorrad ~ Anz_Krad, data = anz_we, span = 0.1, degree = 2)

anz_we$loess_01 <- predict(anz_we_01)

ggplot(anz_we, aes(x = Anz_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "Anzahl Motorräder",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"anz_krad_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



anz_we_03 <- loess(Motorrad ~ Anz_Krad, data = anz_we, span = 0.3, degree = 2)

anz_we$loess_03 <- predict(anz_we_03)

ggplot(anz_we, aes(x = Anz_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "Anzahl Motorräder",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"anz_krad_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#----------------------------#
#* 4.3 NAT50 ####
#--------------------------#


werk <- werktag %>%
  filter(!is.na(NAT50))

we <- weekend %>%
  filter(!is.na(NAT50))



#--------------------------------------------#
#** 4.3.1 werk ####
#--------------------------------------------#


werk_01 <- loess(Motorrad ~ NAT50, data = werk, span = 0.1, degree = 2)

werk$loess_01 <- predict(werk_01)

ggplot(werk, aes(x = NAT50, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT50",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT50_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



werk_03 <- loess(Motorrad ~ NAT50, data = werk, span = 0.3, degree = 2)

werk$loess_03 <- predict(werk_03)

ggplot(werk, aes(x = NAT50, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT50",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT50_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#--------------------------------------------#
#** 4.3.2 we ####
#--------------------------------------------#


we_01 <- loess(Motorrad ~ NAT50, data = we, span = 0.1, degree = 2)

we$loess_01 <- predict(we_01)

ggplot(we, aes(x = NAT50, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT50",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT50_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



we_03 <- loess(Motorrad ~ NAT50, data = we, span = 0.3, degree = 2)

we$loess_03 <- predict(we_03)

ggplot(we, aes(x = NAT50, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT50",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT50_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#----------------------------#
#* 4.4 NAT60 ####
#--------------------------#


werk <- werktag %>%
  filter(!is.na(NAT60))

we <- weekend %>%
  filter(!is.na(NAT60))



#--------------------------------------------#
#** 4.4.1 werk ####
#--------------------------------------------#


werk_01 <- loess(Motorrad ~ NAT60, data = werk, span = 0.1, degree = 2)

werk$loess_01 <- predict(werk_01)

ggplot(werk, aes(x = NAT60, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT60",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT60_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



werk_03 <- loess(Motorrad ~ NAT60, data = werk, span = 0.3, degree = 2)

werk$loess_03 <- predict(werk_03)

ggplot(werk, aes(x = NAT60, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT60",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT60_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#--------------------------------------------#
#** 4.4.2 we ####
#--------------------------------------------#


we_01 <- loess(Motorrad ~ NAT60, data = we, span = 0.1, degree = 2)

we$loess_01 <- predict(we_01)

ggplot(we, aes(x = NAT60, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT60",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT60_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



we_03 <- loess(Motorrad ~ NAT60, data = we, span = 0.3, degree = 2)

we$loess_03 <- predict(we_03)

ggplot(we, aes(x = NAT60, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT60",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22),  # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT60_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#----------------------------#
#* 4.5 NAT70 ####
#--------------------------#


werk <- werktag %>%
  filter(!is.na(NAT70))

we <- weekend %>%
  filter(!is.na(NAT70))



#--------------------------------------------#
#** 4.5.1 werk ####
#--------------------------------------------#


werk_01 <- loess(Motorrad ~ NAT70, data = werk, span = 0.1, degree = 2)

werk$loess_01 <- predict(werk_01)

ggplot(werk, aes(x = NAT70, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT70",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)
  )


#ggsave(paste0(path.loess,"NAT70_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



werk_03 <- loess(Motorrad ~ NAT70, data = werk, span = 0.3, degree = 2)

werk$loess_03 <- predict(werk_03)

ggplot(werk, aes(x = NAT70, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT70",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT70_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#--------------------------------------------#
#** 4.5.2 we ####
#--------------------------------------------#


we_01 <- loess(Motorrad ~ NAT70, data = we, span = 0.1, degree = 2)

we$loess_01 <- predict(we_01)

ggplot(we, aes(x = NAT70, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT70",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT70_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



we_03 <- loess(Motorrad ~ NAT70, data = we, span = 0.3, degree = 2)

we$loess_03 <- predict(we_03)

ggplot(we, aes(x = NAT70, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT70",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT70_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#----------------------------#
#* 4.6 NAT80 ####
#--------------------------#


werk <- werktag %>%
  filter(!is.na(NAT80))

we <- weekend %>%
  filter(!is.na(NAT80))



#--------------------------------------------#
#** 4.6.1 werk ####
#--------------------------------------------#


werk_01 <- loess(Motorrad ~ NAT80, data = werk, span = 0.1, degree = 2)

werk$loess_01 <- predict(werk_01)

ggplot(werk, aes(x = NAT80, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT80",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT80_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



werk_03 <- loess(Motorrad ~ NAT80, data = werk, span = 0.3, degree = 2)

werk$loess_03 <- predict(werk_03)

ggplot(werk, aes(x = NAT80, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "NAT80",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT80_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#--------------------------------------------#
#** 4.6.2 we ####
#--------------------------------------------#


we_01 <- loess(Motorrad ~ NAT80, data = we, span = 0.1, degree = 2)

we$loess_01 <- predict(we_01)

ggplot(we, aes(x = NAT80, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT80",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT80_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



we_03 <- loess(Motorrad ~ NAT80, data = we, span = 0.3, degree = 2)

we$loess_03 <- predict(we_03)

ggplot(we, aes(x = NAT80, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "NAT80",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"NAT80_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)



#----------------------------#
#* 4.7 Emergenz ####
#--------------------------#


werk <- werktag %>%
  filter(!is.na(Emergenz))

we <- weekend %>%
  filter(!is.na(Emergenz))


#------------------------------------------#
#** 4.7.1 Emergenz als unabh. Variable ####
#------------------------------------------#

#--------------------------------------------#
#*** 4.7.1.1 werk ####
#--------------------------------------------#


werk_01 <- loess(Motorrad ~ Emergenz, data = werk, span = 0.1, degree = 2)

werk$loess_01 <- predict(werk_01)

ggplot(werk, aes(x = Emergenz, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "Emergenz",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_werktag_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



werk_03 <- loess(Motorrad ~ Emergenz, data = werk, span = 0.3, degree = 2)

werk$loess_03 <- predict(werk_03)

ggplot(werk, aes(x = Emergenz, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = "Emergenz",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_werktag_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#--------------------------------------------#
#*** 4.7.1.2 we ####
#--------------------------------------------#


we_01 <- loess(Motorrad ~ Emergenz, data = we, span = 0.1, degree = 2)

we$loess_01 <- predict(we_01)

ggplot(we, aes(x = Emergenz, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "Emergenz",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_wochenende_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)



we_03 <- loess(Motorrad ~ Emergenz, data = we, span = 0.3, degree = 2)

we$loess_03 <- predict(we_03)

ggplot(we, aes(x = Emergenz, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = "Emergenz",
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_wochenende_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)



#---------------------------------------------------------------#
#** 4.7.2 Leq als unabh. Variable, nach Emergenz gefiltert ####
#---------------------------------------------------------------#

#---------------------------------------#
#*** 4.7.2.1 Emergenz > 3 - Werktag ####
#---------------------------------------#


leq_emergenz_unter3_werk <- werktag %>%
  filter(emergenz_ueber_3 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber3_werk <- werktag %>%
  filter(emergenz_ueber_3 == 1,
         !is.na(L_Krad))

# span = 0.1
leq_werk_unter3_01 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter3_werk, span = 0.1, degree = 2)
leq_emergenz_unter3_werk$loess_01 <- predict(leq_werk_unter3_01) 

ggplot(leq_emergenz_unter3_werk, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber3_werk_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)


# span = 0.3
leq_werk_unter3_03 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter3_werk, span = 0.3, degree = 2)
leq_emergenz_unter3_werk$loess_03 <- predict(leq_werk_unter3_03) 

ggplot(leq_emergenz_unter3_werk, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber3_werk_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#-----------------------------------------#
#*** 4.7.2.2 Emergenz > 3 - Wochenende ####
#-----------------------------------------#


leq_emergenz_unter3_we <- weekend %>%
  filter(emergenz_ueber_3 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber3_we <- weekend %>%
  filter(emergenz_ueber_3 == 1,
         !is.na(L_Krad))

# span = 0.1
leq_we_unter3_01 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter3_we, span = 0.1, degree = 2)
leq_emergenz_unter3_we$loess_01 <- predict(leq_we_unter3_01) 

ggplot(leq_emergenz_unter3_we, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber3_we_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)


# span = 0.3
leq_we_unter3_03 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter3_we, span = 0.3, degree = 2)
leq_emergenz_unter3_we$loess_03 <- predict(leq_we_unter3_03) 

ggplot(leq_emergenz_unter3_we, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber3_we_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)





#---------------------------------------#
#*** 4.7.3.1 Emergenz > 2 - Werktag ####
#---------------------------------------#


leq_emergenz_unter2_werk <- werktag %>%
  filter(emergenz_ueber_3 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber2_werk <- werktag %>%
  filter(emergenz_ueber_3 == 1,
         !is.na(L_Krad))

# span = 0.1
leq_werk_unter2_01 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter2_werk, span = 0.1, degree = 2)
leq_emergenz_unter2_werk$loess_01 <- predict(leq_werk_unter2_01) 

ggplot(leq_emergenz_unter2_werk, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber2_werk_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)


# span = 0.3
leq_werk_unter2_03 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter2_werk, span = 0.3, degree = 2)
leq_emergenz_unter2_werk$loess_03 <- predict(leq_werk_unter2_03) 

ggplot(leq_emergenz_unter2_werk, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Werktag (Mo-Fr)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber2_werk_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)




#-----------------------------------------#
#*** 4.7.3.2 Emergenz > 2 - Wochenende ####
#-----------------------------------------#


leq_emergenz_unter2_we <- weekend %>%
  filter(emergenz_ueber_3 == 0,
         !is.na(L_Krad))

leq_emergenz_ueber2_we <- weekend %>%
  filter(emergenz_ueber_3 == 1,
         !is.na(L_Krad))

# span = 0.1
leq_we_unter2_01 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter2_we, span = 0.1, degree = 2)
leq_emergenz_unter2_we$loess_01 <- predict(leq_we_unter2_01) 

ggplot(leq_emergenz_unter2_we, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_01), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber2_we_01.jpeg"),device='jpeg',width=15,height=15,dpi=200)


# span = 0.3
leq_we_unter2_03 <- loess(Motorrad ~ L_Krad, data = leq_emergenz_unter2_we, span = 0.3, degree = 2)
leq_emergenz_unter2_we$loess_03 <- predict(leq_we_unter2_03) 

ggplot(leq_emergenz_unter2_we, aes(x = L_Krad, y = Motorrad)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +  # Punkte anpassen
  geom_line(aes(y = loess_03), color = "red", linewidth = 1) +  # loess-Kurve anpassen
  labs(title = "Wochenende (Sa-So)",
       x = expression(L['Aeq,1h']*' in dB'),
       y = "Lärmbelästigung Motorrad") +  
  theme_minimal() +  # Minimalistisches Theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  # Titel anpassen
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Untertitel anpassen
    plot.caption = element_text(hjust = 0, face = "italic", size = 10),  # Quellenangabe anpassen
    axis.title = element_text(face = "bold", size = 22), # Achsentitel anpassen
    axis.text = element_text(size=22)  # Achsentitel anpassen
  )


#ggsave(paste0(path.loess,"Emergenz_diff_ueber2_we_03.jpeg"),device='jpeg',width=15,height=15,dpi=200)

# Environment aufräumen
rm(leq_emergenz_ueber2_we, leq_emergenz_ueber2_werk, leq_emergenz_ueber3_we, leq_emergenz_ueber3_werk, leq_emergenz_unter2_we, leq_emergenz_unter2_werk, 
   leq_emergenz_unter3_we, leq_emergenz_unter3_werk, we_01, we_03, werk_01, werk_03, we, werk, leq_we_unter2_01, leq_we_unter2_03, leq_we_unter3_01,
   leq_we_unter3_03, leq_werk_unter2_01, leq_werk_unter2_03, leq_werk_unter3_01, leq_werk_unter3_03, anzahl_we, anz_we_01, anz_we_03, anz_werk, anz_we_01,
   anz_werk_03, loess_model_filtered, path.loess, anz_we, anz_werk_01)



#------------------#
# 5. 3D-Plots ####
#------------------#


## Werktag
# Variablen extrahieren

motorrad <- werktag$Motorrad
anzahl <- werktag$Anz_Krad
lmax <- werktag$Lmax


data <- werktag %>%
  select(Motorrad, Anz_Krad, Lmax)

data <- na.omit(data)

motorrad <- data$Motorrad
anzahl <- data$Anz_Krad
lmax <- data$Lmax

data_grid <- expand.grid(Motorrad = unique(motorrad), Lmax = unique(lmax))
data_grid$Anzahl <- NA


for (i in 1:nrow(data_grid)) {
  row <- data_grid[i, ]
  match <- werktag[werktag$Motorrad == row$Motorrad & werktag$Lmax == row$Lmax, "Anz_Krad"]
  if (length(match) == 1) {
    data_grid$Anzahl[i] <- match
  } else {
    data_grid$Anzahl[i] <- 0  # Oder eine andere geeignete Füllung
  }
}

# Erstelle die Matrix für den Oberflächenplot
anzahl_matrix <- acast(data_grid, Motorrad ~ Lmax, value.var = "Anzahl", fill = 0)

# Konvertiere die Zeilen- und Spaltennamen in numerische Werte
x_vals <- as.numeric(levels(factor(rownames(anzahl_matrix))))
y_vals <- as.numeric(levels(factor(colnames(anzahl_matrix))))

# Überprüfe die Struktur der Matrix und der Werte
str(x_vals)
str(y_vals)
str(anzahl_matrix)

# Sicherstellen, dass die Matrix numerisch ist
anzahl_matrix <- as.matrix(anzahl_matrix)

# Erstellen eines 3D-Oberflächenplots
persp3d(x_vals, y_vals, anzahl_matrix, col = "lightblue", xlab = "Anzahl Motorräder", ylab = "Lmax", zlab = "Anzahl Überschreitungen je km")

fig <- plot_ly(data, x = ~Motorrad, y = ~Lmax, z = ~Anz_Krad, type = "scatter3d", mode = "markers")
fig <- fig %>%
  layout(scene = list(xaxis = list(title = "Lärmbelästigung Motorrad"),
                      yaxis = list(title = "Lmax"),
                      zaxis = list(title = "Anzahl Motorräder")))
fig


options(expressions = 500000)
Cstack_info()


data_grid <- expand.grid(Motorrad = unique(motorrad), Lmax = unique(lmax))
data_grid$Anzahl <- NA

for (i in 1:nrow(data_grid)) {
  row <- data_grid[i, ]
  match <- werktag[werktag$Motorrad == row$Motorrad & werktag$Lmax == row$Lmax, "Anz_Krad"]
  if (length(match) == 1) {
    data_grid$Anzahl[i] <- match
  } else {
    data_grid$Anzahl[i] <- 0  # Oder eine andere geeignete Füllung
  }
}

# Erstelle die Matrix für den Oberflächenplot
anzahl_matrix <- acast(data_grid, Motorrad ~ Lmax, value.var = "Anzahl", fill = 0)

# Erstellen eines 3D-Oberflächenplots
persp3d(as.numeric(rownames(anzahl_matrix)), as.numeric(colnames(anzahl_matrix)), anzahl_matrix, col = "lightblue", xlab = "Anzahl Motorräder", ylab = "Lmax", zlab = "Anzahl Überschreitungen je km")







# Erstellen einer Matrix für den Oberflächenplot
motorrad_grid <- unique(motorrad)
lmax_grid <- unique(lmax)
anzahl_matrix <- matrix(anzahl, nrow = length(motorrad_grid), ncol = length(lmax_grid), byrow = TRUE)


# Erstellen eines 3D-Oberflächenplots
persp3d(motorrad_grid, lmax_grid, anzahl_matrix, col = "lightblue", xlab = "Anzahl Motorräder", ylab = "Lmax", zlab = "Anzahl Überschreitungen je km")



# Erstelle das 3D-Diagramm mit rgl
# open3d()
# plot3d(werktag$Motorrad, werktag$Lmax, werktag$Anz_Krad, col = 'blue', size = 3, type = 's')
# title3d(xlab = 'Motorrad', ylab = 'Lmax', zlab = 'Anzahl Motorraeder')

