rm(list=ls())

#install.packages("readxl")
#install.packages("writexl")
#install.packages("reshape")
#install.packages("patchwork")


library(readxl)
library(writexl)
library(reshape)
library(ggplot2)
library(tidyverse)
library(dplyr) 
library(patchwork)

setwd("")

#Auslastung
#Rohdaten
Auslastung_colnames <- c("Time", "UtzVorsorter", "UtzNachverpackung",
              "UtzSchwergut", "UtzNachcodierer", "UtzAbträger")
data_R1_A <- read_excel("KPIs_R1.xlsx")
data_R1_A <- data_R1_A[, Auslastung_colnames]
data_R2_A <- read_excel("KPIs_R2.xlsx")
data_R2_A <- data_R2_A[, Auslastung_colnames]
data_R3_A <- read_excel("KPIs_R3.xlsx")
data_R3_A <- data_R3_A[, Auslastung_colnames]
data_R4_A <- read_excel("KPIs_R4.xlsx")
data_R4_A <- data_R4_A[, Auslastung_colnames]
data_R5_A <- read_excel("KPIs_R5.xlsx")
data_R5_A <- data_R5_A[, Auslastung_colnames]

#View(data_R1_A)


#long format
df_R1_A <- data.frame(x=data_R1_A["Time"], data_R1_A[,2:6])
df_R1_A <- melt(df_R1_A, id.vars = "Time")
#View(df_R1_A)
colnames(df_R1_A) <- c("Zeit", "Teilbereich", "Auslastung")

cols <- c("green", "grey", "purple3", "sienna1", "yellow3")

ggplot(df_R1_A, aes(x=Zeit, y=Auslastung, color = Teilbereich))+
  geom_line()+
  scale_color_manual(values = cols)


#Anzahl
#Rohdaten
Anzahl_colnames <- c("Time", "nPakete", "nPakete_bearbeitet",
                     "nPakete_beschädigt", "nPakete_Schwer")
data_R1_Z <- read_excel("KPIs_R1.xlsx", sheet = 2)
data_R1_Z <- data_R1_Z[, Anzahl_colnames]
data_R2_Z <- read_excel("KPIs_R2.xlsx", sheet = 2)
data_R2_Z <- data_R2_Z[, Anzahl_colnames]
data_R3_Z <- read_excel("KPIs_R3.xlsx", sheet = 2)
data_R3_Z <- data_R3_Z[, Anzahl_colnames]
data_R4_Z <- read_excel("KPIs_R4.xlsx", sheet = 2)
data_R4_Z <- data_R4_Z[, Anzahl_colnames]
data_R5_Z <- read_excel("KPIs_R5.xlsx", sheet = 2)
data_R5_Z <- data_R5_Z[, Anzahl_colnames]
#View(data_R1_Z)

#letzter Zeile #data_R1_Z[nrow(data_R1_Z),]
#Zusammenfassung statistischer Werte der 5 Instanzen
Zf_Z <- rbind(tail(data_R1_Z,1),tail(data_R2_Z,1),tail(data_R3_Z,1), tail(data_R4_Z,1), tail(data_R5_Z,1))
Zf_Z_Statistik <- rbind(sapply(Zf_Z[2:5],function(x) c(summary(x), sd(x), var(x))))
rownames(Zf_Z_Statistik) <- c(rownames(Zf_Z_Statistik)[1:6],"Standardabweichung", "Varianz")
Zf_Z_Statistik
# sapply(Zf_Z[2:5],mean)
# sapply(Zf_Z[2:5],sd)
# sapply(Zf_Z[2:5],var)


#long format
df_R1_Z <- data.frame(x=data_R1_Z["Time"], data_R1_Z[,2:5])
df_R1_Z <- melt(df_R1_Z, id.vars = "Time")
#View(df_R1_Z)
colnames(df_R1_Z) <- c("Zeit", "Merkmale", "Anzahl")

cols <- c("palegreen3", "seagreen", "tomato3", "tan")

ggplot(df_R1_Z, aes(x=Zeit, y=Anzahl, color = Merkmale))+
  geom_line()+
  scale_color_manual(values = cols)    



#DLZ
data_Test_D <- read_excel("DLZ_Test.xlsx")
data_Test_D <- data_Test_D[-3]
colnames(data_Test_D) <- c("Endzeit", "Durchlaufzeit", "Durchlaufzeit_abTor")
#View(data_Test_D)
data_Test_D <- add_column(data_Test_D, Startzeit=data_Test_D$Endzeit-data_Test_D$Durchlaufzeit, .before="Endzeit")
data_Test_D <- add_column(data_Test_D, Startzeit_abTor=data_Test_D$Endzeit-data_Test_D$Durchlaufzeit_abTor, .before="Durchlaufzeit")
data_Test_D <- add_column(data_Test_D, Wartezeit_vorTor=data_Test_D$Startzeit_abTor-data_Test_D$Startzeit, .after="Durchlaufzeit_abTor")
data_Test_D


#Boxplot - Durchlaufzeit, Durchlaufzeit_abTor, Wartezeit
#par(mfrow=c(1,5))
boxplot(data_Test_D[4:6])

#Zusammenfassung statistischer Werte einer Instanz
Zf_R1_DLZ_Statistik <- sapply(data_Test_D[4:6], function(x) c(summary(x), sd(x), var(x)))
rownames(Zf_R1_DLZ_Statistik) <- c(rownames(Zf_R1_DLZ_Statistik)[1:6], "Standardabweichung", "Varianz")
Zf_R1_DLZ_Statistik

DLZ1 <- data_Test_D$Durchlaufzeit_abTor


#long format
breaks <- c(-Inf, 60,120,180,240,300,360,420,480)
labels <- c(1:8)
#Zeitfenster wird anhand Startzeit kategorisiert
df_data_Test_D <- data_Test_D %>% 
  mutate(Zeitfenster = cut(data_Test_D$Startzeit, breaks = breaks, labels = labels))
#Zeitfenster_abTor wird anhand Startzeit_abTor kategorisiert
df_data_Test_D <- df_data_Test_D %>% 
  mutate(Zeitfenster_abTor = cut(data_Test_D$Startzeit_abTor, breaks = breaks, labels = labels))

View(df_data_Test_D)

plot(df_data_Test_D$Startzeit, df_data_Test_D$Wartezeit_vorTor)

#Statistische Werte nach Zeitfenster

t <- df_data_Test_D %>%
      group_by(Zeitfenster) %>%
      summarize(
        min(Durchlaufzeit),
        quantile(Durchlaufzeit, prob=0.25),
        median(Durchlaufzeit),
        mean(Durchlaufzeit),
        quantile(Durchlaufzeit, prob=0.75),
        max(Durchlaufzeit),
        sd(Durchlaufzeit),
        var(Durchlaufzeit),
        
        min(Durchlaufzeit_abTor),
        quantile(Durchlaufzeit_abTor, prob=0.25),
        median(Durchlaufzeit_abTor),
        mean(Durchlaufzeit_abTor),
        quantile(Durchlaufzeit_abTor, prob=0.75),
        max(Durchlaufzeit_abTor),
        sd(Durchlaufzeit_abTor),
        var(Durchlaufzeit_abTor))

View(t)

t2 <- df_data_Test_D %>%
  group_by(Zeitfenster_abTor) %>%
  summarize(
    min(Durchlaufzeit),
    quantile(Durchlaufzeit, prob=0.25),
    median(Durchlaufzeit),
    mean(Durchlaufzeit),
    quantile(Durchlaufzeit, prob=0.75),
    max(Durchlaufzeit),
    sd(Durchlaufzeit),
    var(Durchlaufzeit),
    
    min(Durchlaufzeit_abTor),
    quantile(Durchlaufzeit_abTor, prob=0.25),
    median(Durchlaufzeit_abTor),
    mean(Durchlaufzeit_abTor),
    quantile(Durchlaufzeit_abTor, prob=0.75),
    max(Durchlaufzeit_abTor),
    sd(Durchlaufzeit_abTor),
    var(Durchlaufzeit_abTor))

View(t2)

#graph (long format?)

plot1 <- ggplot(df_data_Test_D, aes(x=Zeitfenster, y=Durchlaufzeit))+
  geom_boxplot()

plot2 <- ggplot(df_data_Test_D, aes(x=Zeitfenster, y=Durchlaufzeit_abTor))+
  geom_boxplot()

plot3 <- ggplot(df_data_Test_D, aes(x=Zeitfenster_abTor, y=Durchlaufzeit))+
  geom_boxplot()

plot4 <- ggplot(df_data_Test_D, aes(x=Zeitfenster_abTor, y=Durchlaufzeit_abTor))+
  geom_boxplot()

(plot1 + plot2) / (plot3 + plot4)

summary(DLZ1)

df_DLZ1 <- data.frame(Nummer=c(1:dim(DLZ1)[1]), data_Test_D$Startzeit, data_Test_D$Durchlaufzeit_abTor)
Startzeit <- data_Test_SE["Startzeit"]

#df_DLZ1 <- data.frame(Nummer=c(1:dim(DLZ1)[1]), Zeitfenster=NA, Startzeit[1:dim(DLZ1)[1],], DLZ1)

df_DLZ1 <- df_DLZ1 %>% 
  mutate(Zeitfenster = cut(Startzeit, breaks = breaks, labels = labels))

View(df_DLZ1)

ggplot(df_DLZ1, aes(x=Zeitfenster, y=Durchlaufzeit_abTor))+
  geom_boxplot()

ggplot(df_DLZ1, aes(x=Zeitfenster, y=Durchlaufzeit_abTor))+
  geom_point()


ggplot(df_DLZ1, aes(x=Startzeit, y=Durchlaufzeit_abTor))+
  geom_point()

