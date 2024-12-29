rm(list = ls())

# packages
library(readxl)

# Determining the best alternative


setwd("")

input <- read_excel("excelFileOutput.xlsx")

input2 <- read_excel("excelFileOutput_Alt.xlsx")


Daten <- input$Durchlaufzeit_abTor

Daten_2 <- input2$Durchlaufzeit_abTor

mean_d <- mean(Daten)
mean_d2 <- mean(Daten_2)
percentage_dlz <- 1- (mean(Daten_2)/mean(Daten));percentage_dlz



Daten_Auslastung <-  input$UtzAbträger

Daten_Auslastung_Alt <- input2$UtzAbträger

mean_auslastung <- mean(Daten_Auslastung)
mean_auslastung_2 <- mean(Daten_Auslastung_Alt)

percentage_ausl <- 1- (mean(mean_auslastung_2)/mean(mean_auslastung));percentage_ausl




input00 <- read_excel("excelFileOutput.xlsx", sheet=2)

input11 <- read_excel("excelFileOutput_Alt.xlsx", sheet=2)

Daten_Pakete <- input00$nPakete_bearbeitet

Daten_Pakete_Alt <- input11$nPakete_bearbeitet


mean_auslastung <- mean(Daten_Pakete)
mean_auslastung_2 <- mean(Daten_Pakete_Alt)

percentage_anzahl <- 1- (mean(Daten_Pakete_Alt)/mean(Daten_Pakete));percentage_anzahl





####################################################################################
#Confidence interval
#Parameter1: Vector with observations
#Parameter2: Confidence level alpha


confidence_interval<-function(sample, alpha){
  m<-mean(sample)
  v<-var(sample)
  t<-qt(p=1-(alpha/2),df=length(sample)-1)
  hir<-t*sqrt(v/length(sample))
  lowerb<-m-hir
  upperb<-m+hir

  print(lowerb)
  print(upperb)
  
  plot(sample,	
       xlab='Observation',
       ylab='Value')
  
  abline(h=lowerb, col='red')
  abline(h=m, col='black')
  abline(h=upperb, col='red')
  
}


######################################################################################
#Difference Test (Paired t-Test)

# Für die DLZ

input4 <- Daten
input4_2 <- Daten_2


confidence_interval(input4, 0.05)
confidence_interval(input4_2, 0.05)

#Manual calculation

#Set alpha
alpha<-0.05

#Calculate differences of the two samples
Dif<-input4-input4_2

#Calculate the mean of differences
m<-mean(Dif)
m

#Claculate the variance of differences
v<-var(Dif)
v

#Claculate t value with the quantile function (qt)
t<-qt(p=1-(alpha/2),df=length(input4_2)-1)
t

#Claculate the half interval range
hir<-t*sqrt(v/length(input4_2))

#Determine the lower bound of the conf. interval by subracting the hir from the mean
lowerb<-m-hir

#Determine the upper bound of the conf. interval by adding the hir to the mean
upperb<-m+hir


#Observe if the interval covers the 0
lowerb
upperb



######################################################################################
#Difference Test (Paired t-Test)

# Für die Auslastung

input4 <- Daten_Auslastung
input4_2 <- Daten_Auslastung_Alt

confidence_interval(input4,0.05)
confidence_interval(input4_2,0.05)

#Manual calculation

#Set alpha
alpha<-0.05

#Calculate differences of the two samples
Dif<-input4-input4_2

#Calculate the mean of differences
m<-mean(Dif)
m

#Claculate the variance of differences
v<-var(Dif)
v

#Claculate t value with the quantile function (qt)
t<-qt(p=1-(alpha/2),df=length(input4_2)-1)
t

#Claculate the half interval range
hir<-t*sqrt(v/length(input4_2))

#Determine the lower bound of the conf. interval by subracting the hir from the mean
lowerb<-m-hir

#Determine the upper bound of the conf. interval by adding the hir to the mean
upperb<-m+hir


#Observe if the interval covers the 0
lowerb
upperb






######################################################################################
#Difference Test (Paired t-Test)

# Für die bearbeiteten Pakete


input4 <- Daten_Pakete
input4_2 <- Daten_Pakete_Alt

confidence_interval(input4,0.05)
confidence_interval(input4_2,0.05)

#Manual calculation

#Set alpha
alpha<-0.05

#Calculate differences of the two samples
Dif<-input4-input4_2

#Calculate the mean of differences
m<-mean(Dif)
m

#Claculate the variance of differences
v<-var(Dif)
v

#Claculate t value with the quantile function (qt)
t<-qt(p=1-(alpha/2),df=length(input4_2)-1)
t

#Claculate the half interval range
hir<-t*sqrt(v/length(input4_2))

#Determine the lower bound of the conf. interval by subracting the hir from the mean
lowerb<-m-hir

#Determine the upper bound of the conf. interval by adding the hir to the mean
upperb<-m+hir


#Observe if the interval covers the 0
lowerb
upperb





