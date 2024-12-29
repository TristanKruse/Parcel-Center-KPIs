# sequential procedure


rm(list = ls())
library(readxl)
library(writexl)



setwd("") # set your working directory

input <- read_excel("excelFileOutput.xlsx")
input2 <- read_excel("excelFileOutput_Alt.xlsx")


Daten <- input$Durchlaufzeit_abTor
Datenalt <- input2$Durchlaufzeit_abTor


Sequential_Method<-function(Mean_of_Replication,alpha,gamma){
  
  for (i in 2:length(Mean_of_Replication)){
    
    m<-mean(Mean_of_Replication[1:i])
    v<-var((Mean_of_Replication[1:i]))
    t<-qt(p=1-(alpha/2),df=i-1)
    hir<-t*sqrt(v/i)
    
    if((hir/abs(m))<=(gamma/(1+gamma))){
      part1<-paste("Die Anzahl der Replikationen ist ausreichend. Die maximal gew?nschte Abweichung von gamma = ", gamma," wird bei einen Sicherheitsniveau von ",1-alpha," ab ", i," Replikationen nicht ?berschritten." ,sep="")
      print(part1)
      break()	
    }
    
    if(i==length(Mean_of_Replication)){print("Die Anzahl der Replikationen ist nicht ausreichend!")}
    
  }
  
}

Sequential_Method(Daten,0.05,0.015)

Sequential_Method(Datenalt,0.05,0.015)


summary(Datenalt)
sd(Datenalt)
mean(Datenalt)
cv <- sd(Datenalt)/mean(Datenalt);cv





