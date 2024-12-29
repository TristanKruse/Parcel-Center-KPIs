rm(list = ls())
library(readxl)
library(writexl)



setwd("")

# Production rate
data_production_rate<- read_excel("excelFileOutput.xlsx", sheet = 3)


df <- data.frame(data_production_rate[,2])


# Calculation of the production rate, ifelse statement to avoid divide by zero.

df$production_rate1 <- ifelse(data_production_rate[,1] != 0, data_production_rate[,1] / data_production_rate[,2], 0)
df$production_rate2 <- ifelse(data_production_rate[,3] != 0, data_production_rate[, 3] / data_production_rate[,2], 0)
df$production_rate3 <- ifelse(data_production_rate[,5] != 0, data_production_rate[, 5] / data_production_rate[,2], 0)
df$production_rate4 <- ifelse(data_production_rate[,7] != 0, data_production_rate[, 7] / data_production_rate[ 2], 0)
df$production_rate5 <- ifelse(data_production_rate[,9] != 0, data_production_rate[, 9] / data_production_rate[, 2], 0)



##############################################################################
#Method by Welch
#The following two functions are designed to determine the required warm-up period
#for a simulation experiment


#Welch_Explore
#Parameter1: Dataframe or Matrix with observed values col: replications rows: observations
#Parameter2: maximum w which shall be observed
Welch_Explore<-function(Replication_Table, wmax){
  
  #Calculation of the Replication_Table Dimension
  
  #Sample Size
  n<-dim(Replication_Table)[1]
  
  #Number of Replications
  #r<-dim(Replication_Table)[2]
  
  #Calculation of the mean for every observation
  #Y<-apply(Replication_Table[,-1],1,mean)
  
  #Plotting device	
  par(mfrow=c(ceiling((wmax+1)/3),3)) 
  
  #Start loop for different w
  
  
  for (w in 0:wmax){
    #Creation of the vector for the centered, moving average for w (Gliederzahl 2w+1)
    Yw<-rep(NA,n-w)
    
    
    #Calculation of the centred, moving average
    for(i in 1:(n-w)){
      if(i<=w){Yw[i]<-sum(Replication_Table[,2][c(1:(2*i-1))])/(2*i-1)}
      else {Yw[i]<-sum(Replication_Table[,2][c((i-w):(i+w))])/(2*w+1)}
      
    }
    plot(Yw,
         type= "l",
         main=paste("w=",w,sep=" "),
         xlab='Simulationtime',
         ylab='Centred Moving Average',
         xaxt="n")
    
    axis(side=1,at=c(1:n),labels=Replication_Table[,1])
    
    
    print(Yw)		
  }	
  #End loop for different w
}


#Welch_Print
#Parameter1: Dataframe or Matrix with observed values col: replications rows: observations
#Parameter2: w which shall be observed
#Parameter3: End of warm-up period

Welch_Print<-function(Replication_Table, w, b){
  
  #Calculation of the Replication_Table Dimension
  
  #Sample Size
  n<-dim(Replication_Table)[1]
  
  #Number of Replications
  #r<-dim(Replication_Table)[2]
  
  #Calculation of the mean for every observation
  #Y<-apply(Replication_Table[,-1],1,mean)
  
  
  
  
  #Creation of the vector for the centered, moving average for w (Gliederzahl 2w+1)
  Yw<-rep(NA,n-w)
  
  
  
  #Calculation of the centred, moving average
  
  for(i in 1:(n-w)){
    if(i<=w){Yw[i]<-sum(Replication_Table[,2][c(1:(2*i-1))])/(2*i-1)}
    else {Yw[i]<-sum(Replication_Table[,2][c((i-w):(i+w))])/(2*w+1)}
    
  }
  
  #Plot of the calculated values	
  plot(Yw,
       type= "l",
       main=paste("w=",w,sep=" "),
       xlab='Simulationszeit',
       ylab='Produktionsrate',
       xaxt="n")
  
  axis(side=1,at=c(1:n),labels=Replication_Table[,1])
  
  abline(v=which(Replication_Table[,1]==b), col='red')
  print(Yw)
  
}


#Choose an appropriate max w and determine a plausibile warm-up period
X11()
Welch_Explore(df,14)

dev.off()


#Select a fitting w and select the end of the warm up period
par(mfrow = c(3,1))

Welch_Print(df,1,20)
