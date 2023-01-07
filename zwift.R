library(readr)
zwiftFrameData <- read_csv("C:/Users/Jakob/Desktop/zwiftFrameData.txt", 
                           skip = 1, trim_ws = T)

colnames(zwiftFrameData) = c("Bike","Flat","Climb")
zwiftFrameData

#install.packages("Benchmarking")
library(Benchmarking)

nrow(zwiftFrameData)


zwiftFrameData = zwiftFrameData[-which(is.na(zwiftFrameData$Bike)), ]
zwiftFrameData = head(zwiftFrameData,-4)
zwiftFrameData = as.data.frame(zwiftFrameData)
zwiftFrameData = cbind(zwiftFrameData, watt=300)

zwiftFrameData
##
##library(ggplot2)
##ggplot(data=zwiftFrameData, aes(x=Flat, y=Climb, color=Bike)) +
##  geom_point(stat="identity", size=4)+ theme(legend.position = "none")+
##  geom_text(label=zwiftFrameData$Bike, size = 3)



library(Benchmarking) #load the Benchmarking library

DEA_VRS_OUT <- dea(zwiftFrameData$Flat, zwiftFrameData$Climb, RTS="VRS", ORIENTATION = "in", SLACK = TRUE, DUAL=TRUE)
summary(DEA_VRS_OUT)
dea.plot(zwiftFrameData$Flat, zwiftFrameData$Climb, RTS="VRS", ORIENTATION = "in", 
          xlim=c(3075,3100), ylim=c(2930,2980), lty="dashed",lwd=1,
          cex=1.35, pch=1)

for(i in 1:nrow(zwiftFrameData)){
  text(zwiftFrameData[i,2],zwiftFrameData[i,3],zwiftFrameData$Bike[i], pos=sample(1:4,1), adj=(runif(1,0,1)), cex=0.8)
}


zwiftFrameData = cbind(zwiftFrameData, eff=efficiencies(DEA_VRS_OUT), peers(DEA_VRS_OUT), lambda(DEA_VRS_OUT))
zwiftFrameData



zwiftFrameData[order(zwiftFrameData$eff),]



x <- matrix(zwiftFrameData$watt,ncol=1) #define inputs (vektor med 300)
y <- matrix(zwiftFrameData$Flat,ncol=1) #define outputs (matrix med flat, climb tider)
e_vrs <- dea(x,y, RTS="VRS", ORIENTATION = "in", SLACK = TRUE, DUAL=TRUE)#solve LP problem
summary(e_vrs)
dea.plot(x,y)

x <- matrix(c(20, 40, 40, 60, 70, 50),ncol=1) #define inputs
y <- matrix(c(20, 30, 50, 40, 60, 20),ncol=1) #define outputs
e_vrs <- dea(x,y, RTS="vrs", ORIENTATION="in-out")#solve LP problem
summary(e_vrs)
dea.plot(x,y, RTS="vrs", ORIENTATION="out")
text(x,y, c("A","B","C","D","E","F"), pos=2)
e_vrs
