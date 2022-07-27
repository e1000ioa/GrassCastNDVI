
setwd("C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script")
library(ggplot2)
library(dplyr)
set.seed(123)

#readexcel
GC <- read.csv(file = "az_nm_2000_20202.csv", head = TRUE, sep=",")

#ONLY ARIZONA 
GC <- subset(GCID, GCID$lon < -109.4)

#Subsest by year
GC20 <- subset(GC, year == '2020', select=c("spring_z_ndvi","lon","lat"))
write.csv(GC20, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\2GC20.csv', row.names = TRUE)

#merge all Years
GCY <- aggregate(GC, by = list(GC$year), FUN = mean)
#export to excel
write.csv(GCY, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\GCY.csv')

#merge all Years in Grindrs
GCID <- round(aggregate(GC, by = list(GC$gridID), FUN = mean),5)

#Check distribution for NDVI
hist(GCID$spring_z_ndvi) #Spring Z ndvi had a normal distribution with long tail
histrv <- hist(GCID$spring_z_ndvi, nclass=7)
histrv$breaks
histrv$density

#Z NDVI eliminate outliners 
out <- boxplot(GCID$spring_z_ndvi, plot=FALSE)$out
GCID <- GCID[-which(GCID$spring_z_ndvi %in% out),] #eliminates 1 variables from table


#Creates Class for spring Z NDVI 
GCID$Class <- cut(GCID$spring_z_ndvi,c(-0.00020, -0.00015, -0.00010, -0.00005, 0.00000, 0.00005, 0.00010, 0.00015, 0.00020),labels = FALSE)-1 #Must start at 0 for GEE
hist(GCID$Class, nclass=7)

#Z ANNP eliminate outliners 
out <- boxplot(GCID$spring_z_ndvi, plot=FALSE)$out
GCID <- GCID[-which(GCID$spring_z_anpp %in% out),] #eliminates 3 variables from table

boxplot(GCID$spring_z_anpp,GCID$spring_z_anpp)

#Check distribution spring z ANPP 
boxplot(GCID$spring_z_anpp) 
histrvAN <- hist(GCID$spring_z_anpp)
histrvAN$breaks
histrvAN$density

#Creates Class for spring Z ANPP
GCID$ClassA <- cut(GCID$spring_z_anpp,c(-0.00020, -0.00015, -0.00010, -0.00005, 0.00000, 0.00005, 0.00010, 0.00015, 0.00020),labels = FALSE)-1 #Must start at 0 for GEE
hist(GCID$ClassA, nclass=8)

#Boxplot for  bot NDVI and ANNPP
boxplot(GCID$ClassA,GCID$Class)
boxplot(GCID$spring_z_anpp,GCID$spring_z_ndv)

#Creates Traing CSV
Class_ZNDVI <- subset(GCID, select=c("Class","lon","lat"))
Class_ZANNP <- subset(GCID, select=c("ClassA","lon","lat"))

write.csv(Class_ZNDVI, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\Class_ZNDVI.csv')
write.csv(Class_ZANNP, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\Class_ZANNP.csv')


#Creates Samples
index <- sample(nrow(Class_ZNDVI),250,replace = TRUE, prob = Class_ZNDVI$Class+1) #probability should not be 0
Training_ZNDVI <- Class_ZNDVI[index,]

hist(Training_ZNDVI$Class, nclass=5)$breaks
summary(Training_ZNDVI)

set.seed(12)
index <- sample(1:nrow(Class_ZANNP),50,replace = TRUE,prob = Class_ZANNP$ClassA+1) #probability should not be 0
Training_ZANNP <- Class_ZANNP[index,]

hist(Training_ZANNP$ClassA)
summary(Training_ZANNP)

write.csv(Training_ZNDVI, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\Training_ZNDVI_2.csv')
write.csv(Training_ZANNP, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\Training_ZANNP_4.csv')


#Creates a graft with z and delta information from ANNP
AnnpG <- ggplot(GCID, aes(x = gridID, y = spring_delta_anpp)) +
  geom_line()

AnnpG

#Creates a graft with z and delta information from ANNP
NDVIG2 <- ggplot(GCY, aes(x = year, y = spring_delta_ndvi, fill=spring_z_ndvi)) +
  geom_col(position = "dodge", colour = "black", size = 0.25)

NDVIG2


#Creates a graft with z and delta information from ANNP
NDVIAN <- ggplot(GCY, aes(x = year, y = spring_delta_ndvi, fill=spring_delta_anpp)) +
  geom_col(position = "dodge", colour = "black", size = 0.25)

NDVIAN

plot(GCY$spring_delta_ndvi, GCY$spring_delta_anpp)
sqr


ggplot(GC29, aes(displ="spring_delta_anpp.x", hwy="spring_delta_anpp.y", colour = class)) + 
  geom_point()

plot(GC29$spring_delta_anpp.x,GC29$spring_delta_anpp.y)
lines(GC29$spring_delta_anpp.x, GC29$spring_delta_anpp.y, type = "l", lty = 1)
      
      