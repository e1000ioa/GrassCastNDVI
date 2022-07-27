
setwd("C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script")
library(ggplot2)
library(writexl)


#readexcel
GC <- read.csv(file = "az_nm_2000_20202.csv", head = TRUE, sep=",")

#Subsest by year
GC20 <- subset(GC, year == '2020', select=c("gridID","spring_delta_ndvi"))
GC19 <- subset(GC, year == '2019', select=c("gridID","spring_delta_ndvi"))

#Merge all 20 and 19 by grind
GC29 <- merge(GC20, GC19, by = "gridID", all = TRUE)

#merge all Years in one
GCY <- aggregate(GC, by = list(GC$year), FUN = mean)
#export to excel
write_xlsx(GCY, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\GCY.csv')

#merge all Years in Grindrs
GCID <- aggregate(GC, by = list(GC$gridID), FUN = mean)
write_xlsx(GCID, 'C:\\users\\aguilarcubilla\\iCloudDrive\\Documents\\Research\\GrassCast\\R_access_script\\GCID.csv')


#Creates a graft with z and delta information from ANNP
AnnpG <- ggplot(GCY, aes(x = year, y = spring_delta_anpp, fill=spring_z_anpp)) +
  geom_col(position = "dodge", colour = "black", size = 0.25)

AnnpG

#Creates a graft with z and delta information from ANNP
NDVIG2 <- ggplot(GCY, aes(x = year, y = spring_z_ndvi, fill=spring_z_ndvi)) +
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

plot(GC29$spring_delta_anpp.x, GC29$spring_delta_anpp.y, xlim=30)
lines(GC29$spring_delta_anpp.x, GC29$spring_delta_anpp.y, type = "l", lty = 1)
      