#making GAMs for Emily's Akule project 
library(lubridate)
library(dplyr)
library(mgcv)

setwd("E:/Opelu_Akule")

Akule_Collection_Data<-read.csv("Akule_Temperature_Matching_2016.csv", header=TRUE)
Akule_Collection_Data<-Akule_Collection_Data[Akule_Collection_Data$Taxa=="Selar",]
#identify the number of gear types in this data frame
unique(Akule_Collection_Data$Gear)
#only one, here, all ring net 

#generate a PA variable for this aspect of code
Akule_Collection_Data$PA<-ifelse(Akule_Collection_Data$Count>0, 1, 0)
#how many presence stations are there in total 
sum(Akule_Collection_Data$PA, na.rm=T)
#30 individual larvae 

Akule_Collection_Data$Date_R<-ymd(Akule_Collection_Data$Date)
Akule_Collection_Data=Akule_Collection_Data %>% 
  dplyr::mutate(Year = lubridate::year(DATE_R), 
                month = lubridate::month(DATE_R), 
                day = lubridate::day(DATE_R),
                DOY=lubridate::yday(DATE_R))

#correct the format of distance to shore 
Akule_Collection_Data$Dist.2.shore.m<-as.numeric(Akule_Collection_Data$Dist.2.shore.m)

#get rid of any non-NAs
Akule_Collection_Data_Complete<-Akule_Collection_Data[!is.na(Akule_Collection_Data$Daily_SST_Val) & !is.na(Akule_Collection_Data$Dist.2.shore.m) & !is.na(Akule_Collection_Data$Vol.m3),]


sum(Akule_Collection_Data_Complete$PA[Akule_Collection_Data_Complete$Tow=="neuston"])/length(Akule_Collection_Data_Complete$PA[Akule_Collection_Data_Complete$Tow=="neuston"])
sum(Akule_Collection_Data_Complete$PA[Akule_Collection_Data_Complete$Tow=="vertical"])/length(Akule_Collection_Data_Complete$PA[Akule_Collection_Data_Complete$Tow=="vertical"])
#similar occurrence in each

#making a model with month, seeing if this will work (unlikely)
Akule_Model<-gam(PA~Tow+offset(log(Vol.m3))+Slick+s(Daily_SST_Val,k=4)+s(log10(Dist.2.shore.m),k=4), data=Akule_Collection_Data_Complete, family=binomial, na.action="na.fail")
plot(Akule_Model)
summary(Akule_Model)

#there is not real temporal component here, but lets look at spatial autocorr

Akule_Collection_Data_Complete$Residuals<-resid(Akule_Model)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(ggnewscale)
setwd("C:/Mahi_Diet")
Depth_Hawaii<-raster("Hawaii_Bedrock.tiff")
Depth_dataframe<-as.data.frame(Depth_Hawaii, xy=TRUE)
colnames(Depth_dataframe)<-c("Lon","Lat","Depth")
world<-ne_countries(scale="medium", returnclass = "sf")

setwd("E:/Opelu_Akule")
png("Akule_Model_Spatial_Residuals.png", height=5, width=6, units="in", res=300)
ggplot() + geom_raster(data = Depth_dataframe,aes(x=Lon, y = Lat, fill=Depth)) + scale_fill_gradient(low = "cornflowerblue", high = "grey",limits=c(-7000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_point(data=Akule_Collection_Data_Complete, aes(x=Lon.start, y=Lat.start, colour=Residuals))+scale_color_gradient2(low="blue", mid="white", high="red", limits=c(-2.5,2.5))+
  theme_bw()+labs(fill = "Depth (m)")+geom_sf()+coord_sf(xlim=c(-156.2,-155.8), ylim=c(19, 20.1))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("Akule Model Residuals")
dev.off()



