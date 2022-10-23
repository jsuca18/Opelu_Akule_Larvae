#making GAMs for Emily's opelu project 

setwd("E:/Opelu_Akule")

Opelu_Collection_Data<-read.csv("Opelu_Temperature_Matching.csv", header=TRUE)

#identify the number of gear types in this data frame
unique(Opelu_Collection_Data$Gear)

#how many larvae were captured in each gear type 
#but we only care about frequency of occurrence, so change that 

Opelu_Collection_Data$PA<-ifelse(Opelu_Collection_Data$Count>0, 1, 0)
#how many presence stations are there in total 
sum(Opelu_Collection_Data$PA, na.rm=T)
#only 23..... not sure we will be able to do much 
sum(Opelu_Collection_Data$PA[Opelu_Collection_Data$Gear=="1m ring-net"])
#and it is even distributed among gear types.... 
library(lubridate)

Opelu_Collection_Data$Date_R<-ymd(Opelu_Collection_Data$Date)
library(dplyr)
Opelu_Collection_Data=Opelu_Collection_Data %>% 
  dplyr::mutate(Year = lubridate::year(DATE_R), 
                month = lubridate::month(DATE_R), 
                day = lubridate::day(DATE_R),
                DOY=lubridate::yday(DATE_R))
library(mgcv)
Opelu_Collection_Data$Year<-as.factor(Opelu_Collection_Data$Year)
Opelu_Collection_Data$Dist.2.shore.m<-as.numeric(Opelu_Collection_Data$Dist.2.shore.m)
Opelu_Collection_Data_Complete<-Opelu_Collection_Data[!is.na(Opelu_Collection_Data$Daily_SST_Val) & !is.na(Opelu_Collection_Data$Dist.2.shore.m) & !is.na(Opelu_Collection_Data$Vol.m3),]

#remove 'partial' slick samples
Opelu_Reduced<-Opelu_Collection_Data_Complete[Opelu_Collection_Data_Complete$Slick!=1,]

#set up separate candidate models as month and temp are too interrelated
Opelu_Model_Month<-gam(PA~Year+s(month,k=6,bs="cc")+offset(log(Vol.m3))+Gear+Slick+s(log10(Dist.2.shore.m),k=4), data=Opelu_Reduced, family=binomial, na.action="na.fail")
Opelu_Model_Temp<-gam(PA~Year+offset(log(Vol.m3))+Gear+Slick+s(Daily_SST_Val,k=4)+s(log10(Dist.2.shore.m),k=4), data=Opelu_Reduced, family=binomial, na.action="na.fail")

summary(Opelu_Model_Month)

summary(Opelu_Model_Temp)

#now look into residuals for temporal and spatial autocorrelation 
Opelu_Reduced$Residuals<-resid(Opelu_Model_Temp)

boxplot(Opelu_Reduced$Residuals~Opelu_Reduced$Year)
cor.test(Opelu_Reduced$Residuals,as.numeric(Opelu_Reduced$Year))

boxplot(Opelu_Reduced$Residuals~Opelu_Reduced$month)
cor.test(Opelu_Reduced$Residuals,Opelu_Reduced$month)
#no egregious signs of temporal autocorrelation

#now look at spatial autocorrelation 
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
png("Opelu_Model_Spatial_Residuals.png", height=5, width=6, units="in", res=300)
ggplot(data=world) + geom_raster(data = Depth_dataframe,aes(x=Lon, y = Lat, fill=Depth)) + scale_fill_gradient(low = "cornflowerblue", high = "grey",limits=c(-7000,0))+new_scale_fill()+
  coord_fixed(ratio = 1)+geom_point(data=Opelu_Reduced, aes(x=Lon.start, y=Lat.start, colour=Residuals))+scale_color_gradient2(low="blue", mid="white", high="red", limits=c(-3,3))+
  theme_bw()+labs(fill = "Depth (m)")+geom_sf()+coord_sf(xlim=c(-156.75,-155.25), ylim=c(18.75, 20.3))+
  theme(legend.title=element_text(size=16),legend.text=element_text(size=14),legend.direction = "vertical", legend.box = "vertical")+ggtitle("Opelu Model Residuals")
dev.off()

