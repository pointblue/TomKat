##########################################################
#LOL Soil Plots
#Adapted from Wrangler or my own scatters
#Use for multiple points and multiple sampling years
#May have to modify resample code based on your situation
#Rev. from San Mateo 2024 code by Mel Preston Jan 2025
#########################################################
#Set working directory, read in data and background files
rm(list=ls()) #clear everything before starting
setwd("/Users/melpreston/Desktop/R")

#Calling each function rather than loading in library
#Different syntax but helps with conflicts between functions in different packages
require(soiltexture)
require(stringr)
require(ggplot2)
require(ggrepel)
require(dplyr)

#Read in the data, with only two ranches it's easier to just download transects separately in CADC
#data<-read.csv("clov_soil.csv")
#data<-subset(data, data$Point.Name != "CLOV-232") #only sampled in 2015 and 2018, exclude from the jump
data<-read.csv("toka_soil.csv")

###############################################
#Data Formatting
###############################################
#Format Date fields
#Choose which way your date is formatted
#data$DATE <- as.Date( as.character(data$Date), ("%m/%d/%Y"))
data$DATE <- as.Date( as.character(data$Date), "%Y-%m-%d")
data$YEAR<-as.numeric(format(data$DATE, format = "%Y"))
data$SURVEY<-as.factor(paste(data$Point.Name,"-",data$YEAR,sep=""))

#Format bulk density measurements
#Force the correct bulk density and water volume dimensions
#Only run the next three lines if you used standard bulk density and infiltration rings.
data$Ring.Infiltrometer.Diameter<-15.2
data$Bulk.Density.Diameter<-5.2
data$Water.Volume<-450
data$Bulk.Density.Height<-7.5

#Add columns to calculate the volume of the bulk density ring and calculate the bulk density, corrected for rocks
data$Total.Volume<-pi*((data$Bulk.Density.Diameter/2)^2)*data$Bulk.Density.Height
data$Bulk.Density<-(data$Bulk.Density.Dry.Wt/(data$Total.Volume-data$Bulk.Density.Rock.Vol))

#A data cleaning step, if there is no bulk density, change zeros to NAs
data[,"Bulk.Density"][data[,"Bulk.Density"] <= 0] <- NA

#Format Water Infiltration
#This function converts minutes to seconds
toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 

#Turn blank water infiltration times to 0
data[,"Water.Infiltration.Time.1"][data[,as.character("Water.Infiltration.Time.1")] == ""] <- "00:00:00"
##Create a column to hold water infiltration times in seconds
data$ISec1<-NULL
##Convert water infiltration times to seconds
data$ISec1<-toSeconds(as.character(data$Water.Infiltration.Time.1))
##Convert water infiltration times to decimal minutes
data$ISec1<-data$ISec1/60

#Collapse data to one measurement per location
#Average Infiltration and Bulk Density within location
data[data == 0]<- NA
filtmean<-aggregate(data$ISec1, by=list(data$SURVEY), mean, na.rm=TRUE)
bulkmean<-aggregate(data$Bulk.Density, by=list(data$SURVEY), mean, na.rm=TRUE)
names(bulkmean)<-c("SURVEY", "Bulk.Density")
names(filtmean)<-c("SURVEY","Infilt1")

#Average Carbon within location
carbon<-subset(data, select=c("Transect.Name","Point.Name","SURVEY","YEAR","Carbon.0.10.cm", "Carbon.10.40.cm","Clay.10.40.cm","Sand.10.40.cm","Silt.10.40.cm"))
names(carbon)<-c("Transect","Point","SURVEY","YEAR","Carbon.0.10.cm", "Carbon.10.40.cm","Clay.10.40.cm","Sand.10.40.cm","Silt.10.40.cm")
carbon<-unique(carbon)

#Merge water infiltration, bulk density, and carbon
newdata<-merge(filtmean, bulkmean, by="SURVEY")
newdata<-merge(newdata, carbon, by="SURVEY")
#write.csv(newdata, file.choose(new=TRUE))

###############################################
#Subsettting to Resample Data
###############################################
#Subset to points that were resampled in 2024
unique(newdata$Point)
point24 <- subset(newdata, newdata$YEAR == "2024")
point24 <- subset(point24, select = c("Point"))
soil24 <- dplyr::left_join(point24, newdata, by = "Point")
soil24 <- subset(soil24, soil24$YEAR != "2022")
soil24 <- subset(soil24, soil24$YEAR != "2023")

#Create a table of average SOC values by year - insert values into LOL
soc010mean<-aggregate(soil24$Carbon.0.10.cm, by=list(soil24$YEAR), mean, na.rm=TRUE)
names(soc010mean)<-c("YEAR","SOC 010")

soc1040mean<-aggregate(soil24$Carbon.10.40.cm, by=list(soil24$YEAR), mean, na.rm=TRUE)
names(soc1040mean)<-c("YEAR","SOC 1040")

socmean <- dplyr::left_join(soc010mean, soc1040mean, by = "YEAR")
#write.csv(socmean, file.choose(new=TRUE))

###############################################
#Add more information to soil24
###############################################
#You can use this soil to feed into other plots besides the texture triangle
soil<-soil24
#Assign standard column headings for soiltexture package
soil$CLAY<-soil$Clay.10.40.cm
soil$SILT<-soil$Silt.10.40.cm
soil$SAND<-soil$Sand.10.40.cm

#Remove NAs from textures or else you will get an error
soil<-subset(soil, soil$CLAY!="NA")
soil<-subset(soil, soil$SILT!="NA")
soil<-subset(soil, soil$SAND!="NA")

#Add a column for texture class (coarse vs. fine)
#Based on this characterization of the soil texture triangle:
#https://cpb-us-e1.wpmucdn.com/blogs.cornell.edu/dist/f/5772/files/2017/12/04_CASH_SH_Series_Texture_Fact_Sheet_072717-286kw9f.pdf
#https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/nrcs142p2_053260.pdf  
soil$Texture<- soiltexture::TT.points.in.classes(
  tri.data    = soil,
  class.sys   = "USDA.TT",
  PiC.type    = "t", text.tol=1)
soil$TextCategory = ifelse(soil$CLAY > 25, "Fine", ifelse(soil$CLAY < 15 & soil$SAND > 80, "Coarse","Coarse"))

#Add bulk density targets
soil$BD_target<-replace(soil$BD_target,soil$TextCategory=="Fine",1.1)
soil$BD_target<-replace(soil$BD_target,soil$TextCategory=="Coarse",1.4)

#Water infiltration versus target of 10 - this is NRCS target, converted for the time 1 measurement
soil$Infilt_target<-3.81
soil$Infilt_dist<-soil$Infilt_target-soil$Infilt1
soil$BD_dist<-soil$BD_target-soil$Bulk.Density

#Add a location field so you can plot just the point number
soil$Location<-stringr::str_sub(soil$Point, -3) #look at characters: some are -2, some -3
#for gis join
#write.csv(soil, file.choose(new=TRUE))

###############################################
#Texture Triangle
###############################################
#Texture Triangle Plot
toka_tex<-subset(soil, soil$YEAR=="2024")

toka_tri = soiltexture::TT.plot(
  class.sys = "USDA.TT",
  tri.data = toka_tex,
  pch = 16,
  css.names = c("CLAY", "SILT", "SAND"),
  main = "Soil texture triangle",
  cex.axis = 0.5,
  cex.lab = 0.75,
  cex.main = 0.75,
  lwd.axis = 0.75,
  text.tol=0.2,
  tri.sum.tst=FALSE,
  col="black",
  cex=0.75,
  frame.bg.col="white",
  grid.show=FALSE
)

soiltexture::TT.points(geo=toka_tri,
                       tri.data = toka_tex,
                       pch=16,
                       tri.sum.tst=FALSE,
                       col="black",
                       cex=0.75,
)

#If you want to include the point labels on the triangle, run this section too
#Feel free to mess around with the various position arguments, I researched and experimented with them for a while
#but could not find descriptions of the arguments for TT.text so it's kindof a shit show and this is the best I could get
#We ended up hand labeling the points in google slides
soiltexture::TT.text(
  tri.data = toka_tex,
  geo = toka_tri,
  labels= toka_tex$Location,
  col="black",
  cex=0.75,
  font=0.4,
  family.op=NULL,
  adj=1.5,
  pos=NULL,
  offset=NULL)

###############################################
#Point Scatterplots with Resample Data
###############################################
#Bulk Density for all sampling periods, color coded by year
#wLine at 1.4 g/cm^3 - coarse NRCS target, change to 1.1 for fine soils
#All TOKA points are fine except 006 and 099
ggplot2::ggplot(soil24, aes(x = Bulk.Density, y = Point, color = factor(YEAR)))+
  geom_point(position = position_dodge(0.1), size=3)+
  ggtitle("Bulk Density Over the Years")+
  labs(x="Bulk Density (g/cm^3)", y="Point")+
  guides(color = guide_legend(title="Year"))+
  theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = 1.1, color="gray",linewidth=0.7)

#SOC 0-10 for all sampling periods, color coded by year
ggplot2::ggplot(soil24, aes(x = Carbon.0.10.cm, y = Point, color = factor(YEAR)))+
  geom_point(position = position_dodge(0.1), size = 3)+
  ggtitle("Shallow SOC (0-10 cm) Over the Years")+
  labs(x="% SOC 0-10 cm", y="Point")+
  guides(color = guide_legend(title="Year"))+
  theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5))

#SOC 10-40 for all sampling periods, color coded by year
ggplot2::ggplot(soil24, aes(x = Carbon.10.40.cm, y = Point, color = factor(YEAR)))+
  geom_point(position = position_dodge(0.1), size=3)+
  ggtitle("Deep SOC (10-40 cm) Over the Years")+
  labs(x="% SOC 10-40 cm", y="Point")+
  guides(color = guide_legend(title="Year"))+
  theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5))

############################################################################
#2024 SOC PLOT: shallow vs deep
############################################################################
#Trying to make a plot of shallow vs deep soc over time - same as Wrangler but modified
#This is very busy with all the years, modified to just 2024 and keep the scatterplots for the resample data
soc24<-subset(soil24, soil24$YEAR == "2024")
ggplot2::ggplot(soc24, aes(x = Carbon.10.40.cm, y = Carbon.0.10.cm)) +
  geom_point(size = 4) +
  theme_bw() +
  xlab("% SOC 10-40 cm") +
  ylab("% SOC 0-10 cm") +
  ggrepel::geom_label_repel(data = soc24,aes(label = Point),color='black', segment.color = 'grey50', box.padding = 0.5, show.legend = FALSE) +
  ggtitle("TomKat Ranch SOC 2024")

############################################################################
#2024 Compaction Plot: bulk density by point vs. target line
#############################################################################
ggplot2::ggplot(toka_tex, aes(x = Point, y = Bulk.Density, colour = factor(TextCategory))) +
  theme_bw()+
  theme(
    panel.grid.major.x = element_line(colour = "black", linetype = "dashed", linewidth = 0.2)) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("steelblue3", "orange3")) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab("Point") +
  ylab("Bulk Density (g/cm\u00b3)") + #found unicode symbol for cubic!
  labs(color = "Soil Texture") + 
  geom_hline(yintercept = 1.4, color="steelblue3", linewidth = 0.4) +
  geom_hline(yintercept = 1.1, color="orange3", linewidth = 0.4) +
  ggtitle("TomKat Ranch Compaction 2024")

