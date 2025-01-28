######################################################################################
## TomKat Birds 2025 ##############################################################
######################################################################################
#revised 1/14/25 from San Mateo 2024 code
# Part ONE: Data formatting and prep
# Part TWO: Subsetting to points that were sampled in all sampling periods
# Part THREE: Focal species figures (Grassland-Shrub and Riparian)
######################################################################################
# Part ONE: Data formatting and prep
######################################################################################
# Run this twice: toka_pc is uplands, hocr_pc is riparian (so exclude toka-specific point code)
rm(list=ls()) #clear everything before starting
setwd("/Users/melpreston/Desktop/R")

require(ggplot2)
require(plyr)
require(dplyr)
require(ggpubr)
require(magrittr)

tkpoints <- read.csv("tokagrspbox_points.csv") #for GRSP analysis only
point24 <- read.csv("point24.csv") #same sample points as 2024 veg and soils
point24 <- subset(point24, select = c("Point"))
names(point24) <- c("POINT")

#data<- read.csv("toka_pc.csv") #entire toka pc with all points and years
data <- read.csv("hocr_pc.csv") #riparian transect, process separately after upland data
data$Transect<-as.factor(data$Transect)
data<- data[-1,]

#date formatting
#data$DATE <- as.Date(as.character(data$Date), "%m/%d/%Y")
data$DATE <- as.Date(as.character(data$Date), "%Y-%m-%d")
data$YEAR<-as.numeric(format(data$DATE, format = "%Y"))
data$SURVEY<-as.factor(paste(data$Point,data$YEAR,"V",data$Visit,sep="-"))
data$PointYear<-as.factor(paste(data$Point,data$YEAR, sep="-"))
data$SAMPLE<-as.factor(paste(data$Point,data$YEAR, sep="-"))

# make a table with number of visits
data$Tally<-1
a<-subset(data,duplicated(data$SURVEY)==FALSE)
visits<-stats::aggregate(a$Tally, list(a$PointYear), sum)
names(visits)<-c("PointYear","Visits")

##################################################
## Subsetting to less than 100m, no flyovers  ###
##################################################
data<-subset(data, subset=data$Protocol=="VCP300")
data<-data[data$Distance.Bin.ID!="B30",]
data<-data[data$Distance.Bin.ID!="FLO",]
data$Distance.Bin.ID<-as.numeric(as.character(data$Distance.Bin.ID))
data<-subset(data, subset=data$Distance.Bin.ID <=100)
data$Distance.Bin.ID<-as.factor(data$Distance.Bin.ID)

#remove Juvenile detections
data<-subset(data, subset=data$Detection.Cue !="J")

# Recode any species codes that may be duplicates
#list updated 1/10/24
data$Spp <- replace(data$Spp, data$Spp == "ORJU", "DEJU")
data$Spp <- replace(data$Spp, data$Spp == "WESJ", "CASJ")
data$Spp <- replace(data$Spp, data$Spp == "AUWA", "YRWA")
data$Spp <- replace(data$Spp, data$Spp == "RSFL", "NOFL")
data$Spp <- replace(data$Spp, data$Spp == "WIWR", "PAWR")
data$Spp <- replace(data$Spp, data$Spp == "PSFL", "WEFL")
data$Spp <- replace(data$Spp, data$Spp == "CAGO", "CANG")

######################################
## aggregate count by Point and Year ##
######################################
species<-stats::aggregate(data$Count,list(data$Spp,data$YEAR, data$Point),sum)
names(species)<-c("SPEC", "YEAR", "POINT","COUNT")
species$PointYear<-as.factor(paste(species$POINT,species$YEAR, sep="-"))
pc<-merge(species, visits, by="PointYear", all=TRUE)

add.zeros<-function(sum.data){
  wide<-stats::reshape(pc, v.names="COUNT", idvar="PointYear",timevar="SPEC", direction="wide")
  first<-wide[,1:4]
  second<-wide[,5:length(wide[1,])]
  second0<-second
  second[]<-lapply(second,function(x) replace(x, is.na(x), 0))
  final<-as.data.frame(cbind(first,second))
  narrow<-stats::reshape(final,idvar="PointYear",varying=list(names(final)[5:length(final[1,])]),direction="long",times=names(final)[5:length(final[1,])],v.names="COUNT",timevar="SPEC")
  narrow$SPEC<-as.factor(substr(narrow$SPEC,7,10))
  row.names(narrow)<-NULL
  return(narrow)
}

newpc<-add.zeros(pc)
newpc$ABUNDANCE<-newpc$COUNT/newpc$Visits
newpc$POINT<-as.factor(newpc$POINT)
newpc$Transect = as.factor(substr(newpc$POINT, 1,4))

## Reformat data in wide format
newpc2 <- subset(newpc, select=c("PointYear", "YEAR", "POINT", "Visits", "SPEC", "ABUNDANCE"))
wide.data <- reshape(newpc2, v.names="ABUNDANCE", idvar="PointYear",timevar="SPEC", direction="wide")

#Get rid of the "ABUNDANCE" at the beginning of the row names
JustSpp<-substr(names(wide.data[,5:ncol(wide.data)]),11,14)
colnames(wide.data)[5:ncol(wide.data)] <- JustSpp

#This step orders species alphabetically
first<-wide.data[,1:4]
second<-wide.data[,5:length(wide.data[1,])]
second<-second[,order(colnames(second))]
wide.data<-as.data.frame(cbind(first,second))

# Calculate Number of species at each point
wide.data$Richness<-rowSums(wide.data[,5:ncol(wide.data)] != 0)

#Subset to transect of interest if needed
wide.data$Transect = as.factor(substr(wide.data$POINT, 1,4))
#write.csv(wide.data, file.choose(new=TRUE))

###############################################################################
# Part TWO: Subsetting to points that were sampled in all sampling periods
###############################################################################
#Point sampling histories
visits$point<-as.factor(substr(visits$PointYear, 1, 8))
visits$year<-as.factor(substr(visits$PointYear,10,13))
levels(visits$year) #check there's no weird spaces it will mess you up later

#For total visits at all points
PointFreqTable = aggregate(visits$Visits, list(visits$point),sum)
names(PointFreqTable) = c("POINT","visits")
#write.csv(PointFreqTable, file.choose(new=TRUE))

#For visit history of 2024 sampling points
names(visits) <- c("pointyear","Visits","POINT", "Year")
visits24 <- merge(point24, visits, by="POINT")
visits_historic <- visits24
visits_historic <- subset(visits_historic, visits_historic$POINT != "TOKA-126")
visits_historic <- subset(visits_historic, visits_historic$POINT != "TOKA-129")
visits_historic <- subset(visits_historic, visits_historic$POINT != "TOKA-137")
visits_historic <- subset(visits_historic, visits_historic$POINT != "TOKA-169")
points_historic <- subset(visits_historic, select = c("POINT"))
points_historic <- dplyr::distinct(points_historic, .keep_all = FALSE)

#Merge with pc data
newpc24 = merge(point24, newpc, by="POINT") #for 2024 only figures
newpcH = merge(points_historic, newpc, by="POINT") #for historical figures
#subset newpcH to complete sampling years 2011, 2014, 2018, 2021, 2024
newpcH <- subset(newpcH, newpcH$YEAR != "2012")
newpcH <- subset(newpcH, newpcH$YEAR != "2013")
newpcH <- subset(newpcH, newpcH$YEAR != "2015")
newpcH <- subset(newpcH, newpcH$YEAR != "2016")
newpcH <- subset(newpcH, newpcH$YEAR != "2017")
newpcH <- subset(newpcH, newpcH$YEAR != "2019")
newpcH <- subset(newpcH, newpcH$YEAR != "2020")

#Use newpcH to make a table of mean abundance
toka_meanabund<-stats::aggregate(newpcH$ABUNDANCE,list(newpcH$SPEC,newpcH$YEAR),mean)
names(toka_meanabund)<-c("SPEC","YEAR","mean_abund")
#write.csv(toka_meanabund, file.choose(new=TRUE))

#HOCR table of mean abundance
hocr_meanabund <- stats::aggregate(newpc2$ABUNDANCE,list(newpc2$SPEC,newpc2$YEAR),mean)
names(hocr_meanabund)<-c("SPEC","YEAR","mean_abund")
#write.csv(hocr_meanabund, file.choose(new=TRUE))

#Richness table for upland data
tokarich <- merge(points_historic, wide.data, by="POINT")
tokarich<-subset(tokarich, select = c("YEAR","POINT","Richness"))
#write.csv(tokarich, file.choose(new=TRUE))

tokarichyear <- dplyr::group_by(tokarich, YEAR)%>%
  summarise(
    count = n(),
    mean = mean(Richness, na.rm = TRUE),
    sd = sd(Richness, na.rm = TRUE)
  )
#write.csv(tokarichyear, file.choose(new=TRUE))

#Richness table for riparian data
hocr_rich <- subset(wide.data, select = c("YEAR","POINT","Richness"))
#write.csv(tokarich, file.choose(new=TRUE))

hocr_richyear <- dplyr::group_by(hocr_rich, YEAR)%>%
  summarise(
    count = n(),
    mean = mean(Richness, na.rm = TRUE),
    sd = sd(Richness, na.rm = TRUE)
  )
#write.csv(hocr_richyear, file.choose(new=TRUE))

###############################################################################
# Part THREE: Focal species figures
###############################################################################
#Grassland-Shrub focals for TomKat uplands (manually chosen)
focal<-read.csv("toka_grassfocals.csv")

#subset to just your focal species
newpc5<- merge(newpcH, focal, by.x= "SPEC", by.y="SPEC")
newpc5$YEAR<-as.factor(newpc5$YEAR)

#focal species abundance per year
ggplot(newpc5,aes(x=Common.Name, y=ABUNDANCE, group=YEAR, fill=YEAR))+
  geom_bar(position="dodge", stat="summary", fun = "mean")+
  scale_x_discrete(limits = c("Grasshopper Sparrow", "Savannah Sparrow", "Song Sparrow", 
                              "White-crowned Sparrow", "California Quail", "Lazuli Bunting",
                              "Spotted Towhee", "Wrentit"))+
  ylab("Abundance")+
  xlab("Species")+
  theme(text=element_text(size=14))+
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3",
                               "mediumpurple4", "aquamarine")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10))+
  labs(title="TomKat Ranch Grassland-Shrub Focal Species")

###############################################################################
#Riparian focals for TomKat HOCR transect
focal<-read.csv("hocr_focals.csv")

#subset to just your focal species
newpc5<- merge(newpc2, focal, by.x= "SPEC", by.y="SPEC")
newpc5$YEAR<-as.factor(newpc5$YEAR)

#focal species abundance per year
ggplot(newpc5,aes(x=Common.Name, y=ABUNDANCE, group=YEAR, fill=YEAR))+
  geom_bar(position="dodge", stat="summary", fun = "mean")+
  #scale_x_discrete(limits = c("Grasshopper Sparrow", "Savannah Sparrow", "Song Sparrow", 
                             #"White-crowned Sparrow", "California Quail", "Lazuli Bunting",
                              #"Spotted Towhee", "Wrentit"))+
  ylab("Abundance")+
  xlab("Species")+
  theme(text=element_text(size=14))+
  scale_fill_manual(values = c("olivedrab", "steelblue3", "darkorange","aquamarine", "mediumpurple4",
                               "forestgreen","cyan2","gold1","mistyrose", "firebrick4",
                               "yellowgreen", "cadetblue1", "darkgoldenrod3", "thistle3", "indianred2")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 10))+
  labs(title="TomKat Ranch Riparian Focal Species")

#GRSP deep dive on different file, San Mateo 2024 Analyses conducrted Fall 2024