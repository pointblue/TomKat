############################################################################
### TomKat Plant Species List 2024 #########
############################################################################
#Set working directory, read in data and background files
#CA plant list updated to V3
rm(list=ls()) #clear everything before starting
setwd("/Users/melpreston/Desktop/R")

main<-read.csv("toka_visit.csv")
lpi<-read.csv("toka_lpi.csv")
checklist<-read.csv("toka_check.csv")
CAPlants<-read.csv("CAPlantsv3.csv")
Invasives<-read.csv("Invasivesv1.csv")

#load these packages
require(reshape2)

#################################################################
####Data cleanup to get ready#################################
#################################################################
#Format dates
#Dates can be different formats so check your CADC download###
#2020- .csvs are downloaded in m/d/y format and open that way in excel, but load into R
#as y-m-d which is correct format for point year. So check before running this?
#main$Date <- as.Date( as.character(main$Date), "%m/%d/%Y")
#lpi$Date <- as.Date( as.character(lpi$Date), "%m/%d/%Y")
#checklist$Event.Date = as.Date(as.character(checklist$Event.Date), "%m/%d/%Y")

#create a pointyear column
main$year = format(as.Date(main$Date), "%Y")
main$pointyear = paste(main$PointId, "-", main$year)
lpi$year = format(as.Date(lpi$Date), "%Y")
lpi$pointyear = paste(lpi$Point.Id, "-", lpi$year)
checklist$year = format(as.Date(checklist$Event.Date), "%Y")
checklist$pointyear = paste(checklist$Point.Id, "-", checklist$year)

#remove blank rows
lpi = subset(lpi, subset = Soil.Surface != "")

##########################################################
###########		Species Richness	##########################
##########################################################
lpi$Point.Dir<-paste(lpi$pointyear, lpi$Direction, sep="-")
layers<-subset(lpi, select=c("pointyear", "Top.Layer", "Lower1", "Lower2", "Lower3", "Lower4", "Lower5", "Lower6", "Lower7"))

longlpi<-reshape2::melt(layers, id="pointyear") #error msg:attributes not identical across measure
names(longlpi)<-c("pointyear", "Layer", "Spp")
checklist$Layer<-"extras"
extras<-subset(checklist, select=c("pointyear", "Layer", "USDA.Code"))
names(extras)<-c("pointyear", "Layer", "Spp")

both<-rbind(longlpi, extras)
both$Spp<-as.factor(both$Spp)

#remove the non-spp
Exclude<-c("", "AM", "ASTERA", "APIACE", "BORAGI", "FABACE", "2FA", "2FERN", "2FORB", "2GRAM", "2FP", "2GA", "2GP", "2LICHN", 
           "2LTR", "2LTRWS",  "2PLANT", "2S", "2W", "NOPLANT", "L", "S", "M", "R", "WL", "NA")
both <- both[!(both$Spp %in% Exclude),]
both<-droplevels(both)

#add plants that are in the soil surface hit
richness<-NULL
both1 = both
both1$Layer = NULL
both1 = both1[!duplicated(both1),]
richness = both1
for (i in levels(both$pointyear)) {
  Point<-both[both$pointyear==i,]
  Point<-subset(Point, !duplicated(Point$Spp))
  if (is.null(richness)) {richness<-Point} else {richness<-rbind(richness,Point)}}

richness<-merge(richness, CAPlants, by.x = "Spp", by.y = "Accepted.Symbol", all.x = TRUE, all.y = FALSE)
richness2<-aggregate(richness$Spp, list(richness$pointyear), length)
names(richness2)<-c("pointyear", "NumSpp")
#write.csv(richness, file.choose(new=TRUE))
#write.csv(richness2, file.choose(new=TRUE))

################################################################				
######## Make the Species List	 #######
################################################################
SppList<-subset(richness, select=c("Spp", "Symbol", "Scientific.Name", "Common.Name","Genus","Family","Family.Common.Name",
                                   "Duration","Growth.Habit", "Native.Status" ,"Invasive", "FunGrp"))
SppList<-subset(SppList, !duplicated(SppList$Spp))
#write.csv(SppList, file.choose(new = TRUE))

#make a list of invasives
CAPlantsI<-CAPlants
levels(CAPlantsI$FunGrp) <- c(levels(CAPlantsI$FunGrp), "Invasives")
CAPlantsI$FunGrp[CAPlantsI$Accepted.Symbol %in% Invasives$USDA.code]<-"Invasives"

#make a native/invasive column in SppList
SppList$Native.Status<-substr(SppList$Native.Status, 1, 7)
SppList$Provenance<-NULL
SppList$Provenance <- replace(SppList$Provenance, SppList$Native.Status == "L48 (N)", "Native")
SppList$Provenance <- replace(SppList$Provenance, SppList$Native.Status == "L48 (I)", "Non-native")
#SppList2<-subset(SppList, select=c("Scientific.Name", "Common.Name", "Family", "Provenance", "FunGrp"))
#write.csv(SppList, file.choose(new=TRUE))
