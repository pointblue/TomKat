############################################################################
### TomKat Ranch Veg Code 01-2025 #########
############################################################################
#author: M. Preston

#Set working directory, read in data and background files
#CA plant list updated to V2
#Invasives updated specifically for TomKat
rm(list=ls()) #clear everything before starting

lol.path<-file.path(getwd(),"data_raw","lol")

main<-read.csv(file.path(lol.path,"toka_main.csv"))
lpi<-read.csv(file.path(lol.path, "toka_lpi.csv"))
checklist<-read.csv(file.path(lol.path,"toka_check.csv"))
CAPlants<-read.csv(file.path(lol.path,"CAPlantsv3.csv"))
Invasives<-read.csv(file.path(lol.path,"toka_invasives.csv")) #need file
point24 <- read.csv(file.path(lol.path,"point24.csv"))

#load these packages
require(ggplot2); require(plyr); require(dplyr); require(reshape2); require(tidyr)

#subset to 2024 sampling points (soil, veg, birds)
colnames(main)[3] ="Point"
main <- left_join(point24, main, by="Point")
main <- subset(main, main$Date != "2023-07-05")
main <- subset(main, main$Date != "2023-07-06")
main <- subset(main, main$Date != "2022-06-02")
colnames(lpi)[3] ="Point"
lpi <- left_join(point24, lpi, by="Point")
lpi <- subset(lpi, lpi$Date != "2023-07-05")
lpi <- subset(lpi, lpi$Date != "2023-07-06")
lpi <- subset(lpi, lpi$Date != "2022-06-02")
colnames(checklist)[3] ="Point"
colnames(checklist)[4] ="Date"
checklist <- left_join(point24, checklist, by="Point")
checklist <- subset(checklist, checklist$Date != "2023-07-05")
checklist <- subset(checklist, checklist$Date != "2023-07-06")
checklist <- subset(checklist, checklist$Date != "2022-06-02")

#################################################################
####Data cleanup to get ready#################################
#################################################################
#Format dates
#Dates can be different formats so check your CADC download###
#2020- .csvs are downloaded in m/d/y format and open that way in excel, but load into R
#as y-m-d which is correct format for point year. So check before running this?
str(main) #format is YYYY-MM-DD
#main$Date <- as.Date( as.character(main$Date), "%m/%d/%Y")
#lpi$Date <- as.Date( as.character(lpi$Date), "%m/%d/%Y")
#checklist$Event.Date = as.Date(as.character(checklist$Event.Date), "%m/%d/%Y")

#create a pointyear column
main$year = format(as.Date(main$Date), "%Y")
main$pointyear = paste(main$Point, "-", main$year)
lpi$year = format(as.Date(lpi$Date), "%Y")
lpi$pointyear = paste(lpi$Point, "-", lpi$year)
checklist$year = format(as.Date(checklist$Date), "%Y")
checklist$pointyear = paste(checklist$Point, "-", checklist$year)

#remove blank rows
lpi = subset(lpi, subset = Soil.Surface != "")

#################################################################
##		summary of cover tables  	#########
#################################################################
#Aggregate cover of trees and shrubs
covsum<- ddply(checklist, .(Vegetation.Type,pointyear), summarise, Percent.Cover=sum(Percent.Cover), .drop=F)
shrubs<-subset(covsum, subset=covsum$Vegetation.Type == "shrubs")
names(shrubs)<-c("VegType", "pointyear", "Shrub.Cover")
#shrubs<-shrubs[,-1]
trees<-subset(covsum, subset=covsum$Vegetation.Type == "trees")
names(trees)<-c("VegType", "pointyear", "Tree.Cover")
#trees<-trees[,-1]

#Calculate bare ground
lpi$BG<-0
lpi$BG <- replace(lpi$BG, lpi$Canopy1 == "" & lpi$Top.Layer == "NOPLANT" & lpi$Lower1 == "" & 
                    lpi$Lower2 == "" & lpi$Lower3 == "" & lpi$Soil.Surface == "S", 1)

#Aggregate bare ground by point
BareGround<-aggregate(lpi$BG,list(lpi$pointyear),sum)
names(BareGround)<-c("pointyear", "BareGround")
BareGround$Year<-substr(as.character(BareGround$pointyear), 
                        nchar(as.character(BareGround$pointyear))-3, nchar(as.character(BareGround$pointyear)))

#Calculate Litter
#First, rename all of the things that could be litter into just one term
lpi$Litter<-0
lpi$Litter <- replace(lpi$Litter, lpi$Lower1 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower2 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower3 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower4 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower5 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower6 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower7 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower8 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower9 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Lower10 == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Soil.Surface == "L", 1)
lpi$Litter <- replace(lpi$Litter, lpi$Soil.Surface == "EM", 1)

Litter<-aggregate(lpi$Litter,list(lpi$pointyear),sum)
names(Litter)<-c("pointyear", "Litter")

#join bare ground and litter into one table
#you'll use this for your plots later
bgl<-Litter %>% dplyr::inner_join(BareGround, by=c('pointyear'))
bgl$point = as.factor(substr(bgl$pointyear, 1,8))

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
Exclude<-c("", "AM", "ASTERA", "APIACE", "BORAGI", "FABACE", "2FA", "2FERN", "2FORB", "2FP", "2GA", "2GP", "2LICHN", 
           "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "S", "M", "R", "WL", "NA")
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
#write.csv(richness2, file.choose(new=TRUE))

#Now bring everything together
data.summary<-merge(richness2, Litter, by="pointyear")
data.summary<-merge(data.summary, BareGround,by="pointyear")
data.summary<-merge(data.summary, trees,by="pointyear", all.x=TRUE)
data.summary<-merge(data.summary, shrubs,by="pointyear", all.x=TRUE)

# IF you have transect where Not every point has all 100 subsamples, 
# Then run this to correct for reduced effort
lpi$Tally<-1
a<-aggregate(lpi$Tally, list(lpi$pointyear), sum)
names(a)<-c("pointyear", "NumIndices")
data.summary<-merge(data.summary, a, by="pointyear")
data.summary$Litter<-(data.summary$Litter/data.summary$NumIndices)*100
data.summary$BareGround<-(data.summary$BareGround/data.summary$NumIndices)*100
#write.csv(data.summary, file.choose(new=TRUE))

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
SppList2<-subset(SppList, select=c("Scientific.Name", "Common.Name", "Family", "Provenance", "FunGrp"))
#write.csv(SppList2, file.choose(new=TRUE))

###################################################################
######### lpi.trim to newlpi: more data manipulation ##### 
####################################################################
a<-aggregate(lpi$Tally, list(lpi$pointyear), sum)
names(a)<-c("pointyear", "NumIndices")

#limit fields in lpi to pointyear and all of the layers
lpi.trim<-subset(lpi, select=c("pointyear", "Canopy1", "Canopy2","Canopy3","Top.Layer","Lower1","Lower2", "Lower3", "Lower4","Lower5","Lower6","Lower7", "Lower8","Lower9","Lower10","Soil.Surface"))

#reshape so you have a point field and then all of the plants
lpi.trimlong<-reshape2::melt(lpi.trim, id="pointyear")
names(lpi.trimlong)<-c("pointyear", "Layer", "Spp")
lpi.trimlong<-subset(lpi.trimlong, select=c("pointyear", "Spp"))
lpi.trimlong$Tally<-1

#remove all the non-plant rows of data from lpi.trimlong
lpi.trimlong$Spp<-as.factor(lpi.trimlong$Spp)
Exclude<-c("", "AM", "ASTERA", "APIACE", "BORAGI", "FABACE", "2FA", "2FERN", "2FORB", "2FP", "2GA", "2GP", "2LICHN", #2GP unknown perennial grass, 2FP unkown perennial fob
           "2LTR", "2LTRWS",  "2PLANT",  "2W", "NOPLANT", "L", "S", "M", "R", "WL", "NA")
lpi.trimlong<- lpi.trimlong[!(lpi.trimlong$Spp %in% Exclude),]
lpi.trimlong<-droplevels(lpi.trimlong) 
lpi.trimlong<-lpi.trimlong %>% tidyr::drop_na(Spp)
levels(lpi.trimlong$Spp)

#newlpi used for absolute cover
newlpi<-reshape2::dcast(lpi.trimlong, pointyear~Spp,value.var=c("Tally"), sum)
newlpi<-merge(newlpi, a, by="pointyear")
pointyear<-newlpi$pointyear

#Almost there, just some tidying
drops<-c("","Var.2", "2FA", "2FORB", "2FP", "2FS", "2GA", "2GP", "2LICHN", "2LTR", "2LTRWS", "2MOSS", "2PLANT", 
         "2W", "NA", "NumIndices", "NOPLANT", "UNKNWN", "M", "L", "EM", "AM", "R", "WL", "S")
newlpi <- newlpi[,!(names(newlpi) %in% drops)]

#########################################################################################################
########## Get a species list by abundance (# hits) per YEAR only (no points) ###########################
#########################################################################################################
#create a year column to group by
topspp<-newlpi
topspp$year = strsplit(topspp$pointyear, " - ")
topspp$year = as.numeric(sapply(strsplit(topspp$pointyear,"-"), `[`, 3))
topspp$year = as.factor(topspp$year) #make a year a factor again
topspp$Transect = as.factor(substr(topspp$pointyear, 1,4))

#remove pointyear
topspp<-topspp[,-1]

#group by year then summarise across all columns to sum of hits per species per year
#HAVE TO CHOOSE YOUR COLUMNS MANUALLY
topsppsum <- topspp %>%
  group_by(year)%>%
  summarise(across(1:180, sum)) #EF?

#transpose data for ease of sorting spp
#cannot figure out how to make first column come back (i.e. species)
#sort and modify this list in excel
topsum <- as.data.frame(t(topsppsum), stringsAsFactors = FALSE)
names(topsum)<-c("2016","2018","2021","2024")
topsum <- topsum[-1,]
#write.csv(topsum, file.choose(new=TRUE))

###################################################################
######### Calculating cover by FunGrp WITH Invasives ##### 
###################################################################
# replace list in lpi.trimlong with functional groups including invasives
longlpi2<-lpi.trimlong
longlpi2$FunGrp<-NULL
longlpi2$FunGrp<-CAPlantsI$FunGrp[match(lpi.trimlong$Spp,CAPlantsI$Accepted.Symbol)]
longlpi2$FunGrp<-as.factor(longlpi2$FunGrp)
longlpi2$Provenance<-SppList$Provenance[match(lpi.trimlong$Spp,SppList$Spp)]

#double check your FunGrps
levels(longlpi2$FunGrp)

#table of absolute cover by functional group incl. invasives
Fun.SumI<-aggregate(longlpi2$Tally, list(longlpi2$pointyear, longlpi2$FunGrp), sum)
names(Fun.SumI)<-c("pointyear", "FunGrp", "Count")
Fun.SumI2<-reshape2::dcast(Fun.SumI, pointyear~FunGrp,value.var=c("Count"), sum)
Fun.SumI2<-merge(Fun.SumI2, a, by="pointyear")
pointyear<-Fun.SumI2$pointyear
Fun.SumI2<-(Fun.SumI2[,2:ncol(Fun.SumI2)]/Fun.SumI2$NumIndices) *100
Fun.SumI2<-cbind(pointyear, Fun.SumI2)

#summarize change in perennial grasses #EF
Fun.SumI2$year<-substr(Fun.SumI2$pointyear,12,15) #extract year
Fun.SumI2$point<-substr(Fun.SumI2$pointyear,1,8) #extract point
Fun.SumI3 <- 
  Fun.SumI2 %>%
  group_by(year) %>%
  summarise(Average_Perennial_Grass = mean(`Perennial Grass`, na.rm = TRUE))

Fun.SumI3 <- 
  Fun.SumI2 %>%
  group_by(point) %>%
  summarise(Average_Perennial_Grass = mean(`Perennial Grass`, na.rm = TRUE))

#calcluate differences
pg.diff.24.21 <- Fun.SumI2 %>%
  filter(year %in% c(2024, 2021)) %>%  # Filter rows for years 2024 and 2021
  group_by(point) %>%                  # Group by 'point'
  summarise(
    PG_Difference = `Perennial Grass`[year == 2024] - `Perennial Grass`[year == 2021],
    .groups = "drop"                   # Drop the grouping structure
  )

as.data.frame(Fun.SumI2)


################################################################################
######### get your cover tables ready to plot (with and without invasives) ##### 
################################################################################
#remove extra columns so you can melt
funplotI2<-Fun.SumI2[,-11]

#melt that!
funplotI2<-reshape2::melt(funplotI2, id="pointyear")
names(funplotI2)<-c("pointyear", "FunGrp", "Cover")

#add year and transect cols for plotting
funplotI2$year = strsplit(funplotI2$pointyear, " - ")
funplotI2$year = as.numeric(sapply(strsplit(funplotI2$pointyear,"-"), `[`, 3))
funplotI2$year = as.factor(funplotI2$year) # add year back and make it a factor
funplotI2$transect = as.factor(substr(funplotI2$pointyear, 1,4))
funplotI2$point = as.factor(substr(funplotI2$pointyear, 1,8))
#FINALLY ready to plot!

################################################################################
## Plots: Bare Ground and Litter by point and year
################################################################################
bgl$Transect = as.factor(substr(bgl$pointyear, 1,4))

bgl_year <- dplyr::group_by(bgl, Year)%>%
  summarise(
    count = n(),
    mean = mean(BareGround, na.rm = TRUE),
    sd = sd(BareGround, na.rm = TRUE)
  )
#write.csv(bgl_year, file.choose(new=TRUE))

#Bare ground plot
ggplot2::ggplot(bgl, aes(x = Year, y = BareGround, color = Year))+
  geom_col(aes(fill=Year), position = "identity", colour = NA)+
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3", "mediumpurple4")) +
  facet_wrap(~point, ncol = 5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="% bare ground")

#Litter plot
ggplot2::ggplot(bgl, aes(x = Year, y = Litter, color = Year))+
  geom_col(aes(fill=Year), position = "identity", colour = NA)+
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3", "mediumpurple4")) +
  facet_wrap(~point, ncol = 5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="% litter")

################################################################################
## Plots: Functional Group by Point and Year
################################################################################
## Functional group cover by point and year
ggplot2::ggplot(funplotI2, aes(x=year, y=Cover))+
  geom_col(aes(fill = FunGrp), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3","mediumpurple4",
                               "aquamarine","firebrick4", "indianred2", "gold1", "yellowgreen")) +
  facet_wrap(~point, ncol = 5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="cover")

################################################################################
## Plots: Functional Group by Ranch and Year
################################################################################
#aggregate point data by year and functional group
fungroup<-aggregate(Cover~year+FunGrp, data = funplotI2, mean) #if you loaded transects separately
fungroup$se<-aggregate(Cover~year+FunGrp, data = funplotI2, sd)$Cover/sqrt(aggregate(Cover~year+FunGrp, data = funplotI2, length)$Cover)

g<- ggplot(fungroup, aes(x=year, y=Cover))
g+
  geom_col(aes(fill = FunGrp), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3","mediumpurple4",
                               "aquamarine","firebrick4", "indianred2", "gold1", "yellowgreen")) +
  theme_bw()+
  guides(fill=guide_legend(title="Functional Group"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="cover")

g+
  geom_point(aes(color = FunGrp)) + geom_line(aes(linetype = "dotted", group=FunGrp, color=FunGrp))+
  geom_errorbar(aes(x))+
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3","mediumpurple4",
                               "aquamarine","firebrick4", "indianred2", "gold1", "yellowgreen")) +
  theme_bw()+
  guides(fill=guide_legend(title="Functional Group"))+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="cover (%)")

################################################################################
## Plots: Species Richness by Point and Year, color coded by fun grp
#################################################################################
#table of plant species richness by year 
toka_richyear <- richness2
toka_richyear$point = as.factor(substr(toka_richyear$pointyear, 1,8))
toka_richyear$year = as.factor(substr(toka_richyear$pointyear, 11,15))
toka_richyear <- aggregate(toka_richyear$NumSpp, by=list(toka_richyear$year),mean)
names(toka_richyear) = c("year","plantrich")
#write.csv(toka_richyear, file.choose(new=TRUE))

#richness4 gives species richness by FunGrp for each point with total richness
richness3<-aggregate(richness$Spp, by=list(richness$FunGrp, richness$pointyear), FUN=length)
names(richness3)<-c("FunGrp", "pointyear", "FunSpp")
richness4 = inner_join(richness3, richness2, by ="pointyear")
richness4$point = as.factor(substr(richness3$pointyear, 1,8))
richness4$year = as.factor(substr(richness3$pointyear, 11,15))
richness4$Transect = as.factor(substr(richness4$pointyear, 1,4))

## Species richness by point and year
ggplot2::ggplot(richness4, aes(x=year, y=FunSpp))+
  geom_col(aes(fill = FunGrp), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3","mediumpurple4",
                               "aquamarine","firebrick4", "indianred2", "gold1", "yellowgreen")) +
  #scale_fill_brewer(palette="Dark2")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~point, ncol = 5) +
  theme_bw()+
  labs(x="year", y="species richness")

################################################################################
## Plots: Species Richness by Ranch and Year, color coded by fun grp
#################################################################################
#plot
g<- ggplot(richness4, aes(x=year, y=FunSpp))
g+
  geom_bar(aes(fill = FunGrp), position = position_stack(reverse = TRUE), stat = "summary", fun = "mean") +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("olivedrab", "steelblue3", "orange3", "aquamarine",
                               "mediumpurple4", "firebrick4", "tan", "mistyrose", "yellowgreen")) +
  theme_bw()+
  guides(fill=guide_legend(title="Functional Group"))+
  labs(x="year", y="species richness")

dplyr::group_by(richness4, year)%>%
  summarise(
    count = n(),
    mean = mean(NumSpp, na.rm = TRUE),
    sd = sd(NumSpp, na.rm = TRUE)
  )

################################################################################
## Plots: Perennial Grass
################################################################################
#table of perennial grass cover only by species
toka_pg <- subset(longlpi2, longlpi2$FunGrp == "Perennial Grass")
toka_pg$year = as.numeric(sapply(strsplit(toka_pg$pointyear,"-"), `[`, 3))
toka_pg$year = as.factor(toka_pg$year) # add year back and make it a factor
toka_pg$Point = as.factor(substr(toka_pg$pointyear, 1,8))
toka_pg<-aggregate(toka_pg$Tally, list(toka_pg$pointyear, toka_pg$Spp, toka_pg$Point, toka_pg$year), sum)
names(toka_pg)<-c("pointyear", "Spp", "Point", "year", "Count")
#remove or replace very small counts so they don't complicate the figure
toka_pg$Spp <- replace(toka_pg$Spp, toka_pg$Spp == "NALE2", "NAPU4")
toka_pg <- subset(toka_pg, toka_pg$Spp != "AGROS2")
toka_pg <- subset(toka_pg, toka_pg$Spp != "POPR")
toka_pg <- subset(toka_pg, toka_pg$Spp != "MELIC")
#bcra5 CA Brome #EF
#nale2 foothill needlegrass #EF

#join with the species list so we can put the common name in the legend
toka_pg <- left_join(toka_pg, SppList, by="Spp")
toka_pg$Common.Name <- replace(toka_pg$Common.Name, toka_pg$Spp == "PHAQ", "harding grass")

#Perennial grass cover by point, species, year
ggplot2::ggplot(toka_pg, aes(x=year, y=Count))+
  geom_col(aes(fill = Common.Name), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("steelblue3", "yellowgreen", "firebrick4", "indianred2", 
                               "gold1","forestgreen", "mediumpurple4", "aquamarine", "gray")) +
  facet_wrap(~Point, ncol = 5) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="cover")


#Perennial grass cover by species, year
ggplot2::ggplot(toka_pg, aes(x=year, y=Count))+
  geom_col(aes(fill = Common.Name), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("steelblue3", "yellowgreen", "firebrick4", "indianred2", 
                               "gold1","forestgreen", "mediumpurple4", "aquamarine", "gray")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="year", y="cover")


#########################################################################
# Danthonia specific
tokadan<-subset(topspp, select=c("DACA3","year","Transect"))
#write.csv(tokadan, file.choose(new=TRUE))

tokadan_year <- dplyr::group_by(tokadan, year)%>%
  summarise(
    count = n(),
    mean = mean(DACA3, na.rm = TRUE),
    sd = sd(DACA3, na.rm = TRUE)
  )
#write.csv(tokadan_year, file.choose(new=TRUE))

#ranch-wide DACA3 boxplot
ggplot2::ggplot(tokadan, aes(x=year, y=DACA3)) + 
  geom_boxplot()+
  theme(legend.position = "top") + 
  theme(legend.text = element_text(size=8)) +
  theme(legend.title = element_blank()) +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 1)) +
  labs(x="year", y="% Cover California oatgrass (Danthonia californica)")

