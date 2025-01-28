### TomKat Ranch Bale Grazing Trial (2021-2024)
### Authors: E. Foster, H. Strohm

# Location: Stage South Pasture
# Main research question: How does bale grazing alter plant species composition and soil health (carbon + fungal:bacteria)
# Data sets: biomass, lpi, soil chemistry (ward labs), soil biology (earth fort)
# Field notes: Soil chem and bio were taken from the same composited sample

library(usethis); use_github()

#set up
data.path<-file.path(getwd(), "data_raw", "trials_raw")

bm1<-read.csv(file.path(data.path, "BGTrial_Biomass_2024-2023.csv"))
sb1<-read.csv(file.path(data.path, "BGTrial_SoilBiology_All.csv"))
sc1<-read.csv(file.path(data.path, "BGTrial_Soil_Chem_All.csv"))

head(bm1)
head(sc1)
head(sb1)

str(sc) #Sample.ID, Dat.Recd
str(sb) #ID, date

sb<-sb1[ ,colSums(!is.na(sb1)) > 0] #remove NA columns
sc<-sc1[ ,colSums(!is.na(sc1)) > 0] #remove NA columns


str(sc)
hist(sc$Organic.Carbon..)
boxplot(sc$Organic.Carbon..~sc$Sample.ID)
