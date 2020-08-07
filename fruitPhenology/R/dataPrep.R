#### Data Prep Code - Emma Bush - 2020.05.14 ####

#This code records the processes
#undertaken to subset data from the Lopé long-term phenology dataset for this
#analysis. The Lopé long-term data used can be found here: Tutin, CEG;
#Abernethy, K; White, L; Dimoto, E; Dikangadissi, JT; Jeffery, KJ; Momont, L;
#Ukizintambara, T; Bush, ER (2029): Lopé Tree Phenology Dataset. Version 1.2.
#University of Stirling. Faculty of Natural Sciences. Dataset.
#http://hdl.handle.net/11667/152

##### Load Libraries
library(plyr)
library(zoo)

#### Load data - these data files can be requested from the University of Stirling's Online Repository for Research Data'
#http://hdl.handle.net/11667/103

Pheno <- read.csv("Lopé Phenology Observations v1.2.csv",skip=3)
Species_lookup <- read.csv("Lopé Species lookup v1.2.csv",skip=2)
Tree_lookup <- read.csv("Lopé Tree Lookup v1.2.csv",skip=2)
Species_dim <- read.csv("Lopé Species Reference Dimensions.csv",skip=2)
Tree_DBH <- read.csv("Lopé Tree DBH v1.2.csv",skip=2)

#### Subset and clean phenology data
Pheno$Date<-as.Date(as.character(Pheno$Date), format="%d/%m/%Y")
Pheno<-merge(Pheno,Tree_lookup[,c("TreeID","SpeciesID",
                                  "Stop.reason","Stop.notes",
                                  "Data.management.notes","Action.needed.",
                                  "Persistently.diseased","Exclude.",
                                  "Notes.for.further.analysis")],"TreeID")
Pheno<-merge(Pheno,Species_lookup[,c("SpeciesID","Species","Family")],"SpeciesID")
Pheno<-Pheno[which(!Pheno$Exclude.entry.==TRUE&
                     !Pheno$No.data.==TRUE&
                     !Pheno$Dead==TRUE&
                     !Pheno$Persistently.diseased=="Yes"&
                     !Pheno$Exclude.=="Yes"&
                     !Pheno$SpeciesID %in% c(25,27,31,32,36,55,56,63,69,82,91,92)),]


Pheno$Important<-ifelse(Pheno$SpeciesID %in% c(3,22,39,41,47,58,61,65,71,75,80,81,85,86),
                        TRUE,FALSE)

Pheno<-Pheno[,c("Row","Family","Species","SpeciesID","TreeID","Date","FL","FRI","FRM","Important")]

length(unique(Pheno$TreeID)) #2007
length(unique(Pheno$SpeciesID)) #73
length(unique(Pheno$SpeciesID[Pheno$Important==TRUE])) #14

write.csv(Pheno,"../created/LopéReproPhenology.csv")


##### Extract relative size data
Tree_sample<-data.frame(TreeID=unique(Pheno$TreeID))
Tree_sample<-merge(Tree_sample,Tree_lookup[,c("TreeID","SpeciesID")],"TreeID",all.x=T)

Tree_DBH<-ddply(Tree_DBH,.(TreeID),summarise,
                Max.Diameter_cm=max(Max.Diameter_cm,na.rm=T))
Tree_DBH<-Tree_DBH[!Tree_DBH$Max.Diameter_cm==-Inf,]

Tree_sample<-merge(Tree_sample,Tree_DBH,"TreeID",all.x=T)
Tree_sample<-merge(Tree_sample,Species_dim[,c("SpeciesID","ref_maxDBH")],"SpeciesID")
Tree_sample$ref_maxDBH<-ifelse(Tree_sample$ref_maxDBH<Tree_sample$Max.Diameter_cm,Tree_sample$Max.Diameter_cm,Tree_sample$ref_maxDBH)
Tree_sample$DBH_relative<-Tree_sample$Max.Diameter_cm/Tree_sample$ref_maxDBH
names(Tree_sample)<-c("SpeciesID","TreeID","DBH_measured_cm","DBH_refMax_cm","DBH_relative")

summary(Tree_sample)
write.csv(Tree_sample,"../created/LopéTreesDBH.csv")

