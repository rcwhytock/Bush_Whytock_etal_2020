#### Fruit Phenology Analysis - Emma Bush - 2020.05.14 ####

##### Libraries ####
library(plyr)
library(zoo)
library(lme4)
library(ggplot2)
library(lubridate)
library(cowplot)
library(broom)

##### Load and format data ####
Pheno_all<-read.csv("LopéReproPhenology.csv", skip=3)
allTreesDBH<-read.csv("LopéTreesDBH.csv", skip=2)

Pheno_all$Date<-as.Date(Pheno_all$Date)
Pheno_all$Year<-year(Pheno_all$Date)
Pheno_all$Month<-month(Pheno_all$Date)
Pheno_all$Year_rescaled<-scale(Pheno_all$Year)

Pheno_all$FL_binom<-ifelse(Pheno_all$FL>0,1,0) #Flowers
Pheno_all$FRI_binom<-ifelse(Pheno_all$FRI>0,1,0) #Unripe fruit
Pheno_all$FRM_binom<-ifelse(Pheno_all$FRM>0,1,0) #Ripe fruit
Pheno_all<-Pheno_all[complete.cases(Pheno_all),]
summary(Pheno_all)

Pheno_all$Genus<-ldply(as.character(Pheno_all$Species),.fun=function(x) Genus=strsplit(x,split=" ")[[1]][1])$V1
Pheno_all$SpeciesOnly<-ldply(as.character(Pheno_all$Species),.fun=function(x) strsplit(x,split=" ")[[1]][2])$V1
Pheno_all$SpeciesShort<-paste(substr(Pheno_all$Genus,1,1),". ",Pheno_all$SpeciesOnly,sep="")
Pheno_all$Type<-ifelse(Pheno_all$Important==TRUE,"Important ele fruit","Other")


length(unique(Pheno_all$TreeID)) #2007 individuals
length(unique(Pheno_all$SpeciesID)) #73 species
length(unique(Pheno_all$Family)) #27 families

#functions
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#### Sample description ####

Year_summary<-ddply(Pheno_all,.(Year,Important),summarise,
                    treeSample=length(unique(TreeID)),
                    speciesSample=length(unique(SpeciesID)))
Year_summary$Type<-ifelse(Year_summary$Important=="TRUE","Important elephant fruit species",
                           "Other species")

SI_treeSamplePlot<-ggplot(Year_summary,aes(x=Year,y=treeSample,fill=Type))+
  geom_col(position="dodge",colour="black",size=0.1)+
  ylab("Number of Trees")+
  theme_classic()+
  theme(legend.posit=c(0.2,0.9),legend.title = element_blank())

SI_speciesSamplePlot<-ggplot(Year_summary,aes(x=Year,y=speciesSample,fill=Type))+
  geom_col(position="dodge",colour="black",size=0.1)+
  ylab("Number of Species")+
  theme_classic()+
  theme(legend.posit=c(0.2,0.9),legend.title = element_blank())

Tree_summary<-ddply(Pheno_all,.(Type,Family,SpeciesID,Species,TreeID),summarise,
                    Date_min=min(Date,na.rm=T),
                    Date_max=max(Date,na.rm=T),
                    Period_days=difftime(Date_max,Date_min,units= "days"),
                    Period_months=elapsed_months(Date_max,Date_min)+1)

Species_summary<-ddply(Tree_summary,.(Species,Family,Type),summarise,
                       Sample=length(TreeID),
                       Start=as.yearmon(min(Date_min)),
                       End=as.yearmon(max(Date_max)),
                       Period_months_min=min(Period_months),
                       Period_months_max=max(Period_months),
                       Period_months_mean=round(mean(Period_months),digits=0))

Species_summary$Obs<-paste(Species_summary$Period_months_mean," (",Species_summary$Period_months_min,"-",Species_summary$Period_months_max,")",sep="")
Species_summary$ImportantElephantFruit<-ifelse(Species_summary$Type %in% c("Important ele fruit"), "**", "")

head(Species_summary)
write.csv(Species_summary[,c(1,2,4,5,6,10,11)],"../results/SI_speciesDescription.csv")

Type_summary<-ddply(rbind(data.frame(Tree_summary,Set="All"),
                          data.frame(Tree_summary[Tree_summary$Type %in% c("Important ele fruit"),],Set="Important elephant fruit")),
                    .(Set),summarise,
                    SpeciesSample=length(unique(Species)),
                    TreeSample=length(TreeID),
                       Period_months_mean=round(mean(Period_months),digits=0),
                    Period_months_min=round(min(Period_months),digits=0),
                    Period_months_max=round(max(Period_months),digits=0))

Type_summary$SpeciesTreeSample_mean<-round(c(mean(Species_summary$Sample),
                                       mean(Species_summary$Sample[Species_summary$Type %in% c("Important ele fruit")])),
                                       digits=0)

Type_summary$SpeciesTreeSample_min<-round(c(min(Species_summary$Sample),
        min(Species_summary$Sample[Species_summary$Type %in% c("Important ele fruit")])),
      digits=0)

Type_summary$SpeciesTreeSample_max<-round(c(max(Species_summary$Sample),
                                            max(Species_summary$Sample[Species_summary$Type %in% c("Important ele fruit")])),
                                          digits=0)

write.csv(Type_summary,"../results/SI_sampleDescription.csv")


#### FAS ####
Tree_summary<-ddply(Pheno_all,.(Type,Family,SpeciesID,Species,TreeID),summarise,
                    Date_min=min(Date,na.rm=T),
                    Date_max=max(Date,na.rm=T),
                    Period_days=difftime(Date_max,Date_min,units= "days"),
                    Period_months=elapsed_months(Date_max,Date_min)+1)

Species_year_summary<-ddply(Pheno_all[Pheno_all$Year>1986&Pheno_all$Year<2019,],.(Species,Type,Year),summarise,
                       Sample=length(unique(TreeID)),
                       Sample5=ifelse(Sample>=5,1,0))

Species_FAS_summary<-ddply(Species_year_summary,.(Species,Type),summarise,
                           Consistent=ifelse(sum(Sample5)==32,TRUE,FALSE)) # 40 species measured consistently >5 individuals 1987 - 2018

FAS_spp_ym<-ddply(Pheno_all[which(Pheno_all$Year>1986 & Pheno_all$Year < 2019 &
                                    Pheno_all$Species %in% Species_FAS_summary$Species[which(Species_FAS_summary$Consistent==TRUE)] ),],
              .(Important,Species,Year,Month),summarise,
              p=mean(FRM_binom,na.rm=T),
              c=ifelse(p>0,mean(FRM[which(FRM_binom==1)],na.rm=T)/4,0),
              FAS_spp_ym=p*c
              )

FAS_spp_y<-ddply(FAS_spp_ym,.(Important,Species,Year),summarise,
             FAS_spp_sum_y=sum(FAS_spp_ym,na.rm=T),
             Sample=length(unique(Month)),
             FAS_spp_prop_y=sum(FAS_spp_sum_y)/Sample)

FAS_y<-ddply(FAS_spp_y,.(Year),summarise,
             Sample=length(unique(Species)),
             FAS_prop_y=sum(FAS_spp_prop_y)/Sample)

importantFAS_y<-ddply(FAS_spp_y[which(FAS_spp_y$Important==TRUE),],.(Year),summarise,
                      Sample=length(unique(Species)),
                      FAS_prop_y=sum(FAS_spp_prop_y)/Sample)

interannualFASPlot<-ggplot()+
  geom_line(data=FAS_y,aes(x=Year,y=FAS_prop_y,colour="All fruit (40 spp.)"),linetype="solid",size=0.5)+
  geom_line(data=importantFAS_y,aes(x=Year,y=FAS_prop_y,colour="Important elephant fruit (7 spp.)"),linetype="solid",size=0.5)+
  theme_classic()+
  scale_colour_manual(values=c("cadetblue3","coral2"))+
  ylab("Ripe Fruit Availability")+
  xlab("Year")+
  theme(legend.title=element_blank(),legend.position=c(0.7,0.9))


#### All species models - linear trend in flower and fruit encounter ####

# Flower encounter over time binomial glmm
FL_binom_m1<-glmer(FL_binom~Year_rescaled+
                      (Year_rescaled|Species)+
                      (Year_rescaled|TreeID)+
                      (1|Month),
                   data=Pheno_all,
                   family=binomial)
summary(FL_binom_m1) #260431 - 2007 trees 73 spp, 12 months

# Null model flowers binomial glmm
FL_binom_m2<-glmer(FL_binom~1+
                     (1|Species)+
                     (1|TreeID)+
                     (1|Month),
                   data=Pheno_all,
                   family=binomial)

# Unripe fruit encounter over time binomial glmm
FRI_binom_m1<-glmer(FRI_binom~Year_rescaled+
                      (Year_rescaled|Species)+
                      (Year_rescaled|TreeID)+
                      (1|Month),
                    data=Pheno_all,
                    family=binomial)

#Convergence warnings - https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
tt <- getME(FRI_binom_m1,"theta")
ll <- getME(FRI_binom_m1,"lower")
min(tt[ll==0]) #not close to zero
ss <- getME(FRI_binom_m1,c("theta","fixef"))
FRI_binom_m1_v2 <- update(FRI_binom_m1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

# Null model unripe fruit binomial glmm
FRI_binom_m2<-glmer(FRI_binom~1+
                     (1|Species)+
                     (1|TreeID)+
                     (1|Month),
                   data=Pheno_all,
                   family=binomial)

# Ripe fruit encounter over time binomial glmm
FRM_binom_m1<-glmer(FRM_binom~Year_rescaled+
                      (Year_rescaled|Species)+
                      (Year_rescaled|TreeID)+
                      (1|Month),
                   data=Pheno_all,
                   family=binomial)

# Null model for ripe fruit encounter binomial glmm
FRM_binom_m2<-glmer(FRM_binom~1+
                      (1|Species)+
                      (1|TreeID)+
                      (1|Month),
                   data=Pheno_all,
                   family=binomial)

# Important ele fruit species only

# Ripe fruit encounter over time binomial glmm
FRM_ele_important_binom_m1<-glmer(FRM_binom~Year_rescaled+
                          (Year_rescaled|Species)+
                          (Year_rescaled|TreeID)+
                          (1|Month),
                        data=Pheno_all[which(Pheno_all$Important==TRUE),],
                        family=binomial)

FRM_ele_important_binom_mNull<-glmer(FRM_binom~1+
                          (1|Species)+
                          (1|TreeID)+
                          (1|Month),
                        data=Pheno_all[which(Pheno_all$Important==TRUE),],
                        family=binomial)

summary(FRM_ele_important_binom_m1) # 58319 obs - 466 trees - 14 species

# Important elephant fruit with Month as fixed effect interacting with year
FRM_ele_important_binom_m2<-glmer(FRM_binom~Year_rescaled*factor(Month)+
                          (Year_rescaled|Species)+
                          (Year_rescaled|TreeID),
                        data=Pheno_all[which(Pheno_all$Important==TRUE),],
                        family=binomial)

#Convergence warnings - https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
tt <- getME(FRM_ele_important_binom_m2,"theta")
ll <- getME(FRM_ele_important_binom_m2,"lower")
min(tt[ll==0]) #not close to zero
ss <- getME(FRM_ele_important_binom_m2,c("theta","fixef"))
FRM_ele_important_binom_m2_v2 <- update(FRM_ele_important_binom_m2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

FRM_ele_important_binom_m3<-glmer(FRM_binom~Year_rescaled+factor(Month)+
                                    (Year_rescaled|Species)+
                                    (Year_rescaled|TreeID),
                                  data=Pheno_all[which(Pheno_all$Important==TRUE),],
                                  family=binomial)

#Convergence warnings - https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
tt <- getME(FRM_ele_important_binom_m3,"theta")
ll <- getME(FRM_ele_important_binom_m3,"lower")
min(tt[ll==0]) #not close to zero
ss <- getME(FRM_ele_important_binom_m3,c("theta","fixef"))
FRM_ele_important_binom_m3_v2 <- update(FRM_ele_important_binom_m3,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


#### Save model outputs to csv ####

summary(FRM_ele_important_binom_mNull)
reproAIC<-data.frame(Dataset=c("All species","All species",
                               "All species","All species",
                               "All species","All species",
                               "Important ele. fruit","Important ele. fruit",
                               "Important ele. fruit","Important ele. fruit"),
                     Species=c(73,73,73,73,73,73,14,14,14,14),
                     Observations=c(260431,260431,
                                    260431,260431,
                                    260431,260431,
                                    58319,58319,
                                    58319,58319),
                     Response=c("Flowers", "Flowers", 
                                "Unripe fruit", "Unripe fruit",
                                "Ripe fruit", "Ripe fruit",
                                "Ripe fruit", "Ripe fruit",
                                "Ripe fruit", "Ripe fruit"),
                     Predictors=c("Year","Intercept only",
                                  "Year","Intercept only",
                                  "Year","Intercept only",
                                  "Year","Intercept only",
                                  "Year * Month", "Year + Month"),
                     AIC=c(AIC(FL_binom_m1,FL_binom_m2)$AIC,
                           AIC(FRI_binom_m1_v2,FRI_binom_m2)$AIC,
                           AIC(FRM_binom_m1,FRM_binom_m2)$AIC,
                           AIC(FRM_ele_important_binom_m1,FRM_ele_important_binom_mNull)$AIC,
                           AIC(FRM_ele_important_binom_m2_v2,FRM_ele_important_binom_m3_v2)$AIC),
                     DF=c(AIC(FL_binom_m1,FL_binom_m2)$df,
                          AIC(FRI_binom_m1_v2,FRI_binom_m2)$df,
                          AIC(FRM_binom_m1,FRM_binom_m2)$df,
                          AIC(FRM_ele_important_binom_m1,FRM_ele_important_binom_mNull)$df,
                          AIC(FRM_ele_important_binom_m2_v2,FRM_ele_important_binom_m3_v2)$df))

write.csv(reproAIC, "../results/SI_AIC_reproModels.csv")

SI_coefs_reproModels<-rbind(data.frame(Response="Flowers",Dataset="All spp.", tidy(FL_binom_m1,conf.int=TRUE,effects=c("fixed"))),
                            data.frame(Response="Unripe fruit",Dataset="All spp.",tidy(FRI_binom_m1_v2,conf.int=TRUE,effects=c("fixed"))),
                            data.frame(Response="Ripe fruit",Dataset="All spp.",tidy(FRM_binom_m1,conf.int=TRUE,effects=c("fixed"))),
                            data.frame(Response="Ripe fruit",Dataset="Important ele fruit",tidy(FRM_ele_important_binom_m1,conf.int=TRUE,effects=c("fixed"))))
SI_coefs_reproModels[,c(4,5,6,8,9)]<-round(SI_coefs_reproModels[,c(4,5,6,8,9)],digits=2)
write.csv(SI_coefs_reproModels,"../results/SI_coefs_reproModels.csv")

write.csv(tidy(FRM_ele_important_binom_m2_v2,conf.int=TRUE),"../results/SI_coefs_FRM_ele_important_monthFE_Model.csv")


SI_variance_reproModels<-rbind(
  data.frame(Response="Flowers",Dataset="All spp.", tidy(FL_binom_m1,effects=c("ran_pars"))),
  data.frame(Response="Unripe fruit",Dataset="All spp.", tidy(FRI_binom_m1_v2,effects=c("ran_pars"))),
  data.frame(Response="Ripe fruit",Dataset="All spp.", tidy(FRM_binom_m1,effects=c("ran_pars"))),
  data.frame(Response="Ripe fruit",Dataset="Important ele fruit", tidy(FRM_ele_important_binom_m1,effects=c("ran_pars"))))
SI_variance_reproModels[,5]<-round(SI_variance_reproModels[,c(5)],digits=2)
write.csv(SI_variance_reproModels,"../results/SI_variance_reproModels.csv")



write.csv(rbind(data.frame(Grouping="Species",coef(FL_binom_m1)$Species),
                data.frame(Grouping="Month",coef(FL_binom_m1)$Month),
                data.frame(Grouping="TreeID",coef(FL_binom_m1)$TreeID)),
          "../results/SI_ranefs_FLModel.csv")
write.csv(rbind(data.frame(Grouping="Species",coef(FRI_binom_m1_v2)$Species),
                data.frame(Grouping="Month",coef(FRI_binom_m1_v2)$Month),
                data.frame(Grouping="TreeID",coef(FRI_binom_m1_v2)$TreeID)),
          "../results/SI_ranefs_FRIModel.csv")
write.csv(rbind(data.frame(Grouping="Species",coef(FRM_binom_m1)$Species),
                data.frame(Grouping="Month",coef(FRM_binom_m1)$Month),
                data.frame(Grouping="TreeID",coef(FRM_binom_m1)$TreeID)),
          "../results/SI_ranefs_FRMModel.csv")
write.csv(rbind(data.frame(Grouping="Species",coef(FRM_ele_binom_m1)$Species),
                data.frame(Grouping="Month",coef(FRM_ele_binom_m1)$Month),
                data.frame(Grouping="TreeID",coef(FRM_ele_binom_m1)$TreeID)),
          "../results/SI_ranefs_FRM_ele_Model.csv")
write.csv(rbind(data.frame(Grouping="Species",coef(FRM_ele_important_binom_m1)$Species),
                data.frame(Grouping="Month",coef(FRM_ele_important_binom_m1)$Month),
                data.frame(Grouping="TreeID",coef(FRM_ele_important_binom_m1)$TreeID)),
          "../results/SI_ranefs_FRM_ele_important_Model.csv")
write.csv(rbind(data.frame(Grouping="Species",coef(FRM_ele_important_binom_m2_v2)$Species),
                data.frame(Grouping="TreeID",coef(FRM_ele_important_binom_m2_v2)$TreeID)),
          "../results/SI_ranefs_FRM_ele_important_mothFE_Model.csv")

##### Predict and plot ####

# https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html#prediction
easyPredCI <- function(model,newdata=NULL,alpha=0.05) {
  #baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  #fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],newdata)
  beta <- fixef(model) #fixed-effects coefficients
  V <- vcov(model)     #variance-covariance matrix of beta
  pred.se <- sqrt(diag(X %*% V %*% t(X))) #std errors of predictions
  #inverse-link function
  linkinv <- family(model)$linkinv
  #construct 95% Normal CIs on the link scale and
  # transform back to the response (probability) scale:
  crit <- -qnorm(alpha/2)
  linkinv(cbind(conf.low=pred0-crit*pred.se,
                conf.high=pred0+crit*pred.se))
}

# Fixed effect predictions

# FL
FL_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018))
FL_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018),
                              Prob_predicted=predict(FL_binom_m1,FL_binom_predict,re.form=NA,type="response"),
                              conf.low=easyPredCI(FL_binom_m1,FL_binom_predict)[,1],
                              conf.high=easyPredCI(FL_binom_m1,FL_binom_predict)[,2])

trendAllFlPlot<-ggplot()+
  geom_jitter(data=ddply(Pheno_all,.(Species,Year),summarise, Prob=mean(FL_binom)),
              aes(x=Year,y=Prob),alpha=1/10)+
  geom_line(data=FL_binom_predict,aes(x=Year,y=conf.low),linetype="dashed",size=0.5)+
  geom_line(data=FL_binom_predict,aes(x=Year,y=conf.high),linetype="dashed",size=0.5)+
  geom_line(data=FL_binom_predict,aes(x=Year,y=Prob_predicted),size=1)+
  theme_classic()+
  annotate("text",label="All species (n=73)",x=2015,y=0.95)+
  ylab("Probability Flowers")+
  xlab("Year")+
  ylim(c(0,1))

# FRI
FRI_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018))
FRI_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018),
                              Prob_predicted=predict(FRI_binom_m1,FRI_binom_predict,re.form=NA,type="response"),
                              conf.low=easyPredCI(FRI_binom_m1,FRI_binom_predict)[,1],
                              conf.high=easyPredCI(FRI_binom_m1,FRI_binom_predict)[,2])

trendAllFRIPlot<-ggplot()+
  geom_jitter(data=ddply(Pheno_all,.(Species,Year),summarise, Prob=mean(FRI_binom)),
              aes(x=Year,y=Prob),alpha=1/10)+
  geom_line(data=FRI_binom_predict,aes(x=Year,y=conf.low),linetype="dashed",size=0.5)+
  geom_line(data=FRI_binom_predict,aes(x=Year,y=conf.high),linetype="dashed",size=0.5)+
  geom_line(data=FRI_binom_predict,aes(x=Year,y=Prob_predicted),size=1)+
  theme_classic()+
  annotate("text",label="All species (n=73)",x=2015,y=0.95)+
  ylab("Probability Unripe Fruit")+
  xlab("Year")+
  ylim(c(0,1))

#FRM

FRM_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018))
FRM_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018),
                              Prob_predicted=predict(FRM_binom_m1,FRM_binom_predict,re.form=NA,type="response"),
                              conf.low=easyPredCI(FRM_binom_m1,FRM_binom_predict)[,1],
                              conf.high=easyPredCI(FRM_binom_m1,FRM_binom_predict)[,2])


#percent change -80.88%
1/FRM_binom_predict$Prob_predicted[FRM_binom_predict$Year==1987] #1/10 change encountering ripe fruit in any month in 1987
1/FRM_binom_predict$Prob_predicted[FRM_binom_predict$Year==2018] #1/53 change encountering ripe fruit in any month in 1987

round(100*((FRM_binom_predict$Prob_predicted[FRM_binom_predict$Year==2018]-
    FRM_binom_predict$Prob_predicted[FRM_binom_predict$Year==1987])/
  FRM_binom_predict$Prob_predicted[FRM_binom_predict$Year==1987]),digits=2)

trendAllFRMPlot<-ggplot()+
  geom_jitter(data=ddply(Pheno_all,.(Species,Year),summarise, Prob=mean(FRM_binom)),
              aes(x=Year,y=Prob),alpha=1/10)+
  geom_line(data=FRM_binom_predict,aes(x=Year,y=conf.low),linetype="dashed",size=0.5)+
  geom_line(data=FRM_binom_predict,aes(x=Year,y=conf.high),linetype="dashed",size=0.5)+
  geom_line(data=FRM_binom_predict,aes(x=Year,y=Prob_predicted),size=1)+
  theme_classic()+
  annotate("text",label="All species (n=73)",x=2015,y=0.95)+
  ylab("Probability Ripe Fruit")+
  xlab("Year")+
  ylim(c(0,1))

#Important Ele fruits only
FRM_ele_important_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018))
FRM_ele_important_binom_predict<-data.frame(Year=1987:2018,Year_rescaled=scale(1987:2018),
                              Prob_predicted=predict(FRM_ele_important_binom_m1,FRM_ele_important_binom_predict,re.form=NA,type="response"),
                              conf.low=easyPredCI(FRM_ele_important_binom_m1,FRM_ele_important_binom_predict)[,1],
                              conf.high=easyPredCI(FRM_ele_important_binom_m1,FRM_ele_important_binom_predict)[,2])

#percent change -87.78.88%
1/FRM_ele_important_binom_predict$Prob_predicted[FRM_ele_important_binom_predict$Year==1987] #1/5 change encountering ripe fruit in any month in 1987
1/FRM_ele_important_binom_predict$Prob_predicted[FRM_ele_important_binom_predict$Year==2018] #1/41 change encountering ripe fruit in any month in 1987

round(100*((FRM_ele_important_binom_predict$Prob_predicted[FRM_ele_important_binom_predict$Year==2018]-
              FRM_ele_important_binom_predict$Prob_predicted[FRM_ele_important_binom_predict$Year==1987])/
             FRM_ele_important_binom_predict$Prob_predicted[FRM_ele_important_binom_predict$Year==1987]),digits=2)

FRM_ele_important_binom_predict$Prob_predicted[FRM_ele_important_binom_predict$Year==2018]

trendImportantEleFRMPlot<-ggplot()+
  geom_jitter(data=ddply(Pheno_all[Pheno_all$Important=="Y",],.(Species,Year),summarise, Prob=mean(FRM_binom)),
              aes(x=Year,y=Prob),alpha=1/10)+
  geom_line(data=FRM_ele_important_binom_predict,aes(x=Year,y=conf.low),linetype="dashed",size=0.5)+
  geom_line(data=FRM_ele_important_binom_predict,aes(x=Year,y=conf.high),linetype="dashed",size=0.5)+
  geom_line(data=FRM_ele_important_binom_predict,aes(x=Year,y=Prob_predicted),size=1)+
  annotate("text",label="Important elephant fruit species (n=14)",x=2010,y=0.95)+
  theme_classic()+
  ylab("Probability of Encountering Ripe Fruit")+
  xlab("Year")+
  ylim(c(0,1))

trendAllFruitPlot<-ggplot()+
  geom_line(data=FRM_binom_predict,aes(x=Year,y=conf.low,colour="All fruit (73 spp.)"),linetype="dashed",size=0.5)+
  geom_line(data=FRM_binom_predict,aes(x=Year,y=conf.high,colour="All fruit (73 spp.)"),linetype="dashed",size=0.5)+
  geom_line(data=FRM_binom_predict,aes(x=Year,y=Prob_predicted,colour="All fruit (73 spp.)"),size=1)+
  geom_line(data=FRM_ele_important_binom_predict,aes(x=Year,y=conf.low,colour="Important elephant fruit (14 spp.)"),linetype="dashed",size=0.5)+
  geom_line(data=FRM_ele_important_binom_predict,aes(x=Year,y=conf.high,colour="Important elephant fruit (14 spp.)"),linetype="dashed",size=0.5)+
  geom_line(data=FRM_ele_important_binom_predict,aes(x=Year,y=Prob_predicted,colour="Important elephant fruit (14 spp.)"),size=1)+
  theme_classic()+
  scale_colour_manual(values=c("cadetblue3","coral2"))+
  ylab("Ripe Fruit Encounter")+
  xlab("Year")+
  theme(legend.title=element_blank(),legend.position=c(0.7,0.9))


# Seasonal predictions
FRM_ele_important_binom_month_predict<-ddply(Pheno_all[Pheno_all$Important=="Y",],
                                             .(Year,Year_rescaled,Month=factor(Month)),summarise,FRM_binom_mean=mean(FRM_binom,na.rm=T))
FRM_ele_important_binom_month_predict<-data.frame(FRM_ele_important_binom_month_predict,
                                                  Prob_predicted=predict(FRM_ele_important_binom_m2_v2,FRM_ele_important_binom_month_predict,re.form=NA,type="response"),
                                                  conf.low=easyPredCI(FRM_ele_important_binom_m2_v2,FRM_ele_important_binom_month_predict)[,1],
                                                  conf.high=easyPredCI(FRM_ele_important_binom_m2_v2,FRM_ele_important_binom_month_predict)[,2])

FRM_ele_important_binom_month_predict$YearName<-paste(FRM_ele_important_binom_month_predict$Year," - Important elephant fruit (n = 14 spp.)",sep="")


seasonalImportantEleFruitPlot<-ggplot(data=FRM_ele_important_binom_month_predict[FRM_ele_important_binom_month_predict$Year %in% c(1987,2017),],)+
  geom_pointrange(aes(x=Month,y=Prob_predicted,ymin=conf.low,ymax=conf.high,shape=factor(Year)),position=position_dodge(width=0.1),linetype="dashed",colour='coral2')+
  geom_line(aes(x=Month,y=Prob_predicted,group=Year),colour='coral2')+
  scale_shape_manual(values=c(1,19))+
  theme_classic()+
  ylab("Ripe Fruit Encounter")+
  xlab("Month")+
  theme(legend.title=element_blank(),legend.position=c(0.15,0.9))
  


# Species random slope predictions
FRM_binom_spp_predict<-ddply(Pheno_all,.(SpeciesShort,Species,Year,Year_rescaled),summarise,FRM_binom_mean=mean(FRM_binom,na.rm=T))
FRM_binom_spp_predict<-data.frame(FRM_binom_spp_predict,
                                  Prob_predicted=predict(FRM_binom_m1,FRM_binom_spp_predict,re.form=~(Year_rescaled|Species),type="response"))

SI_speciesSlopesFruitOverTimePlot<-ggplot(FRM_binom_spp_predict,aes(x=Year,y=Prob_predicted))+
  geom_jitter(data=ddply(Pheno_all[Pheno_all$Year>1986,],.(SpeciesShort,Year),summarise, Prob=mean(FRM_binom)),
              aes(x=Year,y=Prob),alpha=0.2)+
  geom_line()+
  theme_classic()+
  ylab("Probability of encountering ripe fruits")+
  facet_wrap(~SpeciesShort,ncol=7)+
  theme(legend.position = "none",strip.text = element_text(face = "italic"))

# Comparison of species random slope and tree size / age

FRM_binom_m1_TreeID<-data.frame(TreeID=rownames(coef(FRM_binom_m1)$TreeID),
                                randomSlope=coef(FRM_binom_m1)$TreeID[2]$Year_rescaled,
                                randomIntercept=coef(FRM_binom_m1)$TreeID[1]$'(Intercept)')
allTreesDBH<-merge(allTreesDBH,FRM_binom_m1_TreeID,"TreeID")

summary(allTreesDBH)

SI_interceptBySize<-ggplot(allTreesDBH,aes(x=DBH_relative,y=randomIntercept))+
  geom_point(alpha=1/5)+
  xlab("Tree Size Relative to Species Max")+
  ylab("Random Intercept")+
  theme_classic()

SI_slopeBySize<-ggplot(allTreesDBH,aes(x=DBH_relative,y=randomSlope))+
  geom_point(alpha=1/5)+
  xlab("Tree Size Relative to Species Max")+
  ylab("Random Slope")+
  theme_classic()



#### Plot all figures  ####

#Main text figure 
pdf("../figures/fruitOverTimePlot.pdf", width = 4, height = 9)
plot_grid(
  trendAllFruitPlot,
  interannualFASPlot,
  seasonalImportantEleFruitPlot,
  nrow = 3,
  labels = "AUTO",
  rel_heights = c(1, 1,1),
  align="hv"
)
dev.off()

#SI figures
pdf("../figures/SI_speciesSlopesFruitOverTimePlot.pdf", width = 8, height = 12)
plot_grid(
  SI_speciesSlopesFruitOverTimePlot,
  rel_heights = c(1, 1, 1),
  align="hv"
)
dev.off()
ed changes


pdf("../figures/SI_randomEffectsBySize.pdf", width = 4, height = 8)
plot_grid(
  SI_interceptBySize,
  SI_slopeBySize,
  labels="AUTO",
  ncol=1,
  rel_heights = c(1, 1, 1),
  align="hv"
)
dev.off()

pdf("../figures/SI_sampleDescriptionPlot.pdf", width = 6, height = 8)
plot_grid(
  SI_treeSamplePlot,
  SI_speciesSamplePlot,
  labels="AUTO",
  ncol=1,
  rel_heights = c(1, 1, 1),
  align="hv"
)
dev.off()

pdf("../figures/SI_modelPredictionPlots.pdf", width = 6, height = 8)
plot_grid(
  trendAllFlPlot,
  trendAllFRIPlot,
  trendAllFRMPlot,
  labels="AUTO",
  ncol=1,
  rel_heights = c(1, 1, 1),
  align="hv"
)
dev.off()


