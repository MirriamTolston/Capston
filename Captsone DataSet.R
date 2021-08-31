rm(list=ls(all=TRUE))
library("haven") #be sure to install these first!
library("survey")
#install.packages("tidyverse")
library(dplyr)
require(data.table)
chs19<-read_sas("~/Capstone:\\chs2019_public.sas7bdat")

#city-wide estimates
chs<-transform(chs17,strata=as.character(strata),all=as.factor(survey))

#define the survey
chs.dsgn<-svydesign(ids = ~1,strata = ~strata,weights=~wt18_dual,data = chs,nest = TRUE,na.rm=TRUE )
#age adjusted survey
pop.agecat4=c(0.128810, 0.401725, 0.299194, 0.170271)
chs.stdes<-svystandardize(subset(chs.dsgn,diabetes17>0 ),by=~agegroup,over=~all,population=pop.agecat4,excluding.missing =~ agegroup+ ~all)

#weighted N
aggregate(chs17$wt18_dual, by=list(Category=chs17$diabetes17), FUN=sum)

#crude prevalance estimates
svyby(~diabetes17==1,~all,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
svyby(~diabetes17==2,~all,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))

#age adjusted prevalance estimates

svyby(~diabetes17==1,~all,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
svyby(~diabetes17==2,~all,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))

#estimate by sex
chs<-transform(chs17,strata=as.character(strata),allsex2=as.factor(sex))

#define the survey
chs.dsgn<-svydesign(ids = ~1,strata = ~strata,weights=~wt18_dual,data = chs,nest = TRUE,na.rm=TRUE )
#age adjusted survey
pop.agecat4=c(0.128810, 0.401725, 0.299194, 0.170271)
chs.stdes<-svystandardize(subset(chs.dsgn,diabetes17>0 ),by=~agegroup,over=~allsex2,population=pop.agecat4,excluding.missing =~ agegroup+ ~allsex2)

#crude prevalance estimates
svyby(~diabetes17==1,~allsex2,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
svyby(~diabetes17==2,~allsex2,subset(chs.dsgn,diabetes17>0),svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))


#age adjusted prevalance estimates

svyby(~diabetes17==1,~allsex2,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))
svyby(~diabetes17==2,~allsex2,chs.stdes,svyciprop,vartype = "ci",method="xlogit",df=degf(chs.dsgn))