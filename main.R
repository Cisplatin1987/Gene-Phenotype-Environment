
library(plotly)

uplimit<-1;
lowlimit<--1;
# Uplimit and low limit for normalization.
source('inienvirondistri.R')
source('inigenetraitdistri.R')
source('inipopudistri.R')
source('inipotentialdistri.R')
source('normaliz.R')
source('makeevoludistri.R')
source('makeoutput.R')


seed<-1000
No_of_genes<-20;
No_of_traits<-10;
No_of_cells<-1000;
No_of_conditions<-40
Ini_Population<-rep(1000, No_of_cells);
Generations<-30;
No_of_environments<-3
fierce<-FALSE
decay = 0.25
Top<-5
oppo<-FALSE
production_level<-1

Gentrait_Ini<-inigenetraitdistri()

Population_Ini<-inipopudistri()

Environment_Ini<-inienvirondistri()

Potential_Ini<-inipotentialdistri()

Population_Evo<-makeevoludistri()

Result<-makeoutput()

