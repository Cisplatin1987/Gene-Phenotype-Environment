# Gene-Phenotype-Environment
Project with Dr. Hayes  
Step 1: download all .R files;  
Step 2: switch the folder with downloaded files as the current one;  
Step 3: run R;  
Step 4: run simulation using source("main.R"), or paste the following parameters into the terminal:  
library(plotly)  
uplimit<-1;  
lowlimit<--1;    
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
Top<-5  
production_level<-1  
Gentrait_Ini<-inigenetraitdistri()  
Population_Ini<-inipopudistri()  
Environment_Ini<-inienvirondistri()  
Potential_Ini<-inipotentialdistri()  
Population_Evo<-makeevoludistri()  
Result<-makeoutput()  

The parameters can be changed for different settings.   
Step 5: The program gives the information on the strains with highest populations at each generation and environment, including their IDs and populations. The output files are about the number of survived strains at the end of every generation, the change of population and population ratio of top strains with the highest population at the end of simulation, the population change of the strains with the smallest populations at the end of simulation.  
