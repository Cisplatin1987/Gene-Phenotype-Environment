inipopudistri<-function(){
  set.seed(seed)
  
  Population<-array(0,dim=c(No_of_cells,Generations*No_of_environments+3));
  Population[,1]<-1:No_of_cells
  Population[,2]<-1
  Population[,3]<-Ini_Population
  
  Population_total<-rep(0, Generations*No_of_environments+1)
  Population_total[1]<-sum(Ini_Population)

  Population_time<-c("ID", "Old", "Ini")
  
  for (i in 1:No_of_environments){
    Population_time<-c(Population_time, paste(i, 1:Generations, sep="_"))
  }
  
  colnames(Population)<-Population_time
  names(Population_total)<-Population_time[3:length(Population_time)]
  
 return(list(Population, Population_total))
}