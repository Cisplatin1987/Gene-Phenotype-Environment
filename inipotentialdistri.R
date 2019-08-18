inipotentialdistri<-function(){
  
  set.seed(seed)
  
  Gene_Trait_Indiv_Distribution<-Gentrait_Ini[[1]]
  Trait_strength<-apply(Gene_Trait_Indiv_Distribution, c(2,3), sum)
  
  Optimal_trait<-Environment_Ini[[3]]

  lambda<-matrix(0, nrow=No_of_cells, ncol=No_of_environments*Generations+2)
  lambda_name<-c("ID")
  for (i in 1:No_of_environments){
    lambda_name<-c(lambda_name, paste(i, 1:Generations, sep="_"))
  }
  
  lambda_name<-c(lambda_name, "END")
  colnames(lambda)<-lambda_name
  
  lambda[,1] = 1:No_of_cells
  
  lambda[,2:(No_of_environments*Generations+1)]<-t(Trait_strength)%*%Optimal_trait
  
  lambda[,2]<-normaliz(lambda[,2], lowlimit, uplimit)
  lambda[,2]<-1+tan(lambda[,2])/1.6
  
  return(lambda)
}