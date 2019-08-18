inienvirondistri<-function(){
  set.seed(seed)
  
  Trait_under_environ<-array(rnorm(No_of_traits*No_of_conditions), dim=c(No_of_traits, No_of_conditions))
  Environment_fluc<-array(0, dim=c(No_of_conditions, No_of_environments*Generations))
  Environments<-array(rnorm(No_of_traits*No_of_environments), dim=c(No_of_conditions, No_of_environments))
  
  Decay<-array(0, dim=c(No_of_conditions, No_of_environments-1))
  Optimal_trait<-array(0, dim=c(No_of_traits, No_of_environments*Generations))
  
  for (i in 1:(No_of_environments-1)){
    Decay[,i]<-Environments[,i+1]-Environments[,i]
    print("OK")
    for (j in 1:Generations){
      Environment_fluc[,(i-1)*Generations+j]<-Environments[,i]+Decay[,i]*(j-1)/Generations
      Optimal_trait[,(i-1)*Generations+j]<-Trait_under_environ%*%Environment_fluc[,(i-1)*Generations+j]
    }
  }
  
 return(list(Decay, Environment_fluc, Optimal_trait, Trait_under_environ))
}