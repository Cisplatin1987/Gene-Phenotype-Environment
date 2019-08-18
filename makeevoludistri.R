makeevoludistri<-function(){
  set.seed(seed)
  
  Decay<-Environment_Ini[[1]]
  Environment_fluc<-Environment_Ini[[2]]
  Optimal_trait<-Environment_Ini[[3]]
  Trait_under_environ<-Environment_Ini[[4]]
  
  Gene_Trait_Indiv_Distribution<-Gentrait_Ini[[1]]
  Gene_Expression<-Gentrait_Ini[[2]]
  Gene_Trait_Map<-Gentrait_Ini[[3]]
  
  Population<-Population_Ini[[1]]
  Population_complete<-c()
  Population_total<-Population_Ini[[2]]
  
  lambda<-Potential_Ini
  
  columnames<-c("ID", "Generation", "Lambda")
  Extinction<-matrix(nrow=0, ncol=length(columnames))
  colnames(Extinction)<-columnames
  
  Survived<-1:No_of_cells
  Survived_total<-rep(0, Generations*No_of_environments+1)
  Survived_total[1]<-No_of_cells
  counter<-No_of_cells
  cut<-0
  
  for (ages in 1:No_of_environments){
    for (i in 1:Generations){
      # cat("Now leaving:", length(Survived), "strains\n")

      
      # print(Survived)
      Survived_temp<-Survived
      current_col<-i+Generations*(ages-1)
      
      if (length(Survived)==1){
        lambda[current_col+1]<-production_level+tan(lambda[current_col+1]/(No_of_genes*No_of_traits*No_of_conditions))/1.6
        #if (Population[1]==Survived){
        #  print("ALL OK")
        #}
      }
      else{
        lambda[, current_col+1]<-production_level+tan(lambda[, current_col+1]/(No_of_genes*No_of_traits*No_of_conditions))/1.6
        }
        

      
      
      


      for (j in Survived){
        


        if (length(Survived_temp)==1){
          Population<-rbind(Population, rep(0, Generations*No_of_environments+3))
          lambda<-rbind(lambda, rep(0, Generations*No_of_environments+2))
        }

          size<-Population[match(j, Population[,1]) ,current_col+2]
          # cat("Size of strain population:", size, "\n")
          P_index<-match(j, Population[, 1])
          l_index<-match(j, lambda[, 1])
          lambda_current<-lambda[l_index,current_col+1]
        
        
        if (size<=0){
          Extinction<-rbind(Extinction, c(j, i, lambda[match(j, lambda[,1]),current_col+1]))
          
          Population_complete<-rbind(Population_complete, Population[P_index, ])
          Population<-Population[-P_index, ]
          lambda<-lambda[-l_index, ]
          

          Survived_temp<-Survived_temp[-match(j, Survived_temp)]
          Gene_Expression<-Gene_Expression[, -match(j, Gene_Expression[1,])]
          next
        }


        offsprings<-0
        

        for (k in 1:size){
          offsprings<-offsprings+rpois(1, lambda_current)
        }
        
        Population[P_index, current_col+3]<-offsprings
        Population[P_index, 2]<-1
        
        if (offsprings>=1000){
          mutation<-floor(offsprings/1000)
          Survived_temp<-c(Survived_temp, (counter+1):(counter+mutation))
          mutate_site<-sample(1:dim(Gene_Trait_Map)[1], size=mutation, replace=TRUE)
          Expression_before<-Gene_Expression[2:dim(Gene_Expression)[1], match(j, Gene_Expression[1,])]
          lambda_new<-array(0, dim=c(mutation, No_of_environments*Generations+2))
          Population_new<-array(0, dim=c(mutation, No_of_environments*Generations+3))
          
          Population_new[,1]<-(counter+1):(counter+mutation)
          Population_new[, current_col+3]<-1
          
          #cat("Mutation Generated!", mutation, "\n")
          
          
          for (k in 1:mutation){
            counter<-counter+1
            
            
            devi<-rnorm(2,sd=abs(rnorm(1)))
            mutate_level<-rnorm(1, mean=devi[1], sd=abs(devi[2]))
            Expression_after<-Expression_before
            Expression_after[mutate_site[k]]<-Expression_after[mutate_site[k]]+mutate_level
            
            Gene_Expression<-cbind(Gene_Expression, c(counter, Expression_after))
            Trait_strength_new<-Expression_after%*%Gene_Trait_Map
            
            lambda_new[k, 1]<-counter
            lambda_new[k, current_col+2]<-Trait_strength_new%*%Optimal_trait[,current_col]
            
            Population<-rbind(Population, Population_new)
          }
         
          lambda<-rbind(lambda, lambda_new)
        }
        while (Population_total[current_col+1] > 2000000){
          Population[, current_col+3]<-ceiling(Population[, current_col+3]/2)
          Population_total[current_col+1]<-sum(Population[, current_col+1])
          cut<-cut+1
          cat("Population cut:", cut, "\n")
        }
        
        

        
      }
      Survived<-Survived_temp
      

      if (length(Survived_temp)==0){
        print("No survival. Experiment ends")
        return(list(Population, Population_total, Survived, Survived_total, lambda))
      }
      
      Population_total[current_col+1]<-sum(Population[, current_col+3])
      Survived_total[current_col+1]<-sum(Population[, current_col+3]>0)
      
      if (!is.na(match(0, Population[,1]))){
        Population<-Population[-match(0, Population[,1]), ]
        lambda<-lambda[-match(0, lambda[,1]), ]  
      }
      
      Population_complete_temp<-rbind(Population_complete, Population)
      order_max<-order(Population_complete_temp[, current_col+3], decreasing=TRUE)[1:Top]

      cat(ages, "," , i, "\t", Survived_total[current_col], "\t", Population_complete_temp[order_max, 1], "\t", Population_complete_temp[order_max, current_col+3], "\n")
    }  
  }
  Population_complete<-rbind(Population_complete, Population)
  return(list(Population, Population_total, Population_complete, Survived, Survived_total, Extinction, lambda))
}