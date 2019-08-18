inigenetraitdistri<-function(){
  
  set.seed(seed)

  Gene_Expression<-array(rnorm(No_of_genes*No_of_cells), dim=c(No_of_genes,No_of_cells))
  Gene_Trait_Map<-array(rnorm(No_of_genes*No_of_traits), dim=c(No_of_genes,No_of_traits))
  Gene_Trait_Indiv_Distribution<-array(0, dim=c(No_of_genes, No_of_traits, No_of_cells))
  
  for (i in 1:No_of_cells){
    Gene_Trait_Indiv_Distribution[,,i]<-diag(Gene_Expression[,i])%*%Gene_Trait_Map
  }
  
  dimnames(Gene_Trait_Indiv_Distribution)[[1]]<-paste("Gene_", 1:No_of_genes, sep="");
  dimnames(Gene_Trait_Indiv_Distribution)[[2]]<-paste("Trait_",1:No_of_traits, sep="");
  dimnames(Gene_Trait_Indiv_Distribution)[[3]]<-paste("Individual_", 1:No_of_cells,sep="");
  
  uplimit<-1;
  lowlimit<--1;
  
  for (i in 1:No_of_traits){
    Gene_Trait_Indiv_Distribution[,i,]<-normaliz(Gene_Trait_Indiv_Distribution[,i,],lowlimit,uplimit);
  }
  
  Gene_Expression<-rbind(1:No_of_cells, Gene_Expression)
  
  return(list(Gene_Trait_Indiv_Distribution, Gene_Expression, Gene_Trait_Map))
}
  