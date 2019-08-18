normaliz<-function(x,newmin,newmax){
  oldmin <- min(x)
  oldmax <- max(x)
  if (oldmin == oldmax){
    return ((newmin+newmax)/2)
  }
  else{
    return ((x-oldmin)/(oldmax-oldmin)*(newmax-newmin)+newmin)
  }  
}