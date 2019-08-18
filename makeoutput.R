makeoutput<-function(){
  Population_complete<-Population_Evo[[3]]
  Population_final<-Population_complete[, c(1, Generations*No_of_environments+3)]
  
  i<-1
  while(sum(Population_final[, 2]!=0)<Top){
    Population_final[, 2]<-Population_complete[, Generations*No_of_environments+3-i]
    i<-i+1
  }
  
  Population_final<-Population_final[Population_final[, 2]>0, ]
  
  order_dec<-order(Population_final[,2], decreasing=TRUE)
  top_max<-order_dec[1:Top]
  
  order_inc<-order(Population_final[, 2])
  top_min<-order_inc[1:Top]
  
  png(file="Decrease of Survivors over Time.png", bg="transparent")
  plot(0:(Generations*No_of_environments), Population_Evo[[5]], ylim=c(0, No_of_cells*10), main='Number of survivors vs. Generations', xlab='Generations', ylab='Survivors') 
  dev.off()
  

  cat("Top", Top, "most productive cells:", Population_final[top_max, 1], "; with population of", Population_final[top_max, 2], "\n")
  cat("Top", Top, "survived least productive cells:", Population_final[top_min, 1], "; with population of", Population_final[top_min, 2], "\n")
  cat("Top", Top, "most productive cells:", Population_final[top_max, 1], "; with population of", Population_final[top_max, 2], "\n", file="./result.txt", append=TRUE)
  cat("Top", Top, "survived least productive cells:", Population_final[top_min, 1], "; with population of", Population_final[top_min, 2], "\n", file="./result.txt", append=TRUE)
  
  
  
  # The bottom part of program goes over the calculation again to find out the growth curve of the top 5
  # most/least productive cells of the survivors. 
  
  ID_top_max<-Population_final[top_max, 1]
  ID_top_min<-Population_final[top_min, 1]
  
  Population_top<-Population_complete[match(c(ID_top_max, ID_top_min), Population_complete[,1]), ]
  
  Population_total<- Population_Evo[[2]]
  
  print(length(Population_total))
  print(dim(Population_top))

  Populationratio_top<-Population_top[, -c(1,2)] %*% diag(1/Population_total)
  
  
  colorLst <- c("black", "red", "green", "blue", "brown",
                "cyan", "gold", "gray", "plum", "magenta",
                "orange", "purple", "yellow", "black", "violet",
                "darkblue", "darkgreen", "darkred", "darkgray", "dimgray",
                "lightblue", "lightgreen", "lightgray", "orchid", "pink")
  
  
  png(file="Population ratio of most productive cells over time.png", bg="transparent")
  plot(Populationratio_top[1, ], col=colorLst[1], type="l",
       lwd = 0.2, lty = 1, xlab="Generation", ylab="Population ratio")
  for (i in 1:Top){
    lines(Populationratio_top[i, ], col=colorLst[i], lty=i)
  }
  legend("topleft", legend=Population_top[1:Top, 1], cex=.6, col = colorLst[1:Top], as.character(top_max), lty=1:Top)
  dev.off()
  
  png(file="Population of most productive cells over time.png", bg="transparent")
  plot(log10(Population_total), col=colorLst[16], lwd = 0.2, lty = 1, xlab="Generation", ylab="Population (log10)", ylim=c(1e-9, 8))
  for (i in 1:(Top)){
    lines(log10(Population_top[i,]), col=colorLst[i], lty=i)
  }
  legend("topleft", legend=Population_top[1:Top, 1], cex=.6, col = colorLst[1:Top], as.character(top_max), lty=1:Top)
  dev.off()
  
  # Show the growth curve of the most productive cells.
  
  png(file="Population of least productive cells over time.png", bg="transparent")
  plot(Population_top[(Top*2),], col=colorLst[Top*2], lwd = 0.2, lty = 1, type = 'l', xlab="Generation", ylab="Population", ylim = c(1,30))
  for (i in (Top+1):(Top*2-1)){
    lines(Population_top[i,], col=colorLst[i], lty=i)
  }
  legend("topleft", cex=.6, col = colorLst[(Top+1):(Top*2)], as.character(top_min), lty=1:Top)
  dev.off()
  
}