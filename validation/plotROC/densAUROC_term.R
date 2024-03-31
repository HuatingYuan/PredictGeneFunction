


# Function: For each method, Plot the density curve of the average AUROC for each GO term in cross validation
#           
#' @@ 
#' @param resAUROC: A list that stores the AUROC values of multiple methods. Each element in it needs to have a name.           
#' @param title: The title of the diagram 
#' @param file: The path and name of the output file
#' @param lineColor: A vector of the same length as the resAUROC, specifying the colors of the curves


densAUROC_term <- function(resAUROC, title, file, lineColor)
{ 
  avgAUROC <- unlist(lapply(resAUROC, mean))
  info <- paste0(names(avgAUROC),": avgAUROC=", round(avgAUROC, 3))
  
  pdf(file)
  plot(density(resAUROC[[1]]), xlab="Average GO AUROCs", ylab="Density", main=title, col=lineColor[1], lwd=3, ylim=c(0,8))
  
  if(length(resAUROC)>1)
  {
    for(i in 2:length(resAUROC)){ lines(density(resAUROC[[i]]), col=lineColor[i], lwd=3) }
  }
  
  legend("topright", legend=info, lwd=3, col=lineColor)
  dev.off()
  
}






