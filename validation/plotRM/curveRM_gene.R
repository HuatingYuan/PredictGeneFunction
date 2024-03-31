


# Function: Plot the remaining uncertainty - misinformation curves of multiple methods
# 
#' @@ 
#' @param resRM: A list that stores the average RM curves of multiple methods. Each element in it needs to have a name.
#' @param title: The title of the diagram 
#' @param file: The path and name of the output file
#' @param lineColor: A vector of the same length as the PRlist, specifying the colors of the curves
#' 


curveRM_gene <- function(resRM, title, file, lineColor)
{ 
  RM <- lapply(resRM, function(x){ x[,c("ru", "mi")] })
  Smeasure <- lapply(resRM, function(x){ x[,"Smeasure"]})
  Smin <- unlist(lapply(Smeasure, min))
  
  info <- paste0(names(Smin), ": Smin=", Smin)

  pdf(file)
  plot(0:1, 0:1, type="n", xlab="Remaining uncertainty", ylab="Misinformation", bty="n", xaxs = "i", yaxs = "i", main=title)
  for(i in 1:length(RM))
  {
    data0 <- RM[[i]]
    indNA <- (is.na(data0[,1])) | (is.na(data0[,2]))
    data1 <- data0[!indNA,,drop=F]
    ind0 <- (data1[,1]==0) & (data1[,2]==0)
    data <- data1[!ind0, , drop=F]
    
    lines(data[,"ru"], data[,"mi"], col=lineColor[i], lwd=2) 
  }
  legend("topright", legend=info, lwd=2, col=lineColor)
  
  dev.off()
}






