



# Function: Plot the average PR curves of multiple methods
#           
#' @@ 
#' @param PRlist: A list that stores the average PR curves of multiple methods. Each element in it needs to have a name.
#' @param AUCPRlist: A list that stores the area under the average PR curves of multiple methods. Each element in it needs to have a name.
#' @param title: The title of the diagram 
#' @param file: The path and name of the output file
#' @param lineColor : A vector of the same length as the PRlist, specifying the colors of the curves
#'              


curvePR <- function(PRlist, AUCPRlist, title, file, lineColor)
{ 
  PR <- lapply(PRlist, function(x){ x[,c("recall", "precision")] })
  Fmeasure <- lapply(PRlist, function(x){ x[,"Fmeasure"]})
  Fmax <- unlist(lapply(Fmeasure, max))
  AUCPR <- round(unlist(AUCPRlist),3)
  
  info <- paste0(names(Fmax),": avgAUCPR=", AUCPR,  ", Fmax=", Fmax)
  
  
  pdf(file)
  plot(0:1, 0:1, type="n", xlab="Recall", ylab="Precision", bty="n", xaxs = "i", yaxs = "i", main=title)
  for(i in 1:length(PR))
  {
    data0 <- PR[[i]]
    indNA <- (is.na(data0[,1])) | (is.na(data0[,2]))
    data1 <- data0[!indNA,,drop=F]
    
    ind0 <- (data1[,1]==0) & (data1[,2]==0)
    data <- data1[!ind0, , drop=F]
    
    lines(data[,"recall"], data[,"precision"], col=lineColor[i], lwd=2) 
  }
  legend("topright", legend=info, lwd=2, col=lineColor)
  
  dev.off()
}






