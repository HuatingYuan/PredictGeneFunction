

# Function: Plot a Sankey diagram of the predictive performance of multiple methods
#           
#' @@ 
#' @param pairedData: A list, each element of which is a numeric vector that stores the data that needs to be compared for the corresponding method
#' @param title: The title of the diagram 
#' @param file: The path and name of the output file
#' @param 


library(ggplot2)
library(ggalluvial)
library(reshape2)
library(tidyr)


pairCompare <- function(pairedData, title, file, ylab)
{ 
  data0 <- as.data.frame(do.call(cbind, pairedData))
  data0$object <- rownames(data0)
  data <- gather(data0, -c(ncol(data0)), key="group", value="value")
  
  p <- ggplot(data,aes(x = group, stratum = object, alluvium = object, fill = object, y=value)) +
    geom_flow() +
    geom_stratum(linetype=0) + 
    theme(legend.position="none") + 
    labs(y=ylab, title= title) + 
    theme(plot.title = element_text(hjust = 0.5, size=12, margin=margin(t=20, b=30))) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    theme(axis.title = element_text(size=12)) + 
    theme(axis.text = element_text(size=12)) 
  ggsave(file=file) 
  
}






