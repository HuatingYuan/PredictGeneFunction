


# Function: Plot a bar graph of the Smin values of multiple methods
#           
#' @@ 
#' @param resRM: A list that stores the average RM curves of multiple methods. Each element in it needs to have a name.
#' @param title: The title of the diagram 
#' @param file: The path and name of the output file
#'              


library("ggplot2")

barSmin_gene <- function(resRM, title, file)
{ 
  Smeasure <- lapply(resRM, function(x){ x[,"Smeasure"]})
  Smin <- unlist(lapply(Smeasure, min))
  data <- data.frame(method=names(Smin), Smin=Smin)
  
  
  label <- data[,"Smin"]
  p1 <- ggplot(data=data, aes(x=method, y=Smin)) + 
    geom_bar(stat="identity", fill="steelblue") +
    ylim(0,0.8) + 
    labs(y="Smin", title= title) + 
    theme(plot.title = element_text(hjust = 0.5, size=12, margin=margin(t=20, b=30))) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    theme(axis.title = element_text(size=12)) +  
    theme(axis.text = element_text(size=12)) +  
    geom_text(aes(label=label, y=0.3),  angle=90, size=5) 
  ggsave(file=file)
  
}






