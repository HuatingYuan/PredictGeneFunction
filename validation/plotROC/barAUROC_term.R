


# Function: Plot a bar graph of the average AUROC of all GO terms for each method
#           
#
#' @@ 
#' @param resAUROC: A list that stores the AUROC values of multiple methods. Each element in it needs to have a name.           
#' @param title: The title of the diagram 
#' @param file: The path and name of the output file
#' @param 


library("ggplot2")

barAUROC_term <- function(resAUROC, title, file)
{ 
  avgAUROC <- unlist(lapply(resAUROC, mean))
  data <- data.frame(method=names(avgAUROC), avgAUROC=avgAUROC)

  label <- round(data[,"avgAUROC"], 3)
  p1 <- ggplot(data=data, aes(x=method, y=avgAUROC)) + 
    geom_bar(stat="identity", fill="steelblue") +
    ylim(0,0.8) + 
    labs(y="avgAUROC", title= title) + 
    theme(plot.title = element_text(hjust = 0.5, size=12, margin=margin(t=20, b=30))) +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +    
    theme(axis.title = element_text(size=12)) +   
    theme(axis.text = element_text(size=12)) +     
    geom_text(aes(label=label, y=0.3),  angle=90, size=5)  
  ggsave(file=file)
  
}






