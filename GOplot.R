

# Function:  Draw a directed acyclic graph of a GO term set
# 
#' @@ 
#' @param  GOnodes： IDs of GO terms that need to be displayed
#' 
#' 
GOplot <- function(GOnodes)
{
  library(GOstats);
  library(GO.db);
  library(Rgraphviz);
  
  g1 = GOGraph(GOnodes, GOBPPARENTS);
  r1 <- makeNodeAttrs(g1, fillcolor=c("white", "red")[as.numeric(names(makeNodeAttrs(g1)$fillcolor) %in% GOnodes)+1]);
  plot(g1,nodeAttrs=r1)
  
}



