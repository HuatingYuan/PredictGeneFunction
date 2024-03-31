

# Function: Randomly divide the test set into n parts
# 
#' @@ 
#' @param GO2gene : Binary matrix, where each row represents a GO term and each column represents a gene.
#'                   1 indicates that the corresponding gene is annotated into the corresponding GO term, 
#'                   while 0 indicates that the corresponding term does not contain the corresponding gene
#'                   
#' @param testGene ： Genes in the test set
#' @param type : The type of cross-validation
#' @param n : n-fold cross-validation
#'                    
#'                    

devideGene <- function(GO2gene, testGene, type=c("LOO", "nfold"), n)
{
  if(type=="LOO"){ nfoldGene <- testGene }
  
  
  if( type =="nfold")
  {
    count <- floor(length(testGene)/n)
    nfoldGene <- ceiling((1:length(testGene))/count)
    names(nfoldGene) <- sample(testGene)
    

    if(length(unique(nfoldGene)) > n )
    {
      outerGene <- names(nfoldGene[ nfoldGene > n])
      nfoldGene[outerGene] <- sample(1:n, length(outerGene)) 
      nfoldGene <- sort(nfoldGene)
    }
  }
  
  return(nfoldGene)
}



