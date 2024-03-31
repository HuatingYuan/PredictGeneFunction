

# Function:  Obtain the relevance score between the GO term and the test gene calculated in each cross-validation, 
#           as well as the real GO term annotation label of the test gene.
#           
#' @@ 
#' @param GO2gene: Binary matrix, where each row represents a GO term and each column represents a gene.
#'                   1 indicates that the corresponding gene is annotated into the corresponding GO term, 
#'                   while 0 indicates that the corresponding term does not contain the corresponding gene
#'                    
#' @param gene2gene: The co-expression matrix of genes
#' @param geneList ： Genes whose functions are to be predicted
#' @param 


source("./predictFunction/predictFunction.R")

predict_term <- function(GO2gene, gene2gene, geneList)
{
  funScore0 <- computeFunScore(GO2gene=GO2gene, gene2gene=gene2gene, geneList=geneList)
  funScore0[is.na(funScore0)] <- 0
  label0 <- GO2gene[,colnames(funScore0),drop=F]
  
  geneCount <- rowSums(label0)
  terms <- names(geneCount[(geneCount>0) & (geneCount<ncol(label0))])
  
  funScore <- funScore0[terms,,drop=F]
  label <- label0[terms,,drop=F]

  result <- list(score=funScore, label=label)
  
  return(result)
}









