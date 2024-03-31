


# Function: Obtain the relevance score between the GO term and the test gene calculated in each cross-validation, 
#           as well as the real GO term annotation label of the test gene.
#           
#' @@ 
#' @param GO2gene ： Binary matrix, where each row represents a GO term and each column represents a gene.
#'                   1 indicates that the corresponding gene is annotated into the corresponding GO term, 
#'                   while 0 indicates that the corresponding term does not contain the corresponding gene
#'                    
#' @param gene2gene ： The co-expression matrix of genes
#' @param geneList ： Genes whose functions are to be predicted
#' 

source("./predictFunction/predictFunction.R")

predict_gene <- function(GO2gene, gene2gene, geneList)
{
  funScore <- computeFunScore(GO2gene=GO2gene, gene2gene=gene2gene, geneList=geneList)
  funScore[is.na(funScore)] <- 0
  label <- GO2gene[,colnames(funScore),drop=F]
  
  result <- list(score=funScore, label=label)
  
  return(result)
}









