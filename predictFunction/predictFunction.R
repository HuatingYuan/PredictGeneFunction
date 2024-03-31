


# Function: Using neighbor-voting algorithm to calculate the score of the functional relevance between a gene and a GO term 
#
#' @@ 
#' @param GO2gene ： Binary matrix, where each row represents a GO term and each column represents a gene.
#'                   1 indicates that the corresponding gene is annotated into the corresponding GO term, 
#'                   while 0 indicates that the corresponding term does not contain the corresponding gene
#'                    
#' @param gene2gene ： The co-expression matrix of genes
#' @param geneList : Genes whose functions are to be predicted
#' 

computeFunScore <- function(GO2gene, gene2gene, geneList)
{
  GO2gene0 <- as.matrix(GO2gene)
  gene2gene0 <- as.matrix(gene2gene)
  
  neighbor0 <- setdiff(colnames(GO2gene0), geneList) 
  neighbor_use <- intersect(neighbor0, rownames(gene2gene0))
  sub_GO2gene <- GO2gene0[, neighbor_use, drop=F]   
  
  sub_gene2gene <- gene2gene0[neighbor_use, geneList, drop=F]  
  

  wMat <-  t(sub_GO2gene %*% sub_gene2gene) 
  degree <- colSums(gene2gene0)[rownames(wMat)] 
  funScore <- t(wMat/degree)
  
  return(funScore)
}



