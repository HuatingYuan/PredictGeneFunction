


# Function: bulk expression pre-processing
# 
#' @@ 
#' @param  ExpMat ： matrix, expression profile
#' @param  NODG ： The minimum number of genes expressed in a sample
#' @param  DetPro ： The minimum proportion of gene expression samples
#' @param  sampleCount ： The minimum number of gene expression samples
#'                    

ExpFilter_bulk <- function(ExpMat, NODG=2000, DetPro=0.2, sampleCount=20){
  
  #---filter samples----
  DetGeneNum <- apply(ExpMat, 2, function(x){sum(x>0)})
  indCell<- DetGeneNum > NODG 
  ExpMatFilted <- ExpMat[,indCell]
  
  
  #-----filter genes----
  DetectedCellPro <- rowMeans(ExpMatFilted > 0)
  ind1 <- DetectedCellPro > DetPro
  DetectedsampleCount <- rowSums(ExpMatFilted > 0)
  ind2 <- DetectedsampleCount > sampleCount
  indGene <- (ind1 & ind2)
  ExpMatFilted <- ExpMatFilted[indGene, ]
  
  return(ExpMatFilted)
}


