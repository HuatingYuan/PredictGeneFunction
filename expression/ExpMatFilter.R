

# Function: Filter single-cell expression profile
# 
#' @@ 
#' @param  ExpMat ： matrix, Expression profile that has undergone logarithmic transformation
#' @param  NODG ： The minimum number of genes expressed in a cell
#' @param  HGAve ： The minimum value of the mean expression of housekeeping genes
#' @param  GeneAggExp ： The minimum value of average gene expression
#' @param  DetCellPro ： The minimum proportion of cells expressing a gene
#' @param  cellCount ： The minimum number of cells expressing a gene
#' @param base ：he base for logarithmic transformation of ExpMat
#' @param Pseudo :  Pseudo counting during logarithmic conversion of ExpMat
#' @param TPMDivided ：The value of TPMDivided when performing "TPM/TPMDivided" processing on ExpMat
#' @param HKG ： Housekeeping genes
#' 

ExpFilter <- function(ExpMat, NODG, HGAve, GeneAggExp, DetCellPro, cellCount, base=2,Pseudo=1,TPMDivided=10,HKG){
  
  #---filter cells----
  DetGeneNum <- apply(ExpMat, 2, function(x){sum(x>0)})
  ind1 <- DetGeneNum > NODG 
  ComGene <- intersect(HKG, rownames(ExpMat))
  HKGExp <- ExpMat[ComGene, ] 
  HKGAveExp <- apply(HKGExp, 2, mean)
  ind2 <- HKGAveExp > HGAve
  indCell <- ind1 & ind2
  ExpMatFilted <- ExpMat[,indCell]
  

  #-----filter genes----
  DetectedCellPro <- rowMeans(ExpMatFilted > 0)
  ind3 <- DetectedCellPro > DetCellPro
  DetectedCellCount <- rowSums(ExpMatFilted > 0)
  ind4 <- DetectedCellCount > cellCount
  TPMExp <- (base^ExpMatFilted-Pseudo)*TPMDivided
  tGeneAggExp <- log2(apply(TPMExp, 1, mean)+1)
  ind5 <- tGeneAggExp > GeneAggExp
  indGene <- ((ind3 & ind4) & ind5)
  ExpMatFilted <- ExpMatFilted[indGene, ]
  
  return(ExpMatFilted)
}



