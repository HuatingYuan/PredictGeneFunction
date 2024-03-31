


# Function: Perform leave-one-out cross-validation on the test set to evaluate prediction performance
#           
#' @@ 
#' @param GO2gene ： Binary matrix, where each row represents a GO term and each column represents a gene.
#'                   1 indicates that the corresponding gene is annotated into the corresponding GO term, 
#'                   while 0 indicates that the corresponding term does not contain the corresponding gene
#'                   
#' @param gene2gene ： The co-expression matrix of genes
#' @param testGene: Genes in the test set
#' @param GOIC :  A data frame, where each row represents a GO term and the row name is the corresponding GO term, 
#'                contains at least one column of "IC", representing the information content of the corresponding GO term
#' @param PRfile: The path and name of the PDF file to store the PR curve
#' 

source("./validation/devideGene.R")
source("./predictFunction/predictFunction.R")
source("./validation/gene/predict_gene.R")
source("./validation/performance/ROC_PR.R")
source("./validation/performance/avgRM.R")
source("./validation/performance/avgPR.R")


performance_gene <- function(GO2gene, gene2gene, testGene, GOIC, PRfile)
{
  nfoldGene <- devideGene(GO2gene, testGene, type="LOO")
  
  score <- list()
  label <- list()
  for(i in 1:length(nfoldGene))
  {
    gene <- nfoldGene[i]
    score_label<- predict_gene(GO2gene=GO2gene, gene2gene=gene2gene, geneList=gene)
    score[[i]] <- score_label$score[,1]
    label[[i]] <- score_label$label[,1]
  }
  names(score) <- nfoldGene
  names(label) <- nfoldGene
  
  pdf(PRfile)
  AUCPR <- ROC_PR.normal(score=score, label=label, type="PR", isCrossValidation=FALSE, all.fold.curvers=FALSE, col=NULL, main=NULL, avg="vertical")
  names(AUCPR) <- names(score)
  dev.off()
  
  allMeasure <- MeanPerformancePR(scoreList=score, labelList=label, from=1, to=0, step=-0.01)
  PR <- na.omit(allMeasure)
  avgAUCPR <- simple_auc(TPR=PR[,"precision"], FPR=PR[,"recall"])
  
  weights <- GOIC[,1]
  names(weights) <- rownames(GOIC)
  RM <- MeanPerformanceRM(scoreList=score, labelList=label, weights=weights, from=1, to=0, step=-0.01)
  
  crossVa_result <- list(AUCPR=AUCPR, PR=PR, avgAUCPR=avgAUCPR, RM=RM)
  return(crossVa_result)
}



 


