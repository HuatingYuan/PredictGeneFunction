

# Function: Perform n-fold cross-validation on the test set to calculate the term-centric evaluation measure for the method
# 
#' @@ 
#' @param GO2gene: Binary matrix, where each row represents a GO term and each column represents a gene.
#'                   1 indicates that the corresponding gene is annotated into the corresponding GO term, 
#'                   while 0 indicates that the corresponding term does not contain the corresponding gene
#' @param gene2gene: The co-expression matrix of genes
#' @param testGene: Genes in the test set
#' @param n:  n-fold cross-validation
#' @param PRfile: The path and name of the PDF file to store the PR curve
#' @param ROCfile: The path and name of the PDF file to store the ROC curve
#' @param 

source("./validation/devideGene.R")
source("./predictFunction/predictFunction.R")
source("./validation/term/predict_term.R")
source("./validation/performance/ROC_PR.R")
source("./validation/performance/avgPR.R")


performance_term <- function(GO2gene, gene2gene, testGene, n, ROCfile)
{
  nfoldGene <- devideGene(GO2gene=GO2gene, testGene=testGene, type="nfold", n=n)

  scoreList0 <- list()
  labelList0 <- list()
  for(i in 1:n )
  {
    gene <- names(nfoldGene[nfoldGene==i])
    resPre<- predict_term(GO2gene=GO2gene, gene2gene=gene2gene, geneList=gene)
    
    score0 <- resPre$score
    score1 <- apply(score0,1,list)
    scoreList0[[i]] <- lapply(score1, unlist)
    
    label0 <- resPre$label
    label1 <- apply(label0,1,list)
    labelList0[[i]] <- lapply(label1, unlist)
  }
  
  terms <- unique(unlist(lapply(labelList0, names)))
  scoreList <- list() 
  labelList <- list() 
  for(i in terms)
  {
    score <- lapply(scoreList0, function(x){x[[i]]})
    label <- lapply(labelList0, function(x){x[[i]]})
    
    ind <- unlist(lapply(label, function(x){ !is.null(x)}))
    score <- score[ind]
    label <- label[ind]
    fold <- paste0("fold",1:length(label)) 
    names(score) <- names(label) <- fold
    
    scoreList[[i]] <- score
    labelList[[i]] <- label
  }
  
  
  AUROC <- AUCPR<- vector(length=length(terms))
  names(AUROC) <- names(AUCPR) <- terms 
  PR_term <- list()
  precision <- c()
  recall <- c()
  
  for(i in terms)
  {
    score <- scoreList[[i]]
    label <- labelList[[i]]
    
    pdf( ROCfile )
    AUROC[i] <- ROC_PR.normal(score=score, label=label, type="ROC", isCrossValidation=TRUE, all.fold.curvers=TRUE, col=NULL, main=i, avg="vertical")
    dev.off()
    
    
    allMeasure <- MeanPerformancePR(scoreList=score, labelList=label, from=1, to=0, step=-0.01)
    AUCPR[i] <- simple_auc(TPR=allMeasure[,"precision"], FPR=allMeasure[,"recall"])
    PR_term[[i]] <- na.omit(allMeasure)
    
    precision <- cbind(precision, allMeasure[,"precision"])
    recall <- cbind(recall, allMeasure[,"recall"])
      
   }
  
  
  avgPR <- cbind(rowMeans(precision, na.rm=TRUE), rowMeans(recall, na.rm=TRUE)) 
  colnames(avgPR) <- c("precision", "recall")
  thresholds = seq(from=1, to=0, by=-0.01) 
  Fmeasure <- round((2 * avgPR[, "precision"] * avgPR[, "recall"])/(avgPR[, "precision"] + avgPR[, "recall"]), 3)
  avgPR <- na.omit(cbind(avgPR, Fmeasure, thresholds)) 
  
  avgAUCPR <-  simple_auc(TPR=avgPR[, "precision"], FPR=avgPR[, "recall"]) 
  
  crossVa_result <- list(AUROC=AUROC, AUCPR=AUCPR, PR_term=PR_term, avgPR=avgPR, avgAUCPR=avgAUCPR)
  return(crossVa_result)
}







