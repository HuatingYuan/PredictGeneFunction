

# Function : calculate the correlation between genes in the expression profile
# 
#' @@ 
#' @param  expMat ： matrix, expression profile
#' @param  method ： the method used to calculate correlation
#' @param  naAction ：the processing method of 0 in the expression profile
#' 

computeCor <- function(expMat, method=c("pearson", "spearman"), naAction=c("everything", "pairwise.complete.obs"))
{
  if(naAction == "everything"){ res <- cor(t(expMat), method=method, use=naAction) }
  if(naAction == "pairwise.complete.obs") 
  {
    expMat_NA <-  expMat  
    expMat_NA[expMat_NA==0] <- NA
    res <- cor(t(expMat_NA), method=method, use=naAction)
  }
  return(res)
}
##================================================================================================================





# Function : Calculate the mutual information between genes in the expression profile
#
#' @@ 
#' @param  expMat ： matrix, expression profile
#' @param  naAction ：The processing method of 0 in the expression profile
#' @param  estimator ：The estimator used to estimate entropy
#' @param  disc ： The method of discretizing a continuous expression profile


library("minet")
computeMI <- function(expMat, naAction=c("everything", "pairwise.complete.obs"), 
                      estimator=c("mi.empirical", "mi.mm", "mi.shrink", "mi.sg", "pearson","spearman","kendall"), 
                      disc=c("none", "equalfreq", "equalwidth", "globalequalwidth"))
{
  exp <- t(expMat)
  genes <- colnames(exp)
  genePair <- t(combn(genes,2))
  
  res0 <- matrix(0, ncol=length(genes), nrow=length(genes))
  colnames(res0) <- rownames(res0) <- genes
  
  if(naAction == "everything")
  {
    for(i in 1:nrow(genePair))
    {
      pair <- genePair[i,]
      subExp <- exp[,pair[1:2],drop=F]
      nbins <- round(sqrt(nrow(subExp)))
      temp <- build.mim(dataset=subExp, estimator = estimator, disc = disc, nbins =nbins)[pair[1],pair[2]]
      res0[pair[1],pair[2]] <- temp  
    }
    
    res1 <- t(res0) 
    res <- res0 + res1 
  }
  
  
  if(naAction == "pairwise.complete.obs")
  {
    for(i in 1:nrow(genePair))
    {
      pair <- genePair[i,]
      subExp <- exp[,pair[1:2],drop=F]
      ind <- (subExp[,1] != 0) & (subExp[,2] != 0) 
      
      if(sum(ind) < 1)
      {
        res0[pair[1],pair[2]] <- NA  
      }else{
        useExp <- subExp[ind,,drop=F]
        nbins <- round(sqrt(nrow(useExp)))
        temp <- build.mim(dataset=useExp, estimator = estimator, disc = disc, nbins =nbins)[pair[1],pair[2]]
        res0[pair[1],pair[2]] <- temp  
      }
    }
    
    res1 <- t(res0) 
    res <- res0 + res1 
  }
  
  return(res)
}









