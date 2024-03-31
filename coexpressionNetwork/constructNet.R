

# Function: Rank gene pairs based on the correlation between them, 
#           then normalize the rank to between 0-1 and use it as the weight of the corresponding edge in the network

#' @@ 
#' @param  coNet ： matrix，the adjacency matrix of co-expression network
#' @param  abs ： TRUE or FALSE, sort edges using correlation coefficients or their absolute values


rankNormNet <- function(coNet, abs=c(TRUE, FALSE))
{
  if(abs){  rankNet <- abs(coNet) }else{ rankNet <- coNet} 
  
  diag(rankNet) <- NA
  rankNet[] <- rank(rankNet, ties.method="average", na.last="keep")
  
  normNet <- rankNet/length(which(!is.na(rankNet)))
  
  normNet[is.na(normNet)] <- 0
  
  return(normNet)
}
##======================================================================================================####
##======================================================================================================####





# Function: Merge network
# 
#' @@ 
#' @param  netList ： list, a list of networks used for merging, 
#                           in which each element represents the adjacency matrix of the co-expression network


integrateNet <- function(netList)
{
  node <- rownames(netList[[1]])
  sortNet <- lapply(netList, function(x){ x[node, node] })
  
  newNet0 <- matrix(0,ncol=ncol(netList[[1]]), nrow=nrow(netList[[1]]))
  for(i in 1:length(sortNet)){ newNet0 <- newNet0+sortNet[[i]] }
  newNet1 <- newNet0/length(sortNet)
  
  
  newNet <- rankNormNet(coNet=newNet1, abs=FALSE)
  
  return(newNet)
}
##======================================================================================================####
##======================================================================================================####





# Function: Select reliable co-expression relationships
# 
#' @@ 
#' @param coMat : co-expression matrix of genes
#' @param k : if the value is between 0-1, it indicates the proportion of co-expressed partners selected; 
#             if the value is a positive integer, it means selecting k genes
#' @param abs ： TRUE or FALSE, sort edges using correlation coefficients or their absolute values
#' @param weight ： whether the co-expression network is weighted

selectEdge <- function(coMat, k, abs=c(TRUE, FALSE), weight=c(TRUE, FALSE))
{
  if(abs){ coNet0 <- abs(coMat) }else{ coNet0 <- coMat }
  diag(coNet0) <- NA
  
  coNet1 <- apply(coNet0, 2, function(x){rank(-x, na.last = T)})
  
  
  if(k<1){ index <- nrow(coMat)*k }else{ index <- k}
  coNet2 <- coNet1
  coNet2[coNet2 <= index] <- 1
  coNet2[coNet2 > index] <- 0
  
  
  coNet3 <- coNet2 + t(coNet2)
  coNet3[coNet3 >= 1] <- 1
  

  if(weight){
    ind0 <- coNet3==0
    coNet <- coNet0
    coNet[ind0] <- 0
    diag(coNet) <- 0
    
  }else{
    coNet <- coNet3
  }
  
  return(coNet)
}



