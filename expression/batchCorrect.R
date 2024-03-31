


# Function: Batch correction for single-cell expression profiles
# 
#' @@ 
#' @param  expList ： list, the elements in the list represent the expression profiles that need to be integrated
#' @param  Integrate_feature ： The genes that need to be included in the integrated expression profile
#' 

library("Seurat")

batchCorrect <- function(expList, Integrate_feature)
{
  expObj0 <- lapply(expList, function(x){CreateSeuratObject(counts = x)}) 

    cellCount <- sapply(expObj0, ncol) 
  allBatch <- names(cellCount)
  smallBatch <- names(cellCount[cellCount <= 30]) 
  
  if( length(smallBatch) == 0){ expObj1 <- expObj0 }
  
  if(length(smallBatch) == 1)
  {
    
    mergeBatch <- names(sort(cellCount)[1:2])
    largeBatch <- allBatch[!(allBatch %in% mergeBatch)]
    
    mBatch <- merge(x=expObj0[[mergeBatch[1]]], y=expObj0[[mergeBatch[2]]])
    expObj1 <- c(expObj0[largeBatch], list(mBatch=mBatch))
  }
  
  if(length(smallBatch) > 1)
  {
    mergeBatch <- smallBatch
    largeBatch <- allBatch[!(allBatch %in% mergeBatch)]
    
    mBatch <- merge(x=expObj0[[mergeBatch[1]]], y=expObj0[mergeBatch[-1]])
    expObj1 <- c(expObj0[largeBatch], list(mBatch=mBatch))
  }
  
  expObj2 <- lapply(expObj1, function(x){ FindVariableFeatures(object=x, selection.method = "vst",nfeatures = 2000, verbose = FALSE) })
  
  k.filter <- min(200, min(sapply(expObj1, ncol)))
  expObj3 <- FindIntegrationAnchors(object.list = expObj2, dims = 1:30, k.filter=k.filter, verbose=F)
  
  expObj4 <- IntegrateData(anchorset = expObj3, dims = 1:30, features.to.integrate = Integrate_feature)
  
  BCexp <- expObj4@assays$integrated
  expMat <- as.matrix(BCexp@data)
  
  return(expMat)
}








