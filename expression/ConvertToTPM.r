
## Function: Convert the Count, FPKM, and RPKM values into TPM values
#' 
#' 
#' @param  Exp: matrix, expression profile
#' @param  matrices: the expression measure of Exp
#' @param  log: Whether Exp has been logarithmically transformed
#' @param  logbase: The base for logarithmic transformation of Exp
#' @param  GeneExonLen: The length of gene exons
#' 
#' 
ConvertToTPM <- function(Exp, matrices = "Fpkm", GeneExonLen = NULL, log = F, logbase = NULL){
  if(!is.null(GeneExonLen)){
  Inconsistent <- setdiff(rownames(Exp), names(GeneExonLen)) 
  if(length(Inconsistent) > 0){
    comGene <- intersect(rownames(Exp), names(GeneExonLen))
    Exp <- Exp[comGene, ]
    GeneExonLen <- GeneExonLen[comGene]
  }
  if(length(Inconsistent) == 0){
    Exp <- Exp
	GeneExonLen <- GeneExonLen[rownames(Exp)]
  }
  }
  
  if(log==T){
    Exp <- logbase^Exp -1 
  }
  
  #caculate gene length
  if(is.null(names(GeneExonLen))){
    GeneExonLen <- GeneExonLen[rownames(Exp)] 
  }
  
  #convert count to TPM
  if(matrices == "count"){
    TPMExp <- apply(Exp, 2, function(x){
      CountToTpm(counts = x, lengths = GeneExonLen)
    })
  }
  
  #convert Fpkm to TPM
  if(matrices == "Fpkm"){
    TPMExp <- apply(Exp, 2, function(x){
      FpkmOrRpkmToTpm(FpkmOrRpkm = x)
    })
  }
  
  #convert Rpkm to TPM
  if(matrices == "Rpkm"){
    TPMExp <- apply(Exp, 2, function(x){
      FpkmOrRpkmToTpm(FpkmOrRpkm = x)
    })
  }
  
  return(TPMExp)
  
}

#---convert count to TPM----
CountToTpm <- function(counts, lengths) {
  rate <- counts / lengths
  rate / sum(rate) * 1e6
}
#---convert Fpkm/Rpkm to TPM---
FpkmOrRpkmToTpm <- function(FpkmOrRpkm) {
  FpkmOrRpkm*1e6/sum(FpkmOrRpkm) 
  #TPM = ( FPKM / sum of FPKM over all genes/transcripts) * 10^6
  #TPM = ( RPKM / sum of RPKM over all genes/transcripts) * 10^6
}






