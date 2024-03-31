

# TODO: Plot ROC and PR curves based on predicted scores and real labels, and calculate the area under the curves
# 
#' @param score : A list storing predicted scores
#' @param label : A list storing the actual label value
#' @param type: Plot ROC or PR curve
#' @param isCrossValidation: Is it the result of cross validation
#' @param all.fold.curvers: When isCrossValidation is TRUE, whether to plot all cross-validation curves
#' 
#' 
ROC_PR.normal <- function(score, label, type=c("ROC", "PR"), isCrossValidation=FALSE, all.fold.curvers=TRUE, col=NULL, main=NULL, avg="vertical")
{

	num.element <- length(score);
	names.element <- names(score);
	
	#library("colortools")
	if(is.null(col)){
		col = rainbow(num.element); #wheel("steelblue", num = 12);
	}
	
	if(type[1] == "ROC"){
		t.x = "fpr"; t.y = "tpr" 
	}else if(type[1] == "PR"){
		t.x = "rec"; t.y = "prec" 
	}
	
	library(ROCR);


	
	##here used for cross-validatioin
	if(isCrossValidation & is.list(score) & is.list(label) & length(score) > 1 & (length(score) == length(label))){
		pred <- prediction(score,label);  
		perf <- performance(pred, t.y, t.x); 
		
		if(type[1] == "ROC"){
		  perf.auc <- performance(pred,"auc"); 
		}else if(type[1] == "PR"){
		  perf.auc <- performance(pred,"aucpr"); 
		}

		auc <- round(mean(unlist(perf.auc@y.values)),3); 
		
		# plot
		tryCatch(
		  {
		    plot(perf,col=col,lwd=2, avg=avg, main=main, xaxs = "i", yaxs = "i"); 
		    if(all.fold.curvers){ plot(perf,col="grey82",lty=3, add=TRUE); } 
		    
		    if(type[1] == "ROC"){
		      info <- paste0("Avegage AUROC: ", auc, collapse="")
		      legend("bottomright", legend=info, lwd=2, col=col)
		    }else if(type[1] == "PR"){
		      info <- paste0("Avegage AUCPR: ", auc, collapse="")
		      legend("topright", legend=info, lwd=2, col=col)
		    }
		    
		  },
		  warning = function(w) { message('Waring') ; return(NA) },
		  error = function(e) { message('Error') ; return(NA) },
		  finally = { message('next...') }
		)
		
		return(auc);
	}else{
		
		all.auc <- vector(length=num.element)
		for(i in 1:num.element){
			pred <- prediction(score[[i]],label[[i]]); 
			perf <- performance(pred, t.y, t.x);   
		
			## calculate AUC
			if(type[1] == "ROC"){
			  perf.auc <- performance(pred,"auc"); 
			}else if(type[1] == "PR"){
			  perf.auc <- performance(pred,"aucpr"); 
			}
			auc <- round(unlist(perf.auc@y.values), 3); 
			all.auc[i] = auc;
			
			if(i == 1){
				plot(perf, col=col[i], lwd=2,  main=main, xaxs = "i", yaxs = "i");
			}else{
				plot(perf, col=col[i], lwd=2,  main=main, xaxs = "i", yaxs = "i", add=TRUE);
			}
			
		}
		info <- paste(names.element,": ",  all.auc, sep="")
		if(type[1] == "ROC"){
		  legend("bottomright", legend=info, lwd=2, col=col)
		}else if(type[1] == "PR"){
		  legend("topright", legend=info, lwd=2, col=col)
		}
		
		return(all.auc);
	}
	
}
###########################################################################


#' TODO: Plot ROC and PR curves based on the evaluation measures (such as TPR and FPR) at different thresholds
#' 
#' @param x.values : A list storing the evaluation measures of different thresholds
#' @param y.values : A list matching x.values
#' @param type :  Plot ROC or PR curve
#' @param col: The color of the curve
#' @param main The title of the image
#' 
#' 
ROC_PR.metrics <- function(x.values, y.values, type=c("ROC", "PR"), col=NULL, main=NULL)
{
	library(ROCR)
	num.element = length(x.values);
	names.element <- names(x.values);
	
	if(is.null(col)){
		col = rainbow(num.element);
	}
	
	if(type[1] == "ROC"){
		x.name = "False positive rate";
		y.name = "True positive rate";
	} else if(type[1] == "PR"){
		x.name = "Recall";
		y.name = "Precision";
	} 

	
	all.auc <- vector(length=num.element)
	for(i in 1:num.element){
		perf = new("performance", x.name = x.name, y.name = y.name, 
				alpha.name = "Cutoff", x.values = list(x.values[[i]]), y.values = list(y.values[[i]]), 
				alpha.values = list(NULL)) 
		
		auc <- round(simple_auc(y.values[[i]], x.values[[i]]), 3);
		all.auc[i] = auc;
		
		if(i == 1){
			plot(perf, col=col[i], lwd=2,  main=main, xaxs = "i", yaxs = "i");
		}else{
			plot(perf, col=col[i], lwd=2,  main=main, xaxs = "i", yaxs = "i", add=TRUE);
		}
		
	}
	info <- paste(names.element,": ",  all.auc, sep="")
	legend("bottomright", legend=info, lwd=2, col=col)
	
	return(all.auc);
}
###########################################################################################



#' TODO: Calculate AUC based on "True positive rate" and "False positive rate"
#' @param TPR : TPR vector
#' @param FPR : FPR vector paired with TPR
#' 
#' 
simple_auc <- function(TPR, FPR){
	
  t.index = which(is.nan(TPR));
	if(length(t.index) > 0){
		TPR = TPR[-t.index];
		FPR = FPR[-t.index];
	}

	t.index = which(is.nan(FPR));
	if(length(t.index) > 0){
		TPR = TPR[-t.index];
		FPR = FPR[-t.index];
	}
	# inputs already sorted, best scores first 
	dFPR <- c(diff(FPR), 0)
	dTPR <- c(diff(TPR), 0)
	sum(TPR * dFPR) + sum(dTPR * dFPR)/2 
}


