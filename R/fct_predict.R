#' Function for predicting data with a trained AI
#'
#' This function predicts new data based on a trained AI. The AI should be
#' created by the function \code{\link{ai_train}}.
#' @param  matched_dtm Matched document-term-matrix of the documents for which
#' the AI should #' predict data. Ideally this matrix is created via
#' \code{\link{match_into_model}}. The text_model must be the same as stored
#' in the \code{trained_learner} object.
#' @param trained_learner Trained learner for predicting the data. The object
#' must be generated via the function \code{\link{ai_train}}.
#' @param verbose \code{TRUE} if information on the progress should be printed
#' to the console.
#' @return Returns a \code{matrix} with the predications for every document.
#' @export
ai_predict<-function(matched_dtm,trained_learner,verbose=FALSE){

  #Schritt 1: ueberfuehrung in Struktur der vorgegebenen DTM

  if(verbose==TRUE){
    print(paste(date(),"Matching document-term-matrices"))
  }

  datamatrix_analysis<-matched_dtm
  datamatrix_analysis<-datamatrix_analysis[,trained_learner$matrix_structure[2,]]

  if(is.vector(datamatrix_analysis)==TRUE){
    datamatrix_analysis<-as.data.frame(t(datamatrix_analysis))
    n_cols=length(datamatrix_analysis)
  } else {
    datamatrix_analysis<-as.data.frame(datamatrix_analysis)
    n_cols=ncol(datamatrix_analysis)
  }


  #Normalisierung der Daten, falls im Training erfolgt
  if(trained_learner$normalization==TRUE){
    if(verbose==TRUE){
    print(paste(date(),"Normalize Input Data"))
    }
    for(i in 1:n_cols){
          datamatrix_analysis[i]<-lapply(X=datamatrix_analysis[i],
                                      FUN=normalize_predict,
                                      min_x=as.numeric(trained_learner$matrix_structure[4,i]),
                                      max_x=as.numeric(trained_learner$matrix_structure[5,i]))
    }
  }
    if(verbose==TRUE){
  print(paste(date(),"Predicting output data"))
    }
  new_prediction<-trained_learner$learner$predict_newdata(as.data.frame(datamatrix_analysis))
  responses<-as.numeric(as.matrix(new_prediction$response))


  if(trained_learner$normalization==TRUE){
    if(verbose==TRUE){
    print(paste(date(),"Re-Normalizing Output Data"))
    }
        responses<-lapply(X=responses,
                        FUN=re_normalize_predict,
                        min_x=trained_learner$target_min,
                        max_x=trained_learner$target_max)
        responses<-as.numeric(responses)


  }

  responses<-as.data.frame(cbind(as.character(rownames(datamatrix_analysis)),as.numeric(responses)))
  colnames(responses)<-c("text_id",trained_learner$category_name)
  if(verbose==TRUE){
    print(paste(date(),"done"))
  }
  return(responses)
  }
