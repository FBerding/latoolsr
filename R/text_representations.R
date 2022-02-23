#'Creates a model of texts
#'
#'Function for creating a textual model that provides the structure for
#'preparing data for training AI and prediction new data.
#'
#'@param basic_rep Basic textual representation generated with the function
#'\code{\link{create_basic_representation}}.
#' @param method Method for building an advanced text representation as input
#' for the AI. See details for more information.
#' @param min_termfreq \code{Int} for determining the minimal appearance of a
#' term in the whole data set.
#' @param min_docfreq \code{Int} for determining the minimal number of documents
#' that must contain a term. If the term is used in a lower number of documents
#' it is excluded from the analysis.
#' @param dim \code{Int} representing the number of modeled dimensions.
#' It is only used in combination with methods "LDA","LSA", and ' "GLOVE".
#' @param max_iter Maximum number of iterations allowed for "LDA", "LSA", and
#' "GLOVE".
#' @param verbose \code{TRUE} if information on the progress should be printed
#' to the console. \code{FALSE} if not desired.
#' @return Returns a \code{list} which contains the method used, the text model
#' \code{transformation_matrix} for further analysis, and a matrix for matching
#' new data into the structure of the
#' generated text model(\code{match_f_v_trim}).
#' @details
#' Currently the function supports four methods.
#' \itemize{
#' \item{"DTM" }{forces that AI to learn based in the document-term-matrix.}
#' \item{"LSA" } {frces that an LSA is performed based on the initial text
#' representation. AI uses the solution of the LSA.}
#' \item{"LDA" } {forces that an LDA is performed based on the initial text
#' representation. AI uses the solution of the LDA}
#' \item{"GLOVE" } {performs the estimation of global vectors based on the initial
#' text representation. The result is used for learning by weighting the the
#' global vectors with the word counts of the document-term-matrix.}
#' }
#' @import text2vec
#' @import quanteda
#' @import methods
#'@export
create_text_model<-function(basic_rep,
                            method="DTM",
                            min_termfreq=2,
                            min_docfreq=2,
                            dim=10,
                            max_iter=5000,
                            verbose=TRUE){
  dtm<-basic_rep$dtm
  fcm<-basic_rep$fcm
  transformation_matrix<-NULL

  feature_names<-dtm@Dimnames$features
  match_f_v<-t(as.matrix(feature_names))
  colnames(match_f_v)<-colnames(x=match_f_v,
                                do.NULL = FALSE,
                                prefix="word")


  dtm_matrix<-quanteda::dfm_trim(x=dtm,min_termfreq = min_termfreq,min_docfreq = min_docfreq)
  match_f_v_trim<-dtm_matrix@Dimnames$features
  match_f_v_trim<-t(subset(t(match_f_v),t(match_f_v[1,]) %in% match_f_v_trim))

  if(method=="DTM"){
    results<-NULL
    results["method"]<-list(method)
    results["match_f_v_trim"]<-list(match_f_v_trim)
    results["transformation_matrix"]<-list(NULL)
    return(results)
  } else {

    new_fcm<-quanteda::fcm_keep(x=fcm,
                              pattern=dtm_matrix@Dimnames$features,
                              valuetype="fixed")
    if(method=="GLOVE"){
      glove <- text2vec::GlobalVectors$new(rank = dim,
                                         x_max = 10
                                        )
      set.seed(0)
      wv_main <- glove$fit_transform(new_fcm,
                                   n_iter = max_iter,
                                   convergence_tol = 1e-5,
                                   n_threads = 8,
                                   progressbar = verbose)
      wv_context <-glove$components
      transformation_matrix <- wv_main + t(wv_context)
      #Normierung der Namen
      for(i in 1:length(match_f_v)){
        m<-match(table=rownames(transformation_matrix),x=match_f_v[i])
        rownames(transformation_matrix)[m]<-colnames(match_f_v)[i]
      }
      colnames(transformation_matrix)<-colnames(x=transformation_matrix,
                                              do.NULL = FALSE,
                                              prefix="F")
    }

  if(method=="LSA"){
    for (i in 1:ncol(dtm_matrix)){
      m<-match(table=match_f_v, x=colnames(dtm_matrix)[i])
      colnames(dtm_matrix)[i]<-colnames(match_f_v)[m]
    }

    lsa<-text2vec::LSA$new(n_topics=dim)
    set.seed(0)
    doc_embeddings<-text2vec::fit_transform(x=dtm_matrix,
                                            lsa,
                                            n_iter=max_iter)
    transformation_matrix<-lsa

  }

  if(method=="LDA"){
    dtm_matrix<-methods::as(dtm_matrix,"dgTMatrix")
    for (i in 1:ncol(dtm_matrix)){
      m<-match(table=match_f_v, x=colnames(dtm_matrix)[i])
      colnames(dtm_matrix)[i]<-colnames(match_f_v)[m]
    }

    lda<-text2vec::LDA$new(n_topics = dim)

    set.seed(0)
    doc_embeddings<-text2vec::fit_transform(x=dtm_matrix,
                                            lda,
                                            n_iter=max_iter,
                                            convergence_tol = 1e-10,
                                            n_check_convergence = 100,
                                            progressbar = verbose)
    transformation_matrix<-lda
  }
  results<-NULL
  results["method"]<-list(method)
  results["match_f_v_trim"]<-list(match_f_v_trim)
  results["transformation_matrix"]<-list(transformation_matrix)
  return(results)
  }
}



#'Matching new data into the text model
#'
#'Function for matching new textual data into an existing text model.
#'
#'@param dtm Document-term-matrix generated containing the new data. Object
#'is ideally generated with the function \code{\link{create_basic_representation}}.
#'@param text_model Textmodel in which the new data should be fitted. Object is
#'ideally created with the function \code{link{create_text_model}}.
#'@param verbose \code{TRUE} if information on the progess should be printed
#'to the console. \code{FALSE} if not desired.
#'@return Returns a \code{matrix} which represents the new data at the fundation
#'of the text model.
#'@export
match_into_model<-function(dtm,
                           text_model,
                           verbose=FALSE){
  dtm_new<-as.data.frame(quanteda::convert(dtm, to="matrix"))

  dtm_target<-as.data.frame(matrix(data = 0,nrow=nrow(dtm_new),ncol=ncol(text_model$match_f_v_trim)))
  colnames(dtm_target)<-text_model$match_f_v_trim[1,]

  #for(word in colnames(dtm_target)){
  #  if(word %in% colnames(dtm_new)==TRUE){
  #    dtm_target[word]<-dtm_new[word]
  #  }
  #}
  for(word in colnames(dtm_new)){
    if(word %in% colnames(dtm_target)==TRUE){
      dtm_target[word]<-dtm_new[word]
    }
  }

  #for(i in 1:ncol(dtm_target)){
  #  m<-match(colnames(dtm_target)[i],table=text_model$match_f_v_trim[1,])
  #  colnames(dtm_target)[i]<-colnames(text_model$match_f_v_trim)[m]
  #}
  dtm_target<-dtm_target[,text_model$match_f_v_trim[1,]]
  colnames(dtm_target)<-colnames(text_model$match_f_v_trim)

  if(text_model$method=="DTM"){
    rownames(dtm_target)<-dtm@docvars$docname_
    return(dtm_target)
  } else {

  if(verbose==TRUE){
    print(paste(date(),"Creating compressed text representation with method",text_model$method))
  }

  if(text_model$method=="GLOVE"){
    datamatrix_analysis<-as.matrix(dtm_target) %*% text_model$transformation_matrix[colnames(dtm_target),]
    rownames(datamatrix_analysis)<-dtm@docvars$docname_
    return(datamatrix_analysis)
  }

  if(text_model$method=="LSA"){
    set.seed(0)
    datamatrix_analysis<-transform(as.matrix(dtm_target),
                                   text_model$transformation_matrix,
                                   progressbar = FALSE)

    colnames(datamatrix_analysis)<-colnames(x=datamatrix_analysis,
                                            do.NULL = FALSE,
                                            prefix="F")
    rownames(datamatrix_analysis)<-dtm@docvars$docname_
    return(datamatrix_analysis)
  }
  if(text_model$method=="LDA"){
    set.seed(0)
    datamatrix_analysis<-transform(methods::as(as.matrix(dtm_target),"dgTMatrix"),
                                   text_model$transformation_matrix,
                                   progressbar = FALSE,
                                   convergence_tol = 1e-10,
                                   n_check_convergence = 100)

    colnames(datamatrix_analysis)<-colnames(x=datamatrix_analysis,
                                            do.NULL = FALSE,
                                            prefix="F")
    rownames(datamatrix_analysis)<-dtm@docvars$docname_
    return(datamatrix_analysis)
  }
  }
}
