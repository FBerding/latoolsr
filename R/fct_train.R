#' Training of Artificial Intelligence
#'
#' Function for training an AI with several configurations and options
#' provided by several packages (e.g. mlr3, text2vec).
#'
#' @param basic_text_rep Textual representation from which the AI should learn.
#' The object should be generated with the function
#' \code{\link{create_basic_representation}}.
#' @param text_model The model of the texts that should be used for training AI.
#' This object is must be generated with \code{\link{create_text_model}}.
#' @param save_text_model \code{TRUE} if the text_model should be saved within
#' the trained AI. \code{FALSE} if not desired.
#' @param learner Learner used for modeling the AI. It must be created with
#' mlr3.
#' @param normalize \code{TRUE} if the independent and depended variables should be normalized
#' before training AI. See details for more information.
#' @param n_performance_estimation Number of iterations for estimating the
#' performance of AI via bootstrap.
#' @param ratio_performance_estimation Ratio for splitting the data into
#' training data and test data.
#' @param category_name Name of the target variable that should be predicted.
#' @param filter Name of the filter that should be used.
#' @param filter_ratio Amount of features that should be included
#' in training the AI after applying a filter. For including all features set 1.0.
#' @param verbose \code{TRUE} if information on the progress should be printed
#' to the console. \code{FALSE} if not desired.
#' @param na.rm \code{TRUE} if the analysis should use only complete cases.
#' \code{FALSE} if not.
#' @return Function returns an object of class \code{List} that contains
#' the trained learner and all necessary components for predicting new data.
#'
#' @details
#' \itemize{
#' \item{\code{method:}}{Currently the function supports four methods.
#' \strong{"DTM"} forces that AI to learn based in the document-term-matrix.
#' \strong{"LSA} forces that an LSA is performed based on the initial text
#' representation. AI uses the solution of the LSA.
#' \strong{"LDA"} forces that an LDA is performed based on the initial text
#' representation. AI uses the solution of the LDA.
#' \strong{"GLOVE"} performs the estimation of global vectors based on the initial
#' text representation. The result is used for learning by weighting the the
#' global vectors with the word counts of the document-term-matrix.}
#' \item{\code{normalize: }}{If \code{TRUE} all data is normalized by applying the
#' following formula to all columns of the data.frame:
#' \deqn{(0.95*(x - min(x)) / (max(x) - min(x)))+0.01}}
#' }
#' @export
ai_train<-function(basic_text_rep,
                   text_model,
                   save_text_model=TRUE,
                   learner,
                   normalize=FALSE,
                   n_performance_estimation=30,
                   ratio_performance_estimation=0.75,
                   category_name,
                   filter="jmim",
                   filter_ratio=1.0,
                   verbose=FALSE,
                   na.rm=TRUE){


  data_analysis<-match_into_model(dtm=basic_text_rep$dtm,
                                  text_model = text_model,
                                  verbose=TRUE)
  data_analysis<-as.data.frame(data_analysis)

  category_data<-basic_text_rep$dtm@docvars[,category_name]
  target_max=max(category_data)
  target_min=min(category_data)

  data_analysis<-cbind(data_analysis,category_data)
  data_analysis<-as.data.frame(data_analysis)

  if(na.rm==TRUE){
    data_analysis<-stats::na.omit(data_analysis)
  }

  datamatrix_non_normalized<-data_analysis
  if(normalize==TRUE)
  {
    if(verbose==TRUE){
      print(paste(date(),"Normalizing Data"))
    }
    data_analysis<-lapply(data_analysis,normalize_train)
    data_analysis<-as.data.frame(data_analysis)
  }

  data_analysis$category_data<-as.factor(data_analysis$category_data)

  n_sampling<-nrow(data_analysis)
  n_p_test<-floor(nrow(data_analysis)*(1-ratio_performance_estimation))
  n_p_train<-n_sampling-n_p_test
  n_iteration<-n_performance_estimation


  max_features=ncol(data_analysis)-1
  n_features<-floor(filter_ratio*max_features)

  task_training<-mlr3::TaskClassif$new(id="training", backend = data_analysis, target="category_data")
  task_training$col_roles$stratum="category_data"
  outer_sampling<-mlr3::rsmp("subsampling", ratio= ratio_performance_estimation, repeats=n_performance_estimation)
  train_set_split<-outer_sampling$instantiate(task_training)
  filter=mlr3filters::flt(filter)

  results_performance<-matrix(data=NA,nrow = 4+length(table(data_analysis$category_data)),ncol=n_performance_estimation)

  results_filtervalues<-matrix(data=NA,nrow=n_performance_estimation, ncol=(ncol(data_analysis)-1))
  colnames(results_filtervalues)<-colnames(data_analysis)[1:(ncol(data_analysis)-1)]

  for(i in 1:n_performance_estimation)
  {
    #print(i)
    train_set = train_set_split$train_set(i)
    test_set = train_set_split$test_set(i)
    #print(i)
    task_filter<-mlr3::TaskClassif$new(id="filter_estimation", backend = data_analysis[train_set,], target="category_data")
    task_filter$col_roles$stratum="category_data"
    #print(i)
    filter$calculate(task_filter)
    abc<-filter
    Liste_Features<-as.vector(rownames(as.matrix(filter$scores[1:floor(filter_ratio*max_features)])))
    for (j in 1:(ncol(data_analysis)-1)){
      results_filtervalues[i,names(filter$scores)[j]]<-filter$scores[names(filter$scores)[j]]
    }

    #print(i)
    task_performance<-mlr3::TaskClassif$new(id="performance_estimation", backend = data_analysis[c(Liste_Features,"category_data")], target="category_data")
    task_performance$col_roles$stratum="category_data"
    #print(i)
    learner$train(task_performance,row_ids=train_set)
    prediction<-learner$predict(task_performance,row_ids = test_set)
    #print(i)

    performance<-prediction$score(mlr3::msr("classif.Iota_AVG"))
    results_performance[1,i]<-i
    results_performance[2,i]<-prediction$score(mlr3::msr("classif.Iota_AVG"))
    results_performance[3,i]<-prediction$score(mlr3::msr("classif.acc"))
    results_performance[4,i]<-prediction$score(mlr3::msr("classif.kalpha"))
    for(j in 1:(nrow(results_performance)-4)){
      if(j<10){
        num<-paste("0",j,sep="")
      }
      if(j>=10){
        num<-j
      }
      results_performance[j+4,i]<-prediction$score(mlr3::msr(paste("classif.Iota_K",num,sep="")))
    }

    if(verbose==TRUE){
      print(paste(date(),"performance estimation:",i,"von",n_performance_estimation,mlr3::msr("classif.Iota_AVG")$id,performance))
    }
  }

  if(verbose==TRUE){
    print(paste(date(),"final training of the learner"))
  }

  #Training mit allen Daten----
  task_filter<-mlr3::TaskClassif$new(id="filter_estimation", backend = data_analysis, target="category_data")
  task_filter$col_roles$stratum="category_data"

  filter$calculate(task_filter)
  Liste_Features<-as.vector(rownames(as.matrix(filter$scores[1:floor(filter_ratio*max_features)])))

  task_training_all<-mlr3::TaskClassif$new(id="training_complett", backend = data_analysis[c(Liste_Features,"category_data")], target="category_data")
  task_training_all$col_roles$stratum="category_data"
  learner$train(task_training_all)

  #datamatrix_min_max<-datamatrix[c(Liste_Features)]
  datamatrix_min_max<-datamatrix_non_normalized[c(Liste_Features)]

  matrix_structure<-matrix(data=NA,nrow=5,ncol =n_features)
  for (i in 1:n_features)
  {
    matrix_structure[1,i]<-i
    matrix_structure[2,i]<-Liste_Features[i]
    #matrix_structure[3,i]<-match_f_v[1,Liste_Features[i]]
    #Min-Werte fuer Normalisierung
    matrix_structure[4,i]<-min(datamatrix_min_max[,i])
    #Max-Werte fuer Normalisierung
    matrix_structure[5,i]<-max(datamatrix_min_max[,i])
  }

  iota<-matrix(data=NA,nrow=(nrow(results_performance)-4),ncol=4)
  colnames(iota)<-c("Label","LCI95","Mean","UCI95")
  rownames(iota)<-rownames(iota,do.NULL = FALSE, prefix = "row")
  for(j in 1:(nrow(results_performance)-4)){
    if(j<10){
      num<-paste("0",j,sep="")
    }
    if(j>=10){
      num<-j
    }
    rownames(iota)[j]<-paste("K",num,sep="")
    iota[j,1]<-rownames(table(datamatrix_non_normalized$category_data))[j]
    iota[j,2]<-mean(results_performance[j+4,])-stats::qnorm(0.975)*stats::sd(results_performance[j+4,])/sqrt(length(results_performance[j+4,]))
    iota[j,3]<-mean(results_performance[j+4,])
    iota[j,4]<-mean(results_performance[j+4,])+stats::qnorm(0.975)*stats::sd(results_performance[j+4,])/sqrt(length(results_performance[j+4,]))
  }
  iota<-as.data.frame(iota)
  iota$Label<-as.character(iota$Label)
  iota$LCI95<-as.numeric(iota$LCI95)
  iota$Mean<-as.numeric(iota$Mean)
  iota$UCI95<-as.numeric(iota$UCI95)

  reliability<-list(
    "AC"=mean(results_performance[3,]),
    "iota1"=iota,
    "KAlpha"=mean(results_performance[4,])
  )

  filtervalues<-rbind(
    colMeans(results_filtervalues)-stats::qnorm(0.975)*apply(X=results_filtervalues,MARGIN = 2,FUN = stats::sd)/sqrt(nrow(results_filtervalues)),
    colMeans(results_filtervalues),
    colMeans(results_filtervalues)+stats::qnorm(0.975)*apply(X=results_filtervalues,MARGIN = 2,FUN = stats::sd)/sqrt(nrow(results_filtervalues))
  )
  filtervalues<-filtervalues[,order(filtervalues[2,],decreasing = TRUE)]
  filtervalues<-rbind(filtervalues,
                      seq(from=1,to=ncol(filtervalues)))
  rownames(filtervalues)<-c("LCI95","Mean","UCI95","Rank")
  filtervalues<-as.data.frame(t(filtervalues))

  filter_summary<-list(
    "Filter"=filter$id,
    "Inclusion_Percentage"=filter_ratio,
    "Scores"=filtervalues
  )

  if(save_text_model==TRUE){
    trained_learner<-list("text_model"=text_model)
  } else {
    trained_learner<-list("text_model"=NULL)
  }
  trained_learner<-list(
    "category_name"=category_name,
    "text_model"=trained_learner$text_model,
    "learner"=learner,
    "n_sample"=nrow(data_analysis),
    "n_input_variables"=ncol(matrix_structure),
    "reliability"=reliability,
    "normalization"=normalize,
    "target_max"=target_max,
    "target_min"=target_min,
    "n_sampling"=n_sampling,
    "n_iteration"=n_iteration,
    "n_p_test"=n_p_test,
    "n_p_train"=n_p_train,
    "n_features"=n_features,
    "matrix_structure"=matrix_structure,
    "filter_summary"=filter_summary
  )

  if(verbose==TRUE){
    print(paste(date(),"Done"))
  }

  return(trained_learner)
}


