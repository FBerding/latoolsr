#'Get pre configurated learners
#'
#'Function for getting pre-configurated learners for convenience.
#'
#'@param learner_name Name of the learner to be used. See notes for more
#'details.
#'@param autotuning \code{TRUE} if the learner should be auto tuned while
#'learning. \code{FALSE} if not desired.
#'@param use_smote \code{TRUE} if smote should be applied. \code{FALSE} if not.
#'@param smote_K Default value for applying smote.
#'@param smote_k_max Maximum value for applying smote wihtin auto tuning.
#'@param smote_dup Default value for applying smote.
#'@param smote_dup_max Maximum value for applying smote wihtin auto tuning
#'@param inner_sampling Kind of inner sampling for auto tuning.
#'@param cr_optim Criterion for deciding about the best set of
#'hyperparameters.
#'@param max_n_tuning Maximum number of iterations for auto tuning.
#'@import paradox
#'@import mlr3pipelines
#'@import mlr3
#'@import mlr3learners
#'@export
 get_configurated_learner<-function(learner_name,
                                    use_smote=TRUE,
                                    autotuning=FALSE,
                                    smote_K=1,
                                    smote_k_max=6,
                                    smote_dup=1,
                                    smote_dup_max=5,
                                    inner_sampling,
                                    cr_optim,
                                    max_n_tuning)
{
  std_learner<-NULL
  tuned_learner_smote<-NULL
  tuned_learner<-NULL
  Vorhersage_Typ<-"response"

  if(learner_name=="rpart")
  {

    std_learner<-mlr3::lrn("classif.rpart",predict_type=Vorhersage_Typ, cp=0.01, maxdepth=30, minbucket=7, minsplit=20)

    tune_parameter=paradox::ParamSet$new(list(
      paradox::ParamDbl$new("classif.rpart.cp", lower = 0, upper = 0.01),
      paradox::ParamInt$new("classif.rpart.maxdepth", lower = 25, upper = 30),
      paradox::ParamInt$new("classif.rpart.minbucket", lower = 1, upper = 5),
      paradox::ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 5)
    ))

    tune_parameter_smote=paradox::ParamSet$new(list(
      paradox::ParamDbl$new("classif.rpart.cp", lower = 0, upper = 0.01),
      paradox::ParamInt$new("classif.rpart.maxdepth", lower = 25, upper = 30),
      paradox::ParamInt$new("classif.rpart.minbucket", lower = 1, upper = 5),
      paradox::ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 5),

      paradox::ParamInt$new("smote.dup_size", lower = 1, upper = smote_dup_max),
      paradox::ParamInt$new("smote.K", lower = 1, upper = smote_k_max)
    ))

    po_smote= mlr3pipelines::po("smote",dup_size=smote_dup, K=smote_K)
    learner_smote=  mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(po_smote, std_learner, in_place=FALSE))

    tuned_learner = mlr3tuning::AutoTuner$new(
      learner=std_learner,
      resampling = inner_sampling,
      measure = cr_optim,
      search_space =tune_parameter_smote,
      terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      tuner=mlr3tuning::tnr("random_search")
    )

    tuned_learner_smote = mlr3tuning::AutoTuner$new(
      learner=learner_smote,
      resampling = inner_sampling,
      measure = cr_optim,
      search_space =tune_parameter_smote,
      terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      tuner=mlr3tuning::tnr("random_search")
    )


  }

  if (learner_name=="nnet")
  {
    std_learner<-mlr3::lrn("classif.nnet",predict_type=Vorhersage_Typ,
                     maxit=6*1e+6,
                     decay=0,abstol=1e-04,
                     reltol=1e-08,
                     size =5,
                     MaxNWts=1000000,
                     trace=FALSE
                     )

    tune_parameter=paradox::ParamSet$new(list(
      paradox::ParamDbl$new("classif.nnet.decay", lower=0, upper =.2),
      #ParamDbl$new("classif.nnet.abstol", lower=1e-08, upper =1e-02),
      #ParamDbl$new("classif.nnet.reltol", lower=1e-16, upper =1e-04),
      paradox::ParamInt$new("classif.nnet.size", lower=2, upper =20)
    ))

    tune_parameter_smote=paradox::ParamSet$new(list(
      paradox::ParamDbl$new("classif.nnet.decay", lower=0, upper =.2),
      #ParamDbl$new("classif.nnet.abstol", lower=1e-08, upper =1e-02),
      #ParamDbl$new("classif.nnet.reltol", lower=1e-16, upper =1e-04),
      paradox::ParamInt$new("classif.nnet.size", lower=2, upper =20),

      paradox::ParamInt$new("smote.dup_size", lower = 1, upper = smote_dup_max),
      paradox::ParamInt$new("smote.K", lower = 1, upper = smote_k_max)
    ))

    po_smote=mlr3pipelines::po("smote",dup_size=smote_dup, K=smote_K)
    learner_smote= mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(po_smote, std_learner, in_place=FALSE))

    tuned_learner = mlr3tuning::AutoTuner$new(
      learner=std_learner,
      resampling = inner_sampling,
      measure = cr_optim,
      search_space =tune_parameter_smote,
      terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      tuner=mlr3tuning::tnr("random_search")
    )

    tuned_learner_smote = mlr3tuning::AutoTuner$new(
      learner=learner_smote,
      resampling = inner_sampling,
      measure = cr_optim,
      search_space =tune_parameter_smote,
      terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      tuner=mlr3tuning::tnr("random_search")
    )
  }

  if (learner_name=="ranger")
  {
    std_learner<-mlr3::lrn("classif.ranger",predict_type=Vorhersage_Typ,
                     max.depth=30,
                     num.trees=500,
                     splitrule="gini",
                     replace=TRUE
    )

    tune_parameter=paradox::ParamSet$new(list(
      paradox::ParamInt$new("classif.ranger.max.depth",lower=25, upper=90),
      paradox::ParamFct$new("classif.ranger.splitrule",levels=c("gini","extratrees")),
      paradox::ParamLgl$new("classif.ranger.replace")
    ))

    tune_parameter_smote=paradox::ParamSet$new(list(
      paradox::ParamInt$new("classif.ranger.max.depth",lower=25, upper=90),
      paradox::ParamFct$new("classif.ranger.splitrule",levels=c("gini","extratrees")),
      paradox::ParamLgl$new("classif.ranger.replace"),

      paradox::ParamInt$new("smote.dup_size", lower = 1, upper = smote_dup_max),
      paradox::ParamInt$new("smote.K", lower = 1, upper = smote_k_max)
    ))

    po_smote=mlr3pipelines::po("smote",dup_size=smote_dup, K=smote_K)
    learner_smote= mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(po_smote, std_learner, in_place=FALSE))

    tuned_learner = mlr3tuning::AutoTuner$new(
      learner=std_learner,
      resampling = inner_sampling,
      measure = cr_optim,
      search_space =tune_parameter_smote,
      terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      tuner=mlr3tuning::tnr("random_search")
    )

    tuned_learner_smote = mlr3tuning::AutoTuner$new(
      learner=learner_smote,
      resampling = inner_sampling,
      measure = cr_optim,
      search_space =tune_parameter_smote,
      terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      tuner=mlr3tuning::tnr("random_search")
    )
  }

  if(autotuning==FALSE & use_smote==FALSE)
  {
    return(std_learner)
  }
  if(autotuning==FALSE & use_smote==TRUE){
    return(learner_smote)
  }
  if(autotuning==TRUE & use_smote==TRUE){
    return(tuned_learner_smote)
  }
  if(autotuning==TRUE & use_smote==FALSE){
      return(tuned_learner)
  }

}
