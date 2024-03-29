% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/learner_configuration.R
\name{get_configurated_learner}
\alias{get_configurated_learner}
\title{Get pre configurated learners}
\usage{
get_configurated_learner(
  learner_name,
  use_smote = TRUE,
  autotuning = FALSE,
  smote_K = 1,
  smote_k_max = 6,
  smote_dup = 1,
  smote_dup_max = 5,
  inner_sampling,
  cr_optim,
  max_n_tuning
)
}
\arguments{
\item{learner_name}{Name of the learner to be used. See notes for more
details.}

\item{use_smote}{\code{TRUE} if smote should be applied. \code{FALSE} if not.}

\item{autotuning}{\code{TRUE} if the learner should be auto tuned while
learning. \code{FALSE} if not desired.}

\item{smote_K}{Default value for applying smote.}

\item{smote_k_max}{Maximum value for applying smote wihtin auto tuning.}

\item{smote_dup}{Default value for applying smote.}

\item{smote_dup_max}{Maximum value for applying smote wihtin auto tuning}

\item{inner_sampling}{Kind of inner sampling for auto tuning.}

\item{cr_optim}{Criterion for deciding about the best set of
hyperparameters.}

\item{max_n_tuning}{Maximum number of iterations for auto tuning.}
}
\description{
Function for getting pre-configurated learners for convenience.
}
