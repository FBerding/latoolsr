#'@import R6
.onLoad<-function(libname, pkgname){
  MeasureKalpha=R6::R6Class("MeasureKalpha",
                            inherit = mlr3::MeasureClassif,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  # custom id for the measure
                                  id = "classif.kalpha",

                                  # additional packages required to calculate this measure
                                  packages = character(),

                                  # properties, see below
                                  properties = character(),

                                  # required predict type of the learner
                                  predict_type = "response",

                                  # feasible range of values
                                  range = c(-1, 1),

                                  # minimize during tuning?
                                  minimize = FALSE
                                )
                              }
                            ),

                            private = list(
                              # custom scoring function operating on the prediction object
                              .score = function(prediction, ...) {
                                fun.classif.kalpha=function(truth, response)
                                {
                                  Wert<-irr::kripp.alpha(rbind(truth,response), method = c("ordinal"))
                                  Wert$value
                                }
                                fun.classif.kalpha(prediction$truth, prediction$response)

                              }
                            )
  )

  mlr3::mlr_measures$add("classif.kalpha", MeasureKalpha)


  MeasureKalpha=R6::R6Class("AverageIota",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_AVG",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_AVG=function(truth, response)
                              {
                                Wert<-iotarelr::compute_iota1(cbind(truth,response))
                                Wert$average_iota
                              }
                              fun.classif.Iota_AVG(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_AVG", MeasureKalpha)

MeasureKalpha=R6::R6Class("MinimumIota",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_MIN",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_MIN=function(truth, response)
                              {
                                Wert<-iotarelr::compute_iota1(cbind(truth,response))
                                Wert<-min(Wert$iota)
                              }
                              fun.classif.Iota_MIN(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_MIN", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K01",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K01",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K01=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[1]))=="try-error"){
                                Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[1]
                                }

                              }
                              fun.classif.Iota_K01(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K01", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K02",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K02",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K02=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[2]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[2]
                                }

                              }
                              fun.classif.Iota_K02(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K02", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K03",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K03",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K03=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[3]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[3]
                                }

                              }
                              fun.classif.Iota_K03(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K03", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K04",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K04",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K04=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[4]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[4]
                                }

                              }
                              fun.classif.Iota_K04(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K04", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K05",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K05",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K05=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[5]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[5]
                                }

                              }
                              fun.classif.Iota_K05(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K05", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K06",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K06",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K06=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[6]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[6]
                                }

                              }
                              fun.classif.Iota_K06(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K06", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K07",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K07",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K07=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[7]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[7]
                                }

                              }
                              fun.classif.Iota_K07(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K07", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K08",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K08",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K08=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[8]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[8]
                                }

                              }
                              fun.classif.Iota_K08(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K08", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K09",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K09",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K09=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[9]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[9]
                                }

                              }
                              fun.classif.Iota_K09(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K09", MeasureKalpha)

MeasureKalpha=R6::R6Class("Iota_K10",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_K10",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_K10=function(truth, response)
                              {
                                Iota_Values<-iotarelr::compute_iota1(cbind(truth,response))
                                if(class(try(Wert<-Iota_Values$iota[10]))=="try-error"){
                                  Wert<-NA
                                }
                                else{
                                  Wert<-Iota_Values$iota[10]
                                }

                              }
                              fun.classif.Iota_K10(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_K10", MeasureKalpha)
}
