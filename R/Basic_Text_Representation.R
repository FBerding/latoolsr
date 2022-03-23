#' Creates basic text representations
#'
#' Function for generating the document-term-matrix and the feature-co-
#' occurrence-matrix for training and predicting.
#'
#'@param path_language_model Path to the language model which should be used
#'for analyzing the data with help of the package udpipe.
#'@param data Object containing the concrete texts.Ideally this object is
#'created via the function \code{readtext} from the readtext package.
#'@param verbose \code{TRUE} if information on the progress should be printed
#'to the console. \code{FALSE} if not.
#'@return Returns a \code{List} with the following two components:
#'\itemize{
#' \item{\code{dtm: }}{Document-term-matrix of class \code{dfm} created with
#' the package quanteda.}
#' \item{\code{fcm: }}{Feature-co-occurence-matrix of class \code{fcm} created
#' with the package quanteda.}
#' }
#'@export
create_basic_representation<-function(path_language_model,data,verbose=TRUE)
{

#Phase 1: Analyse der Texte mit udpipe
ud_language_modell<-udpipe::udpipe_load_model(file = path_language_model)
ud_text_analysis<-udpipe::udpipe_annotate(ud_language_modell,x=data$text,doc_id = data$doc_id, trace = verbose, parser = "none")
ud_text_analysis<-as.data.frame(ud_text_analysis)
ud_text_analysis$ID<-udpipe::unique_identifier(ud_text_analysis, fields = c("doc_id", "paragraph_id", "sentence_id"))

#Ermittlung der Woerter, die im Originaltext ein Nomen, ein Verb oder ein Adjektiv sind
material_from<-subset(ud_text_analysis, ud_text_analysis$upos %in% c("NOUN", "ADJ","VERB"), term="upos")
material_from<-material_from$token

#Erstellung der Lemmata zu den Woertern, die im Originaltext ein Nomen, ein Verb oder ein Adjektiv sind
material_to<-subset(ud_text_analysis, ud_text_analysis$upos %in% c("NOUN", "ADJ","VERB"), term="lemma")
material_to<-material_to$lemma

#Phase 2: Korpusbildung mit quanteda
textual_corpus <-quanteda::corpus(data)
token<-quanteda::tokens(textual_corpus,
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove_numbers = TRUE,
              remove_url = TRUE,
              verbose = verbose)
token<-quanteda::tokens_remove(x=token,pattern=quanteda::stopwords(language ="de" ))
token<-quanteda::tokens_replace(x=token,
                                pattern = material_from,
                                replacement = material_to,
                                valuetype = "fixed",
                                verbose=verbose)
token<-quanteda::tokens_tolower(x=token)


dtm<-quanteda::dfm(token)

fcm<-quanteda::fcm(token,
         context = "window",
         count = "weighted",
         weights = 1 / (1:5),
         tri = TRUE)

results<-NULL
results<-list(
  "dtm"=dtm,
  "fcm"=fcm
)

return(results)
}


