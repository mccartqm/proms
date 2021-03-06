#' Calculates the QuickDASH score
#'
#' Calculates the disability/symptom QuickDASH score, and the
#' optional high performance sport/music or work module scores when specified.
#' The QuickDASH score will \strong{not} be calculated if more than 1 item is
#' missing. An optional module score will \strong{not} be calculated if any
#' of the items are missing.
#'
#' @param data A data frame.
#' @param symptomvars A character vector of length eleven specifying the names of
#' the eleven questions used to calculate the disability/symptom score.
#' @param include_work Logical value specifying whether to calculate a score for
#' the optional work module
#' @param workvars An optional character vector of length four specifying the
#' names of the four questions used to score the work module.
#' @param include_sports Logical value specifying whether to calculate a score for
#' the optional sports/performing arts module
#' @param sportsvars An optional character vector of length four specifying the
#' names of the four questions used to score the sports/performing arts module.
#'
#' @return This function returns the original data frame provided as input with
#' an additional variable storing the QuickDASH score. Additional variables
#' storing the optional module scores are created upon request.
#'
#' @examples
#' df <- data.frame(id = 1:200, replicate(15, sample(1:5, 200, replace = T)))
#'
#' ## disability/symptom score only
#' df <- quickdash(df, names(df)[2:12])
#'
#' ## optional work module score
#' df <- quickdash(df, names(df)[2:12], include_work=T, workvars=names(df)[13:16])
#'
#' @export

quickdash <- function(data, symptomvars, include_work=F, workvars=NULL,
                      include_sports=F, sportsvars=NULL){

  # replace undefined values with missing values
  for (i in symptomvars){
    data[[i]][!(is.na(data[[i]]) | data[[i]] %in% c(1,2,3,4,5))] <- NA
  }

  data$qdscore <- ifelse(rowSums(!is.na(data[symptomvars])) >= 10,
                         (rowSums(data[symptomvars], na.rm = T)/rowSums(!is.na(data[symptomvars])) - 1)*25,
                         NA)

  if(include_work==T){

    for (i in workvars){
      data[[i]][!(is.na(data[[i]]) | data[[i]] %in% c(1,2,3,4,5))] <- NA
    }

    data$qdwork <- (rowSums(data[workvars])/4 - 1)*25
  }

  if(include_sports==T){

    for (i in sportsvars){
      data[[i]][!(is.na(data[[i]]) | data[[i]] %in% c(1,2,3,4,5))] <- NA
    }

    data$qdsports <- (rowSums(data[sportsvars])/4 - 1)*25
  }

  data
}
