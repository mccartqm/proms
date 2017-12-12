#' Calculates the pediatric International Knee Documentation Committee (pedi-IKDC)
#' score
#'
#' The \code{pedi_ikdc} function replaces any undefined values with NA before
#' computing the score. The pedi-IKDC score will not be calculated if more than
#' two items are missing. Per the pedi-IKDC scoring instructions, item 12 does not
#' factor into the overall score.
#'
#' @param data A data frame.
#' @param vars A character vector of length 20 specifying the names of
#' the 20 questions that factor into the score.
#'
#' @return This function returns the original data frame provided as input with
#' an additional variable storing the pedi-IKDC score.
#'
#' @examples
#' df <- data.frame(id=1:50,
#'                  q1=sample(0:4, 50, replace = T), q2=sample(0:10, 50, replace = T),
#'                  q3=sample(0:10, 50, replace = T), q4=sample(0:4, 50, replace = T),
#'                  q5=sample(0:4, 50, replace = T), q6=sample(0:4, 50, replace = T),
#'                  q7=sample(0:1, 50, replace = T), q8=sample(0:1, 50, replace = T),
#'                  q9=sample(0:4, 50, replace = T), q10=sample(0:4, 50, replace = T),
#'                  q11a=sample(0:4, 50, replace = T), q11b=sample(0:4, 50, replace = T),
#'                  q11c=sample(0:4, 50, replace = T), q11d=sample(0:4, 50, replace = T),
#'                  q11e=sample(0:4, 50, replace = T), q11f=sample(0:4, 50, replace = T),
#'                  q11g=sample(0:4, 50, replace = T), q11h=sample(0:4, 50, replace = T),
#'                  q11i=sample(0:4, 50, replace = T), q12=rep(0, 50),
#'                  q13=sample(0:10, 50, replace = T))
#'
#' ## item 12 does not factor into the score
#' vars <- names(df)[c(2:20, 22)]
#'
#' df <- pedi_ikdc(df, vars)
#'
#' @export


pedi_ikdc <- function(data, vars){

  # replace undefined values with missing values
  for (i in vars){
    data[[i]][!(is.na(data[[i]]) | data[[i]] %in% c(0,1,2,3,4,5,6,7,8,9,10))] <- NA
  }

  # create a data frame with max number of points for each question
  maxpts <- c(4, 10, 10, 4, 4, 4, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 10)
  maxmatrix <- matrix(rep(maxpts, nrow(data)),
                      ncol = length(maxpts),
                      byrow = T)
  maxmatrix[is.na(data[vars])] <- NA

  # calculate pedi-IKDC score
  data$pediIKDC <- ifelse(rowSums(!is.na(data[vars])) >= 18,
                          (rowSums(data[vars], na.rm = T)/rowSums(maxmatrix, na.rm = T))*100,
                         NA)
  data
}
