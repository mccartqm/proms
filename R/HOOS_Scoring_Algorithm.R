#' Calculates the Hip Disability and Osteoarthritis Outcomes Survey subscale scores (HOOS).
#'
#' The \code{hoos} function first replaces any undefined values with NA before
#' computing the subscale scores. At least half of the items in each subscale
#' require a response in order for that subscale score to be calculated.
#'
#' @param data A data frame
#' @param symptomvars A character vector of length 5 specifying the names of the
#' symptom variables (which include symptoms and stiffness)
#' @param painvars A character vector of length 10 specifying the names of the
#' pain variables
#' @param adlvars A character vector of length 17 specifying the names of the
#' function in daily living variables
#' @param sportvars A character vector of length 4 specifying the names of the
#' function in sport and recreation variables
#' @param qolvars A character vector of length 4 specifying the names of the
#' hip-related quality of life variables
#'
#' @return This function returns the original data frame provided as input with
#'    five additional variables storing each subscale score.
#'
#' @examples
#' fai <- read.csv("FAI test data.csv", stringsAsFactors = F)
#'
#' hoosvars <- names(fai)[c(31:109)]
#' symptoms <- names(fai)[seq(31, 39, 2)]
#' pain <- names(fai)[c(41, 59, 2)]
#' adl <- names(fai)[c(61, 93, 2)]
#' sport <- names(fai)[c(95, 101, 2)]
#' qol <- names(fai)[c(103, 109, 2)]
#'
#' library(dplyr)
#' test <- cbind.data.frame(select(fai, -(hoossy1r:hoosq4l)),
#'                          apply(fai[hoosvars],
#'                                2,
#'                                function(x) ifelse(x==1, 0,
#'                                                   ifelse(x==2, 1,
#'                                                          ifelse(x==3, 2,
#'                                                                 ifelse(x==4, 3,
#'                                                                        ifelse(x==5, 4,
#'                                                                               x)))))))
#' df <- hoos(df, symptoms, pain, adl, sport, qol)
#'
#' @export

hoos <- function(data, symptomvars, painvars, adlvars, sportvars, qolvars){

  hoosdf <- cbind.data.frame(data[, !names(data) %in% c(symptomvars, painvars, adlvars, sportvars, qolvars)],
                             apply(data[c(symptomvars, painvars, adlvars, sportvars, qolvars)], 2,
                                   function(x) ifelse(!(is.na(x) | x %in% c(0,1,2,3,4)), NA, x)))

  # count number of nonmissing items for each subscale
  hoosdf$HoosSyN <- rowSums(!is.na(hoosdf[symptomvars]))
  hoosdf$HoosPnN <- rowSums(!is.na(hoosdf[painvars]))
  hoosdf$HoosAdlN <- rowSums(!is.na(hoosdf[adlvars]))
  hoosdf$HoosSpN <- rowSums(!is.na(hoosdf[sportvars]))
  hoosdf$HoosQolN <- rowSums(!is.na(hoosdf[qolvars]))

  # score outcomes
  hoosdf$HoosSy <- ifelse(hoosdf$HoosSyN >= 3, 100 - 100*rowSums(hoosdf[symptomvars], na.rm = T)/(4*hoosdf$HoosSyN), NA)
  hoosdf$HoosPn <- ifelse(hoosdf$HoosPnN >= 5, 100 - 100*rowSums(hoosdf[painvars], na.rm = T)/(4*hoosdf$HoosPnN), NA)
  hoosdf$HoosAdl <- ifelse(hoosdf$HoosAdlN >= 9, 100 - 100*rowSums(hoosdf[adlvars], na.rm = T)/(4*hoosdf$HoosAdlN), NA)
  hoosdf$HoosSp <- ifelse(hoosdf$HoosSpN >= 2, 100 - 100*rowSums(hoosdf[sportvars], na.rm = T)/(4*hoosdf$HoosSpN), NA)
  hoosdf$HoosQol <- ifelse(hoosdf$HoosQolN >= 2, 100 - 100*rowSums(hoosdf[qolvars], na.rm = T)/(4*hoosdf$HoosQolN), NA)

  hoosdf[, !names(hoosdf) %in% c("HoosSyN", "HoosPnN", "HoosAdlN", "HoosSpN", "HoosQolN")]
}
