#' Calculates the Hip Disability and Osteoarthritis Outcomes Survey subscale scores (HOOS).
#'
#' The \code{hoos} function first replaces any undefined values with NA before
#' computing the subscale scores. At least 50% of the items in each subscale
#' require a response in order for the subscale score to be calculated.
#'
#' @param data A data frame.
#' @param symptomVars A character vector of length 5 specifying the name of the pain variable
#' @param painVars A character vector of length 10 specifying the name of the limp variable
#' @param adlVars A character vector of length 17 specifying the name of the support variable
#' @param sportVars A character vector of length 4 specifying the name of the distance variable
#' @param qolVars A character vector of length 4 specifying the name of the stairs variable
#'
#' @return This function returns the original data frame provided as input with
#'    five additional variables storing each subscale score.
#'
#' @examples df <- hoos(df, symptoms, pain, adl, sport, qol)
#'
#' @export

hoos <- function(data, symptomVars, painVars, adlVars, sportVars, qolVars){

  hoosdf <- cbind.data.frame(data[, !names(data) %in% c(symptomVars, painVars, adlVars, sportVars, qolVars)],
                             apply(data[c(symptomVars, painVars, adlVars, sportVars, qolVars)], 2,
                                   function(x) ifelse(!(is.na(x) | x %in% c(0,1,2,3,4)), NA, x)))

  # count number of nonmissing items for each subscale
  hoosdf$HoosSyN <- rowSums(!is.na(hoosdf[symptomVars]))
  hoosdf$HoosPnN <- rowSums(!is.na(hoosdf[painVars]))
  hoosdf$HoosAdlN <- rowSums(!is.na(hoosdf[adlVars]))
  hoosdf$HoosSpN <- rowSums(!is.na(hoosdf[sportVars]))
  hoosdf$HoosQolN <- rowSums(!is.na(hoosdf[qolVars]))

  # score outcomes
  hoosdf$HoosSy <- ifelse(hoosdf$HoosSyN >= 3, 100 - 100*rowSums(hoosdf[symptomVars], na.rm = T)/(4*hoosdf$HoosSyN), NA)
  hoosdf$HoosPn <- ifelse(hoosdf$HoosPnN >= 5, 100 - 100*rowSums(hoosdf[painVars], na.rm = T)/(4*hoosdf$HoosPnN), NA)
  hoosdf$HoosAdl <- ifelse(hoosdf$HoosAdlN >= 9, 100 - 100*rowSums(hoosdf[adlVars], na.rm = T)/(4*hoosdf$HoosAdlN), NA)
  hoosdf$HoosSp <- ifelse(hoosdf$HoosSpN >= 2, 100 - 100*rowSums(hoosdf[sportVars], na.rm = T)/(4*hoosdf$HoosSpN), NA)
  hoosdf$HoosQol <- ifelse(hoosdf$HoosQolN >= 2, 100 - 100*rowSums(hoosdf[qolVars], na.rm = T)/(4*hoosdf$HoosQolN), NA)

  hoosdf[, !names(hoosdf) %in% c("HoosSyN", "HoosPnN", "HoosAdlN", "HoosSpN", "HoosQolN")]
}
