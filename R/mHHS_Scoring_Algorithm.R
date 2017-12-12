#' Calculates the modified version of the Harris Hip Score (mHHS).
#'
#' The \code{mhhs} function replaces undefined values with NA.
#'
#' @param data A data frame.
#' @param pain A character vector of length one specifying the name of the pain variable
#' @param limp A character vector of length one specifying the name of the limp variable
#' @param support A character vector of length one specifying the name of the support variable
#' @param distance A character vector of length one specifying the name of the distance variable
#' @param stairs A character vector of length one specifying the name of the stairs variable
#' @param shoes A character vector of length one specifying the name of the shoes variable
#' @param sitting A character vector of length one specifying the name of the sitting variable
#' @param transport A character vector of length one specifying the name of the transport variable
#'
#' @return This function returns the original data frame provided as input with
#'    an additional variable storing the modified HHS score.
#'
#' @importFrom dplyr left_join
#' @import magrittr
#'
#' @examples df <- mhhs(df, "hhs1r", "hhs2r", "hhs3r", "hhs4r", "hhs5r", "hhs6r", "hhs7r", "hhs8r")
#'
#' @export

mhhs <- function(data, pain, limp, support, distance, stairs, shoes, sitting, transport){

  mhhsvars <- c(pain, limp, support, distance, stairs, shoes, sitting, transport)

  # replace undefined values with missing values
  for (i in mhhsvars){
    data[[i]][!(is.na(data[[i]]) | data[[i]] %in% c(1,2,3,4,5,6,7,8))] <- NA
  }

  itemScores <- left_join(data, pain_info[, c("pain", "pain_score")], by = setNames("pain", pain)) %>%
    left_join(limp_info[, c("limp", "limp_score")], by = setNames("limp", limp)) %>%
    left_join(support_info[, c("support", "support_score")], by = setNames("support", support)) %>%
    left_join(distance_info[, c("distance", "distance_score")], by = setNames("distance", distance)) %>%
    left_join(stairs_info[, c("stairs", "stairs_score")], by = setNames("stairs", stairs)) %>%
    left_join(shoes_info[, c("shoes", "shoes_score")], by = setNames("shoes", shoes)) %>%
    left_join(sitting_info[, c("sitting", "sitting_score")], by = setNames("sitting", sitting)) %>%
    left_join(transport_info[, c("transport", "transport_score")], by = setNames("transport", transport))

  data$mHHS <- rowSums(itemScores[, c("pain_score", "limp_score",
                                      "support_score", "distance_score",
                                      "stairs_score", "shoes_score",
                                      "sitting_score", "transport_score")], na.rm = T)
  data
}
