
#' Calculates the disability/symptom section and the optional high performance 
#' sport/music or work modules of the QuickDASH.
#' 
#' @param data A data frame.
#' @param varnames A character vector of length eleven specifying the names of
#' the eleven questions used to calculate the disability/symptom score.
#' 
#' @return This function returns the original data frame provided as input with
#' an additional variable storing the QuickDASH score.
#' 
#' @example df <- data.frame(id = 1:200,
#' q1 = sample(1:5, 200, replace = T), q2 = sample(1:5, 200, replace = T),
#' q3 = sample(1:5, 200, replace = T), q4 = sample(1:5, 200, replace = T),
#' q5 = sample(1:5, 200, replace = T), q6 = sample(1:5, 200, replace = T),
#' q7 = sample(1:5, 200, replace = T), q8 = sample(1:5, 200, replace = T),
#' q9 = sample(1:5, 200, replace = T), q10 = sample(1:5, 200, replace = T),
#' q11 = sample(1:5, 200, replace = T))
#' 
#' df <- quickdash(df, names(df)[2:11])
#' 
#' @export

quickdash <- function(data, varnames){
  
  # replace undefined values with missing values
  for (i in varnames){
    data[[i]][!(is.na(data[[i]]) | data[[i]] %in% c(1,2,3,4,5))] <- NA
  }
  
  data$qdscore <- ifelse(rowSums(!is.na(data[varnames])) >= 10, (rowSums(data[varnames], na.rm = T)/rowSums(!is.na(data[varnames])) - 1)*25, NA)
  
  data
}