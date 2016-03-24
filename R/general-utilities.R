#' Converts Two-Level List to Data Frame
#'
#' @param list List with One level of sub-lists (sub-lists cannot contain lists)
#'
#' @return Data frame
#' @export
list2frame = function(list){
  frame = data.frame(matrix(nrow=length(list),ncol = length(list[[1]])))
  colnames(frame)= names(list[[1]])
  for (i in 1:length(list)){
    frame[i,] = unlist(list[[i]])
  }
  return(frame)
}

#' Calculate natural breaks for specified fields in table
#'
#' This function will calculate natural (Jenks) breaks for each specified field in the input table and return a table of the break
#' points based on the number of breaks desired.
#'
#' @param table The data frame or matrix object with the data and fields you need to calculate breaks for
#' @param fields A vector of character strings corresponding to the fields you need to calculate breaks for
#' @param numBreaks The number of break points to calculate
#'
#' @return Data frame with break points for each field specified
#' @export
#'
naturalBreaks = function(table,fields,numBreaks){
  result = data.frame(matrix(nrow=numBreaks+1,ncol=length(fields)))
  colnames(result)=fields
  for(i in 1:length(fields)){
    field = fields[i]
    classified = classInt::classIntervals(as.numeric(unlist(table[,field])),style="jenks", n = numBreaks)
    breaks = classified$brks
    result[,field]=breaks
  }
  return(result)
}
