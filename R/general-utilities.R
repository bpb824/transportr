#' Expand ID Column Vector
#'
#' @param id_vec The vector of unique IDs you would like to 'expand' (i.e. turn [1,2] into [1,1,2,2])
#' @param n_rep The number of repetitions in the 'expansion'
#'
#' @return Expanded vector
#' @export
expand_id_col = function(id_vec,n_rep){
  new_col = vector(length=length(id_vec)*n_rep)
  x = 1
  for(i in 1:length(id_vec)){
    new_col[x:(x+n_rep-1)]=id_vec[i]
    x=x+n_rep
  }
  return(new_col)
}


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

#' Wrap long strings
#'
#' Wraps long strings onto multiple lines for plotting
#'
#' @param stringVector Vector of strings to split onto multiple lines
#' @param width Line width to trim strings to (in number of characters)
#'
#' @return Returns strings split onto multiple lines if they exceed the width parameter.
#'
#' @export
#'
#' @author RickyB
#' @references \url{http://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles}
#'
wrap_sentance <- function(stringVector, width) {
  output = vector()
  for (j in 1:length(stringVector)){
    words <- unlist(strsplit(stringVector[j], " "))
    fullsentence <- ""
    checklen <- ""
    for(i in 1:length(words)) {
      checklen <- paste(checklen, words[i])
      if(nchar(checklen)>(width+1)) {
        fullsentence <- paste0(fullsentence, "\n")
        checklen <- ""
      }
      fullsentence <- paste(fullsentence, words[i])
    }
    fullsentence <- sub("^\\s", "", fullsentence)
    fullsentence <- gsub("\n ", "\n", fullsentence)
    output[j]=fullsentence
  }
  return(output)
}
