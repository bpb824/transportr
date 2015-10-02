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
