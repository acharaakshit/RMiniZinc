#' setup the constraints of the model. structure of output should look like constraint 〈Boolean expression〉;
#' @export
#' @param  model the oath of the model_mzn file
#' @param constraint_list list of the constraints of the model

setup_constraints <- function(model,constraint_list){
  for (x in seq(1,length(constraint_list),1)) {
    write(paste0("constraint ",as.character(constraint_list[x]),";","\n"), model, append = TRUE)
  }
}
