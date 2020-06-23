####################################################################
# @file: utility.r
#   Provides general utility functions for use in project scripts
#
# @author: Max Vestrand
#



# Creates a new table for storing results
create_results_table <- function() {
  return (list(acc=data.frame(method=character(), accuracy=double()), pred=list() ))
}

# Updates or creates a new row in the results table
update_results_table <- function(results_table, method_name, pred, accuracy) {
  # Add the predicted values
  results_table$pred[method_name] <- list(pred)
  
  
  results_table$acc <- results_table$acc %>%
    filter(method != method_name) %>%
    rbind(data.table(method=method_name, accuracy=accuracy))
  return (results_table)
}
