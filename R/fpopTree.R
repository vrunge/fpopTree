

##  GPL-3 License
## Copyright (c) 2021 Vincent Runge

#' Functional Pruning Optimal Partitioning for data structures in tree
#'
#' @description Functional pruning optimal partitioning with data in a tree structure
#' @param vertex_data vector of data associated to each vertex
#' @param tree tree structure encoded in a list
#' @param type a string defining the cost model to use: "mean", "variance", "poisson", "exp", "negbin"
#' @param weights vector of weights (positive numbers), same size as data
#' @param testMode boolean. False by default. Used to debug the code
#' @return a gfpop object = (changepoints, states, forced, parameters, globalCost)
#' \describe{
#' \item{\code{changepoints}}{is the vector of changepoints (we give the last element of each segment)}
#' \item{\code{states}}{is the vector giving the state of each segment}
#' \item{\code{forced}}{is the vector specifying whether the constraints of the graph are active (=1) or not (=0)}
#' \item{\code{parameters}}{is the vector of successive parameters of each segment}
#' \item{\code{globalCost}}{is a number equal to the total loss: the minimal cost for the optimization problem with all penalty values excluded}
#'  }
fpopTree <- function(vertex_data, tree, type = "mean", weights = NULL, testMode = FALSE)
{

  allowed.types <- c("mean", "variance", "poisson", "exp", "negbin")
  if(!type %in% allowed.types){stop('type must be one of: ', paste(allowed.types, collapse=", "))}

  ### Transform the tree into a temporal structure
  tree_t <- tree_time(tree)

  ###########################
  ### CALL Rcpp functions ###
  ###########################

  res <- fpopTreeTransfer(vertex_data, tree_t, type, weights, testMode)


  return(res)
}

