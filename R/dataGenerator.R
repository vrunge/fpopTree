#' Generating binary tree function
#'
#' @description Genarate a list encoding a binary tree
#' @param n number of nodes in the tree
#' @return a list in which the element at index i is a vector containing the index/indices of the 0, 1 or 2 child nodes
BinaryTree <- function(n)
{
  if(n%%1 != 0){stop("n is not an integer")}
  if(n < 2){stop("A tree with less than 2 nodes is not an interesting tree")}

  m <- floor(n/2)
  z <- 1:m

  matrix_child <- cbind(2*z, 2*z + 1)
  if(n %% 2 == 0){matrix_child[m, 2] <- 0}
  matrix_leafs <- matrix(0, nrow = n-m, ncol = 2)
  tree <- rbind(matrix_child, matrix_leafs)

  #generate a list
  l_tree <- apply(tree, 1, FUN = function(x)
                                {
                                  a <- x[x!=0]
                                  if(length(a)  == 0) a <- c(0)
                                  return(a)
                                }
                  )
  return(l_tree)
}


