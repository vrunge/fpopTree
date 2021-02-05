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



#' Generating a random tree function
#'
#' @description Genarate a list encoding a random tree with n nodes and 1, 2 or 3 children
#' @param n number of nodes in the tree
#' @param proba probability to have 1, 2 or 3 children
#' @return a list in which the element at index i is a vector containing the index/indices of the 1, 2 or 3 child nodes
RandomTree <- function(n, proba = rep(1/3,3))
{
  if(n%%1 != 0){stop("n is not an integer")}
  if(n < 2){stop("A tree with less than 2 nodes is not an interesting tree")}
  if(!all(proba >= 0)){stop("Probabilities must be non negative")}
  if(sum(proba) == 0){stop("Probabilities can not be all zero")}
  k <- 1
  i <- 1
  l_tree <- list()
  while(k < n)
  {
      nb <- sample(1:3, size = 1, prob = proba)
      l_tree[[i]] <- (k+1):(k+nb)
      k <- k + nb
      i <- i + 1
  }
  l_tree[[i-1]] <- (l_tree[[i-1]])[l_tree[[i-1]] <= n]
  for(j in i:n){ l_tree[[j]] <- 0}
  return(l_tree)
}



# internal function
getSubTree <- function(l_tree, roots, i)
{
  roots <- roots[!(roots %in% i)]
  explore <- 1
  subtree <- i
  while(sum(explore > 0))
  {
    ex <- which(explore == 1) #the indices of nodes to explore
    explore[ex] <- 0
    for(j in 1:length(ex))
    {
      stop_index <- unlist(l_tree[subtree[ex[j]]]) %in% c(0, roots)
      if(!all(stop_index))
      {
        explore <- c(explore, c(1,1)[!stop_index])
        subtree <- c(subtree, unlist(l_tree[subtree[ex[j]]])[!stop_index])
      }
    }
  }
  return(sort(subtree))
}

#' Generating a binary tree with changes and gaussian cost
#'
#' @description Genarate a list associated to a binary tree
#' @param l_tree a list encoding a binary tree
#' @param k the number of changes
#' @param roots the position of the changes (roots of the subtrees)
#' @return a list assiciated to the binary tree with changes and edge means
gauss_binaryTree <- function(l_tree, k, roots = NULL)
{
  n <- length(l_tree)
  l_dval <- vector("list", 3) ## Create an empty list

  ### roots ###
  if(is.null(roots) == TRUE)
  {
    roots <- sort(sample(c(2:floor(n/2)), k, replace = FALSE)) ## draw at ramdom the roots of the subtrees among the n/2 first nodes
  }
  else
  {
    k <- length(roots)
  }

  ### mean values ###
  gauss <- sample(c(-k:k), k, replace = FALSE) ## draw at random the mean of the gaussian distributions among integers between -k and k
  values <- rep(0, n) ## Simulate n_tree observations (Gaussian distribution and means  = gauss)

  subTrees <- list()
  for(i in 1:k)
  {
    subTrees[[i]] <- getSubTree(l_tree,roots,roots[i])
  }

  d_val <- rep(0,n)
  for (i in 1:k) ## Create the vector with the observations
  {
    sub <- subTrees[[i]]
    d_val[sub] <- rnorm(length(sub), gauss[i])
  }
  l_dval[[1]] <- d_val
  l_dval[[2]] <- roots
  l_dval[[3]] <- gauss

  return(l_dval)
}


getLeaves <- function(l_tree)
{
  return(which(rowSums(do.call(rbind,l_tree)) == 0))
}

#' Generating the time structure of the tree
#'
#' @description Genarate the time structure of the tree from parents to children
#' @param l_tree A tree encoded in a list describing the parent-to-child structure
#' @return a list in which the position i gives a vector with couples (index, number of children to take in position i-1 in the same list)
tree_time <- function(l_tree)
{
  #build the hight vector = time structure
  hight <- rep(1,length(l_tree))
  for(i in 1:length(l_tree))
  {
    v <- unlist(l_tree[[i]])
    hight[v] <- hight[v] + hight[i]
  }
  hight <- max(hight) - hight + 1

  #group the nodes parent to children
  levels <- vector(mode = "list", length = max(hight))
  for(i in 1:max(hight))
  {
    for(j in which(hight == i))
    {
      nb <- sum(l_tree[[j]] != 0)
      levels[[i]] <- c(levels[[i]], j, nb)
    }
  }
  return(levels)
}
