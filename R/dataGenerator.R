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

getSubTree <- function(l_tree, roots, i)
{
  explore <- 1
  subtree <- i
  while(sum(explore > 0))
  {
    print(paste("explore ", paste(explore, collapse = " ")))
    print(paste("subtree ", paste(subtree, collapse = " ")))
    expo <- which(explore == 1)
    explore[explore == 1] <- 0
    for(j in length(expo))
    {
      stop_index <- l_tree[expo[j]] %in% c(0, roots)
      if(!all(stop_index))
      {
        explore <- c(explore, c(1,1)[!stop_index])
        subtree <- c(subtree, unlist(l_tree[expo[j]][!stop_index]))
      }
    }
  }

  return(sort(subtree))
}


dval_gauss <- function(l_tree, k)
{
  n <- length(l_tree)
  l_dval <- vector("list", 3) ## Create an empty list

  roots <- sort(sample(c(2:floor(n/2)), k, replace = FALSE)) ## draw at ramdom the roots of the subtrees among the n/2 first nodes
  gauss <- sample(c(-k:k), k + 1, replace = FALSE) ## draw at random the mean of the gaussian distributions among integers between -k and k
  values <- rep(0, n) ## Simulate n_tree observations (Gaussian distribution and means  = gauss)

  subTrees <- list()
  for(i in 1:k)
  {
    subTrees[[i]] <- getSubTree(l_tree,roots,i)
  }


    for (i in 1:k) ## Create the vector with the observations
    {
      sub <- subTrees[[i]]
      d_val[sub] <- rnorm(length(sub), gauss[i])
    }
    l_dval[[1]] = d_val
    l_dval[[2]] = roots
    l_dval[[3]] = gauss


  return(l_dval)
}


## This function simulate Gaussian observations according to a random partition
## on the nodes of a tree l_tree.
## n_tree is the size of the tree and k+1 the number of subtrees in the random partition
## Output a list :
## vector of size n_tree with the observations
## The roots of the sub-trees in the random partition
## the mean of the gaussian distribution on the sub-trees

## TODO: optimize this function
## split root generation and dval generation
dval_gauss <- function(l_tree, k)
{
  n_tree <- length(l_tree)
  if(k > 0){
    stop <- F  ## stop criterion
    while(stop == F){ ## while STOP is false continue
      stop <- T ## stop is set to true a priori
      l_dval = vector("list", 3) ## Create an empty list
      roots <- sort(sample(c(2:round(n_tree/2)), k, replace = FALSE)) ## draw at ramdom the roots of the subtrees among the n_tree/2 first nodes
      gauss <- sample(c(-k:k), k + 1, replace = FALSE) ## draw at random the mean of the gaussian distributions among integers between -k and k
      d_val <- rnorm(n_tree, gauss[k + 1], 1) ## Simulate n_tree observations (Gaussian distribution and means  = gauss)
      subTrees <- getSubtrees(l_tree, roots) ## Get the subTrees
      for (i in c(1:k)) ## Remove the redondant nodes from the sub-trees
      {
        for (j in c((i + 1):(k + 1)))
        {
          subTrees[[i]] <- subTrees[[i]][!subTrees[[i]] %in% subTrees[[j]]]

        }
      }

      for (i in c(1:k)){ ## Make sure that the sub-trees are of size 3 at minimum
        sub <- subTrees[[i]]
        if (length(sub) <3) stop <- F
      }
    }

    for (i in c(1:k)) ## Create the vector with the observations
    {
      sub <- subTrees[[i]]
      d_val[sub] <- rnorm(length(sub), gauss[i], 1)
    }
    l_dval[[1]] = d_val
    l_dval[[2]] = roots
    l_dval[[3]] = gauss
  } else { ## NO CHANGE
    l_dval = vector("list", 3)
    l_dval[[1]] = rnorm(n_tree)
    l_dval[[2]] = c()
    l_dval[[3]] = c(0)
  }
  return(l_dval)
}

