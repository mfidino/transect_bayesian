#############################
#
#
# Functions to prepare matrices for analysis
#
#
#
#

###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     make_jmat   make_jmat   make_jmat   make_jmat   make_jmat   make_jmat          
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### contract

  # makes the j matrix, requires the first level of the species array
  # i.e. n_obs <- big_array[[1]], the number of species, number of sites,
  # and number of seasons.

  # n_obs is sorted as a 3 dimensional array [# site, # sample days each season,
  # # seasons]

  # returns a a 3 dimensional array[# species, # sites, # years]

make_jmat <- function(n_obs = NULL, nspec = NULL, nsite = NULL, nyear = NULL){
  if(missing(n_obs)|missing(nspec)|missing(nsite)|missing(nyear)){
    stop("Need to include all arguments in this function.\n",
         "\t\tCheck prep_functions.R and read make_jmats contract.")
  }
  jmat <- array(0, dim = c(nspec, nsite, nyear))
  for(i in 1:nsite){
    for(j in 1:nyear){
      jmat[,i,j] <- 28 - length(which(is.na(n_obs[i,,j])==TRUE))
    }
  }
  return(jmat)
}

##################################################################################
##################################################################################



###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     make_ymat   make_ymat   make_ymat   make_ymat   make_ymat   make_ymat
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Contract

  # makes the y matrix, uses the raw_data array, which is the list of multiple
  # arrays sorted as [# site, # days in season, # seasons].
  # also requires specifications for array size of y matrix
  # as the last three agruments

make_ymat <- function(raw_data = NULL, nspec = NULL, nsite = NULL, nyear = NULL){
  
  if(missing(raw_data)|missing(nspec)|missing(nsite)|missing(nyear)){
    stop("Need to include all arguments in this function.\n",
         "\t\tCheck prep_functions.R and read make_ymats contract.")
  }
  ymat <- array(0, dim = c(nspec, nsite, nyear))
  
  for(i in 1:nspec){
    for(j in 1:nsite){
      for(t in 1:nyear){
        ymat[i,j,t] <- sum(raw_data[[i]][j,,t], na.rm=TRUE)
        if(sum(is.na(raw_data[[i]][j,,t]))==28){
          ymat[i,j,t] <- NA
        }
      }
    }
  }
  return(ymat)
}

# calculate inital z matrix from the y matrix

initial_z <- function(ymat, nspec, nsite, nyear){
  zinit <- array(dim = c(site, nspec, nyear))
  
  for (j in 1:site) {
    for (i in 1:nspec) {
      for (t in 1:nyear) {
        zinit[j, i, t] <- ymat[i, j, t ] # change to one if greater than.
      }
    }
  }
  zinit[zinit>0] <- 1
  return(zinit)
  
}
  
  
 #ba
