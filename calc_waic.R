####################################################
#
#
#
# Function to calculate WAIC of a model
#
#  initial code from https://github.com/mbjoseph/MLM_EcologyInSilico/blob/master/calc_waic.R
#
#  Modified to fit dynamic occupancy models by Mason Fidino 2/16/2015
#
#

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     calc_waic   calc_waic   calc_waic   calc_waic   calc_waic   calc_waic
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Contract.
  
  # This function requires a jags model output in the posterior argument
  # and the initial data in the data argument. This would be whatever
  # you put into the 'data' argument of the function jags.model.
  # Finally, you need to specify the number of species, number of seasons
  # and number of sites.

  # Returns a list of length 4 that includs the log posterior predictive density,
  # p_waic, waic, and the mean likelihood in that order.


nspec = 5   # Provided specifically for our project
nyear = 10  # Provided specifically for our project
nsite = 118 # Provided specifically for our project



calc_waic <- function(posterior, data, nspec, nyear, nsite){
  if(missing(posterior)|missing(data)|missing(nspec)|missing(nsite)|missing(nyear)){
    stop("Need to include all arguments in this function.\n",
         "\t\tCheck calc_waic.R and read calc_waics contract.")
  }
  with(data,{
    chains <- length(posterior)
    store <- dim(posterior[[1]])[1]
    L <- array(dim=c(site,nspec, nyear,  chains, store))
    L_bar <- array(NA, dim = c(nsite, nspec, nyear))
    var_LL <- array(NA, dim = c(nsite, nspec, nyear))
    
    pb <- txtProgressBar(min = 1, max = nyear, style = 3)
    
    for (i in 1:nyear){
      for(k in 1:nspec){
        for(w in 1:nsite){
      for (j in 1:chains){

        post_sims <- posterior[[j]]
        indx_z <- which(dimnames(post_sims)[[2]] == 
                          paste("z[",w,",", k,",",i, "]", sep=""))
        zvals <- post_sims[, indx_z]
        indx_p <- which(dimnames(post_sims)[[2]] == 
                          paste("p[", k, "]", sep=""))
        pvals <- post_sims[, indx_p]
        L[w,k,i, j, ] <- dbinom(rep(y[k, w, i], store), 
                            size=jmat[k, w, i], 
                            prob = zvals * pvals, 
                            log=TRUE)
      }
      L_bar[w,k,i] <- mean(exp(c(L[w,k,i,, ])), na.rm=TRUE)
      var_LL[w,k,i] <- var(c(L[w,k,i, , ]), na.rm=TRUE)
      setTxtProgressBar(pb, i)
    }}}
    
    lppd <- sum(log(L_bar), na.rm=TRUE)
    p_WAIC <- sum(var_LL, na.rm=TRUE)
    WAIC <- -2 * (lppd - p_WAIC)
    return(list(lppd=lppd, p_waic=p_WAIC, waic=WAIC, L_bar))
  })
}

##################################################################################
##################################################################################
##################################################################################