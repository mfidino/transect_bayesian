####################
#
#
# Model preperation script
#
# By Mason Fidino
#
#   This script includes all of the 'preperatory' data manipulation necessary
#   for running the most basic of our hierarchical models.  Current functionality
#   includes covariates, be they temporally varying or not, in continous or
#   discrete format.





nspec = 5     # number of species
nyear = 10    # number of seasons (10 'years')
nsite = 118    # number of sites

# read in covariate data in the working directory
cov_dat <- read.table("cov_dat_1k_hous.txt", header = TRUE, sep = "\t")
cov_dat[80,2] <- 0.0001
# look at data and determine if we need to sqrt it

# do some scaling of the data

cov_dat$area <- scale(log(cov_dat$area))
cov_dat$housing <- scale(sqrt(cov_dat$housing))
cov_dat$canopy <-scale(sqrt(cov_dat$canopy))
cov_dat$prox <- scale(sqrt(cov_dat$prox))



big_array <- readRDS("../../../R/bayes/bayes/big_array.txt") # read in presence data

# numner of observations at a site does not change between species

n_obs <- big_array[[1]]

jmat <- make_jmat(n_obs, nspec, nsite, nyear)

ymat <- make_ymat(big_array, nspec, nsite, nyear)


zinit <- initial_z(ymat, nspec, nsite, nyear)

# temperature related stuff
the_temp <- c(52,68,45,17.5,43,71,45,26,46,69)

temp_mat <- matrix(rep(the_temp / (sd(the_temp)*2), each = 118), ncol = 10)
# seasonal stuff as dummy regressors
seasons <- data.frame(spring = c(1,0,0,0,1,0,0,0,1,0),
                      summer = c(0,1,0,0,0,1,0,0,0,1),
                      fall =   c(0,0,1,0,0,0,1,0,0,0))

win.data <- list(y=ymat, site=site, nspec = nspec, nyear = nyear, 
                 cov = cov_dat, jmat = jmat, temp = temp_mat, season = seasons)





