######################################################
#
#
# Analysis script
# 
# Written by Mason Fidino
#
#
#
#

# This is just the base script to run a bunch of jags models. Not all
# of the models are currently up on github, and the win.data will need
# to be changed for a few of these models.


# for full model

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_housing_g = runif(1,0, 3),
              mu_housing_p = runif(1,0, 3),
              mu_area_g = runif(1,0, 3),
              mu_area_p = runif(1,0, 3),
              mu_canopy_g = runif(1,0, 3),
              mu_canopy_p = runif(1,0, 3),
              mu_prox_g = runif(1,0, 3),
              mu_prox_p = runif(1,0, 3),
              sigma_housing_g = runif(1,0, 3),
              sigma_housing_p = runif(1,0, 3),
              sigma_area_g = runif(1,0, 3),
              sigma_area_p = runif(1,0, 3),
              sigma_canopy_g = runif(1,0, 3),
              sigma_canopy_p = runif(1,0, 3),
              sigma_prox_p = runif(1,0, 3),
              sigma_prox_g = runif(1,0, 3),
              size_g = runif(1, -3, 3))
              
}

params <- c( "z",
              "gam", "phi", 
             "mu_housing_g",
             "sigma_housing_g",
             "mu_housing_p",
             "sigma_housing_p",
             "mu_area_g",
             "sigma_area_g",
             "mu_area_p",
             "sigma_area_p",
             "mu_canopy_g",
             "sigma_canopy_g",
             "mu_canopy_p",
             "sigma_canopy_p",
             "mu_prox_g",
             "sigma_prox_g",
             "mu_prox_p",
             "sigma_prox_p",
             "psi_in",
             "p",
             "size_g")

ocmod <- jags.model(file = "all_covars.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m1 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5 )

m1_aic <- calc_waic(m1, win.data)

# save the output as an R object

saveRDS(m1, "m1.txt")

# remove the object so we don't fill up all the space

rm(m1)

######################################################################

# now do area, prox, housing, and size

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_housing_g = runif(1,-3, 3),
              mu_housing_p = runif(1,-3, 3),
              mu_area_g = runif(1,-3, 3),
              mu_area_p = runif(1,-3, 3),
              mu_prox_g = runif(1,-3, 3),
              mu_prox_p = runif(1,-3, 3),
              sigma_housing_g = runif(1,0, 3),
              sigma_housing_p = runif(1,0, 3),
              sigma_area_g = runif(1,0, 3),
              sigma_area_p = runif(1,0, 3),
              sigma_prox_g = runif(1,0, 3),
              sigma_prox_p = runif(1,0, 3),
              size_g = runif(1, 0, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_housing_g",
             "sigma_housing_g",
             "mu_housing_p",
             "sigma_housing_p",
             "mu_area_g",
             "sigma_area_g",
             "mu_area_p",
             "sigma_area_p",
             "mu_prox_g",
             "sigma_prox_g",
             "mu_prox_p",
             "sigma_prox_p",
             "psi_in",
             "p",
             "size_g")

ocmod <- jags.model(file = "area_prox_housing_size.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m2 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)

m2_aic <- calc_waic(m2, win.data)

# save the output as an R object

saveRDS(m2, "m2.txt")

# remove the object so we don't fill up all the space

rm(m2)


######################################################################

# now do area, prox, and housing

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_housing_g = runif(1,-3, 3),
              mu_housing_p = runif(1,-3, 3),
              mu_area_g = runif(1,-3, 3),
              mu_area_p = runif(1,-3, 3),
              mu_prox_g = runif(1,-3, 3),
              mu_prox_p = runif(1,-3, 3),
              sigma_housing_g = runif(1,0, 3),
              sigma_housing_p = runif(1,0, 3),
              sigma_area_g = runif(1,0, 3),
              sigma_area_p = runif(1,0, 3),
              sigma_prox_g = runif(1,0, 3),
              sigma_prox_p = runif(1,0, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_housing_g",
             "sigma_housing_g",
             "mu_housing_p",
             "sigma_housing_p",
             "mu_area_g",
             "sigma_area_g",
             "mu_area_p",
             "sigma_area_p",
             "mu_prox_g",
             "sigma_prox_g",
             "mu_prox_p",
             "sigma_prox_p",
             "psi_in",
             "p")


ocmod <- jags.model(file = "area_prox_housing.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m3 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)

# save the output as an R object

m3_aic <- calc_waic(m3, win.data)

saveRDS(m3, "m3.txt")

# remove the object so we don't fill up all the space

rm(m3)

#########################################################

# just do area and prox

# area

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_area_g = runif(1,-3, 3),
              mu_area_p = runif(1,-3, 3),
              mu_prox_g = runif(1,-3, 3),
              mu_prox_p = runif(1,-3, 3),
              sigma_area_g = runif(1,0, 3),
              sigma_area_p = runif(1,0, 3),
              sigma_prox_g = runif(1,0, 3),
              sigma_prox_p = runif(1,0, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_area_g",
             "sigma_area_g",
             "mu_area_p",
             "sigma_area_p",
             "mu_prox_g",
             "sigma_prox_g",
             "mu_prox_p",
             "sigma_prox_p",
             "psi_in",
             "p")

ocmod <- jags.model(file = "area_prox.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)

m4<- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)

m4_aic <- calc_waic(m4, win.data)


# save the output as an R object

saveRDS(m4, "m4.txt")

# remove the object so we don't fill up all the space

rm(m4)


#########################################################

# just going to do some single models now

# canopy

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_canopy_g = runif(1,-3, 3),
              mu_canopy_p = runif(1,-3, 3),
              sigma_canopy_g = runif(1,0, 3),
              sigma_canopy_p = runif(1,0, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_canopy_g",
             "sigma_canopy_g",
             "mu_canopy_p",
             "sigma_canopy_p",
             "psi_in",
             "p")

ocmod <- jags.model(file = "canopy2.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m5 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)

m5_aic <- calc_waic(m5, win.data)

# save the output as an R object

saveRDS(m5, "m5.txt")

# remove the object so we don't fill up all the space

rm(m5)

##########################################

# area

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_area_g = runif(1,-3, 3),
              mu_area_p = runif(1,-3, 3),
              sigma_area_g = runif(1,0, 3),
              sigma_area_p = runif(1,0, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_area_g",
             "sigma_area_g",
             "mu_area_p",
             "sigma_area_p",
             "psi_in",
             "p")

ocmod <- jags.model(file = "area.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m6 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)


m6_aic <- calc_waic(m6, win.data)

# save the output as an R object

saveRDS(m6, "m6.txt")

# remove the object so we don't fill up all the space

rm(m6)

##########################################

# housing

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_housing_g = runif(1,-3, 3),
              mu_housing_p = runif(1,-3, 3),
              sigma_housing_g = runif(1,0, 3),
              sigma_housing_p = runif(1,0, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_housing_g",
             "sigma_housing_g",
             "mu_housing_p",
             "sigma_housing_p",
             "psi_in",
             "p")

ocmod <- jags.model(file = "housing.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m7 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)

m7_aic <- calc_waic(m7, win.data)

# save the output as an R object

saveRDS(m7, "m7.txt")

# remove the object so we don't fill up all the space

rm(m7)

############################################

# prox

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_prox_g = runif(1,-3, 3),
              mu_prox_p = runif(1,-3, 3),
              sigma_prox_g = runif(1,0, 5),
              sigma_prox_p = runif(1,0, 5))
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_prox_g",
             "sigma_prox_g",
             "mu_prox_p",
             "sigma_prox_p",
             "psi_in",
             "p")

ocmod <- jags.model(file = "prox.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m8 <- coda.samples(ocmod, n.iter = 2000, variable.names = params)

m8_aic <- calc_waic(m8, win.data)

# save the output as an R object

saveRDS(m8, "m8.txt")

# remove the object so we don't fill up all the space

rm(m8)

################################################

# size

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              size_g = runif(1, -3, 3))
  
}

params <- c( "z",
             "gam", "phi", 
             "psi_in",
             "p",
             "size_g")

ocmod <- jags.model(file = "size.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 20000
update(ocmod, n.iter = nburn)
m9 <- coda.samples(ocmod, n.iter = 20000, variable.names = params, thin = 10)

m9_aic <- calc_waic(m9, win.data)

# save the output as an R object

saveRDS(m9, "m9.txt")

# remove the object so we don't fill up all the space

rm(m9)

############################################################

# prox and housing

inits <- function() { # specify initial values
  list(       psi_in = runif(5, 0.001, .999), z = zinit,
              mu_prox_g = runif(1,-3, 3),
              sigma_prox_g = runif(1,0, 5),
              sigma_prox_p = runif(1,0, 5),
              mu_housing_g = runif(1, -3, 3),
              sigma_housing_g = runif(1, 0, 5),
              sigma_housing_p = runif(1, 0, 5)
              )
  
}

params <- c( "z",
             "gam", "phi", 
             "mu_prox_g",
             "sigma_prox_g",
             "sigma_prox_p",
             "mu_housing_g",
             "sigma_housing_g",
             "sigma_housing_p",
             "psi_in",
             "p")

ocmod <- jags.model(file = "prox_housing.txt", 
                    inits = inits, data = win.data, n.chains = 2)
nburn <- 10000
update(ocmod, n.iter = nburn)
m10 <- coda.samples(ocmod, n.iter = 10000, variable.names = params, thin = 5)

m10_aic <- calc_waic(m10, win.data)

m10_sum <- summary(m10)
# save the output as an R object

saveRDS(m10, "m10.txt")

# remove the object so we don't fill up all the space

rm(m9)
