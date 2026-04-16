# Fait par Melinda Krouri et Charlie Oudinot
# TER Réassurance

# importing libraries
library(stats) # statistic functions
library(dplyr) # table manipulation
library(Lmoments)
library(distillery)
library(extRemes) # functions and datasets of extreme climatic events
library(evir)
library(evd)

# import damage dataset
data(damage) 

# cleaning table
dt = damage[, 2:3]

# graph of the total loss caused by the biggest hurricane of the year
plot.new()
plot(damage$Year,damage$Dam, type='h', xlab='Année', ylab='Pertes totales')


#######################################

# block maxima : we group obs by year and take the max for each year
dt_max = dt %>% group_by(Year) %>% summarise(Annual_loss = max(Dam))
dt_max = rbind(dt_max, data.frame(Year=c(1925, 1927, 1930, 1931, 1937, 1939, 1958),
                         Annual_loss=c(0, 0, 0, 0, 0, 0, 0))) # we add 0 when no hurricane
dt_max = dt_max[order(dt_max$Year),] # order by year

# histogram
hist(dt_max$Annual_loss, breaks = 15, xlab = "Pertes  annuelles", ylab = "Fréquence", main = "")

# estimate the parameters
gev1 = gev(dt_max$Annual_loss, 1) # create a GEV object and calculate the estimates of the parameters
xi_h = gev1$par.ests[1]
sigma_h = gev1$par.ests[2]
mu_h = gev1$par.ests[3]

# plot the estimated distribution
x = seq(-0.5, 1.75, by=0.0005)
plot.new()
plot(x,
     dgev(x,loc = mu_h, scale = sigma_h, shape = xi_h),
     ylim = c(0,5),
     ylab = 'g(x)',
     type = 'l'
     )
grid(nx = 5,ny = 5, col = 'grey')

# plot QQ-plot and residuals
plot.gev(gev1)

# compute the mode with estimated parameters
mu_h + sigma_h*(((log(2))**(-xi_h) - 1)/xi_h)



#######################################

# import dataset
data(danish)

# histogram of data before threshold
hist(danish, breaks=60,
     xlim = c(0,50),
     xlab = 'Perte',
     ylab = 'Fréquence',
     main = "")

# Hill estimator to check if danish is heavy-tailed
hill(danish, option = 'xi')

# Mean Residual Life Plot
mrlplot(danish)

# POT : we keep observations that are above the threshold u
# and we estimate the parameters
u = 3.73 # threshold
gpd1 = gpd(data = danish, threshold = u, method = 'ml')
xi_h = gpd1$par.ests[1]
beta_h = gpd1$par.ests[2]

# histogram of data after threshold
hist(gpd1$data, breaks=60,
     xlim = c(0,80),
     xlab = 'Perte',
     ylab = 'Fréquence',
     main = "")

# plot the estimated distribution
x = seq(-0.25, 4, by=0.0005)
plot.new()
plot(x,
     dgpd(x, scale = beta_h, shape = xi_h),
     ylim = c(0,1),
     ylab = 'g(x)',
     type = 'l'
)
grid(nx = 5,ny = 5, col = 'grey')

# plot QQ-plot and residuals
plot(gpd1)
