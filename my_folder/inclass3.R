library(tidyverse)
library(knitr)

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

plot(dat, pch = 16)
dat$d <- abs(dat$x-0)
dat <- arrange(dat,d)
dat.knn <- dat[1:5,]
mean(dat.knn$y)
##########
r <- 1
dat2 <-filter(dat,dat[,3]<r)
plot(dat2[,1:2], pch = 16)
mean(dat2$y)


#####################
dat2 <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  dat2$d <- abs(dat2$x-x)
  dat2 <- arrange(dat2,d)
  dat2.knn <- dat2[1:5,]
  yhat <- mean(dat2.knn$y)
  return(yhat)
})
loess_estimates <- map_dbl(xgrid, function(x){
  dat2$d <- abs(dat2$x-x)
  r <- 1
  dat2.2 <-filter(dat2,dat2[,3]<r)
  yhat2 <- mean(dat2.2$y)
  return(yhat2)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour= 1) +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
