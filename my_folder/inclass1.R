library(tidyverse)

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

sample <- genreg(100)
x1.r <- sample$x1
x2.r <- sample$x2

sample <-mutate(
  sample,
  y_hat1=5,
  y_hat2=5-x1.r,
  y_hat3=5+2*x2.r,
  y_hat4=5-x1.r+2*x2.r
)

mse.1 <-(sum((sample$y_hat1-sample$y)**2))/100;mse.1
mse.2 <-(sum((sample$y_hat2-sample$y)**2))/100;mse.2
mse.3 <-(sum((sample$y_hat3-sample$y)**2))/100;mse.3
mse.4 <-(sum((sample$y_hat4-sample$y)**2))/100;mse.4


ggplot(tibble(x=c(-7, 7)), aes(x)) +
  stat_function(fun=function(x) 0.8/(1+exp(-x))) +
  ylim(c(0,1)) +
  geom_hline(yintercept=c(0,0.8), linetype="dashed", alpha=0.5) +
  theme_bw() +
  labs(y="P(Y=B|X=x)")

gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-0.2-x)))
  tibble(x=x, y=y)
}

sample.2 <- gencla(100)
##X=1:
(pB<-0.8/(1+exp(-1)))
(pA <-0.2)
(Pc<- 1-pB-pA)




