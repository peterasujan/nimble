allModels <- c(# vol1
               'blocker', 'bones', 'dyes', 'equiv', 'line', 'oxford', 'pump', 'rats',
               # vol2
               'dugongs')

sapply(allModels, testBUGSmodel, useInits = TRUE)

# special cases in vol1: 'epil', 'leuk', 'salm', 'seeds'

# Problem cases
# dinterval, I(), T(), random indexing:
# kidney, litters, lsat, mice 

# data preparation issue in data block: inhaler (has (k+):(k) style indexing
               
# various cases where we need to refer to a differently-named .bug file:

testBUGSmodel('epil', model = 'epil2.bug', inits = 'epil-inits.R',
              data = 'epil-data.R', useInits = TRUE)
testBUGSmodel('epil', model = 'epil3.bug', inits = 'epil-inits.R',
              data = 'epil-data.R', useInits = TRUE)
testBUGSmodel('seeds', model = 'seedsuni.bug', inits = 'seeds-init.R',
              data = 'seeds-data.R', useInits = FALSE)
testBUGSmodel('seeds', model = 'seedssig.bug', inits = 'seeds-init.R',
              data = 'seeds-data.R', useInits = FALSE)
testBUGSmodel('birats', model = 'birats1.bug', inits = 'birats-inits.R',
              data = 'birats-data.R', useInits = TRUE)
testBUGSmodel('birats', model = 'birats3.bug', inits = 'birats-inits.R',
              data = 'birats-data.R', useInits = TRUE)
testBUGSmodel('ice', model = 'icear.bug', inits = 'ice-inits.R',
              data = 'ice-data.R', useInits = TRUE)
testBUGSmodel('beetles', model = 'beetles-logit.bug', inits = 'beetles-inits.R',
              data = 'beetles-data.R', useInits = TRUE)
testBUGSmodel('birats', model = 'birats2.bug', inits = 'birats-inits.R',
              data = 'birats-data.R', useInits = TRUE)

# various cases where we need to modify the BUGS code, generally the indexing

# test of leuk; needs var info on Y and dN since these are used in data block
system(paste0("echo 'var\nY[N,T],\ndN[N,T];' >> ", file.path(tempdir(), "leuk.bug")))
system(paste("cat", system.file('classic-bugs','vol1','leuk','leuk.bug', package = 'nimble'), ">>", file.path(tempdir(), "leuk.bug")))
# need nimbleStep in data block as we no longer have step
system(paste("sed -i -e 's/step/nimbleStep/g'", file.path(tempdir(), "leuk.bug"))) 
testBUGSmodel('leuk', dir = "", model = file.path(tempdir(), "leuk.bug"), data = system.file('classic-bugs','vol1','leuk','leuk-data.R', package = 'nimble'),  inits = system.file('classic-bugs','vol1','leuk','leuk-init.R', package = 'nimble'),  useInits = TRUE)

# salm: need dimensionality of logx
system(paste0("echo 'var\nlogx[doses];' >> ", file.path(tempdir(), "salm.bug"))) 
system(paste("cat", system.file('classic-bugs','vol1','salm','salm.bug', package = 'nimble'), ">>", file.path(tempdir(), "salm.bug")))
#system(paste("sed -i -e 's/logx\\[\\]/logx\\[1:doses\\]/g'", file.path(tempdir(), "salm.bug"))) # alternative way to get size info in there
testBUGSmodel('salm', dir = "", model = file.path(tempdir(), "salm.bug"), data = system.file('classic-bugs','vol1','salm','salm-data.R', package = 'nimble'),  inits = system.file('classic-bugs','vol1','salm','salm-init.R', package = 'nimble'),  useInits = TRUE)


system(paste("cat", system.file('classic-bugs','vol2','air','air.bug', package = 'nimble'), ">>", file.path(tempdir(), "air.bug")))
system(paste("sed -i -e 's/mean(X)/mean(X\\[\\])/g'", file.path(tempdir(), "air.bug"))) 
testBUGSmodel('air', dir = "", model = file.path(tempdir(), "air.bug"), data = system.file('classic-bugs','vol2','air','air-data.R', package = 'nimble'),  inits = system.file('classic-bugs','vol2','air','air-inits.R', package = 'nimble'),  useInits = TRUE)


# need the next line or gives error:
#1: In replaceConstantsRecurse(x, constEnv, constNames) :
#  Code age was given as known but evaluates to a non-scalar.  This is probably # not what you want.
system(paste("sed 's/mean(age)/mean(age\\[1:M\\])/g'", system.file('classic-bugs','vol2','jaw','jaw-linear.bug', package = 'nimble'), ">", file.path(tempdir(), "jaw-linear.bug"))) # alternative way to get size info in there
testBUGSmodel('jaw', dir = "", model = file.path(tempdir(), "jaw-linear.bug"), inits = system.file('classic-bugs', 'vol2', 'jaw','jaw-inits.R', package = 'nimble'), data = system.file('classic-bugs', 'vol2', 'jaw','jaw-data.R', package = 'nimble'), useInits = TRUE)


testBUGSmodel('dipper', dir = system.file('classic-bugs', 'other', 'dipper', package = 'nimble'), useInits = FALSE)

# various simple tests of multivariate nodes

# simple test of dmnorm/wishart

K <- 2
y <- c(.1, .3)
model <- function() {
  y[1:K] ~ dmnorm(mu[1:K], prec[1:K,1:K]);
  for(i in 1:K) {
    mu0[i] <- 0
  }
  R[1,1] <- .01
  R[2,2] <- .01
  R[1,2] <- 0
  R[2,1] <- 0
  Omega[1,1] <- .01
  Omega[2,2] <- .01
  Omega[1,2] <- 0
  Omega[2,1] <- 0
  
  mu[1:K] ~ dmnorm(mu0[1:K], Omega[1:K,1:K])
  prec[1:K,1:K] ~ dwish(R[1:K,1:K], 5)
}

inits <- list(mu = c(0,0), prec = matrix(c(.005,.001,.001,.005), 2))
data <- list(K = K, y = y)

testBUGSmodel(example = 'testN', dir = "",
              model = model, data = data, inits = inits,
              useInits = TRUE)
  
# test multi/Dirichlet

set.seed(0)
n <- 100
alpha <- c(10, 30, 15, 60, 1)
K <- length(alpha)
if(require(nimble)) {
  p <- rdirch(1, alpha)
  y <- rmulti(1, n, p)
} else {
  p <- c(.12, .24, .10, .53, .01)
  y <- c(rmultinom(1, n, p))
}

model <- function() {
  y[1:K] ~ dmulti(p[1:K], n);
  p[1:K] ~ ddirch(alpha[1:K]);
  for(i in 1:K) {
    log(alpha[i]) ~ dnorm(0, sd = 100);
  }
  # log(alpha) ~ dmnorm(0, .001)
}

inits <- list(p = rep(1/K, K), alpha = rep(1/K, K))
data <- list(n = n, K = K, y = y)

testBUGSmodel(example = 'test', dir = "",
              model = model, data = data, inits = inits,
              useInits = TRUE)

# simple test of dt()

model <- function() {
  y ~ dt(1, mu, 1)
  mu ~ dt(3, 0, 1)
}

inits <- list(mu = 0)
data <- list(y = 3)

testBUGSmodel(example = 'testt', dir = "",
              model = model, data = data, inits = inits,
              useInits = TRUE)

# simple test of negbin

model <- function() {
  y ~ dnegbin(p, n)
  p ~ dbeta(3,3)
}

data = list(n = 10, y = 3)
inits <- list(p = 0.5)

testBUGSmodel(example = 'testnb', dir = "",
              model = model, data = data, inits = inits,
              useInits = TRUE)

# test of multivariate data nodes as rows of 3-d array
# and of multivariate latent nodes as rows of matrix

set.seed(0)
n <- 100
m <- 10
g <- 3
alpha <- c(10, 30, 15, 60, 1)
K <- length(alpha)
p <- matrix(0, g, K)
y <- array(0, c(m, g, K))

if(require(nimble)) {
  for(i in seq_len(g))
    p[i, ] <- rdirch(1, alpha)
} else {
  p[1,]  <- c(.12, .24, .10, .53, .01)
  p[2,]  <- c(.2, .3, .05, .2, .25)
  p[3,]  <- c(.05, .05, .10, .3, .5)
  rmulti <- rmultinom
}

for(i in seq_len(g)) 
  for(j in seq_len(m))
    y[j, i, ] <- rmulti(1, n, p[i, ])


model <- function() {
  for(i in 1:g)
    for(j in 1:m)
      y[j, i, 1:K] ~ dmulti(p[i, 1:K], n);
  for(i in 1:g)
    p[i, 1:K] ~ ddirch(alpha[1:K]);
  for(i in 1:K) {
    log(alpha[i]) ~ dnorm(0, sd = 100);
  }
}

inits <- list(p = matrix(1/K, g, K), alpha = rep(1/K, K))
data <- list(g =g, m = m, n = n, K = K, y = y)

testBUGSmodel(example = 'test', dir = "",
              model = model, data = data, inits = inits,
              useInits = TRUE)
