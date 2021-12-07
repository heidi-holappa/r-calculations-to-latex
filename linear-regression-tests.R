# Linear Regression functions

rounded.0d <- function(n) {
  return(format(round(n, 0), nsmall = 2))
}

rounded.2d <- function(n) {
  return(format(round(n, 2), nsmall = 2))
}

sxx <- function(x) {
  bar.x <- mean(x)
  x.length <- length(x)
  running_sum <- 0
  formula <- '\\sum_{i=1}^n(X_{i}-\bar{X})^2'
  calc <- 'S_{xx}=&'
  for (i in 1:x.length) {
    running_sum <- running_sum + (x[i]-bar.x)^2
    if (i < x.length) {
      calc <- paste(calc, '(', rounded.2d(x[i]), '-', rounded.2d(bar.x), ')^2+', sep="")
    } else {
      calc <- paste(calc, '(', rounded.2d(x[i]), '-', rounded.2d(bar.x), ')^2', sep="")
    }
  }
  calc <- paste(calc, '=', rounded.2d(running_sum), sep="")
  result = list(formula, calc, running_sum)
  names(result) <- c('formula', 'calculation', 'result')
  return(result)
}

syy <- function(y) {
  bar.y <- mean(y)
  y.length <- length(y)
  running_sum <- 0
  formula <- '\\sum_{i=1}^n(Y_{i}-\bar{Y})^2'
  calc <- 'S_{yy}=&'
  for (i in 1:y.length) {
    running_sum <- running_sum + (y[i]-bar.y)^2
    if (i < y.length) {
      calc <- paste(calc, '(', rounded.2d(y[i]), '-', rounded.2d(bar.y), ')^2+', sep="")
    } else {
      calc <- paste(calc, '(', rounded.2d(y[i]), '-', rounded.2d(bar.y), ')^2', sep="")
    }
  }
  calc <- paste(calc, '=', rounded.2d(running_sum), sep="")
  result = list(formula, calc, running_sum)
  names(result) <- c('formula', 'calculation', 'result')
  return(result)
}

sxy <- function(x, y) {
  barx <- mean(x)
  bary <- mean(y)
  len <- length(x)
  running_sum <- 0
  ts.mid <- ''
  ts <- 'S_{xy}='
  for (i in 1:len) {
    calc <- (x[i]-barx)*(y[i]-bary)
    running_sum <- running_sum + calc
    if (i < len) {
      ts.mid <- paste(ts.mid, '(', format(calc, digits=5), ')+', sep="")
    } else {
      ts.mid <- paste(ts.mid, '(', format(calc, digits=5), ')=', sep="")
    }
    if (i < len) {
      ts <- paste(ts, '(', x[i], '-' , format(barx, digits=5), ')\\cdot(', y[i], '-' , format(bary, digits=5), ')+' ,sep="")
    } else {
      ts <- paste(ts, '(', x[i], '-' , format(barx, digits=5), ')\\cdot(', y[i], '-' , format(bary, digits=5), ')=' ,sep="")
    }
    #print((x[i]-barx)*(y[i]-bary))
  }
  ts <- paste(ts, ts.mid, running_sum)
  sxy.function <- 'S_{xy}=(x_{i}-\bar{x})\\cdot(y_{i}-\bar{y})'
  result <- list(sxy.function, ts, running_sum)
  names(result) <- c('formula', 'calculation', 'test statistic')
  return(result)
}

params <- function(x, y) {
  n <- length(x)
  bar.x <- mean(x)
  bar.y <- mean(y)
  result <- paste('n=&',n,'\\', ' \bar{X}=&',rounded.2d(bar.x),'\\', ' \bar{Y}=&',rounded.2d(bar.y), sep="")
  return(result)
}

calc.beta <- function(sxy.b, sxx.b) {
  beta.function <- '\beta=&\frac{S_{xy}=(x_{i}-\bar{x})\\cdot(y_{i}-\bar{y})}{\\sum_{i=1}^n(X_{i}-\bar{X})^2}'
  beta.value <- sxy.b/sxx.b
  beta.calculation <- paste('\beta=&\frac{',sxy.b,'}{',sxx.b,'}=',rounded.2d(beta.value), sep="")
  result <- list(beta.function, beta.calculation, beta.value)
  names(result) <- c('beta formula', 'beta calculation', 'beta value' )
}

compile <- function(x,y) {
  params.c <- params(x,y)
  sxx.c <- sxx(x)
  syy.c <- syy(y)
  sxy.c <- sxy(x,y)
  beta.c <- calc.beta(sxy.c, sxx.c)
  result = list(params.c, sxx.c, syy.c, sxy.c, beta.c)
  names(result) = c('parameters', 'S_xx', 'S_yy', 'S_xy', 'Beta')
  return(result)
}

  
#Examples

sample.x <- c(214, 167, 242, 279, 223, 214, 251)
sample.y <- c(206, 182, 217, 240, 212, 199, 223)

# Parameters - n, bar.x, bar.y
params.ex1 <- params(sample.x, sample.y)
params.ex1
# SXY
sxy.ex1 <- sxy(sample.x, sample.y) 
sxy.ex1
# SXX
sxx.ex1 <- sxx(sample.x)
sxx.ex1
#SYY
syy.ex1 <- sxx(sample.y)
syy.ex1
# Bring all together
result.ex1 <- compile(sample.x, sample.y)
result.ex1

