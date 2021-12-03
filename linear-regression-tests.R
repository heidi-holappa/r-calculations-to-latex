# Linear Regression functions

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
  result <- list('S_{xy}=(x_{i}-\bar{x})\\cdot(y_{i}-\bar{y})', ts)
  return(result)
}


#Examples

sample.x <- c(214, 167, 242, 279, 223, 214, 251)
sample.y <- c(206, 182, 217, 240, 212, 199, 223)

# SXY
sxy.ex1 <- sxy(sample.x, sample.y) 
sxy.ex1
