rounded <- function(n) {
  return(format(round(n, 2), nsmall = 2))
}

rounded.d3 <- function(n) {
  return(format(round(n, 3), nsmall = 2))
}

goodness_of_fit.test <- function(test.proportions, test.sample) {
  n <- length(test.sample)
  m <- sum(test.sample)
  place <- 1
  ei.value <- c()
  ts <- 'TS='
  running_sum <- 0
  for (i in 1:n) {
    ei <- m * test.proportions[i]/100
    running_sum <- running_sum + (test.sample[i]-ei)^2/ei
    ei.value[place] <- paste("e_{",i,"}=", m ,'\\', 'cdot', rounded.d3(test.proportions[i]/100), "=", rounded(ei), " \\" , sep="")
    if (place < n) {
      ts <- paste(ts, '\frac{(',rounded(test.sample[i]), '-' , rounded(ei), ')^2}{', rounded(ei), '}+' ,sep="")
    } else {
      ts <- paste(ts, '\frac{(',rounded(test.sample[i]), '-' , rounded(ei), ')^2}{', rounded(ei), '}=',running_sum,'\approx' ,rounded(running_sum) ,sep="")
    }
    place <- place + 1
  }
  result <- list(matrix(ei.value, ncol=1), ts, running_sum)
  names(result) <- c("e_i.values", "ts.calcuation", "ts.sum" )
  return(result)
}


tc_independence.test <- function(sample_matrix) {
  cols <- ncol(sample_matrix)
  rows <- nrow(sample_matrix)
  matrix_sum <- sum(sample_matrix)
  running_sum = 0
  place <- 1
  e.ij.vector <- c()
  ts <- 'TS =&'
  ts.part.1 = ''
  ts.part.2 = '=&'
  for (i in 1:rows) {
    for (j in 1:cols) {
      eij.current <- sum(sample_matrix[i,])*sum(sample_matrix[,j]) / matrix_sum
      running_sum <- running_sum + (sample_matrix[i,j]-eij.current)^2/eij.current
      e.ij.vector[place] <- paste("e_{",i ,j,"}=", "\frac{", rounded(sum(sample_matrix[i,])), '\\', 'cdot', rounded(sum(sample_matrix[,j])), "}{",rounded(matrix_sum), "}=", rounded(eij.current), " \\" , sep="")
      if (place < length(sample_matrix)) {
        ts.part.1 <- paste(ts.part.1, '\frac{(',rounded(sample_matrix[i,j]), '-' , rounded(eij.current), ')^2}{', rounded(eij.current), '}+' ,sep="")
      } else {
        ts.part.1 <- paste(ts.part.1, '\frac{(',rounded(sample_matrix[i,j]), '-' , rounded(eij.current), ')^2}{', rounded(eij.current), '}' ,sep="")
      }
      if (place < length(sample_matrix)) {
        ts.part.2 <- paste(ts.part.2, '\frac{', rounded((sample_matrix[i,j]-eij.current)^2),'}{', rounded(eij.current), '}+' ,sep="" )
      } else {
        ts.part.2 <- paste(ts.part.2, '\frac{', rounded((sample_matrix[i,j]-eij.current)^2),'}{', rounded(eij.current), '}=',running_sum, '\approx', rounded(running_sum),sep="" )
      }
      place <- place + 1
    }
  }
  params <- paste('n=&', length(sample_matrix), '\\', sep="")
  params <- paste(params, 'sum(Matrix)=&', matrix_sum, '\\', sep="")
  func.1 <- 'e_ij=\frac{Ni\\cdotMj}{n}'
  func.2 <- 'TS=\\sum_{i}\\sum_{j}\frac{(N_{ij}-\\hat{e}_{ij})^2}{\\hat{e}_{ij}}'
  ts <- paste(ts, ts.part.1, ts.part.2)
  result <- list(params, func.1 ,matrix(e.ij.vector, ncol=1), func.2,ts, running_sum)
  names(result) <- c('parameters','calculation.1.function', 'ts.calculation.1', 'calculation.2.function', 'ts.calculation.2', 'ts.sum')
  return(result)
}

# goodness_of_fit.test - Example
ex.proportions <- c(13.7,32.5,26.3,17.1,10.4)
ex.sample <- c(24,94,48,35,39)
gof.example <- goodness_of_fit.test(ex.proportions, ex.sample)
gof.example

# tc_independence.test - Example
tc_example.matrix <- matrix(c(35,170,79,190,57,66), byrow = TRUE, ncol=2)
tc_example <- tc_independence.test(tc_example.matrix)
tc_example