# install.packages("rmarkdown")

# library(rmarkdown)

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
    ei.value[place] <- paste("e_{",i,"}=", m ,'\\', 'cdot', osuudet[i]/100, "=", ei, " \\" , sep="")
    if (place < n) {
      ts <- paste(ts, '\frac{(',test.sample[i], '-' , ei, ')^2}{', ei, '}+' ,sep="")
    } else {
      ts <- paste(ts, '\frac{(',test.sample[i], '-' , ei, ')^2}{', ei, '}=',running_sum ,sep="")
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
  ts <- 'TS ='
  for (i in 1:rows) {
    for (j in 1:cols) {
      eij.current <- sum(sample_matrix[i,])*sum(sample_matrix[,j]) / matrix_sum
      running_sum <- running_sum + (sample_matrix[i,j]-eij.current)^2/eij.current
      e.ij.vector[place] <- paste("e_{",i ,j,"}=", "\frac{", sum(sample_matrix[i,]), '\\', 'cdot', sum(sample_matrix[,j]), "}{",matrix_sum, "}=", eij.current, " \\" , sep="")
      if (place < length(sample_matrix)) {
        ts <- paste(ts, '\frac{(',sample_matrix[i,j], '-' , eij.current, ')^2}{', eij.current, '}+' ,sep="")
      } else {
        ts <- paste(ts, '\frac{(',sample_matrix[i,j], '-' , eij.current, ')^2}{', eij.current, '}=',running_sum ,sep="")
      }

      place <- place + 1
    }
  }
  result <- list('e_ij=}frac{Ni\\cdotMj}{n}',matrix(e.ij.vector, ncol=1), 'TS=sum_{i}sum_{j}\frac{(N_{ij}-\\hat{e}_{ij})^2}{\\hat{e}_{ij}}',ts, running_sum)
  names(result) <- c('calculation.1.function', 'ts.calculation.1', 'calculation.2.function', 'ts.calculation.2', 'ts.sum')
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