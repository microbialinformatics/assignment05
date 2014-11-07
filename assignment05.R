

createMatrix <- function(variable, nrows=250, ncols=250){
    matrix(sample(variable, nrows*ncols, TRUE), nrow = nrows, ncol = ncols)
    #Variable must be a list of conditions to add to each cell of the matrix
    #Help from:  http://stackoverflow.com/questions/16915853/r-generate-an-simple-integer-matrix-with-defined-number-of-row-and-column
}

conditions <- c("S", "R", "C", "E")
matrix <- createMatrix(conditions, nrows=10, ncols=10)


runSimulations <- function(matrix, nsim=3000){
  nrow <- sample(1:nrow(matrix), 1)
  ncol <- sample(1:ncol(matrix), 1)
  matrix[nrow, ncol]
}

