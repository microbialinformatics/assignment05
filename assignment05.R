

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
  index <- matrix[nrow, ncol]
  cell1 <- matrix[nrow-1,ncol-1]
  cell2 <- matrix[nrow-1,ncol]
  cell3 <- matrix[nrow-1,ncol+1]
  cell4 <- matrix[nrow,ncol-1]
  cell5<- matrix[nrow,ncol+1]
  cell6 <- matrix[nrow+1,ncol-1]
  cell7 <- matrix[nrow+1,ncol]
  cell8 <- matrix[nrow+1,ncol+1]  
  local <- c(cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8)
  
  
  
findAround <- function(matrix, index){  

  
  
  
  
}

