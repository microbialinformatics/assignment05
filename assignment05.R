

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
  index <- c(nrow, ncol)
  index_value <- matrix[nrow, ncol]  
  if(nrow!=1 & nrow != nrow(matrix) & ncol != 1 & ncol != ncol(matrix)){ # IF IN MIDDLE
    cell1 <- matrix[nrow-1,ncol-1]
    cell2 <- matrix[nrow-1,ncol]
    cell3 <- matrix[nrow-1,ncol+1]
    cell4 <- matrix[nrow,ncol-1]
    cell5 <- matrix[nrow,ncol+1]
    cell6 <- matrix[nrow+1,ncol-1]
    cell7 <- matrix[nrow+1,ncol]
    cell8 <- matrix[nrow+1,ncol+1]  
  } else if(nrow == 1 & ncol != 1 & ncol != ncol(matrix)){ #only for those in row 1 and NO CORNERS!
      cell1 <- matrix[nrow(matrix),ncol-1]
      cell2 <- matrix[nrow(matrix),ncol]
      cell3 <- matrix[nrow(matrix),ncol+1]
      cell4 <- matrix[nrow,ncol-1]
      cell5 <- matrix[nrow,ncol+1]
      cell6 <- matrix[nrow+1,ncol-1]
      cell7 <- matrix[nrow+1,ncol]
      cell8 <- matrix[nrow+1,ncol+1]  
  } else if(nrow == 1 & ncol == 1){ #only for those in row 1 and column 1!!! MUST BE TOP LEFT CORNER!
      cell1 <- matrix[nrow(matrix),ncol(matrix)] #bottom right corner
      cell2 <- matrix[nrow(matrix),ncol]
      cell3 <- matrix[nrow(matrix),ncol+1]
      cell4 <- matrix[nrow,ncol(matrix)] #wrap!
      cell5 <- matrix[nrow,ncol+1]
      cell6 <- matrix[nrow+1,ncol(matrix)] #wrap!
      cell7 <- matrix[nrow+1,ncol]
      cell8 <- matrix[nrow+1,ncol+1]  
  } else if(nrow == 1 & ncol == ncol(matrix)){ #only for those in row 1 and column 250!  TOP RIGHT CORNER
      cell1 <- matrix[nrow(matrix),ncol-1]
      cell2 <- matrix[nrow(matrix),ncol]
      cell3 <- matrix[nrow(matrix),1] #WRAP!
      cell4 <- matrix[nrow,ncol-1]
      cell5 <- matrix[nrow,1]#WRAP!
      cell6 <- matrix[nrow+1,ncol-1]
      cell7 <- matrix[nrow+1,ncol]
      cell8 <- matrix[nrow+1,1]  
  } else if(nrow == nrow(matrix) & ncol != 1 & ncol != ncol(matrix)){ # NO CORNERS LAST ROW
      cell1 <- matrix[nrow-1,ncol-1]
      cell2 <- matrix[nrow-1,ncol]
      cell3 <- matrix[nrow-1,ncol+1]
      cell4 <- matrix[nrow,ncol-1]
      cell5 <- matrix[nrow,ncol+1]
      cell6 <- matrix[1,ncol-1]
      cell7 <- matrix[1,ncol]
      cell8 <- matrix[1,ncol+1] 
  } else if(nrow == nrow(matrix) & ncol == 1){ # BOTTOM LEFT CORNER
      cell1 <- matrix[nrow-1,ncol(matrix)] #matrix!
      cell2 <- matrix[nrow-1,ncol] 
      cell3 <- matrix[nrow-1,ncol+1]
      cell4 <- matrix[nrow,ncol(matrix)] #wrap
      cell5 <- matrix[nrow,ncol+1]
      cell6 <- matrix[1,ncol(matrix)] #kiddy corner !
      cell7 <- matrix[1,ncol] #wrap
      cell8 <- matrix[1,ncol+1] #wrap  
  } else if(nrow == nrow(matrix) & ncol == ncol(matrix)){ # BOTTOM RIGHT CORNER!
      cell1 <- matrix[nrow-1,ncol-1]
      cell2 <- matrix[nrow-1,ncol]
      cell3 <- matrix[nrow-1,1] #WRAP
      cell4 <- matrix[nrow,ncol-1]
      cell5 <- matrix[nrow,1] #WRAP
      cell6 <- matrix[1,ncol-1]
      cell7 <- matrix[1,ncol]
      cell8 <- matrix[1,1]  #WRAP!
 } else if(nrow != 1 & nrow != nrow(matrix) & ncol == 1){ #middle column 1
      cell1 <- matrix[nrow-1,ncol(matrix)]
      cell2 <- matrix[nrow-1,ncol]
      cell3 <- matrix[nrow-1,ncol+1]
      cell4 <- matrix[nrow,ncol(matrix)]
      cell5 <- matrix[nrow,ncol+1]
      cell6 <- matrix[nrow+1,ncol(matrix)]
      cell7 <- matrix[nrow+1,ncol]
      cell8 <- matrix[nrow+1,ncol+1]  
 } else { #middle last column!          (ncol == ncol(matrix) & nrow != 1 & nrow != nrow(matrix)) 
      cell1 <- matrix[nrow-1,ncol-1]
      cell2 <- matrix[nrow-1,ncol]
      cell3 <- matrix[nrow-1,1]
      cell4 <- matrix[nrow,ncol-1]
      cell5 <- matrix[nrow,1]
      cell6 <- matrix[nrow+1,ncol-1]
      cell7 <- matrix[nrow+1,ncol]
      cell8 <- matrix[nrow+1,1]
 }
 
local <- c(cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8)   


# 
#probability: 
    # identify index_value 
    # identify local cells 
          ## proportion of C
          #if index dies -> do 
findWinner <- function (local){
  index_death <- 
  #combine probabilities + proportions
  proportions <- table(local)
  # random death?
  winner <- 
}

# S beats R!
# R beats C!
# C beats S!

        ##Probabilities of dispersal
            #  C = 0.25
            #  S = 0.5
            #  R = 0.25
            ### Probability of Death
                    ## D(C) = 1/3
                    ## D(R) = 10/32
                    ## D(S, 0) + T(fC) = D (total death)
                            # D(S,0 = 1/4)
                            # T = 3/4
                            # fC = proportion C in local
 # C = Red
 # S = Blue
 # R = Green
  
#drawbacks of this model:
### Evolution of S to R?????
### death within local?


#  findAround <- function(matrix, index){   
    
#  }
  
  