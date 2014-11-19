

createMatrix <- function(variable, nrows=50, ncols=50){
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




findWinner <- function (matrix, local, index, index_value){
  ## proportion of C
  probs <- prop.table(table(local))
  fC <- probs["C"]
  nrow1 <- index[1]
  ncol1 <- index[2]
  if(index_value == "S") {
    deltaSO <- 1/4 #natural death of S
    tau <- 3/4 #toxicity of colicin 
    s_death <- deltaSO + tau*fC    #death
    s_survive <- 1 - s_death    #survival
    ## D(S, 0) + T(fC) = D (total death)
    # fC = proportion C in local
    s_winner <- sample(c("S", "E"), 1, prob = c(s_survive, s_death)) #survival vs. death
    matrix[nrow1, ncol1] <- s_winner #replace with new outcome
  } else if(index_value == "R") {
    r_death <- 10/32  #death
    r_survive <- 1- r_death   #survival
    r_winner <- sample(c("R", "E"), 1, prob = c(r_survive, r_death)) #survival  
    matrix[nrow1, ncol1] <- r_winner #replace with new outcome
  } else if(index_value == "C") {
    c_death <- 1/3  #death
    c_survive <-  1 - c_death #survival
    c_winner <- sample(c("C", "E"), 1, prob = c(c_survive, c_death)) #survival vs death
    matrix[nrow1, ncol1] <- c_winner #replace with new outcome
  } else {
    fS <- probs["S"]
    fR <- probs["R"]
    fE <- 1- fS - fR - fC
    e_winner <- sample(c("S", "R", "C", "E"), 1, prob = c(fS, fR, fC, fE))   #dispersal
    matrix[nrow1, ncol1] <- e_winner #replace with new outcome
  }





                    
 # C = Red
 # S = Blue
 # R = Green
  
#drawbacks of this model:
### Evolution of S to R?????
### death within local?


#  findAround <- function(matrix, index){   
    
#  }
  
  