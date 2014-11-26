#This file has all of our functions used for local and global matrices.

###########################################################################################
createMatrix <- function(variable, nrows=50, ncols=50){
  matrix(sample(variable, nrows*ncols, replace =TRUE), nrow = nrows, ncol = ncols)
  #Variable must be a list of conditions to add to each cell of the matrix
  #Help from:  http://stackoverflow.com/questions/16915853/r-generate-an-simple-integer-matrix-with-defined-number-of-row-and-column
}


findIndex <- function(matrix){
  ####### FIND THE INDEX
  nrow <- sample(1:nrow(matrix), 1) #randomly creates a row index 
  ncol <- sample(1:ncol(matrix), 1) #randomly creates a column index 
  index <- c(nrow, ncol)
  index_value <- matrix[nrow, ncol] #Grabs the condition in that index
  info <- c(index, index_value)
  return(info); # print character list where seat 1 is nrow, seat 2 is ncol, and seat 3 is condition in cell
}

##############################   Search for the Local Cells ########################
findLocal <- function(matrix){ #, nsim=3000
  info <- findIndex(matrix)
  nrow <- as.numeric(info[1])  #get row index
  ncol <- as.numeric(info[2])  #get column index
  index_value <- info[3]  #get index value
  ####### FIND THE LOCAL INFORMATION
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
    cell8 <- matrix[nrow+1,1] }
  local <- c(cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8, nrow, ncol, index_value) 
  return(local)
}

#######################  FIND THE LOCAL WINNER ######################################
findWinnerLoc <- function (matrix, local, R_death_rate = 10/32){
  nrow1 <- as.numeric(local[9])  #get row index
  ncol1 <- as.numeric(local[10])  #get column index
  index_value <- local[11]  #get index value
  local <- local[1:8]
  ########## INCLUDE PROBABILITIES
  fC <- sum(local == "C")/8  
  if(index_value == "S") {
    deltaSO <- 1/4 #natural death of S
    tau <- 3/4 #toxicity of colicin 
    s_death <- deltaSO + tau*fC    #death
    s_survive <- 1 - s_death    #survival
    s_winner <- sample(c("S", "E"), 1, prob = c(s_survive, s_death)) #survival vs. death
    matrix[nrow1, ncol1] <- s_winner #replace with new outcome
  } else if(index_value == "R") {
    r_death <- R_death_rate  #death
    r_survive <- 1- r_death   #survival
    r_winner <- sample(c("R", "E"), 1, prob = c(r_survive, r_death)) #survival  
    matrix[nrow1, ncol1] <- r_winner #replace with new outcome
  } else if(index_value == "C") {
    c_death <- 1/3  #death
    c_survive <-  1 - c_death #survival
    c_winner <- sample(c("C", "E"), 1, prob = c(c_survive, c_death)) #survival vs death
    matrix[nrow1, ncol1] <- c_winner #replace with new outcome
  } else {
    fR <- sum(local == "R")/8
    fS <- sum(local == "S")/8
    fE <- sum(local == "E")/8
    e_winner <- sample(c("S", "R", "C", "E"), 1, prob = c(fS, fR, fC, fE))   #dispersal
    matrix[nrow1, ncol1] <- e_winner }   #replace with new outcome
  return(matrix)
}


##########################  Combine the local and calculate the winner #####################################
findLocalWinner <- function(matrix, Rdeath2 = 10/32){
  loca <- findLocal(matrix)
  wins <- findWinnerLoc(matrix, loca, R_death_rate = Rdeath2)
  return(wins)
}


##########################  LOCAL SIMULATIONS #####################################
runLocalSims2 <- function(matrix, Rdeath3 = 10/32){ #This one goes faster!
  colDim <- nrow(matrix)*ncol(matrix) # column dimensions
  conds <- c("NA")  #fill matrix on next line with NAs
  time_step <- createMatrix(conds, nrows = colDim, ncols = 1001) #Make a matrix of NAs 
  j <- 1 # column to be filled in time_step matrix
  work <- matrix # will be updated with each for loop, this initiates the process.
  for (i in 1:(colDim*1000)) {
    if (i == 1 | i %% colDim == 0) { # ORDER IMPORTANT: if divisible by 2500 do ...
      time_step[,j] <- work  # append to new matrix called time_step
      j <- j + 1 # for next iteration
    }
    work <- findLocalWinner(work, Rdeath2 = Rdeath3)  #run findlocalwinner
  } 
  colnames(time_step) <- 1:1001 # name all columns by numer of col
  return(time_step)
}  
##############################################################################




##############################   Global Calculations and Simulations   ########################
findWinnerGlobal <- function(matrix, R_death_rate = 10/32) {  #This finds the global winner
  index_info <- findIndex(matrix) #nrow, ncol, 
  nrow1 <- as.numeric(index_info[1])  #get row index
  ncol1 <- as.numeric(index_info[2])  #get column index
  index_value <- index_info[3]  #get index value
  colDim <- nrow(matrix)*ncol(matrix)
  ########## INCLUDE PROBABILITIES
  maxVec <- as.vector(matrix)
  if(index_value == "S") { 
    fC <- sum(maxVec == "C")/colDim
    deltaSO <- 1/4 #natural death of S
    tau <- 3/4 #toxicity of colicin 
    s_death <- deltaSO + tau*fC    #death
    s_survive <- 1 - s_death    #survival
    s_winner <- sample(c("S", "E"), 1, prob = c(s_survive, s_death)) #survival vs. death
    matrix[nrow1, ncol1] <- s_winner #replace with new outcome
  } else if(index_value == "R") {
    r_death <- R_death_rate  #death
    r_survive <- 1- r_death   #survival
    r_winner <- sample(c("R", "E"), 1, prob = c(r_survive, r_death)) #survival  
    matrix[nrow1, ncol1] <- r_winner #replace with new outcome
  } else if(index_value == "C") {
    c_death <- 1/3  #death
    c_survive <-  1 - c_death #survival
    c_winner <- sample(c("C", "E"), 1, prob = c(c_survive, c_death)) #survival vs death
    matrix[nrow1, ncol1] <- c_winner #replace with new outcome
  } else {
    fC <- sum(maxVec == "C")/colDim
    fR <- sum(maxVec == "R")/colDim 
    fS <- sum(maxVec == "S")/colDim
    fE <- (sum(maxVec == "E") - sum(index_value == "E"))/colDim 
    e_winner <- sample(c("S", "R", "C", "E"), 1, prob = c(fS, fR, fC, fE))   #dispersal
    matrix[nrow1, ncol1] <- e_winner }   #replace with new outcome
  return(matrix)
}


runGlobalSims2  <- function(matrix, Rdeath = 10/32) {  #Runs global simulations
  colDim <- nrow(matrix)*ncol(matrix) # column dimensions
  conds <- c("NA") #fill matrix on next line with NAs
  time_step <- createMatrix(conds, nrows = colDim, ncols = 501) #Make a matrix of NAs 
  j <- 1 # column to be filled in time_step matrix
  work <- matrix # will be updated with each for loop, this initiates the process.
  for (i in 1:(colDim*500)){
    if (i == 1 | i %% colDim == 0){ # ORDER IMPORTANT: if divisible by 2500 do ...
      time_step[,j] <- work  # append to new matrix called time_step
      j <- j + 1 #next for loop, fill in the next column
    }
    work <- findWinnerGlobal(work, R_death_rate = Rdeath)  #run findlocalwinner
  } 
  colnames(time_step) <- 1:501
  return(time_step)
}  
###########################################################################################




###########################################################################################
########################   CHANGE MATRIX FROM CHARACTERS TO NUMBERS   ##########################
#This function is an all-in-one charToNum + numMat, thanks to pat :)
chartoNumNum <- function(matrix){  
  ifelse(matrix == "R",1, ifelse(matrix == "C",2, ifelse(matrix == "S", 3, 4)))
}


########################## TAKE A COLUMN of TABLE AND MAKE A MATRIX  ##########################
colMatrix <- function(colMat){  #for one column make a matrix
  #i <- sqrt(nrow(colMat))
  return(matrix(colMat, nrow = 50, ncol = 50))
}

########################## TAKE MATRIX MAKE HEATPLOT  ##########################
plotHeat <- function(newmat){
  colors <- c("forestgreen", "red", "blue","white")
  image(z=newmat, axes = FALSE, col = colors) #x=1:nrow(newmat), y=1:ncol(newmat),
}

##########################  MAKE A GIF WITH MANY PLOTS ##########################
#http://stackoverflow.com/questions/9973610/using-the-animation-package
#http://stackoverflow.com/questions/12038893/combining-r-markdown-and-animation-package
library(animation)
gif <- function(bigmatrix) {
  namat <- chartoNumNum(bigmatrix)
  oopt <- ani.options(interval = 0., nmax = ncol(namat))
  for(i in 1:ani.options("nmax")){
    mat <- colMatrix(namat[,i])
    plotHeat(mat)
    ani.pause()
  }
}
##############################################################################



##############################################################################
##########################   FREQUENCY OF EACH CONDITION ##########################
freqConds <- function(time_step){ #create a table with 4 rows (S, R, E, C) and their freq in all columns
  gahhh <- as.data.frame(time_step)
  oop <- sapply(gahhh, function(x) table(factor(x, levels=conditions)))
  loop <- log10(oop)
  loop[is.infinite(loop)] = 0  
  loop <- as.data.frame(loop)
  colnames(loop) <- 1:ncol(time_step)
  return(loop)
}


plotLogTime <- function(time_step, title){
  freqConds_output <- freqConds(time_step)
  ff <- as.data.frame(t(freqConds_output))
  plot(ff$S, type = "l", col = "blue", xlim=c(0,ncol(freqConds_output)),ylim=c(0,5), main = c(title, "Neighborhood"),
       xlab = "Time", ylab = "Log(abundance)", lwd = 2, bty = "n") 
  #lines(ff$E, type = "l", col = "black", lwd = 2)  
  lines(ff$C, type = "l", col = "red", lwd = 2)
  lines(ff$R, type = "l", col = "forestgreen", lwd = 2) 
  legend("topright",c("S","R","C"),col=c("blue","forestgreen","red"),lty = 1, lwd = 3)
}
##############################################################################






