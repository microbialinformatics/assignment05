
########  RUN LOCAL SIMULATIONS 
conditions <- c("S", "R", "C", "E")
max <- createMatrix(conditions, nrows = 50, ncols = 50)
bigmax <- runLocalSims2(max)
### TIME TO GIF IT OUT! (not walk it out)
gif(bigmax)
###### MAKE LINE PLOT FOR LOCAL
eeee <- abundCols(bigmax)
plotLocal(eeee)

########  RUN GLOBAL SIMULATIONS 
cultmax <- findWinnerGlob(max) # cult = culture, we're going global, baby
cultbigmax <- runGlobalSims2(max)
### TIME TO GIF IT OUT!
cultdood <- charToNum(cultbigmax)
gif(cultdood)
###### MAKE LINE PLOT FOR GLOBAL
abundCondsGlob <- abundCols(cultbigmax)
plotLocal(abundCondsGlob)



image(z = mat, axes = FALSE, col = colors)
colors <- c("white", "red", "forestgreen", "blue")
mat <- chartoNumNum(max)



###########################################################################################


createMatrix <- function(variable, nrows=50, ncols=50){
  matrix(sample(variable, nrows*ncols, TRUE), nrow = nrows, ncol = ncols)
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


findWinnerLoc <- function (matrix, local){
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
    fR <- sum(local == "R")/8
    fS <- sum(local == "S")/8
    fE <- sum(local == "E")/8
    e_winner <- sample(c("S", "R", "C", "E"), 1, prob = c(fS, fR, fC, fE))   #dispersal
    matrix[nrow1, ncol1] <- e_winner }   #replace with new outcome
 return(matrix)
}

findLocalWinner <- function(matrix){
    loca <- findLocal(matrix)
    wins <- findWinnerLoc(matrix, loca)
    return(wins)
}
  


runLocalSims <- function(matrix){  # this one is very inefficient!  Don't run it if its bigger than 10 by 10
  colDim <- nrow(matrix)*ncol(matrix) # column dimensions
  conds <- c("NA") #fill matrix on next line with NAs
  rawsim <- createMatrix(conds, nrows = colDim, ncols = (colDim*1000)) # bigass data frame 2500 * 2500000
  rawsim[,1] <- matrix #data frame for raw (every sim) simulation data 
  time_step <- createMatrix(conds, nrows = colDim, ncols = 1000)
  for (i in 2:ncol(rawsim)-1){ 
    work <- matrix(rawsim[,i], nrow = nrow(matrix), ncol = ncol(matrix))  #make matrix from nas
    sim_win <- findLocalWinner(work)  #run findlocalwinner
    vec <- as.vector(sim_win)  #make findlocalwiner a vector
    rawsim[ ,i+1] <- vec #append vector to rawsim + 1
    if ((i+1)%%colDim == 0){ #if divisible by 2500 do ...
      time_step[,(i+1)/colDim] <- rawsim[ ,i+1]  # append to new matrix called time_step 
    }
  } 
  colnames(time_step) <- 1:1000 #name all columns by number of col
  return(time_step) 
}  



runLocalSims2 <- function(matrix){ #This one goes faster!
  colDim <- nrow(matrix)*ncol(matrix) # column dimensions
  conds <- c("NA")  #fill matrix on next line with NAs
  time_step <- createMatrix(conds, nrows = colDim, ncols = 1001) #Make a matrix of NAs 
  j <- 1 # column to be filled in time_step matrix
  for (i in 1:(colDim*1000)) {
    if (i == 1 | i %% colDim == 0) { # ORDER IMPORTANT: if divisible by 2500 do ...
      time_step[,j] <- matrix  # append to new matrix called time_step
      j <- j + 1 # for next iteration
    }
    work <- findLocalWinner(matrix)  #run findlocalwinner
  } 
  colnames(time_step) <- 1:1001 # name all columns by numer of col
  return(time_step)
}  




##############################   Global Calculations and Simulations   ########################

findWinnerGlob <- function(matrix) {  #This finds the global winner
  index_info <- findIndex(max) #nrow, ncol, 
  nrow1 <- as.numeric(index_info[1])  #get row index
  ncol1 <- as.numeric(index_info[2])  #get column index
  index_value <- index_info[3]  #get index value
  colDim <- nrow(max)*ncol(max)
  ########## INCLUDE PROBABILITIES
  maxVec <- as.vector(max)
  #?find, inject, reduct, fold, select, detect
  #apropos("C", where = maxVec)
  fC <- sum(maxVec == "C")/colDim  
  if(index_value == "S") {
    deltaSO <- 1/4 #natural death of S
    tau <- 3/4 #toxicity of colicin 
    s_death <- deltaSO + tau*fC    #death
    s_survive <- 1 - s_death    #survival
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
    fR <- sum(maxVec == "R")/colDim
    fS <- sum(maxVec == "S")/colDim
    fE <- sum(maxVec == "E")/colDim
    e_winner <- sample(c("S", "R", "C", "E"), 1, prob = c(fS, fR, fC, fE))   #dispersal
    matrix[nrow1, ncol1] <- e_winner }   #replace with new outcome
  return(matrix)
}



runGlobalSims2  <- function(matrix) {  #Runs global simulations
    colDim <- nrow(matrix)*ncol(matrix)
    conds <- c("NA")
    time_step <- createMatrix(conds, nrows = colDim, ncols = 501)
    j <- 1
    for (i in 1:(colDim*500)){
      if (i == 1 | i %% colDim == 0){ # ORDER IMPORTANT: if divisible by 2500 do ...
        time_step[,j] <- matrix  # append to new matrix called time_step
        j <- j + 1
      }
      work <- findWinnerGlob(matrix)  #run findlocalwinner
      
    } 
    colnames(time_step) <- 1:501
    return(time_step)
  }  












########################   CHANGE NUMBERS 

#This function is an all-in-one charToNum + numMat, thanks to pat :)
chartoNumNum <- function(matrix){  
  ifelse(matrix == "S",1, ifelse(matrix == "C",2, ifelse(matrix == "R", 3, 4)))
}




#This function makes a character number matrix from a character character matrix
charToNum <- function(matnums){
  tf_E <- matnums == "E"
  E_Index <- which(tf_E,TRUE)
  matnums[E_Index] <- 1
  
  tf_C <- matnums=="C"
  C_Index <- which(tf_C, TRUE)
  matnums[C_Index] <- 2
  
  tf_R <- matnums=="R"
  R_Index <- which(tf_R, TRUE)
  matnums[R_Index] <- 3
  
  tf_S <- matnums=="S"
  S_Index <- which(tf_S, TRUE)
  matnums[S_Index] <- 4  
  
  return(matnums)
}



# creates numeric matrix:  converts a character column to numeric in place
numMat <- function(charMat) {
  ncols <- ncol(charMat)
  nrows <- nrow(charMat)
  namat <- matrix(0, nrow = nrows, ncol = ncols)
  for(i in 1:nrows) {
    for(j in 1:ncols) {
      namat[i,j] = as.numeric(charMat[i,j])
    }
  }
  return(namat)
}






colMatrix <- function(colMat){  #for one column make a matrix
  #i <- sqrt(nrow(colMat))
  return(matrix(colMat, nrow = 50, ncol = 50))
}

plotHeat <- function(newmat){
  colors <- c("white", "red", "forestgreen", "blue")
  image(z=newmat, axes = FALSE, col = colors) #x=1:nrow(newmat), y=1:ncol(newmat),
}

gif <- function(bigmatrix) {
  library(animation)
  
  namat <- chartoNumNum(bigmax)
  oopt <- ani.options(interval = 0.1, nmax = ncol(namat))
  for(i in 1:ani.options("nmax")){
    mat <- colMatrix(namat[,5])
    plotHeat(mat)
    ani.pause()
  }
}



abundCols <- function(time_step){ #create a table with 4 rows (S, R, E, C) and their freq in all columns
  gahhh <- as.data.frame(time_step)
  oop <- sapply(gahhh, function(x) table(factor(x, levels=conditions)))
  loop <- log(oop)
  loop[is.infinite(loop)] = 0  
  loop <- as.data.frame(loop)
  colnames(loop) <- 1:ncol(time_step)
  return(loop)
}



plotLocal <- function(abundCol_output){
  ff <- as.data.frame(t(abundCol_output))
  plot(ff$S, type = "l", col = "blue", xlim=c(0,1000),ylim=c(0,10), main = "Local neighborhood",
       xlab = "Time", ylab = "Log(abundance)", lwd = 2) 
  lines(ff$E, type = "l", col = "black", lwd = 2)  
  lines(ff$C, type = "l", col = "red", lwd = 2)
  lines(ff$R, type = "l", col = "forestgreen", lwd = 2) 
  legend("bottomright",c("S","R","C","E"),col=c("blue","forestgreen","red","black"),lty = 1, lwd = 3)
}






#max <- createMatrix(conditions)
#rar <- runSims(max)























bigdata <- data.matrix(matnums)











ugh <- matrix(rar[,1], nrow = 10, ncol = 10)
if (ugh == "S") <- 1 {
  paste(1)
} else if(ugh == "R"){
  paste(2)
} else if(igh == "C"){
  paste(3)
} else()











nas[,i+1 ] <- findLocalWinner(nas[,i]))
nas[,i+1 ] <- as.vector(findLocalWinner(nas[,i]))


make matrix from nas
fun findlocalwinner
make findlocalwiner a vector
append vector to nas + 1

    
    
    winner <- replicate(2500, findLocalWinner(max), simplify = "list")
    nas[,i] <- nas
  
  
  na_mat <- rep(0, )
  
  
  
  
  return(wins)
}


runSim <- function(matrix){
  for i in (1:2500){
    
    
    winner <- replicate(2500, findLocalWinner(max), simplify = "list")
  }
  
  1000*2500
  
  # 
  #Output log(abundance) and each time step
  
}

                    
 # C = Red
 # S = Blue
 # R = Green
  
#drawbacks of this model:
### Evolution of S to R?????
### death within local?


  
  