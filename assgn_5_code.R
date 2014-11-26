---
  title: "assignment_5"
author: "Iris"
date: "November 7, 2014"
output: html_document
---
  
# death probabilities

```{r, eval=TRUE}
delta.c <- 1/3
delta.r <- 10/32
tau <- 3/4
delta.s0 <- 1/4
delta.s <- delta.s0 + tau*fc
```

#put out initial outlay - start at equal abundances (from Fig.1 line graphs)

#0 = empty
#1=C
#2=R
#3=S

```{r}
# make the starting matrix
possible.states <- c(0, 1, 2, 3)

states <- sample(possible.states, 50*50, replace=TRUE)

rpsmat <- matrix(states, nrow=50, ncol=50)

# this is the alternate matrix, for our experiment. It is divided into quarters horizontally, with 
# empty cells, colicogenic cells, resistant cells, and susceptible cells in lines from top to bottom

nonrandom <- c(rep(0, 625), rep(1, 625), rep(2, 625), rep(3, 625))
nonrandom.mat <- matrix(nonrandom, nrow=50, ncol=50)


# function for updating individual cells
update <- function(matrix, row, column, prob.e, prob.s){
  if(focal==0){
    matrix[row,column] = sample(c(0,1,2,3), 1, prob=prob.e)
  } else if(focal==1) {
    matrix[row, column] = sample(c(0,1), 1, prob=c(delta.c, 1-delta.c))
  } else if(focal==2){
    matrix[row, column] = sample(c(0,2), 1, prob=prob.s)
  } else if (focal==3) matrix[row, column] = sample(c(0,3), 1, prob=c(delta.r, 1-delta.r), replace=TRUE)
  return(matrix[row, column])
}  

# rpsmat[row, column] <- update(rpsmat, row, column, c(fc, fr, fs), c(delta.s, 1-delta.s))

# for loop

# number of loops to run
ngen=1000*50*50

# output storage vectors
abundance.c <- c()
abundance.s <- c()
abundance.r <- c()


# set type of replacement - global or local
type="global"

for(k in 1:ngen){
  
  column <- sample(1:50, 1)
  row <- sample(1:50, 1)
  
  frequency of each cell type in global environment
  fc <- sum(rpsmat==1)/(nrow(rpsmat)*ncol(rpsmat))
  fr <- sum(rpsmat==2)/(nrow(rpsmat)*ncol(rpsmat))
  fs <- sum(rpsmat==3)/(nrow(rpsmat)*ncol(rpsmat))
  
  # death probability of susceptibles
  delta.s <- delta.s0 + tau*fc
  
  # frequencies for local neighborhood
  fc.loc <- sum(rpsmat[(row-1):(row+1), (column-1):(column+1)]==1)/9
  
  fr.loc <- sum(rpsmat[(row-1):(row+1), (column-1):(column+1)]==2)/9
  
  fs.loc <- sum(rpsmat[(row-1):(row+1), (column-1):(column+1)]==3)/9
  
  # select a focal cell for updating
  focal <- rpsmat[row, column]
  
  # rpsmat[row, column] <- update(rpsmat, row, column, c(fc, fr, fs), c(delta.s, 1-delta.s))
  
  if(type=="global"){
  
  rpsmat[row, column] <- update(rpsmat, row, column, c(sum(rpsmat==0)/(nrow(rpsmat)*ncol(rpsmat)), sum(rpsmat==1)/(nrow(rpsmat)*ncol(rpsmat)),sum(rpsmat==2)/(nrow(rpsmat)*ncol(rpsmat)),sum(rpsmat==3)/(nrow(rpsmat)*ncol(rpsmat))), c(delta.s0 + tau*sum(rpsmat==1)/(nrow(rpsmat)*ncol(rpsmat)), 1-(delta.s0 + tau*sum(rpsmat==1)/(nrow(rpsmat)*ncol(rpsmat)))))
  
  } else if(type=="local") { 
  if(row>2 & row<49 & column>2 & column<49){
  
     rpsmat[row, column] <- update(rpsmat, row, column, c(sum(c(rpsmat[row-1,(column-1):(column+1)],rpsmat[row+1,(column-1):(column+1)],rpsmat[row,column-1],rpsmat[row,column+1])==0)/8, sum(c(rpsmat[row-1,(column-1):(column+1)],rpsmat[row+1,(column-1):(column+1)],rpsmat[row,column-1],rpsmat[row,column+1])==1)/8, sum(c(rpsmat[row-1,(column-1):(column+1)],rpsmat[row+1,column-1:column+1],rpsmat[row,column-1],rpsmat[row,column+1])==2)/8, sum(c(rpsmat[row-1,(column-1):(column+1)],rpsmat[row+1,(column-1):(column+1)],rpsmat[row,column-1],rpsmat[row,column+1])==3)/8), c(delta.s0 + tau*sum(c(rpsmat[row-1,(column-1):(column+1)],rpsmat[row+1,(column-1):(column+1)],rpsmat[row,column-1],rpsmat[row,column+1])==1)/8, (1-delta.s0 + tau*sum(c(rpsmat[row-1,(column-1):(column+1)],rpsmat[row+1,(column-1):(column+1)],rpsmat[row,column-1],rpsmat[row,column+1])==1)/8)))
  
  } else if(row==1 & column>2 & column<49){
  
      rpsmat[row, column] <-  update(rpsmat, row, column, c(sum(c(rpsmat[2,(column-1):(column+1)],rpsmat[1,column-1],rpsmat[1,column+1],rpsmat[50,(column-1):(column+1)])==0)/8, (sum(c(rpsmat[2,(column-1):(column+1)],rpsmat[1,column-1],rpsmat[1,column+1],rpsmat[50,(column-1):(column+1)])==1)/8), sum(c(rpsmat[2,(column-1):(column+1)],rpsmat[1,column-1],rpsmat[1,column+1],rpsmat[50,(column-1):(column+1)])==2)/8, sum(c(rpsmat[2,(column-1):(column+1)],rpsmat[1,column-1],rpsmat[1,column+1],rpsmat[50,(column-1):(column+1)])==3)/8), c(delta.s0 + tau*sum(c(rpsmat[2,(column-1):(column+1)],rpsmat[1,column-1],rpsmat[1,column+1],rpsmat[50,(column-1):(column+1)])==1)/8, 1-(delta.s0 + tau*sum(c(rpsmat[2,(column-1):(column+1)],rpsmat[1,column-1],rpsmat[1,column+1],rpsmat[50,(column-1):(column+1)])==1)/8)))
  
  } else if(row==50 & column>2 & column<49){
  
     rpsmat[row, column] <- update(rpsmat, row, column, c(sum(c(rpsmat[49,(column-1):(column+1)],rpsmat[50,column-1],rpsmat[50,column+1],rpsmat[1,(column-1):(column+1)])==0)/8, sum(c(rpsmat[49,(column-1):(column+1)],rpsmat[50,column-1],rpsmat[50,column+1],rpsmat[1,(column-1):(column+1)])==1)/8, sum(c(rpsmat[49,(column-1):(column+1)],rpsmat[50,column-1],rpsmat[50,column+1],rpsmat[1,(column-1):(column+1)])==2)/8, sum(c(rpsmat[49,(column-1):(column+1)],rpsmat[50,column-1],rpsmat[50,column+1],rpsmat[1,(column-1):(column+1)])==3)/8), c(delta.s0 + tau*sum(c(rpsmat[49,(column-1):(column+1)],rpsmat[50,column-1],rpsmat[50,column+1],rpsmat[1,(column-1):(column+1)])==1)/8, 1-(delta.s0 + tau*sum(c(rpsmat[49,(column-1):(column+1)],rpsmat[50,column-1],rpsmat[50,column+1],rpsmat[1,(column-1):(column+1)])==1)/8)))
  
  } else if(row>2 & row<49 & column==1){
  
     rpsmat[row, column] <- update(rpsmat, row, column, c(sum(c(rpsmat[row,2],rpsmat[row-1,1:2],rpsmat[row+1,1:2],rpsmat[(row+1):(row-1),50])==0)/6, sum(c(rpsmat[row,2],rpsmat[row-1,1:2],rpsmat[row+1,1:2],rpsmat[(row+1):(row-1),50])==1)/8, sum(c(rpsmat[row,2],rpsmat[row-1,1:2],rpsmat[row+1,1:2],rpsmat[(row+1):(row-1),50])==2)/8, sum(c(rpsmat[row,2],rpsmat[row-1,1:2],rpsmat[row+1,1:2],rpsmat[(row+1):(row-1),50])==3)/8), c(delta.s0 + tau*sum(c(rpsmat[row,2],rpsmat[row-1,1:2],rpsmat[row+1,1:2],rpsmat[(row+1):(row-1),50])==1)/8, 1-(delta.s0 + tau*sum(c(rpsmat[row,2],rpsmat[row-1,1:2],rpsmat[row+1,1:2],rpsmat[(row+1):(row-1),50])==1)/8)))
  
  } else if(row>2 & row<49 & column==50){
      rpsmat[row, column] <- update(rpsmat, row, column, c(sum(c(rpsmat[row,49],rpsmat[row-1,49:50],rpsmat[row+1,49:50],rpsmat[(row+1):(row-1),1])==0)/8, sum(c(rpsmat[row,49],rpsmat[row-1,49:50],rpsmat[row+1,49:50],rpsmat[(row+1):(row-1),1])==1)/8, sum(c(rpsmat[row,49],rpsmat[row-1,49:50],rpsmat[row+1,49:50],rpsmat[(row+1):(row-1),1])==2)/8, sum(c(rpsmat[row,49],rpsmat[row-1,49:50],rpsmat[row+1,49:50],rpsmat[(row+1):(row-1),1])==3)/8), c(delta.s0 + tau*sum(c(rpsmat[row,49],rpsmat[row-1,49:50],rpsmat[row+1,49:50],rpsmat[(row+1):(row-1),1])==1)/8, 1-(delta.s0 + tau*sum(c(rpsmat[row,49],rpsmat[row-1,49:50],rpsmat[row+1,49:50],rpsmat[(row+1):(row-1),1])==1)/8)))
  
  } else if(row==1 & column==1){
  
  rpsmat[row, column] <- update(rpsmat, row, column, c(sum(c(rpsmat[1,2],rpsmat[2,2],rpsmat[2,1],rpsmat[49,50], +rpsmat[49,49],rpsmat[50, 49])==0)/6, sum(c(rpsmat[1,2],rpsmat[2,2],rpsmat[2,1],rpsmat[49,50], +rpsmat[49,49],rpsmat[50, 49])==1)/6, sum(c(rpsmat[1,2],rpsmat[2,2],rpsmat[2,1],rpsmat[49,50], +rpsmat[49,49],rpsmat[50, 49])==2)/6, sum(c(rpsmat[1,2],rpsmat[2,2],rpsmat[2,1],rpsmat[49,50], +rpsmat[49,49],rpsmat[50, 49])==3)/6), c(delta.s0 + tau*sum(c(rpsmat[1,2],rpsmat[2,2],rpsmat[2,1],rpsmat[49,50], +rpsmat[49,49],rpsmat[50, 49])==1)/6, 1-(delta.s0 + tau*sum(c(rpsmat[1,2],rpsmat[2,2],rpsmat[2,1],rpsmat[49,50], +rpsmat[49,49],rpsmat[50, 49])==1)/6)))
  
  } else if(row==1 & column==50){
  
    rpsmat[row, column] <-   update(rpsmat, row, column, c(sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==0)/6, sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==1)/6, sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==2)/6, sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==3)/6), c(delta.s0 + tau*sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==1)/6, 1-(delta.s0 + tau*sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==1)/6)))
  
  } else if(row==50 & column==1){
  
    rpsmat[row, column] <-  update(rpsmat, row, column, c(sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==0)/6, sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==1)/6, sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==2)/6, sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==3)/6), c(delta.s0 + tau*sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==1)/6, 1-(delta.s0 + tau*sum(c(rpsmat[1,49],rpsmat[2,49],rpsmat[2,50],rpsmat[49,1], +rpsmat[49,2],rpsmat[50, 2])==1)/6)))
  
  } else if(row==50 & column==50){
  
      rpsmat[row, column] <-  update(rpsmat, row, column, c(sum(c(rpsmat[50,49],rpsmat[49,49],rpsmat[49,50],rpsmat[2,1], +rpsmat[2,2],rpsmat[1, 2])==0)/6, sum(c(rpsmat[50,49],rpsmat[49,49],rpsmat[49,50],rpsmat[2,1], +rpsmat[2,2],rpsmat[1, 2])==1)/6, sum(c(rpsmat[50,49],rpsmat[49,49],rpsmat[49,50],rpsmat[2,1], +rpsmat[2,2],rpsmat[1, 2])==2)/6, sum(c(rpsmat[50,49],rpsmat[49,49],rpsmat[49,50],rpsmat[2,1], +rpsmat[2,2],rpsmat[1, 2])==3)/6), c(delta.s0 + tau*sum(c(rpsmat[50,49],rpsmat[49,49],rpsmat[49,50],rpsmat[2,1], +rpsmat[2,2],rpsmat[1, 2])==1)/6, 1-(delta.s0 + tau*sum(c(rpsmat[50,49],rpsmat[49,49],rpsmat[49,50],rpsmat[2,1], +rpsmat[2,2],rpsmat[1, 2])==1)/6)))
  
  } 
  }
  

  
  abundance.c[k] <- sum(rpsmat==1)
  abundance.r[k] <- sum(rpsmat==2)
  abundance.s[k] <-  sum(rpsmat==3)
  
  
}

plot(x=1:ngen, y=seq(from=0, to=4, by=4/(ngen-1)), type="n")

lines(log10(abundance.r), col="green")
lines(log10(abundance.s), col="blue")
lines(log10(abundance.c), col="red")




