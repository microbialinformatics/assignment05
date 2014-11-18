#Creation of grid
types <- c("E", "R", "C", "S")
data <- sample(types, 62500, replace = T)
grid <- matrix(data, nrow = 250, ncol = 250)

#Just to check outcomes without having super big matrices
smalldata <- sample(types, 25, replace = T)
smallgrid <- matrix(smalldata, nrow = 5, ncol = 5)


#Setting up local neighbourhood and calculating fractions
#Input = the location of the focal point
find_local <- function (focalpoint_row, focalpoint_column, grid){
#If the focal point is situated on the edges, the local neighbourhood will have strange shape. Therefore, all 
#edge cases are one by one implemented:
  # if first row: that row + next,
  # if last row: previous row + that row,
  # if inbetween: previous row + that row + next row,
  # if first column: that column + next,
  # if last column: previous column + that column,
  # if inbetween: previous column + that column + next column.
  

  # When the focal point is found on the first row, ...
  if (focalpoint_row == 1){
    local <- grid[(focalpoint_row):(focalpoint_row+1),]
    if (focalpoint_column == 1){
      local <- local[, (focalpoint_column):(focalpoint_column+1)]
      local[1, 1] <- NA
      divider <- 3}
    else if (focalpoint_column == 250){
      local <- local[, (focalpoint_column-1):(focalpoint_column)]
      local[1, 2] <- NA
      divider <- 3}
    else{
      local <- local[, (focalpoint_column-1):(focalpoint_column+1)]
      local[1, 2] <- NA
      divider <- 6
    }}
  #When the focal point of found on the last row, ...
  else if (focalpoint_row == 250){
    local <- grid[(focalpoint_row-1):(focalpoint_row),]
    if (focalpoint_column == 1){
      local <- local[, (focalpoint_column):(focalpoint_column+1)]
      local[2, 1] <- NA
      divider <- 3}
    else if (focalpoint_column == 250){
      local <- local[, (focalpoint_column-1):(focalpoint_column)]
      local[2, 2] <- NA
      divider <- 3}
    else{
      local <- local[, (focalpoint_column-1):(focalpoint_column+1)]
      local[2, 2] <- NA
      divider <- 6
    }}
  #When the focal point of found on in between, ...
  else {
  local <- grid[(focalpoint_row-1):(focalpoint_row+1),]
  if (focalpoint_column == 1){
    local <- local[, (focalpoint_column):(focalpoint_column+1)]
    local[2, 1] <- NA
    divider <- 6}
  else if (focalpoint_column == 250){
    local <- local[, (focalpoint_column-1):(focalpoint_column)]
    local[2, 2] <- NA
    divider <- 6}
  else{
    local <- local[, (focalpoint_column-1):(focalpoint_column+1)]
    local[2, 2] <- NA
    divider <- 8
  }}
  
  f_e <- length(grep("E", local))/divider
  f_r <- length(grep("R", local))/divider
  f_c <- length(grep("C", local))/divider
  f_s <- length(grep("S", local))/divider
  
  info_local <- list(local, f_r, f_e, f_c, f_s)
  
  return(info_local)
}

#Setting up parameters. Should be converted into a function in the end, with the actual values as input. 
delta_r <- 10/32
delta_c <- 1/3
s_0 <-1/4
tau <- 3/4


#Updating the grid. This happens randomly and 62500 updates are called an 'epoch'.
#Input = the amount of updates you want to occur
update_grid_once <- function(grid){
#update_grid_once <- function(grid, updates){
  #for (i in 1:updates){
  #first update
  #Relocated to following code to find_local, so that the row + column are not inputs anymore of this function
  #randomly selecting a location + obtaining its value
  focalpoint_row = sample(1:nrow(grid), 1)
  focalpoint_column = sample(1:ncol(grid), 1)
  focalpoint <- grid[focalpoint_row, focalpoint_column]
  #is it E, R , S, C + change the focal point
    #call upon find_local to find the probabilities
    info_local <- find_local(focalpoint_row, focalpoint_column, grid)
  if (focalpoint == "E"){
    #update empty space based on the occurence of the different types in the neighbourhood
    grid[focalpoint_row, focalpoint_column] <- sample(types, 1, replace = T, prob = c(info_local[[2]], info_local[[3]], info_local[[4]], info_local[[5]]))}
    #kill microorganism, with a probability equal to the given delta's. 
    else if (focalpoint == "R"){
    grid[focalpoint_row, focalpoint_column] <- sample(c("E", "R"), 1, replace = T, prob = c(delta_r, (1-delta_r)))}
    else if (focalpoint == "C"){
    grid[focalpoint_row, focalpoint_column] <- sample(c("E", "C"), 1, replace = T, prob = c(delta_c, (1-delta_c)))}
    else {
    delta_s <- s_0 + tau*info_local[[4]]
    grid[focalpoint_row, focalpoint_column] <- sample(c("E", "C"), 1, replace = T, prob = c(delta_s, (1-delta_s)))}
  #}
  return(grid)
  #with for loop: takes about 1 minute
}

update_grid_epoch <- function(grid){
  updated_grids <- replicate(62500, update_grid_once(grid))
  epoch <- updated_grids[,,62500]
  return(epoch)
  #with replicate: takes more than 5 minutes (then stopped). Maybe I can find a way that replicate does not 
  #add the new matrix to the old one, but rather replaces it. This could save computation time.
}

#Generating the plot of the abundance over time, for the local neighbourhoods. 

#We take a random point, and evaluate the local neighbourhood for this one 
#=> is impossible: you need the neighbours to be able to update this section! 
#We update the entire grid 5000x62500 times and then 
#1. go through each point, saving the abundance information of each point and than adding these abundances(?)
#2. Select a certain location and merely save the abundance information of that neighbourhood (?)

#Need to 'update' this focalpoints 5000x62500 times. Every 62500 times, we need to calculate the abundances
#of E, R, C, and S and save these to create a plot.
#since neither my computer  nor VirtualSites can do this, I'm cutting the grid down to 5x5 to see
#if that gives us the good results. 

updated_smallgrids<- replicate(3, update_grid(smallgrid))
#updated_smallgrids<- replicate(5000, update_grid_epoch(smallgrid)))
last_updated <- updated_smallgrids[,,3]
columns <- col(last_updated)
rows <- row(last_updated)

info_local <- mapply(find_local, columns, rows, last_updated)

# abundance_local <- function(focalpoint_row, focalpoint_column, grid){
#   local <- info_local(focalpoint_row, focalpoint_column, grid)
#   E_abundance <- 
  
  

  
  
  
#}




#try-outs
# time_lapse <- seq(1,5000*62500,by=62500)
# updated_grids <- sapply(time_lapse, update_grid(grid, 62500))
# abundance_R <- sapply(time_lapse, "R" %in% update_grid(62500))
#abundance_R <- sapply(time_lapse, length(grep("R", update_grid(62500))))
#updated_grids<- mapply(update_grid, grid = grid, updates = 5000*62500)
#abundance_R <- sapply(update_grid(grid), pmatch("R"))