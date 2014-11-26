### Uncomment line 11 and line 18 to see the local and global GIFs


########  RUN LOCAL SIMULATIONS 
conditions <- c("S", "R", "C", "E")
set.seed(1)
max <- createMatrix(conditions, nrows = 50, ncols = 50)
bigmax <- runLocalSims2(max)
###### MAKE LINE PLOT FOR LOCAL
plotLogTime(bigmax, "Local")
### TIME TO GIF IT OUT  
#gif(bigmax)

########  RUN GLOBAL SIMULATIONS 
cultbigmax <- runGlobalSims2(max) # cult = culture, we're going global
###### MAKE LINE PLOT FOR GLOBAL
plotLogTime(cultbigmax, "Global")
### TIME TO GIF IT OUT
#gif(cultbigmax)

###############################################EXPERIMENT################

########  RUN LOCAL SIMULATIONS 
expmax <- runLocalSims2(max, Rdeath3 = 1/3)
plotLogTime(expmax, "Local")


########  RUN GLOBAL SIMULATIONS 
expbigmax <- runGlobalSims2(max, Rdeath = 1/3) # cult = culture, we're going global
###### MAKE LINE PLOT FOR GLOBAL
plotLogTime(expbigmax, "Global")



################GIF in HTML Output

saveHTML({
 gif(bigmax3)
}, img.name = "local_plot", imgdir = "local_dir", htmlfile = "local.html", autobrowse = FALSE, 
title = "Local GIF", description = "Original Local Simulation GIF")

saveHTML({
  gif(cultbigmax3)
}, img.name = "global_plot", imgdir = "global_dir", htmlfile = "global.html", autobrowse = FALSE, 
title = "Global GIF", description = "Original Global Simulation GIF")