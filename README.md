Assignment 5
============

### The problem...
One problem that confounds biologists is how best to explain the
biodiversity that surrounds us.  For example, there are antibiotic producing
and sensitive strains of the same species that reside in the same
populations.  Also, there are phage that stably co-exist with their
bacterial prey.  These types of relationships were studied in a [paper published
in the journal Nature](http://www.nature.com/nature/journal/v418/n6894/full/nature00823.html) using the model of [Rock-Paper-Scissors](http://en.wikipedia.org/wiki/Rock-paper-scissors).

### The assignment...
You will quickly notice that the authors did not make their code publicly
available and do not detail all of their assumptions. Your job for this
assignment is to attempt to recreate the line graphs shown in Figure 1 from
either of these two papers.  At a minimum, you need to create functions that
generate the starting matrix and a function that uses the starting matrix to
runs the simulation for a specified number of generations.  You also need to
make the functions flexible enough to allow the user to alter one or more set of
variables. As output, you need to generate a knitr document that describes...

* the problem
* your assumptions
* a reproduction of their line graphs
* an experiment where you outline your hypothesis and what you learned from the experiment.
* if you can figure out how to create time-lapsed heatmaps of the state of the
population (also shown in Fig. 1) as a GIF, I will give you a considerable
amount of extra credit.

### Game plan...
Complete the exercise and submit it as a pull request to the [Assignment 5 repository](https://github.com/microbialinformatics/assignment05). You should not use any packages beyond the base R system. This assignment is due on Friday, November 22nd. Be sure to include all R, Rmd, and md files in your commits. There are two approaches you and your partner can take to work on this project:

* Sit at the same computer and work together and have one person make all of the commits and pushes to GitHub.
* Divide and conquer the functions with one person creating the repository and make the other person a collaborator. Synthesize your code using pull requests
