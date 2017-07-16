genetic-scheming

================

AI \#15 is due on Tuesday, 6 May. Write a genetic algorithm (GA) that learns all ones for a 32 bit 
chromosome. Make your program able to work for any size input string. It should be able to randomly 
generate the initial population. Do the standard selection, crossover, and mutation as discussed 
in class. At each x generations display the best individual along with its fitness and the average 
fitness of the population.

================

## Selection
Tony: I assume standard selection is two stochastically picked parents. The probability of a parent 
getting selected is its fitness divided by the sum of all the fitnesses. 

Parker: if you have a population of 100 individuals this should happen 50 to 100 times each 
generation.  50 times if each crossover produces 2 children and 100 times if each crossover 
produces 1 child.  If 1 child, randomized which parent is first.

## Crossover

Tony: I assume crossover is randomly determined for each child in a generation. We can either due 
one point or two point random crossover. After the points are randomly selected, we randomly pick 
which parent each region is from. We can have as many children as we like. 

Parker:  one point crossover

## Mutation

Tony: After the chromosome is generated, each gene is then checked for mutation. Every gene has a 1 
in BIG_NUMBER or so chance of mutating. Does that sound right?

Parker: use 1/300 chance for each bit in the chromosome.  This allows for the possibility of 0, 1, 
or more bits mutated in a single chromosome.
