# Route About
Routeabout is an evolutionary program that evolves running routes. Most days, when the cross country team leaves for our runs, we find it hard to come up with original running routes that fit our needs. Sometimes we want hilly runs to build strength; other times, we want to take technical trails and soft surfaces on recovery days.

The program takes in a starting point and a target route length and then evolves a loop from a series of segments. We downloaded the segments from OverPass Turbo and found their distances with GraphHopper. The program generates specifically curated running routes through a provided city with the desired distance, surface type, elevation gain, and “loopiness.”

# To run the project:

The program consists of two files. The first file, jsoncsvtest.core, takes the points and their attributes which are in a JSON file, and makes them into a vector of maps. In order to run the program, load this file into the REPL first. The other file, routeabout-final.core, contains the genetic program. This file also includes the functions used for the dummy data set. When the user runs the program, they should input the population size, number of generation, starting point, and target distance. The target distance is in meters, so we recommend inputting a number over 200, so the program can evolve a loop. We also included the start of the process to export the segments into a map.
