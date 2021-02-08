# Pacman Tests

This folder contains all the tests for all the tasks.

# Running the tests
To run all tests open the ```Tests.hs``` file in ghci like so:
```sh
$ ghci Tests.hs -i../src
```
After that invoke the ```testAll``` function. You should see the results of all the tests.

Should you want to run the tests for one specified module, say the N-th task, open the ```TestTN.hs``` file with the ``` -i../src``` parameter and call the ```runTNTests```.


# How the tests work

## Tarefa 1

The tests for Tarefa 1 are pretty straightfoward. They are stored in ```testsT1``` , as a list of triples of integers (```[(Int, Int, Int)]```), with the first element corresponding to the width, the second one to the height, and the third one to the seed used to generate the maze.

When running the tests, you should see something like this:
``` sh
*TestT1> runT1Tests
Width: 15
Height: 10
Seed: 123
###############
# ..#  .###...#
#.           .#
#. ###   ### .#
   #       #   
 . ######### # 
#             #
#......##.....#
###...........#
###############
Level: 0
Players: 



Test PASSED


```

As you can see the mazes are printed, so as to be possible to check their correctness manually. However, the ```isValidMaze``` function does that automatically and its result is also printed alongside the maze.


## Tarefa 2 / Tarefa 4

The tests for Tarefa2 (and 4) are stored in the ```tests/mazes/Tarefa2``` (```tests/mazes/Tarefa4```) folder as two text files: one for the input state and another one for the expected output of the function. Here is an example of such a file:
```
####################
# # ..#..... ....###
#.... ....##..#...##
##......##... .....#
#..##... .#.###.#. #
# #..... ...#..#.. #
#.#.#.##...#.......#
#.o.o          ....#
#...# ###  ###  .#.#
 .... #      # .... 
 ..o. ######## ...# 
##.##          #...#
#....#.....###. .#.#
#........#......#..#
#.... ....o...#### #
#.....#..... ...#..#
#.#.##..o.. .... o##
# ......##  #... ..#
#..... ..........#.#
####################
--end-maze--
0
10
0
1.0
L
1
1
Pacman
0.0
Open
Normal
--end-players--
5
```
The lines until ```--end-maze--``` naturally correspond to the maze in question (Note that no players are on the maze itself). After said line, there is the list of players of the state, with no separation between them. There are 11 lines per Pacman and 9 per Ghost. The first lines are common to both of them, as they correspond to their ``PlayerState``` and have the following structure:

- Id
- First Coordinate (y-axis)
- Second Coordinate (x-axis)
- Player speed
- Orientation
- Points
- Lives

After these, if the Player is a Pacman, the next line is equal to ```Pacman```. Otherwise it is equal to ```Ghost``` (if it does not match any of these, by default it will be considered a ghost). If it is a Pacman, the next lines contain:
- The time in Mega mode it has remaining
- The state of its mouth (```Open``` or ```Closed```)
- The Pacmode (```Dying```, ```Normal```, or ```Mega```)


If it is a ghost, the next line is either ```Alive``` or ```Dead```


In addition to these files, there is a list in the code for ```TestT2.hs``` (```TestT4.hs```) with all the tests. For Tarefa 2, each test is a triple of type ```(String, Play, String)``` corresponding to the location of the input file, the wanted ```Play``` and the location of the output file, respectively. For Tarefa 4, it is of type ```(String,String)``` corresponding to the location of the input and output files, respectively.


To run the tests, execute the ```runT2Tests``` (```runT4Tests```) function. The output consists of the input state, the expected output state, and the actual output, as per the following example

```
Running test mazes/Tarefa2/cross_tunnel_and_eat_ghost_input.txt

###############
# #....##. ...#
#.# ......#.#.#
#.#.o..... ...#
#. ..#..#. #..#
##           .#
## ###   ### .#
}  #       # .M
#. ######### .#
#.           ##
#....#..#... .#
#...#.. #.#.# #
#.......#... .#
#..#.. .#.  .##
###############
Level: 5
Players: 
ID:0 Points:1 Lives:1 Coords:(7,0)
ID:1 Points:1 Lives:1 Coords:(7,14)



Move 0 L

Expected
###############
#<#....##. ...#
#.# ......#.#.#
#.#.o..... ...#
#. ..#..#. #..#
##           .#
## ###   ### .#
   #   M   # . 
#. ######### .#
#.           ##
#....#..#... .#
#...#.. #.#.# #
#.......#... .#
#..#.. .#.  .##
###############
Level: 5
Players: 
ID:0 Points:1 Lives:1 Coords:(1,1)
ID:1 Points:1 Lives:1 Coords:(7,7)


Result
###############
#>#....##. ...#
#.# ......#.#.#
#.#.o..... ...#
#. ..#..#. #..#
##           .#
## ###   ### .#
   #   M   # . 
#. ######### .#
#.           ##
#....#..#... .#
#...#.. #.#.# #
#.......#... .#
#..#.. .#.  .##
###############
Level: 5
Players: 
ID:0 Points:1 Lives:0 Coords:(1,1)
ID:1 Points:1 Lives:1 Coords:(7,7)





Test PASSED

```


## Tarefa 3

Just as in Tarefa 2, the tests are stored in files located in the ```tests/maazes/Tarefa3``` folder. There is one file per test, consisting of the maze in question, and the expected result,as per the following example:
```
##########
#        #
# . . . .#
#        #
# ...  ..#
# . . . .#
#        #
#o......o#
#        #
##########
--ans--
Instruct [(10,#)]
Instruct [(1,#),(8, ),(1,#)]
Instruct [(1,#),(1, ),(1,.),(1, ),(1,.),(1, ),(1,.),(1, ),(1,.),(1,#)]
Repeat 1
Instruct [(1,#),(1, ),(3,.),(2, ),(2,.),(1,#)]
Repeat 2
Repeat 1
Instruct [(1,#),(1,o),(6,.),(1,o),(1,#)]
Repeat 1
Repeat 0
```

After the ```--ans--``` line, each of the following lines contain one ```Instruct``` of the expected output

The list of tests are also stored in the ```TestT3.hs``` file, as a list of strings. To run them, invoke the ```runT3Tests```. The output should be as per the following example:

```
Running testCase from file mazes/Tarefa3/3.txt
##########
#........#
##########
#........#
##########
#........#
##########
#........#
##########
##########
Level: 0
Players: 



Expected
[Instruct [(10,#)],Instruct [(1,#),(8,.),(1,#)],Repeat 0,Repeat 1,Repeat 0,Repeat 1,Repeat 0,Repeat 1,Repeat 0,Repeat 0]
Result
[Instruct [(10,#)],Instruct [(1,#),(8,.),(1,#)],Repeat 0,Repeat 1,Repeat 0,Repeat 1,Repeat 0,Repeat 1,Repeat 0,Repeat 0]



Test PASSED
```
