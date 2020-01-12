###############################
# UARK PSYC 6343 (Multivariate)
# R overview/refresher
# Authored by: Austin Eubanks
##############################

##################################.
# 1. Before we get started
##################################

# If you have not already, run this command to get the packages
# that you will need for this script:

#t install.packages("tidyverse")
# install.packages("psych")

# Now that the packages are installed, you have to load them using library( )

library(tidyverse)
library(psych)  

# One thing to note here (using an example of the describe( ) function in the
# psych library, which we will use later to give us basic descriptive
# statistics)... If you try to use a function that is in a library that you
# didn't load (e.g., describe(data) before you run library(psych)), you'll get
# an error. If there is something you want to run, but don't want to load the
# package for that one thing, you can explicitly call the package using 2
# colons, like this: psych::describe(data)  <- That would run even if you didn't
# run library(psych) first. I tell you this so that in the future, if you see
# something::somethingelse(), you'll know "something" is the library, and
# "somethingelse" is the function name. I will use this type of notation from
# time to time as a way of letting you know what library a specific function is
# being called from, but if the library has been loaded, you can leave of the
# colons and what proceeds them.


# We will get more into script/naming conventions later on, but for now, just as
# a heads up, you will ALWAYS have ALL packages loaded within a script at the
# very top. My convention is to have a header at the top of any script called
# "Libraries."

# Rstudio panes/appearance

# Important general notes:
  ## R is case sensitive. 
  ## R does not read spaces; they are only there to improve readability.
  ## All forms of "brackets", ( ), [ ], { }, must come in PAIRS. If you leave one
  ## out, R will yell at you for it. 
  ## Don't reinvent the wheel. (Copy and paste is your friend.)


##################################
# 2. Not your twitter's hashtag
##################################

# The pound sign/"hashtag" is used for comments. (Fun fact, on a phone, it's
# official name is an octothorpe... The more you know --*). Anything that comes
# after a # is not read by R. We'll discuss this more later.

# If you put 4 #s at the beginning and end of a line, it will make it
# collapsible. You will use this feature to make going through your scripts much
# easier. For instance, a homework assignment may look like this:

#### Question 1 ####
# stuff
# answers

#### Question 2 ####
# more answers

# As you see above, if you click the triangle beside Question 1, it collapses
# between there and the next set of 4 #s. Keeping your script easy to read and
# navigate will become incredibly important as scripts get longer and more
# complex.


# When writing R code, there is no limit to how long (to the side) a line will
# go. So it's important that you keep line length reasonable (~80 characters),
# so you can fit it on one screen, and avoid having to scroll over to see
# anything.


    ## Tip: R has a keyboard shortcut that will "reflow" long comments, which
    ## means it will automatically adjust and wrap comments. For Macs, the
    ## shortcut is (command + shift + / ). For Windows, (Ctrl + shift + / ).
   
 ## This reflow command is "mindful" of the formatting of your #s too. Notice
 ## this section is indented, and has ##, whereas the previous sections were
    
        #### single #s and not indented. Reflow will match whatever format you
        #### start with.

##################################
# 3. Data types and structures
##################################

# There are several types/classes of data that R uses. I'll only be covering the
# 4 types we need to know for now:

# Character ("m", "duck")
# Numeric (real or decimals, aka "double")
# Integer (2 will show in the environment  as 2L so R knows it's an integer)
# Boolean (logical: TRUE / FALSE)

# Factor (nominally different)

# What *kind* of data is only half of the story though. We also need to know its
# *structure*. I will introduce you to the 4 data structures we will be using
# for now; they are broken down dimensions, and homogeneity. 

# Dimensions: Think of a 1-dimension (1D) data structure as a single row of
# values. Its only dimension is the length. A 2D structure is like a table;
# the two dimensions are length and height. 

# Homogeneity: This deals with whether the data inside the structure are all
# the same kind of data (Homogeneous; e.g., all numeric), or if there are
# different types of data within the structure (Heterogeneous, e.g., a mixture
# of numbers and TRUE / FALSE values).

#        Homogeneous       Heterogeneous
# 1D    Atomic Vector          List
# 2D        Matrix          Data frame (tibble)

# Note, often "atomic vector" is just referred to as a vector. 

##################################
# 4. Coding basics
##################################

# You can use R as a calculator. 

2 + 6 # Addition

9 - 2 # Subtraction

4 * 9 # Multiplication

8 / 2 # Division

# Use spaces before/after all infix operators (=, +, -, <, >, etc.) for
# clarity (unless it is a negative sign).

3--5    # Bad
3 - -5  # Good
(8+4)/(5-2)+5  # This will run, but is bad (hard to read)
(8 + 4) / (5 - 2) + 5  # Same answer, but better code writing.

# Inequalities return Boolean Logic values ("TRUE" or "FALSE")

5 < 2 # "less than or equal to" is written: <=
5 > 2 # "greater than or equal to" is written: >=

# To test if something is exactly equal to something else, use two equal signs.
# Exclamation points mean "not". (Also, for later, as logic operators, "AND" is
# &, "OR" is | (i.e., the \ button if you hit shift))

5 == 2
5 == 5
5 != 2

##################################
# 5. Writing R code
##################################

# You can create and store "objects" with the assignment operator <- or ->. The
# arrow points in the direction of the assignment. You can use a keyboard
# shortcut to produce a left-pointing assignment operator. Mac: option + -
# (minus sign); Windows: Alt + - (minus sign). Note when you do this, it
# automatically adds a space before and after the arrow, as you should do
# (remember, spacing = readability).

a <- 5
b <- 3

# We have now assigned a the value of 5, and b the value of 3. If you run a line
# with a variable name, it'll display what value that variable holds. You can
# check for yourself...

a
b

# You can do arithmetic using variables, and also store the output as a new
# object. Notice when we create new objects, they appear in the Environment
# pane of our workspace.

a + 2
a * b
b >= 9

c <- a * b

c

# Technically you can also use a single equal sign (a = 5), but you will NEVER
# do that (say it with me... "I will NEVER use an equals sign as an assignment
# operator") because it *will* cause confusion later.  

# You can also store character strings as objects

phrase <- "Hello world!" #cliché

# Note here if you highlight "phrase" above and hit Crtl/Cmd + Enter (like you
# would to run the whole line), you can run partial lines. Highlighting and
# running "phrase" above is the exact same as typing "phrase" on the next line
# and running it on its own, like with did with a, b, and c above.

# You can also store lists, vectors, data frames, and many other things that
# we'll get to down the road, as objects. If you need R to recognize a group of
# "stuff", you can concatenate them (i.e., link them together) using c( ). This
# basically tells R, "these are different things, but all go together as one
# vector."

d <- c(3,7,2,12,9)

d

# You can use typeof( ) to identify what type of data an object is.

typeof(d) # Notice the console returns "double" instead of "numeric"... 
# All you need to know is that they are the same. 
# Again, double == numeric.

# You can corerce (i.e., force) R to make data into other types where you need to.
e <- as.integer(d)
typeof(e)

f <- as.character(d)
typeof(f)

g <- as.factor(d) 
str(g)


e + 5 # This adds 5 to each value in the vector
f + 5 # Here we get an error, because f is a character vector, and you 
# can't do math with characters. It will become increasingly important to
# be aware of what data type your object is. Sometimes you'll run into
# functions that require one vs. another, and you'll have to convert them
# to get things to run smoothly.

animals <- c("dog", "cat", "monkey", "bear", "emu")

animals

# If we want to check the structure  of something, we use str( ).

str(animals) # This is a character  vector, which has 5 strings in it.


########### Sequences and indexing ########### 

# If you need something to have multiple values, there are ways to do that. If
# it's a consecutive sequence of integers, you can just put the first and last
# number, separated by a colon (note, this is an exception to spacing around
# punctuation marks). 

1:10 # (run these lines and check the results in the console)
4:9

# There is also a sequence function, seq( ), that allows for more flexibility.

#################################################
#  SIDEBAR.... 
#################################################

# If you find yourself in need of help, try adding a question mark before a
# function to pull up the help for that particular function. If one doesn't
# work, try two-- one will only work when trying to find the help on a
# function of a library that is currently loaded. Two question marks will pull
# up anything.

# Beyond the built-in R help, there is a vast amount of resources online...
# Indeed, this is why R is one of the fastest growing programming languages
# (along with Python). Google is your friend. GOOGLE IS YOUR FRIEND. 
# G O O G L E   I S   Y O U R    F R I E N D. I can't stress this enough...
# Do not feel like you are "not getting it" or "struggling" because you are 
# referencing Google, cheat sheets, notes, help/documentation, etc. There is 
# far more information about R and all of its various packages and such than
# one person could possibly retain in their memory. Even people with exceptional 
# R skills, and extensive amounts of time spent in R use references and look things
# up. We will talk more about ways to find answers to questions in the lab, and in
# the future.

#################################################
# END SIDEBAR....(back to sequences)
#################################################

?seq() # Now we can see all the possible arguments, like:

seq(from = 4, to = 10, by = 0.5)
seq(4, 6, .5) # From, to, and by are the defaults, so if those are your only
# 3 values, you don't *have* to make it explicit.

# Instead of "to," you can tell R "I want a sequence starting with X, that goes
# by Y, Z many times."

seq(from = 0, by = .41987, length.out = 30) 


###### Indexing 

# There will be times you need to call a subset from within a vector. You do
# this with brackets [ ] beside the object:

animals[1] # The 1st item
animals[2:4] # The 2nd, 3rd, and 4th items
animals[c(1,4,5)] # The 1st, 4th, and 5th items

# If you are indexing something from a data frame or matrix, there are two
# values in the brackets: [row, col]. For instance, df[1,3] will select the
# value in the 1st row, 3rd col df[1, ] will select the 1st row, ALL cols, or
# df[ ,3] will select all the values in the 3rd col.

##################################
# 6. What's in a name?
##################################

# It cannot be stressed enough how important it is to have AND STICK TO naming
# conventions. As we go through this year, it'll become crystal clear to you why
# this is the case. You will have *some* flexibility on what conventions you
# use, but there are a few requirements.

## "Snake case": Snake case is a naming convention wherein you use lowercase
## letters divided by periods (names.like.this). Some people prefer/use
## underscores (like_this), but I prefer periods because it's more effecient. 

## File names will always be [yourlastname].[hw/drill].#, for instance,
## eubanks.hw.1

## Variable names should be meaningful, yet concise... You'll be typing these
## names *a lot*, and if you name your variable
## log.transformed.reverse.scored.anxiety.item.4, you're going to have a bad
## time. At the same time, naming it "var1" doesn't help that much either.
## Also, objects CANNOT begin with digits, only characters.
    
    # *(okay, so technically, if you're using tibbles instead of data frames,
    # you *can*) start with digits or include spaces/extra characters, but we'll
    # cross that bridge and discuss how to go about that when we get there.

## For the purposes of our MV class, my personal naming conventions won't always
## be applied though. There's a combination of A) it not being *my* data files
## (i.e., the variables already have names that may or may not (probably not) go
## by my preferred conventions; and B) some things I'll do to stay consistent
## with Ana's historical way of doing it so the SPSS folks can understand what
## we're talking about since the SPSS way is already set.

##################################
# 7. Using R Projects
##################################

getwd() # This shows your current working directory. 

# By default, when you start RStudio, you will be in your computer's "home directory".
# It is called home because most of the files that you create, work with, and care about,
# will be contained in folders further down your directory tree than "home". Files above 
# your home directory are mostly files that you will not be interacting with in the course
# of a normal day. 

# In the future, when we export things (whether it's tables in a .csv file, or
# graphs exporting as .jpg files) they will save to your working directory.

# But, we will be working with R projects. R projects is a way of having
# self-contained projects that maintain consistency with your settings, data,
# script, etc. 


### Create a new project titled "lastname.hw1.Rproj"


# When a project is opened within RStudio the following actions are taken:

# A new R session (process) is started
# The .Rprofile file in the project's main directory (if any) is sourced by R
# The .RData file in the project's main directory is loaded (if project options indicate 
# that it should be loaded).
# The .Rhistory file in the project's main directory is loaded into the RStudio History 
#pane (and used for Console Up/Down arrow command history).
# The current working directory is set to the project directory.
# Previously edited source documents are restored into editor tabs
# Other RStudio settings (e.g. active tabs, splitter positions, etc.) are restored to where 
#they were the last time the project was closed.

# When you are within a project and choose to either Quit, close the project, or
# open another project the following actions are taken:

# .RData and/or .Rhistory are written to the project directory (if current options indicate 
#they should be)
# The list of open source documents is saved (so it can be restored next time the project 
# is opened)
# Other RStudio settings (as described above) are saved.
# The R session is terminated.


##################################
# 8. Reading data into R
##################################

# Most data in "the real world" that you have to read in will either be in
# tab-delineated (.dat) or comma-separated value (.csv) formats.

# .dat files are read with the command: read_tsv()
# .csv files are read with the command: read_csv()

# If you have the data file in your working directory (i.e., in the same
# directory as your .Rproj file, which of course it should be), you can just
# enter the file name in quotation marks:
# df <- read_tsv("HW1_Data.dat")
# df <- read_csv("HW1_Data.csv")

hw1 <- read_csv("hw.1.csv")

# You will often see data frames named "df" or "d" in examples (e.g., when you
# are on StackExchange trying to find answers to a question you have). I will
# use those as temporary names occasionally (for examples, or if we are ONLY
# going to be working from one data frame in a script), but going back to naming
# conventions, data frame names should be meaningful as well. It's all great and
# grand to know df is a data frame, but *what* data frame?

# Sometimes we may give you a link to data, and in that case, you can replace
# the file name with the URL you are given. Also, if your data is not located in
# the working directory, you can specify the file path. 

# df <- read_csv("https://www.dropbox.com/sh/1vltnkriefy4h0r/AAC9AH0c24PKU_M-iBBkMi8Xa/Week%201/Files_Homework_week_1/HW1_Data.csv?dl=0")
# df <- read_csv("~/Desktop/folder/HW1_Data.csv")

# Note that in the filepath (and this is true for all filepaths), the tilde (~)
# is "shorthand" for "home directory"... In other words, it's like the user
# folder on the computer.... From there you can go into whatever folders and
# work your way down the rabbit hole. ~/Documents/... ~/Desktop/... ~/Downloads/... etc.

# Similarly, two periods is "shorthand" for "go up one folder from the current
# working directory. For example, let's say your folder structure is set up to
# whre your working directory is Unit 1:
    # Documents > school > stats > Multivariate > Unit 1

# But your data is is in a different folder in Multivariate:
    # Documents > school > stats > Multivariate > Data

# Then telling R "filename.csv" will automatically read as
# "~/Documents/school/stats/Multivariate/Unit 1/filename.csv" and it will tell
# you no such file exists. By contrast, "../Data/filename.csv" is read as "from
# the working directory (Unit 1), go up one folder level to Multivariate (..),
# then drill down into the Data folder (/Data/) and that's where filename.csv
# is.

# At least at the start of the semester, we will be reading in SPSS files,
# because that's what we have to work with. I hope to get far enough ahead in
# getting this course converted into R scripts that I can create csv files from
# the SPSS files that are workable, for for now, we're working with what we
# have. We'll read SPSS files with a function called read.spss() in library(foreign)

# For this intro, we're just going to use a dataset that is by default in
# Rstudio. It is called "mtcars" and is from Henderson and Velleman (1981),
# Building multiple regression models interactively. Biometrics, 37, 391–411.

?mtcars # Check the documentation to see what the variables are.

cars <- mtcars # Saving mtcars as a new dataframe in our environment called "cars"

######################################
#  9. Working with your data
######################################

#First let's explore our data...

str(cars) # base R
glimpse(cars) # Tidyverse

head(cars)
head(cars, 15)
tail(cars)

## The primary verbs of dplyr (for our purposes):
# filter( ) - picks observations based on their values
# select( ) - picks variables by their names
# mutate( ) - creates new variables from existing variables
# summarise( ) - collapses values down to a single summary
# group_by( ) - can do any of the above functions, but breaking things down
# by a specified group.

# All verbs work similarly: First argument is a df, second tells R what to do
# with that df, and the result is a new df (which makes it easier later to
# string a series of functions across data).

# Let's try these out. First, let's look at only automatic cars that get at least 20 MPG.
filter(cars, mpg > 20)
filter(cars, am == 0, mpg > 20) # or only automatics with > 20 MPG. 

# Say we're only interested in three columns, mpg, cyl, and hp:
whatever <- select(cars, mpg, cyl, hp)

# Let's check the descriptives of our mpg.
summarise(cars, 
          mean.mpg = mean(mpg),
          sd.mpg = sd(mpg),
          min.mpg = min(mpg),
          max.mpg = max(mpg),
          n.mpg = length(mpg))

# But that's not helpful. How about if we group by cyl?

summarize(group_by(cars, cyl), 
          mean.mpg = mean(mpg),
          sd.mpg = sd(mpg),
          min.mpg = min(mpg),
          max.mpg = max(mpg))



# psych::describe() is a much easier way of getting descriptives at a glance

describe(cars)



# Creating a new variable from variables: Let's say (for whatever reason), you
# are interested in calculating the ratio of the car's weight (in 1000 lbs) to
# the 1/4 mile time.... 

mutate(cars, w.t.ratio = wt/qsec) # Now you see this variable in the console. 

# If you want to append the new variable into the df (instead of just seeing it
# in the console), you'll have to assign that back to the df like this:

cars <- mutate(cars, w.t.ratio = wt/qsec) # Now we have 12 variables 

# But mutate isn't only for arithmetic; it can apply any function (inucing
# "custom" user defined functions).

cars <- mutate(cars, gear = as.factor(gear))

mutate(cars, effecient = if_else(mpg > 30, "yes", "no"))


### Quick notes

descriptives <- describe(cars)

# write_csv(descriptives, file = "desc.table.csv")

# na.rm = TRUE

cars[3,1] <- NA
mean(cars[,1])
mean(cars[,1], na.rm = TRUE)


# Wrapping in parantheses just runs the line (storing whatever), but also
# displays the output in the console.
(sex.example <- sample(c("male", "female"), size = 100, replace = T))

# Table gives you quick counts
table(sex.example)

(color.example <- sample(c("red", "white", "blue"), size = 100, replace = T))

# If we put those two variables together into a data.frame, table() will give us
# "cross tab" counts
tbl.ex <- data.frame(sex.example, color.example)

table(tbl.ex)


##################################
# New Stuff
##################################

# R cheat sheets:
    # https://rstudio.com/wp-content/uploads/2019/01/Cheatsheets_2019.pdf
    # https://rstudio.com/resources/cheatsheets/


# Laying the pipe

# This:  %>%   is a pipe. A pipe takes whatever is on the left hand side (lhs)
# of it, and passes it through to whatever is on the right hand side (rhs) of
# it. It is useful for chaining together several steps. A keybord shortcut to
# insert a pipe is Cmd + Shift + M (Mac) or Ctrl + Shift + M (Windows). I set
# mine up to use Cmd + p to insert a pipe then automatically break down to a new
# line, but that's not a default behavior.



# Going back to the dplyr code we did above, it could be rewritten as:

cars %>% 
  filter(mpg > 20)

cars %>% 
  filter(am == 0, mpg < 20) %>% 
  group_by(cyl) %>% 
  summarize(mean.mpg = mean(mpg),
            sd.mpg = sd(mpg),
            min.mpg = min(mpg),
            max.mpg = max(mpg))

cars %>% 
  filter(am == 0, mpg < 20) %>% 
  group_by(cyl) %>% 
  summarize(mean.mpg = mean(mpg),
            sd.mpg = sd(mpg),
            min.mpg = min(mpg),
            max.mpg = max(mpg)) %>% 
  mutate(`m+sd` = mean.mpg + sd.mpg, # Note the special char in the var name and the `` marks
         range = max.mpg - min.mpg)


# Sometimes, with pipes, you still need to explicitly call the "left hand" data
# (lhs). Sometimes this is the result of pipes piping the lhs as the first
# argument of rhs (e.g., x %>% function(y) becomes function(x, y) ), but you
# need it to appear elsewhere in the function; other times it may be because
# there are nested functions, etc. You do this by inserting a period.

cars %>% 
  {c(min(), max())} # Doesn't work

cars %>% 
  select_if(is.numeric) %>% 
  {c(min(., na.rm = T), max(., na.rm = T))} # Returns the min and max numbers of the entire data set

cars %>% 
  select_if(is.numeric) %>% 
  {c(min(.$wt), max(.$wt))} # Returns the min and max of the variable "wt"

# See:
help("%>%") # (magrittr forward-pipe operator)


### The apply family: apply(), lapply(), sapply(), mapply()

# lapply(X, FUN, ...) Applies function (FUN) across all elements in X and
# returns a list
cars <- mtcars

lapply(cars, mean)

cars %>% 
  select(mpg, disp:wt) %>% 
  lapply(MSD)


# User defined functions

paste("How", "Paste", "Works") # Default seperator is a space but can be changed. 
paste("How", "Paste", "Works", sep = "_")
paste("How", "Paste", "Works", sep = "@")
paste("How", "Paste", "Works", sep = "") 
paste0("How", "Paste", "Works")


MSD <- function(x){
 paste0( round(mean(x), 2), " (", round(sd(x),2), ")")
}


MSD(cars$mpg)

something <- function(first, second){
  first + second
}


### Wide/long form transformations of data

# Your df looks like this:

# Participant   Var1    Var2    Var 3
#     1          a       b        c
#     2          d       e        f
#     3          g       h        i


# But you want it to look like this:

# Participant   Var     Value
#     1          1       a        
#     1          2       b        
#     1          3       c        
#     2          1       d        
#     2          2       e        
#     2          3       f        
#     3          1       g        
#     3          2       h        
#     3          3       i        


df <- tibble(participant = 1:5, 
             var1 = sample(letters, 5),
             var2 = sample(letters, 5),
             var3 = sample(letters, 5),
             condition = sample(c("ctrl", "exp"), 5, replace = T))

df

# So now you'll use pivot_longer... 

df %>% 
  pivot_longer(var1:var3, names_to = "variablexxx", values_to = "numbahs")

df %>% 
  pivot_longer(-c(participant, condition), names_to = "variablexxx", values_to = "numbahs")


df %>% 
  gather(var1:var3, key = "variablexxx", value = "numbahs")
