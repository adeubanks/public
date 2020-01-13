###############################
# UARK PSYC 6343 (Multivariate)    
# Unit 1 functions overview       
# Authored by: Austin Eubanks
##############################

#### Libraries #####
library(tidyverse) # it's everywhere
library(psych)     # cor.ci, corr.test, describe

# One-offs:

# foreign    (read SPSS files)
# janitor    (freq tables)
# MVN        (multivariate normality)
# finalfit   (missing map, missing patters)
# data.table (rbindlist)

?msleep   # An example data frame to demonstrate functions.

ex <- msleep %>% 
  mutate_if(is.character, as.factor) # Just for example, convert chr to factors



### Total number of subjects
glimpse(ex)
str(ex)

length(ex$name)


### Frequency table for categorical variables 

janitor::tabyl(ex$vore)

ex %>% 
  select(vore:conservation) %>% 
  lapply(janitor::tabyl) 

# If you want to bind all of the output of tabyl() to one big table (e.g., to
# export as a csv to copy and paste into a word document), add another pipe and
# pipe it into data.table::rbindlist(fill = T)... the "fill = T" argument just
# tells R to ignore that some parts have 3 cols, and some have 4 (depending on
# if there are NA values, resulting in the "valid_percent" column being
# created), so the ones that only have 3 get the 4th col filled with NA.


# If you delete the # below on the data.table line, it will pipe the table into
# the next line which writes a csv called "example.table.csv" and saves it in
# your working directory.

ex %>% 
  select(vore:conservation) %>% 
  lapply(janitor::tabyl) %>%      
  data.table::rbindlist(fill = T)  #%>% 
  #write_csv("example.table.csv")


### Collapsing/re-valuing variables

levels(ex$vore)

  # revalue vore so that "insecti" becomes "carni"
ex <- ex %>% 
  mutate(vore.recode = lvls_revalue(vore,
                                    new_levels = c("carni", "herbi", "carni", "omni")))


# view(ex) 

# Please don't include view() in your scripts unless you hash it out; otherwise,
# as I'm trying to cruise through your script, it changes my screen and slows me
# down

  # Just for example, collapse all of the single digit observation conservation
  # levels into a single level called "new.level" under a new variable called
  # "cons.recode".

levels(ex$conservation)

ex <- ex %>% 
  mutate(cons.recode = fct_collapse(conservation,
                                    new.level = c("cd", "en", "nt", "vu"))) 

levels(ex$cons.recode)


  # Look at the freq tables for these two new factors now

ex %>% 
  select(vore.recode, cons.recode) %>% 
  lapply(janitor::tabyl) 


# case_when: when you have multiple ifelse() statements. 

    # Let's say you wanted to recode the vore variable into color labels for
    # whatever reason... You could use nested ifelse statements, but thats a bad
    # idea...

# ifelse( {logical statement} , {output if statement is TRUE} , {output if statement is FALSE} )

ifelse(ex$vore == "carni", "red", 
       ifelse(ex$vore == "herbi", "green",
              ifelse(ex$vore == "insecti", "black", 
                     ifelse(ex$vore == "omni", "grey", NA))))

    # Or, you can use case_when which just reads the statements as sequential
    # ifelse() statements in the order they are entered... This can be handy for
    # e.g., manually assigning numeric values to specific levels of some factor.

case_when(ex$vore == "carni" ~ "red",
          ex$vore == "herbi" ~ "green",
          ex$vore == "insecti" ~ "black",
          ex$vore == "omni" ~ "grey")

# Notice the NAs? Sometimes you might want those to have a specific value (e.g.,
# -1) so they aren't "missing" but you know they were NA values. Furthermore,
# You can end a case_when() with TRUE ~ [something] and that's a "catch all" to
# see at a glance if there are any variables you didn't account for (basically
# anything else that hasn't already been covered by one of the conditions listed
# above it will be coded by the TRUE). Instead of making the output character
# labels (e.g., colors), you could make it numeric values, but everything has to
# be the same type (e.g., you can't code some things as numeric and some as
# characters). If you want your NAs to stay NAs, just add another mutate, and
# change your -1s back to NAs.

case_when(ex$vore == "carni" ~ 1,
          ex$vore == "herbi" ~ 2,
          ex$vore == "insecti" ~ 3,
          ex$vore == "omni" ~ 4,
          is.na(ex$vore) == T ~ -1,
          TRUE ~ -999) 
  
  
# Note there are no -999s in this... But if we had something in the data that
# wasn't one of those 4 labels or NA, it would...

ex$vore <- as.character(ex$vore)
ex$vore[3] <- "something different"

unique(ex$vore)


case_when(ex$vore == "carni" ~ 1,
          ex$vore == "herbi" ~ 2,
          ex$vore == "insecti" ~ 3,
          ex$vore == "omni" ~ 4, 
          is.na(ex$vore) == T ~ -1,
          TRUE ~ -999)


### Descriptive stats for numeric data

ex %>% 
  select_if(is.numeric) %>% 
  describe() %>% 
  tibble %>% 
  select(mean, sd, min, mad, skew)


### Extracting numbers

  # Let's say the "sleep_total" scale maxed out at 19, and instead the data read
  # "19+" (or "19 or more") or whatever... 

table(ex$sleep_total > 19) # There are 3 cases > 19

ex$sleep_total[ex$sleep_total > 19] <- "19+" # Change those 3 to "19+"

as.numeric(ex$sleep_total) # Notice we have 3 NAs if we try to make it numeric,
                           # so we need to remove the "+"

# You could just reverse what was done above... 

ex$sleep_total[ex$sleep_total == "19+"] <- 19

str(ex$sleep_total) # But it's still a character col, so you 
                    # need to make it numeric again.

ex <- ex %>% 
  mutate(sleep_total = as.numeric(sleep_total))


# Or you could do it by extracting the numbers the tidyverse way with
# str_extract() which uses regular expression (regex) to extract specific string
# patterns from character strings.

ex %>%  
  mutate(sleep_total = as.numeric(str_extract(.$sleep_total, "[[:digit:]]+")))


# [[:digit:]] means "any 0-9 digit" and the + means "any number of the
# proceeding thing" (i.e., any amount of digits), so it says to extract all
# consecutive digits, but ONLY consecutive digits. 


# Visually checking for  missing responses
ex %>% 
  select_if(is.numeric) %>% 
  finalfit::missing_plot()


# Let's say sleep_cycle had been mean centered before we got the data.

ex <- ex %>% 
  mutate(sleep_cycle = sleep_cycle - mean(sleep_cycle, na.rm = T))

# Graph the continuous variables in the df using histograms with superimposed normal
# curves.  

MVN::mvn(data = select_if(ex, is.numeric), 
         univariateTest= "SW", univariatePlot = "histogram")

# Keep in mind (almost) no *real* data are perfectly normally distributed, but
# rather we're concerned if it departs normality enoungh to be a problem, and
# Shapiro-Wilk will almost always reject the null on large datasets (i.e., it'll
# tell you it's not normal, even if it's normal enough). Use your judgment on
# how far from normality things look.


# But clearly, some things are badly skewed, and we could try to transform them
# to approach normality... but you cannot log transform any values <= 0, so we
# need to add a constant to make the minimum value > 0. 

min(ex$sleep_cycle, na.rm = T) # So if we add a constant of .4, it'll be > 0

ex <- ex %>% 
  mutate(log.sleep_cycle = log(sleep_cycle + .4))

# Compare... 
MVN::mvn(data = select(ex, sleep_cycle, log.sleep_cycle), 
         univariateTest= "SW", univariatePlot = "histogram")

### What is the correlation between the raw and transformed vars?

cor.test(ex$sleep_cycle, ex$log.sleep_cycle) 


### Creating sets of bi-variate plots. 

    # Let's say you want to create plots of all bi-variate pairs of between the
    # following X and Y variables: 
        # X: bodywt, sleep_total, brainwt
        # Y: vore, conservation (made into numeric)

ggplot(data = ex, aes(x = bodywt, y = as.numeric(vore.recode))) + # create base
  geom_point() + # add points
  geom_smooth(method = lm) + # add line of best fit (regression w/ 95% CI)
  theme_classic() # just my preference to clean up the background

# If you want to save that plot, just use ggsave(); the default (unless you
# specify something) is to save the last plot that was plotted and to save it to
# your working directory (which should be whatever folder you opened the project
# from).

# ggsave("bodywt.vore.PNG")

ggplot(data = ex, aes(x = sleep_total, y = as.numeric(vore.recode))) + 
  geom_point() +
  geom_smooth(method = lm) + 
  theme_classic() 

ggplot(data = ex, aes(x = bodywt, y = as.numeric(conservation))) + 
  geom_point() +
  geom_smooth(method = lm) + 
  theme_classic() 

ggplot(data = ex, aes(x = sleep_total, y = as.numeric(conservation))) + 
  geom_point() +
  geom_smooth(method = lm) + 
  theme_classic() 


# Or, with the help of pivot_longer(), you can convert your df to long form, plot them
# together, and use facets.

ex %>% 
  select(vore.recode, sleep_total, bodywt, brainwt) %>% 
  pivot_longer(names_to = "xlab", values_to = "xval", -vore.recode) %>% 
  ggplot(aes(x = xval, y = as.numeric(vore.recode))) + 
  theme_classic() + 
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_wrap(xlab~., scales = "free_x") 

  
# Correlations
ex %>% 
  select(sleep_total:bodywt) %>% 
  corr.test()


# If you want CIs instead of just p values:
(cors <- ex %>% 
  select(sleep_total:bodywt) %>% 
  cor.ci())
cors$rho
# $rho is the cor table. 
# write_csv(cors$rho, "corsexample.csv")

# If you get an error saying "Error in plot.new() : figure margins too large",
# you need to make your plot pane bigger (it's giving you an error because it
# can't fit the plot in your window).


# Alternatively, you can use chart.Correlation() to display your sig stars
# visually (as well as uni- and bivariate distributions).
ex %>% 
  select_if(is.numeric) %>% 
  PerformanceAnalytics::chart.Correlation()



# What percent of each variable in the data set is missing?

colMeans(is.na(ex)) * 100

tibble(variable = colnames(ex),
       miss.pct = colMeans(is.na(ex)) * 100 ) #%>%
#write_csv("missing.csv")


# Dummy code sleep_cycle as not missing (1) or missing (0) and create a new var
# called "sc.miss"

ex <- ex %>% 
  mutate(sc.miss = ifelse(is.na(.$sleep_cycle), 0, 1))


# Now conduct a t-test and a chi-square with the outcomes of sleep_total and
# vore.recode and the predictor of sc.miss to see if there is a significant
# relationship between the missingness of sleep_cycle and the other two vars.

t.test(ex$sleep_total ~ ex$sc.miss) # p = .23
chisq.test(ex$vore.recode,  ex$sc.miss) # p = .31

# Let's pretend your p values were .048 and .012 instead, and that you also made
# 5 other comparisons that had p values of .03, .52, .021, .019, and .02. Apply
# a bonferroni correction on their p values:

    # How many comparisons? 7. So divide alpha (.05) by 2.
    c(.048, .012, .03, .52, .021, .019, .02) < .05/7
    
    # So none of those p values would be significant once adjutsed. 

    
# Let's just say you wanted to fill in all the missing values of sleep_rem by
# just filling the NAs with the mean of sleep_rem...
    
mean(ex$sleep_rem, na.rm = T) # 1.87541

# So you could say:
ex$sleep_rem.imputed <- ex$sleep_rem # Don't write over your variables; 
                                     # create new ones

ex$sleep_rem.imputed[is.na(ex$sleep_rem.imputed)] <- mean(ex$sleep_rem.imputed, na.rm = T)

# Or you can use imputeTS::na_mean()  
  
ex %>% 
  mutate(sleep_rem.imputed = imputeTS::na_mean(sleep_rem))
