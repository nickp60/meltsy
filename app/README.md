# Mixed Effects Linear modelling for Time Series

# Overview
Meltsy is an easy interface for exploring mixed models.  For a nice introduction to the technique, check out [this tutorial](http://bodowinter.com/tutorial/bw_LME_tutorial.pdf).  Meltsy allows you to upload your data, make some preliminary plots, build models, run differnt models, and compare models for statistical significance.

# Caveats
This is supposed to be the "gateway drug" towards performing robust mixed models analysis.  It takes some of the work out of running the models, but it is up to the user to determine the validity of the models.  This does no work to check whether your model fits well or not, so be cautious.  That said, have fun!


# Data
By default, test data is loaded. If you want to upload your own data, it needs to be formatted correctly.

1. Each row should represent the data of one subject/timepoint (ie, a unique combination of all independent variables), and can have multiple response variables (wide data). 
2. All values that can be treated as number will be, so adjust the data appropriately (ie, a column with replicates codes as 1 2 3 should be changed to A B C etc). 
3. data should be in csv format. Upload your data using the button on the right.

# Plots
Meltsy allows you to make some basic plots of your data. Adding fixed effects adds color groupings and other things. If you want anything more complex, it will be worth your time learning some R.  There are loads of good tutorials online.

# Model
The 'model' tab shows the model which will later be used by the lme4 package. Hitting the `Evaluate Model` button runs the model and displays the results. If you have no random effects, a simple linear model will be run. See this doc for some info on interpretting the results: http://lme4.r-forge.r-project.org/book/Ch1.pdf. Hitting the `Save Model` button records the model and the parameters so that you can perfom comparisons later.

# Model 
Statistical significance is determined by running an ANOVA between two models.  Say you are interested whether in this case 'treatment' effects the methane by day, you would compare the model `methane ~ day` and `methane ~ day + treatment`.


