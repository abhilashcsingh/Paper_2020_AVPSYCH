#########################################################################################################

# Needed packages to perform the analysis ; 
# UNCOMMENT AND INSTALL THE VERY FIRST TIME ONLY
install.packages(c("foreign","gmodels","xtable",
                   "sjlabelled","sjmisc","sjPlot"))
load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("lavaan","semPlot","semTools","nonnest2","htmlTable")
load(packages)


library(lavaan)
library(dplyr)
library(nFactors)
library(psy)
library(psych)

######################################################
# Reading in SPSS data (not needed in this analysis) #
######################################################

# # Start by setting your working directory
setwd("C:/Users/as719/Google Drive/Research/5a. ML in AV paper/New/Code")

# # Read in data from SPSS (SPSS Data can be really helpful in visualizing the freq. tables and cross-tabs)
# my_data_spss = foreign::read.spss(file="data.sav")
# 
# # Then, convert the data from a list to a data frame
# my_data = as.data.frame(x=my_data_spss)
# 
# # Get the variable labels from the original SPSS file
# my_data_var_labels = attr(x=my_data_spss,which="variable.labels")
# 
# # Apply the variable labels to the data loaded in R
# my_data = sjlabelled::set_label(x=my_data,label=my_data_var_labels)


###########################################
# Reading data from disk in other formats #
###########################################

# Read in data from disk (CSV format data will be used in SEM modeling)
my_data_sem = read.csv(file="datasem.csv")
my_data_sem_efa <- my_data_sem[41:67]
my_data_sem_efa2 <- my_data_sem[,c(41:55,68:78)]
# select 41st to 78th for EFA analysis


####################################################
# Performing EFA analysis  #
####################################################

# choose number of factors
ev <- eigen(cor(my_data_sem_efa)) # get eigenvalues
ap <- parallel(subject=nrow(my_data_sem_efa),var=ncol(my_data_sem_efa), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
# summary(nS)

#efa with 5 factors
n.factors <- 5 
fit <- factanal(my_data_sem_efa, 
                n.factors,                # number of factors to extract
                scores=c("regression"),
                rotation="varimax")
print(fit, digits=2, cutoff=.35, sort=TRUE)


# choose number of factors from another subset of data
ev2 <- eigen(cor(my_data_sem_efa2)) # get eigenvalues
ap2 <- parallel(subject=nrow(my_data_sem_efa2),var=ncol(my_data_sem_efa2), rep=100, cent=.05)
nS2 <- nScree(x=ev2$values, aparallel=ap2$eigen$qevpea)
plotnScree(nS2)
# summary(nS)

#efa with 5 factors
n.factors2 <- 5 
fit2 <- factanal(my_data_sem_efa2, 
                 n.factors2,                # number of factors to extract
                 scores=c("regression"),
                 rotation="varimax")
print(fit2, digits=2, cutoff=.35, sort=TRUE)


####################################################
# Development of an SEM MODEL   #
####################################################

sem1 <- '
#measurement model
    AU =~ AVenthu + AVworry + AVsafe1 + AVsafe2 + AVaccper + AVfeedback + AVfuture_elderly
    PS =~ AVreg_lanes + AVreg_restr + AVreg_driver
    PU =~ AVfuture_own + AVfuture_job + AVfuture_drive 
    RI =~ WORK3A + WORK3B + WORK3C + WORK3D + WORK3E + WORK3F
    AA =~ predict_doc + predict_busi_auto + predict_dronedelivery + predict_3dprinting

#regressions
    AU ~ PS + AA + RI
    PU ~ PS + AA + RI
    PS ~ AA + RI
    AA ~ RI

#residual correlations
    # AVenthu ~~ AVworry + AVsafe1 + AVsafe2 + AVaccper + AVfeedback + AVfuture_elderly
    AVenthu ~~ AVworry + AVsafe2 + AVaccper 
    # AVworry ~~ AVsafe1 + AVsafe2 + AVaccper + AVfeedback + AVfuture_elderly
    AVworry ~~ AVsafe2 + AVaccper + AVfuture_elderly
    # AVsafe1 ~~ AVsafe2 + AVfeedback + AVfuture_elderly
    AVsafe1 ~~ AVsafe2
    AVsafe2 ~~ AVaccper + AVfeedback + AVfuture_elderly
    # AVaccper ~~ AVfeedback + AVfuture_elderly
    AVaccper ~~ AVfuture_elderly
    # AVfeedback ~~ AVfuture_elderly

    # WORK3A ~~ WORK3B + WORK3C + WORK3D + WORK3E + WORK3F
    WORK3A ~~ WORK3B + WORK3C + WORK3D + WORK3E
    # WORK3B ~~ WORK3C + WORK3D + WORK3E + WORK3F
    WORK3B ~~ WORK3C + WORK3D + WORK3E
    # WORK3C ~~ WORK3D + WORK3E + WORK3F
    WORK3C ~~ WORK3E
    # WORK3D ~~ WORK3E + WORK3F
    # WORK3D ~~ WORK3F
    # WORK3E ~~ WORK3F

    # predict_doc ~~ predict_busi_auto + predict_dronedelivery + predict_3dprinting
    predict_doc ~~ predict_dronedelivery
    # predict_busi_auto ~~ predict_dronedelivery + predict_3dprinting
    predict_busi_auto ~~ predict_3dprinting
    # predict_dronedelivery ~~ predict_3dprinting

    AVreg_lanes ~~ AVreg_restr + AVreg_driver
    # AVreg_restr ~~ AVreg_driver

    AVfuture_own ~~ AVfuture_job + AVfuture_drive
    # AVfuture_job ~~ AVfuture_drive
'
# fit the model
fitsem <- sem(sem1, data=my_data_sem)
# display summary output
summary(fitsem)
# Obtain goodness of fit indicators of the model
fitMeasures(fitsem, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))

#########################################################################################################