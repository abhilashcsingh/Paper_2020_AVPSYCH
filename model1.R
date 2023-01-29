####################################################################################################
# Following are the names of the variables along with what they stand for for each of the latent construct:

#Perceived Usefulness
# AVaccper (3) CARS8. If driverless vehicles become widespread, do you think that the number of people killed or injured in traffic accidents will increase, decrease, or stay about the same?
# AVfuture_elderly (2) CARS10A. If driverless vehicles become widespread, which of the following do you think are likely to happen as a result? Elderly and disabled people will be able to live more independently
# AVfuture_own (2) CARS10C. If driverless vehicles become widespread, which of the following do you think are likely to happen as a result? Owning a car would become much less important to people
# AVfuture_traffic (2) CARS10E. If driverless vehicles become widespread, which of the following do you think are likely to happen as a result? There would be much less traffic in major cities
# pos_multitask (2) CARS6pos2 Can do other things while driving (What is the main reason you would want to ride in a driverless vehicle)
# pos_indep (2) CARS6pos4 Independence (What is the main reason you would want to ride in a driverless vehicle)
# pos_conv (2) CARS6pos7 Convenience (What is the main reason you would want to ride in a driverless vehicle)

#Perceived Safety
# AVsafe1 (4) CARS7A. How safe would you feel sharing the road with a driverless passenger vehicle?
# AVsafe2 (4) CARS7B. How safe would you feel sharing the road with a driverless freight truck?
# AVreg_lanes (4) CARS9A. Would you strongly favor, favor, oppose, or strongly oppose the following rules and regulations for driverless vehicles? Requiring them to travel in dedicated lanes
# AVreg_restr (4) CARS9B. Would you strongly favor, favor, oppose, or strongly oppose the following rules and regulations for driverless vehicles? Restricting them from traveling near certain areas, such as schools
# AVreg_driver (4) CARS9C. Would you strongly favor, favor, oppose, or strongly oppose the following rules and regulations for driverless vehicles? Requiring them to have a person in the driver's seat who could take control in an emergency situation
# pos_safe (2) CARS6pos1 Safety (What is the main reason you would want to ride in a driverless vehicle)
# pos_stress (2) CARS6pos5 Less stressful (What is the main reason you would want to ride in a driverless vehicle)
# pos_long (2) CARS6pos6 Good for long trips (What is the main reason you would want to ride in a driverless vehicle)

#Perceived Risks
# AVfuture_job (2) CARS10B. If driverless vehicles become widespread, which of the following do you think are likely to happen as a result? Many people who drive for a living would lose their jobs
# AVfuture_drive (2) CARS10D. If driverless vehicles become widespread, which of the following do you think are likely to happen as a result? Most people would never learn how to drive a car on their own
# neg_safe (2) CARS6neg1 Safety (What is the main reason you would NOT want to ride in a driverless vehicle)
# neg_tech (2) CARS6neg3 Technology not ready yet (What is the main reason you would NOT want to ride in a driverless vehicle)
# neg_hack (2) CARS6neg4 Potential for hacking (What is the main reason you would NOT want to ride in a driverless vehicle)
# neg_trust (2) CARS6neg5 Trust/Control issues (What is the main reason you would NOT want to ride in a driverless vehicle)

#Attitudes towards Use of av's
# AVenthu (4) CARS3A. How ENTHUSIASTIC are you, if at all, about the development of driverless vehicles?
# AVworry (4) CARS3B. How WORRIED are you, if at all, about the development of driverless vehicles?
# pos_exper (2) CARS6pos3 For the experience (What is the main reason you would want to ride in a driverless vehicle)

#Acceptance of Automation in life
# predict_doc PREDICTA. Do you think each of the following things will or will not happen in the next 20 years? Doctors will rely on computer programs to diagnose most diseases and determine treatments
# predict_busi_auto PREDICTB. Do you think each of the following things will or will not happen in the next 20 years? Most stores and retail businesses will be fully automated and involve little or no human interaction between customers and employees
# predict_dronedelivery PREDICTC. Do you think each of the following things will or will not happen in the next 20 years? Most deliveries in cities will be made by robots or drones instead of humans
# predict_3dprinting PREDICTD. Do you think each of the following things will or will not happen in the next 20 years? When people want to buy most common products, they will create them at home using a 3-D printer

#Response to Innovation in life
# WORK3A. Have the following technologies had a [positive impact, a negative impact / negative impact, positive impact], or no impact either way on you and your job or career? Industrial robots
# WORK3B. Have the following technologies had a [positive impact, a negative impact / negative impact, positive impact], or no impact either way on you and your job or career? Word processing or spreadsheet software
# WORK3C. Have the following technologies had a [positive impact, a negative impact / negative impact, positive impact], or no impact either way on you and your job or career? Email or social media
# WORK3D. Have the following technologies had a [positive impact, a negative impact / negative impact, positive impact], or no impact either way on you and your job or career? Software that manages your daily work schedule or routine
# WORK3E. Have the following technologies had a [positive impact, a negative impact / negative impact, positive impact], or no impact either way on you and your job or career? Smartphones
# WORK3F. Have the following technologies had a [positive impact, a negative impact / negative impact, positive impact], or no impact either way on you and your job or career? Technologies that help customers serve themselves on their own

#Subjective Norms
# AVaware (3) CARS1. How much have you seen or heard about the effort to develop driverless vehicles - that is, cars and trucks that can operate on their own without a human driver?
# AVfeedback (3) CARS2. Has what you've seen or heard about driverless vehicles been mostly positive, mostly negative, or a mix of both?
# predict_AVonroads CARS4. How long, if ever, do you think it will take for MOST of the vehicles on the road to be driverless, rather than driven by humans?

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



















# write.table(x=fit2, 
#             file="efa_results.csv", 
#             row.names=FALSE, 
#             dec = ".", 
#             sep = ";")

# # plot factor 1 by factor 2 
# load <- fit$loadings[,1:2] 
# plot(load,type="n") # set up plot 
# text(load,labels=names(my_data_sem_efa),cex=.7) # add variable names
# # plot factor 2 by factor 3 
# load <- fit$loadings[,c(2,3)] 
# plot(load,type="n") # set up plot 
# text(load,labels=names(my_data_sem_efa),cex=.7) # add variable names

# # check eigenvalue plot
# scree.plot(fit$correlation)

# # test function fa
# solution <- fa(r = cor(my_data_sem_efa), nfactors = 5, rotate = "oblimin", fm = "pa")
# plot(solution,labels=names(my_data_sem_efa),cex=.7, ylim=c(-.1,1))q
# print(solution, digits=2, cutoff=.35, sort=TRUE)

# fit <- factanal(my_data_sem_efa2, 4, rotation="varimax")
# print(fit,  digits = 2, cutoff = .2, sort = TRUE)

####################################################
# Individual CFA Model for each latent construct  #
####################################################

# Theoretically, following are the indicator loadings on latent constructs which seem logical:
#Perceived Usefulness:              PU =~ AVaccper + AVfuture_elderly + AVfuture_own + AVfuture_traffic + pos_multitask + pos_indep + pos_conv
#Perceived Safety:                  PS =~ AVsafe1 + AVsafe2 + AVreg_lanes + AVreg_restr + AVreg_driver + pos_safe + pos_stress + pos_long
#Perceived Risks:                   PR =~ AVfuture_job + AVfuture_drive + neg_safe + neg_tech + neg_hack + neg_trust
#Attitudes towards Use of av's:     AU =~ AVenthu + AVworry + pos_exper
#Acceptance of Automation in life:  AA =~ predict_doc + predict_busi_auto + predict_dronedelivery + predict_3dprinting
#Response to Innovation in life:    RI =~ WORK3A + WORK3B + WORK3C + WORK3D + WORK3E + WORK3F
#Subjective Norms:                  SN =~ AVaware + AVfeedback + predict_AVonroads

####################################################
# Development of PU latent construct  #
####################################################
model1 <- '
#measurement model
    PU =~ AVaccper + AVfuture_elderly + AVfuture_own + AVfuture_traffic + pos_multitask + pos_indep + pos_conv

#regressions
#   PU ~ PS 

#residual correlations
    AVaccper ~~ AVfuture_own
    AVfuture_own ~~ AVfuture_traffic
    pos_multitask ~~ pos_indep + pos_conv
    
    # AVaccper ~~ AVfuture_elderly + AVfuture_own + AVfuture_traffic + pos_multitask + pos_indep + pos_conv
    # AVfuture_elderly ~~ AVfuture_own + AVfuture_traffic + pos_multitask + pos_indep + pos_conv
    # AVfuture_own ~~ AVfuture_traffic + pos_multitask + pos_indep + pos_conv
    # AVfuture_traffic ~~ pos_multitask + pos_indep + pos_conv
    # pos_multitask ~~ pos_indep + pos_conv
    # pos_indep ~~ pos_conv
'

# fit the model
fit1 <- sem(model1, data=my_data_sem)

# display summary output
summary(fit1)
# Obtain goodness of fit indicators of the model
fitMeasures(fit1, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################


####################################################
# Development of PS latent construct  #
####################################################

model2 <- '
#measurement model
    PS =~ AVsafe1 + AVsafe2 + AVreg_lanes + AVreg_restr + AVreg_driver + pos_safe + pos_stress + pos_long

#regressions
#   PU ~ PS 

#residual correlations
    AVreg_lanes ~~ AVreg_restr + AVreg_driver
    AVreg_restr ~~ AVreg_driver + pos_safe + pos_long
    pos_safe ~~ pos_stress
    
    # AVsafe1 ~~ AVsafe2 + AVreg_lanes + AVreg_restr + AVreg_driver + pos_safe + pos_stress + pos_long
    # AVsafe2 ~~ AVreg_lanes + AVreg_restr + AVreg_driver + pos_safe + pos_stress + pos_long
    # AVreg_lanes ~~ AVreg_restr + AVreg_driver + pos_safe + pos_stress + pos_long
    # AVreg_restr ~~ AVreg_driver + pos_safe + pos_stress + pos_long
    # AVreg_driver ~~ pos_safe + pos_stress + pos_long
    # pos_safe ~~ pos_stress + pos_long
    # pos_stress ~~ pos_long
'

# fit the model
fit2 <- sem(model2, data=my_data_sem)

# display summary output
summary(fit2)
# Obtain goodness of fit indicators of the model
fitMeasures(fit2, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################


####################################################
# Development of PR latent construct    #
####################################################

model3 <- '
#measurement model
    PR =~ AVfuture_job + AVfuture_drive + neg_safe + neg_tech + neg_hack + neg_trust

#regressions
#   PU ~ PS 

#residual correlations
    neg_safe ~~ neg_hack + neg_trust
    neg_tech ~~ neg_trust
    

    # AVfuture_job ~~ AVfuture_drive + neg_safe + neg_tech + neg_hack + neg_trust
    # AVfuture_drive ~~ neg_safe + neg_tech + neg_hack + neg_trust
    # neg_safe ~~ neg_tech + neg_hack + neg_trust
    # neg_tech ~~ neg_hack + neg_trust
    # neg_hack ~~ neg_trust
'

# fit the model
fit3 <- sem(model3, data=my_data_sem)

# display summary output
summary(fit3)
# Obtain goodness of fit indicators of the model
fitMeasures(fit3, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################


####################################################
# Development of AU latent construct      #
####################################################
 
model4 <- '
#measurement model
    AU =~ AVenthu + AVworry + pos_exper

#regressions
#   PU ~ PS 

#residual correlations 
    #AVenthu ~~ AVworry + pos_exper
    #AVworry ~~ pos_exper
'

# fit the model
fit4 <- sem(model4, data=my_data_sem)

# display summary output
summary(fit4)
# Obtain goodness of fit indicators of the model
fitMeasures(fit4, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################


####################################################
# Development of AA latent construct        #
####################################################
 
model5 <- '
#measurement model
    AA =~ predict_doc + predict_busi_auto + predict_dronedelivery + predict_3dprinting

#regressions
#   PU ~ PS 

#residual correlations 
      
    # predict_doc ~~ predict_busi_auto + predict_dronedelivery + predict_3dprinting
    # predict_busi_auto ~~ predict_dronedelivery + predict_3dprinting
    # predict_dronedelivery ~~ predict_3dprinting
'

# fit the model
fit5 <- sem(model5, data=my_data_sem)

# display summary output
summary(fit5)
# Obtain goodness of fit indicators of the model
fitMeasures(fit5, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################


####################################################
# Development of RI latent construct  #
####################################################
  
model6 <- '
#measurement model
    RI =~ WORK3A + WORK3B + WORK3C + WORK3D + WORK3E + WORK3F

#regressions
#   PU ~ PS 

#residual correlations 
    # WORK3A ~~ WORK3B
    WORK3A ~~ WORK3C
    # WORK3A ~~ WORK3D
    # WORK3A ~~ WORK3E
    # WORK3A ~~ WORK3F
    # WORK3B ~~ WORK3C
    WORK3B ~~ WORK3D + WORK3E
    WORK3C ~~ WORK3D + WORK3E + WORK3F
    WORK3D ~~ WORK3E
'

# fit the model
fit6 <- sem(model6, data=my_data_sem)

# display summary output
summary(fit6)
# Obtain goodness of fit indicators of the model
fitMeasures(fit6, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################


####################################################
# Development of SN latent construct   #
####################################################
 
model7 <- '
#measurement model
    SN =~ AVaware + AVfeedback + predict_AVonroads

#regressions
#   PU ~ PS 

#residual correlations 
    # AVaware ~~ AVfeedback + predict_AVonroads
    # AVfeedback ~~ predict_AVonroads
'

# fit the model
fit7 <- sem(model7, data=my_data_sem)

# display summary output
summary(fit7)
# Obtain goodness of fit indicators of the model
fitMeasures(fit7, c("cfi", "rmsea", "chisq", "srmr", "gfi", "ecvi"))
####################################################





####################################################
# # Obtain confidence intervals for the estimated coefficients
# parameterEstimates(fit1, ci = TRUE, level = 0.95)
# standardizedSolution(fit1)
# reliability(fit1)
# 
# # Exporting lavaan output to tables
# # There are many possibilities but these are some parameters I use on a daily basis
# htmlTable(txtRound(parameterEstimates(fit, ci = TRUE, level = 0.95), digits=3, excl.cols=1), align="l|r|r|r|r|r|r|r|r|r")
# htmlTable(txtRound(reliability(fit),3), align="l|r|r|r|r")
####################################################

#############################
# Frequencies and crosstabs #
#############################

# # Describing continuous variables (here we do not have any)
# # sjmisc::descr(x=my_data[c("gender","income_agg")], out="viewer")
# 
# # Describing categorical variables
# sjmisc::frq(x=my_data[c("gender","income_agg")], out="viewer")
# 
# # Simple crosstab
# my_table = xtabs(formula =~ gender + income_agg, data=my_data)
# ftable(x=my_table) # Only counts
# my_table_perc = (my_table / nrow(my_data))*100
# ftable(x=my_table_perc) # Only percentages
# 
# # Creating cross-tabulations for categorical variables
# sjPlot::sjt.xtab(var.row=my_data$gender,
#                  var.col=my_data$income_agg,
#                  show.col.prc=TRUE)
# 
# sjPlot::sjt.xtab(var.row=my_data$age,
#                  var.col=my_data$educ_aggre,
#                  show.col.prc=TRUE)
# 
# # Using another library for crosstabs
# gmodels::CrossTable(x=my_data$age, # Row variable
#                     y=my_data$educ_aggre, # Column variable
#                     prop.r = F,
#                     prop.t = F,
#                     prop.chisq = F,
#                     format = "SPSS")

