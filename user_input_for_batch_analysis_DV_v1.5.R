#####################################################################################################
# User inputs required for batch analysis of temperature daily value data
# 
# 
# Jason Williams, IDEQ Lewiston Regional office; Robert Esquivel, IDEQ State Office
# last update: 2-17-2021
####################################################################################################

####################################################################################################
# USER INPUT
####################################################################################################

####################################################################################################
# PART 1: USER INPUT
####################################################################################################

# specify project name; the excel output files will have this in the filename; NO SPACES IN NAME!
projectname <-"template_example"

# specify name of excel file with data
filename <-"template_for_batch_analysis.xlsx"

# specify names of tabs in template excel file
data.tab = "Data"
criteria.tab = "when criteria apply"


# flag & exclude days where daily mean differs from previous or subsequent daily mean above this value
# If you don't want to include this threshold in the analysis, enter 100

consecutive.mean.threshold <- 3

# flag & exclude days where daily max differs from previous or subsequent daily max above this value
# If you don't want to include this threshold in the analysis, enter 100

consecutive.max.threshold <- 10

# flag & exclude days with daily max above this threshold
# If you don't want to include this threshold in the analysis, enter 100

daily.max.threshold <-28

# flag & exclude days during October-May with daily mean above this threshold
# If you don't want to include this threshold in the analysis, enter 100
oct.to.may.mean.threshold <-15

# flag & exclude days with daily range above this threshold
# if you don't want to include this threshold in the analysis, enter 100
daily.range.threshold <-20

# flag & exclude days where # observations does not exceed this threshold
# if you don't want to include this threshold in the analysis, enter 1000
minimum.daily.obs.threshold <-18

# require that each site/day combo have # of daily observations reported
# enter "Y" to exclude cases where # obs not reported, or "N"
require.Nobs <-"N"

###################################################################################################
# DO NOT MODIFY
###################################################################################################
# creates data frame with user inputs; will be included in excel output

# creates data frame with user inputs; will be included in excel output

parameter <-c("project", "filename","consecutive.mean.threshold", "consecutive.max.threshold", 
              "daily.max.threshold","oct.to.may.mean.threshold", 
              "daily.range.threshold", "minimum.daily.obs.threshold", 
              "require.Nobs", "run date")
user_input <-c(projectname, filename, consecutive.mean.threshold, consecutive.max.threshold, 
               daily.max.threshold, oct.to.may.mean.threshold, daily.range.threshold,
               minimum.daily.obs.threshold, require.Nobs, date())

user_inputs <-data.frame(cbind(parameter, user_input))
