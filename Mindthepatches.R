#=================================================================================================================

# Mind the patches: Mind the patches: Exploring the optimization of large-scale camera trap sampling through 
# clustered grids. 2026. Ecosphere.

# Jorge Sereno-Cadierno, Mizar Torrijo-Salesa, Roberto Pascual-Rico, Marta Monfort-Calatayud & Pelayo Acevedo

# script materials

##===================================================================================================================##


################################################################################
#                                                                              #
# Analyses                                                                     #
#                                                                              #
################################################################################

data <- read.csv("allresultsCuencav2.1.csv", sep = ";", dec=".", header=TRUE, as.is=TRUE) #

# Load libraries

library(glmmTMB)
library(dplyr)
library(performance)

################################################################################
#                          Previous transformations                            #
################################################################################

data$type_clust<-as.factor(ifelse(grepl("clust", data$type,ignore.case = TRUE), 1,0))
data$type_rand<-as.factor(ifelse(grepl("rand", data$type,ignore.case = TRUE), 1,0))
data$type_syst<-as.factor(ifelse(grepl("sist", data$type,ignore.case = TRUE), 1,0))
data$indTot<-as.numeric(data$indTot)
data$effort<-as.factor(data$effort)

# Regular models

# activity 

modelo<-glmmTMB::glmmTMB(act~type_clust*distr+(1|sp),data=data, family = beta_family)
drop1(modelo, test="Chisq")
summary(modelo)
performance::r2(modelo)

# day range 
modelo<-glmmTMB::glmmTMB(s.km.day.~type_clust+distr+(1|sp),data=data, family = lognormal)
check_collinearity(modelo)
performance::r2(modelo)
summary(modelo)

# trapping rate
modelo<-glmmTMB::glmmTMB(indTot~type_clust*distr+offset(log(t..days.))+(1|sp),data=data, family = nbinom2)
drop1(modelo, test="Chisq")
check_collinearity(modelo)
performance::r2(modelo)
summary(modelo)

# density

modelo<-glmmTMB::glmmTMB(d.ind.km2.~type_clust*distr+effort+(1|sp),data=data, family = lognormal)
drop1(modelo, test="Chisq")
check_collinearity(modelo)
performance::r2(modelo)
summary(modelo)

################################################################################

#CV models

#activity
data$CV_act <- data$act_se / data$act
modelo<-glmmTMB::glmmTMB(CV_act~type_clust*distr+effort+(1|sp),data=data, family = beta_family)
summary(modelo)
drop1(modelo, test="Chisq")

#day range
data$CV_dr <- data$s_se.km.day. / data$s.km.day.
modelo<-glmmTMB::glmmTMB(CV_dr~type_clust*distr+effort+(1|sp),data=data, family = beta_family())
AIC(modelobeta, modelognor, modelopois, modeloneg)
drop1(modelo, test="Chisq")

#trapping rate
data$CV_tr <- data$tr_se / data$tr
modelo<-glmmTMB::glmmTMB(CV_tr~type_clust*distr+effort+(1|sp),data=data,family = beta_family())
drop1(modelo, test="Chisq")

#density
data$CV_d <- data$d_se.ind.km2. / data$d.ind.km2.
modelo<-glmmTMB::glmmTMB(CV_d~type_clust+effort+distr+(1|sp),data=data, family = beta_family())
summary(modelo)
drop1(modelo, test="Chisq")
