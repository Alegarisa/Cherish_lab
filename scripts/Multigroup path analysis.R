#Parce multigroup path analysis#

# packages <- c("dplyr", "tidyr", "haven", "car", "lme4",
#               "emmeans", "lmerTest", "rstatix", "performance",
#               "ggplot2", "afex", "sjPlot", "jtools", "lavaan", "semPlot")
# for (package in packages) {
#   if (!require(package, character.only = TRUE, quietly = TRUE)) {
#     install.packages(package)
#     library(package, character.only = TRUE)
#   }
# }

library(dplyr)
library(tidyr)
library(haven)
library(car)
library(lme4)
library(emmeans)
library(lmerTest)
library(rstatix)
library(performance)
library(ggplot2)
library(lme4)
library(afex)
library(sjPlot)
library(jtools)
library(lavaan)
library(semPlot)
library(lavaanPlot)


pa<-read.csv("D:/Documents/1. Postdoc/1. Papers/Cherish_lab/data/Mexican Pop 07.17.25.csv")

pa2<- pa[!is.na(pa$back_migration), ]
pa2$back_migration<-as.factor(pa2$back_migration)

#modelo
a<-"
  #Variables endógenas
    ANXIETY ~ FPINT + FRFC + PSS + income_mx
    DEPRESS ~ FPINT + FRFC + PSS + income_mx
    AUDIT_C ~ FPINT + FRFC + PSS + income_mx
    
  #Variables mediadoras
    FPINT ~ PSS + income_mx
    FRFC ~ PSS + income_mx

  #Covarianzas
  PSS ~~ income_mx
  FPINT ~~ FRFC
"
gpa<-sem(a, data=pa2, estimator ="MLMV", se = "robust", group = "back_migration",
         group.equal=c("intercepts"))
summary(gpa, fit.measures=T, standardized=T, rsquare=T)



#Modelo con multigroup path name
p<-"
 # Variables endógenas (endogenous variables)
    ANXIETY ~ c(intANX1, intANX2)*FPINT + c(conANX1, conANX2)*FRFC + PSS + income_mx
    DEPRESS ~ c(intDEP1, intDEP2)*FPINT + c(conDEP1, conDEP2)*FRFC + PSS + income_mx
    AUDIT_C ~ c(intAUD1, intAUD2)*FPINT + c(conAUD1, conAUD2)*FRFC + PSS + income_mx

# Variables mediadoras
    FPINT ~ c(pssINT1, pssINT2)*PSS + c(monINT1, monINT2)*income_mx
    FRFC  ~ c(pssCON1, pssCON2)*PSS + c(monCON1, monCON2)*income_mx

# Covarianzas (covariances)
    PSS ~~ income_mx
    FPINT ~~ FRFC

# Efectos indirectos para grupo 1
#¿Quién es? Es 3
    pssINTanx1 := pssINT1 * intANX1
    pssCONanx1 := pssCON1 * conANX1
    pssINTdep1 := pssINT1 * intDEP1
    pssCONdep1 := pssCON1 * conDEP1
    pssINTaud1 := pssINT1 * intAUD1
    pssCONaud1 := pssCON1 * conAUD1
    
    #Biyuyo
    monINTanx1 := monINT1 * intANX1
    monCONanx1 := monCON1 * conANX1
    monINTdep1 := monINT1 * intDEP1
    monCONdep1 := monCON1 * conDEP1
    monINTaud1 := monINT1 * intAUD1
    monCONaud1 := monCON1 * conAUD1

#Efectos Indirectos para grupo 2
#¿Quién es? Es 3
    pssINTanx2 := pssINT2 * intANX2
    pssCONanx2 := pssCON2 * conANX2
    pssINTdep2 := pssINT2 * intDEP2
    pssCONdep2 := pssCON2 * conDEP2
    pssINTaud2 := pssINT2 * intAUD2
    pssCONaud2 := pssCON2 * conAUD2

# Biyuyo
    monINTanx2 := monINT2 * intANX2
    monCONanx2 := monCON2 * conANX2
    monINTdep2 := monINT2 * intDEP2
    monCONdep2 := monCON2 * conDEP2
    monINTaud2 := monINT2 * intAUD2
    monCONaud2 := monCON2 * conAUD2
"

#prueba
gpa<-sem(p, data=pa2, estimator ="MLMV", se = "robust", group = "back_migration",
         group.equal=c("intercepts", "residuals", "residual.covariances"))
summary(gpa, fit.measures=T, standardized=T, rsquare=T)

parameters::model_parameters(gpa, component = c("regression", "residual"))

semPaths(gpa, whatLabels = "std", style = "ram", edge.label.cex = 1.5, sizeMan = 8,
            edge.color = ifelse(parameters::model_parameters(gpa, ci=0.95, component = c("regression", "residual"))$pd > .9,
                                "black", "gray90"), label.prop = 0.9, layout = "tree2", rotation = 2, nCharNodes = 0, residuals = F,
            intercepts = F, rescale=T)
