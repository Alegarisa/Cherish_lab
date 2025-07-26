# #Parce multigroup path analysis#
packages <- c("dplyr", "tidyr", "haven", "car", "lme4",
              "emmeans", "lmerTest", "rstatix", "performance",
              "ggplot2", "afex", "sjPlot", "jtools", "lavaan",
              "semPlot", "lavaanPlot", "semptools")
for (package in packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

pa<-read.csv("D:/Documents/1. Postdoc/1. Papers/Cherish_lab/data/Mexican Pop 07.17.25.csv")

pa2<- pa[!is.na(pa$back_migration), ]
pa2$back_migration<-as.factor(pa2$back_migration)

#Modelo con multigroup path names para efectos indirectos y totales por grupo####
p<-"
#Covariables a estrés
    PSS ~ income_mx + age + sex + education
    
# Variables endógenas (endogenous variables)
    ANXIETY ~ c(intANX1, intANX2)*FPINT + c(conANX1, conANX2)*FRFC + c(pssANX1, pssANX2)*PSS
    DEPRESS ~ c(intDEP1, intDEP2)*FPINT + c(conDEP1, conDEP2)*FRFC + c(pssDEP1, pssDEP2)*PSS
    AUDIT_C ~ c(intAUD1, intAUD2)*FPINT + c(conAUD1, conAUD2)*FRFC + c(pssAUD1, pssAUD2)*PSS

# Variables mediadoras
    FPINT ~ c(pssINT1, pssINT2)*PSS
    FRFC  ~ c(pssCON1, pssCON2)*PSS

# Covarianzas
    FPINT ~~ FRFC
    ANXIETY ~~ AUDIT_C + DEPRESS
    AUDIT_C ~~ DEPRESS
      # sex ~~ income_mx + age + education
      income_mx ~~ age + education
      age ~~ education
      
# Efectos indirectos para grupo 1
#¿Quién es? Es 3
    pssINTanx1 := pssINT1 * intANX1
    pssCONanx1 := pssCON1 * conANX1
    pssINTdep1 := pssINT1 * intDEP1
    pssCONdep1 := pssCON1 * conDEP1
    pssINTaud1 := pssINT1 * intAUD1
    pssCONaud1 := pssCON1 * conAUD1

#Efectos Indirectos para grupo 2
#¿Quién es? Es 3
    pssINTanx2 := pssINT2 * intANX2
    pssCONanx2 := pssCON2 * conANX2
    pssINTdep2 := pssINT2 * intDEP2
    pssCONdep2 := pssCON2 * conDEP2
    pssINTaud2 := pssINT2 * intAUD2
    pssCONaud2 := pssCON2 * conAUD2
    
#Totales
    TOTpssANX1 := pssANX1 + pssINTanx1
    TOTpssDEP1 := pssDEP1 + pssINTdep1
    TOTpssAUD1 := pssAUD1 + pssCONaud1
    TOTpssANX2 := pssANX2 + pssINTanx2
    TOTpssDEP2 := pssDEP2 + pssINTdep2
    TOTpssAUD2 := pssAUD2 + pssCONaud2
"

#Output modelo paralelo####
gpa<-sem(p, data=pa2, estimator ="MLMV", se = "robust", group = "back_migration")
summary(gpa, fit.measures=T, standardized=T, rsquare=T)

#Diagrama####
forma <- matrix(
  c("sex",        NA,     "FPINT",      NA,
    NA,           NA,     NA, "ANXIETY",
    NA,           NA,     NA,      NA,
    "income_mx",  NA,     NA,      NA,
    NA,           "PSS",  NA,      "AUDIT_C",
    "age",        NA,     NA,      NA,
    NA,           NA,     NA,      NA,
    NA,           NA,     NA,  "DEPRESS",
    "education",  NA,     "FRFC",      NA),
  ncol = 4,
  byrow = TRUE)

#Diagrama por grupo gpa2 con regresiones constreñidas####
d<-semPaths(gpa, whatLabels = "std", style = "ram", edge.label.cex = 1.5, sizeMan = 8,
            edge.color = ifelse(parameters::model_parameters(gpa, 
            component = c("regression", "correlation"))$p < .05, "black", "gray90"),
            label.prop = 0.9, layout = forma, nCharNodes = 0, residuals = F, 
            intercepts = F, rescale=T)
curves<-c("sex ~~ income_mx" = 2, "sex ~~ age" = 2, "sex ~~ education" = -2,
          "income_mx ~~ age" = -2, "income_mx ~~ education" = -2,
          "age ~~ education" = -2, "FPINT ~~ FRFC" = -2,
          "ANXIETY ~~ AUDIT_C" = 2, "ANXIETY ~~ DEPRESS" = 2,
          "AUDIT_C ~~ DEPRESS" = -2)

#Tidy-ish diagrams####
migra <- semptools::set_edge_label_position(d[[1]],
                                            c(c("DEPRESS ~ FPINT" = .4),
                                              c("AUDIT_C ~ PSS" = .5),
                                              c("AUDIT_C ~ FPINT" = .6),
                                              c("AUDIT_C ~ FRFC" = .6),
                                              c("ANXIETY ~ FRFC" = .4)))
migra<- set_curve(migra, curves)
mexa <- semptools::set_edge_label_position(d[[2]],
                                           c(c("DEPRESS ~ FPINT" = .4),
                                             c("AUDIT_C ~ PSS" = .5),
                                             c("AUDIT_C ~ FPINT" = .6),
                                             c("AUDIT_C ~ FRFC" = .6),
                                             c("ANXIETY ~ FRFC" = .4)))
mexa<- set_curve(mexa, curves)

#Diagramas finales#
plot(migra)
plot(mexa)

#Diagramas finales#
plot(migra)
title(main="Familias con migración de retorno", cex.main = 1.5, 
      col.main = "black", line =2)
plot(mexa)
title(main="Familias sin migración", cex.main = 1.5, 
      col.main = "black", line = 2)
