rm(list = ls())

###########
# CONTEXT #
###########

# Chemin d'accès au package ----
dir = "C:/Users/STOC/Documents/Mathilde Vimont/Tendances/Code/V1/"

scrDir = paste0(dir, "scripts/")
dataDir = paste0(dir, "data/")

# Importation des librairies ----
source(paste0(scrDir, "basic/libraries.R"))

# Importation des paramètres ----
source(paste0(scrDir, "basic/parameters.R"))

# Importation des fonctions de base ----
source(paste0(scrDir, "functions/baseFunctions.R"))

#############################
# DATA TREATMENT & ANALYSIS #
#############################
       
# Importation des données ----
source(paste0(scrDir, "functions/importData.R"))
data <- importData(path = paste0(dataDir, "abondance/"))

# Vérification de l'existence d'absences dans les données ----
source(paste0(scrDir, "functions/fillAbsence.R"))
data <- fillAbsence(data = data, method = "once")

# Statistiques descriptives sur l'ensemble du jeu de données

rmarkdown::render(
  input  = paste0(scrDir, 'statsDesc.Rmd'),
  params = list())

# Analyses sur chacune des espèces

speciesList <- unique(unlist(data[, "species"]))

for (sp in speciesList){
  
  # Extraire les indices correspondant à l'espèce sp
  indSp <- which(data[,"species"] == sp)
  
  # Extraire les données issues de l'espèce sp
  dataSp <- data[indSp,]
  
  # Chiffres clés et statistiques descriptives pour l'espèce sp
  rmarkdown::render(
    input  = paste0(scrDir, 'rmd/preDiagnostic.Rmd'),
    params = list(sp = sp,
                  data = dataSp,
                  interestVar = interestVar,
                  fixedEffects = fixedEffects,
                  randomEffects = randomEffects,
                  factorVariables = factorVariables))
  
  # Calculer la tendance annuelle pour cette espèce  
  source(paste0(scrDir, "functions/tendencyModeling.R"))
  annualModel <- tendencyModeling(data = dataSp, 
                                  interestVar = interestVar,
                                  fixedEffects = fixedEffects,
                                  randomEffects = randomEffects,
                                  factorVariables = factorVariables, 
                                  method = method,
                                  distribution = distrib,
                                  scaling = scaling)
  
  # Reformater les sorties du modèle - BRUT
  source(paste0(scrDir, "functions/summaryOutput.R"))
  sum <- summaryOutput(model = annualModel, 
                       data = dataSp, 
                       distribution = distrib, 
                       transform = F,
                       scaling = scaling)

  # Reformater les sorties du modèle - TRANSFORME  
  sum <- summaryOutput(model = annualModel, 
                       data = dataSp, 
                       distribution = distrib, 
                       transform = T,
                       scaling = scaling)
  
  # Extraire les tendances calculées
  varRange <- max(data$year) - min(data$year) + 1
  
  source(paste0(scrDir, "functions/analyseCoef.R"))
  coefs <- analyseCoef(model = annualModel, 
                     distribution = distrib, 
                     effectVar = "year",
                     varRange = varRange)
  
  # Analyser les tendances à partir des sorties du modèle
  print(coefs)
  
  # Calculer la tendance interannuelle
  interAnnualModel <- tendencyModeling(data = dataSp, 
                                       interestVar = interestVar,
                                       fixedEffects = fixedEffects,
                                       randomEffects = randomEffects,
                                       factorVariables = c("year", factorVariables), 
                                       method = method,
                                       distribution = distrib,
                                       scaling = scaling)
  
  sum <- summaryOutput(model = interAnnualModel, 
                       data = dataSp, 
                       distribution = distrib, 
                       transform = F,
                       scaling = scaling)
  
  # Reformater les sorties du modèle - TRANSFORME  
  sum <- summaryOutput(model = interAnnualModel, 
                       data = dataSp, 
                       distribution = distrib, 
                       transform = T,
                       scaling = scaling)
  
  # Extraire les tendances calculées
  varRange <- max(data$year) - min(data$year) + 1
  
  source(paste0(scrDir, "functions/analyseCoef.R"))
  coefs <- analyseCoef(model = annualModel, 
                       distribution = distrib, 
                       effectVar = "year",
                       varRange = varRange)
  
  }


# GLM pour indicateurs inter-annuels ----
if (interannualIndicators){
  interannualModel <- tendencyModeling(data = data, 
                                       interestVar = interestVar,
                                       fixedEffects = fixedEffects,
                                       randomEffects = randomEffects,
                                       factorVariables = c(factorVariables, "year"),
                                       method = method,
                                       distribution = distrib,
                                       scaling = scaling)
}


# Diagnostic des données ----
if(preDiag){
  
  rmarkdown::render(
    input  = paste0(scrDir, 'preDiagnostic.Rmd'),
    params = list(scrDir = scrDir,
                  data = data, 
                  fixedEffects = fixedEffects,
                  randomEffects = randomEffects,
                  factorVariables = factorVariables))
  
  source(paste0(scrDir, "rmd/preDiagnostic.Rmd"))
}



# Diagnostic pré-analyses ----
if(vifDiag){
  
}


# Diagnostic post-analyses ----
if(resDiag){
  
}


# Export & visualisation des résultats ----
if(results){
  
}