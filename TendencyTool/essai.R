rm(list = ls())

###########
# CONTEXT #
###########

# Chemin d'accès au package ----
dir = "C:/Users/STOC/Documents/Mathilde Vimont/Tendances/Code/V1/"

resDir = paste0(dir, "results/")
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

# Analyses sur chacune des espèces

speciesList <- unique(unlist(data[, "species"]))


# Initialize lists
coefList <- list()
modelList <- list()
summaryList <- list()
plotList <- list()

for (sp in speciesList[1:10]){
  
  # Création du répertoire dédié à l'espèce sp
  dir.create(path = paste0(resDir, "/", sp), showWarnings = F)
  
  # Extraire les indices correspondant à l'espèce sp
  indSp <- which(data[,"species"] == sp)
  
  # Extraire les données issues de l'espèce sp
  dataSp <- data[indSp,]
  
  ####################
  # TENDANCE GLOBALE #
  ####################
  
  # Calculer la tendance annuelle pour cette espèce  
  source(paste0(scrDir, "functions/makeGLM.R"))
  annualModel <- makeGLM(data = dataSp, 
                         interestVar = interestVar,
                         fixedEffects = fixedEffects,
                         randomEffects = randomEffects,
                         factorVariables = factorVariables, 
                         method = method,
                         distribution = distrib,
                         scaling = FALSE)
  
  # Reformater les sorties du modèle - BRUT
  source(paste0(scrDir, "functions/summaryOutput.R"))
  annualSum <- summaryOutput(model = annualModel, 
                             distribution = distrib, 
                             factorVariables = factorVariables,
                             transform = T,
                             scaling = FALSE)
  
 
  # Extraire les tendances calculées
  varRange <- max(dataSp$year) - min(dataSp$year) + 1
  
  source(paste0(scrDir, "functions/analyseCoef.R"))
  coefs <- analyseCoef(model = annualModel, 
                       distribution = distrib, 
                       effectVar = "year",
                       varRange = varRange)
  
  # Analyser les tendances à partir des sorties du modèle
  print(coefs)
  
  #############################
  # VARIABILITE INTERANNUELLE #
  #############################
  
  # Calculer la tendance annuelle pour cette espèce  
  source(paste0(scrDir, "functions/makeGLM.R"))
  interAnnualModel <- makeGLM(data = dataSp, 
                              interestVar = interestVar,
                              fixedEffects = fixedEffects,
                              randomEffects = randomEffects,
                              factorVariables = c("year",factorVariables), 
                              method = method,
                              distribution = distrib,
                              scaling = FALSE)
  
  # Reformater les sorties du modèle - BRUT
  source(paste0(scrDir, "functions/summaryOutput.R"))
  interAnnualSum <- summaryOutput(model = interAnnualModel, 
                                  distribution = distrib, 
                                  factorVariables = c("year",factorVariables),
                                  transform = T,
                                  scaling = FALSE)
  
  
  source(paste0(scrDir, 'functions/plotGLM.R'))
  plot <- plotGLM(summary = interAnnualSum, effect = "year", 
                  distribution = distrib)
  
  ##################
  # FILL THE LISTS #
  ##################
  summaryList[[sp]] <- list(annual = annualSum, interAnnual = interAnnualSum)
  coefList[[sp]] <- list(coefs)
  modelList[[sp]] <- list(annual = annualModel, interAnnual = interAnnualModel)
  plotList[[sp]] <- list(plot)

  
  rmarkdown::render(
    input  = paste0(scrDir, 'rmd/essai.Rmd'),
    output_dir = paste0(resDir, "/", sp),
    output_file = "Analysis",
    params = list(sp = sp,
                  data = dataSp, 
                  interestVar = interestVar,
                  fixedEffects = fixedEffects,
                  randomEffects = randomEffects,
                  factorVariables = factorVariables,
                  annualModel = annualModel,
                  annualSum = annualSum,
                  varRange = varRange,
                  coefs = coefs,
                  interAnnualModel = interAnnualModel,
                  interAnnualSum = interAnnualSum,
                  plot = plot))
  
  
  }
