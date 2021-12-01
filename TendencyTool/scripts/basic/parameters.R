####################################
# Paramètres : périmètre d'intérêt #
####################################

# Quelles espèces souhaitez-vous analyser ?
speciesList <- c()

# Quelles sont les dates de début et de fin à prendre en compte ?
yearRange <- c()

####################################
# Paramètres : analyses à réaliser #
####################################

# Voulez-vous faire un diagnostic des données ?
# (Abondance moyenne, nombre d'occurrences, ...)
dataDiag <- T

# Voulez-vous faire un diagnostic du VIF ?
vifDiag <- T

# Voulez-vous faire les analyses continues ?
annualIndicators <- T

# Voulez-vous faire les analyses inter-annuelles ?
interannualIndicators <- T

# Voulez-vous analyser et exporter les résultats ?
results <- T

#########################################
# Paramètres : méthodologie à appliquer #
#########################################

# Quelle est la variable à étudier ?
interestVar <- "count"

# Quels sont les effets fixes à considérer ?
fixedEffects <- c("year", "longitude", "latitude")

# Quels sont les effets aléatoires à considérer ?
randomEffects <- c("site_id")

# Quels sont les variables catégorielles ?
factorVariables <- c()

# Quel type de modèle appliquer ?
model <- "glm"

# Quelle hypothèse de distribution faire ?
distrib <- "poisson"

# Doit-on normaliser les variables ?
scaling <- F