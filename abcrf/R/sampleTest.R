# Une fonction qui prend comme arguments :
# data : le training data utilisé pour la RF de classif (qui doit contenir en plus les paramètres!!!)
# weightsObs : les poids pour la première RF de classif.
# modelObs : le scenario sélectionné au final.
# nToSample :  un entirer indiquant combien on veut de données.
# bootstrap : un logical qui dit si on doit avoir des replicats ou non dans la table finale, si non on prend les nToSample données avec le plus fort poids.
# *group used* : Si grouping, il va falloir s'assurer que l'on tire dans le groupe associé.

# On va tirer dans ces individus qui en plus sont du scenario choisi, un nombre nToSample, bootstrap avec remise si pas assez de données, sinon on prends les meilleurs.

makeTestTable <- function(data, weightsObs, modelObs, nToSample, bootstrap){
  
  # Je dois récupérer la réponse
  # Dans ces réponses, prendre les éléments qui ont le scénario voulu
  # Je croise avec ceux qui ont un poids non nuls
  # Il faut vérifier qu'il y en a bien nToSample
  
  nData <- nrow(data)
  
  idxMod <- which(data$modelIndex == modelObs)
  
  if(bootstrap){
    
    idxTest <- sample(idxMod, size=nToSample, replace=TRUE, prob = weightsObs[idxMod])

  } else {
    
    min(c(nToSample, sum(weightsObs!=0)))
    
    idxWeightsGivenMod <- order(weightsObs[idxMod], decreasing = TRUE)[1:nToSample]
    
    idxTest <- idxMod[indWeightsGivenMod] # index of the individuals of the desired model with the nToSample best weights

  }
  
  return(data[idxTest,,drop=FALSE])
  
}

# Test 

poids <- c(0.2,0.3,0.4,0.5,0.1,0.2,0.1,0.4,0.8,0.01)
modele <- c(1,1,1,2,2,2,3,3,3,1)
indMod <- which(modele==1)
indWeightsGivenMod <- order(poids[indMod], decreasing = TRUE)[1:3]
poids[indMod][indWeightsGivenMod] # les 3 meilleurs poids sachant le modèle

indMod[indWeightsGivenMod] # C'est les indices des 3 individus dans la classe 1 avec le meilleur poids.
