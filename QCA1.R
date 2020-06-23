# Nettoyer la console
rm(list = ls())


# Chargement des extensions
library(QCA); 
library(SetMethods)


# Définition du dossier de travail
setwd("E:/thèse/Etude Empirique/0 - Etude logicielle")

# Nommer fichier de travail
QCA1 <- read.csv("qca1.csv", row.names = 1, sep = ";")


# Vérification du fichier : montre le début du fichier
head(QCA1)

# Transformer les noms en majuscules
names(QCA1) <- toupper(names(QCA1))

### ANALYSE DE NECESSITE ###

# Test de toutes les conditions et leur négation
QCAfit(QCA1[, 1 : 6], QCA1$REU, necessity = TRUE, names(QCA1[, 1 : 6]))

### ANALYSE DE SUFFISANCE ###

# Création table de vérité
TT <- truthTable(QCA1, outcome = "REU",
                  conditions = c("PREC", "ORGA", "ENGAG", "RENT", "KPI", "RENV"),
                     incl.cut1 = 0.8,
                     complete = FALSE,
                 show.cases = TRUE,
                     sort.by = c("n"))
TT

# Solution conservatrice
sol_c <- minimize(TT, show.cases=FALSE, details=TRUE)
sol_c


# Solution parcimonieuse
sol_p <- minimize(TT, details = TRUE, include = "?")
sol_p


# Montrer les hypothèses simplificatrices
sol_p$SA

# Solution intermédiaire. 
sol_i <- minimize(TT, details = TRUE, show.cases=TRUE, include = "?", dir.exp = c(1,"-",1,"-",1,"-"))
sol_i

# Voir les hypothèses incluses dans la solution intermédiaire
sol_i$i.sol$C1P1$EC

# Graphique de la solution intermédiaire
pimplot(data = QCA1, results=sol_i,
        outcome="REU",
        crisp=TRUE)



### TESTS DE ROBUSTESSE ###


# Seuil de consistance à 0.7
TTR1 <- truthTable(QCA1, outcome = "REU", 
                   conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                   incl.cut = 0.7,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("n"))
TTR1

# Seuil de consistance à 0.9

ttcons1 <- truthTable(QCA1, outcome = "REU", 
                      conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                      incl.cut = 0.9,
                      show.cases = TRUE,
                      complete = FALSE,
                      sort.by = c("incl", "n"))
ttcons1

# Suppression du cas Apple

ROB3 <- read.csv("qca1_rob2.csv", row.names = 1, sep = ";")
head(ROB3)

QCAfit(ROB3[, 1:6], ROB3$REU, necessity = TRUE, names(ROB1[, 1:6]))

ttcas <- truthTable(ROB3, outcome = "REU", 
                    conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                    incl.cut = 0.8,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("n"))
ttcas

# Solution intermédiaire sans Apple 
solia <- minimize(ttcas, details = TRUE, include = "?", dir.exp = c(1,1, "-", "-", 1, "-"))
solia


### ANALYSE POST QCA ###

# Identification des cas déviants en consistance

smmr(sol_i, outcome = "REU", match = FALSE, 
    cases = 3)

# Identification des cas déviants en couverture

smmr(sol_i, outcome = "REU", match = FALSE, 
    cases = 4)

# Association de cas typiques & non-pertinents pour chaque chemin

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 2, term =1)

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 2, term =2)

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 2, term =3)

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 2, term =4)

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 2, term =5)

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 2, term =6)

# Association cas déviants en couverture & non-pertinents:

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 4)


