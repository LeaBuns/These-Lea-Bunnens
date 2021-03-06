# Nettoyer la console
rm(list = ls())


# Chargement des extensions
library(QCA); 
library(SetMethods)


# D�finition du dossier de travail
setwd("E:/th�se/Etude Empirique/0 - Etude logicielle")

# Nommer fichier de travail
QCA1 <- read.csv("qca1.csv", row.names = 1, sep = ";")


# V�rification du fichier : montre le d�but du fichier
head(QCA1)

# Transformer les noms en majuscules
names(QCA1) <- toupper(names(QCA1))

### ANALYSE DE NECESSITE ###

# Test de toutes les conditions et leur n�gation
QCAfit(QCA1[, 1 : 6], QCA1$REU, necessity = TRUE, names(QCA1[, 1 : 6]))

### ANALYSE DE SUFFISANCE ###

# Cr�ation table de v�rit�
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


# Montrer les hypoth�ses simplificatrices
sol_p$SA

# Solution interm�diaire. 
sol_i <- minimize(TT, details = TRUE, show.cases=TRUE, include = "?", dir.exp = c(1,"-",1,"-",1,"-"))
sol_i

# Voir les hypoth�ses incluses dans la solution interm�diaire
sol_i$i.sol$C1P1$EC

# Graphique de la solution interm�diaire
pimplot(data = QCA1, results=sol_i,
        outcome="REU",
        crisp=TRUE)



### TESTS DE ROBUSTESSE ###


# Seuil de consistance � 0.7
TTR1 <- truthTable(QCA1, outcome = "REU", 
                   conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                   incl.cut = 0.7,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("n"))
TTR1

# Seuil de consistance � 0.9

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

# Solution interm�diaire sans Apple 
solia <- minimize(ttcas, details = TRUE, include = "?", dir.exp = c(1,1, "-", "-", 1, "-"))
solia


### ANALYSE POST QCA ###

# Identification des cas d�viants en consistance

smmr(sol_i, outcome = "REU", match = FALSE, 
    cases = 3)

# Identification des cas d�viants en couverture

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

# Association cas d�viants en couverture & non-pertinents:

smmr(sol_i, outcome = "REU", match = TRUE, 
    cases = 4)


