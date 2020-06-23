# Nettoyer la console
rm(list = ls())


# Chargement des extensions
library(QCA); library(SetMethods)


# Définition du dossier de travail
setwd("E:/thèse/Etude Empirique/0 - Etude logicielle")

# Nommer fichier de travail
QCA2 <- read.csv("qca2.csv", row.names = 1, sep = ";")

# Vérification du fichier : montre le début du fichier
head(QCA2)
 
# Transformer les noms en majuscules
names(QCA2) <- toupper(names(QCA2))

### ANALYSE DE NECESSITE ###

# Test de toutes les conditions et leur négation
QCAfit(QCA2[, 1:6], QCA2$PERF, necessity = TRUE, names(QCA2[, 1:6]))

### ANALYSE DE SUFFISANCE ###

# Création table de vérité


TT2 <- truthTable(QCA2, outcome = "PERF", 
                     conditions = c("PREC", "ORGA", "ENGAG", "RENT", "KPI", "RENV"),
                     incl.cut = 0.8,
                     show.cases = TRUE,
                     complete = FALSE,
                     sort.by = c("incl", "n"))
TT2


# Solution conservatrice
sol2_c <- minimize(TT2, show.cases = TRUE, details = TRUE)
sol2_c


# Solution parcimonieuse
sol2_p <- minimize(TT2, details = TRUE, include = "?")
sol2_p

# Montrer les hypothèses simplificatrices
sol2_p$SA

# Solution intermédiaire. 
sol2_i <- minimize(TT2, details = TRUE, show.cases=TRUE, include = "?", dir.exp = c("-","-", 1, "-", 1, "-"))
sol2_i

# Voir les hypothèses incluses 
sol2_i$i.sol$C1P1$EC

# Graphique de la solution intermédiaire
pimplot(QCA2bis, sol2_ib, "PERF", crisp=TRUE)


### TESTS DE ROBUSTESSE ###


# Seuil de consistance à 0.7
TTR2 <- truthTable(QCA2, outcome = "PERF", 
                   conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                   incl.cut = 0.7,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("incl", "n"))
TTR2


sol2_ri <- minimize(TTR2, details = TRUE, include = "?", dir.exp = c("-",1, "-", "-", 1, "-"))
sol2_ri

# Seuil de consistance à 0.9

ttcons <- truthTable(QCA2, outcome = "PERF", 
                     conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                     incl.cut = 0.9,
                     show.cases = TRUE,
                     complete = FALSE,
                     sort.by = c("incl", "n"))
ttcons

solp_cons <- minimize(ttcons, details = TRUE, include = "?")
solp_cons

soli_cons <- minimize(ttcons, details = TRUE, include = "?", dir.exp = c("-",1, "-", "-", 1, "-"))
soli_cons

# Suppression du cas Apple

ROB4 <- read.csv("qca2_rob2.csv", row.names = 1, sep = ";")


QCAfit(ROB4[, 1:6], ROB4$PERF, necessity = TRUE, names(ROB4[, 1:6]))

ttcas <- truthTable(ROB4, outcome = "PERF", 
                    conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                    incl.cut = 0.8,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("incl", "n"))
ttcas


solpa <- minimize(ttcas, details = TRUE, include = "?")
solpa


solia <- minimize(ttcas, details = TRUE, include = "?", dir.exp = c("-",1, "-", "-", 1, "-"))
solia


### ANALYSE POST QCA ###

# Cas typiques
smmr(sol2_i, outcome = "PERF", match = FALSE, 
    cases = 1)

# Cas déviants en consistance

smmr(sol2_i, outcome = "PERF", match = FALSE, 
    cases = 3)

# Cas déviants en couverture

smmr(sol2_i, outcome = "PERF", match = FALSE, 
    cases = 4)

### ANALYSE NEGATION ##

# Analyse de nécessité pour négation
QCAfit(QCA2[, 1:6], QCA2$PERF, 
       neg.out = T,
       necessity = TRUE, paste(names(QCA2[, 1:6])))


# Analyse de suffisance pour la négation
TT_n <- truthTable(QCA2, outcome = "PERF", neg.out = TRUE,
                     conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                     incl.cut = 0.8,
                     show.cases = TRUE,
                     complete = FALSE,
                     sort.by = c("incl", "n"))
TT_n


# Voir les lignes contradictoires (Notée 1 pour le résultat et sa négation en même temps)

SSR<-intersect(rownames(TT2$tt)[TT2$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR

