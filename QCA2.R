# Nettoyer la console
rm(list = ls())


# Chargement des extensions
library(QCA); library(SetMethods)


# D�finition du dossier de travail
setwd("E:/th�se/Etude Empirique/0 - Etude logicielle")

# Nommer fichier de travail
QCA2 <- read.csv("qca2.csv", row.names = 1, sep = ";")

# V�rification du fichier : montre le d�but du fichier
head(QCA2)
 
# Transformer les noms en majuscules
names(QCA2) <- toupper(names(QCA2))

### ANALYSE DE NECESSITE ###

# Test de toutes les conditions et leur n�gation
QCAfit(QCA2[, 1:6], QCA2$PERF, necessity = TRUE, names(QCA2[, 1:6]))

### ANALYSE DE SUFFISANCE ###

# Cr�ation table de v�rit�


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

# Montrer les hypoth�ses simplificatrices
sol2_p$SA

# Solution interm�diaire. 
sol2_i <- minimize(TT2, details = TRUE, show.cases=TRUE, include = "?", dir.exp = c("-","-", 1, "-", 1, "-"))
sol2_i

# Voir les hypoth�ses incluses 
sol2_i$i.sol$C1P1$EC

# Graphique de la solution interm�diaire
pimplot(QCA2bis, sol2_ib, "PERF", crisp=TRUE)


### TESTS DE ROBUSTESSE ###


# Seuil de consistance � 0.7
TTR2 <- truthTable(QCA2, outcome = "PERF", 
                   conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                   incl.cut = 0.7,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("incl", "n"))
TTR2


sol2_ri <- minimize(TTR2, details = TRUE, include = "?", dir.exp = c("-",1, "-", "-", 1, "-"))
sol2_ri

# Seuil de consistance � 0.9

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

# Cas d�viants en consistance

smmr(sol2_i, outcome = "PERF", match = FALSE, 
    cases = 3)

# Cas d�viants en couverture

smmr(sol2_i, outcome = "PERF", match = FALSE, 
    cases = 4)

### ANALYSE NEGATION ##

# Analyse de n�cessit� pour n�gation
QCAfit(QCA2[, 1:6], QCA2$PERF, 
       neg.out = T,
       necessity = TRUE, paste(names(QCA2[, 1:6])))


# Analyse de suffisance pour la n�gation
TT_n <- truthTable(QCA2, outcome = "PERF", neg.out = TRUE,
                     conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV"),
                     incl.cut = 0.8,
                     show.cases = TRUE,
                     complete = FALSE,
                     sort.by = c("incl", "n"))
TT_n


# Voir les lignes contradictoires (Not�e 1 pour le r�sultat et sa n�gation en m�me temps)

SSR<-intersect(rownames(TT2$tt)[TT2$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR

