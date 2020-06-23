# Nettoyer la console
rm(list = ls())


# Chargement des extensions
library(QCA); library(SetMethods)


# Définition du dossier de travail
setwd("E:/thèse/Etude Empirique/0 - Etude logicielle")

# Nommer fichier de travail
QCA2bis <- read.csv("qca2bis.csv", row.names = 1, sep = ";")

# Vérification du fichier : montre le début du fichier
head(QCA2bis)

# Transformer les noms en majuscules 
names(QCA2bis) <- toupper(names(QCA2bis))

### ANALYSE DE NECESSITE ###

# Test de toutes les conditions et leur négation
QCAfit(QCA2bis[, 1:7], QCA2bis$PERF, necessity = TRUE, names(QCA2bis[, 1:7]))

### ANALYSE DE SUFFISANCE ###

# Création table de vérité

TT2bis <- truthTable(QCA2bis, outcome = "PERF", 
                  conditions = c("PREC", "ORGA", "ENGAG", "RENT", "KPI", "RENV", "INDUS"),
                  incl.cut = 0.8,
                  show.cases = TRUE,
                  complete = FALSE,
                  sort.by = c("incl", "n"))
TT2bis


# Solution conservatrice
sol2_cb <- minimize(TT2bis, show.cases = TRUE, details = TRUE)
sol2_cb


# Solution parcimonieuse
sol2_pb <- minimize(TT2bis, details = TRUE, include = "?")
sol2_pb

# Montrer les hypothèses simplificatrices
sol2_pb$SA

# Solution intermédiaire. 
sol2_ib <- minimize(TT2bis, details = TRUE, show.cases=TRUE, include = "?", dir.exp = c("-", "-", 1, "-", 1, "-", 0))
sol2_ib

#Voir les hypothèses incluses (qui correspondent aux dir.exp)
sol2_ib$i.sol$C1P1$EC

# Graphique de la solution intermédiaire
pimplot(QCA2bis, sol2_ib, "PERF", crisp=TRUE)

### ANALYSE POST QCA ###
# Identification des cas déviants en consistance
smmr(sol2_ib, outcome = "PERF", match = FALSE, 
     cases = 3)

# Identification des cas déviants en couverture

smmr(sol2_ib, outcome = "PERF", match = FALSE, 
     cases = 4)

# Identification des cas typiques

smmr(sol2_ib, outcome="PERF", match=FALSE, cases=1)

# Identification des cas non-pertinents

smmr(sol2_ib, outcome="PERF", match=FALSE, cases=5)


### TESTS DE ROBUSTESSE ###


# Seuil de consistance à 0.7
TTRb <- truthTable(QCA2bis, outcome = "PERF", 
                   conditions = c("PREC", "ORGA", "ENGAG", "RENT", "KPI", "RENV", "INDUS"),
                   incl.cut = 0.7,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("incl", "n"))
TTRb


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

# Suppression du cas Apple

ROB5 <- read.csv("qca2bis_rob.csv", row.names = 1, sep = ";")


QCAfit(ROB5[, 1:7], ROB5$PERF, necessity = TRUE, names(ROB5[, 1:7]))

ttbr <- truthTable(ROB5, outcome = "PERF", 
                    conditions = c("PREC", "ORGA", "ENGAG", "RENT", "KPI", "RENV", "INDUS"),
                    incl.cut = 0.8,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("incl", "n"))
ttbr


solpr <- minimize(ttbr, details = TRUE, include = "?")
solpr


solir <- minimize(ttcas, details = TRUE, include = "?", dir.exp = c("-","-", 1, "-", 1, "-", 0 ))
solir



### ANALYSE NEGATION ###

# Analyse de nécessité pour
QCAfit(QCA2bis[, 1:7], QCA2bis$PERF, 
       neg.out = T,
       necessity = TRUE, paste(names(QCA2bis[, 1:7])))


# Analyse de suffisance 

TT_nb <- truthTable(QCA2bis, outcome = "PERF", neg.out = TRUE,
                   conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV", "INDUS"),
                   incl.cut = 0.65,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("incl", "n"))
TT_nb
TTnb <- esa(TT_nb, nec_cond="INDUS")
TTnb

# Voir les lignes contradictoires 

SSR<-intersect(rownames(TT2bis$tt)[TT2bis$tt$OUT==1],rownames(TTnb$tt)[TTnb$tt$OUT==1])
SSR

# Solution conservatrice
sol_nc <- minimize(TTnb, show.cases = TRUE, details = TRUE)
sol_nc


# Solution parcimonieuse
sol_np <- minimize(TTnb, details = TRUE, include = "?")
sol_np

# Montrer les hypothèses simplificatrices
sol_np$SA

# Solution intermédiaire. 
sol_ni <- minimize(TTnb, details = TRUE, show.cases=TRUE, include = "?", dir.exp = c("-", 0, "-", "-", 0, "-", 1))
sol_ni

# Graphique de la solution intermédiaire
pimplot(QCA2bis, sol_ni, "~PERF", crisp=TRUE)

### TESTS DE ROBUSTESSE POUR LA NEGATION ###

# Seuil de consistance à 0.9

ttn <- truthTable(QCA2bis, outcome = "PERF", neg.out=TRUE, 
                     conditions = c("PREC", "ENGAG", "ORGA", "RENT", "KPI", "RENV", "INDUS"),
                     incl.cut = 0.9,
                     show.cases = TRUE,
                     complete = FALSE,
                     sort.by = c("incl", "n"))
ttn
ttnr <- esa(ttn, nec_cond="INDUS")
ttnr


solpnr <- minimize(ttnr, details = TRUE, include = "?")
solpnr


solinr <- minimize(ttnr, details = TRUE, include = "?", dir.exp = c("-",0, "-", "-", 0, "-", 1 ))
solinr

# Suppression du cas 3 Suisses

ROB6 <- read.csv("qca2bis_nrob.csv", row.names = 1, sep = ";")


QCAfit(ROB6[, 1:7], ROB6$PERF, neg.out=TRUE, necessity = TRUE, names(ROB6[, 1:7]))

tt_nr <- truthTable(ROB6, outcome = "PERF", neg.out=TRUE, 
                   conditions = c("PREC", "ORGA", "ENGAG", "RENT", "KPI", "RENV", "INDUS"),
                   incl.cut = 0.75,
                   show.cases = TRUE,
                   complete = FALSE,
                   sort.by = c("incl", "n"))
tt_nr
tt_r <- esa(tt_nr, nec_cond="INDUS")
tt_r


solnrob <- minimize(tt_r, details = TRUE, include = "?")
solnrob


solirob <- minimize(tt_r, details = TRUE, include = "?", dir.exp = c("-","-", 0, "-", 0, "-", 1 ))
solirob

# ANALYSE POST QCA ###

# Identification de cas déviants en consistance
smmr(sol_ni, outcome = "PERF", neg.out=TRUE, match = FALSE, 
     cases = 3)

# Identification de cas déviants en couverture
smmr(sol_ni, outcome = "PERF", neg.out=TRUE, match = FALSE, 
     cases = 4)

