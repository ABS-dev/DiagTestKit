library(ggplot2)
library(data.table)
devtools::load_all()
lst <- NULL
v3.6 <- new.env()
v4.0 <- new.env()
v4.1 <- new.env()
v4.3 <- new.env()
getwd()

load("notes/version3.6.3.rdata")
list2env(lst, envir = v3.6)
load("notes/version4.0.3.rdata")
list2env(lst, envir = v4.0)
load("notes/version4.1.3.rdata")
list2env(lst, envir = v4.1)
load("notes/version4.3.1.rdata")
list2env(lst, envir = v4.3)

v3.6$ex1
v4.0$ex1
v4.1$ex1
v4.3$ex1

ls(envir = v3.6) == ls(envir = v4.0)

for (nm in ls(envir = v3.6)) {
  cat(nm, class(v3.6[[nm]]), class(v4.0[[nm]]), class(v3.6[[nm]]) == class(v4.0[[nm]]), "\n")
}

# dat Numeric

length(v3.6$dat)
length(v4.0$dat)
v3.6$dat == v4.0$dat

# iter.n numeric
length(v3.6$iter.n)
length(v4.0$iter.n)
v3.6$iter.n == v4.0$iter.n

# N.vec numeric
length(v3.6$N.vec)
length(v4.0$N.vec)
v3.6$N.vec == v4.0$N.vec

# nstates integer
length(v3.6$nstates)
length(v4.0$nstates)
v3.6$nstates == v4.0$nstates

# parm NULL
TRUE

# prev.vec matrix
dim(v3.6$prev.vec)
dim(v4.0$prev.vec)
all(v3.6$prev.vec == v4.0$prev.vec)
dt <- as.data.table(v3.6$prev.vec)

ggplot(dt, aes(V1)) +
  geom_density()

# rep.iter logical
length(v3.6$rep.iter)
length(v4.0$rep.iter)
v3.6$rep.iter == v4.0$rep.iter

# SnR.vec matrix
dim(v3.6$SnR.vec)
dim(v4.0$SnR.vec)
all(v3.6$SnR.vec == v4.0$SnR.vec)
dt <- as.data.table(v3.6$SnR.vec)
ggplot(dt, aes(V1)) +
  geom_density()

# SpR.vec matrix
dim(v4.0$SpR.vec)
dim(v3.6$SpR.vec)
all(v3.6$SpR.vec == v4.0$SpR.vec)
dt <- as.data.table(v3.6$SpR.vec)
ggplot(dt, aes(V1)) +
  geom_density()
ggplot(dt, aes(V2)) +
  geom_density()

# tolerance numeric
length(v3.6$tolerance)
length(v4.0$tolerance)
all(v3.6$tolerance == v4.0$tolerance)

# ex1 snsp
names(v3.6$ex1) == names(v4.0$ex1)
names(v3.6$ex1)
class(v3.6$ex1$detailOut)
class(v3.6$ex1$calcVal)

names(v3.6$ex1$detailOut) == names(v4.0$ex1$detailOut)
names(v3.6$ex1$calcVal) == names(v4.0$ex1$calcVal)

names(v3.6$ex1$detailOut)

# ex1$detailOut$Exp.Sn FALSE Both sets of random numbers
class(v3.6$ex1$detailOut$Exp.Sn)
length(v3.6$ex1$detailOut$Exp.Sn)
all(v3.6$ex1$detailOut$Exp.Sn == v4.0$ex1$detailOut$Exp.Sn)
summary(v3.6$ex1$detailOut$Exp.Sn)
summary(v4.0$ex1$detailOut$Exp.Sn)

dt <- data.table(v3.6 = v3.6$ex1$detailOut$Exp.Sn,
                 v4.0 = v4.0$ex1$detailOut$Exp.Sn)
dt <- melt(dt, id.vars = NULL)

ggplot(dt, aes(x = value, color = variable)) +
  geom_density()

# ex1$detailOut$Exp.Sp FALSE v3.6 is random numbers, but v4.0 is all 1's
class(v3.6$ex1$detailOut$Exp.Sp)
all(v3.6$ex1$detailOut$Exp.Sp == v4.0$ex1$detailOut$Exp.Sp)
# v3.6$ex1$detailOut$Exp.Sp
# v4.0$ex1$detailOut$Exp.Sp

summary(v3.6$ex1$detailOut$Exp.Sp)
summary(v4.0$ex1$detailOut$Exp.Sp)

dt <- data.table(v3.6 = v3.6$ex1$detailOut$Exp.Sp,
                 v4.0 = v4.0$ex1$detailOut$Exp.Sp)
dt <- melt(dt, id.vars = NULL)
dt[, .(min(value), max(value)), variable]
ggplot(dt, aes(x = value, color = variable)) +
  geom_density()


# ex1$detailOut$Converge TRUE  Both all 0's
class(v3.6$ex1$detailOut$Converge)
all(v3.6$ex1$detailOut$Converge == v4.0$ex1$detailOut$Converge)

# ex1$detailOut$Message FALSE
class(v3.6$ex1$detailOut$Message)
all(v3.6$ex1$detailOut$Message == v4.0$ex1$detailOut$Message)
sum(v3.6$ex1$detailOut$Message != v4.0$ex1$detailOut$Message)
idx <- which(v3.6$ex1$detailOut$Message != v4.0$ex1$detailOut$Message)
idx
data.frame(v3.6 = v3.6$ex1$detailOut$Message[idx],
           v4.0 = v4.0$ex1$detailOut$Message[idx])

names(v3.6$ex1$calcVal)
# ex1$calcVal$Nsim TRUE
class(v3.6$ex1$calcVal$Nsim)
length(v3.6$ex1$calcVal$Nsim)
all(v3.6$ex1$calcVal$Nsim == v4.0$ex1$calcVal$Nsim)
v3.6$ex1$calcVal$Nsim
v4.0$ex1$calcVal$Nsim
# ex1$calcVal$Confidence TRUE
class(v3.6$ex1$calcVal$Confidence)
length(v3.6$ex1$calcVal$Confidence)
all(v3.6$ex1$calcVal$Confidence == v4.0$ex1$calcVal$Confidence)
v3.6$ex1$calcVal$Confidence
v4.0$ex1$calcVal$Confidence
# ex1$calcVal$SnPE FALSE
class(v3.6$ex1$calcVal$SnPE)
length(v3.6$ex1$calcVal$SnPE)
all(v3.6$ex1$calcVal$SnPE == v4.0$ex1$calcVal$SnPE)
v3.6$ex1$calcVal$SnPE
v4.0$ex1$calcVal$SnPE
# ex1$calcVal$SnInterval FALSE
class(v3.6$ex1$calcVal$SnInterval)
length(v3.6$ex1$calcVal$SnInterval)
all(v3.6$ex1$calcVal$SnInterval == v4.0$ex1$calcVal$SnInterval)
v3.6$ex1$calcVal$SnInterval
v4.0$ex1$calcVal$SnInterval
# ex1$calcVal$SpPE FALSE
class(v3.6$ex1$calcVal$SpPE)
length(v3.6$ex1$calcVal$SpPE)
all(v3.6$ex1$calcVal$SpPE == v4.0$ex1$calcVal$SpPE)
v3.6$ex1$calcVal$SpPE
v4.0$ex1$calcVal$SpPE
# ex1$calcVal$SpInterval FALSE
class(v3.6$ex1$calcVal$SpInterval)
length(v3.6$ex1$calcVal$SpInterval)
all(v3.6$ex1$calcVal$SpInterval == v4.0$ex1$calcVal$SpInterval)
v3.6$ex1$calcVal$SpInterval
v4.0$ex1$calcVal$SpInterval

