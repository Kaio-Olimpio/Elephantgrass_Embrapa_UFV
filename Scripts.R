
data1 <- read.table("data.txt", , header=TRUE, quote="\"")

data1 <- data
data1 <- data1[order(data1$c), ]


# Transform
data1$Trat. = as.factor(data1$Trat.)
data1$block = as.factor(data1$block)
data1$c = as.factor(data1$c)
data1$male = as.factor(data1$male)
data1$female = as.factor(data1$female)
data1$concat = as.factor(data1$concat)
str(data1)

########### Exclude the checks #############
dat <- droplevels(subset(data1, concat != 88))
str(dat)

########## Data split by cut ################
dat_1 <- droplevels(subset(dat, c == 1))
str(dat_1)

dat_2 <- droplevels(subset(dat, c == 2))

library(asreml)


########### individual model cut 1 ########

M1 <- asreml(DBP ~ block,
             random = ~ male + and(female) + concat,
             maxit = 100,  data = dat_1)
summary(M1)$varcomp # Variance components

M1r = asreml(DBP ~ block,
             random =  ~ male + and(female),
             maxit = 100,  data = dat_1)
lrt(M1,M1r)

############# individual model cut 2 ###########

M2 <- asreml(DBP ~ block,
             random = ~ male + and(female) + concat,
             maxit = 100,  data = dat_2)
summary(M2)$varcomp

M2r = asreml(DBP ~ block,
             random = ~ concat,
             maxit = 100,  data = dat_2)
lrt(M2,M2r)

############ Complete diallel analysis ############

M3 <- asreml(DBP ~ block + c + block:c,
             random = ~ male + and(female) + concat +
               male:c + and(female:c) + concat:c
             + concat:block,
             residual  = ~dsum(~units|c),
             equate.levels = c('male','female'),
             #residual = ~ ar1(C):ar1(R),
             maxit = 100,  data = dat)

M3 <- update(M3)

summary(M3)$varcomp # Variance components



########### Effects ##########
blup_CGC<-summary(M3,coef=TRUE)$coef.random;blup_CGC
write.table(blup_CGC, "EffectsMStonOUT.txt")


########## acc ########
vg = summary(M5)$varcomp[1,1]
pred1= predict(m2, classify = "gen:loc",levels = list(loc = i), sed=T)
pred1$pvals  #Predicted values
sedmat = pred1$sed^2    
PEV = mean(sedmat[upper.tri(sedmat,diag=F)])
rgg = sqrt(1-(PEV/(vg)));rgg 


