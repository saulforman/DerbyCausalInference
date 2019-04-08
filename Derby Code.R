setwd('/Users/saulforman/Desktop/baseball')
###############
#data processing
library('dplyr')

#splits data
splits <- read.csv('halfsplits.csv')
first <- splits[which(splits$Split == '1st Half'),]
second <- splits[-which(splits$Split == '1st Half'),]

#subset to variables of interest and rename first half
myvars <- c('Year','Name','AB','BA','HR','RBI','SLG','OBP','OPS')
first2 <- first[, myvars]
colnames(first2) <- c('Year','Name','AB_First','AVG_First','HR_First','RBI_First',
                      'SLG_First','OBP_First','OPS_First')

#subset to variables of interest and rename second half
myvars2 <- c('Year','Name','BA','HR','RBI','SLG','OBP','OPS')
second2 <- second[, myvars2]
colnames(second2) <- c('Year','Name','AVG_Second','HR_Second','RBI_Second',
                       'SLG_Second','OBP_Second','OPS_Second')

#join all
full <- left_join(first2, second2, by = c('Name', 'Year'))

#get list of players with lahman id
id <- read.csv('People.csv')
id$Name <- paste(id$nameFirst, id$nameLast, sep = ' ')
id <- id[,c('playerID','Name')]

#join to data and fix errant ids
data <- read.csv('Derby.csv')
data$Name <- paste(data$X, data$Name, sep = ' ')
data$NameYr <- paste(data$Name, data$Year, sep = ' ')

#add derby data
full$NameYr <- paste(full$Name, full$Year, sep = ' ')
full$Derby <- 0
full[(full$NameYr %in% data$NameYr),]$Derby <- 1

#add asg data
asg <- read.csv('AllStarFull.csv')
asg <- left_join(asg, id, by = 'playerID')
asg$NameYr <- paste(asg$Name, asg$yearID, sep = ' ')
full$ASG <- 0
full[(full$NameYr %in% asg$NameYr),]$ASG <- 1

#add awards data
awards <- read.csv('AwardsPlayers.csv')
awards <- left_join(awards, id, by = 'playerID')
awards$yearID <- awards$yearID + 1
awards$NameYr <- paste(awards$Name, awards$yearID, sep = ' ')
full$Awards <- 0
full[(full$NameYr %in% awards$NameYr),]$Awards <- 1

#add prior hr totals
batting <- read.csv('Batting.csv')
batting <- left_join(batting, id, by = 'playerID')
class(batting$yearID)
batting$yearID <- batting$yearID + 1
batting <- batting[,c('Name','yearID','stint','HR')]
colnames(batting) <- c('Name','Year','Stint','HR_LastYr')

#add Jr. to batting (Don't worry about Seniors - NameYr will take care of those)
Names <- full[grepl('Jr.', full$Name),]$Name
Names <- sub(' Jr.', '', Names)
batting[batting$Name %in% Names == TRUE,]$Name <- paste(batting[batting$Name %in% Names == TRUE,]$Name, 'Jr.', sep = ' ')
batting$NameYr <- paste(batting$Name, batting$Year, sep = ' ')
batting <- batting[,c('NameYr','Stint','HR_LastYr')]

#fix stint issue with batting table
batting2 <- aggregate(batting$HR_LastYr, by = list(batting$NameYr), FUN = sum)
colnames(batting2) <- c('NameYr','HR_LastYr')
full <- left_join(full, batting2, by = 'NameYr')

#remove NAs for matching
match <- na.omit(full)
match <- match[which(match$Year != 2018),]
treatment <- as.numeric(as.character(match$Derby))

##################
#load packages
library('tableone')
library('Matching')
library('optmatch')
library('rcbalance')

#list covariates
xvars <- c('Year','AB_First','AVG_First','HR_First','RBI_First',
           'SLG_First','OBP_First','HR_LastYr','ASG','Awards')

#look at an unmatched Table 1
table1 <- CreateTableOne(vars = xvars, strata = 'Derby', data = match, test = FALSE)
## include standardized mean difference
print(table1, smd = TRUE)

#do greedy matching on Mahalanobis distance
greedymatch <- Match(Tr = treatment, M = 1, X = match[xvars])
matched <- match[unlist(greedymatch[c('index.treated','index.control')]),]
matchedtab <- CreateTableOne(vars = xvars, strata = 'Derby', 
                             data = matched, test = FALSE)
print(matchedtab, smd = TRUE)

unlist(greedymatch[c('index.treated','index.control')])

match[7140,]
match[6262,]

match[7083,]
match[7074,]

match[6863,]
match[7073,]

match[7079,]
match[7071,]

match[6212,]
match[7063,]

match[5770,]
match[7061,]

match[6637,]
match[7059,]


unlist(greedymatch[c('index.treated','index.control')])
#do optimal matching on Mahalanobis distance
my.dist <- build.dist.struct(z = match$Derby == 1, X = match[xvars], 
                             calip.option = 'none')
match.out <- rcbalance(my.dist,
                       treated.info = match[match$Derby == 1,],
                       control.info = match[match$Derby != 1,])
matchedtab1 <- CreateTableOne(vars = xvars, strata = 'Derby',
                              data = matched2, test = FALSE)

#outcome analysis - AVG
y1_trt <- matched$AVG_Second[matched$Derby == 1]
y1_con <- matched$AVG_Second[match.out$matches]
##pairwise difference
diffy1 <- y1_trt - y1_con
##paired t-test
t.test(diffy1)

#outcome analysis - HR
y2_trt <- matched$HR_Second[matched$Derby == 1]
y2_con <- matched$HR_Second[match.out$matches]
##pairwise difference
diffy2 <- y2_trt - y2_con
##paired t-test
t.test(diffy2)

#outcome analysis - RBI
y3_trt <- matched$RBI_Second[matched$Derby == 1]
y3_con <- matched$RBI_Second[match.out$matches]
##pairwise difference
diffy3 <- y3_trt - y3_con
##paired t-test
t.test(diffy3)

#outcome analysis - SLG
y4_trt <- matched$SLG_Second[matched$Derby == 1]
y4_con <- matched$SLG_Second[match.out$matches]
##pairwise difference
diffy4 <- y4_trt - y4_con
##paired t-test
t.test(diffy4)

#outcome analysis - OBP
y5_trt <- matched$OBP_Second[matched$Derby == 1]
y5_con <- matched$OBP_Second[match.out$matches]
##pairwise difference
diffy5 <- y5_trt - y5_con
##paired t-test
t.test(diffy5)

################
#load packages
library('MatchIt')
library('gtools')
#matched propensity score
psmodel <- glm(treatment~Year+AB_First+HR_First+AVG_First+RBI_First+SLG_First+
                 OBP_First+ASG+Awards+HR_LastYr,
               family = binomial(), data = match)
#show coefficients
summary(psmodel)
#create propensity score
pscore <- psmodel$fitted.values
#match on propensity score
m.out <- matchit(treatment~Year+AB_First+HR_First+AVG_First+RBI_First+SLG_First+
                   OBP_First+ASG+Awards+HR_LastYr, data = match, method = 'nearest')
summary(m.out)
plot(m.out, type = 'jitter')
plot(m.out, type = 'hist')

#do greedy matching on logit(PS) without caliper
psmatch <- Match(Tr = match$Derby, M = 1, X = logit(pscore), replace = FALSE)
matched <- match[unlist(psmatch[c('index.treated','index.control')]),]
unlist(psmatch[c('index.treated','index.control')])

xvars <- c('Year','AB_First','AVG_First','HR_First','RBI_First',
           'SLG_First','OBP_First','HR_LastYr','ASG','Awards')
matchedtab1 <- CreateTableOne(vars = xvars, strata = 'Derby',
                              data = matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#re-do matching using a caliper
psmatch <- Match(Tr = match$Derby, M = 10, X = logit(pscore), replace = FALSE,
                 caliper = .1)
matched <- match[unlist(psmatch[c('index.treated','index.control')]),]
xvars <- c('Year','AB_First','AVG_First','HR_First','RBI_First',
           'SLG_First','OBP_First','HR_LastYr','ASG','Awards')
matchedtab1 <- CreateTableOne(vars = xvars, strata = 'Derby',
                              data = matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis - AVG
y1_trt <- matched$AVG_Second[matched$Derby == 1]
y1_con <- matched$AVG_Second[matched$Derby == 0]
##pairwise difference
diffy1 <- y1_trt - y1_con
##paired t-test
t.test(diffy1)

#outcome analysis - HR
y2_trt <- matched$HR_Second[matched$Derby == 1]
y2_con <- matched$HR_Second[matched$Derby == 0]
##pairwise difference
diffy2 <- y2_trt - y2_con
##paired t-test
t.test(diffy2)

#outcome analysis - RBI
y3_trt <- matched$RBI_Second[matched$Derby == 1]
y3_con <- matched$RBI_Second[matched$Derby == 0]
##pairwise difference
diffy3 <- y3_trt - y3_con
##paired t-test
t.test(diffy3)

#outcome analysis - SLG
y4_trt <- matched$SLG_Second[matched$Derby == 1]
y4_con <- matched$SLG_Second[matched$Derby == 0]
##pairwise difference
diffy4 <- y4_trt - y4_con
##paired t-test
t.test(diffy4)

#outcome analysis - OBP
y5_trt <- matched$OBP_Second[matched$Derby == 1]
y5_con <- matched$OBP_Second[matched$Derby == 0]
##pairwise difference
diffy5 <- y5_trt - y5_con
##paired t-test
t.test(diffy5)

############
#IPTW estimation
library('ipw')
library('sandwich')
library('survey')

xvars <- c('Year','AB_First','AVG_First','HR_First','RBI_First',
           'SLG_First','OBP_First','HR_LastYr','ASG','Awards')
#propensity score model
psmodel <- glm(Derby ~ Year + AB_First + AVG_First + HR_First
               + RBI_First + SLG_First + OBP_First + HR_LastYr +
                 ASG + Awards, data = match, family = binomial(link = 'logit'))
ps <- predict(psmodel, type = 'response')
summary(psmodel)

#create weights
weight <- ifelse(match$Derby == 1, 1/(ps), 1/(1-ps))
weighteddata <- svydesign(ids = ~ 1, data = match, weights = ~ weight)
weightedtable <- svyCreateTableOne(vars = xvars, strata = 'Derby',
                                   data = weighteddata, test = FALSE)
print(weightedtable, smd = TRUE)

###########
#get causal relative risk
glm.obj <- glm(OBP_Second ~ Derby, data = match, weights = weight)
#summary
betaiptw <- coef(glm.obj)
#use asymptotic (sandwich) variance
SE <- sqrt(diag(vcovHC(glm.obj, type = 'HC0')))
#get point estimate and CI for relative risk
causalrr <- exp(betaiptw[2])
lcl <- exp(betaiptw[2]-1.96*SE[2])
ucl <- exp(betaiptw[2]+1.96*SE[2])
c(lcl,causalrr,ucl)

#get weight model using truncation
weightmodel <- ipwpoint(exposure = Derby, family = 'binomial', link = 'logit',
                        denominator = ~ Year + AB_First + AVG_First + HR_First
                        + RBI_First + SLG_First + OBP_First + HR_LastYr +
                          ASG + Awards, data = match, trunc = 0.01)
wt <- weightmodel$weights.trunc
ipwplot(weightmodel$ipw.weights, logscale = FALSE, main = 'Weights',
        xlim = c(0,10))
weighteddata <- svydesign(ids = ~ 1, data = match, weights = ~ wt)
msm <- (svyglm(OPS_Second~Derby, design = svydesign(~1, weights = ~wt, data = match)))
coef(msm)
confint(msm)

weightedtable <- svyCreateTableOne(vars = xvars, strata = 'Derby',
                                   data = weighteddata, test = FALSE)
print(weightedtable, smd = TRUE)

#
