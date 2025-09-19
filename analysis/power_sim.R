
here::i_am("flag_project_root.R")
here::here()
data_path = here::here()

# power based on 40 ppt exploratory sample 
dftotal<- read.csv(paste0(data_path, "/data/processed/df_alltasks.csv" ),header = TRUE)
unique_participants <- unique(dftotal$subject)

subject_list <- c(
  "2bqu5ktkg43fh3p","brsvshywlogvxcq","9oe8onshjdy2khe","regcubm60dchcxr","hkdemly7qs0y8be","nvk3jd25saegh24",
  "gnd1v5zo5x7qjad","duzv7z4jux5olpv","mdtwfshb8x8v70p","2nzfnkk2uulwmkz","8es63ovt8uw2mgg","jvcerddevc3rwm0",
  "k7xa918wsdj832n","6be4c2orpqft8ul","5pgqs0v0feqyejp","7pjw0pz87exlgmd","v2rgepeoz23lhyb","djb1vpwld9c7hmk",
  "8le0rzndxgpur8r","n56opcxqwcyuk97","2usnf1r3h88bett","3jrram86yd3uhgb","pv8gfb6dwc0u8md","m603l2rxt7hfqax",
  "qeaducsgetncgeb","hvawyrkxreuq7d8","jk1c41xga9cmsrf","1tq0jt4dd31u8lq","r018cukle69r1bf","uaf4fed1xj02ps4",
  "swmufx7egboodpe","qw7q50m2o58ugvl","fqnkabfbs65dy5r","ws44qtt9xen2xsl","t53tjn7r8ydtmtq","qreecv8wv03uqw8",
  "rf8fvjnsk6r07uc",'9ho9tv1b4jdy3p6','lucnd0k3f5h8ez4','kp5boe0z4uwwu7r'
)

df40 <- dftotal[dftotal$subject %in% subject_list, ]
df40$anx_source <- "sticsa"
df40$anx <- df40$sticsa
length(unique(df40$subject))

#wrangling & checks
dfg <- df40[df40$task %in% "gen",]
dfg$response = as.numeric(dfg$response)
dfg$response_recoded = as.numeric(dfg$response_recoded)
dfg$gs_cat <- ordered(dfg$gs, levels=seq(0,8))
dfg$distance <- as.numeric(dfg$distance)
dfg$ou = factor(dfg$ou)
dfg$pu = factor(dfg$pu)
dfg$rule_f = factor(dfg$rule)

# add numeric ID
dfg <- dfg %>%
  group_by(subject) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

# check anx/iou distribution
dfg$anx_z <- (dfg$sticsa-mean(dfg$sticsa))/sd(dfg$sticsa)
hist(dfg$anx_z) #range sticsa 21- 84


# Get eff. size from pilot data - Model: Anx*distance
dfgg <- dfg[, c('response_recoded','pu','ou', 'gs_cat', 'iou_z', 'anx_z','ID', 'distance', 'rule')]
dfgg$ID = as.factor(dfgg$ID)

m.g.add<- glmmTMB(data=dfgg, response_recoded ~ distance*pu + distance*ou + distance*rule + distance*anx_z+(1|ID), family = beta_family(link = "logit"))
car::Anova(m.g.add)
summary(m.g.add)


#Simulate some data 
set.seed(111)
nSubj = 150 # participants
gauss_ids <- sample(1:nSubj, size = nSubj/2, replace = FALSE)
nStim <- 9 # CS+ and 4 GSs on each side
nOutc = 3 # levels of outcome uncertainty
nPerc = 2 # number of perceptual uncertainty 
nSamples = 4 # number of data points for each rating
nCond = 6 

oulevels = c(0.25, 0.5, 0.75)  # outcome uncertainty levels
pulevels = c(0.4, 0.2)         # perceptual uncertainty levels
mean = 0
xmin = -4
xmax = 4
cspos = 5
xs = seq(xmin, xmax, by=((xmax - (xmin))/(nStim - 1))) # stim -4:4

NLtrials <- 24 # Learning trials per condition
NGtrials <- 36 # Generalization trials per condition
Ntrials = NLtrials + NGtrials  # Total number of trials per condition
task = c(rep(0, NLtrials), rep(1, NGtrials))  # Task indicator: 0 = learning, 1 = generalization

# Simulate anxiety (anx)
anxsim <- rexp(n = nSubj, rate = 0.17) + 20  # Simulated anxiety scores
hist(anxsim)

dataf <- data.frame()
for (sub in 1:nSubj) {
  for (ou in 1:length(oulevels)) {
    for (pu in 1:length(pulevels)) {
      
      trial_sequence <- c(rep(mean, NLtrials), sample(rep(xs, times = nSamples)))
      df_cond <- data.frame(stim = trial_sequence, 
                            ou = rep(oulevels[ou], Ntrials, 1), 
                            pu = rep(pulevels[pu], Ntrials, 1), 
                            sub = rep(sub, Ntrials, 1), 
                            task = task)
      
      if (sub %in% gauss_ids) {
        df_cond$rule = 'gauss'
      } else {
        df_cond$rule = 'lin_l'
      }
      dataf <- rbind(dataf, df_cond)
    }
  }
}

dsim <- dataf[dataf$task == 1, ]  # Select generalization trials
dsim$anxiety <- anxsim[match(dsim$sub, unique(dsim$sub))]
head(dsim)

#Make lmer
dsim$distance = as.numeric(dsim$stim)
dsim$distance = abs(dsim$stim)
dsim$ou = as.factor(dsim$ou )
dsim$pu = as.factor(dsim$pu )
dsim$sub = as.numeric(dsim$sub)
dsim$rule_f = as.factor(dsim$rule)
dsim$anx_z <- (dsim$anxiety-mean(dsim$anxiety))/sd(dsim$anxiety)
dsim$iou_z <- (dsim$iou-mean(dsim$iou))/sd(dsim$iou)

cc_a = summary(m.g.add)$coefficients
ccc_a = cc_a$cond
ccc_a = as.data.frame(ccc_a)
ccc_a = ccc_a$Estimate
fixed_a = ccc_a

vc <- VarCorr(m.g.add)
str(vc)
rand_var <- vc$cond$ID[1, 1]
rand <- matrix(rand_var, nrow = 1)
res <- sigma(m.g.add) 

model_add <- makeLmer(response ~ distance*pu + distance*ou + distance*rule_f + distance*anx_z +(1|sub), fixef=fixed_a, VarCorr=rand, sigma=res, data=dsim)
model_add

#sim power
sim_anx <- powerSim(model_add, nsim=100, test = fcompare(~distance*pu + distance*ou + distance*rule_f + anx_z))
sim_anx
#power curve
p_curve_pu1<- powerCurve(model_add, test=fcompare(~distance*pu + distance*ou + distance*rule_f + anx_z), along="sub", nsim= 100)
plot(p_curve_pu1)

#conserv.
fixed_a[length(fixed_a)] <- 0.04
model_add <- makeLmer(response ~ distance*pu + distance*ou + distance*rule_f + distance*anx_z + (1|sub), fixef=fixed_a, VarCorr=rand, sigma=res, data=dsim)
model_add

sim_anx2 <- powerSim(model_add, nsim=100, test = fcompare(~distance*pu + distance*ou + distance*rule_f + anx_z))
sim_anx2
p_curve_pu2<- powerCurve(model_add, test=fcompare(~distance*pu + distance*ou+ distance*rule_f + anx_z), along="sub", nsim= 500)
plot(p_curve_pu2)
powersim <- summary(p_curve_pu2)
#write.csv(powersim, paste0(data_path, "/data/processed/power_sim.csv", row.names = FALSE)



