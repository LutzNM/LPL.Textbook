library(tidyverse)
library(jtools)
library(ggpubr)

# Chapter 6

## Scenario Example

fitness <- cbind(time = rnorm(90, 23.6, 4.3), area = "fitness")
library <- cbind(time = rnorm(40, 41.8, 9.7), area = "library")
yard <- cbind(time = rnorm(70, 20.7, 1.2), area = "yard")

df.scen <- data.frame(rbind(fitness, library, yard))

df.scen <- mutate(df.scen,
                  time = as.numeric(time))

str(df.scen)

fit.hist <-
df.scen%>%
  filter(area == "fitness")%>%
  ggplot(aes(x = time))+
  geom_histogram(binwidth = 1.25)+
  theme_apa()+
  labs(x = "Time spent (in minutes)")+
  ggtitle("Fitness Center")

lib.hist <-
  df.scen%>%
  filter(area == "library")%>%
  ggplot(aes(x = time))+
  geom_histogram(binwidth = 3)+
  theme_apa()+
  labs(x = "Time spent (in minutes)",
       y = "")+
  ggtitle("Library")

yard.hist <-
  df.scen%>%
  filter(area == "yard")%>%
  ggplot(aes(x = time))+
  geom_histogram(binwidth = 0.33)+
  theme_apa()+
  labs(x = "Time spent (in minutes)",
       y = "")+
  ggtitle("Yard")

ggarrange(fit.hist, lib.hist, yard.hist,
          ncol = 3)

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.Scenario.png")

# Dice Rolling

1/6

d1 <- seq(1, 6, 1)
d2 <- seq(1, 6, 1)

d1s <- rep(d1, times = length(d2))
d2s <- rep(d2, each = length(d1))

df.dice <- data.frame(d1s, d2s)

df.dice <- mutate(df.dice,
                  sum = d1s + d2s)

df.dice%>%
  summarize(mean = mean(sum),
            sd = sd(sum))

df.dice$dev = df.dice$sum - mean(sum)
df.dice$dev2 = df.dice$dev^2

dice.sd.pop <-sqrt(sum(df.dice$dev2)/nrow(df.dice))

dice.sd.pop/sqrt(500)

dice.probs <-
df.dice%>%
  group_by(sum)%>%
  summarize(prob = n()/length(df.dice)/12)

dice.probs%>%
  ggplot(aes(x = factor(sum), y = prob))+
  geom_bar(stat = "identity")+
  theme_apa()+
  labs(x = "Observed Dice Roll Total",
       y = "Probability")

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.Dice.Rolls.png")

roll1 <- sample(x = seq(1, 6, 1), size = 100, replace = TRUE)
roll2 <- sample(x = seq(1, 6, 1), size = 100, replace = TRUE)

rolls <- roll1 + roll2

hist(rolls)
mean(rolls)

500*5/60*6/60
500*5/60*500/60/24

sims <- 500

trial.1 <- cbind(value1 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 value2 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 trial = "Trial One")
trial.2 <- cbind(value1 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 value2 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 trial = "Trial Two")
trial.3 <- cbind(value1 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 value2 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 trial = "Trial Three")
trial.4 <- cbind(value1 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 value2 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 trial = "Trial Four")
trial.5 <- cbind(value = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 value2 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 trial = "Trial Five")
trial.6 <- cbind(value1 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 value2 = sample(x = seq(1, 6, 1), size = sims, replace = TRUE),
                 trial = "Trial Six")

df.rolls <-
  data.frame(
    rbind(trial.1, trial.2, trial.3,
          trial.4, trial.5, trial.6))%>%
  mutate(value1 = as.numeric(value1),
         value2 = as.numeric(value2),
         sum = value1 + value2,
         trial = factor(trial, levels = c("Trial One", "Trial Two", "Trial Three", 
         "Trial Four", "Trial Five", "Trial Six")))

str(df.rolls)

df.rolls%>%
  ggplot(aes(x = factor(sum)))+
  geom_bar(width = .5)+
  facet_wrap(.~trial)+
  theme_apa()+
  labs(x = "Observed Dice Roll Total")

df.rolls%>%
  group_by(trial)%>%
  summarize(mean = mean(sum))

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.Six.Dice.Roll.Trials.png")

set.seed(1)

ntrials <- 500
nrolls <- 500

dice.means <- rep(NA, ntrials)

for(i in 1:ntrials){
  roll1 <- sample(x = seq(1, 6, 1), size = nrolls, replace = TRUE)
  roll2 <- sample(x = seq(1, 6, 1), size = nrolls, replace = TRUE)
  sum <- roll1 + roll2
  dice.means[i] <- mean(sum)
}

df.500.500.rolls <- data.frame(dice.means)

ggplot()+
  geom_histogram(aes(x = dice.means), data = df.500.500.rolls, binwidth = .045)+
  theme_apa()+
  labs(x = "Mean of Dice Rolls",
       y = "Count")+
  xlim(c(6.5, 7.5))
  
ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.500.500.Dice.Roll.Trials.png")

# Same example but with the normal curve overlaid

x <- seq(6.25, 7.75, .01)
y <- dnorm(x, mean = mean(dice.means), sd = sd(dice.means))*sqrt(ntrials)

plot(x, y, type = "l")

norm <- data.frame(x, y)

norm.curve <-
  norm%>%
  ggplot(aes(x = x, y = y))+
  geom_line()+
  theme_apa()

ggplot()+
  geom_histogram(aes(x = dice.means), data = df.500.500.rolls, binwidth = .045, alpha = .5)+
  geom_line(aes(x = x, y = y), data = norm, lty = "dashed")+
  theme_apa()+
  labs(x = "Mean of Dice Rolls",
       y = "Count")+
  xlim(c(6.5, 7.5))

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.CLT.500.500.Dice.Roll.Trials.png")

df.500.500.rolls%>%
  summarize(
    min = min(dice.means),
    max = max(dice.means),
    mean = mean(dice.means),
    med  = median(dice.means),
    perfect = sum(dice.means == 7),
    prop.perfect = sum(dice.means == 7)/n())

meta.mean <- mean(dice.means)
meta.sd <- sd(dice.means)

meta.mean
meta.sd

meta.mean - 3*meta.sd
meta.mean + 3*meta.sd

pnorm(7.5, meta.mean, meta.sd)
pnorm(7.105877, meta.mean, meta.sd) - pnorm(6.888363, meta.mean, meta.sd)
pnorm(7.323391, meta.mean, meta.sd) - pnorm(6.670849, meta.mean, meta.sd)

pnorm(1, 0, 1) - pnorm(-1, 0, 1)
pnorm(2, 0, 1) - pnorm(-2, 0, 1)
pnorm(3, 0, 1) - pnorm(-3, 0, 1)

9/1198

# Law of Large Numbers

trial.10 <- cbind(value1 = sample(x = seq(1, 6, 1), size = 10, replace = TRUE),
                  value2 = sample(x = seq(1, 6, 1), size = 10, replace = TRUE),
                   trial = "N = 10")

trial.50 <- cbind(value1 = sample(x = seq(1, 6, 1), size = 50, replace = TRUE),
                  value2 = sample(x = seq(1, 6, 1), size = 50, replace = TRUE),
                  trial = "N = 50")

trial.100 <- cbind(value1 = sample(x = seq(1, 6, 1), size = 100, replace = TRUE),
                  value2 = sample(x = seq(1, 6, 1), size = 100, replace = TRUE),
                  trial = "N = 100")

trial.500 <- cbind(value1 = sample(x = seq(1, 6, 1), size = 500, replace = TRUE),
                  value2 = sample(x = seq(1, 6, 1), size = 500, replace = TRUE),
                  trial = "N = 500")

trial.1000 <- cbind(value1 = sample(x = seq(1, 6, 1), size = 1000, replace = TRUE),
                  value2 = sample(x = seq(1, 6, 1), size = 1000, replace = TRUE),
                  trial = "N = 1000")

trial.10000 <- cbind(value1 = sample(x = seq(1, 6, 1), size = 10000, replace = TRUE),
                    value2 = sample(x = seq(1, 6, 1), size = 10000, replace = TRUE),
                    trial = "N = 10000")


df.rolls <-
  data.frame(
    rbind(trial.10, trial.50, trial.100,
          trial.500, trial.1000, trial.10000))%>%
  mutate(value1 = as.numeric(value1),
         value2 = as.numeric(value2),
         sum = value1 + value2,
         trial = factor(trial,
                        levels = c("N = 10", "N = 50", "N = 100", 
                                   "N = 500", "N = 1000", "N = 10000")
                        ))

str(df.rolls)

df.rolls%>%
  ggplot(aes(x = factor(sum)))+
  geom_bar(width = .5)+
  facet_wrap(.~trial, scales = "free_y")+
  theme_apa()+
  labs(x = "Observed Dice Roll Total")

df.rolls%>%
  group_by(trial)%>%
  summarize(mean = mean(sum))

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.LLN.Six.Dice.Roll.Trials.png")

set.seed(922)

nsim <- 10000

samp.means <- rep(NA, nsim)

for(i in 1:nsim){
  value1 = sample(x = seq(1, 6, 1), size = i, replace = TRUE)
  value2 = sample(x = seq(1, 6, 1), size = i, replace = TRUE)
  sums <- value1 + value2
  samp.means[i] <- mean(sums)
}

n <- seq(1, nsim, 1)

df.lln.sims <- data.frame(cbind(n, samp.means))

str(df.lln.sims)

plot.10000 <-
df.lln.sims%>%
  ggplot(aes(x = n, y = samp.means))+
  geom_line(alpha = .5)+
  geom_hline(yintercept = 7, lty = "dashed", color = "red")+
  ylim(c(5, 9))+
  theme_apa()+
  labs(y = "Sample Mean",
       x = "N")+
  ggtitle("All 10,000 simulations")

plot.500 <- 
df.lln.sims%>%
  filter(n < 500)%>%
  ggplot(aes(x = n, y = samp.means))+
  geom_line(alpha = .5)+
  geom_hline(yintercept = 7, lty = "dashed", color = "red")+
  ylim(c(5, 9))+
  theme_apa()+
  labs(y = "Sample Mean",
       x = "N")+
  ggtitle("First 500 simulations")

ggarrange(plot.10000, plot.500)

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.LLN.10000.Dice.Roll.Trials.png")

set.seed(922)

nsim <- 100

ntrials <- 500

samp.sds <- rep(NA, nsim)
samp.means <- rep(NA, ntrials)

for(i in 1:nsim){
  for(j in 1:ntrials){
  value1 = sample(x = seq(1, 6, 1), size = i, replace = TRUE)
  value2 = sample(x = seq(1, 6, 1), size = i, replace = TRUE)
  sums <- value1 + value2
  samp.means[j] <- mean(sums)
  }
  samp.sds[i] <- sd(samp.means)
}

n <- seq(1, nsim, 1)

df.se.sims <- data.frame(cbind(n, samp.sds))

str(df.se.sims)

plot.100 <-
  df.se.sims%>%
  ggplot(aes(x = n, y = samp.sds))+
  geom_point(alpha = .5)+
  geom_hline(yintercept = 2.42, lty = "dashed", color = "red")+
  ylim(c(0, 2.5))+
  theme_apa()+
  labs(y = "Sampling SD",
       x = "N")

plot.100

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.LLN.100SDs.Dice.Roll.Trials.png")

Ns <- seq(1, 100, 1)

SEs <- dice.sd.pop/sqrt(Ns)

df.pop <- data.frame(Ns, SEs)

ggplot()+
  geom_point(aes(x = n, y = samp.sds), data = df.se.sims, alpha = .5)+
  geom_line(aes(x = Ns, y = SEs), data = df.pop, lty = "dashed", color = "red")+
  ylim(c(0, 2.5))+
  theme_apa()+
  labs(y = "Sampling SD",
       x = "N")

ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.LLN.Guide.100SDs.Dice.Roll.Trials.png")

set.seed(1)

ntrials <- 500
nrolls <- 500

dice.means <- rep(NA, ntrials)

for(i in 1:ntrials){
  roll1 <- sample(x = seq(1, 6, 1), size = nrolls, replace = TRUE)
  roll2 <- sample(x = seq(1, 6, 1), size = nrolls, replace = TRUE)
  sum <- roll1 + roll2
  dice.means[i] <- mean(sum)
}

df.500.500.rolls <- data.frame(dice.means)

ggplot()+
  geom_histogram(aes(x = dice.means), data = df.500.500.rolls, binwidth = .045)+
  theme_apa()+
  labs(x = "Mean of Dice Rolls",
       y = "Count")+
  xlim(c(6.5, 7.5))

se.500 <- dice.sd.pop/sqrt(500)

ci.l <- qnorm(.025, 7, se.500)
ci.u <- qnorm(.975, 7, se.500)

qnorm(.025)
qnorm(.975)

x <- seq(6.5, 7.5, .01)
y <- dnorm(x, 7, se.500)

df <- data.frame(x, y)

df%>%
  ggplot()+
  geom_line(aes(x = x, y = y))+
  geom_segment(x = ci.l, xend = ci.l, y = 0, yend = y, lty = "dashed", color = "red")+
  geom_segment(x = ci.u, xend = ci.u, y = 0, yend = y, lty = "dashed", color = "red")+
  theme_apa()+
  labs(x = "Sampling Means",
       y = "")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  
ggsave("/Users/lutznm/Desktop/Lurigio Book/R Output/Ch.6.CI.Dice.Roll.Trials.png")

means <- c(23.6, 41.8, 20.7)
sds <- c(4.3, 9.7, 1.2)
ns <- c(90, 40, 70)

df <- data.frame(means, sds, ns)

df$se <- sds/sqrt(ns - 1)

df$ci.l <- df$means - (df$se*1.96)
df$ci.u <- df$means + (df$se*1.96)

df
