library(nonmem2rx)
library(rxode2)
library(tidyvpc)
library(tidyverse)
library(magrittr)
library(psych)
asf <- nonmem2rx(file = "zpd6.ctl")

#not nominal time ----

dosing <- eventTable(
  amount.units="mg", 
  time.units="hr"
) |> 
  add.dosing(dose=10, nbr.doses=1) |> 
  add.sampling(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12)) |> 
  et(id = 1:23)

dosing1 <- tibble(
  id   = rep(c(1:23), each = 13),
  low  = NA,
  time = obs$TIME,
  high = NA,
  cmt  = rep( c("(obs)", "(default)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)", "(obs)"), time = 23 ),
  amt  = ifelse(cmt == "(default)", 10, NA),
  rate = ifelse(cmt == "(default)", 0, NA),
  ii   = ifelse(cmt == "(default)", 0, NA),
  addl = ifelse(cmt == "(default)", 0, NA),
  evid = ifelse(cmt == "(default)", 1, 0),
  ss   = ifelse(cmt == "(default)", 0, NA),
  dur  = ifelse(cmt == "(default)", 0, NA)
)


sim <- rxSolve(asf, dosing, nSub = 23, nsim = 1000)

sim |> plot(ipred)
sim_tidy <- sim |> 
  select(sim.id, id, time, ipred) |> 
  mutate(NTIME = rep(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12), time = 23000))


obs <- read.csv("PK_Zolpidem_MFDS_COV_1.csv") |> 
  filter(CMT == 2) |> 
  mutate(DV = as.double(DV),
         DV = ifelse(is.na(DV),0, DV),
         NTIME = rep(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12), time = 23))

vpc <- observed(obs, x = NTIME, y = DV) |> 
  simulated(sim2, y = ipred) |> 
  binless(qpred = c(0.05, 0.5, 0.95), optimize = FALSE, lambda = c(1,3,2)) |> 
  vpcstats()


plot(vpc)

sim_tidy |> 
  filter(time == 0)

ggplot(vpc$stats, aes(x=x)) +
  
  geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
  geom_line(aes(y=md, col=qname, group=qname)) +
  scale_colour_manual(
    name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks=c("q0.05", "q0.5", "q0.95"),
    values=c("red", "blue", "red"),
    labels=c("5%", "50%", "95%")) +
  scale_fill_manual(
    name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks=c("q0.05", "q0.5", "q0.95"),
    values=c("red", "blue", "red"),
    labels=c("5%", "50%", "95%")) +
  scale_linetype_manual(
    name="Observed Percentiles\n(black lines)",
    breaks=c("q0.05", "q0.5", "q0.95"),
    values=c("dotted", "solid", "dashed"),
    labels=c("5%", "50%", "95%")) +
  guides(
    fill=guide_legend(order=2),
    colour=guide_legend(order=2),
    linetype=guide_legend(order=1)) +
  theme(
    legend.position="top",
    legend.key.width=grid::unit(1, "cm")) +
  labs(x="Time (h)", y="Concentration (ng/mL)")


View(vpc$stats)


sim_0.05 <- sim_tidy |> 
  group_by(time) |> 
  mutate(`q05` = quantile(ipred,0.05),
         sd = sd(`q05`)) |> 
  distinct(id, .keep_all = TRUE) |> 
  mutate(hi  = `q05` + 1.96*sd/sqrt(1000),
         low = `q05` - 1.96*sd/sqrt(1000)
  )



ggplot() + geom_point(data = obs, aes(x = TIME, y = DV))


sim_0.05 <- sim_tidy |> 
  mutate(sim.id = as.character(sim.id),
         id = as.character(id),
         time = as.double(time),
         ipred = as.double(ipred)
  ) |>
  summarize(median = median(ipred), .by = c(time)
  )
sim |> 
  write.csv("sim.csv")



# nominal time ----


dosing <- zolpidem_dosing <- eventTable(
  amount.units="mg", 
  time.units="hr"
) |> 
  add.dosing(dose=10, nbr.doses=1) |> 
  add.sampling(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12)) |> 
  et(id = 1:23)

sim2 <- rxSolve(asf, dosing, nSub= 23, nsim = 1000)

sim_tidy2 <- sim2 |> 
  select(sim.id, id, time, ipred) 

q05 <- sim_tidy2 |> 
  group_by(sim.id, time) |> 
  mutate(q05 = quantile(ipred, 0.05)
  ) |> 
  ungroup(sim.id) |> 
  mutate(median = median(q05),
         mean = mean(q05),
         sd  = sd(q05)
  ) |> 
  select(1,2,3,5,6,7,8) |> 
  rename(ipred = q05) |> 
  mutate(qname = "q05",
         hi = mean + 1.96*sd/sqrt(1000),
         low = mean - 1.96*sd/sqrt(1000)
  )


q50 <- sim_tidy2 |> 
  group_by(sim.id, time) |> 
  mutate(q50 = quantile(ipred, 0.5)
  ) |> 
  ungroup(sim.id) |> 
  mutate(median = median(q50),
         mean = mean(q50),
         sd  = sd(q50)
  ) |> 
  select(1,2,3,5,6,7,8) |> 
  rename(ipred = q50) |> 
  mutate(qname = "q50",
         hi = mean + 1.96*sd/sqrt(1000),
         low = mean - 1.96*sd/sqrt(1000))

q95 <- sim_tidy2 |> 
  group_by(sim.id, time) |> 
  mutate(q95 = quantile(ipred, 0.95)
  ) |> 
  ungroup(sim.id) |> 
  mutate(median = median(q95),
         mean = mean(q95),
         sd  = sd(q95)
  ) |> 
  select(1,2,3,5,6,7,8) |> 
  rename(ipred = q95) |> 
  mutate(qname = "q95",
         hi = mean + 1.96*sd/sqrt(1000),
         low = mean - 1.96*sd/sqrt(1000))



conc <- bind_rows(q05, q50, q95)


ggplot(data = conc, aes(x = time)) + 
  geom_ribbon(aes(ymin=low, ymax=hi, fill=qname,  group=qname), alpha=0.1, col=NA) +
  geom_line(aes(y=median, col=qname, group=qname)) +
  scale_colour_manual(
    name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks=c("q0.05", "q0.5", "q0.95"),
    values=c("red", "blue", "red"),
    labels=c("5%", "50%", "95%")
  ) +
  scale_fill_manual(
    name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks=c("q0.05", "q0.5", "q0.95"),
    values=c("red", "blue", "red"),
    labels=c("5%", "50%", "95%")
  ) +
  scale_linetype_manual(
    name="Observed Percentiles\n(black lines)",
    breaks=c("q0.05", "q0.5", "q0.95"),
    values=c("dotted", "solid", "dashed"),
    labels=c("5%", "50%", "95%")) +
  guides(
    fill=guide_legend(order=2),
    colour=guide_legend(order=2),
    linetype=guide_legend(order=1)) +
  theme(
    legend.position="top",
    legend.key.width=grid::unit(1, "cm")) +
  labs(x="Time (h)", y="Concentration (ng/mL)")


View(vpc$stats)

#0305 ----


dosing <- eventTable(
  amount.units="mg", 
  time.units="hr"
) |> 
  add.dosing(dose=10, nbr.doses=1) |> 
  add.sampling(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12)) |> 
  et(id = 1:23)

sim <- rxSolve(asf, dosing, nSub = 23, nsim = 500)

sim |> plot(ipred)


obs <- read.csv("PK_Zolpidem_MFDS_COV_1.csv") |> 
  filter(CMT == 2) |> 
  mutate(DV = as.double(DV),
         DV = ifelse(is.na(DV),0, DV),
         NTIME = rep(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12), time = 23),
         obs ="obs")


sim_tidy <- sim |> 
  select(sim.id, sim ,time, ipred) |> 
  mutate(NTIME = rep(c(0,0.25,0.5,0.75, 1, 1.5,2,3,4,6,8,12), time = 11500),
         TIME = time
  )



#plot
library(tidyvpc)
vpc <- observed(obs, x = TIME, y= DV  ) |> 
  simulated(sim_tidy, y = ipred) |> 
  binning(bin = NTIME) |> 
  vpcstats()
plot(vpc)


View(vpc$stats)


ggplot(vpc$stats, aes(x=xbin)) +
  geom_ribbon(aes(ymin=lo, ymax=hi, fill=qname, col=qname, group=qname), alpha=0.1, col=NA) +
  geom_line(aes(y=md, col=qname, group=qname)) +
  scale_colour_manual(
    name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks=c("q0.05", "q0.5", "q0.95", "Obs"),
    values=c("blue", "red", "blue", "black"),
    labels=c("5%", "50%", "95%", "Observed")) +
  scale_fill_manual(
    name="Simulated Percentiles\nMedian (lines) 95% CI (areas)",
    breaks=c("q0.05", "q0.5", "q0.95", "Obs"),
    values=c("blue", "red", "blue", "black"),
    labels=c("5%", "50%", "95%", "Observed")) +
  geom_point(data = obs, aes(x = TIME, y = DV, shape = "Observed"), color = "black", alpha = 0.3) +
  scale_shape_manual(
    name = "Observed Concetration (μg/L)",
    values = c("Observed" = 19),
    labels = "Obs ") +
  guides(
    fill=guide_legend(order=2),
    colour=guide_legend(order=2),
    shape=guide_legend(order=1)) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color="black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 15)) +
  labs(x="Time (h)", y="Plasma Concentration (μg/L)") + scale_x_continuous(breaks = seq(0,12,2)) + scale_y_continuous(breaks = seq(0,300, 50))




