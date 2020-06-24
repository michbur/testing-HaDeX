library(HaDeX)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(reshape2)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
calc_dat <- lapply(unique(dat[["State"]]), function(x){
  calculate_state_deuteration(dat,
                              protein = dat[["Protein"]][1],
                              state = x,
                              time_in = 0.001,
                              time_chosen = 1,
                              time_out = 1440)
}) %>%
  bind_rows() %>%
  select(Protein, Sequence, State, Start, End, frac_exch_state,  abs_frac_exch_state, abs_avg_theo_in_time) %>%
  mutate(N = nchar(Sequence),
         calc_abs = frac_exch_state * N / 100,
         MaxUptake = N - 2 - str_count(Sequence, "P"),
         calc_abs_maxuptake = frac_exch_state * MaxUptake / 100) %>%
  mutate(calc_abs_diff = abs(abs_frac_exch_state - calc_abs),
         calc_abs_maxuptake_diff = abs(abs_frac_exch_state - calc_abs_maxuptake),
         calc_abs_avg_theo_in_time_diff = abs(abs_avg_theo_in_time - calc_abs))


p <- select(calc_dat, 
       calc_abs_diff, 
       calc_abs_maxuptake_diff,
       calc_abs_avg_theo_in_time_diff,
       State) %>% 
  melt %>% 
  ggplot() +
  geom_histogram(aes(x = value)) +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 20)) +
  facet_grid(variable ~ State) +
  theme_bw()

png("p.png")
print(p)
dev.off()
