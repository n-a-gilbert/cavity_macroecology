library(here)
library(tidyverse)
library(brms)
library(tidybayes)
library(MCMCvis)

# load model objects and formatted data
load( here::here("results/us_canada_edge_results2.RData"))

# new data for prediction - across species
m1_nd <- tidyr::expand_grid(
  x = seq(from = min(final1$x), 
          to = max(final1$x), 
          length.out = 100))

# new data for prediction - species level
m1_nd_sp <- tidyr::expand_grid(
  x = seq(from = min(final1$x), 
          to = max(final1$x), 
          length.out = 100),
  code = unique(final1$code))

# scale to get the attributes for backtransforming
m1_sc <- scale(log1p(final1$value))

# model predictions: cross-species
m1_p <- tidybayes::add_linpred_draws(
  newdata = m1_nd, 
  object = m1_brm,
  ndraws = 4000,
  re_formula = NA,
  scale = "response") |> 
  dplyr::group_by(x) |> 
  dplyr::summarise(
    mean = mean(.linpred), 
    l95 = quantile(.linpred, c(0.025)),
    u95 = quantile(.linpred, c(0.975))) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "all other species") |> 
  dplyr::mutate( xnew = ( x*attr(m1_sc, "scaled:scale")) + attr(m1_sc, "scaled:center")) |> 
  dplyr::filter(xnew >= 0)

# model predictions: species level
m1_p_sp <- tidybayes::add_linpred_draws(
  newdata = m1_nd_sp, 
  object = m1_brm,
  ndraws = 4000,
  # re_formula = NA,
  scale = "response") |> 
  dplyr::group_by(x, code) |> 
  dplyr::summarise(
    mean = mean(.linpred), 
    l95 = quantile(.linpred, c(0.025)),
    u95 = quantile(.linpred, c(0.975))) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "all other species") |> 
  dplyr::mutate( xnew = ( x*attr(m1_sc, "scaled:scale")) + attr(m1_sc, "scaled:center")) |> 
  dplyr::filter(xnew >= 0)

m1_coeff <- MCMCvis::MCMCpstr(
  m1_brm, 
  params = "x",
  type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer( dplyr::starts_with("V"), 
                       names_to = "iter", 
                       values_to = "val") |> 
  dplyr::summarise(mean = mean(val), 
                   l95 = quantile(val, c(0.025)), 
                   l68 = quantile(val, c(0.160)), 
                   u68 = quantile(val, c(0.840)), 
                   u95 = quantile(val, c(0.975))) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "all other species")

### same process for m2
m2_nd <- tidyr::expand_grid(
  x = seq(from = min(final2$x), 
          to = max(final2$x), 
          length.out = 100))

m2_sc <- scale(log1p(final2$value))

m2_p <- tidybayes::add_linpred_draws(
  newdata = m2_nd, 
  object = m2_brm,
  ndraws = 4000,
  re_formula = NA,
  scale = "response") |> 
  dplyr::group_by(x) |> 
  dplyr::summarise(
    mean = mean(.linpred), 
    l95 = quantile(.linpred, c(0.025)),
    u95 = quantile(.linpred, c(0.975))) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "w/in 50% mass") |>
  dplyr::mutate( xnew = ( x*attr(m2_sc, "scaled:scale")) + attr(m2_sc, "scaled:center")) |> 
  dplyr::filter(xnew >= 0)

m2_coeff <- MCMCvis::MCMCpstr(
  m2_brm, 
  params = "x",
  type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer( dplyr::starts_with("V"), 
                       names_to = "iter", 
                       values_to = "val") |> 
  dplyr::summarise(mean = mean(val), 
                   l95 = quantile(val, c(0.025)), 
                   l68 = quantile(val, c(0.160)), 
                   u68 = quantile(val, c(0.840)), 
                   u95 = quantile(val, c(0.975))) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "w/in 50% mass")

### m3
m3_nd <- tidyr::expand_grid(
  x = seq(from = min(final3$x), 
          to = max(final3$x), 
          length.out = 100))

m3_sc <- scale(log1p(final3$value))

m3_p <- tidybayes::add_linpred_draws(
  newdata = m3_nd, 
  object = m3_brm,
  ndraws = 4000,
  re_formula = NA,
  scale = "response") |> 
  dplyr::group_by(x) |> 
  dplyr::summarise(
    mean = mean(.linpred), 
    l95 = quantile(.linpred, c(0.025)),
    u95 = quantile(.linpred, c(0.975))) |> 
  tibble::add_column( guild = "excavator", 
                      size = "all other species")|> 
  dplyr::mutate( xnew = ( x*attr(m3_sc, "scaled:scale")) + attr(m3_sc, "scaled:center")) |> 
  dplyr::filter(xnew >= 0)

m3_coeff <- MCMCvis::MCMCpstr(
  m3_brm, 
  params = "x",
  type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer( dplyr::starts_with("V"), 
                       names_to = "iter", 
                       values_to = "val") |> 
  dplyr::summarise(mean = mean(val), 
                   l95 = quantile(val, c(0.025)), 
                   l68 = quantile(val, c(0.160)), 
                   u68 = quantile(val, c(0.840)), 
                   u95 = quantile(val, c(0.975))) |> 
  tibble::add_column( guild = "excavator", 
                      size = "all other species")

### m4
m4_nd <- tidyr::expand_grid(
  x = seq(from = min(final4$x), 
          to = max(final4$x), 
          length.out = 100))

m4_sc <- scale(log1p(final4$value))

m4_p <- tidybayes::add_linpred_draws(
  newdata = m4_nd, 
  object = m4_brm,
  ndraws = 4000,
  re_formula = NA,
  scale = "response") |> 
  dplyr::group_by(x) |> 
  dplyr::summarise(
    mean = mean(.linpred), 
    l95 = quantile(.linpred, c(0.025)),
    u95 = quantile(.linpred, c(0.975))) |> 
  tibble::add_column( guild = "excavator", 
                      size = "w/in 50% mass") |> 
  dplyr::mutate( xnew = ( x*attr(m4_sc, "scaled:scale")) + attr(m4_sc, "scaled:center")) |> 
  dplyr::filter(xnew >= 0)

m4_coeff <- MCMCvis::MCMCpstr(
  m4_brm, 
  params = "x",
  type = "chains")[[1]] |> 
  tibble::as_tibble() |> 
  tidyr::pivot_longer( dplyr::starts_with("V"), 
                       names_to = "iter", 
                       values_to = "val") |> 
  dplyr::summarise(mean = mean(val), 
                   l95 = quantile(val, c(0.025)), 
                   l68 = quantile(val, c(0.160)), 
                   u68 = quantile(val, c(0.840)), 
                   u95 = quantile(val, c(0.975))) |> 
  tibble::add_column( guild = "excavator", 
                      size = "w/in 50% mass")

# join up all coefficients (slopes of heterospecific abundance) for annotation
coef_all <-
  dplyr::full_join(
    m1_coeff, m2_coeff) |> 
  dplyr::full_join(
    m3_coeff) |> 
  dplyr::full_join(
    m4_coeff) |> 
  dplyr::mutate( label = paste0(
    sprintf("%.2f", round(mean,2)), " (95% CI: ", sprintf("%.2f", round(l95, 2)), ", ", 
    sprintf("%.2f", round(u95, 2)), ")")) |>
  dplyr::select(-mean) |> 
  tibble::add_column(xnew = c(3.5,1.6,3.5,3.5),
                     mean = 0.12) |> 
  dplyr::mutate(guild = factor(guild, 
                               levels = c("non-excavator", "excavator")))

# collate and prepare for plotting
me_all <- dplyr::full_join(m1_p, m2_p) |> 
  dplyr::full_join(m3_p) |> 
  dplyr::full_join(m4_p) |> 
  dplyr::mutate(guild = factor(guild, 
                               levels = c("non-excavator", "excavator")))

# figure 3: cross species results
ggplot() +
  facet_grid(size ~ guild) +
  geom_ribbon(data = me_all, aes(x = xnew, ymin = l95, ymax = u95), color = NA, alpha = 0.4) +
  geom_line(data = me_all, aes(x = xnew, y = mean), linewidth = 1.5) +
  geom_text(data = coef_all, aes(x = xnew, y = mean, label = label),
            size = 2.45) +
  theme_minimal() +
  labs(x = "ln( 1 + abundance of other species)",
       y = "focal non-excavator abundance") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2),
    axis.title = element_text(color = "black", size = 9), 
    axis.text = element_text(color = "black", size = 8), 
    strip.text = element_text(color = "black", size = 9, face = "bold"),
    panel.background = element_rect(color = NA, fill = "gray95"),
    plot.background = element_rect(color = NA, fill = "white"))

ggsave(
  filename = here::here("figures/figure_03.png"), 
  width = 5, 
  height = 3.5, 
  units = "in", 
  dpi = 600)

# key of codes/species names
key <- final1 |> 
  dplyr::select(sp = code, com, sci = scientific_name) |> 
  dplyr::distinct()

# species-level slopes
m1_ranef <- ranef(m1_brm,
                  probs = c(0.025, 0.160, 0.840, 0.975))$code |> 
  reshape2::melt() |> 
  tibble::as_tibble() |> 
  dplyr::filter(!Var2 == "Est.Error") |> 
  dplyr::filter(!Var3 == "Intercept") |> 
  dplyr::select(
    sp = Var1, 
    param = Var3,
    stat = Var2,
    value) |> 
  tidyr::pivot_wider(names_from = stat, values_from = value) |> 
  janitor::clean_names() |> 
  dplyr::rename(l95 = q2_5,
                l68 = q16,
                u68 = q84,
                u95 = q97_5) |> 
  dplyr::left_join(key) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "all other species")


m2_ranef <- ranef(m2_brm,
                  probs = c(0.025, 0.160, 0.840, 0.975))$code |> 
  reshape2::melt() |> 
  as_tibble() |> 
  dplyr::filter(!Var2 == "Est.Error") |> 
  dplyr::filter(!Var3 == "Intercept") |> 
  dplyr::select(
    sp = Var1, 
    param = Var3,
    stat = Var2,
    value) |> 
  tidyr::pivot_wider(names_from = stat, values_from = value) |> 
  janitor::clean_names() |> 
  dplyr::rename(l95 = q2_5,
                l68 = q16,
                u68 = q84,
                u95 = q97_5) |> 
  dplyr::left_join(key) |> 
  tibble::add_column( guild = "non-excavator", 
                      size = "w/in 50% mass")

m3_ranef <- ranef(m3_brm,
                  probs = c(0.025, 0.160, 0.840, 0.975))$code |>  
  reshape2::melt() |> 
  as_tibble() |> 
  dplyr::filter(!Var2 == "Est.Error") |> 
  dplyr::filter(!Var3 == "Intercept") |> 
  dplyr::select(
    sp = Var1, 
    param = Var3,
    stat = Var2,
    value) |> 
  tidyr::pivot_wider(names_from = stat, values_from = value) |> 
  janitor::clean_names() |> 
  dplyr::rename(l95 = q2_5,
                l68 = q16,
                u68 = q84,
                u95 = q97_5) |> 
  dplyr::left_join(key) |> 
  tibble::add_column( guild = "excavator", 
                      size = "all other species")

m4_ranef <- ranef(m4_brm,
                  probs = c(0.025, 0.160, 0.840, 0.975))$code |>  
  reshape2::melt() |> 
  as_tibble() |> 
  dplyr::filter(!Var2 == "Est.Error") |> 
  dplyr::filter(!Var3 == "Intercept") |> 
  dplyr::select(
    sp = Var1, 
    param = Var3,
    stat = Var2,
    value) |> 
  tidyr::pivot_wider(names_from = stat, values_from = value) |> 
  janitor::clean_names() |> 
  dplyr::rename(l95 = q2_5,
                l68 = q16,
                u68 = q84,
                u95 = q97_5) |> 
  dplyr::left_join(key) |> 
  tibble::add_column( guild = "excavator", 
                      size = "w/in 50% mass")

# bean-count how many species show positive and negative effects
dplyr::full_join(m1_ranef, m2_ranef) |> 
  dplyr::full_join(m3_ranef) |> 
  dplyr::full_join(m4_ranef) |> 
  dplyr::mutate(overlap = ifelse(l95 < 0 & u95 > 0, "overlap", "no overlap")) |> 
  dplyr::mutate(sign = ifelse(estimate < 0, "negative", "positive")) |> 
  dplyr::filter(overlap == "no overlap") |> 
  dplyr::group_by(guild, size, sign) |> 
  dplyr::count()

# summary table for a "heatmap" visualization of species-level effects
for_heatmap <- dplyr::full_join(m1_ranef, m2_ranef) |> 
  dplyr::full_join(m3_ranef) |> 
  dplyr::full_join(m4_ranef) |> 
  dplyr::mutate(sign = ifelse(estimate < 0, "negative", "positive")) |> 
  dplyr::mutate(overlap95 = ifelse(l95 < 0 & u95 > 0, "overlap", "no overlap")) |> # does 95% CI overlap 0
  dplyr::mutate(overlap68 = ifelse(l68 < 0 & u68 > 0, "overlap", "no overlap")) |> # does 68% CI overlap 0
  # label - what is the direction of the effect and how certain are we
  dplyr::mutate( effect = 
                   ifelse( overlap95 == "no overlap" & sign == "negative", "decreases abundance (95% confidence)", 
                           ifelse(overlap95 == "no overlap" & sign == "positive", "increases abundance (95% confidence)", 
                                  ifelse(overlap68 == "no overlap" & overlap95 == "overlap" & sign == "negative", 
                                         "decreases abundance (68% confidence)",
                                         ifelse(overlap68 == "no overlap" & overlap95 == "overlap" & sign == "positive", 
                                                "increases abundance (68% confidence)", "no effect"))))) |> 
  dplyr::mutate(label = ifelse(guild == "non-excavator" & size == "all other species", 
                               "non-excavators\n(all)",
                               ifelse(guild == "non-excavator" & size == "w/in 50% mass", 
                                      "non-excavators\n(similar size)", 
                                      ifelse(guild == "excavator" & size == "all other species", 
                                             "excavators\n(all)", "excavators\n(similar size)")))) |> 
  dplyr::mutate(label = factor(label, 
                               levels = c(
                                 "non-excavators\n(similar size)", 
                                 "non-excavators\n(all)",
                                 "excavators\n(similar size)",
                                 "excavators\n(all)"))) |> 
  dplyr::mutate( effect = 
                   factor(effect, 
                          levels = c(
                            "decreases abundance (95% confidence)",
                            "decreases abundance (68% confidence)",
                            "no effect",
                            "increases abundance (68% confidence)",
                            "increases abundance (95% confidence)")))

# score species so they're ordered from mostly facilitation at top to mostly competition at bottom
sp_levels <- for_heatmap |> 
  dplyr::mutate(pos68 = ifelse(estimate > 0 & overlap68 == "no overlap", 1, 0),
                neg68 = ifelse(estimate < 0 & overlap68 == "no overlap", 1, 0), 
                pos95 = ifelse(estimate > 0 & overlap95 == "no overlap", 1, 0), 
                neg95 = ifelse(estimate < 0 & overlap95 == "no overlap", 1, 0)) |> 
  dplyr::group_by(sp, com, sci) |>
  dplyr::mutate( sum_pos68 = sum(pos68), 
                 sum_neg68 = sum(neg68),
                 sum_pos95 = sum(pos95), 
                 sum_neg95 = sum(neg95)) |>
  dplyr::select(sp, com, sci, sum_pos68:sum_neg95) |>
  dplyr::distinct() |>
  dplyr::mutate( lab = (-1*sum_neg68) + (-1.05*sum_neg95) + (1.05 * sum_pos95) + sum_pos68) |>
  dplyr::arrange(lab, com) |>
  dplyr::pull(com) 

hm2 <- for_heatmap |> 
  dplyr::mutate(pos68 = ifelse(estimate > 0 & overlap68 == "no overlap", 1, 0),
                neg68 = ifelse(estimate < 0 & overlap68 == "no overlap", 1, 0), 
                pos95 = ifelse(estimate > 0 & overlap95 == "no overlap", 1, 0), 
                neg95 = ifelse(estimate < 0 & overlap95 == "no overlap", 1, 0)) |> 
  dplyr::group_by(sp, com, sci) |>
  dplyr::mutate( sum_pos68 = sum(pos68), 
                 sum_neg68 = sum(neg68),
                 sum_pos95 = sum(pos95), 
                 sum_neg95 = sum(neg95)) |>
  dplyr::mutate(com = factor(com, levels = sp_levels))

ggplot() +
  geom_tile(data = hm2, aes(x = label, y = com, fill = effect)) +
  scale_x_discrete(position = "top") + 
  scale_fill_manual(values = c(
    MetBrewer::MetPalettes$Isfahan1[[1]][2],
    MetBrewer::MetPalettes$Isfahan1[[1]][4],
    "gray90",
    MetBrewer::MetPalettes$Isfahan1[[1]][5],
    MetBrewer::MetPalettes$Isfahan1[[1]][7])) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = -0.05,
                                   vjust = -20,
                                   color = "black",
                                   size = 9),
        axis.text.y = element_text(color = "black", 
                                   size = 8),
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.2),
        strip.text = element_text(color = "black", size = 10, face = "bold"),
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8),
        legend.key.height = unit(1.5, "pt"),
        legend.justification = "left",
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        legend.margin = margin(-5, 0, 0, 0)) +
  guides(fill = guide_legend(ncol = 1))

ggsave(
  filename = here::here("figures/figure_04.png"), 
  width = 4.5, 
  height = 9, 
  units = "in", 
  dpi = 600)

# identify which species show some evidence of competition and facilitation patterns, 
# i.e. at least one negative estimate of non-excavator heterospecifics and one positive estimate of excavator heterospecifics
for_heatmap |> 
  dplyr::select(sp, size, guild, sign, overlap95) |> 
  dplyr::mutate(comp_flag = ifelse(sign == "negative" & overlap95 == "no overlap" & grepl("non", guild), 1, 0),
                fac_flag = ifelse(sign == "positive" & overlap95 == "no overlap" & guild == "excavator", 1, 0)) |> 
  dplyr::group_by(sp) |> 
  dplyr::summarise(comp = sum(comp_flag), 
                   fac = sum(fac_flag)) |> 
  dplyr::filter(comp > 0 & fac > 0)

# CREATE FIGURE 5
# Patterns for Great Crested Flycatcher - one of the few that showed the expected patterns

# get scaling attributes for back transformation
final1.sc <- scale(final1$value)
final2.sc <- scale(final2$value)
final3.sc <- scale(final3$value)
final4.sc <- scale(final4$value)

# get minimum and maximum of response variable for each model
gcfl_m1_min_max <- final1 |> 
  dplyr::filter(com == "Great Crested Flycatcher") |> 
  dplyr::filter(x == min(x) | x == max(x)) |> 
  pull(x) |> sort()

gcfl_m2_min_max <- final2 |> 
  dplyr::filter(com == "Great Crested Flycatcher") |> 
  dplyr::filter(x == min(x) | x == max(x)) |> 
  pull(x) |> sort()

gcfl_m3_min_max <- final3 |> 
  dplyr::filter(com == "Great Crested Flycatcher") |> 
  dplyr::filter(x == min(x) | x == max(x)) |> 
  pull(x) |> sort()

gcfl_m4_min_max <- final4 |> 
  dplyr::filter(com == "Great Crested Flycatcher") |> 
  dplyr::filter(x == min(x) | x == max(x)) |> 
  pull(x) |> sort()

# "new data" tables for each model
gcfl_nd_m1 <- tidyr::expand_grid(
  code = "grcfly", 
  x = seq(from = min(gcfl_m1_min_max), 
          to = max(gcfl_m1_min_max) + 1, 
          length.out = 100))

gcfl_nd_m2 <- tidyr::expand_grid(
  code = "grcfly", 
  x = seq(from = min(gcfl_m2_min_max), 
          to = max(gcfl_m2_min_max) + 1, 
          length.out = 100))

gcfl_nd_m3 <- tidyr::expand_grid(
  code = "grcfly", 
  x = seq(from = min(gcfl_m3_min_max), 
          to = max(gcfl_m3_min_max) + 1, 
          length.out = 100))

gcfl_nd_m4 <- tidyr::expand_grid(
  code = "grcfly", 
  x = seq(from = min(gcfl_m4_min_max), 
          to = max(gcfl_m4_min_max) + 1, 
          length.out = 100))

# predictions / marginal effects for each model
gcfl_me_m1 <- tidybayes::add_epred_draws(
  gcfl_nd_m1, 
  m1_brm, 
  ndraws = 1000) |> 
  dplyr::group_by(code, x) |> 
  dplyr::summarise(mean = mean(.epred), 
                   l95 = quantile(.epred, c(0.025)), 
                   u95 = quantile(.epred, c(0.975))) |> 
  tibble::add_column(guild = "non-excavator",
                     size = "all other species") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(x = x*attr(final1.sc, "scaled:scale") + attr(final1.sc, "scaled:center"))

gcfl_me_m2 <- tidybayes::add_epred_draws(
  gcfl_nd_m2, 
  m2_brm, 
  ndraws = 1000) |> 
  dplyr::group_by(code, x) |> 
  dplyr::summarise(mean = mean(.epred), 
                   l95 = quantile(.epred, c(0.025)), 
                   u95 = quantile(.epred, c(0.975))) |> 
  tibble::add_column(guild = "non-excavator",
                     size = "w/in 50% mass") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(x = x*attr(final2.sc, "scaled:scale") + attr(final2.sc, "scaled:center"))

gcfl_me_m3 <- tidybayes::add_epred_draws(
  gcfl_nd_m3, 
  m3_brm, 
  ndraws = 1000) |> 
  dplyr::group_by(code, x) |> 
  dplyr::summarise(mean = mean(.epred), 
                   l95 = quantile(.epred, c(0.025)), 
                   u95 = quantile(.epred, c(0.975))) |> 
  tibble::add_column(guild = "excavator",
                     size = "all other species") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(x = x*attr(final3.sc, "scaled:scale") + attr(final3.sc, "scaled:center"))

gcfl_me_m4 <- tidybayes::add_epred_draws(
  gcfl_nd_m4, 
  m4_brm, 
  ndraws = 1000) |> 
  dplyr::group_by(code, x) |> 
  dplyr::summarise(mean = mean(.epred), 
                   l95 = quantile(.epred, c(0.025)), 
                   u95 = quantile(.epred, c(0.975))) |> 
  tibble::add_column(guild = "excavator",
                     size = "w/in 50% mass") |> 
  dplyr::ungroup() |> 
  dplyr::mutate(x = x*attr(final4.sc, "scaled:scale") + attr(final4.sc, "scaled:center"))

gcfl_p <- dplyr::full_join(
  gcfl_me_m1, gcfl_me_m2) |> 
  dplyr::full_join(gcfl_me_m3) |> 
  dplyr::full_join(gcfl_me_m4) |>
  dplyr::mutate(guild = factor(guild, levels = c("non-excavator", "excavator"))) |> 
  dplyr::mutate( sign = ifelse( size == "w/in 50% mass" & guild == "non-excavator", "negative", "positive")) |> 
  dplyr::filter(x > 0 & x < 19)

alldat <- final1 |> 
  dplyr::filter(com == "Great Crested Flycatcher") |> 
  dplyr::full_join(
    final2 |> 
      dplyr::filter(com == "Great Crested Flycatcher")) |> 
  dplyr::full_join(
    final3 |> 
      dplyr::filter(com == "Great Crested Flycatcher")) |> 
  dplyr::full_join(
    final4 |> 
      dplyr::filter(com == "Great Crested Flycatcher")) |>  
  dplyr::mutate(mass_ratio = tidyr::replace_na(mass_ratio, "all")) |>
  dplyr::mutate(mass_ratio = ifelse(mass_ratio == "all", "all other species", "w/in 50% mass")) |> 
  dplyr::mutate(group = ifelse(group == "secondary", "non-excavator", "excavator")) |> 
  dplyr::rename(size = mass_ratio, 
                guild = group) |> 
  dplyr::mutate(guild = factor(guild, levels = c("non-excavator", "excavator"))) |> 
  dplyr::select(-x) |> 
  dplyr::rename(x = value)

ggplot() +
  facet_grid(size~guild) +
  geom_point(data = alldat, aes(x = x, y = n), alpha = 0.1, color = "gray30") +
  geom_ribbon(data = gcfl_p, aes(x = x, ymin = l95, ymax = u95, fill = sign), color = NA, alpha = 0.3) +
  geom_line(data = gcfl_p, aes(x = x, y = mean, color = sign), linewidth = 1) +
  scale_color_manual(values = MetBrewer::MetPalettes$Isfahan1[[1]][c(2,7)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Isfahan1[[1]][c(2,7)]) +
  theme_minimal() +
  labs(x = "ln( 1 + abundance of other species)",
       y = "focal non-excavator abundance") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.2),
    axis.title = element_text(color = "black", size = 9), 
    axis.text = element_text(color = "black", size = 8), 
    strip.text = element_text(color = "black", size = 9, face = "bold"),
    panel.background = element_rect(color = NA, fill = "gray95"),
    plot.background = element_rect(color = NA, fill = "white"),
    legend.position = "none")

ggsave(
  here::here("figures/figure_05.png"), 
  width = 3.5, 
  height = 2.75, 
  units = "in", 
  dpi = 600)