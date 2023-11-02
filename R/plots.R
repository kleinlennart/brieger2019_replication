# Figure 1. Environmental concern and communal identity.

dat <- readRDS("data/export/cleaned_dat.rds")
clean <- dat %>% drop_na()

country_stats <- readRDS("data/export/country_stats.rds")
# drop_na()

country_stats %>%
  drop_na() %>%
  ggplot(aes(x = sid_communal_avg, y = env_concern_avg)) +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  ggrepel::geom_text_repel(aes(label = country_chr), size = 3) +
  theme_bw()
