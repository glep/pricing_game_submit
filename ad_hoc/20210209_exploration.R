source("model.R")  # Load your code.
theme_set(theme_bw())

res_gam <- load_model()

res_gam$gam_model$model$dinfo$gp %>% 
  map(lobstr::obj_size)

environment(res_gam$gam_model$model$dinfo$gp$pf) <- NULL
environment(res_gam$gam_model$model$dinfo$gp$fake.formula) <- NULL
environment(res_gam$gam_model$model$dinfo$gp$pred.formula) <- NULL

environment(res_gam$gam_model$recipe$steps[[1]]$inputs$density) <- NULL
environment(res_gam$gam_model$recipe$steps[[2]]$inputs$density) <- NULL
environment(res_gam$gam_model$recipe$steps[[3]]$inputs$density) <- NULL
environment(res_gam$gam_model$recipe$steps[[4]]$inputs$density) <- NULL
environment(res_gam$gam_model$recipe$steps[[5]]$inputs$density) <- NULL
environment(res_gam$gam_model$recipe$steps[[6]]$inputs$density) <- NULL
for (k in seq_along(res_gam$gam_model$recipe$steps)) {
  for (xx in names(res_gam$gam_model$recipe$steps[[k]]$inputs)) {
    environment(res_gam$gam_model$recipe$steps[[k]]$inputs[[xx]]) <- NULL
  }
}

res_gam$gam_model$recipe$steps[[4]] %>% names

res_gam$gam_model$recipe$steps %>% 
  map(lobstr::obj_size)

res_gam$gam_model$model %>% summary
res_gam$gam_model$model %>% plot

pred <- res_gam$gam_model$model$fitted.values


df <- read_csv("training.csv")

df$pred <- pred

ee <- df %>% select(id_policy, vh_make_model, claim_amount, pred)

rr <- ee %>%
  group_nest(vh_make_model) %>% 
  rowwise() %>% 
  mutate(nb = n_distinct(data$id_policy)) %>% 
  arrange(desc(nb)) %>% 
  filter(nb > 100) %>% 
  mutate(
    data1 = 
      list(
        replicate(
          n = 100,
          expr = {
            data %>% 
              slice_sample(n = floor(nb * .5), replace = TRUE) %>% 
              summarise(lr = sum(claim_amount) / sum(pred)) %>% 
              pull(lr)
          }
        )
      )
  )
rr %>%
  select(vh_make_model, nb, data1) %>% 
  unnest(data1) %>% 
  mutate(data1 = data1 %>% pmin(2) %>% pmax(0)) %>% 
  mutate(vh_make_model = forcats::fct_reorder(vh_make_model, data1, mean)) %>% 
  ggplot(aes(vh_make_model, data1)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 1), linetype = 3, color = "blue") +
  coord_flip() +
  ylim(c(0, 2))

rr %>%
  select(vh_make_model, nb, data1) %>% 
  mutate(
    niveau = mean(data1, trim = .05),
    freq_gt_1 = mean(data1 > 1),
    freq_lt_1 = mean(data1 < 1)
    ) %>% 
  arrange(niveau) %>% 
  view
  print
  unnest(data1) %>% 
  group_by(mak)