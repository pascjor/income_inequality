save_as_png <- function(df_model)
{
  library(flextable)
  library(webshot2)
  library(tidyverse)
  set_flextable_defaults(background.color = "white")
  model_estimates_rd <- df_model %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>% rename(Ethnizität=race)
  
  ft <- flextable(model_estimates_rd) %>%
    set_header_labels(mu = "\u03BC", sigma2 = "\u03C3\u00B2") %>% 
    autofit() %>% theme_box() %>% fontsize(size = 10)
  save_as_image(ft, path = "bs_table.png")
}