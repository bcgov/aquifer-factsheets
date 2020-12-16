library(tesseract)
library(magrittr)
library(magick)
library(stringr)
library(tidyverse)

p <- tibble(f = list.files("figures/piperplots", full.names = TRUE, include.dirs = FALSE)) %>%
  filter(!str_detect(f, "\\.xlsx|\\.db")) %>%
  mutate(image = map(f, image_read),
         image = map(image, image_trim),
         info = map(image, image_info)) %>%
  unnest(info) %>%
  mutate(crop = height - width,
         txt = pmap(list(image, height, crop),
                    ~image_crop(..1, geometry = geometry_area(height = ..3))),
         image = pmap(list(image, height, crop),
                      ~image_crop(..1, geometry = geometry_area(height = ..2 - ..3,
                                                                y_off = ..3)))) %>%
  select(-format, -width, -height, -colorspace, -matte, -filesize, -density) %>%
  mutate(image = map(image, image_scale, 600),
         image = map2(image, f,
                      ~image_write(.x,
                                   path = file.path("figures/piperplots_trimmed", basename(.y)),
                                   format = "jpeg")))

         #txt = map_chr(image, ocr, engine = tesseract("eng")))

p %>%
  mutate(txt = str_trim(txt),
         ow = str_extract(tolower(txt), "ow[0-9]{1,4}"),
         txt_trim = str_remove(txt, ow),
         aquifer_id = str_extract(txt_trim, "[0-9]{1,4}"),
         missing_piperplot = !str_detect(txt, "Piperplot|Piper Plot"),
         leading_zero = any(str_detect(aquifer_id, "^0"), str_detect(ow, "^OW0"), na.rm = TRUE),
         extra_spaces = str_detect(txt, " "),
         underlines = str_detect(txt, "_"),
         inconsistent = !str_detect(txt, "Piperplot-[1-9]{1}[0-9]{1,4}-OW[1-9]{1}[0-9]{1,4}")) %>%
  filter(inconsistent) %>%
  arrange(missing_piperplot) %>%
  mutate(f = basename(f), other = inconsistent & !underlines & !extra_spaces & !missing_piperplot & !leading_zero) %>%
  select(file = f, title_in_file = txt, missing_piperplot, leading_zero, extra_spaces, underlines, other) %>%
  as.data.frame() %>%
  write_csv("piper_plot_titles_inconsistencies.csv")


ocr_data("figures/piperplots/Piperplot_0001_OW0451.jpg", engine = tesseract("eng"))
