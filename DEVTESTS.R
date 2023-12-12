library(targets)

# Testing single factsheet runs
tar_load(c(fs_aquifers, fs_ow, figs_p1, figs_p2, figs_p3))
tar_load_globals()

a <- 15


aq <- filter(fs_aquifers, aquifer_id == a)
ow <- filter(fs_ow, aquifer_id == a)
figs_p1 <- filter(figs_p1, aquifer_id == a)
figs_p2 <- filter(figs_p2, aquifer_id == a)
figs_p3 <- filter(figs_p3, aquifer_id == a)


factsheet(aq, ow, figs_p1, figs_p2, figs_p3, pages = 3, draft = FALSE,
          data_folder = NULL, out_folder = f("factsheets"),
          template_path = NULL,
          keep_tex = FALSE)


figs_p3 |>
  mutate(fill = NA_real_,
         fill = replace_na(fill, 0.5),
         fill = fill - 0.01,
         dim = map(loc, \(x) magick::image_info(magick::image_read(x))[c("width", "height")])) |>
  unnest(dim) |>
  mutate(text_position = if_else(width > height,
                                 paste0("height = ", fill, "\\paperheight"),
                                 paste0("width = ", fill, "\\paperwidth")))
