library(targets)

# Testing single factsheet runs
tar_load(p1)
tar_load(figs_p1)
tar_load(figs_p2)
tar_load(figs_p3)
tar_load_globals()
tar_source()

a <- 162
p1 <- filter(p1, aquifer_id == a)
f_p1 <- filter(figs_p1, aquifer_id == a)
f_p2 <- filter(figs_p2, aquifer_id == a)
f_p3 <- filter(figs_p3, aquifer_id == a)

f_p3$p3[[1]]$p3_txt <-
  stringr::str_replace(f_p3$p3[[1]]$p3_txt, "\\{Budget", "")

f_p3$p3[[1]]$p3_txt <-
  stringr::str_remove(f_p3$p3[[1]]$p3_txt, "\\\\ul")

f_p3$p3[[1]]$p3_txt <-
  "Based on the water budget estimates for mapped aquifers within WR6 (NR),\nmoderately high to highly stressed aquifers appear to dominate this\nregion. Only aquifer 165 located in South Wellington exhibits a moderate\nstress level. The most stressed aquifers include the Upper Cassidy\nAquifer (161), the Cedar Yellow Point aquifer (162), and the small\nQuadra Aquifer 163 mapped near the Holden Cross Road and Haro Road. Many\nof the aquifers have moderate to higher density wells that likely\ncontribute to well to well interference, particularly in the lower\nproductivity bedrock aquifers with limited recharge.\n\n\\vspace{0.5cm}\n\\textbf{Reference: \\link{https://www.rdn.bc.ca/water-budget}{Waterline Resources Inc (June 2013): Water Budget Project: RDN phase one (Vancouver Island) 1924-11-001 Regional District of Nanaimo, British Columbia}}\n"
f_p3$p3[[1]]$p3_txt

factsheet(p1, f_p1, f_p2, f_p3, pages = 3, draft = FALSE,
          data_folder = NULL, out_folder = f["factsheets"],
          template_path = NULL,
          keep_tex = TRUE)


figs_p3 |>
  unnest(p3) |>
  drop_na(p3_image) |>
  filter(stringr::str_detect(p3_txt, "\\\\ul"))


# fmt_exra_page_index -----
library(targets)
tar_source()
tar_load_globals()
tar_load(extra_files)
t <- fmt_extra_page_index(extra_files)

filter(t, aquifer_id == 74) |>
  pull(txt) |>
  cat()




figs_p3 |> filter(aquifer_id == 1281) |> unnest(p3) |> pull(p3_txt)

figs_p3 |>
  mutate(fill = NA_real_,
         fill = replace_na(fill, 0.5),
         fill = fill - 0.01,
         dim = map(loc, \(x) magick::image_info(magick::image_read(x))[c("width", "height")])) |>
  unnest(dim) |>
  mutate(text_position = if_else(width > height,
                                 paste0("height = ", fill, "\\paperheight"),
                                 paste0("width = ", fill, "\\paperwidth")))



f <- list.files("Original Workflow/figures/extra/")
a <- readr::read_csv("Original Workflow/out/extra_page_images.csv")

dplyr::filter(a, setdiff(f, .data$image))


