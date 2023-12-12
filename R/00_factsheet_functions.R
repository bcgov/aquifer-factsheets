factsheet <- function(aq, ow, figs_p1, figs_p2, figs_p3, pages = 3, draft = FALSE,
                      data_folder = NULL, out_folder = f("factsheets"),
                      template_path = NULL,
                      keep_tex = FALSE) {

  # Checks
  if(is.null(data_folder)) data_folder <- getwd()
  if(is.null(template_path)) template_path <- f("template", f = "factsheet_template.Rmd")
  if(tolower(tools::file_ext(template_path)) != "rmd") stop("template_path must point to an .Rmd file")

  # File name
  out_file <- paste0("AQ_", sprintf("%05d", aq$aquifer_id), "_Aquifer_Factsheet_",
                     Sys.Date()) # Get aquifer_id with leading zeros
  if(draft) out_file <- paste0(out_file, "_DRAFT.pdf") else out_file <- paste0(out_file, ".pdf")

  # Page 1
  table <- fs_aq_table(aq)        # Prep Table
  aq <- fs_aq_check_map_link(aq)  # Check mapping link
  # TODO: Write to log if broken link?

  # Page 2
  figs_p2 <- dplyr::semi_join(figs_p2, tidyr::drop_na(ow, piper_text),
                              by = c("aquifer_id", "ow")) # Don't publish piper plots without a blurb

  # Create factsheet
  rmarkdown::render(template_path,
                    params = list(aq = aq,
                                  ow = ow,
                                  figs_p1 = figs_p1,
                                  figs_p2 = figs_p2,
                                  figs_p3 = figs_p3,
                                  table = table,
                                  pages = pages,
                                  draft = draft),
                    output_options = list(keep_tex = keep_tex),
                    output_file = out_file,
                    output_dir = out_folder, clean = TRUE, quiet = TRUE)
}


# Page 1 Aquifer description table
fs_aq_table <- function(aq) {

  # Transform for factsheet
  t <- aq |>
    dplyr::select(-"aquifer_id", -"title", -"subtitle", -"desc", -"map_report_link") |>
    dplyr::mutate(dplyr::across(dplyr::contains("min-max"), \(x) tidyr::replace_na(x, "no data available"))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(x) as.character(tidyr::replace_na(x, "Unknown")))) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Name", values_to = "Data")

  # Here we create the table in latex code using the kable function.
  # This is added to page 1 as the 'table' in a tikzpicture

  # Remove the headings
  t <- as.matrix(t)
  colnames(t) <- NULL

  # Create the table and edit
  t <- kableExtra::kable(t, format = "latex", escape = FALSE, booktabs = TRUE, linesep = "") %>%
    kableExtra::kable_styling(full_width = FALSE, latex_options = "striped",
                  position = "left", font_size = 9, stripe_color = "tablegrey") %>%
    kableExtra::add_header_above(c("Aquifer Details" = 2), bold = TRUE) %>%
    kableExtra::column_spec(1, width = "3.4cm") %>%
    kableExtra::column_spec(2, width = "4.2cm") %>%
    kableExtra::add_footnote(label = "* min - max", escape = FALSE, notation = "none") %>%
    kableExtra::add_footnote(label = "For Hydraulic Connection see \\link{https://a100.gov.bc.ca/pub/acat/documents/r50832/HydraulicConnectMW3_1474311684426_4310694949.pdf}{guidance document}", escape = FALSE, notation = "none")

  stringr::str_replace(t, "max\\}\\\\\\\\", "max\\}\\\\\\\\\\[-0.3em\\]")
}

# Get details for Page 1
# NOTE: We don't check for the existance of the map report here, because would
#   need to be re-checked if any details changed, instead we check as needed
#   in factsheet()
fs_aq_details <- function(aquifers, obs_wells) {

  ow <- obs_wells |>
    dplyr::arrange(ow_status, ow) |>
    dplyr::mutate(
      ow_ch = sprintf("%04d", ow),
      format = dplyr::case_when(ow_status == "Active" ~ paste0("\\textbf{", ow, "}"),
                                TRUE ~ as.character(ow))) |>
    # Only get the first twenty observation wells for each aquifer
    dplyr::group_by(aquifer_id) |>
    dplyr::mutate(n = dplyr::n(), n_active = sum(ow_status == "Active")) |>
    dplyr::slice(1:20) |>
    dplyr::mutate(format = dplyr::if_else(n < 20, format, "...")) |>
    dplyr::summarize(ow = paste0(format, collapse = ", "), .groups = "drop") |>
    dplyr::select(aquifer_id, ow)

  aq <- aquifers |>
    dplyr::left_join(ow, by = "aquifer_id") |>
    dplyr::mutate(

      # Description (with Mapping Report and subtype)
      map_report_link = paste0(
        "https://s3.ca-central-1.amazonaws.com/aquifer-docs/",     # Main site
        sprintf("%05d", round_any(aquifer_id, 100, f = floor)),    # Folder round to lower 100
        "/AQ_",                                                    # Prefix
        sprintf("%05d", aquifer_id),                               # Aq number, 5 digits
        "_Aquifer_Mapping_Report.pdf"),                            # Suffix
      desc = paste0("\\link{", map_report_link, "}",
                          "{\\textbf{Mapping Report - ", mapping_year, "}}"),
      desc = paste0("\\textbf{Aquifer Description} (", desc, "):\\\\",
                    str_replace(description, ".$", ""), # Replace the trailing period with nothing
                    " (subtype = ", aquifer_subtype_code, ")."),

      # Title
      title = paste0("\\titlelink{https://apps.nrs.gov.bc.ca/gwells/aquifers/",
                     aquifer_id, "}{Aquifer \\#", aquifer_id, "}"),
      # Subtitle
      subtitle = aquifer_name,
      subtitle = dplyr::if_else(
        subtitle == "" | subtitle == sprintf("%04d", aquifer_id) |
          subtitle == aquifer_id | is.na(subtitle),
        "", subtitle),

      # AQ Link - Link to factsheet pdf
      # TODO: Check, should this be used somewhere?
      aq_link = paste0("https://s3.ca-central-1.amazonaws.com/aquifer-docs/",
                       sprintf("%05d", round_any(aquifer_id, 100, f = floor)),
                       "/AQ\\_",
                       sprintf("%05d", aquifer_id),
                       "\\_Aquifer\\_Factsheet.pdf")
    )

  # Add details for tables

  aq_tbl <- aq |>
    # Escape & symbols
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character),
      ~stringr::str_replace_all(., c("\\&" = "\\\\&", "\\#" = "\\\\#")))) |>

    dplyr::mutate(
      ow = tidyr::replace_na(ow, "None"),
      size_km2 = dplyr::if_else(size_km2 != "Unknown",
                                paste(size_km2, "km\\textsuperscript{2}"),
                                as.character(size_km2)),
      conductivity = purrr::pmap_chr(
        list(conductivity_min, conductivity_max, conductivity_n),
        \(x, y, z) fs_fmt_props(x, y, z, units = "m/s")),
      transmissivity = purrr::pmap_chr(
        list(transmissivity_min, transmissivity_max, transmissivity_n),
        \(x, y, z) fs_fmt_props(x, y, z, units = "m\\textsuperscript{2}/s")),
      storativity = purrr::pmap_chr(
        list(storativity_min, storativity_max, storativity_n),
        \(x, y, z) fs_fmt_props(x, y, z, units = "")),

      n_licences = replace(n_licences, n_licences == 0, "Unknown")#,
      #hydraulic_connectivity = "\\link{https://a100.gov.bc.ca/pub/acat/documents/r50832/HydraulicConnectMW3_1474311684426_4310694949.pdf}{see guidance document}"
    ) |>

    # Note that `~` (below) means a non-breaking space, which forces the tables
    # to use a different spot to wrap the text
    dplyr::select(
      "aquifer_id",
      "Region" = "region",
      "Water District" = "water_district",
      "Aquifer Area" = "size_km2",
      #"No. Wells Correlated to Aquifer" = reported_no_wells,
      "No. Wells Correlated" = "reported_no_wells",
      "Vulnerability to Contamination" = "vulnerability",
      "Productivity" = "productivity",
      "Aquifer Classification" = "aquifer_classification",
      #"Hydraulic~Connection" = "hydraulic_connectivity",
      "Hydraulic~Conductivity *" = "conductivity",
      "Transmissivity *" = "transmissivity",
      "Storativity *" = "storativity",
      #"Aquifer Stress Index" = "aquifer_pumping_stress_index",
      "No. Water Licences Issued~to~Wells" = "n_licences",
      "Observation Wells \\footnotesize{(\\textbf{Active},~Inactive)}" = "ow") |>

    # Add hyperlinks
    dplyr::rename(
      "\\link{http://www.bclaws.ca/civix/document/id/complete/statreg/38_2016}{Water District}" =
        "Water District",
      "\\link{http://www.env.gov.bc.ca/wsd/plan_protect_sustain/groundwater/aquifers/reports/aquifer_maps.pdf\\#page=16}{Vulnerability to Contamination}" =
        "Vulnerability to Contamination",
      "\\link{http://www.env.gov.bc.ca/wsd/plan_protect_sustain/groundwater/aquifers/reports/aquifer_maps.pdf\\#page=19}{Productivity}" =
        "Productivity",
      "\\link{http://www.env.gov.bc.ca/wsd/plan_protect_sustain/groundwater/aquifers/reports/aquifer_maps.pdf\\#page=15}{Aquifer Classification}" =
        "Aquifer Classification")

  dplyr::select(aq, "aquifer_id", "title", "subtitle", "desc", "map_report_link") |>
    dplyr::left_join(aq_tbl, by = "aquifer_id")
}

fs_ow_details <- function(ow) {

  # Get Piper text for each obs well and add link
  piper_text <- readxl::read_excel(f("in_data", f = "piper_text.xlsx"), sheet = 1) |>
    rename_all(tolower) |>
    select("ow" = "obs_well", "piper_text" = "hydrogeochemistry", "ems_id") |>
    filter(piper_text != "Do not publish") |> # Omit bad plots
    mutate(
      piper_text = paste0(
        piper_text,
        " \\link{https://a100.gov.bc.ca/pub/ems/mainmenu.do?",
        "userAction=monitoringLocationsCriteria&bean.p_mon_locn_id=", ems_id,
        "}{For EMS water chemistry data, see EMS ID ", ems_id, "}."),
      piper_text = str_replace_all(piper_text, c("\\&" = "\\\\&", "\\#" = "\\\\#"))) |>
    complete(ow = .env$ow$ow, fill = list(piper_text = "No summary at this point")) |>
    select(-"ems_id")

  ow |>
    filter(ow_status == "Active") |>
    left_join(piper_text, by = "ow") |>
    mutate(
      map_link = paste0("https://governmentofbc.maps.arcgis.com/apps/webappviewer/",
                        "index.html?id=b53cb0bf3f6848e79d66ffd09b74f00d&find=OBS\\%20WELL\\%20",
                        sprintf("%03d", ow)),
      well_record = paste0("https://apps.nrs.gov.bc.ca/gwells/well/", well_tag_number))
}


fs_draft_tag <- function() {
  cat("\\begin{tikzpicture}",
      "\\node[at = (current page.center), opacity = 0.2, font=\\fontsize{240}{164}\\selectfont, rotate = 45] {DRAFT};",
      "\\end{tikzpicture}"
  )
}

fs_fmt_props <- function(x_min, x_max, x_n, units) {

  fmt <- function(x) {
    x <- format(x, digits = 2, scientific = TRUE)
    stringr::str_replace(x, "e-0*([0-9]{1,2})", "x10\\\\textsuperscript{-\\1}")
  }

  if(is.na(x_min)) {
    x <- NA_character_
  } else {
    if(x_n == 1) {
      x <- fmt(x_min)
    } else {
      x <- paste0(fmt(x_min), " - ", fmt(x_max))
    }
    x <- paste0(x, if_else(units == "", "", paste0(" ", units)),
                " (n=", x_n, ")")
  }
  x
}

fs_aq_check_map_link <- function(aq) {
  if(httr::http_error(httr::GET(aq$map_report_link))) {
    aq$desc <- stringr::str_replace(aq$desc, "\\\\link\\{.*\\}", "\\\\link\\{]]}")
  }
  aq
}

fs_figs_p1 <- function(aq_ids, ...) {
  tibble(files = unlist(list(...))) |>
    mutate(type = stringr::str_extract(files, "water_depth|well_depth|well_yield"),
           aquifer_id = as.numeric(stringr::str_extract(files, "\\d{4}"))) |>
    tidyr::drop_na() |>
    tidyr::pivot_wider(names_from = type, values_from = files) |>
    mutate(across(
      -"aquifer_id",
      \(x) if_else(is.na(x), f("boxplots", f = paste0(cur_column(), "_NA.jpg")), x))) |>
    complete(aquifer_id = aq_ids) |>
    filter(aquifer_id %in% aq_ids) |>
    mutate(maps = f("maps", f = paste0("Aquifer_Map_", sprintf("%04d", aquifer_id), ".pdf")))
}

fs_figs_p2 <- function(aq_ids, ...) {
  tibble(files = unlist(list(...))) |>
    mutate(type = stringr::str_extract(files, "gwl_ppt|gwl_trends|piperplots"),
           aquifer_id = as.numeric(stringr::str_extract(files, "\\d{4}(?=_)")),
           ow = as.numeric(stringr::str_extract(files, "\\d{4}(?=\\.)"))) |>
    tidyr::drop_na() |>
    tidyr::pivot_wider(names_from = type, values_from = files) |>
    mutate(across(
      -c("aquifer_id", "ow"),
      \(x) if_else(is.na(x), f("in_na", f = paste0("figure_missing_", cur_column(), ".png")), x))) |>
    complete(aquifer_id = aq_ids) |>
    filter(aquifer_id %in% aq_ids)
}

fs_figs_p3 <- function(aq_ids, images, index) {

  txt <- tibble(path = list.files(f("extra"), ".txt", full.names = TRUE)) |>
    mutate(name = str_extract(path, "[^/]+$"),
           name = str_remove(name, "\\.[[:alpha:]]+$"),
           text = map_chr(path, read_file),
           text = str_trim(text)) |>
    select("name", "text")

  # TODO: Calculate the dimensions for the iamge
  #  add option in files to makes smaller if need more space for the text
  #  clip extra white space

  # Extra page three figures by Aquifer
  read_csv(images, show_col_types = FALSE) |>
    left_join(read_excel(index), by = "type") |>
    arrange(order, image) |>
    distinct() |> #in case of duplicates
    mutate(name = str_remove(image, "\\.[[:alpha:]]+$")) |>
    left_join(txt, by = "name") |>
    mutate(loc = f("in_extra", f = image)) |>
    filter(aquifer_id %in% aq_ids,
           fs::file_exists(loc)) |> # TODO: alert if in index but no file?
    mutate(fill = replace_na(fill, 0.5),
           fill = fill - 0.01,
           fill_text = 0.93 - fill,
           dim = map(loc, \(x) magick::image_info(magick::image_read(x))[c("width", "height")])) |>
    unnest(dim) |>
    mutate(image_position = if_else(width > height,
                                    paste0("height = ", fill, "\\paperheight"),
                                    paste0("width = ", fill, "\\paperwidth")),
           text_position = if_else(width > height,
                                   paste0("height = ", fill_text, "\\paperheight"),
                                   paste0("width = ", fill_text, "\\paperwidth"))) |>
    complete(aquifer_id = aq_ids)
}


fig_extra <- function() {
  # Figures by aquifer by OW
  p2 <- ow_index |>
    select("aquifer_id", "ow") |>
    mutate(aq_num_ch = sprintf("%04d", aquifer_id),
           ow_ch = sprintf("%04d", ow)) |>
    expand_grid(type = c("gwl_ppt", "gwl_trends", "piperplot")) |>
    mutate(fig = paste0(type, "_", aq_num_ch, "_OW", ow_ch, ".png"),
           fig = map2_chr(type, fig,  \(x, y) f(x, f = y)),
           exists = file.exists(fig),
           # Don't publish piper plots without a blurb
           exists = if_else(type == "piper" & !as.numeric(.data$ow) %in% .env$piper_text$obs_well,
                            FALSE, exists),
           fig = replace(fig, !exists, f("in_na", f = paste0("figure_missing_", type[!exists], ".png")))) |>
    mutate(missing = sum(!exists), .by = "ow") |>
    pivot_wider(names_from = type, values_from = fig) |>
    select(-"aq_num_ch", -"ow_ch")


  # Extra page three figures by Aquifer
  p3 <- read_csv(f("in_extra", f = "extra_page_images.csv"), show_col_types = FALSE) |>
    left_join(read_excel(f("in_extra", f = "extra_page_index.xlsx")), by = "type") |>
    arrange(order, image) |>
    distinct() |> #in case of duplicates
    mutate(loc = f("in_extra", f = image))

  left_join(p1, p2, by = "aquifer_id") |>
    left_join(p3, by = "aquifer_id")
}


