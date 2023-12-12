# # Load functions, packages and data
# source("00_functions.R")
# source("00_header.R")
# load("tmp/aquifer_factsheet_data.RData")
#
# wd <- st_intersection(select(bcmaps::water_districts(), district = DISTRICT_NAME),
#                       select(aquifer_map, aquifer_id = AQ_TAG)) %>%
#   # Calculate area of overlap
#   mutate(area = as.numeric(st_area(.))) %>%
#   # Calculate dominant water district (max area)
#   group_by(aquifer_id) %>%
#   mutate(parea = area / sum(area) * 100,
#          n = n())
#
# wd_final <- wd %>%
#   filter(parea == max(parea)) %>%
#   select(-area) %>%
#   # Remove spatialness
#   st_set_geometry(NULL) %>%
#   ungroup() %>%
#   mutate(aquifer_id = as.numeric(aquifer_id))
#
#
# p <- full_join(wd_final, rename_all(water_district, tolower), by = "aquifer_id") %>%
#   filter(district != water_district |  #where current doesn't match original
#            (is.na(district) & !is.na(water_district))) %>% #where current is missing, but original not
#   as.data.frame()
#
#
#
# # Missing districts -------------------------------------------------------
# # Are any of the missing district aquifers actually in the aquifers map file? NO!
# any(p$aquifer_id[is.na(p$district)] %in% as.numeric(aquifer_map$AQ_TAG))
#
# # Because either a) MERGED, b) RETIRED, or c) "469"
# filter(aquifer_db_raw, aquifer_id %in% p$aquifer_id[is.na(p$district)]) %>%
#   select(aquifer_id, aquifer_name, location_description)
#
# filter(aquifer_db_raw,
#        aquifer_id %in% p$aquifer_id[is.na(p$district)],
#        aquifer_id %in% aquifers) %>%
#   select(aquifer_id, aquifer_name, location_description)
#
#
# # Different districts -----------------------------------------------------
# filter(p, !is.na(district) | as.numeric(aquifer_id == 469)
# filter(p, aquifer_id %in% as.numeric(aquifer_map$AQ_TAG))
#
# filter(wd, as.numeric(aquifer_id) %in% p$aquifer_id) %>%
#   st_set_geometry(NULL) %>%
#   mutate(district_new = district[parea == max(parea)]) %>%
#   ungroup() %>%
#   mutate(aquifer_id = as.numeric(aquifer_id)) %>%
#   select(-n) %>%
#   left_join(p) %>%
#   group_by(aquifer_id) %>%
#   mutate(district_orig = water_district[!is.na(water_district)]) %>%
#   select(aquifer_id,
#          district_options = district,
#          district_new,
#          district_orig,
#          area, parea) %>%
#   bind_rows(cbind(aquifer_id = 469, district_options = NA, district_new = NA, district_orig
#   write_csv("out/LOG_DISTRICTS.csv")
#
