# # Copyright 2020 Province of British Columbia
# #
# # Licensed under the Apache License, Version 2.0 (the "License"); you may not
# # use this file except in compliance with the License. You may obtain a copy of
# # the License at
# #
# # http://www.apache.org/licenses/LICENSE-2.0
# #
# # Unless required by applicable law or agreed to in writing, software
# # distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# # WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# # License for the specific language governing permissions and limitations under
# # the License.
#
#
# # Extra pages
# extra_index <- read_excel("./data/extra_page_index.xlsx")
# extra_images <- tibble(image = list.files("./figures/extra/"),
#                        type = NA_character_) %>%
#   mutate(
#     aquifer_ids = str_extract(tolower(image),
#                               "(?<=aquifer(s?) )[0-9]{1,4}((-| and )[0-9]{1,4})*"),
#     aquifer_ids = str_split(aquifer_ids, "-| and ")) %>%
#   unnest(aquifer_ids)
#
# for(i in seq_len(nrow(extra_index))) {
#   m <- str_replace_all(tolower(extra_index$match[i]), ", *", "|")
#   extra_images <- mutate(
#     extra_images,
#     type = if_else(is.na(type) & str_detect(tolower(image), m),
#                    extra_index$type[i],
#                    type))
# }
#
# if(any(is.na(extra_images$type))) stop("Problems assigning Supplementary pages to types (check figures/extra and extra_page_index.xlsx", call. = FALSE)
#
# write_csv(extra_images, "out/extra_page_images.csv")
