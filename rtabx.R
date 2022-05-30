extract_column <- function(pdf, 
                           pages = 1, 
                           row_split_at = "\\r\\n", 
                           col_split_at = "\\s++",
                           same_cols = FALSE,
                           grp_names = NULL,
                           horizontal = FALSE,
                           hg_split_at = "(?:^|\\r\\n)(?= ?[a-zA-Z])",
                           hr_split_at = " ?(,|:) ?",
                           tabulizer = FALSE
) {
  # 
  require(tabulizer)
  require(tidyverse)
  if (tabulizer) {
    extract_areas(file = pdf, pages = pages, output='data.frame')
  } else {
    # 1 . Locate areas of interest
    # 
    area <- locate_areas(file = pdf, pages = pages)
      if (!horizontal) {
      # 2. Define function to extract columns
      
      tdf <- function (Apk) {
        A <- Apk[1]; pk <- Apk[[2]]
        raw_string <- extract_text(file = pdf, pages = pk, area = A, encoding = "UTF-8")
        row_string <- str_subset(unlist(str_split(raw_string, row_split_at)), "^$", negate = T)
        as.data.frame(str_split(row_string, col_split_at, simplify = T))
      }
      # 
      # 3. Apply function to the specified areas
      # 
      tdf0 <- lapply(map2(area,pages, ~list(.x,.y)), tdf)
      # 
      # 4. Decide the groupings and naming
      # 
      err_string <- "length grp_names do not match the number of tables extracted" 
      # 
      if (same_cols) {
        tdf1 <- bind_rows(tdf0,.id = "id")
        if (!is.null(grp_names)) {
          if (length(grp_names)!=length(tdf0)) stop(err_string)
          mutate(.data = tdf1,
                 id = factor(id,
                             levels = unique(id),
                             labels = grp_names))
        } else {
          tdf1
        }
      } else {
        if (!is.null(grp_names)) {
          if (length(grp_names)!=length(tdf0)) stop(err_string)
          setNames(tdf0, grp_names)
        } else {
          tdf0
        }
      }
    } else {
      # horiztonal splitting to columns
      if (length(pages)!=1) stop("The `horizantal` option requires length(page)==1.")
      extract_text(file = pdf, pages = 1, area = area, encoding = "UTF-8") %>% 
        str_split(hg_split_at) %>% 
        unlist %>% 
        str_subset("^$", negate = T) %>% 
        str_replace_all("\\r\\n","") %>% 
        str_split(hr_split_at)  %>%  
        map(~{
          if (length(.x) != 1) {
            `colnames<-`(data.frame(.x[-1]),.x[1])
          } else {
            data.frame(grp = .x)
          } %>% 
            mutate(across(everything(),~str_trim(.x)))
        })
    }
  }
}


# extract_column <- function (pdf,
#                             pages = NULL,
#                             area = NULL,
#                             resolution = 300,
#                             plus_symbol = "\001",
#                             plus_replace = "+",
#                             row_split_at = "\\r\\n",
#                             col_split_at = "\\s",
#                             col_names = NULL,
#                             horizontal = FALSE,
#                             first_element = FALSE,
#                             tabulizer = FALSE, 
#                             multi_row = FALSE,
#                             multi_group_names = NULL) {
#   npages <- tabulizer::get_n_pages(pdf)
#   if (is.null(pages) & npages>1) stop("ERROR: Multi-page detected. Please indicate the page number to check.")
#   
#   if (!multi_row) {
#     if (!tabulizer) {
#       raw_text <- tabulizer::extract_text(file = pdf, pages = pages,
#                                           area = tabulizer::locate_areas(file = pdf, pages = pages, resolution = resolution))
#       col_vec <- unlist(stringr::str_split(raw_text, pattern = row_split_at))
#       if (!horizontal) {
#         if (!is.null(plus_symbol)) {
#           col_part <- stringr::str_replace(col_vec, pattern = plus_symbol, replacement = plus_replace)
#         } else {
#           col_part <- col_vec
#         }
#         col_fin <- stringr::str_subset(col_part, pattern = "^$", negate = T)
#         `row.names<-`(dplyr::bind_rows(purrr::map(stringr::str_split(col_fin, 
#                                                                      pattern = ifelse(!is.null(col_split_at),col_split_at,"^$")),
#                                                   ~{
#                                                     as.data.frame(rbind(stringr::str_trim(.x)), row.names = NULL)
#                                                   })),NULL)
#       } else {
#         if (!is.null(plus_symbol)) {
#           col_part <- stringr::str_replace(col_vec, pattern = plus_symbol, replacement = plus_replace)
#         } else {
#           col_part <- col_vec
#         }
#         if (!first_element) {
#           as.data.frame(stringr::str_trim(col_part), row.names = NULL)
#         } else {
#           `colnames<-`(as.data.frame(stringr::str_trim(col_part[-1])), col_part[1])
#         }
#       }
#     } else {
#       tabulizer::extract_areas(file = pdf, pages = pages, output='data.frame')
#     }
#   } else {
#     if (is.null(pages)) {
#       stop("Please provide the pages or number of groups to extract")
#     } else {
#       
#       areas_to_check <- tabulizer::locate_areas(file = pdf, pages = pages)
#       
#       if (is.null(multi_group_names)) {
#         
#       } else {
#         
#       }
#     }
#   }
# }
# 
# # pdftools approach to table extraction
# 
# ## pdftools::pdf_subset("pdfs/another-multi-page.pdf", pages = 5, output = "pdfs/page34.pdf")
# # pdftools::pdf_text("pdfs/6mp.pdf") %>% "["(16) %>% stringi::stri_enc_toascii(.) %>% str_split("\n") %>% "[["(1) %>% 
# #   "["(10:56) %>% "["(-c(8:13)) %>%
# #   {
# #     xx <- .
# #     els <- str_length(xx)
# #     nmax <- els[els==max(els)]
# #     cbind(xx,strrep(" ",nmax-els)) %>% 
# #       apply(1, str_c, collapse = "")
# #   } -> my_text
# # 
# # 
# # my_text %>% 
# #   str_locate_all("\\s+") -> hehe
# # 
# # hehe %>% 
# #   map(~{
# #     .x %>% "["({.[,1]-.[,2]}!=0,)
# #   }) %>% 
# #   do.call(rbind,.) %>% 
# #   IntervalSurgeon::depth(., include_intervals = T) %>% 
# #   {
# #     xx <- .
# #     xx$intervals[xx$depth==length(hehe),]
# #   } -> cols_pos
# # 
# # my_text
# # 
# # sort(cols_pos) %>% "["(-1) %>% c(.,-1) %>% 
# #   matrix(byrow = T, nrow = nrow(cols_pos)) %>% 
# #   apply(1,function(x) {
# #     str_sub(my_text,  start = x[1], end = x[2])
# #   })
# 
