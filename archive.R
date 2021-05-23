
# 2021-05-22 --------------------------------------------------------------

fp <- "data/SDOT-Speed-Studies/2019_01_03_10932_SGT.pdf"

tbl <- extract_tables(file = fp,
                      pages = 1, 
                      output = "data.frame")

# tbl2 <- extract_areas(fp, 1, output = "data.frame")

areas <- locate_areas(file = fp, pages = 1, widget = "shiny")






tbl2 <- extract_tables(file = fp, 
                       pages = 1, 
                       area = areas, 
                       output = "data.frame")


tbl3 <- tbl2[[1]] %>% 
  as_tibble() 

cleaned_colnames <- str_c(
  as.vector(slice(tbl3,1)),
  as.vector(slice(tbl3,2))) %>% 
  str_remove_all("\\..") %>% 
  str_remove_all("\\s") %>% 
  str_remove_all("-") %>% 
  str_replace_all("<","to")

tbl4 <- set_names(tbl3, cleaned_colnames)

tbl5 <- tbl4 %>% 
  slice(-1,-2)


tbl6 <- tbl5 %>% 
  separate(col = '2530to30to35',
           into = c("25t030","30to35"),
           sep = "\\s")

tbl6 %>% View()

