
library(tidyr)
FB <- read.csv("/home/davidho/MAgPIEG/magpieEU/magpie-EU/modules/70_livestock/fbask_jan16/input/f70_feed_baskets.cs3", comment="*")

colnames(FB) <- c("time","region","product","feed",colnames(FB)[5:10])
for(prod in c("livst_chick", "livst_egg", "livst_pig", "livst_rum", "livst_milk")){
        print(prod)
        FB <- tibble(FB)
        FB %>% dplyr::filter(product == prod) %>% dplyr::filter(region %in% c("DEU","EUN","EUW","EUS","EUC") )%>% 
                dplyr::filter(time == "y2000") %>% dplyr::arrange(desc(`constant`) ) %>% dplyr::group_by(region) %>%
                dplyr::summarise(constant = sum(`constant`)) %>% dplyr::arrange(constant) %>% print()
        print("\n")
}

