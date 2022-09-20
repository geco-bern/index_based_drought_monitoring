# RSVI

Analysing remotely sensed vegetation indices and their response to drought.

This repository contains R code. For a demo knit the RMarkdown file `vignette_rsvi.Rmd`.

## Site selection

Site selection was done based on:

1. Site where fLUE method worked and where clear soil moisture droughts were identified. I.e., site belongs to clusters cDD, cGR, or cLS as described in Stocker et al. (2018) *New Phytologist*.
2. Site was among the subset of homogenous sites, selected by Manuela Balzarolo. All sites with a homogenous surrounding are listed in file `data/FLUXNET-2015_Tier1/meta/fluxnet_quality_check_homogenous_OK_PRI_mbalzarolo.csv`. 

Reproduce it as follows:

Get sites where fLUE data is available
```r
read_csv("~/data/flue/flue_stocker18nphyt.csv")
df_flue_sites <- select(df_flue, site, cluster) %>% unique()
```

Add meta information for sites
```r
library(rsofun)
df_flue_sites <- df_flue_sites %>% rename(sitename=site) %>% left_join( rsofun::metainfo_Tier1_sites_kgclimate_fluxnet2015, by="sitename" )
```

Subset sites belonging to cluster cGR, cDD, or cLS
```r
df_sub <- df_flue_sites %>% filter(cluster %in% c("cGR", "cDD", "cLS"))
write_csv(df_sub, path = "~/data/flue/metainfo_sites_clusters_cGR_cDD_cLS_flue_fluxnet2015.csv")
```

Get list of sites with homogenous surrounding and subset fLUE sites base on that list
```r
df_homo <- read_csv("~/data/FLUXNET-2015_Tier1/meta/fluxnet_quality_check_homogenous_OK_PRI_mbalzarolo.csv")
df_sub_homo <- filter( df_sub, sitename %in% df_homo$sitename )
write_csv(df_sub_homo, path = "./data/sites.csv")
```