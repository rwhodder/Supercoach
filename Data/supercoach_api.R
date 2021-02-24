install.packages("jsonlite")
library(jsonlite)
library(tidyverse)
library(plyr)

supercoach_2021 <- read_json("https://supercoach.heraldsun.com.au/2021/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2020 <- read_json("https://supercoach.heraldsun.com.au/2020/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2019 <- read_json("https://supercoach.heraldsun.com.au/2019/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2018 <- read_json("https://supercoach.heraldsun.com.au/2018/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2017 <- read_json("https://supercoach.heraldsun.com.au/2017/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2016 <- read_json("https://supercoach.heraldsun.com.au/2016/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2015 <- read_json("https://supercoach.heraldsun.com.au/2015/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2014 <- read_json("https://supercoach.heraldsun.com.au/2014/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2013 <- read_json("https://supercoach.heraldsun.com.au/2013/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2012 <- read_json("https://supercoach.heraldsun.com.au/2012/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")

supercoach_2011 <- read_json("https://supercoach.heraldsun.com.au/2011/api/afl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&fbclid=IwAR2qMsnobcxzOlLJ9ifol-T6eGtxu3ZKoTgrvHgeV5iZyKVGEY_FsK2O1L4")


data_2021 <- list <- unlist(supercoach_2021, recursive = FALSE)
df <- do.call("rbind", list)


data <- ldply(supercoach_2021,data.frame)
 ldp