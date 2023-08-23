kelp0 = read.csv(file = "F:/滑铁卢大学/master/S23/STAT 844/project/data/kelp dynamics/kelp.csv", header = FALSE)
kelp0 = kelp0[, c(1, 2, 3)]

kelp1 = read.csv(file = "F:/滑铁卢大学/master/S23/STAT 844/project/data/kelp dynamics/kelp1.csv", header = FALSE)
kelp1 = kelp1[, c(1, 2, 3)]

kelp2 = read.csv(file = "F:/滑铁卢大学/master/S23/STAT 844/project/data/kelp dynamics/kelp2.csv", header = FALSE)
kelp2 = kelp2[, c(1, 2, 3)]

kelp3 = read.csv(file = "F:/滑铁卢大学/master/S23/STAT 844/project/data/kelp dynamics/kelp3.csv", header = FALSE)
kelp3 = kelp3[, c(1, 2, 3)]

kelp4 = read.csv(file = "F:/滑铁卢大学/master/S23/STAT 844/project/data/kelp dynamics/kelp4.csv", header = FALSE)
kelp4 = kelp4[, c(1, 2, 3)]

ss_inx <- c()

# select out the years that are between 2010 and 2021
for (i in 2: 159) {
  if ((2010 <= kelp0[i, 1]) && (kelp0[i, 1] <= 2022)) {
    ss_inx <- c(ss_inx, i)
  }
  
}

kelp0 <- kelp0[ss_inx, ]



ss_inx <- c()

# select out the years that are between 2010 and 2021
for (i in 2: 159) {
  if ((2010 <= kelp1[i, 1]) && (kelp1[i, 1] <= 2022)) {
    ss_inx <- c(ss_inx, i)
  }
  
}

kelp1 <- kelp1[ss_inx, ]




ss_inx <- c()

# select out the years that are between 2010 and 2021
for (i in 2: 159) {
  if ((2010 <= kelp2[i, 1]) && (kelp2[i, 1] <= 2022)) {
    ss_inx <- c(ss_inx, i)
  }
  
}

kelp2 <- kelp2[ss_inx, ]


ss_inx <- c()

# select out the years that are between 2010 and 2021
for (i in 2: 159) {
  if ((2010 <= kelp3[i, 1]) && (kelp3[i, 1] <= 2022)) {
    ss_inx <- c(ss_inx, i)
  }
  
}

kelp3 <- kelp3[ss_inx, ]




ss_inx <- c()

# select out the years that are between 2010 and 2021
for (i in 2: 159) {
  if ((2010 <= kelp4[i, 1]) && (kelp4[i, 1] <= 2022)) {
    ss_inx <- c(ss_inx, i)
  }
  
}

kelp4 <- kelp4[ss_inx, ]


# add up the kelp areas
kelp_df <- merge(kelp0, kelp1, by = c("V1", "V2"), all.x = FALSE)
kelp_df <- merge(kelp_df, kelp2, by = c("V1", "V2"), all.x = FALSE)
kelp_df <- merge(kelp_df, kelp3, by = c("V1", "V2"), all.x = FALSE)
kelp_df <- merge(kelp_df, kelp4, by = c("V1", "V2"), all.x = FALSE)
colnames(kelp_df) <- c("year", "season", "area1", "area2", "area3", "area4", "area5")

kelp_df_sum <- data.frame()
for (i in 1: 52) {
  
}

kelp_df$area1 <- as.numeric(kelp_df$area1)
kelp_df$area2 <- as.numeric(kelp_df$area2)
kelp_df$area3 <- as.numeric(kelp_df$area3)
kelp_df$area4 <- as.numeric(kelp_df$area4)
kelp_df$area5 <- as.numeric(kelp_df$area5)

kelp_df$area <- kelp_df$area1 + kelp_df$area2 + kelp_df$area3 + kelp_df$area4 + kelp_df$area5


write.csv(kelp_df, file = "F:/滑铁卢大学/master/S23/STAT 844/project/data/kelp dynamics/kelp_area.csv")

