lst_df <- read.csv("F:/滑铁卢大学/master/S23/STAT 844/project/data/LST.csv")

ss_inx <- c()
for (i in 1: 251) {
  if ((2010 <= lst_df$year[i]) && (lst_df$year[i] <= 2021)) {
    ss_inx <- c(ss_inx, i)
  }
}

lst_df <- lst_df[ss_inx, ]

lst_by_season <- data.frame()

for (i in 2010: 2021) {
  for (j in 1: 4) {
    lst_by_season <- rbind(lst_by_season, c(i, j, 0))
  }
}

colnames(lst_by_season) <- c("year", "season", "mean_LST")
rownames(lst_df) <- 1: 143

for (i in 1: 143) {
  for (j in 1: 48) {
    if ((lst_by_season$year[j] == lst_df$year[i]) && (floor((lst_df$month[i] - 1) / 3) + 1 == lst_by_season$season[j])) {
      lst_by_season$mean_LST[j] <- lst_by_season$mean_LST[j] + lst_df$LST[i]
    }
  }
}


write.csv(lst_by_season, "F:/滑铁卢大学/master/S23/STAT 844/project/data/LST_by_season.csv")

kelp_df <- kelp_df[1: 48, c("year", "season", "area")]

analysis_df <- merge(lst_by_season, kelp_df, by = c("year", "season"))




# do fft for kelp area
kelp_area_fft <- fft(kelp_df$area, inverse = FALSE)

# plot the spectrum
plot((2: 48), Mod(kelp_area_fft[2: 48]), main = "Kelp area on frequency domain",
     xlab = "k", ylab = "Frequency magnitude")

# filter out the low-coefficient frequencies
ss_inx <- c()
for (i in 1: 48) {
  if (Mod(kelp_area_fft[i]) > 4e+08) {
    ss_inx <- c(ss_inx, i)
  }
}
kelp_area_fft_filtered <- kelp_area_fft[ss_inx]

# plot the full fft sum
idft_full <- function(fft, x) {
  sigma <- 0
  n <- length(fft)
  for (i in 1: n) {
    sigma <- sigma + fft[i] * (cos(2 * pi * (i - 1) * (x - 1) / n) + sqrt(-1+0i) * sin(2 * pi * (i - 1) * (x - 1) / n))
  }
  sigma <- sigma / n
  return(sigma)
}

yy_full <- function(fft, xx) {
  n <- length(fft)
  m <- length(xx)
  yy <- c()
  for (i in 1: m) {
    yy <- c(yy, idft_full(fft, xx[i]))
  }
  return(yy)
}




xx <- seq(0, 52, by = 0.01)
plot(rownames(kelp_df), kelp_df$area)
lines(xx, yy_full(kelp_area_fft, xx), type = "l")


# plot the part idft
xx <- seq(0, 52, by = 0.01)
yy_incom <- yy_full(kelp_area_fft_filtered, xx)
plot(rownames(kelp_df), kelp_df$area)
lines(xx, yy_incom - yy_incom[1], type = "l")


# add cos(t*pi/2) and sin(t*pi/2) to the function basis
# create the model matrix
GAM_model_matrix_time <- function(x, knots) {
  # note that knots are internal breakpoints
  matrix <- splineDesign(knots, x, ord = 4)
  
  # add the trinometric functions
  trig_mat <- cbind(cos((pi / 2) * x), sin((pi / 2) * x))
  
  comb_matrix <- cbind(trig_mat, matrix)
  return(comb_matrix)
}

knots <- seq(0, 48, by = 4)
knots <- c(0, 0, 0, knots, 48, 48, 48)

time_model_matrix <- GAM_model_matrix_time(as.numeric(rownames(analysis_df)), knots)
# ↑ remember that as.numeric(rownames(analysis_df)) is the covariate time.

GAM_model_matrix_LST <- function(x, knots) {
  # note that knots are internal breakpoints
  matrix <- bs(x)

  return(matrix)
}


LST_model_matrix <- GAM_model_matrix_LST(analysis_df$mean_LST, knots = knots)

# used in 
model_matrix <- cbind(time_model_matrix, LST_model_matrix)

