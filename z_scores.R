#find z-scores for a single vector
data <- c(6, 7, 7, 12, 13, 13, 15, 16, 19, 22)

z_scores <- (data-mean(data))/sd(data)

z_scores


#find z-scores for a single column in a data frame
df <- data.frame(assists = c(4, 4, 6, 7, 9, 13),
                 points = c(24, 29, 13, 15, 19, 22),
                 rebounds = c(5, 5, 7, 8, 14, 15))

z_scores <- (df$points-mean(df$points))/sd(df$points)

z_scores


#find z-scores of each column in a data frame
df <- data.frame(assists = c(4, 4, 6, 7, 9, 13),
                 points = c(24, 29, 13, 15, 19, 22),
                 rebounds = c(5, 5, 7, 8, 14, 15))


sapply(df, function(df) (df-mean(df))/sd(df))