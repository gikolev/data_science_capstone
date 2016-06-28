predict <- function(x) {
        y <- strsplit(tolower(x), " ")[[1]]
        n <- length(y)
        if (n == 1) {
                q <- which(df_list[[2]]$V1 == y)
                print(df_list[[2]][min(q), "V2"])
        } else if (n == 2) {
                q <- which(df_list[[3]]$V1 == y[1]
                           & df_list[[3]]$V2 == y[2])
                if (length(q) == 0) {
                        q <- which(df_list[[2]]$V1 == y[2])
                        print(df_list[[2]][min(q), "V2"])
                } else {
                        print(df_list[[3]][min(q), "V3"])
                }
        } else if (n >= 3) {
                z <- tail(y, n = 3) 
                q <- which(df_list[[4]]$V1 == z[1] 
                           & df_list[[4]]$V2 == z[2] 
                           & df_list[[4]]$V3 == z[3])
                if (length(q) == 0) {
                        q <- which(df_list[[3]]$V1 == z[2]
                                   & df_list[[3]]$V2 == z[3])
                        if (is.null(q)) {
                                q <- which(df_list[[2]]$V1 == z[3])
                                print(df_list[[2]][min(q), "V2"])
                        } else {
                                print(df_list[[3]][min(q), "V3"])
                        }
                } else {
                        print(df_list[[4]][min(q), "V4"])
                }
        }
}