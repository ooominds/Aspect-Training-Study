
# Corrects extreme rates (cf., Macmillan & Kaplan, 1985)
correct_extremes = function(m, csum) {
    zeros = which(m == 0, arr.ind=TRUE)
    if (nrow(zeros) > 0) {
        for (i in 1:nrow(zeros)) {
            m[zeros[i,1],zeros[i,2]] = m[zeros[i,1],zeros[i,2]] + 0.5
        }
    }
    max = which(m == csum, arr.ind=TRUE)
    if (nrow(max) > 0) {
        for (i in 1:nrow(max)) {
            m[max[i,1],max[i,2]] = m[max[i,1],max[i,2]] - 0.5
        }
    }
    return(m)
}

