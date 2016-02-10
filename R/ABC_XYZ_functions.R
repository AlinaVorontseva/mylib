#'  Function to perform ABC-analysis
#'
#' @param mydata Data.frame with items sales
#' @param ncol_what Number of column where analysis should be performed
#' @param ncol_by number of column, that data should be aggregated by
#' @param a,b Shares coefficients of A and B groups
#'
#' @return list with sum value for \code{ncol_what} by values from \code{ncol_by}, divided to A, B, C groups
#' @export
ABC <- function(mydata, ncol_what, ncol_by, a = 0.6, b = 0.2){
    ITEM <- data.frame(aggregate(as.numeric(as.character(mydata[, ncol_what])),
                                by = list(as.character(mydata[, ncol_by])), FUN = sum, na.rm = T))
    ITEM <- ITEM[order(ITEM$x, decreasing = TRUE), ]
    Total_Quantity <- sum(as.numeric(as.character(ITEM$x)), na.rm = TRUE)

    Percent <- cumsum(ITEM$x / Total_Quantity)
    ITEM$ABC <- ifelse(Percent > a + b, "C", ifelse(Percent < a, "A", "B"))

    groupA <- ITEM[ITEM$ABC == "A", 1:2]
    groupB <- ITEM[ITEM$ABC == "B", 1:2]
    groupC <- ITEM[ITEM$ABC == "C", 1:2]

    colnames(groupA)[1] <- "ITEM_ID"
    colnames(groupA)[2] <- "TOTAL_REVENUE"
    colnames(groupB)[1] <- "ITEM_ID"
    colnames(groupB)[2] <- "TOTAL_REVENUE"
    colnames(groupC)[1] <- "ITEM_ID"
    colnames(groupC)[2] <- "TOTAL_REVENUE"

    return(list(A = groupA, B = groupB, C = groupC))
}


#' Function to perform XYZ-analysis
#'
#' @param mydata Data.frame with items sales
#' @param ncol_what Number of column where analysis should be performed
#' @param ncol_by number of column, that data should be aggregated by
#' @param x,y Shares coefficients of X and Y groups
#'
#' @return list with sd value for \code{ncol_what} by values from \code{ncol_by}, divided to X, Y, Z groups
#' @export
XYZ <- function(mydata, ncol_what, ncol_by, x = 0.6, y = 0.2){
    ITEM<- data.frame(aggregate(as.numeric(as.character(mydata[, ncol_what])),
                                by = list(as.character(mydata[, ncol_by])), FUN = sd, na.rm=T))
    ITEM <- ITEM[order(ITEM$x, decreasing = FALSE), ]
    Total_Quantity <- sum(as.numeric(as.character(ITEM$x)), na.rm = TRUE)

    Percent <- cumsum(ITEM$x / Total_Quantity)
    ITEM$ABC <- ifelse(Percent > x + y, "Z", ifelse(Percent < x, "X", "Y"))

    groupX <- ITEM[ITEM$ABC == "X", 1:2]
    groupY <- ITEM[ITEM$ABC == "Y", 1:2]
    groupZ <- ITEM[ITEM$ABC == "Z", 1:2]

    colnames(groupX)[1] <- "ITEM_ID"
    colnames(groupX)[2] <- "SD"
    colnames(groupY)[1] <- "ITEM_ID"
    colnames(groupY)[2] <- "SD"
    colnames(groupZ)[1] <- "ITEM_ID"
    colnames(groupZ)[2] <- "SD"

    return(list(X=groupX,Y=groupY,Z=groupZ))
}
