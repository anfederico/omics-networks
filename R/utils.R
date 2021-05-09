#' Inverted versions of in
#' 
#' @noRd
`%not_in%` <- Negate(`%in%`)

#' Removes null values from a vector
#' 
#' @noRd
drop_nulls <- function(x){
    x[!sapply(x, is.null)]
}

#' Remove NA values from a vector
#' 
#' @noRd
drop_nas <- function(x){
    x[!sapply(x, is.na)]
}

#' Replaces NA values in a vector
#' 
#' @noRd
replace_nas <- function(x, y){
    x[is.na(x)] <- y
    x
}

#' Normalize values between a given range
#' 
#' @param x Values to normalize
#' @param a Min of range
#' @param b max of range
#' 
#' @return Normalized values
#' 
#' @export
normalize.range <- function(x, a=0, b=1) {
    (b-a)*( (x-min(x)) / (max(x)-min(x)) )+a
}

#' Normalize significance values
#' 
#' @param x Values to normalize
#' @param base Base of logarithm
#' 
#' @return Normalized values
#' 
#' @return Normalized values
#' 
#' @export
normalize.nlog <- function(x, base=10) {
    -log(x, base=base)
}

#' Normalize significance values between a given range
#' 
#' @param x Values to normalize
#' @param base Base of logarithm
#' @param a Min of range
#' @param b max of range
#' 
#' @return Normalized values
#' 
#' @export
normalize.nlog.range <- function(x, base=10, a=0, b=1) {
    normalize.range(normalize.nlog(x, base=base), a=a, b=b)
}

#' Colorize numerical values
#' 
#' @param x Values to normalize
#' @param resolution Limit resolution for small values
#' @param pal Color palette (?viridis)
#' 
#' @return Colorized values
#' 
#' @import viridis
#' 
#' @export
colorize <- function(x, resolution=4, pal=viridis::plasma) {
    multiplier <- 100*resolution
    colors <- pal(multiplier+1)
    colors[round(normalize.range(x, 0, 1)*multiplier, 0)+1]
}

#' Large distinct color palettes
#' 
#' @param n Number of colors
#' 
#' @return Colors
#' 
#' @export
ncolors <- function(n) {
    colors <-
        c("#353E7C","#007094","#009B95","#00BE7D","#96D84B","#FDE333","#040404","#3E134F","#851170","#C53270",
          "#F36E35","#F8B83C","#A23D47","#A96C00","#9A9800","#75C165","#50E2BB","#B0F4FA","#26185F","#005D9E",
          "#18BDB0","#9ADCBB","#D7F4CF","#DD008F","#DB6AC0","#D5A6DB","#F6F6FC","#CBA079","#928261","#605F4C",
          "#363932","#001889","#87008D","#DAFF47","#88002D","#FE945C","#FFE2C0","#004533","#006F69","#0091AD",
          "#EDD788","#AB4A3D","#73243C","#AC0535","#EB2C31","#EF4868","#F56553","#404E9C","#3D7CB8","#4BA5BF",
          "#55C7B1","#A3E292","#FAEF8B","#0A1230","#3C2871","#7A3392","#B7509A","#E68375","#F1C687","#985277",
          "#A37B49","#9BA453","#86CBA0","#7DEBEA","#C0FCFC","#2C2C7D","#396ABC","#5DC6DB","#AAE6EA","#DEFDFD",
          "#CA40B4","#CF82E3","#D1B6F3","#F7FFFF","#C7AEAE","#928F93","#626C7B","#3A465F","#25309C","#7B31A9",
          "#DEFD99","#7D245B","#F3A698","#FDF0F3","#215061","#367A94","#4B9CD2","#EBE4C4","#A15D70","#6B3868",
          "#9F2D65","#DB4D6A","#DF6598","#E77C8B")
    return(rep(colors, floor(n/length(colors))+1)[1:n])
}
