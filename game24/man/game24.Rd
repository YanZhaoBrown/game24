\name{game24}
\alias{game24}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{game24}
%%  ~~function to do ... ~~

\description{
game24 gives different solutions to get 24 or other integers by using four basic operation for the four integers user entered
}
\usage{
game24(A=c(w,x,y,z),b=24,Sign=c("+","-","*","/"))
game24(A=c(w,x,y,z))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{a numeric vector that has length of 4
%%     ~~Describe \code{x} here~~
}
\item{b}{a numeric value that people want to use the 4 integers to get. The default is set to 24.
%%     ~~Describe \code{x} here~~
}
\item{Sign}{a character vector that users should enter "+","-","*","/".
%%     ~~Describe \code{x} here~~
}

}


\details{Put non-integers or length of the vector not equal to four into A will return error.
b does not have to be integer, but non-integer will always give you no solution since A are all integers.
Repeated signs does not matter. Other strings except the operation sign will be ignored.

}


\examples{

game24(c(1,5,11,12))
"Method  1 : ( 11 - 1 ) = 10 then 10 * 12 = 120 then 120 / 5 = 24"
"Method  2 : ( 11 - 1 ) = 10 then 10 / 5 = 2 then 2 * 12 = 24"
"Method  3 : ( 11 - 1 ) = 10 then 5 / 10 = 0.5 then 12 / 0.5 = 24"


game24(c(1,2,11,12),b=36)
"Method  1 : ( 1 + 11 ) = 12 then 12 * 2 = 24 then ( 24 + 12 ) = 36"
"Method  2 : 2 * 12 = 24 then ( 24 + 11 ) = 35 then ( 35 + 1 ) = 36"
"Method  3 : 12 * 2 = 24 then ( 24 + 1 ) = 25 then ( 25 + 11 ) = 36"


game24(c(1,5,11,12),Sign=c("-","/"))
"Method  1 : ( 11 - 1 ) = 10 then 5 / 10 = 0.5 then 12 / 0.5 = 24"
}
