\name{game_check}
\alias{game_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
game_check
}
\description{
For a given number series, this function can help you check whether you can use this number series with the basic operations(dividing,plusing,suntracting and multiplying)to generate a specific number.
}
\usage{
game_check(A,b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
a vector of integer(s) with the length of at least 1
}
\item{b}{
the specific number you want to generate with the A

}
}



\author{

}


\examples{

 game_check(c(7,888,42,99),4324)
 [1] "We cant get 4324 with the numbers you provide"
 game_check(c(8,88,888,288,48),56)
 [1] "This is at least one way to get 56"
 game_check(20,16)
 [1] "No one could do this!"
 game_check(20,20)
 [1] "20 is just what you want"
}
