\name{game_other}
\alias{game_other}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
game_other
}
\description{
game_other allows users to use the same cards to make the combinations of calculations of other possible number if the cards cannot make the combinations of 24.
}
\usage{
game_other(A, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{A is a numerical vector including 4 integers that user want to play with.
%%     ~~Describe \code{A} here~~
}
  \item{b}{b is a vector of other integers that user want to take them as the calculation targets. The calculation will stop until they find the first target in the vector that could be the result for combination of calculations.
%%     ~~Describe \code{b} here~~
}
}
\details{
The result will be the combinations of calculations of one number. User should put b by sequence of importance.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
game_other(c(7,7,9,5), 25:30)
[1] "Method  1 : ( 7 + 7 ) = 14 then ( 14 - 9 ) = 5 then 5 * 5 = 25"
[1] "Method  2 : ( 5 - 7 ) = -2 then -2 * 9 = -18 then ( 7 - -18 ) = 25"
[1] "Method  3 : ( 7 - 5 ) = 2 then 2 * 9 = 18 then ( 18 + 7 ) = 25"
[1] "Method  4 : ( 7 - 9 ) = -2 then ( -2 + 7 ) = 5 then 5 * 5 = 25"
[1] "Method  5 : ( 9 - 7 ) = 2 then ( 7 - 2 ) = 5 then 5 * 5 = 25"
[1] "Method  1 : ( 7 + 7 ) = 14 then ( 14 - 9 ) = 5 then 5 * 5 = 25"
[1] "Method  2 : ( 5 - 7 ) = -2 then -2 * 9 = -18 then ( 7 - -18 ) = 25"
[1] "Method  3 : ( 7 - 5 ) = 2 then 2 * 9 = 18 then ( 18 + 7 ) = 25"
[1] "Method  4 : ( 7 - 9 ) = -2 then ( -2 + 7 ) = 5 then 5 * 5 = 25"
[1] "Method  5 : ( 9 - 7 ) = 2 then ( 7 - 2 ) = 5 then 5 * 5 = 25"
[1] TRUE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
