\name{game24prob}
\alias{game24prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{game24prob
%%  ~~function to do ... ~~
}
\description{
game24prob gives users the probability of combinations of cards that not being able to get 24. Users should select any single integer between 1 and 13 as input. And the calculated probability is based on all of the combinations that include at least one of the user selected number.
}
\usage{
game24prob(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
x is an user defined integer between 1 and 13. Each combinations of cards will include at least one of x.
}
}
\details{
This function calculates all of the possible combinations based on user input. Results will be decimal.
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

}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
probability = game24prob(7)

"Given that at least one card is the number you input, the probability of not being able to calculate 24 is  0.304195804195804"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
