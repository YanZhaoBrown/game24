\name{plot_comparison}
\alias{plot_comparison}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot_comparison
}
\description{
This function gives user a comparison barplot of the probability of each cards getting 24 vs getting other number instead of 24. y axis is the probability to get 24 or other number for each combination containing the specific cards. We know that some of the combinations are not being able to get 24 or other number, so we make the comparison plot.
}
\usage{
plot_comparison(b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{
b should be an integer that users are interested to compare the probability with 24. We recommand user choose an integer between 20 and 30.
}
}
\details{
We use the plot to explain why it is so-called game24? 24 is the greatest number with highest probability to get by using the combinations of four cards (1-13). The total probability of getting 24 is clearly larger than the probability of other test number. And almost for each cards, they are more likely to do the combinations to get 24 instead of other numbers.
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
plot_comparison(27)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
