\name{game24_makeCards}
\alias{game24_makeCards}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
game24_makeCards
}
\description{
This function allows you to play the famous card game 24 on the console.
It gives the user four random cards between 1-13 and allows the user to input when they would like to see solutions.
}
\usage{
game24_makeCards()
}
%- maybe also 'usage' for other objects documented here.
\details{
 If you enter the number other than 24 when you want the solution, the warning of "That was not 24. Please enter 24." will pop up. If there is no solution for the random four numbers, you will get "You can not get 24".
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
 game24_makeCards()
  "Make 24 from these cards: 3 1 12 11"
  Enter your 24 when you want solutions: 24
  "Method  1 : 3 * 12 = 36 then ( 36 - 11 ) = 25 then ( 25 - 1 ) = 24"
  "Method  2 : 3 * 12 = 36 then ( 36 - 1 ) = 35 then ( 35 - 11 ) = 24"
  "Method  3 : ( 11 + 1 ) = 12 then 12 * 3 = 36 then ( 36 - 12 ) = 24"
}
