\name{gcount}
\alias{gcount}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate genotype counts from trios dataset
}
\description{
Calculate the counts for genotype 2, 1, and 0 separately in case- and control-trios assuming an additive model.
}
\usage{
gcount(sample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{
  A sample of case- and control-trios with first column as the disease status (1=case, 0=control), second column as mother genotype, third father genotype and fourth child genotype.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
\item{case}{case-trios}
\item{control}{control-trios}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Lam Opal Huang
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rtrios}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
trios=rtrios(1000,50,0.1,0.2,0.3,0.5,0.5,0.1)
gcount(rbind(trios[[1]],trios[[2]]))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
