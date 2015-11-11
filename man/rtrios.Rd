\name{rtrios}
\alias{rtrios}
\title{
Generate simulated popoulation of case- and control-trios
}
\description{
Using penetrance factors, ratio of maternal- to paternal-induced disease risk for a heterozygous child, tranmission ratio of minor allele from mother and father to offspring, and minor allele frequency (MAF) to generate a simulated population of case- and/or control-trios dataset with first column as disease status (1=case, 0=control), the second, third and forth columns as maternal, paternal and child genotypes. The fifth column as the parent-of-origin indicator of disease allele in a heterozygous child (a value of 1 indicates the mother transmitted the disease allele and 0 indicates the father transmitted). The parent-of-origin indicator is only meaningful  for a heterozygous child. Minor allele normally refers to the disease allele, which has lower frequency than the alternative allele(s).


}
\usage{
rtrios(n, ssize, f0, f1, f2, g, tmother, tfather, maf)
}
\arguments{
  \item{n}{
simulated population size
}
  \item{ssize}{
sample size
}
  \item{f0}{
penetrance for individuals with genotype 0 (additive model)
}
  \item{f1}{
penetrance for individuals with genotype 1 (additive model)
}
  \item{f2}{
penetrance for individuals with genotype 2 (additive model)
}
  \item{g}{
Ratio of maternal- over paternal-induced disease risk
}
  \item{tmother}{
transmission ratio of minor allele from mother to child
}
  \item{tfather}{
transmission ratio of minor allele from father to child
}
  \item{maf}{
minor allele frequency
}
}
\value{
\item{case}{case-trios data with parent-of-origin indicator}
\item{ctrl}{control-trios data with parent-of-origin indicator}
}
\author{
Lam Opal Huang
}
\seealso{
\code{\link{find.maf}}
\code{\link{find.t}}
\code{\link{gcount}}
\code{\link{ll}}
\code{\link{prev}}
\code{\link{strata.cnt}}
\code{\link{tdt}}
}
\examples{
trios=rtrios(100000,500,0.1,0.2,0.3,1,0.5,0.5,0.1)
}
