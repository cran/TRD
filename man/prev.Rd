\name{prev}
\alias{prev}
\title{
Calculate theoretical prevalence of disease
}
\description{
Use 3 penetrance factors for genotype 0, 1 and 2 (additive model),  ratio of maternal to paternal-induced disease risk for a heterozygous child, tranmission ratio of minor allele from mother and father to offspring, and minor allele frequency (MAF) to calculate the disease prevalence. Additive model means the genotype is the number of copies of minor allele that the individual carries assuming a bi-allelic locus. This genotype will be 0 for homozygous wild-type, 1 for heterozygous, and 2 for homozygous mutant.  Minor allele normally refers to the disease allele, which has lower frequency than the alternative allele(s).
}
\usage{
prev(f0, f1, f2, g, tmother, tfather, maf)
}
\arguments{
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
\item{d}{disease prevalence}
}
\author{
Lam Opal Huang
}
\examples{
prev(0.1,0.2,0.3,1,0.5,0.5,0.1)
}
