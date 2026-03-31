\name{run_app}
\alias{run_app}
\title{Run the sMCDA Tool}
\usage{
run_app(..., port = getOption("shiny.port"), launch.browser = interactive())
}
\arguments{
\item{...}{Named options passed through \code{golem::with_golem_options()}.}

\item{port}{Port used by the Shiny application.}

\item{launch.browser}{Whether to open the app in a browser.}
}
\value{
Invisibly returns the running Shiny app.
}
\description{
Starts the sMCDA Tool using the golem-compatible wrapper and the legacy
application definitions bundled in the package.
}
\examples{
\dontrun{
run_app()
}
}
