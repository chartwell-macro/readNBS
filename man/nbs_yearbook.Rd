\name{nbs_yearbook}
\alias{nbs_yearbook_discover}
\alias{nbs_yearbook_discover_live}
\alias{nbs_yearbook_parse_table}
\alias{nbs_yearbook_routes}
\alias{nbs_yearbook_targets}
\title{Discover and parse frozen NBS Yearbook tables}
\usage{
nbs_yearbook_discover(index_html, yearbook,
  index_url = .nbs_yearbook_index_url(yearbook))

nbs_yearbook_discover_live(yearbook, client = nbs_client())

nbs_yearbook_parse_table(table_html, route, retrieved_at = Sys.time(),
  raw_sha256 = NULL)

nbs_yearbook_routes(index_html, yearbook,
  index_url = .nbs_yearbook_index_url(yearbook))

nbs_yearbook_targets(yearbook = NULL)
}
\arguments{
  \item{index_html}{A retained official Yearbook contents page.}
  \item{yearbook}{A four-digit Yearbook publication year.}
  \item{index_url}{The official contents-page URL.}
  \item{client}{An object returned by \code{nbs_client()}.}
  \item{table_html}{One locally stored official Yearbook table.}
  \item{route}{One resolved route returned by \code{nbs_yearbook_routes()}.}
  \item{retrieved_at}{The snapshot retrieval time.}
  \item{raw_sha256}{Optional SHA-256 of the source bytes.}
}
\value{
Discovery returns table routes; targets return frozen table contracts; parsing
returns normalized long observations with table, URL, timestamp, SHA-256 and
schema-era provenance.
}
\description{
Utilities for the opt-in Yearbook maintenance path. Normal package reads use
the bundled annual legacy archive and do not make network requests. Retained
table geometry and Chinese labels are checked strictly before publication.
}
\details{
\code{nbs_yearbook_discover_live()} is a maintenance-only network operation.
Applications should retain the discovered pages and use the other functions
on the frozen copies. The bundled 2011 snapshots are decoded deterministically
from GB18030 by the archive builder.
}
