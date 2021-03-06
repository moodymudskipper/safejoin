% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_safe_joins.R
\name{safe_joins}
\alias{safe_joins}
\alias{safe_left_join}
\alias{safe_right_join}
\alias{safe_inner_join}
\alias{safe_full_join}
\alias{safe_semi_join}
\alias{safe_anti_join}
\alias{safe_nest_join}
\title{Safe joins}
\usage{
safe_left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  na_matches = c("na", "never"),
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)

safe_right_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  na_matches = c("na", "never"),
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)

safe_inner_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  na_matches = c("na", "never"),
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)

safe_full_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  na_matches = c("na", "never"),
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)

safe_semi_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  na_matches = c("na", "never"),
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)

safe_anti_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  na_matches = c("na", "never"),
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)

safe_nest_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  keep = FALSE,
  name = NULL,
  match_fun = NULL,
  check = "~blC",
  conflict = NULL
)
}
\arguments{
\item{x, y}{tbls to join}

\item{by}{A character vector of variables to join by.

If \code{NULL}, the default, \verb{*_join()} will perform a natural join, using all
variables in common across \code{x} and \code{y}. A message lists the variables so that you
can check they're correct; suppress the message by supplying \code{by} explicitly.

To join by different variables on \code{x} and \code{y}, use a named vector.
For example, \code{by = c("a" = "b")} will match \code{x$a} to \code{y$b}.

To join by multiple variables, use a vector with length > 1.
For example, \code{by = c("a", "b")} will match \code{x$a} to \code{y$a} and \code{x$b} to
\code{y$b}. Use a named vector to match different variables in \code{x} and \code{y}.
For example, \code{by = c("a" = "b", "c" = "d")} will match \code{x$a} to \code{y$b} and
\code{x$c} to \code{y$d}.

To perform a cross-join, generating all combinations of \code{x} and \code{y},
use \code{by = character()}.}

\item{copy}{If \code{x} and \code{y} are not from the same data source,
and \code{copy} is \code{TRUE}, then \code{y} will be copied into the
same src as \code{x}.  This allows you to join tables across srcs, but
it is a potentially expensive operation so you must opt into it.}

\item{suffix}{If there are non-joined duplicate variables in \code{x} and
\code{y}, these suffixes will be added to the output to disambiguate them.
Should be a character vector of length 2.}

\item{na_matches}{Should \code{NA} and \code{NaN} values match one another?

The default, \code{"na"}, treats two \code{NA} or \code{NaN} values as equal, like
\code{\%in\%}, \code{\link[=match]{match()}}, \code{\link[=merge]{merge()}}.

Use \code{"never"} to always treat two \code{NA} or \code{NaN} values as different, like
joins for database sources, similarly to \code{merge(incomparables = FALSE)}.}

\item{match_fun}{Vectorized function
given two columns, returning \code{TRUE} or \code{FALSE} as to whether they are a
match. Can be a list of functions one for each pair of columns specified in
by (if a named list, it uses the names in x). If only one function is given
it is used on all column pairs.}

\item{check}{a string, see details}

\item{conflict}{if \code{NULL}, in case of column conflict both columns are
suffixed as in \emph{dplyr}, if a function of two parameters or a formula,
a function is applied on both columns. If the string "patch", matched
values from \code{y} will overwrite existing values in \code{x} while the other
values will be kept}

\item{keep}{Should the join keys from both \code{x} and \code{y} be preserved in the
output? Only applies to \code{nest_join()}, \code{left_join()}, \code{right_join()}, and
\code{full_join()}.}

\item{name}{The name of the list column nesting joins create.
If \code{NULL} the name of \code{y} is used.}
}
\description{
Wrappers around \emph{dplyr}'s joining function that allow to check a variety of
things on the fly and either \code{inform}, \code{warn} or \code{abort} as a result.
}
\details{
\code{check} is a combination of characters which will trigger different checks:
\describe{
\item{b}{as in \strong{b}y, check that \code{by} was given explicitly. Default behavior
in \emph{dplyr} is to trigger a message}
\item{c}{as in \strong{c}olumn \strong{c}onflict, check if, among non join
columns, some column names are found in both \code{x} and \code{y}. Default behavior
in \emph{dplyr}'s joining functions is to suffix them silently.}
\item{u}{as in \strong{u}nique, check if no set of values of join columns
is duplicated in \code{x}}
\item{v}{the letter after \strong{u}, check if no set of values of join columns
is duplicated in \code{y}}
\item{m}{as in \strong{m}atch, check if all sets of values of join columns in
\code{x} wil be matched in \code{y}}
\item{n}{the letter after \strong{m}, check if all sets of values of join columns in
\code{y} wil be matched in \code{x}}
\item{e}{as in \strong{e}xpanded, check that all combinations of values of
join columns are present in \code{x}}
\item{f}{the letter after \strong{e}, check that all combinations of values of
join columns are present in \code{y}}
\item{l}{as in \strong{l}evels, check that join columns are consistent in term of
factor levels}
\item{t}{as in  \strong{t}ype, check that joining columns have same class and type}
}

An upper case letter will trigger \code{abort}, a lower case letter will trigger
\code{warn}, a lower case letter prefixed with \code{~} will trigger a message. Other
characters will be ignored.
}
