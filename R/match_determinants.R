# Matching determinants and the primitives that act on them.
#
# A determinant says what should govern who ends up with whom. Two kinds are supported and
# both are optional, so a population with nothing but ages can still be matched:
#
#   * a gap determinant  - a numeric column and a target distribution of differences,
#                          for instance the age gap between partners
#   * a pairing determinant - a categorical column and a target table of level pairs,
#                          for instance origin or educational homogamy
#
# Any number of pairing determinants may be supplied. They are applied by nesting: each one
# splits the pool into cells whose sizes reproduce the target table as closely as the
# available people allow, and the gap determinant then orders the match inside every cell.
# Nothing here knows anything about a particular country's categories.

#' Define a Target Distribution of Differences
#'
#' Builds the target distribution a gap determinant matches against. Bands are given by
#' explicit `from` and `to` bounds rather than parsed out of a label, so that negative
#' bounds are unambiguous.
#'
#' @param from Numeric vector of lower bounds, one per band.
#' @param to Numeric vector of upper bounds, one per band. For an exact value, set equal
#'   to `from`.
#' @param count Numeric vector of counts or relative frequencies, one per band. Rescaled
#'   internally, so published counts can be passed straight through.
#'
#' @return A `gsp_gap_distribution` data frame with columns `from`, `to` and `count`.
#'
#' @details
#' Bands are drawn from uniformly between their bounds. A distribution of partner age gaps
#' published as "no difference", "1 to 5 years older" and so on becomes a set of bands with
#' the sign carried by `from` and `to`: a partner five to ten years younger is
#' `from = -10, to = -5`.
#'
#' @examples
#' # Partner age gap, positive meaning the reference partner is older
#' gap_distribution(
#'   from  = c(-20, -10,  -5,  -1, 0,  1,  5, 10, 20),
#'   to    = c(-40, -20, -10,  -5, 0,  5, 10, 20, 40),
#'   count = c(133, 496, 2276, 10375, 8023, 29171, 12331, 3438, 1180)
#' )
#'
#' @seealso [match_on_gap()]
#' @export
gap_distribution <- function(from, to, count) {
    if (length(from) != length(to) || length(from) != length(count)) {
        stop("'from', 'to' and 'count' must be the same length.")
    }
    lo <- pmin(from, to)
    hi <- pmax(from, to)
    keep <- !is.na(count) & count > 0
    if (!any(keep)) stop("A gap distribution needs at least one band with a positive count.")
    out <- data.frame(from = as.numeric(lo[keep]), to = as.numeric(hi[keep]),
                      count = as.numeric(count[keep]))
    out$count <- out$count / sum(out$count)
    structure(out, class = c("gsp_gap_distribution", "data.frame"))
}

#' Match on the Difference Between Two Values
#'
#' Declares that pairs should reproduce a target distribution of differences in a numeric
#' column, most often age.
#'
#' @param column Name of the numeric column in the population.
#' @param distribution A [gap_distribution()]. If `NULL`, the pairing is left free on this
#'   column and only the other determinants apply.
#' @param floor Optional hard lower bound on the difference. Pairs that would violate it
#'   are pulled back to the bound before matching, and the number affected is reported.
#'   Used for a minimum plausible parent-child gap.
#' @param orient_by Optional name of a categorical column that fixes which side of a pair
#'   the difference is measured from, so that a signed distribution keeps its meaning.
#' @param orient_reference The level of `orient_by` the difference is measured from. A
#'   positive difference means this side has the larger value.
#'
#' @return A `gsp_determinant` object of kind `"gap"`.
#'
#' @examples
#' match_on_gap("age",
#'   distribution = gap_distribution(from = c(0, 1), to = c(0, 5), count = c(1, 4)),
#'   orient_by = "sex", orient_reference = "male")
#'
#' @export
match_on_gap <- function(column, distribution = NULL, floor = NULL,
                         orient_by = NULL, orient_reference = NULL) {
    if (!is.null(distribution) && !inherits(distribution, "gsp_gap_distribution")) {
        stop("'distribution' must come from gap_distribution().")
    }
    if (!is.null(orient_by) && is.null(orient_reference)) {
        stop("'orient_by' needs an 'orient_reference' level to measure the gap from.")
    }
    structure(list(kind = "gap", column = column, distribution = distribution,
                   floor = floor, orient_by = orient_by,
                   orient_reference = orient_reference),
              class = "gsp_determinant")
}

#' Match on a Table of Level Pairs
#'
#' Declares that pairs should reproduce a target table of category combinations, which is
#' how homogamy in origin, education or any other categorical attribute is imposed.
#'
#' @param column Name of the categorical column in the population.
#' @param table A data frame with columns `level_a`, `level_b` and `count`, giving the
#'   target number or relative frequency of pairs for each combination. Combinations may be
#'   listed once; the table is treated as unordered unless `ordered = TRUE`.
#' @param ordered Whether `level_a` and `level_b` name distinguishable sides. Left `FALSE`
#'   for a symmetric attribute such as origin, where a pair of two levels is the same pair
#'   whichever way round it is written.
#'
#' @return A `gsp_determinant` object of kind `"pairing"`.
#'
#' @details
#' The target table is adjusted to the people actually present in each stratum before it is
#' used, so a national homogamy table can be applied to a neighbourhood whose composition
#' differs from the national one. The adjustment preserves the association in the table
#' while meeting the local availability of each level, which is the same logic the package
#' already applies to contingency tables through iterative proportional fitting.
#'
#' @examples
#' match_on_pairing("migration_background", data.frame(
#'   level_a = c("Origin_Dutch", "Origin_Dutch", "Origin_OutsideEurope"),
#'   level_b = c("Origin_Dutch", "Origin_OutsideEurope", "Origin_OutsideEurope"),
#'   count   = c(70, 8, 22)
#' ))
#'
#' @export
match_on_pairing <- function(column, table, ordered = FALSE) {
    needed <- c("level_a", "level_b", "count")
    if (!all(needed %in% colnames(table))) {
        stop("The pairing table for '", column, "' needs columns ",
             paste(needed, collapse = ", "), ".")
    }
    table <- table[!is.na(table$count) & table$count > 0, needed, drop = FALSE]
    if (!nrow(table)) stop("The pairing table for '", column, "' has no positive counts.")
    table$level_a <- as.character(table$level_a)
    table$level_b <- as.character(table$level_b)
    structure(list(kind = "pairing", column = column, table = table, ordered = ordered),
              class = "gsp_determinant")
}

#' @export
print.gsp_determinant <- function(x, ...) {
    if (x$kind == "gap") {
        cat("<gap determinant on '", x$column, "'",
            if (!is.null(x$orient_by)) paste0(", oriented by '", x$orient_by, "' = '",
                                              x$orient_reference, "'") else "",
            if (!is.null(x$floor)) paste0(", floor ", x$floor) else "",
            ">\n", sep = "")
    } else {
        cat("<pairing determinant on '", x$column, "', ", nrow(x$table),
            " combination(s)>\n", sep = "")
    }
    invisible(x)
}

# ---------------------------------------------------------------------------------------
# Primitives
# ---------------------------------------------------------------------------------------

# Draw `n` gaps whose realised composition matches the target distribution exactly, using
# the package's existing rounding rule so the counts sum to n. Band interiors are filled
# uniformly, which keeps the drawn values continuous within a band rather than piling them
# on the bound.
draw_gaps <- function(distribution, n, integer_valued = TRUE) {
    if (n <= 0) return(numeric(0))
    if (is.null(distribution)) return(rep(0, n))
    counts <- GenSynthPop::calculate_group_counts(distribution$count, n)
    out <- numeric(0)
    for (i in seq_len(nrow(distribution))) {
        k <- counts[i]
        if (k <= 0) next
        lo <- distribution$from[i]
        hi <- distribution$to[i]
        v <- if (isTRUE(all.equal(lo, hi))) rep(lo, k) else stats::runif(k, lo, hi)
        out <- c(out, v)
    }
    if (integer_valued) out <- round(out)
    out
}

# Adjust a target table of pairs so that it can be realised with the people actually
# available, for two distinguishable pools. This is ordinary iterative proportional fitting
# on a matrix: the association in the target is preserved while the row and column totals
# are moved onto the available counts.
fit_pairs_bipartite <- function(target, avail_a, avail_b, max_iter = 60, tol = 1e-9) {
    m <- target
    m[m <= 0] <- 1e-12
    for (i in seq_len(max_iter)) {
        rs <- rowSums(m)
        m <- m * ifelse(rs > 0, avail_a / rs, 0)
        cs <- colSums(m)
        m <- sweep(m, 2, ifelse(cs > 0, avail_b / cs, 0), "*")
        if (max(abs(rowSums(m) - avail_a)) < tol) break
    }
    m[!is.finite(m)] <- 0
    m
}

# The same adjustment for a single pool, where both members of a pair are drawn from the
# same people. The margin a symmetric pair table has to meet is the number of *people* of
# each level, and a pair of two people of the same level consumes two of them, so the
# implied demand is the row sum plus the diagonal. The multiplicative update below is the
# symmetric counterpart of the bipartite step above.
fit_pairs_symmetric <- function(target, avail, max_iter = 200, tol = 1e-7) {
    m <- target
    m[m <= 0] <- 1e-12
    for (i in seq_len(max_iter)) {
        demand <- rowSums(m) + diag(m)
        ratio <- ifelse(demand > 0, avail / demand, 0)
        adj <- sqrt(outer(ratio, ratio))
        m <- m * adj
        if (max(abs(demand - avail)) < tol) break
    }
    m[!is.finite(m)] <- 0
    m
}

# Turn a fitted real-valued pair table into whole couples that consume no more people than
# are present. Rounding is done on the flattened table with the package's existing rule, and
# any level left over- or under-subscribed by rounding is repaired one couple at a time.
integerise_pairs <- function(fitted, avail, symmetric) {
    levs_a <- rownames(fitted)
    levs_b <- colnames(fitted)
    flat <- as.vector(fitted)
    total <- sum(fitted)
    n_pairs <- floor(total + 1e-9)
    counts <- if (n_pairs > 0 && sum(flat) > 0) {
        GenSynthPop::calculate_group_counts(flat / sum(flat), n_pairs)
    } else {
        rep(0L, length(flat))
    }
    cnt <- matrix(counts, nrow = nrow(fitted), dimnames = dimnames(fitted))

    if (symmetric) {
        # Fold the table onto its upper triangle: a pair of levels is one pair however it
        # is written, and keeping both cells would double-count it.
        cnt[lower.tri(cnt)] <- 0
        used <- rowSums(cnt) + colSums(cnt)
        repeat {
            slack <- avail - used
            if (all(slack <= 0) || !any(slack > 0)) break
            i <- which.max(slack)
            slack[i] <- -Inf
            j <- if (any(slack > 0)) which.max(slack) else i
            if (avail[i] - used[i] < 1 || avail[j] - used[j] < 1) break
            if (i == j && avail[i] - used[i] < 2) break
            a <- min(i, j); b <- max(i, j)
            cnt[a, b] <- cnt[a, b] + 1
            used <- rowSums(cnt) + colSums(cnt)
        }
        while (any(used > avail)) {
            i <- which.max(used - avail)
            cand <- which(cnt[i, ] > 0)
            cand2 <- which(cnt[, i] > 0)
            if (!length(cand) && !length(cand2)) break
            if (length(cand)) cnt[i, cand[1]] <- cnt[i, cand[1]] - 1
            else cnt[cand2[1], i] <- cnt[cand2[1], i] - 1
            used <- rowSums(cnt) + colSums(cnt)
        }
    } else {
        while (any(rowSums(cnt) > avail$a) || any(colSums(cnt) > avail$b)) {
            r <- rowSums(cnt) - avail$a
            cc <- colSums(cnt) - avail$b
            if (max(r) >= max(cc)) {
                i <- which.max(r); j <- which.max(cnt[i, ])
            } else {
                j <- which.max(cc); i <- which.max(cnt[, j])
            }
            if (cnt[i, j] <= 0) break
            cnt[i, j] <- cnt[i, j] - 1
        }
    }
    cnt
}

# Build the target pair matrix for a determinant over the levels present, filling
# combinations the table does not mention with a small positive weight so that a stratum
# whose only feasible pairing is unlisted still produces couples rather than none.
pairing_target_matrix <- function(determinant, levels_a, levels_b) {
    m <- matrix(1e-6, nrow = length(levels_a), ncol = length(levels_b),
                dimnames = list(levels_a, levels_b))
    tb <- determinant$table
    for (k in seq_len(nrow(tb))) {
        a <- tb$level_a[k]; b <- tb$level_b[k]; v <- tb$count[k]
        if (a %in% levels_a && b %in% levels_b) m[a, b] <- m[a, b] + v
        # The mirror image of an unordered combination, but only when the two levels
        # differ: a pair of two people of the same level is one cell, and adding it twice
        # would double every same-level pairing.
        if (!determinant$ordered && a != b && b %in% levels_a && a %in% levels_b) {
            m[b, a] <- m[b, a] + v
        }
    }
    m
}
