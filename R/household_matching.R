# Forming households out of an already-fitted population.
#
# The population is left exactly as it was fitted. Household formation only decides who
# ends up with whom, never what anyone is, which is the difference between this and an
# approach that rewrites household positions until the pairing succeeds. Where a stratum
# cannot supply a partner the agent is carried up to a wider spatial level and, failing
# that, reported as unmatched.
#
# Each person is given a target partner value drawn from the requested distribution and is
# matched to the nearest person still free. Pairing the two sides in sorted order would be
# cheaper, and is the optimal assignment for total absolute deviation, but it cannot impose
# a target distribution of differences: where the two sides have similar value
# distributions the sorted coupling maps each rank onto its own rank and cancels the drawn
# gaps, so the realised differences collapse towards zero however the target is shaped.
# Matching to the nearest free candidate reproduces the target instead, and degrades only in
# the tails, where the values needed have run out.
#
# The candidates are held in a sorted array with a skip list over the entries already taken,
# so a lookup costs a binary search plus an amortised constant rather than a scan of the
# remaining pool. That is what keeps the pass near linear where walking the pool for every
# person would be quadratic.

# Draw from a vector without base R's one-argument surprise: sample(x) treats a length-one
# x as the number of items to permute rather than as the item itself, which silently invents
# indices whenever a stratum has exactly one person left to place.
resample <- function(x, n = length(x), replace = FALSE) {
    if (!length(x)) return(x[0])
    x[sample.int(length(x), n, replace = replace)]
}

# Build a lookup over `values` that returns the nearest entry not yet taken.
make_free_finder <- function(values) {
    o <- order(values)
    sorted <- values[o]
    n <- length(sorted)
    # Both lists mark a free entry by pointing at itself. Taking entry i points nxt[i] one
    # step forward and prv[i] one step back, so later searches walk straight past it, and the
    # path compression in each walk keeps the cost amortised constant.
    nxt <- seq_len(n + 1L)
    prv <- seq_len(n)

    find_next <- function(i) {
        r <- i
        while (r <= n && nxt[r] != r) r <- nxt[r]
        while (i <= n && nxt[i] != r) { j <- nxt[i]; nxt[i] <<- r; i <- j }
        r
    }
    find_prev <- function(i) {
        r <- i
        while (r >= 1L && prv[r] != r) r <- prv[r]
        while (i >= 1L && prv[i] != r) { j <- prv[i]; prv[i] <<- r; i <- j }
        r
    }
    list(
        # Position in the original vector of the free entry closest to `target`, or NA when
        # none is free within `upper`. The bound is a real restriction on what may be
        # chosen, not a nudge applied to the target: a target can be pulled back to a bound
        # and still be matched to a candidate beyond it, which is how an impossible pair
        # such as a parent younger than their child gets through.
        nearest = function(target, upper = Inf) {
            # findInterval gives the last entry at or below a value, so the nearest free
            # entry is either at or below that point, or strictly above it. Both directions
            # have to be tried; searching only downwards would bias every match young.
            limit <- if (is.finite(upper)) findInterval(upper, sorted) else n
            if (limit < 1L) return(NA_integer_)
            p <- min(findInterval(target, sorted), limit)
            b <- if (p >= 1L) find_prev(p) else 0L
            a <- find_next(p + 1L)
            cand <- c(if (a <= limit) a, if (b >= 1L) b)
            if (!length(cand)) return(NA_integer_)
            pick <- cand[which.min(abs(sorted[cand] - target))]
            nxt[pick] <<- pick + 1L
            prv[pick] <<- pick - 1L
            o[pick]
        },
        # Position in the original vector of the largest free entry, or NA.
        take_largest = function() {
            b <- find_prev(n)
            if (b < 1L) return(NA_integer_)
            nxt[b] <<- b + 1L
            prv[b] <<- b - 1L
            o[b]
        },
        any_free = function() find_prev(n) >= 1L
    )
}

# Pair two sets of values so that their differences reproduce a target distribution as
# closely as the values allow. Returns the matched positions on each side; when the sides
# are of unequal size the surplus is left out and reported by the caller.
assign_by_gap <- function(values_a, values_b, distribution, floor = NULL,
                          bound_a = NULL) {
    n <- min(length(values_a), length(values_b))
    if (n == 0) {
        return(list(a = integer(0), b = integer(0), floor_hits = 0L, floor_blocked = 0L))
    }
    # Which members of the longer side take part is decided at random, so that being early
    # in the population confers no advantage.
    keep_a <- if (length(values_a) > n) sample.int(length(values_a), n) else seq_len(n)
    va <- values_a[keep_a]
    # The value a floor is measured from need not be the value the distribution is drawn
    # against. A parent-child gap is drawn against the parent it was published for, but the
    # floor has to hold for every adult in the household, so the caller can supply the
    # limiting value separately.
    vbound <- if (is.null(bound_a)) va else bound_a[keep_a]
    gaps <- draw_gaps(distribution, n)

    target <- va - gaps
    floor_hits <- 0L
    bound <- rep(Inf, n)
    if (!is.null(floor)) {
        # A hard bound on the difference, such as the least plausible gap between a parent
        # and a child. The drawn target is pulled back to the bound, and the bound is also
        # imposed on which candidates may be taken, so that a pair violating it cannot be
        # formed at all. How often the target had to be pulled back is worth reporting.
        limit <- vbound - floor
        breach <- target > limit
        floor_hits <- sum(breach)
        target[breach] <- limit[breach]
        bound <- limit
    }

    # Processed in random order, so that no one systematically gets first choice and the
    # people matched last are a random subset rather than whoever the data happened to list
    # at the end.
    finder <- make_free_finder(values_b)
    out_a <- integer(n); out_b <- integer(n); k <- 0L
    blocked <- 0L
    for (i in sample.int(n)) {
        j <- finder$nearest(target[i], bound[i])
        if (is.na(j)) {
            # Nobody free within the bound. The pool may still hold candidates for others,
            # so this one is left unmatched rather than ending the pass; the caller carries
            # it up the spatial ladder and reports whatever is still unmatched at the end.
            if (!finder$any_free()) break
            blocked <- blocked + 1L
            next
        }
        k <- k + 1L
        out_a[k] <- keep_a[i]
        out_b[k] <- j
    }
    list(a = out_a[seq_len(k)], b = out_b[seq_len(k)],
         floor_hits = as.integer(floor_hits), floor_blocked = as.integer(blocked))
}

# Split a pool in two along the ordering of a value, so that a set drawn from one pool can
# be paired with itself. Used for couples of the same category, where there is no second
# pool to draw a partner from.
split_pool_by_value <- function(pos, values) {
    n <- floor(length(pos) / 2)
    if (n == 0) return(list(upper = integer(0), lower = integer(0)))
    o <- order(values)
    list(lower = pos[o[seq_len(n)]], upper = pos[o[seq(length(pos) - n + 1, length(pos))]])
}

# Pair a cell of the pool, applying pairing determinants one after another and finishing
# with the gap determinant. Each pairing determinant splits both sides by its levels and
# adjusts its target table to the levels actually present, so a table compiled nationally
# can be applied to a neighbourhood with a different composition.
#
# `symmetric` means both members of a pair come from the same set of people.
pair_recursive <- function(pos_a, pos_b, dat, determinants, k, symmetric, gap,
                           bounds = NULL) {
    pair_ds <- determinants$pairing
    if (k <= length(pair_ds)) {
        d <- pair_ds[[k]]
        col <- d$column
        if (symmetric) {
            lv_a <- as.character(dat[[col]][pos_a])
            levs <- sort(unique(lv_a))
            avail <- as.numeric(table(factor(lv_a, levels = levs)))
            names(avail) <- levs
            fitted <- fit_pairs_symmetric(pairing_target_matrix(d, levs, levs), avail)
            cnt <- integerise_pairs(fitted, avail, symmetric = TRUE)
            by_level <- split(pos_a, factor(lv_a, levels = levs))
            out <- list()
            for (a in levs) for (b in levs) {
                m <- cnt[a, b]
                if (m <= 0) next
                if (a == b) {
                    take <- utils::head(by_level[[a]], 2 * m)
                    by_level[[a]] <- setdiff(by_level[[a]], take)
                    out[[length(out) + 1]] <- pair_recursive(take, take, dat, determinants,
                                                             k + 1, TRUE, gap, bounds)
                } else {
                    ta <- utils::head(by_level[[a]], m)
                    tb <- utils::head(by_level[[b]], m)
                    by_level[[a]] <- setdiff(by_level[[a]], ta)
                    by_level[[b]] <- setdiff(by_level[[b]], tb)
                    out[[length(out) + 1]] <- pair_recursive(ta, tb, dat, determinants,
                                                             k + 1, FALSE, gap, bounds)
                }
            }
            return(combine_pairs(out))
        } else {
            lv_a <- as.character(dat[[col]][pos_a])
            lv_b <- as.character(dat[[col]][pos_b])
            levs_a <- sort(unique(lv_a)); levs_b <- sort(unique(lv_b))
            avail_a <- as.numeric(table(factor(lv_a, levels = levs_a)))
            avail_b <- as.numeric(table(factor(lv_b, levels = levs_b)))
            fitted <- fit_pairs_bipartite(pairing_target_matrix(d, levs_a, levs_b),
                                          avail_a, avail_b)
            cnt <- integerise_pairs(fitted, list(a = avail_a, b = avail_b), symmetric = FALSE)
            grp_a <- split(pos_a, factor(lv_a, levels = levs_a))
            grp_b <- split(pos_b, factor(lv_b, levels = levs_b))
            out <- list()
            for (i in seq_along(levs_a)) for (j in seq_along(levs_b)) {
                m <- cnt[i, j]
                if (m <= 0) next
                ta <- utils::head(grp_a[[i]], m); grp_a[[i]] <- setdiff(grp_a[[i]], ta)
                tb <- utils::head(grp_b[[j]], m); grp_b[[j]] <- setdiff(grp_b[[j]], tb)
                out[[length(out) + 1]] <- pair_recursive(ta, tb, dat, determinants,
                                                         k + 1, FALSE, gap, bounds)
            }
            return(combine_pairs(out))
        }
    }

    # No pairing determinants left: order the match by the gap determinant.
    if (is.null(gap)) {
        n <- if (symmetric) floor(length(pos_a) / 2) else min(length(pos_a), length(pos_b))
        if (n == 0) return(empty_pairs())
        if (symmetric) {
            s <- resample(pos_a)
            return(list(a = s[seq_len(n)], b = s[n + seq_len(n)], floor_hits = 0L,
                        floor_blocked = 0L))
        }
        return(list(a = resample(pos_a, n), b = resample(pos_b, n),
                    floor_hits = 0L, floor_blocked = 0L))
    }

    values <- dat[[gap$column]]
    if (symmetric) {
        halves <- split_pool_by_value(pos_a, values[pos_a])
        if (!length(halves$upper)) return(empty_pairs())
        # Within one category there is no side the sign belongs to, so the distribution is
        # applied to the size of the difference and the older half takes the positive end.
        res <- assign_by_gap(values[halves$upper], values[halves$lower],
                             abs_distribution(gap$distribution), gap$floor)
        return(list(a = halves$upper[res$a], b = halves$lower[res$b],
                    floor_hits = res$floor_hits, floor_blocked = res$floor_blocked))
    }

    # Orientation decides which side a positive gap belongs to. Where the determinant names
    # a column and a reference level, the side holding that level takes the positive end;
    # otherwise the first side does, which is the convention the parent-child pass uses.
    flip <- FALSE
    if (!is.null(gap$orient_by)) {
        lev_a <- unique(as.character(dat[[gap$orient_by]][pos_a]))
        lev_b <- unique(as.character(dat[[gap$orient_by]][pos_b]))
        if (!(length(lev_a) == 1 && lev_a == gap$orient_reference) &&
             (length(lev_b) == 1 && lev_b == gap$orient_reference)) {
            flip <- TRUE
        }
    }
    if (flip) {
        res <- assign_by_gap(values[pos_b], values[pos_a], gap$distribution, gap$floor)
        return(list(a = pos_a[res$b], b = pos_b[res$a], floor_hits = res$floor_hits,
                    floor_blocked = res$floor_blocked))
    }
    res <- assign_by_gap(values[pos_a], values[pos_b], gap$distribution, gap$floor,
                         bound_a = if (!is.null(bounds)) bounds[pos_a])
    list(a = pos_a[res$a], b = pos_b[res$b], floor_hits = res$floor_hits,
         floor_blocked = res$floor_blocked)
}

empty_pairs <- function() list(a = integer(0), b = integer(0), floor_hits = 0L,
                              floor_blocked = 0L)

combine_pairs <- function(lst) {
    if (!length(lst)) return(empty_pairs())
    list(a = unlist(lapply(lst, `[[`, "a"), use.names = FALSE),
         b = unlist(lapply(lst, `[[`, "b"), use.names = FALSE),
         floor_hits = sum(vapply(lst, `[[`, integer(1), "floor_hits")),
         floor_blocked = sum(vapply(lst, `[[`, integer(1), "floor_blocked")))
}

# Fold a signed distribution onto its absolute value, for pairs where the sign has no side
# to belong to.
abs_distribution <- function(d) {
    if (is.null(d)) return(NULL)
    lo <- pmin(abs(d$from), abs(d$to))
    hi <- pmax(abs(d$from), abs(d$to))
    gap_distribution(lo, hi, d$count)
}

# Fit a distribution over household sizes so that a given number of units holds a given
# number of members. The shape of the published distribution is kept and shifted by
# exponential tilting, which is the least-committal way to move a discrete distribution
# onto a required mean, and the result is then rounded to whole households.
fit_size_counts <- function(sizes, weights, n_units, n_members) {
    if (n_units <= 0 || !length(sizes)) return(stats::setNames(integer(0), character(0)))
    sizes <- as.numeric(sizes)
    weights <- as.numeric(weights)
    ok <- is.finite(sizes) & is.finite(weights) & weights > 0
    sizes <- sizes[ok]; weights <- weights[ok]
    if (!length(sizes)) return(stats::setNames(integer(0), character(0)))

    # A published top category is normally open-ended - "three or more children" - so the
    # largest listed size is extended when the households present have to absorb more
    # members than it allows. Without this the surplus would be reported as unplaceable
    # purely because the source table stopped counting.
    need <- ceiling(n_members / n_units)
    if (is.finite(need) && need > max(sizes)) {
        extra <- seq(max(sizes) + 1, need)
        top <- max(weights[sizes == max(sizes)])
        sizes <- c(sizes, extra)
        weights <- c(weights, pmax(top * 0.5 ^ seq_along(extra), 1e-12))
    }

    # Shift the published shape onto the mean the area actually requires, by exponential
    # tilting, which is the least-committal way to move a discrete distribution onto a
    # required mean. Weights are handled in logs so that a steep tilt over a wide range of
    # sizes cannot overflow.
    logp <- log(weights / sum(weights))
    target_mean <- min(max(n_members / n_units, min(sizes)), max(sizes))
    tilted <- function(lambda) {
        lw <- logp + lambda * sizes
        w <- exp(lw - max(lw))
        w / sum(w)
    }
    tilt <- function(lambda) sum(tilted(lambda) * sizes) - target_mean

    lambda <- 0
    if (abs(tilt(0)) > 1e-9) {
        lo <- -25; hi <- 25
        if (tilt(lo) * tilt(hi) < 0) {
            lambda <- tryCatch(stats::uniroot(tilt, c(lo, hi))$root, error = function(e) 0)
        } else {
            lambda <- if (tilt(0) < 0) hi else lo
        }
    }
    p <- tilted(lambda)
    if (anyNA(p) || !sum(p)) p <- weights / sum(weights)
    counts <- GenSynthPop::calculate_group_counts(p, n_units)
    counts[is.na(counts)] <- 0L
    counts <- pmax(counts, 0L)

    # Rounding fixes the number of households but not the number of members, so move
    # households between adjacent sizes until both hold.
    guard <- 0L
    while (sum(counts * sizes) != n_members && guard < 200000L) {
        guard <- guard + 1L
        short <- n_members - sum(counts * sizes)
        if (short > 0) {
            from <- which(counts > 0 & sizes < max(sizes))
            if (!length(from)) break
            i <- from[which.min(sizes[from])]
            j <- which(sizes == sizes[i] + 1)[1]
        } else {
            from <- which(counts > 0 & sizes > min(sizes))
            if (!length(from)) break
            i <- from[which.max(sizes[from])]
            j <- which(sizes == sizes[i] - 1)[1]
        }
        if (is.na(j)) break
        counts[i] <- counts[i] - 1L
        counts[j] <- counts[j] + 1L
    }
    stats::setNames(as.integer(counts), as.character(sizes))
}

# Group children into sibling sets of the requested sizes. The oldest child not yet placed
# seeds each set, and its siblings are the children closest to the ages implied by drawing
# from the spacing distribution, so realised sibling gaps follow the published birth spacing
# rather than collapsing onto the nearest available age.
group_siblings <- function(pos, ages, sizes, spacing) {
    if (!length(pos) || !length(sizes)) return(list())
    vals <- ages[pos]
    finder <- make_free_finder(vals)
    sets <- vector("list", length(sizes))

    for (s in seq_along(sizes)) {
        seed <- finder$take_largest()
        if (is.na(seed)) break
        members <- seed
        k <- sizes[s]
        if (k > 1) {
            wanted <- vals[seed] - cumsum(abs(draw_gaps(spacing, k - 1)))
            for (w in wanted) {
                j <- finder$nearest(w)
                if (is.na(j)) break
                members <- c(members, j)
            }
        }
        sets[[s]] <- pos[members]
    }
    Filter(Negate(is.null), sets)
}
