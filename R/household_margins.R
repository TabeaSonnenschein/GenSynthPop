# Turning published household counts into person-level margins.
#
# Household counts and synthetic populations are in different units. A spatial statistic
# usually says how many households of a kind an area holds, while the population is a table
# of people, so the two cannot be compared until one is converted into the other. The
# conversion is exact arithmetic once it is known how many people of each role a household
# of each type contains: a household of a given type in a given area contributes that many
# people of that role, and summing over types gives a person-level target the existing
# attribute fitting can use as an ordinary margin.
#
# Doing it this way means household counts constrain the population rather than falling out
# of it, so the household formation that follows never has to rewrite an already-fitted
# attribute to make the numbers work.

#' Convert Published Household Counts Into Person-Level Margins
#'
#' Takes the household counts published for each spatial unit and turns them into a margin
#' on the population's household-role column, so that [Conditional_attribute_adder()] can
#' fit the role against them like any other spatial margin. Also returns the implied number
#' of households of each type per area, which [assign_households()] builds against.
#'
#' @param spatial_households Data frame of published counts, with one column identifying the
#'   spatial unit, one naming the published household category, and a `count` column.
#' @param category_split Data frame splitting each published category into the household
#'   types the model uses, with columns for the category, the type, and a `share` or `count`
#'   column giving the split. Optionally carries a region column, so the split can vary by
#'   region. Pass `NULL` when the published categories are already the model's types.
#' @param composition Data frame giving how many people of each role a household of each
#'   type contains, with columns for the type, the role, and `members`. Optionally carries a
#'   region column. Fractional members are expected and are what a mean number of children
#'   per household looks like.
#' @param structure A [household_structure()], used to check that every role named in
#'   `composition` is one the population actually carries.
#' @param area Name of the spatial unit column in `spatial_households`. Default `"area"`.
#' @param category Name of the published category column. Default `"category"`.
#' @param type Name of the household type column in `category_split` and `composition`.
#'   Default `"household_type"`.
#' @param role Name of the role column in `composition`, and the name the returned margin
#'   uses. Default `"role"`.
#' @param region Optional name of the region column in `category_split` and `composition`.
#'   When given, `area_region` must map each area to a region.
#' @param area_region Optional data frame mapping each area to its region, with the `area`
#'   and `region` columns.
#' @param residents Optional data frame of the total population of each area, with the
#'   `area` column and a `count` column. When supplied, the difference between it and the
#'   implied private-household population is returned, which measures the people the
#'   published household counts do not cover.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{`margin`}{Long data frame of area, role and `count`, ready to pass to
#'       [Conditional_attribute_adder()] in its `margins` argument.}
#'     \item{`households`}{Long data frame of area, household type and `count`, the number
#'       of households of each type the area should end up with.}
#'     \item{`residual`}{Data frame of area, `residents`, `implied` and `residual`, or
#'       `NULL` when `residents` was not supplied.}
#'   }
#'
#' @details
#' The residual is worth reading rather than discarding. Household statistics normally cover
#' private households only, so people living in institutions appear as a shortfall spread
#' thinly across every area, plus a large shortfall in the few areas that contain an
#' institution. That makes the residual a usable spatial margin for a role of kind
#' `"excluded"`, and it identifies the areas where household formation should not be
#' attempted at all.
#'
#' @examples
#' spatial <- data.frame(
#'   area     = c("A", "A", "A", "B", "B", "B"),
#'   category = rep(c("one_person", "no_children", "with_children"), 2),
#'   count    = c(120, 80, 60, 200, 90, 140)
#' )
#' split <- data.frame(
#'   category       = c("one_person", "no_children", "with_children", "with_children"),
#'   household_type = c("single", "couple", "couple_kids", "single_parent"),
#'   share          = c(1, 1, 0.72, 0.28)
#' )
#' composition <- data.frame(
#'   household_type = c("single", "couple", "couple_kids", "couple_kids",
#'                      "single_parent", "single_parent"),
#'   role    = c("LivingAlone", "PartnerNoChildren", "PartnerWithChildren", "ChildAtHome",
#'               "SingleParent", "ChildAtHome"),
#'   members = c(1, 2, 2, 1.83, 1, 1.54)
#' )
#' household_person_margins(spatial, split, composition)
#'
#' @seealso [assign_households()], [household_structure()]
#' @importFrom stats aggregate setNames
#' @export
household_person_margins <- function(spatial_households, category_split, composition,
                                     structure = NULL,
                                     area = "area", category = "category",
                                     type = "household_type", role = "role",
                                     region = NULL, area_region = NULL,
                                     residents = NULL) {

    if (!all(c(area, category, "count") %in% colnames(spatial_households))) {
        stop("'spatial_households' needs columns '", area, "', '", category, "' and 'count'.")
    }
    if (!all(c(type, role, "members") %in% colnames(composition))) {
        stop("'composition' needs columns '", type, "', '", role, "' and 'members'.")
    }
    if (!is.null(structure)) {
        unknown <- setdiff(unique(as.character(composition[[role]])), structure$levels)
        if (length(unknown)) {
            stop("'composition' names role(s) the household structure does not declare: ",
                 paste(unknown, collapse = ", "), ".")
        }
    }

    sh <- spatial_households[, c(area, category, "count"), drop = FALSE]
    sh[[area]] <- as.character(sh[[area]])
    sh[[category]] <- as.character(sh[[category]])
    sh <- sh[!is.na(sh$count), , drop = FALSE]

    # Attach the region each area belongs to, so a split or composition that varies by
    # region reaches the right areas. Without a region column everything is global.
    if (!is.null(region)) {
        if (is.null(area_region) || !all(c(area, region) %in% colnames(area_region))) {
            stop("'region' was given, so 'area_region' must map '", area, "' to '", region, "'.")
        }
        lookup <- area_region[, c(area, region), drop = FALSE]
        lookup[[area]] <- as.character(lookup[[area]])
        sh <- merge(sh, lookup, by = area, all.x = TRUE)
        missing_region <- unique(sh[[area]][is.na(sh[[region]])])
        if (length(missing_region)) {
            warning("No region for ", length(missing_region), " area(s), e.g. ",
                    paste(utils::head(missing_region, 3), collapse = ", "),
                    ". Their households cannot be split by type and are dropped.",
                    call. = FALSE)
            sh <- sh[!is.na(sh[[region]]), , drop = FALSE]
        }
    }

    # Published categories to model household types.
    if (is.null(category_split)) {
        sh[[type]] <- sh[[category]]
        hh <- sh
    } else {
        cs <- category_split
        share_col <- if ("share" %in% colnames(cs)) "share" else
            if ("count" %in% colnames(cs)) "count" else
                stop("'category_split' needs a 'share' or 'count' column.")
        if (!all(c(category, type) %in% colnames(cs))) {
            stop("'category_split' needs columns '", category, "' and '", type, "'.")
        }
        by_cols <- c(if (!is.null(region) && region %in% colnames(cs)) region, category)
        cs <- cs[, c(by_cols, type, share_col), drop = FALSE]
        names(cs)[names(cs) == share_col] <- ".share"
        # Normalise within category, so published counts can be passed unscaled.
        key <- do.call(paste, c(cs[by_cols], sep = "\r"))
        cs$.share <- cs$.share / stats::ave(cs$.share, key, FUN = sum)
        hh <- merge(sh, cs, by = by_cols, all.x = TRUE)
        unsplit <- unique(hh[[category]][is.na(hh$.share)])
        if (length(unsplit)) {
            warning("No split given for published categor(ies) ",
                    paste(sQuote(unsplit), collapse = ", "),
                    "; they are treated as a household type of the same name.", call. = FALSE)
            hh[[type]][is.na(hh$.share)] <- hh[[category]][is.na(hh$.share)]
            hh$.share[is.na(hh$.share)] <- 1
        }
        hh$count <- hh$count * hh$.share
        hh$.share <- NULL
    }

    households <- stats::aggregate(
        stats::setNames(list(hh$count), "count"),
        by = c(stats::setNames(list(hh[[area]]), area),
               stats::setNames(list(hh[[type]]), type),
               if (!is.null(region)) stats::setNames(list(hh[[region]]), region)),
        FUN = sum, na.rm = TRUE)

    # Households to people, one role at a time.
    comp_by <- c(if (!is.null(region) && region %in% colnames(composition)) region, type)
    comp <- composition[, c(comp_by, role, "members"), drop = FALSE]
    persons <- merge(households, comp, by = comp_by, all.x = TRUE)
    no_comp <- unique(persons[[type]][is.na(persons$members)])
    if (length(no_comp)) {
        warning("No composition given for household type(s) ",
                paste(sQuote(no_comp), collapse = ", "),
                "; they contribute no people to the margin.", call. = FALSE)
        persons <- persons[!is.na(persons$members), , drop = FALSE]
    }
    persons$count <- persons$count * persons$members

    margin <- stats::aggregate(
        stats::setNames(list(persons$count), "count"),
        by = c(stats::setNames(list(persons[[area]]), area),
               stats::setNames(list(persons[[role]]), role)),
        FUN = sum, na.rm = TRUE)
    margin$count <- round(margin$count)

    residual <- NULL
    if (!is.null(residents)) {
        if (!all(c(area, "count") %in% colnames(residents))) {
            stop("'residents' needs columns '", area, "' and 'count'.")
        }
        res <- residents[, c(area, "count"), drop = FALSE]
        names(res)[names(res) == "count"] <- "residents"
        res[[area]] <- as.character(res[[area]])
        implied <- stats::aggregate(
            stats::setNames(list(margin$count), "implied"),
            by = stats::setNames(list(margin[[area]]), area), FUN = sum)
        residual <- merge(res, implied, by = area, all.x = TRUE)
        residual$implied[is.na(residual$implied)] <- 0
        residual$residual <- residual$residents - residual$implied
        residual$ratio <- ifelse(residual$residents > 0,
                                 residual$implied / residual$residents, NA_real_)

        overall <- sum(residual$implied) / sum(residual$residents)
        off <- sum(abs(residual$ratio - 1) > 0.25, na.rm = TRUE)
        message("Household margins cover ", round(100 * overall, 1),
                "% of the resident population across ", nrow(residual), " area(s). ",
                off, " area(s) differ by more than 25%, which normally means people ",
                "living outside private households; see the 'residual' element.")
    }

    households[[area]] <- as.character(households[[area]])
    households$count <- round(households$count)
    list(margin = margin,
         households = households[, c(area, type, "count"), drop = FALSE],
         residual = residual)
}
