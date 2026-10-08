# Specification objects describing how a population's household roles fit together.
#
# Nothing here is specific to one country's statistics. A role is whatever value the
# population carries in its household-position column, and the specification says what
# that role does when households are formed: live alone, form the adult core of a
# household, join one as a child, attach to one as an extra member, or stay out of the
# process entirely. Everything downstream - the count reconciliation, the matching, the
# verification - reads the structure rather than hard-coded category names.

#' Describe One Household Role
#'
#' Declares what a single value of the population's household-position column means when
#' households are formed. Roles are collected into a [household_structure()].
#'
#' @param level The value as it appears in the population's role column, for example
#'   `"PartnerWithChildren"`. One string.
#' @param kind What the role does when households are formed. One of:
#'   \describe{
#'     \item{`"partner"`}{Forms the adult core of a household. `n_adults = 1` is a person
#'       living alone or a lone parent; `n_adults = 2` triggers couple matching.}
#'     \item{`"child"`}{Joins a household whose adult core has one of the types named in
#'       `type`. Grouped into sibling sets and matched to an adult core.}
#'     \item{`"attached"`}{Joins an already-formed household as an extra member, used to
#'       reach a published mean household size.}
#'     \item{`"excluded"`}{Left out of household formation altogether, for people who do
#'       not live in a private household.}
#'   }
#' @param type The household type this role belongs to. For `kind = "partner"` exactly one
#'   type; for `kind = "child"` one or more types whose adult cores may host the child.
#'   Ignored for `"attached"` and `"excluded"`.
#' @param n_adults Number of people of this role in one household. Only meaningful for
#'   `kind = "partner"`, where it must be 1 or 2.
#'
#' @return A `gsp_hh_role` object.
#'
#' @examples
#' hh_role("LivingAlone", kind = "partner", type = "single", n_adults = 1)
#' hh_role("ChildAtHome", kind = "child", type = c("couple_kids", "single_parent"))
#'
#' @seealso [household_structure()]
#' @export
hh_role <- function(level, kind = c("partner", "child", "attached", "excluded"),
                    type = NULL, n_adults = 1) {
    kind <- match.arg(kind)
    if (length(level) != 1 || is.na(level)) {
        stop("'level' must be a single non-missing value.")
    }
    if (kind %in% c("partner", "child") && is.null(type)) {
        stop("Role '", level, "' is of kind '", kind, "' and needs a 'type'.")
    }
    if (kind == "partner") {
        if (length(type) != 1) {
            stop("Role '", level, "' is a partner role, so 'type' must name exactly one ",
                 "household type.")
        }
        if (!n_adults %in% c(1, 2)) {
            stop("Role '", level, "' has n_adults = ", n_adults,
                 "; only 1 or 2 are supported.")
        }
    }
    structure(list(level = as.character(level), kind = kind,
                   type = if (is.null(type)) character(0) else as.character(type),
                   n_adults = if (kind == "partner") as.integer(n_adults) else NA_integer_),
              class = "gsp_hh_role")
}

#' Collect Household Roles Into a Structure
#'
#' Gathers [hh_role()] declarations into the specification that [assign_households()] and
#' [household_person_margins()] work from, and checks that they describe a consistent set
#' of households: every child role points at a household type that some partner role
#' actually forms, and no household type is claimed by two partner roles.
#'
#' @param ... One or more [hh_role()] objects.
#'
#' @return A `gsp_hh_structure` object: a list of roles plus lookup tables from role level
#'   to kind, household type and adult count.
#'
#' @details
#' The structure is what makes the household functions portable. A Dutch population
#' carrying CBS household positions and a population carrying any other classification
#' differ only in the levels named here.
#'
#' @examples
#' structure_nl <- household_structure(
#'   hh_role("LivingAlone",           kind = "partner",  type = "single",        n_adults = 1),
#'   hh_role("PartnerNoChildren",     kind = "partner",  type = "couple",        n_adults = 2),
#'   hh_role("PartnerWithChildren",   kind = "partner",  type = "couple_kids",   n_adults = 2),
#'   hh_role("SingleParent",          kind = "partner",  type = "single_parent", n_adults = 1),
#'   hh_role("ChildAtHome",           kind = "child",
#'           type = c("couple_kids", "single_parent")),
#'   hh_role("OtherHouseholdMember",  kind = "attached"),
#'   hh_role("InstitutionalHousehold", kind = "excluded")
#' )
#'
#' @seealso [hh_role()], [assign_households()]
#' @export
household_structure <- function(...) {
    roles <- list(...)
    if (length(roles) == 1 && is.list(roles[[1]]) && !inherits(roles[[1]], "gsp_hh_role")) {
        roles <- roles[[1]]
    }
    if (!length(roles) || !all(vapply(roles, inherits, logical(1), "gsp_hh_role"))) {
        stop("household_structure() takes hh_role() objects.")
    }

    levels <- vapply(roles, function(r) r$level, character(1))
    if (anyDuplicated(levels)) {
        stop("Duplicate role level(s): ",
             paste(unique(levels[duplicated(levels)]), collapse = ", "))
    }
    names(roles) <- levels

    kinds <- vapply(roles, function(r) r$kind, character(1))
    partner_types <- unlist(lapply(roles[kinds == "partner"], function(r) r$type))
    if (anyDuplicated(partner_types)) {
        stop("Household type(s) claimed by more than one partner role: ",
             paste(unique(partner_types[duplicated(partner_types)]), collapse = ", "),
             ". Each household type needs exactly one adult core.")
    }
    child_types <- unlist(lapply(roles[kinds == "child"], function(r) r$type))
    orphan <- setdiff(child_types, partner_types)
    if (length(orphan)) {
        stop("Child role(s) point at household type(s) with no partner role: ",
             paste(orphan, collapse = ", "), ".")
    }

    structure(list(
        roles         = roles,
        levels        = levels,
        kind_of       = stats::setNames(kinds, levels),
        type_of       = stats::setNames(lapply(roles, function(r) r$type), levels),
        adults_of     = stats::setNames(vapply(roles, function(r) r$n_adults, integer(1)),
                                        levels),
        partner_types = unname(partner_types)
    ), class = "gsp_hh_structure")
}

#' @export
print.gsp_hh_structure <- function(x, ...) {
    cat("<household structure: ", length(x$roles), " roles>\n", sep = "")
    for (r in x$roles) {
        detail <- switch(r$kind,
            partner  = paste0("forms '", r$type, "' with ", r$n_adults, " adult(s)"),
            child    = paste0("child in ", paste0("'", r$type, "'", collapse = " or ")),
            attached = "attaches to an existing household",
            excluded = "not placed in a household")
        cat(sprintf("  %-24s %-9s %s\n", r$level, paste0("[", r$kind, "]"), detail))
    }
    invisible(x)
}

# Roles of a given kind, as a character vector of levels.
roles_of_kind <- function(structure, kind) {
    structure$levels[structure$kind_of[structure$levels] %in% kind]
}

# The partner role that forms a given household type.
partner_role_for <- function(structure, type) {
    for (r in structure$roles) {
        if (r$kind == "partner" && identical(r$type, type)) return(r$level)
    }
    NA_character_
}

# The child roles that may live in a given household type.
child_roles_for <- function(structure, type) {
    keep <- vapply(structure$roles,
                   function(r) r$kind == "child" && type %in% r$type, logical(1))
    structure$levels[keep]
}
