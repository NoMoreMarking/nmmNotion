#' Creates a new notion page from a parent page
#'
#' @export
#' @param basePage The base page id.
#' @param pageTitle The title of the new page.
#' @param notionKey Secret notion key
#' @return The page id
#'
createPage <- function (basePage, pageTitle, notionKey) {

  url <- "https://api.notion.com/v1/pages"
  parent <- list(type = "page_id",page_id = basePage)

  properties <- list(
    type = "title",
    title = list(
      list(
        type = "text",
        text = list(
          content = paste(pageTitle,today())
        )
      )
    )
  )

  r <- httr::POST(
    url,
    body = list(parent=parent, properties=properties),
    encode = "json",
    httr::add_headers("Authorization" = paste0('Bearer ',notionKey), "Notion-Version" = "2021-05-13"))

  parsed <- jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = FALSE)
  return (parsed$id)
}

#' Updates a notion page with new content
#'
#' @export
#' @param pageid The page id
#' @param color The text colour
#' @param content The text to add to the page
#' @param notionKey Secret notion key
#' @return A parsed response object
#'
updatePage <- function(pageid, color, content, notionKey){

  patchUrl <- paste0('https://api.notion.com/v1/blocks/',pageid,'/children')

  patchChildren <- list(
    list(
      object = "block",
      type = "paragraph",
      paragraph = list(
        text = list(
          list(
            type = "text",
            annotations = list(bold=TRUE,color=color),
            text = list(
              content = content
            )
          )
        )
      )
    )
  )

  r <- httr::PATCH(
    patchUrl,
    body = list(children=patchChildren),
    encode = "json",
    httr::add_headers("Authorization" = paste0('Bearer ',notionKey), "Notion-Version" = "2021-05-13"))

  parsed <- jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = FALSE)
  return(parsed)
}
