output$sliderBox <-
  renderUI({
    div(
      id = "scoreSlider",
      box(
        status = "warning",
        width = 12,
        selectizeInput(
          inputId = "angle",
          label = "angle du vue",
          choices = c("lateral",
                      "arriere",
                      "angle-arriere"),
          selected = "side",
          width = "100%"
        ),
        selectizeInput(
          inputId = "sex",
          label = "sexe",
          choices = c("femelle", "male", "inconnu"),
          selected = "inconnu",
          width = "100%"
        ),
        selectizeInput(
          inputId = "age",
          label = "age",
          choices = c("adulte", "immature", "bebe"),
          selected = "adulte",
          width = "100%"
        ),
        selectizeInput(
          inputId = "sick",
          label = "blesse ou malade",
          choices = c("non", "oui"),
          selected = "no",
          width = "100%"
        ),
        sliderInput(
          inputId = "nEles",
          "nombre d'individus",
          min = 1,
          max = 10,
          value = 1
        ),
        sliderInput(
          inputId = "sliderScore",
          label = "score de condition physique",
          min = 1,
          max = 10,
          value = 1,
          step = 1,
          animate = animationOptions(interval = 500, loop = TRUE)
        ),
        # Images to display
        lapply(
          X = seq_len(length(list.files(
            "./www/refImages/"
          ))),
          FUN = function(k) {
            # condition on the slider value
            conditionalPanel(
              condition = paste0("input.sliderScore == ", k),
              img(
                src = paste0("refImages/", letters[k], ".jpg"),
                width = "100%",
                height = "100%"
              )
            )
          }
        ),
        br(),
        actionButton(
          inputId = "undoButton",
          label = "annuler",
          width = "100%",
          style = "color: #fff; background-color: #4f4f4f; border-color: #141414"
        ),
        br(),
        br(),
        actionButton(
          inputId = "scoreButton",
          label = "sauvegarder et continuer",
          width = "100%",
          style = "color: #fff; background-color: #4f4f4f; border-color: #141414"
        )
      )
    )
  })
