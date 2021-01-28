test_that("basic functionality", {
  expect_equal(
    prep_data(
      tibble::tibble(
        time = as.POSIXct(lubridate::today()),
        author = "Chatty Chatter",
        text = "Well it is,,,  cloudy  today! 😂",
        source = "some/path",
        emoji = "😂",
        emoji_name = "face with tears of joy"
      )
    ),
    tibble::tibble(
      time = as.POSIXct(lubridate::today()),
      author = "Chatty Chatter",
      text = "Well it is,,,  cloudy  today! 😂",
      emoji = "😂",
      emoji_name = "face with tears of joy",
      n_words = 5,
      n_emojis = 1,
      wday = lubridate::wday(lubridate::today())
    )
  )
  expect_error(
    prep_data(
      tibble::tibble(
        time = as.POSIXct(lubridate::today()),
        author = "Chatty Chatter",
        text = "Well it is,,,  cloudy  today! 😂",
        source = "some/path",
        emoji = "😂"
      )
    )
  )
  expect_error(
    prep_data(
      list(
        time = as.POSIXct(lubridate::today()),
        author = "Chatty Chatter",
        text = "Well it is,,,  cloudy  today! 😂",
        source = "some/path",
        emoji = "😂",
        emoji_name = "face with tears of joy"
      )
    )
  )
})
