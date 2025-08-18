test('server logic works correctly', {
  expect_equal(server_function(input = 1), expected_output)
  expect_error(server_function(input = invalid_input), "Expected error message")
})