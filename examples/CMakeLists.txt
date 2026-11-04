# Read example
add_executable(read_wpsim_example read_wpsim_example.F90)
target_link_libraries(read_wpsim_example PRIVATE ${PROJECT_NAME})

# Write example
add_executable(write_wpsim_example write_wpsim_example.F90)
target_link_libraries(write_wpsim_example PRIVATE ${PROJECT_NAME})

# Register tests
add_test(
  NAME read_wpsim_example
  COMMAND read_wpsim_example
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_test(
  NAME write_wpsim_example
  COMMAND write_wpsim_example
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)