add_executable(example example.cpp strncmp_debug.cpp)
target_link_libraries(example PRIVATE ${PROJECT_NAME})
#target_compile_options(
#    example PRIVATE
#    -Wall
#    -Wextra
#    -pedantic
#    -Wshadow
#    -Wdouble-promotion
#    -Wnull-dereference
#)

add_test(NAME example COMMAND example)
