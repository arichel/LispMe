cmake_minimum_required(VERSION 3.2)
project(lispme C)

file(GLOB SOURCES "src/*.c")

add_executable(${PROJECT_NAME} ${SOURCES})

find_library(MATH_LIB m)

target_compile_options(${PROJECT_NAME} PRIVATE -Wall)
target_include_directories(${PROJECT_NAME} PUBLIC "${PROJECT_SOURCE_DIR}/include")
target_compile_definitions(${PROJECT_NAME} PRIVATE MAIN_REPL) # DUMP_STEPS)
target_link_libraries(${PROJECT_NAME} PRIVATE ${MATH_LIB})
