cmake_minimum_required(VERSION 3.6)
project(compiler)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_COMPILER "/usr/bin/clang")
set(CMAKE_CXX_COMPILER "/usr/bin/clang++")

add_compile_options(-Wall -fjump-tables -O3)

set(SOURCE_FILES src/main.cpp src/shader_processor.cpp)

add_executable(${PROJECT_NAME} ${SOURCE_FILES} ${HEADER_FILES})

target_link_libraries(${PROJECT_NAME}
    /usr/lib/x86_64-linux-gnu/libstdc++.so.6
)
