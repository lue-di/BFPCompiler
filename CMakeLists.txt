cmake_minimum_required(VERSION 3.31)
project(BFplus C)

set(CMAKE_C_STANDARD 11)
add_subdirectory(brainfuck)
#add_executable(BFplus main.c)
add_executable(BFplus main.c)
