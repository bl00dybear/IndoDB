cmake_minimum_required(VERSION 3.10)
project(IndoDB C)

# Setează standardul C
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Adaugă directorul src pentru a găsi header-urile
include_directories(
        ${CMAKE_SOURCE_DIR}/src
        ${CMAKE_SOURCE_DIR}/src/rows_btree/headers/
)

# Caută toate fișierele sursă în directorul src
file(GLOB_RECURSE SOURCES "${CMAKE_SOURCE_DIR}/src/*.c")

# Definește executabilul principal
add_executable(indodb ${SOURCES})

# Opțional: adaugă flag-uri de compilare
target_compile_options(indodb PRIVATE -Wall -Wextra)

# Opțional: setează directorul de ieșire
set_target_properties(indodb PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/output"
)

# Dacă ai nevoie de biblioteci externe
# find_package(SomeLibrary REQUIRED)
# target_link_libraries(indodb PRIVATE SomeLibrary)