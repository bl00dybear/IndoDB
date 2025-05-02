cmake_minimum_required(VERSION 3.10)
project(IndoDB C)

# Setează standardul C
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0")


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

# Setează directorul de ieșire
set_target_properties(indodb PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/output"
)

# Creează directoarele necesare pentru baza de date
file(MAKE_DIRECTORY "${CMAKE_SOURCE_DIR}/database")
file(MAKE_DIRECTORY "${CMAKE_SOURCE_DIR}/output")

# Comandă post-build pentru a verifica dacă există fișierele de bază de date
add_custom_command(TARGET indodb POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E echo "Checking database directory structure..."
        COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_SOURCE_DIR}/database"
)