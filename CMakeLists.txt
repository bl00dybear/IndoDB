cmake_minimum_required(VERSION 3.10)
project(IndoDB C)

# Setează standardul C
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Setează build-ul pe Debug pentru Valgrind
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_C_FLAGS_DEBUG "-g -O0")

# Adaugă directorul src pentru a găsi header-urile
include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/src/rows_btree/headers/
    ${CMAKE_SOURCE_DIR}/include
)

# Caută toate fișierele sursă în directorul src, cu excepția celor din parser
file(GLOB_RECURSE SOURCES "${CMAKE_SOURCE_DIR}/src/*.c")
list(FILTER SOURCES EXCLUDE REGEX ".*parser/.*")

# Definește executabilul principal
add_executable(indodb ${SOURCES})

# Adaugă warning-uri + flaguri debug
target_compile_options(indodb PRIVATE -Wall -Wextra -g -O0)

# Setează directorul de ieșire
set_target_properties(indodb PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/output"
)

# Creează directoarele necesare pentru baza de date
file(MAKE_DIRECTORY "${CMAKE_SOURCE_DIR}/databases")
file(MAKE_DIRECTORY "${CMAKE_SOURCE_DIR}/output")

# Găsește Cabal pentru compilare
find_program(CABAL cabal)

if(CABAL)
    file(WRITE ${CMAKE_BINARY_DIR}/compile_haskell.sh
        "#!/bin/bash\n"
        "set -e\n"
        "rm -f ${CMAKE_SOURCE_DIR}/src/parser/*.o\n"
        "rm -f ${CMAKE_SOURCE_DIR}/src/parser/*.hi\n"
        "cd ${CMAKE_SOURCE_DIR}/src/parser\n"
        "${CABAL} v2-build\n"
        "EXEC_PATH=\$(find dist-newstyle -name sql_parser -type f -executable | head -n 1)\n"
        "if [ -n \"\$EXEC_PATH\" ]; then\n"
        "  cp \"\$EXEC_PATH\" ${CMAKE_SOURCE_DIR}/output/\n"
        "  echo \"Compilare finalizată cu succes și executabilul a fost copiat în output/\"\n"
        "else\n"
        "  echo \"Eroare: Executabilul sql_parser nu a fost găsit în dist-newstyle/\"\n"
        "  exit 1\n"
        "fi\n"
    )

    execute_process(COMMAND chmod +x ${CMAKE_BINARY_DIR}/compile_haskell.sh)

    add_custom_command(
        OUTPUT ${CMAKE_SOURCE_DIR}/output/sql_parser
        COMMAND ${CMAKE_BINARY_DIR}/compile_haskell.sh
        DEPENDS
            ${CMAKE_SOURCE_DIR}/src/parser/Main.hs
            ${CMAKE_SOURCE_DIR}/src/parser/sql-parser.cabal
        COMMENT "Compiling Haskell SQL parser with Cabal"
    )

    add_custom_target(sql_parser ALL
        DEPENDS ${CMAKE_SOURCE_DIR}/output/sql_parser
    )

    add_dependencies(indodb sql_parser)

else()
    # Încearcă să găsească GHC ca alternativă
    find_program(GHC ghc)
    if(GHC)
        message(WARNING "Cabal not found, falling back to GHC. This may not work correctly with the cabal file.")
        file(WRITE ${CMAKE_BINARY_DIR}/compile_haskell.sh
            "#!/bin/bash\n"
            "set -e\n"
            "rm -f ${CMAKE_SOURCE_DIR}/src/parser/*.o\n"
            "rm -f ${CMAKE_SOURCE_DIR}/src/parser/*.hi\n"
            "cd ${CMAKE_SOURCE_DIR}/src/parser\n"
            "${GHC} -O2 -o ${CMAKE_SOURCE_DIR}/output/sql_parser Main.hs -XOverloadedStrings -XDeriveGeneric -XDeriveAnyClass \\\n"
            "  -hide-all-packages \\\n"
            "  -package base \\\n"
            "  -package aeson \\\n"
            "  -package parsec \\\n"
            "  -package bytestring\n"
            "echo \"Compilare finalizată cu succes\"\n"
        )

        execute_process(COMMAND chmod +x ${CMAKE_BINARY_DIR}/compile_haskell.sh)

        add_custom_command(
            OUTPUT ${CMAKE_SOURCE_DIR}/output/sql_parser
            COMMAND ${CMAKE_BINARY_DIR}/compile_haskell.sh
            DEPENDS ${CMAKE_SOURCE_DIR}/src/parser/Main.hs
            COMMENT "Compiling Haskell SQL parser with GHC (fallback)"
        )

        add_custom_target(sql_parser ALL
            DEPENDS ${CMAKE_SOURCE_DIR}/output/sql_parser
        )

        add_dependencies(indodb sql_parser)
    else()
        message(WARNING "Neither Cabal nor GHC found. SQL parser will not be compiled.")
    endif()
endif()

# Comandă post-build pentru a verifica dacă există fișierele de bază de date
add_custom_command(TARGET indodb POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E echo "Checking database directory structure..."
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_SOURCE_DIR}/databases"
)
