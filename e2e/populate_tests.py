"""
populate_tests.py

Muta automat perechi <SQL> / <expected output> din bulk_input.txt 
in fisierele corespunzatoare din `tests/test_XX/`.

Structura fișierului bulk:
    bloc SQL
    [linie goala]
    bloc OUTPUT
    [linie goala]
    bloc SQL
    [linie goala]
    bloc OUTPUT
    ...

Orice linie complet goala separa blocurile.
"""

from pathlib import Path
import sys

BULK_FILE = "bulk_input.txt"   #fisierul cu textul
TESTS_DIR = "tests"            #folderul cu teste
SKIP_EXISTING = False #true => nu rescrie fisierul daca exista deja


def split_blocks(text):
    #delimitam textul dupa liniile goale
    blocks, current = [], []
    for line in text.splitlines():
        if line.strip() == "":
            if current:
                blocks.append("\n".join(current).rstrip() + "\n")
                current = []
        else:
            current.append(line)
    if current:
        blocks.append("\n".join(current).rstrip() + "\n")
    return blocks


def main():
    root = Path(__file__).resolve().parent
    bulk_path = root / BULK_FILE
    tests_path = root / TESTS_DIR

    if not bulk_path.exists():
        print(f"Fisierul '{bulk_path}' nu exista.", file=sys.stderr)
        sys.exit(1)

    tests_path.mkdir(exist_ok=True)

    blocks = split_blocks(bulk_path.read_text(encoding="utf-8"))

    if len(blocks) % 2 != 0:
        print("Numar impar de blocuri - lipseste un OUTPUT pentru un SQL.",
              file=sys.stderr)
        sys.exit(1)

    num_pairs = len(blocks) // 2
    width = 2 

    for idx in range(num_pairs):
        sql_block = blocks[2 * idx]
        exp_block = blocks[2 * idx + 1]

        test_dir = tests_path / f"test_{idx:0{width}d}"
        test_dir.mkdir(parents=True, exist_ok=True)

        in_file = test_dir / "input.txt"
        out_file = test_dir / "expected_output.txt"

        if (in_file.exists() or out_file.exists()) and SKIP_EXISTING:
            print(f"{test_dir.name} exista deja — skip")
            continue

        try:
            in_file.write_text(sql_block, encoding="utf-8")
            out_file.write_text(exp_block, encoding="utf-8")
            print(f"[+] Populat {test_dir.name}")
        except Exception as exc:
            print(f"[!] Eroare la scriere în {test_dir}: {exc}", file=sys.stderr)


main()
