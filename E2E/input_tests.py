"""
input_tests.py
creeaza in fisierul tests cate teste vrei
exemplu:
 tests
    ├── test_0
    │   ├── error.txt
    │   ├── expected_output.txt
    │   ├── input.txt
    │   └── output.txt
si tot asa test_1 etc
"""

from pathlib import Path
import sys

NUM_TESTS = 100
BASE_DIR = "tests"
SKIP_EXISTING = False

DEFAULT_FILENAMES = [
    "input.txt",
    "expected_output.txt",
    "output.txt",
    "error.txt",
]

def create_files(test_dir: Path):
    """
    Creeaza fisierele goale
    """
    for name in DEFAULT_FILENAMES:
        fp = test_dir / name
        if not fp.exists():
            try:
                fp.write_text("", encoding = "utf-8")
            except Exception as exc:
                print(f"Eroare la crearea {fp}: {exc}", file=sys.stderr)

def main():
    base_path = (Path(__file__).resolve().parent / BASE_DIR).resolve()
    base_path.mkdir(parents=True, exist_ok=True)

    for i in range(NUM_TESTS):
        test_name = f"test_{i}"
        test_dir = base_path / test_name

        if test_dir.exists():
            if SKIP_EXISTING:
                print(f"{test_name} deja exista — skip.")
                continue
                else:
            test_dir.mkdir(parents=True)

        create_empty_files(test_dir)
        print(f"Generat {test_name}")
 
 print("GATA")

main()