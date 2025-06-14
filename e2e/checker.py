import filecmp
import sys
from pathlib import Path
from tester import run_tests
import os

TESTS_ROOT = Path(__file__).parent / "tests"

def normalize_ascii(path: str) -> str:
    with open(path, encoding="utf8", errors="ignore") as f:
        data = f.read()
    return "".join(ch for ch in data if 32 <= ord(ch) <= 126)


def run_checks():
    failed = []
    all_tests = sorted(TESTS_ROOT.glob("test_*"))

    # first let s run the tests to generate the outputs
    print("Running tests to generate outputs...")
    run_tests("tests")
    print("Done.\n")
    print("Comparing expected outputs with actual outputs...\n")

    for td in all_tests:
        expected = td / "expected_output.txt"
        output = td /"output.txt"

        if not expected.exists() or not output.exists():
            print(f"[{td.name}] MISSING FILE(S)")
            failed.append(td)
            continue
        
        identical = normalize_ascii(expected) == normalize_ascii(output)
        if identical:
            print(f"[{td.name}] OK")
        else:
            print(f"[{td.name}] FAIL")
            failed.append(td)
    
    print("\nSummary:")
    print(f" Passed: {len(all_tests) - len(failed)}")
    print(f" Failed: {len(failed)}")
    return failed

def main():
    # -rd argument optional
    remove_db = False
    if len(sys.argv) > 1 and sys.argv[1] == "-rd":
        remove_db = True
    
    if remove_db:
        print("Removing all databases...")
        # remove all folders from the ../databases
        cmd = "rm -rf ../databases/*"
        os.system(cmd)

    if not TESTS_ROOT.is_dir():
        print(f"Error: directory '{TESTS_ROOT}' nu a fost gasit.", file = sys.stderr)
        sys.exit(2)

    failed = run_checks()
    sys.exit(1 if failed else 0)

main()