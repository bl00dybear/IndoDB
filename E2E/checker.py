import filecmp
import sys
from pathlib import Path

TESTS_ROOT = Path(__file__).parent / "tests"



def run_checks():
    failed = []
    all_tests = sorted(TESTS_ROOT.glob("test_*"))

    for td in all_tests:
        expected = td / "expected_output.txt"
        output = td /"output.txt"

        if not expected.exists() or not output.exists():
            print(f"[{td.name}] MISSING FILE(S)")
            failed.append(td)
            continue
        
        identical = filecmp.cmp(expected, output, shallow=False)
        if identical:
            print(f"[{td.name}] OK")
        else:
            print(f"[{td.name}] FAIL")
            failed.append(td)
    
    print("\nSummary:")
    print(f" Trecut: {len(all_tests) - len(failed)}")
    print(f" Failed: {len(failed)}")
    return failed

def main():
    if not TESTS_ROOT.is_dir():
        print(f"Error: directory '{TESTS_ROOT}' nu a fost gasit.", file = sys.stderr)
        sys.exit(2)

    failed = run_checks()
    sys.exit(1 if failed else 0)

main()