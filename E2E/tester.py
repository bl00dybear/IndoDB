import subprocess
import os
import time
import select
import contextlib

CLI_BIN = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "output", "indodb")
)
PROMPT = "indodb> "
READ_TIMEOUT = 15 #secunde

def read_file(path):
    """
    Citeste continutul unui fisier la calea specificata.
    :param path: Calea fisierului.
    :return: Continutul fisierului.
    """
    with open(path, 'r', encoding="utf8") as file:
        return file.read()

def write_file(path, content):
    """
    Scrie continutul intr-un fisier la calea specificata.
    :param path: Calea fisierului.
    :param content: Continutul de scris in fisier.
    """
    with open(path, 'w', encoding ="utf8") as file:
        file.write(content)

def continut_to_list(continut):
    """
    Converteste un continut de tip string intr-o lista de linii.
    :param continut: Continutul de convertit
    :return: Lista de linii
    """
    comenzi = continut.split(";")
    comenzi = [comanda.strip() for comanda in comenzi if comanda.strip()]
    return comenzi

def run_tests(root: str = "tests", sleep_sec: float = 0.05):
    """
    Executa fiecare test din test_* si trimite
    toate fisierele din input.txt intr-un singur
    proces IndoDB
    nu se astapta raspunsul cu promptul
    este doar un sleep de 0.05 sec
    rezultatul este scris in output.txt
    si error.txt
    """


    test_dirs = sorted(
        d for d in os.listdir(root)
        if d.startswith("test_")
            and len(d) == 7          # 5 caractere „test_” + 2 cifre
            and d[5:].isdigit()
            and os.path.isdir(os.path.join(root, d))
    )

    for td in test_dirs:
        print(td)
        dir_path = os.path.join(root, td)
        input_file = os.path.join(dir_path, "input.txt")
        output_file = os.path.join(dir_path, "output.txt")
        error_file = os.path.join(dir_path, "error.txt")
        

        sql = read_file(input_file)
        comenzi = continut_to_list(sql)

        proc = subprocess.Popen(
            [CLI_BIN],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0,
        )



        try:
            for cmd in comenzi:

                line = cmd.strip()
                if line.lower() in {"exit"}:
                    continue
                if not line.endswith(";"):
                    line += ";"
                proc.stdin.write(line + '\n')
                proc.stdin.flush()
                time.sleep(sleep_sec)

            proc.stdin.write("exit\n")
            proc.stdin.flush()

            stdout, stderr = proc.communicate()

            lines = stdout.splitlines(keepends=True)
            # delete the last 2 lines
            stdout = ''.join(lines[:-2]) if len(lines) > 2 else ''.join(lines)

            # remove every line that starts with the prompt
            stdout = ''.join(line for line in stdout.splitlines(keepends=True) if not line.startswith(PROMPT))
            stderr = ''.join(line for line in stderr.splitlines(keepends=True) if not line.startswith(PROMPT))
            

            write_file(output_file, stdout)
            write_file(error_file, stderr)

        finally:
            with contextlib.suppress(Exception):
                proc.kill()
# main("tests")