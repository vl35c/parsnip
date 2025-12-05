import subprocess


with open ("data/test.md", "rb") as file:
    data = file.read();

    v = subprocess.check_output(["cabal", "exec", "parsnip", data], text=True)
    print(v)
