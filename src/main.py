import subprocess
import json

from enum import Enum


class MDFile:
    def __init__(self):
        self.classes: list[MDClass] = {}


class MDClass:
    def __init__(self, name: str, description: str, superclass: str = None):
        self.name: str = name
        self.properties: list[MDProperty] = []
        self.methods: list[MDMethod] = {}
        self.description: str = description
        self.superclass = superclass

    def __str__(self):
        if self.superclass is not None:
            return f"{self.name}({self.superclass})"
        return f"{self.name}"


class MDMethod:
    def __init__(self, name: str, description: str):
        self.name: str = name
        self.params = {}
        self.description: str = description

    def __str__(self):
        return f"{self.name}"

    def set_params(self, params: str) -> None:
        for param in params[1:-1].split(','):
            _, name, varType = param.split("<p>")
            self.params[name] = varType


class MDProperty:
    def __init__(self, name: str, description: str):
        self.name: str = name
        self.description: str = description

    def __str__(self):
        return f"{self.name}"


class PYFile:
    def __init__(self):
        self.classes = {}


class PYClass:
    def __init__(self, name: str):
        self.name: str = name
        self.properties: list[PYProperty] = []
        self.methods: list[PYMethod] = {}
        self.comments: list[str] = []

    def __str__(self):
        return f"{self.name}"


class PYMethod:
    def __init__(self, name: str):
        self.name: str = name
        self.params = {}
        self.comments: list[str] = []

    def __str__(self):
        return f"{self.name}"

    def set_params(self, params: str) -> None:
        # 1:-1 to remove [] and split to get each param
        for param in params[1:-1].split(','):
            _, name, type_ = param.split("<p>")
            self.params[name] = [type_]


class PYProperty:
    def __init__(self, name: str, type_: str, value: str):
        self.name: str = name
        self.type_: str = type_
        self.value = value


class AttrType(Enum):
    properties = 1;
    methods = 2;


class Parsnip:
    def __init__(self):
        self.md_file = MDFile()
        self.py_file = PYFile()

        self.attr_store_type = AttrType.properties;

        self.current_class = None
        self.current_scope = None

        md_data = self.parse_md_file("data/test.md")
        self.store_md_data(md_data)
        
        py_data = self.parse_py_file("data/test.py")
        self.store_py_data(py_data)

    def parse_md_file(self, filepath: str) -> list[str]:
        with open(filepath, "rb") as md:
            data = md.read();  # read in .md file
            # :-1 to remove trailing '\0'
            parsed_data = subprocess.check_output(["cabal", "exec", "parsnip", data, "md"], text=True)[:-1]
            md_data = json.loads(parsed_data)

            return md_data

    def parse_py_file(self, filepath: str) -> list[str]:
        with open(filepath, "rb") as py:
            data = py.read()
            parsed_data = subprocess.check_output(["cabal", "exec", "parsnip", data, "py"], text=True)[:-1]
            py_data = json.loads(parsed_data)

            return py_data

    def store_md_data(self, data: list[str]) -> None:
        for line in data:
            items = line.split("<s>")  # <s> is the custom delimiter
            item_type = items[0]       # get the first value of the list, which is the markdown type
            rest = items[1:]           # store remaining values

            if item_type in ["Class", "Subclass"]:
                class_ = MDClass(*rest)
                self.md_file.classes[rest[0]] = class_  # store the remaining data in a class [name, description, ?superclass]
                self.current_class = class_
                self.attr_store_type = AttrType.properties

            elif item_type == "Subheader":
                # checking whether we are about to read in properties or methods
                if rest[0] == "PROPERTIES:":
                    self.attr_store_type = AttrType.properties
                elif rest[0] == "METHODS:":
                    self.attr_store_type = AttrType.methods
                else:
                    # if neither properties or methods is read in, throw error
                    raise Exception(f"Unknown subheader [{rest[0]}] in .md file")

            elif (item_type == "CodeSnippet"):
                if self.attr_store_type == AttrType.properties:
                    self.current_class.properties.append(MDProperty(*rest))
                elif self.attr_store_type == AttrType.methods:
                    if rest[0] == "Function":
                        name = rest[1]
                        params = rest[2]
                        description = rest[3]

                        method = MDMethod(name, description)

                        self.current_class.methods[name] = method
                        method.set_params(params)

    def store_py_data(self, data: list[str]):
        for line in data:
            items = line.split("<s>")
            item_type = items[0]
            rest = items[1:]

            if item_type == "Class":
                class_ = PYClass(*rest)
                self.py_file.classes[rest[0]] = class_
                self.current_class = class_
                self.current_scope = class_

            elif item_type == "Function":
                name = rest[0]
                params = rest[1]
                method = PYMethod(name)
                method.set_params(params)

                self.current_class.methods[name] = method
                self.current_scope = method

            elif item_type == "Property":
                self.current_class.properties.append(PYProperty(*rest))

            elif item_type == "Comment":
                if self.current_scope is None:
                    continue

                comment = rest[0]
                self.current_scope.comments.append(comment)

    def match_files(self):
        for class_name, c in self.py_file.classes.items():
            if class_name not in self.md_file.classes:
                if not "!ignore" in c.comments:
                    print(f"[parsnip]: missing class     - {red(underline(c))}")
            try:
                md_c = self.md_file.classes[class_name]
            except KeyError:
                ...

            for method_name, m in c.methods.items():
                if m.name not in md_c.methods and m.name != "__init__":
                    if not "!ignore" in m.comments:
                        print(f"[parsnip]: missing method    - {red(c)}.{yellow(underline(m))}")

                try:
                    md_m = md_c.methods[method_name]
                
                    for param in m.params:
                        if param not in md_m.params:
                            print(f"[parsnip]: missing parameter - {red(c)}.{yellow(m)}({green(underline(param))})")
                except KeyError:
                    ...



def red(s: str): return f"\x1b[;31m{s}\x1b[;39m"
def green(s: str): return f"\x1b[;32m{s}\x1b[;39m"
def yellow(s: str): return f"\x1b[;33m{s}\x1b[;39m"
def blue(s: str): return f"\x1b[;34m{s}\x1b[;39m"
def pink(s: str): return f"\x1b[;35m{s}\x1b[;39m"
def cyan(s: str): return f"\x1b[;36m{s}\x1b[;39m"

def red_bg(s: str): return f"\x1b[0;41m{s}\x1b[0;39m"
def green_bg(s: str): return f"\x1b[0;42m{s}\x1b[0;39m"
def yellow_bg(s: str): return f"\x1b[0;43m{s}\x1b[0;39m"
def blue_bg(s: str): return f"\x1b[0;44m{s}\x1b[0;39m"
def pink_bg(s: str): return f"\x1b[0;45m{s}\x1b[0;39m"
def cyan_bg(s: str): return f"\x1b[0;46m{s}\x1b[0;39m"

def bold(s: str): return f"\x1b[1;29m{s}\x1b[0;39m"
def dim(s: str): return f"\x1b[2;29m{s}\x1b[0;39m"
def italic(s: str): return f"\x1b[3;29m{s}\x1b[0;39m"
def underline(s: str): return f"\x1b[4;29m{s}\x1b[0;39m"


if __name__ == "__main__":
    Parsnip().match_files()

