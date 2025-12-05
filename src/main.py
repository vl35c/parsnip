import subprocess
import json

from enum import Enum


class MDFile:
    def __init__(self):
        self.classes: list[MDClass] = []


class MDClass:
    def __init__(self, name: str, description: str, superclass: str = None):
        self.name: str = name
        self.properties: list[MDProperty] = []
        self.methods: list[MDMethod] = []
        self.description: str = description
        self.superclass = superclass

    def __str__(self):
        if self.superclass is not None:
            return f"{self.name}({self.superclass})"
        return f"{self.name}"


class MDMethod:
    def __init__(self, name: str, description: str):
        self.name: str = name
        self.params = []
        self.description: str = description

    def __str__(self):
        return f"{self.name}"


class MDProperty:
    def __init__(self, name: str, description: str):
        self.name: str = name
        self.description: str = description

    def __str__(self):
        return f"{self.name}"


class AttrType(Enum):
    properties = 1;
    methods = 2;


class Parsnip:
    def __init__(self):
        self.md_file = MDFile()

        self.attr_store_type = AttrType.properties;

        md_data = self.parse_md_file("data/test.md")
        self.store_md_data(md_data)

    def parse_md_file(self, filepath: str) -> list[str]:
        with open (filepath, "rb") as md:
            data = md.read();  # read in .md file
            # :-1 to remove trailing '\0'
            parsed_data = subprocess.check_output(["cabal", "exec", "parsnip", data], text=True)[:-1] 
            md_data = json.loads(parsed_data)

            return md_data

    def store_md_data(self, data: list[str]) -> None:
        for line in data:
            items = line.split("<s>")  # <s> is the custom delimiter
            item_type = items[0]       # get the first value of the list, which is the markdown type
            rest = items[1:]           # store remaining values

            if item_type == "Class":
                self.md_file.classes.append(MDClass(*rest))  # store the remaining data in a class [name, description]

            if item_type == "Subclass":
                self.md_file.classes.append(MDClass(*rest))

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
                    # add to most recent class
                    self.md_file.classes[-1].properties.append(MDProperty(*rest))
                elif self.attr_store_type == AttrType.methods:
                    self.md_file.classes[-1].methods.append(MDMethod(*rest))


if __name__ == "__main__":
    app = Parsnip()

    for c in app.md_file.classes:
        print(c)

