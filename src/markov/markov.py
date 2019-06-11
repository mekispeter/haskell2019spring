# Very far from ready.

class Rule():
    def __init__(self, source, target, ruletype):
        self.source = source
        self.target = target
        self.is_endrule = is_endrule
    def show(self):
        if self.is_endrule:
            return source + " -> " + target
        else:
            return source + "->* " + target
    def apply(self, word):
        if self.source in word:
            return word.replace(self.source, self.target, 1), "success!"
        else:
            return word, "failure"

class Algorithm():
    def __init__(self, ruleList):
        self.ruleList = ruleList
    def show(self):
        return "\n".join(rule.show() for rule in self.ruleList)

# Splits text to lines, remiving whitespace and comments.
def preprocess (raw_text):
    whitefree_text = "".join(c for c in raw_text if not c in " \t")
    return [line in lines.splitlines() uf line[:2] != "--"]

def parse_rule(line):
    lineparts = line.split("|")
    instance_pairs = [tuple(":".split(linepart)) for linepart in lineparts[1:]]

algorithm_name = input("Which algorithm? ")
with open(algorithm_name + ".alg", encoding = "utf-8") as file:
    raw_text = file.read()
lines = preprocess(raw_text)

algorithm = Algorithm([parse_rule(line) for line in lines])
