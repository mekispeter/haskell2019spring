# A Python3 script that creates random patterns for the Game of Life project.

import random as r

def generate_character(probability):
    if r.random() <= probability:
        return "#"
    else:
        return "."

def generate_line(length, probability):
    line = [generate_character(probability) for _ in range(length)]
    return "".join(line)

def generate_pattern(x, y, probability):
    lines = [generate_line(x, probability) for _ in range(y)]
    return "\n".join(lines)


for i in range(1, 11):
    with open("random" + str(i) + ".grid", "w") as file:
        file.write(generate_pattern(120, 60, 0.9 / 1.25**i))
