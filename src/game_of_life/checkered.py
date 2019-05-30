# A Python3 script that creates checkered patterns.

for side in range(3,13):
    repetitions_vert = 25 // side
    repetitions_hor = repetitions_vert * 2
    line1 = ("#"*side + "."*side) * repetitions_hor + "\n"
    line2 = line1[side:-1] + line1[:side] + "\n"
    section = line1 * side + line2 * side
    pattern = (section * repetitions_vert)[:-1]
    with open("check" + str(side-2) + ".grid", "w") as file:
        file.write(pattern)
