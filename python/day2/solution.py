# This opens a handle to your file, in 'r' read mode
file_handle = open('input.txt', 'r')
#file_handle = open('test.txt', 'r')

lines_list = file_handle.readlines()
ordered_line_list = []

for line in lines_list:
    ordered_line_list.append(sorted(line))

numThreeLetters = 0
numTwoLetters = 0
for line in ordered_line_list:
    threeLetters = False
    twoLetters = False
    index = 0
    while index < len(line):
        index2 = index + 1
        index3 = index + 2
        if ((index3 < len(line)) and (line[index3] == line[index])):
            threeLetters = True
            index += 3
        elif ((index2 < len(line)) and (line[index2] == line[index])):
            twoLetters = True
            index += 2
        else:
            index += 1
    
    if threeLetters:
        numThreeLetters += 1
    if twoLetters:
        numTwoLetters += 1


checksum = numThreeLetters * numTwoLetters

print(f"num3: {numThreeLetters}")
print(f"num2: {numTwoLetters}")
print(f"checksum: {checksum}")

for lineIndex,line in enumerate(lines_list):
    for otherLine in lines_list[lineIndex+1:]:
        differsByAtLeastOne = False
        differsByAtLeastTwo = False
        for letterIndex,letter in enumerate(line):
            if (letter != otherLine[letterIndex]):
                if (differsByAtLeastOne == True):
                    differsByAtLeastTwo = True
                    break
                differsByAtLeastOne = True
                indexOfDiffer = letterIndex

        if (not differsByAtLeastTwo and differsByAtLeastOne):
            print (f"line: {line} index: {lineIndex}")
            print (f"otherLine: {otherLine}")
            linesolution = line[0:indexOfDiffer] + line[indexOfDiffer+1:]
            print(linesolution)



                
                
        



