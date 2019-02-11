import time
from sortedcontainers import SortedList
# This opens a handle to your file, in 'r' read mode
file_handle = open('input.txt', 'r')

lines_list = file_handle.readlines()

int_list = []
for val in lines_list:
    int_list.append(int(val))

#int_list = [+3, +3, +4, -2, -4]
print(sum(int_list))

sum_list = SortedList([])
temp_value = int_list[0]
index = 1

start = time.time()
while temp_value not in sum_list:
    sum_list.add(temp_value)
    temp_value += int_list[index]
    index += 1
    if index == len(int_list):
        index = 0
        #print(len(sum_list))
end = time.time()

print(f"value: {temp_value}")
print(f"time: {end - start}")

