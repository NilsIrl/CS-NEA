for i in range(3,100000+1):
    bail = False
    j = 3
    while not bail:
        if i % j == 0:
            bail = True
        j += 1

    if j == i + 1:
        biggest = i

print(biggest)
