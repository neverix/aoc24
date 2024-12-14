import z3
a = open("input.txt").read().split("\n\n")
total = 0
for l in a:
    a, b, c = l.split("\n")
    ax, ay = map(int, (a[12:14], a[18:20]))
    bx, by = map(int, (b[12:14], b[18:20]))
    c = c[9:]
    tx, ty = map(int, [a for a in c.split(", Y=")])
    tx = 10000000000000 + tx
    ty = 10000000000000 + ty
    
    # s = z3.Solver()
    s = z3.Optimize()
    a, b = z3.Int("a"), z3.Int("b")
    v = a >= 0, b >= 0, a * ax + b * bx == tx, a * ay + b * by == ty
    s.add(*v)
    h = s.minimize(a * 3 + b)
    if s.check() == z3.unsat:
        continue
    # if not s.check():
    #     continue
    total += int(str(s.lower(h)))
    # a * ax
print(total)